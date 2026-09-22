/* Running a child and waiting for it.
 *
 * This is a shim for the same reason `sysl/fs/__posix__/dirent.c` is one: what the module needs
 * from POSIX is not reachable by symbol alone. Three separate things put it here rather than in
 * sysl --
 *
 *   - `WIFEXITED`, `WEXITSTATUS`, `WIFSIGNALED` and `WTERMSIG` are macros over the bits of an int
 *     that no header publishes as a layout, so how a child ended can only be decoded in C;
 *   - everything between `fork` and `execvp` runs in a process that has a copy of this one's
 *     address space and must not allocate, which is not a thing to express across an FFI boundary;
 *   - `pid_t` and the argument vector's exact type differ enough between platforms to be worth
 *     naming once here instead of transcribing.
 *
 * It sits under `__posix__` so that it is absent on a target with no processes to start, which is
 * what lets the module go on being compiled for every target.
 */

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

/* How long a child that has been asked to stop is given before it is made to.
 *
 * Deliberately short. A child that means to tidy up on `SIGTERM` has already had the whole of its
 * timeout to finish, and one that ignores the signal is not going to honour a longer wait either --
 * the caller asked for a bound, and the grace is part of it rather than an extension to it.
 */
#define SYSL_PROC_GRACE_MS 200

/* The longest a bounded wait sleeps between asking whether the child has ended.
 *
 * It starts at a millisecond and doubles up to this, so a child that ends at once is noticed at
 * once and one that runs for an hour is not asked about a thousand times a second.
 */
#define SYSL_PROC_NAP_MAX_MS 20

/* The monotonic clock in milliseconds, which is what a deadline is measured against.
 *
 * Monotonic rather than the wall clock, because a deadline compared against a clock somebody can
 * set backwards is a deadline that can be moved after the fact -- an NTP step during a long child
 * would either cut its timeout short or extend it indefinitely.
 */
static long long now_ms(void) {
    struct timespec ts = {0, 0};

    clock_gettime(CLOCK_MONOTONIC, &ts);

    return (long long) ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

/* Sleeps for `ms`, or for however much of it a signal leaves.
 *
 * What is left of an interrupted sleep is not carried over: the cost of cutting one short is
 * looking at the child a little early, and the deadline is checked against the clock rather than
 * against a count of naps, so nothing drifts.
 */
static void nap(long long ms) {
    struct timespec want = { (time_t) (ms / 1000), (long) (ms % 1000) * 1000000L };

    nanosleep(&want, NULL);
}

/* One of the child's streams pointed at a file the parent named. Answers an `errno`, or zero.
 *
 * A path that is empty, or absent, means the stream is left alone -- so a capture of standard
 * output only, of standard error only, or of both is the same code path with a different pair of
 * arguments, and there is no combination the caller can ask for that this does not answer.
 */
static int redirect(const char *path, int fd_no) {
    if (!path || !path[0]) return 0;

    int fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0600);

    if (fd < 0) return errno;

    if (dup2(fd, fd_no) < 0) {
        int e = errno;

        close(fd);
        return e;
    }

    close(fd);
    return 0;
}

/* Everything the child does before it becomes the other program, as one function so that the
 * caller below is a straight line. Answers an `errno`, or zero.
 *
 * It allocates nothing and opens at most one descriptor per stream, which is the constraint the
 * whole between-fork-and-exec window is written under.
 */
static int child_setup(const char *const *names, const char *const *values,
                       const char *dir, const char *out_path, const char *err_path) {
    /* `setenv` here rather than a whole `envp` handed to `execve`, so that a caller adds to the
     * environment instead of replacing it -- a child that lost PATH, HOME and TMPDIR because its
     * parent wanted to set one variable is a surprise nobody wants. This is also the one place
     * `setenv` is safe: the child is single-threaded by construction and is about to exec, so the
     * thread-safety objection that keeps it out of `sysl.env` does not apply. */
    if (names && values) {
        for (int i = 0; names[i]; i++) {
            if (setenv(names[i], values[i], 1) != 0) return errno;
        }
    }

    if (dir && dir[0] && chdir(dir) != 0) return errno;

    int e = redirect(out_path, STDOUT_FILENO);

    if (e != 0) return e;

    return redirect(err_path, STDERR_FILENO);
}

/* Whether the child has ended, reaping it if it has.
 *
 * Answers 1 for ended, 0 for still running, and -1 for a failure whose `errno` is left in `*err`.
 * `EINTR` is retried rather than reported, for the reason the blocking wait below retries it: a
 * signal arriving here is not the child's doing and must not turn it into a failure.
 */
static int reaped(pid_t pid, int *status, int *err) {
    for (;;) {
        pid_t ended = waitpid(pid, status, WNOHANG);

        if (ended == pid) return 1;

        if (ended == 0) return 0;

        if (errno == EINTR) continue;

        *err = errno;
        return -1;
    }
}

/* Wait for the child for as long as it takes. Answers 0, or an `errno`. */
static int wait_out(pid_t pid, int *status) {
    while (waitpid(pid, status, 0) < 0) {
        if (errno != EINTR) return errno;
    }

    return 0;
}

/* Wait for the child until `deadline`. Answers 1 if it ended in time, 0 if the deadline arrived
 * first, and -1 for a failure whose `errno` is left in `*err`.
 *
 * **Polling rather than waiting for `SIGCHLD`, because the alternatives all change state that
 * belongs to the whole program.** A handler, a blocked signal or a `sigtimedwait` is process-wide:
 * installing one here would be a library deciding what a caller's own signal handling looks like,
 * and restoring it afterwards still races with anything else running at the time. `sigtimedwait` is
 * not on macOS in any case. A `WNOHANG` loop asks the kernel a question and leaves nothing behind,
 * and what it costs is a wake-up every few milliseconds while a child runs.
 */
static int wait_until(pid_t pid, int *status, long long deadline, int *err) {
    long long step = 1;

    for (;;) {
        int ended = reaped(pid, status, err);

        if (ended != 0) return ended;

        long long left = deadline - now_ms();

        if (left <= 0) return 0;

        nap(step < left ? step : left);

        if (step < SYSL_PROC_NAP_MAX_MS) step *= 2;
    }
}

/* Start `program`, wait for it, and say how it ended.
 *
 * Returns 0 having set `*code` and `*sig`, or an `errno` if the child could not be started at all.
 *
 * `timeout_ms` bounds the whole of the child's life, and zero or less means it is unbounded. On a
 * deadline that arrives first the child is sent `SIGTERM`, given `SYSL_PROC_GRACE_MS` to go, then
 * sent `SIGKILL` -- and `*timed_out` is set, because the exit status of a child that was killed
 * because it ran out of time says nothing a caller wants to hear.
 *
 * **The signal goes to the child and not to its process group**, which is the same restraint the
 * module keeps everywhere else: putting the child in a group of its own would take it out of the
 * terminal's foreground group, so a person's own interrupt would stop reaching it. What that costs
 * is that a child which forked grandchildren of its own leaves them behind -- for which the answer
 * is to run the program rather than a shell that runs it, which is what this module does anyway.
 *
 * **The pipe is how a failed `execvp` is told from a program that ran and exited 127**, which is
 * the distinction a caller most wants and the one `system(3)` cannot make. It is close-on-exec, so
 * a successful exec closes it and the parent's read sees end-of-file; a failure writes the `errno`
 * into it first. Without this, a missing program and a program whose own exit status is 127 are the
 * same answer -- and "no such file or directory" is the single most likely thing to go wrong when a
 * tool shells out.
 */
int sysl_proc_run(const char *program, char *const *argv,
                  const char *const *env_names, const char *const *env_values,
                  const char *dir, const char *out_path, const char *err_path,
                  int timeout_ms, int *code, int *sig, int *timed_out) {
    *timed_out = 0;

    /* **Everything this program has written, written, before anything else can write.**
     *
     * A C library buffers standard output, and it buffers it *fully* rather than by line whenever
     * the destination is not a terminal -- a pipe, a file, a CI log. The child writes to the same
     * file description directly and is not buffered by anything of ours, so without this its output
     * lands ahead of text the parent printed first and the log reads in the wrong order. It looks
     * like the parent forgot to say what it was doing.
     *
     * `NULL` flushes every output stream rather than just `stdout`, which is what makes it correct
     * for a program writing to both channels: they are separately buffered and would otherwise be
     * separately out of order.
     *
     * It is also the reason this belongs to the fork rather than to the caller. Any buffered bytes
     * still held here are duplicated into the child by `fork`, and a child that did something other
     * than `exec` immediately would print them a second time -- flushing first is what makes that
     * unreachable rather than merely unlikely.
     */
    fflush(NULL);

    int report[2];

    if (pipe(report) != 0) return errno;

    if (fcntl(report[1], F_SETFD, FD_CLOEXEC) != 0) {
        int e = errno;

        close(report[0]);
        close(report[1]);
        return e;
    }

    pid_t pid = fork();

    /* The clock starts here, so the timeout covers the child's whole life rather than only the
     * part of it after the exec. */
    long long started_at = now_ms();

    if (pid < 0) {
        int e = errno;

        close(report[0]);
        close(report[1]);
        return e;
    }

    if (pid == 0) {
        close(report[0]);

        int e = child_setup(env_names, env_values, dir, out_path, err_path);

        if (e == 0) {
            execvp(program, argv);
            e = errno;
        }

        /* The parent is about to learn why from the pipe; the status is the shell's convention for
         * a command that could not be run, and is what a caller sees if the write is lost. */
        ssize_t ignored = write(report[1], &e, sizeof e);

        (void) ignored;
        _exit(127);
    }

    close(report[1]);

    int child_errno = 0;
    ssize_t got = read(report[0], &child_errno, sizeof child_errno);

    close(report[0]);

    int status = 0;

    if (timeout_ms <= 0) {
        int e = wait_out(pid, &status);

        if (e != 0) return e;
    } else {
        int wait_errno = 0;
        int in_time = wait_until(pid, &status, started_at + timeout_ms, &wait_errno);

        if (in_time < 0) return wait_errno;

        if (in_time == 0) {
            /* Asked first and made second. A child that stops on being asked gets to run whatever
             * it does on the way out -- flush what it was writing, remove what it was building --
             * and one that does not is still gone when this returns. */
            kill(pid, SIGTERM);

            in_time = wait_until(pid, &status, now_ms() + SYSL_PROC_GRACE_MS, &wait_errno);

            if (in_time < 0) return wait_errno;

            if (in_time == 0) {
                kill(pid, SIGKILL);

                /* `SIGKILL` cannot be caught or ignored, so this waits for something that is
                 * already on its way rather than for the child's cooperation. */
                int e = wait_out(pid, &status);

                if (e != 0) return e;
            }

            *timed_out = 1;
        }
    }

    if (got == (ssize_t) sizeof child_errno && child_errno != 0) return child_errno;

    if (WIFEXITED(status)) {
        *code = WEXITSTATUS(status);
        *sig = 0;
    } else if (WIFSIGNALED(status)) {
        *code = 0;
        *sig = WTERMSIG(status);
    } else {
        *code = 0;
        *sig = 0;
    }

    return 0;
}

/* A path nothing else holds, created empty so that it stays that way, written into the caller's
 * own buffer.
 *
 * `mkstemp` rather than a name built from a clock and a counter, because it creates the file and
 * hands back the descriptor in one step -- there is no window in which a second process could take
 * the same name. The descriptor is closed straight away: what the caller wants is the path, to hand
 * to a child as its standard output.
 */
int sysl_proc_temp_path(char *buf, size_t n) {
    const char *tmp = getenv("TMPDIR");

    if (!tmp || !tmp[0]) tmp = "/tmp";

    int wrote = snprintf(buf, n, "%s/sysl-proc-XXXXXX", tmp);

    if (wrote < 0 || (size_t) wrote >= n) return ENAMETOOLONG;

    int fd = mkstemp(buf);

    if (fd < 0) return errno;

    close(fd);
    return 0;
}
