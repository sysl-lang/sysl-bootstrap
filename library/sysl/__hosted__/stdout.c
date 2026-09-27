/* Standard output as one call per buffer, which is what `sysl.putbytes` writes through on a machine
 * with a C library under it.
 *
 * **It is C because `stdout` is a macro.** Darwin's `<stdio.h>` defines it to `__stdoutp`, glibc and
 * musl to their own names, so no `extern` written in sysl reaches the stream by one spelling
 * everywhere -- and this is the one line that has to name it.
 *
 * **It stays on stdio rather than going to `write(1, ...)`**, because standard output is shared: a
 * package's C calling `printf`, or anything else that writes through `FILE *stdout`, lands in stdio's
 * buffer, and a raw descriptor write would overtake whatever that buffer still held. One `fwrite`
 * joins the same queue as every other writer, so the order a program printed in is the order it
 * comes out in.
 *
 * It answers how many bytes went, which is short only when the stream has failed; the caller loops
 * on a short answer and stops on none, exactly as `eputbytes` does for `write`.
 *
 * Selected by the `__hosted__` directory it sits in, so a freestanding target -- whose `putchar` is
 * whatever its board supplies, and which has no `FILE` -- compiles none of it.
 */

#include <stddef.h>
#include <stdio.h>

size_t sysl_stdout_write(const unsigned char *p, size_t n) {
    return fwrite(p, 1, n, stdout);
}
