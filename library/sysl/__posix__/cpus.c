/* How many processors this machine has online, for `sysl.cpu_count`.
 *
 * A shim rather than an `extern "sysconf"` in sysl because the question is a constant whose *value*
 * differs between systems -- `_SC_NPROCESSORS_ONLN` is 58 on macOS and 84 on glibc -- and only the
 * platform's own header knows which. It sits under `__posix__` so that a target with no `sysconf`
 * never compiles it.
 */

#include <unistd.h>

/* The logical processors online now, and never fewer than one.
 *
 * `sysconf` answers -1 where it cannot say, and a count of zero would be a divisor waiting to
 * happen in every caller sizing a pool by it -- so both become one, which is the count every
 * machine that is running this has at least.
 */
long sysl_cpu_count(void) {
    long n = sysconf(_SC_NPROCESSORS_ONLN);

    return n < 1 ? 1 : n;
}
