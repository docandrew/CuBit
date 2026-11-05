/*
 * CuBit libc: file calls beyond POSIX.
 *
 * cubit_flock_timeout: flock(2) with an explicit limit. operation is
 * LOCK_SH or LOCK_EX (optionally | LOCK_NB) or LOCK_UN, as flock. Returns
 * 0, or a negative errno value (errno itself is not set): -EWOULDBLOCK
 * (LOCK_NB, held elsewhere), -ETIMEDOUT (a blocking request gave up after
 * timeout_ms milliseconds; a negative timeout_ms waits without limit),
 * -EINTR (the descriptor unlocked or closed meanwhile), -EBADF, -EINVAL. The lock
 * belongs to the descriptor's handle at the filesystem service: it ends at
 * LOCK_UN, at close, and when the process exits however it ends.
 */
#ifndef _CUBIT_FILES_H
#define _CUBIT_FILES_H
#ifdef __cplusplus
extern "C" {
#endif
long cubit_flock_timeout(int fd, int operation, long timeout_ms);
#ifdef __cplusplus
}
#endif
#endif
