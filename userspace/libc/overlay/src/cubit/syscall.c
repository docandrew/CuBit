/*
 * CuBit libc: the Linux x86-64 system call interface, implemented in the
 * process over CuBit syscalls and services (docs/servo-port.md).
 *
 * musl calls __cubit_syscall wherever it would execute `syscall`. Calls map
 * onto CuBit as follows; everything else returns -ENOSYS.
 *
 *   threads      clone (clone.s), exit -> THREAD_CREATE/THREAD_EXIT
 *   futexes      futex -> FUTEX_WAIT/FUTEX_WAKE (requeue wakes instead)
 *   memory       brk, anonymous mmap -> SBRK; munmap/mprotect/madvise are
 *                accepted but do not unmap or protect yet (no region API)
 *   time         clock_gettime/nanosleep -> the kernel millisecond clock
 *   descriptors  write, read, close, fstat, poll -> fd.c: a table of CuBit
 *                objects (stdout/stderr are the program's CuBit streams)
 *   files        open/stat/read/getdents/mmap of a file -> file.c, through
 *                filesystem.svc; paths resolve only inside the program's
 *                filesystem scopes. Read-only for now.
 *   process      exit_group -> EXIT; getpid -> GETPID; kill of self exits
 *   randomness   getrandom -> RDRAND (not yet the entropy service)
 *   signals      none: masks and handlers are accepted and never fire
 */
#define _GNU_SOURCE
#include <errno.h>
#include <stdint.h>
#include <string.h>
#include <time.h>
#include <signal.h>
#include <sched.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <sys/utsname.h>
#include <sys/resource.h>
#include <poll.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include "syscall.h"
#include "pthread_impl.h"
#include "lock.h"
#include "cubit_fd.h"

/* CuBit system calls (kernel/src/syscall.ads). */
enum {
	CUBIT_EXIT = 0,
	CUBIT_GETPID = 6,
	CUBIT_SBRK = 8,
	CUBIT_WRITE = 12,
	CUBIT_GETTIME = 27,
	CUBIT_SLEEP = 28,
	CUBIT_THREAD_EXIT = 91,
	CUBIT_FUTEX_WAIT = 92,
	CUBIT_FUTEX_WAKE = 93,
};

#define CUBIT_FUTEX_RETRY 1
#define CUBIT_FUTEX_TIMED_OUT 2
#define CUBIT_FOREVER ((unsigned long)-1)

/* The main thread's tid (threads THREAD_CREATE makes are 1..1023). */
#define MAIN_TID 0x3fffffff

static inline unsigned long cubit(unsigned long n, unsigned long a,
	unsigned long b, unsigned long c, unsigned long d, unsigned long e)
{
	unsigned long ret;
	register unsigned long r10 __asm__("r10") = d;
	register unsigned long r8 __asm__("r8") = e;
	__asm__ __volatile__ ("syscall" : "=a"(ret)
		: "a"(n), "D"(a), "S"(b), "d"(c), "r"(r10), "r"(r8)
		: "rcx", "r11", "memory");
	return ret;
}

static unsigned long now_ms(void)
{
	return cubit(CUBIT_GETTIME, 0, 0, 0, 0, 0);
}

/* Diagnostics only (the kernel console; see <cubit/debug.h>). */
static void debug_write(const void *p, size_t n)
{
	cubit(CUBIT_WRITE, 1, (unsigned long)p, n, 0, 0);
}

static _Noreturn void exit_process(int code)
{
	for (;;) cubit(CUBIT_EXIT, (unsigned long)code, 0, 0, 0, 0);
}

/* An argument this layer does not support: reported once per (what, value)
 * on the diagnostics console, like unimplemented calls. */
hidden void report_unsupported(const char *what, long value)
{
	static struct { const char *what; long value; } seen[32];
	static int count;
	for (int i = 0; i < count; i++)
		if (seen[i].what == what && seen[i].value == value) return;
	if (count < 32) { seen[count].what = what; seen[count].value = value; count++; }
	char msg[64] = "cubit-libc: unsupported ";
	size_t n = strlen(msg);
	for (const char *w = what; *w && n < 40; ) msg[n++] = *w++;
	msg[n++] = ' ';
	char digits[20];
	int d = 0;
	unsigned long v = value < 0 ? (unsigned long)-value : (unsigned long)value;
	do { digits[d++] = (char)('0' + v % 10); v /= 10; } while (v && d < 20);
	if (value < 0) msg[n++] = '-';
	while (d) msg[n++] = digits[--d];
	msg[n++] = '\n';
	debug_write(msg, n);
}

/* --- memory ------------------------------------------------------------- */

static unsigned long current_break;

static long sys_brk(unsigned long want)
{
	unsigned long cur = cubit(CUBIT_SBRK, 0, 0, 0, 0, 0);
	if (!current_break) current_break = cur;
	if (want <= cur) return cur;            /* CuBit's heap only grows */
	unsigned long got = cubit(CUBIT_SBRK, want - cur, 0, 0, 0, 0);
	if (got == (unsigned long)-1) return cur;
	current_break = want;
	return want;
}

static long sys_mmap(unsigned long addr, unsigned long len, long prot,
	long flags, long fd, long off)
{
	(void)prot;
	if (!len) return -EINVAL;
	if (flags & MAP_FIXED) return -ENOMEM;  /* no region API yet */
	(void)addr;
	if (!(flags & MAP_ANONYMOUS)) {
		/* A file: a private copy of its bytes (read-only use is what
		 * programs here need; writes are not carried back). */
		if (off & 4095) return -EINVAL;
		long base = sys_mmap(0, len, prot, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
		if (base < 0) return base;
		long got = __cubit_fd_pread((int)fd, (void *)base, len, off);
		if (got < 0) return got;        /* the region is not returned yet */
		return base;
	}
	len = (len + 4095) & ~4095UL;
	/* Page-aligned growth of the heap; CuBit maps it zero-filled. */
	unsigned long base = cubit(CUBIT_SBRK, len, 0, 0, 0, 0);
	if (base == (unsigned long)-1 || !base) return -ENOMEM;
	if (base & 4095) {
		/* The break was not page aligned: waste the head. */
		unsigned long pad = 4096 - (base & 4095);
		if (cubit(CUBIT_SBRK, pad, 0, 0, 0, 0) == (unsigned long)-1)
			return -ENOMEM;
		base += pad;
	}
	return (long)base;
}

/* --- futexes ------------------------------------------------------------ */

#define FUTEX_WAIT 0
#define FUTEX_WAKE 1
#define FUTEX_REQUEUE 3
#define FUTEX_CMP_REQUEUE 4
#define FUTEX_WAIT_BITSET 9
#define FUTEX_WAKE_BITSET 10
#define FUTEX_PRIVATE 128
#define FUTEX_CLOCK_REALTIME 256

static unsigned long deadline_after(const struct timespec *rel)
{
	if (!rel) return CUBIT_FOREVER;
	unsigned long ms = (unsigned long)rel->tv_sec * 1000
		+ ((unsigned long)rel->tv_nsec + 999999) / 1000000;
	return now_ms() + ms;
}

static unsigned long realtime_ms(void);

/* An absolute time on the monotonic clock (kernel milliseconds since boot)
 * or on the realtime clock (converted to the kernel's clock). */
static unsigned long deadline_at(const struct timespec *abs, int realtime)
{
	if (!abs) return CUBIT_FOREVER;
	unsigned long ms = (unsigned long)abs->tv_sec * 1000
		+ ((unsigned long)abs->tv_nsec + 999999) / 1000000;
	if (!realtime) return ms;
	unsigned long now = now_ms(), wall = realtime_ms();
	return ms <= wall ? now : now + (ms - wall);
}

static long sys_futex(int *uaddr, int op, int val,
	const struct timespec *timeout, int *uaddr2, int val3)
{
	(void)uaddr2;
	int cmd = op & ~(FUTEX_PRIVATE | FUTEX_CLOCK_REALTIME);
	unsigned long r;
	switch (cmd) {
	case FUTEX_WAIT:
	case FUTEX_WAIT_BITSET:
		r = cubit(CUBIT_FUTEX_WAIT, (unsigned long)uaddr, (unsigned)val,
			cmd == FUTEX_WAIT ? deadline_after(timeout)
				: deadline_at(timeout, (op & FUTEX_CLOCK_REALTIME) != 0),
			0, 0);
		if (r == 0) return 0;
		if (r == CUBIT_FUTEX_RETRY) return -EAGAIN;
		if (r == CUBIT_FUTEX_TIMED_OUT) return -ETIMEDOUT;
		return -EFAULT;
	case FUTEX_WAKE:
	case FUTEX_WAKE_BITSET:
		return (long)cubit(CUBIT_FUTEX_WAKE, (unsigned long)uaddr,
			val < 0 ? 0 : (unsigned)val, 0, 0, 0);
	case FUTEX_CMP_REQUEUE:
		if (*(volatile int *)uaddr != val3) return -EAGAIN;
		/* fall through */
	case FUTEX_REQUEUE:
		/* No requeue: wake every waiter; they recheck and contend.
		 * Correct for condition variables (spurious wakeups are allowed),
		 * just less efficient. */
		return (long)cubit(CUBIT_FUTEX_WAKE, (unsigned long)uaddr,
			CUBIT_FOREVER, 0, 0, 0);
	default:
		return -ENOSYS;
	}
}

/* --- time --------------------------------------------------------------- */

/* Wall-clock time: a snapshot from clock.svc (the fixed clock slot, present
 * when the manifest requests the clock service), advanced by the kernel's
 * millisecond clock and re-read each minute. Without a clock endpoint or a
 * valid time the realtime clock stays boot-relative, reported once. */
#define SLOT_CLOCK 25
#define CLOCK_SNAPSHOT 0x0B01
#define CLOCK_RESYNC_MS 60000

static volatile int wall_lock[1];
static unsigned long wall_base_ms;      /* kernel ms at the snapshot */
static unsigned long wall_base_utc;     /* UTC seconds at the snapshot */
static int wall_state;                  /* 0 unknown, 1 valid, -1 unavailable */

static void wall_sync(unsigned long ms)
{
	struct { uint32_t label; uint8_t length, flags; uint16_t reserved;
		uint64_t authority; uint64_t words[4]; } m = { CLOCK_SNAPSHOT, 0 };
	unsigned long tag = cubit(41, SLOT_CLOCK, (unsigned long)&m, 0, 0, 0);
	/* quality: 1 rtc-only, 4 network, 5 network-authenticated are valid */
	if (tag != (unsigned long)-1 && (uint32_t)tag == 0xF000 &&
	    (m.words[3] == 1 || m.words[3] == 4 || m.words[3] == 5)) {
		wall_base_utc = m.words[0];
		wall_base_ms = now_ms();
		wall_state = 1;
		return;
	}
	if (wall_state == 0) {
		static const char msg[] = "cubit-libc: no wall-clock time (clock service"
			" unavailable or not synchronized); realtime is time since boot\n";
		debug_write(msg, sizeof msg - 1);
	}
	if (wall_state != 1) wall_state = -1;
	(void)ms;
}

/* Milliseconds since the Unix epoch, or since boot without wall time. */
static unsigned long realtime_ms(void)
{
	unsigned long ms = now_ms();
	LOCK(wall_lock);
	if (wall_state == 0 || (wall_state == 1 && ms - wall_base_ms >= CLOCK_RESYNC_MS))
		wall_sync(ms);
	unsigned long r = wall_state == 1
		? wall_base_utc * 1000 + (now_ms() - wall_base_ms) : ms;
	UNLOCK(wall_lock);
	return r;
}

static long sys_clock_gettime(clockid_t clk, struct timespec *ts)
{
	if (clk == CLOCK_REALTIME || clk == CLOCK_REALTIME_COARSE) {
		unsigned long ms = realtime_ms();
		ts->tv_sec = ms / 1000;
		ts->tv_nsec = (ms % 1000) * 1000000;
		return 0;
	}
	switch (clk) {
	case CLOCK_REALTIME:
	case CLOCK_REALTIME_COARSE:
	case CLOCK_MONOTONIC:
	case CLOCK_MONOTONIC_RAW:
	case CLOCK_MONOTONIC_COARSE:
	case CLOCK_BOOTTIME:
		break;
	default:
		report_unsupported("clock", clk);
		return -EINVAL;
	}
	unsigned long ms = now_ms();
	ts->tv_sec = ms / 1000;
	ts->tv_nsec = (ms % 1000) * 1000000;
	return 0;
}

static long sleep_until_ms(unsigned long deadline)
{
	volatile int word = 0;
	while (now_ms() < deadline)
		cubit(CUBIT_FUTEX_WAIT, (unsigned long)&word, 0, deadline, 0, 0);
	return 0;
}

/* --- poll ------------------------------------------------------------------ */

/* Readiness comes from each descriptor's CuBit object (fd.c), which also
 * waits for it. A zero deadline means do not wait. */
static long sys_poll(struct pollfd *fds, unsigned long n, unsigned long deadline)
{
	return __cubit_fd_poll(fds, n, deadline);
}

/* --- randomness --------------------------------------------------------- */

static long sys_getrandom(unsigned char *buf, size_t len)
{
	for (size_t i = 0; i < len; i += 8) {
		unsigned long v;
		unsigned char ok = 0;
		for (int tries = 0; tries < 16 && !ok; tries++)
			__asm__ __volatile__ ("rdrand %0; setc %1" : "=r"(v), "=qm"(ok));
		if (!ok) return i ? (long)i : -EAGAIN;
		size_t n = len - i < 8 ? len - i : 8;
		memcpy(buf + i, &v, n);
	}
	return (long)len;
}

/* --- dispatch ----------------------------------------------------------- */

static __thread char thread_name[16];

static void report_unimplemented(long n)
{
	static unsigned char reported[512];
	if (n >= 0 && n < 512) {
		if (reported[n]) return;
		reported[n] = 1;
	}
	char msg[] = "cubit-libc: unimplemented system call    \n";
	unsigned long v = (unsigned long)n;
	for (int i = 40; i >= 38 && v; i--) { msg[i] = (char)('0' + v % 10); v /= 10; }
	debug_write(msg, sizeof msg - 1);
}

hidden long __cubit_syscall(long n, long a, long b, long c, long d, long e, long f)
{
	(void)f;
	switch (n) {
	case SYS_write: {
		struct iovec v = { (void *)b, (size_t)c };
		return __cubit_fd_writev((int)a, &v, 1);
	}
	case SYS_writev:
		return __cubit_fd_writev((int)a, (const struct iovec *)b, (int)c);
	case SYS_read:
		return __cubit_fd_read((int)a, (void *)b, (size_t)c);
	case SYS_readv: {
		const struct iovec *v = (const struct iovec *)b;
		long total = 0;
		for (int i = 0; i < (int)c; i++) {
			long got = __cubit_fd_read((int)a, v[i].iov_base, v[i].iov_len);
			if (got < 0) return total ? total : got;
			total += got;
			if ((size_t)got < v[i].iov_len) break;
		}
		return total;
	}
	case SYS_pread64:
		return __cubit_fd_pread((int)a, (void *)b, (size_t)c, (off_t)d);
	case SYS_close:
		return __cubit_fd_close((int)a);
	case SYS_ioctl:
		return -ENOTTY;                 /* no terminals: streams are not ttys */
	case SYS_fstat:
		return __cubit_fd_fstat((int)a, (struct stat *)b);
	case SYS_lseek:
		return __cubit_fd_lseek((int)a, (off_t)b, (int)c);
	case SYS_open:
		return __cubit_fd_open((const char *)a, (int)b);
	case SYS_openat:
		if ((int)a != AT_FDCWD && ((const char *)b)[0] != '/'
			&& ((const char *)b)[0] != '@')
			return -ENOTSUP;        /* no directory-relative names yet */
		return __cubit_fd_open((const char *)b, (int)c);
	case SYS_stat:
	case SYS_lstat:
		return __cubit_path_stat((const char *)a, (struct stat *)b);
	case SYS_newfstatat:
		if ((d & AT_EMPTY_PATH) && !*(const char *)b)
			return __cubit_fd_fstat((int)a, (struct stat *)c);
		if ((int)a != AT_FDCWD && ((const char *)b)[0] != '/'
			&& ((const char *)b)[0] != '@')
			return -ENOTSUP;
		return __cubit_path_stat((const char *)b, (struct stat *)c);
	case SYS_access:
	case SYS_faccessat: {
		const char *path = (const char *)(n == SYS_access ? a : b);
		int mode = (int)(n == SYS_access ? b : c);
		struct stat st;
		long r = __cubit_path_stat(path, &st);
		if (r) return r;
		return (mode & W_OK) ? -EROFS : 0;
	}
	case SYS_getdents64:
		return __cubit_fd_getdents((int)a, (void *)b, (size_t)c);
	case SYS_fcntl:
		return __cubit_fd_fcntl((int)a, (int)b, c);
	case SYS_getcwd:
		if ((size_t)b < 2) return -ERANGE;
		memcpy((char *)a, "/", 2);
		return 2;

	case SYS_brk:
		return sys_brk((unsigned long)a);
	case SYS_mmap:
		return sys_mmap(a, b, c, d, e, f);
	case SYS_munmap:
	case SYS_mprotect:
	case SYS_madvise:
		return 0;
	case SYS_mremap:
		return -ENOMEM;                 /* musl falls back to mmap + copy */

	case SYS_futex:
		return sys_futex((int *)a, (int)b, (int)c,
			(const struct timespec *)d, (int *)e, (int)f);
	case SYS_set_tid_address:
		return MAIN_TID;
	case SYS_set_robust_list:
		return 0;
	case SYS_exit:
		for (;;) cubit(CUBIT_THREAD_EXIT, 0, 0, 0, 0, 0);
	case SYS_exit_group:
		exit_process((int)a);
	case SYS_getpid:
		return (long)cubit(CUBIT_GETPID, 0, 0, 0, 0, 0);
	case SYS_gettid:
		/* The thread's id as clone/set_tid_address gave it to musl. */
		return __pthread_self()->tid;
	case SYS_sched_yield: {
		volatile int word = 0;
		cubit(CUBIT_FUTEX_WAIT, (unsigned long)&word, 0, 0, 0, 0);
		return 0;
	}
	case SYS_sched_getaffinity: {
		/* Four CPUs until the kernel reports its count. */
		size_t size = (size_t)b;
		if (size < 8) return -EINVAL;
		memset((void *)c, 0, size);
		*(unsigned long *)c = 0xf;
		return 8;
	}

	case SYS_clock_gettime:
		return sys_clock_gettime((clockid_t)a, (struct timespec *)b);
	case SYS_clock_getres:
		if (b) { ((struct timespec *)b)->tv_sec = 0; ((struct timespec *)b)->tv_nsec = 1000000; }
		return 0;
	case SYS_nanosleep:
		return sleep_until_ms(deadline_after((const struct timespec *)a));
	case SYS_clock_nanosleep:
		return sleep_until_ms((b & TIMER_ABSTIME)
			? deadline_at((const struct timespec *)c,
				(clockid_t)a == CLOCK_REALTIME)
			: deadline_after((const struct timespec *)c));

	case SYS_poll:
		return sys_poll((struct pollfd *)a, (unsigned long)b,
			(int)c < 0 ? CUBIT_FOREVER : (int)c == 0 ? 0
			: now_ms() + (unsigned long)(int)c);
	case SYS_ppoll: {
		const struct timespec *t = (const struct timespec *)c;
		return sys_poll((struct pollfd *)a, (unsigned long)b,
			!t ? CUBIT_FOREVER : (!t->tv_sec && !t->tv_nsec) ? 0
			: deadline_after(t));
	}
	case SYS_pipe:
		return __cubit_fd_pipe((int *)a, 0);
	case SYS_pipe2:
		return __cubit_fd_pipe((int *)a, (int)b);
	case SYS_socketpair:
		/* (domain, type|flags, protocol, sv): connected local stream
		 * pairs only; network sockets go through netstack (not yet). */
		if ((int)a != AF_UNIX || ((int)b & 0xf) != SOCK_STREAM)
			return -EAFNOSUPPORT;
		return __cubit_fd_socketpair((int *)d,
			(((int)b & SOCK_NONBLOCK) ? O_NONBLOCK : 0) |
			(((int)b & SOCK_CLOEXEC) ? O_CLOEXEC : 0));
	case SYS_socket: {
		int type = (int)b & 0xf;
		int flags = (((int)b & SOCK_NONBLOCK) ? O_NONBLOCK : 0) |
			(((int)b & SOCK_CLOEXEC) ? O_CLOEXEC : 0);
		if ((int)a == AF_INET && type == SOCK_STREAM &&
		    (c == 0 || c == IPPROTO_TCP))
			return __cubit_fd_socket_tcp(flags);
		if ((int)a == AF_INET || (int)a == AF_INET6 || (int)a == AF_UNIX)
			return (int)a == AF_INET ? -EPROTONOSUPPORT : -EAFNOSUPPORT;
		return -EAFNOSUPPORT;
	}
	case SYS_connect: {
		int nonblock;
		struct cubit_tcp *t = __cubit_fd_tcp((int)a, &nonblock);
		if (!t) return __cubit_fd_is_socket((int)a) ? -EISCONN : -ENOTSOCK;
		return __cubit_tcp_connect(t, (const struct sockaddr *)b,
			(unsigned)c, nonblock);
	}
	case SYS_getsockopt: {
		/* (fd, level, name, value, length) */
		struct cubit_tcp *t = __cubit_fd_tcp((int)a, 0);
		if (!t && !__cubit_fd_is_socket((int)a)) return -ENOTSOCK;
		socklen_t *len = (socklen_t *)e;
		if (*len < sizeof(int)) return -EINVAL;
		int value = 0;
		if (b == SOL_SOCKET && c == SO_ERROR) value = t ? (int)__cubit_tcp_so_error(t) : 0;
		else if (b == SOL_SOCKET && c == SO_TYPE) value = SOCK_STREAM;
		else if (b == SOL_SOCKET && (c == SO_RCVBUF || c == SO_SNDBUF)) value = 32768;
		else if (!(b == IPPROTO_TCP && c == TCP_NODELAY) &&
		         !(b == SOL_SOCKET && c == SO_KEEPALIVE)) {
			report_unsupported("getsockopt", c);
			return -ENOPROTOOPT;
		}
		*(int *)d = value;
		*len = sizeof(int);
		return 0;
	}
	case SYS_setsockopt:
		/* netstack chooses these; accept the common tuning options. */
		if (!__cubit_fd_is_socket((int)a)) return -ENOTSOCK;
		if ((b == IPPROTO_TCP && c == TCP_NODELAY) ||
		    (b == SOL_SOCKET && (c == SO_KEEPALIVE || c == SO_REUSEADDR ||
		     c == SO_RCVBUF || c == SO_SNDBUF || c == SO_LINGER)))
			return 0;
		report_unsupported("setsockopt", c);
		return -ENOPROTOOPT;
	case SYS_getpeername: {
		struct cubit_tcp *t = __cubit_fd_tcp((int)a, 0);
		if (!t) return __cubit_fd_is_socket((int)a) ? -ENOTCONN : -ENOTSOCK;
		return __cubit_tcp_peer(t, (struct sockaddr *)b, (unsigned *)c);
	}
	case SYS_getsockname: {
		/* netstack does not report the local end; an unspecified one. */
		if (!__cubit_fd_is_socket((int)a)) return -ENOTSOCK;
		struct sockaddr_in local = { .sin_family = AF_INET };
		socklen_t *len = (socklen_t *)c;
		memcpy((void *)b, &local, *len < sizeof local ? *len : sizeof local);
		*len = sizeof local;
		return 0;
	}
	case SYS_bind:
	case SYS_listen:
	case SYS_accept:
	case SYS_accept4:
		return -EOPNOTSUPP;             /* no listening sockets yet */
	case SYS_recvfrom:                  /* connected pairs: no address */
		if (e) return -EOPNOTSUPP;
		return __cubit_fd_read((int)a, (void *)b, (size_t)c);
	case SYS_sendto: {
		if (e) return -EOPNOTSUPP;
		struct iovec v = { (void *)b, (size_t)c };
		return __cubit_fd_writev((int)a, &v, 1);
	}
	case SYS_dup:
		return __cubit_fd_dup((int)a, 0, -1, 0);
	case SYS_dup2:
		return __cubit_fd_dup((int)a, 0, (int)b, 0);
	case SYS_dup3:
		if ((int)a == (int)b) return -EINVAL;
		return __cubit_fd_dup((int)a, 0, (int)b, ((int)c & O_CLOEXEC) != 0);
	case SYS_shutdown: {
		struct cubit_tcp *t = __cubit_fd_tcp((int)a, 0);
		if (t) return __cubit_tcp_shutdown(t, (int)b);
		return __cubit_fd_is_socket((int)a) ? 0 : -ENOTSOCK;
	}

	case SYS_getrandom:
		return sys_getrandom((unsigned char *)a, (size_t)b);

	case SYS_rt_sigprocmask:
		/* (how, set, oldset, size): no signals are ever blocked. */
		if (c && d) memset((void *)c, 0, (size_t)d > 128 ? 128 : (size_t)d);
		return 0;
	case SYS_rt_sigaction:
		if (c) memset((void *)c, 0, 32);
		return 0;
	case SYS_sigaltstack:
		return 0;
	case SYS_kill:
	case SYS_tkill:
	case SYS_tgkill: {
		int sig = (int)(n == SYS_tgkill ? c : b);
		if (sig == 0) return 0;
		exit_process(128 + sig);        /* no handlers: signals are fatal */
	}

	case SYS_getuid: case SYS_geteuid: case SYS_getgid: case SYS_getegid:
		return 0;
	case SYS_uname: {
		struct utsname *u = (struct utsname *)a;
		memset(u, 0, sizeof *u);
		strcpy(u->sysname, "CuBit");
		strcpy(u->nodename, "cubit");
		strcpy(u->release, "0.1");
		strcpy(u->version, "0.1");
		strcpy(u->machine, "x86_64");
		return 0;
	}
	case SYS_prlimit64:
		if (d) {
			struct rlimit *r = (struct rlimit *)d;
			r->rlim_cur = r->rlim_max = RLIM_INFINITY;
		}
		return 0;
	case SYS_prctl:
		/* Thread names (PR_SET_NAME/PR_GET_NAME) are kept per thread in
		 * the process; the kernel has no thread names yet. */
		if (a == 15) {                  /* PR_SET_NAME */
			strncpy(thread_name, (const char *)b, 15);
			thread_name[15] = 0;
			return 0;
		}
		if (a == 16) {                  /* PR_GET_NAME */
			memcpy((char *)b, thread_name, 16);
			return 0;
		}
		return -EINVAL;
	case SYS_membarrier:
		return -ENOSYS;
	default:
		report_unimplemented(n);
		return -ENOSYS;
	}
}
