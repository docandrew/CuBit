/* Native checks for the CuBit libc (userspace/libc): stdio, malloc,
 * pthreads, thread-local storage, time and formatting. */
#define _GNU_SOURCE
#include <pthread.h>
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <errno.h>
#include <sys/mman.h>
#include <sys/syscall.h>
#include <sys/stat.h>
#include <sys/select.h>
#include <limits.h>
#include <dirent.h>
#include <fcntl.h>
#include <poll.h>
#include <sys/socket.h>
#include <stdarg.h>
#include <cubit/debug.h>

static int failures;

/* Test verdicts go to the debug console (read by the headless runner);
 * the program's stdout is its CuBit stream, exercised separately. */
static void say(const char *fmt, ...)
{
	char buf[256];
	va_list ap;
	va_start(ap, fmt);
	int n = vsnprintf(buf, sizeof buf, fmt, ap);
	va_end(ap);
	if (n > (int)sizeof buf - 1) n = sizeof buf - 1;
	cubit_debug_write(buf, (size_t)n);
}

static void check(int ok, const char *name)
{
	say("libc-check: %s %s\n", name, ok ? "PASS" : "FAIL");
	if (!ok) failures++;
}

static pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t cond = PTHREAD_COND_INITIALIZER;
static long counter;
static int go;
static __thread long per_thread = 7;

static void *worker(void *arg)
{
	long id = (long)arg;
	per_thread = id * 10;
	pthread_mutex_lock(&lock);
	while (!go) pthread_cond_wait(&cond, &lock);
	pthread_mutex_unlock(&lock);
	for (int i = 0; i < 20000; i++) {
		pthread_mutex_lock(&lock);
		counter++;
		pthread_mutex_unlock(&lock);
	}
	return (void *)(per_thread == id * 10 ? id : -1);
}

static int cmp(const void *a, const void *b)
{
	return *(const int *)a - *(const int *)b;
}

static void *late_writer(void *arg)
{
	usleep(50000);
	write(*(int *)arg, "w", 1);
	return 0;
}

int main(void)
{
	/* sched_yield returns to the caller (it no longer waits on a futex). */
	check(sched_yield() == 0 && sched_yield() == 0, "sched_yield returns");
	check(mmap(0, 4096, PROT_READ | PROT_EXEC,
	      MAP_PRIVATE | MAP_ANONYMOUS, -1, 0) == MAP_FAILED && errno == ENOTSUP,
	      "executable mmap is unsupported, not false success");
	check(mmap(0, 4096, PROT_READ | PROT_WRITE | PROT_EXEC,
	      MAP_PRIVATE | MAP_ANONYMOUS, -1, 0) == MAP_FAILED && errno == ENOTSUP,
	      "writable executable mmap is denied");
	void *protection_probe = mmap(0, 4096, PROT_READ | PROT_WRITE,
	      MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	check(protection_probe != MAP_FAILED &&
	      mprotect(protection_probe, 4096, PROT_READ | PROT_EXEC) == -1 && errno == ENOSYS,
	      "mprotect RX cannot falsely succeed");
	check(protection_probe != MAP_FAILED &&
	      mprotect(protection_probe, 4096, PROT_NONE) == 0,
	      "mprotect guard installed");
	check(mprotect(protection_probe, 4096, PROT_READ | PROT_WRITE) == 0,
	      "restore owned page access");
	if (protection_probe == MAP_FAILED) return 1;
	volatile unsigned char *probe = protection_probe;
	probe[0] = 0x37;
	/* Bypass musl's public wrapper, which rounds addresses/lengths before
	 * invoking the shim. These malformed values must reach kernel admission. */
	check(syscall(SYS_mprotect, (char *)protection_probe + 1, 1, PROT_NONE) == -1
	      && errno == EINVAL, "unaligned protection rejected");
	check(mprotect(protection_probe, 8192, PROT_NONE) == -1
	      && errno == EINVAL, "protection beyond allocation rejected");
	check(syscall(SYS_mprotect, protection_probe, (size_t)-1, PROT_NONE) == -1
	      && errno == EINVAL, "wrapping protection size rejected");
	check(mprotect(protection_probe, 0, PROT_NONE) == -1
	      && errno == EINVAL, "empty owned protection rejected");
	check(probe[0] == 0x37, "rejected protections preserve readable data");
	probe[0] = 0x73;
	check(probe[0] == 0x73, "rejected protections preserve write access");
	check(mprotect(protection_probe, 4096, PROT_READ) == 0 && probe[0] == 0x73,
	      "read-only transition preserves data");
	check(mprotect(protection_probe, 4096, PROT_READ | PROT_WRITE) == 0,
	      "read-only mapping can return to writable");
	probe[0] = 0x19;
	check(munmap(protection_probe, 4096) == 0, "release protection probe");
	void *guard = mmap(0, 8192, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	check(guard != MAP_FAILED && mprotect((char *)guard + 4096, 4096,
	      PROT_READ | PROT_WRITE) == 0, "enable usable stack suffix");
	if (guard != MAP_FAILED) {
		((volatile char *)guard)[4096] = 42;
		check(munmap(guard, 8192) == 0, "release mixed guard and data pages");
	}
	for (int round = 0; round < 128; round++) {
		unsigned char *p = mmap(0, 1 << 20, PROT_READ | PROT_WRITE,
		                       MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
		check(p != MAP_FAILED, "repeated owned mmap");
		if (p == MAP_FAILED) return 1;
		check(p[0] == 0 && p[(1 << 20) - 1] == 0, "owned mmap zero fill");
		memset(p, 0x5a, 1 << 20);
		check(munmap(p, 4096) == -1 && errno == EINVAL,
		      "partial munmap rejected");
		check(p[0] == 0x5a && p[(1 << 20) - 1] == 0x5a,
		      "rejected unmap preserves contents");
		check(munmap(p, 1 << 20) == 0, "whole munmap releases allocation");
		check(munmap(p, 1 << 20) == -1 && errno == EINVAL,
		      "duplicate munmap rejected");
	}
	say("libc-check: hello from musl on CuBit\n");
	/* stdout is the program's stdout stream, not the console. */
	check(printf("libc-check: to the stdout stream\n") > 0 && fflush(stdout) == 0,
	      "stdout writes to the CuBit stream");
	check(isatty(1) == 0, "stdout is a stream, not a terminal");
	check(read(0, (char[1]){0}, 1) < 0, "no implicit stdin");

	void *raw = mmap(0, 1 << 20, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

	check(raw != MAP_FAILED, "anonymous mmap");
	char *big = malloc(8 << 20);
	if (!big) {
		say("libc-check: malloc 8 MiB failed (errno %d)\nLIBC: FAIL\n", errno);
		return 1;
	}
	memset(big, 0xab, 8 << 20);
	char *small[1000];
	for (int i = 0; i < 1000; i++) small[i] = malloc(i + 1);
	for (int i = 0; i < 1000; i++) free(small[i]);
	check(big && big[(8 << 20) - 1] == (char)0xab, "malloc large and small");
	free(big);

	pthread_t t[8];
	for (long i = 0; i < 8; i++) {
		int error = pthread_create(&t[i], 0, worker, (void *)(i + 1));
		if (error) {
			say("libc-check: pthread_create failed %d\nLIBC: FAIL\n", error);
			return 1;
		}
	}
	struct timespec pause = { 0, 5 * 1000000 };
	nanosleep(&pause, 0);
	pthread_mutex_lock(&lock);
	go = 1;
	pthread_cond_broadcast(&cond);
	pthread_mutex_unlock(&lock);
	long ok = 1;
	for (long i = 0; i < 8; i++) {
		void *r;
		pthread_join(t[i], &r);
		ok &= (long)r == i + 1;
	}
	check(counter == 160000, "pthread mutex across 8 threads");
	check(ok, "__thread variables per thread and join values");
	check(per_thread == 7, "main thread's __thread initial value");

	struct timespec a, b;
	clock_gettime(CLOCK_MONOTONIC, &a);
	usleep(30000);
	clock_gettime(CLOCK_MONOTONIC, &b);
	long ms = (b.tv_sec - a.tv_sec) * 1000 + (b.tv_nsec - a.tv_nsec) / 1000000;
	check(ms >= 30 && ms < 2000, "usleep and clock_gettime");

	char buf[64];
	snprintf(buf, sizeof buf, "%.3f %e %d %s", 3.14159, 12345.678, -42, "x");
	check(strcmp(buf, "3.142 1.234568e+04 -42 x") == 0, "printf floats");
	check(strtod("2.5e3", 0) == 2500.0, "strtod");

	int v[] = { 5, 3, 9, 1, 7 };
	qsort(v, 5, sizeof v[0], cmp);
	check(v[0] == 1 && v[4] == 9, "qsort");

	/* The main thread's stack, as the start code recorded it. */
	pthread_attr_t sa;
	void *stack_base = 0;
	size_t stack_size = 0;
	int local = 0;
	check(pthread_getattr_np(pthread_self(), &sa) == 0 &&
	      pthread_attr_getstack(&sa, &stack_base, &stack_size) == 0 &&
	      (char *)&local > (char *)stack_base &&
	      (char *)&local < (char *)stack_base + stack_size &&
	      stack_size == (1 << 20), "main thread stack bounds");

	char name[16] = { 0 };
	check(pthread_setname_np(pthread_self(), "libc-main") == 0 &&
	      pthread_getname_np(pthread_self(), name, sizeof name) == 0 &&
	      strcmp(name, "libc-main") == 0, "thread names");

	/* Pipes: in-process objects; poll blocks until another thread writes. */
	int pp[2];
	char byte = 0;
	check(pipe2(pp, O_CLOEXEC | O_NONBLOCK) == 0 &&
	      read(pp[0], &byte, 1) < 0 && errno == EAGAIN, "pipe2, empty non-blocking read");
	pthread_t writer;
	struct pollfd pw = { pp[0], POLLIN, 0 };
	pthread_create(&writer, 0, late_writer, &pp[1]);
	int polled = poll(&pw, 1, -1);
	pthread_join(writer, 0);
	check(polled == 1 && (pw.revents & POLLIN) && read(pp[0], &byte, 1) == 1 &&
	      byte == 'w', "poll wakes when another thread writes a pipe");
	close(pp[1]);
	pw.revents = 0;
	check(poll(&pw, 1, 0) == 1 && (pw.revents & POLLHUP) &&
	      read(pp[0], &byte, 1) == 0 && close(pp[0]) == 0, "pipe end of file");

	int sv[2];
	char two[2] = { 0 };
	check(socketpair(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0, sv) == 0 &&
	      send(sv[0], "ab", 2, 0) == 2 && read(sv[1], two, 2) == 2 &&
	      two[0] == 'a' && write(sv[1], "z", 1) == 1 &&
	      recv(sv[0], two, 1, 0) == 1 && two[0] == 'z' &&
	      close(sv[0]) == 0 && read(sv[1], two, 1) == 0 && close(sv[1]) == 0,
	      "socketpair both ways and end of file");

	int dp[2], copy;
	check(pipe(dp) == 0 && (copy = fcntl(dp[1], F_DUPFD_CLOEXEC, 0)) >= 0 && copy != dp[1] &&
	      close(dp[1]) == 0 && write(copy, "d", 1) == 1 &&
	      read(dp[0], &byte, 1) == 1 && byte == 'd' && close(copy) == 0 &&
	      read(dp[0], &byte, 1) == 0 && close(dp[0]) == 0,
	      "dup shares the pipe; end of file after the last writer");

	/* Files through filesystem.svc, inside the manifest's scope. */
	struct stat st;
	check(stat("/tls/roots.der", &st) == 0 && S_ISREG(st.st_mode) &&
	      st.st_size > 64, "stat a file in scope");
	int fd = open("/tls/roots.der", O_RDONLY | O_CLOEXEC);
	unsigned char head[4] = { 0 }, again[4] = { 0 };
	check(fd >= 3 && read(fd, head, 4) == 4 && head[0] == 0x30,
	      "open and read (DER begins with a SEQUENCE)");
	check(lseek(fd, 0, SEEK_SET) == 0 && read(fd, again, 4) == 4 &&
	      memcmp(head, again, 4) == 0 &&
	      pread(fd, again, 2, 1) == 2 && again[0] == head[1] &&
	      lseek(fd, 0, SEEK_END) == st.st_size &&
	      read(fd, again, 4) == 0, "lseek, pread and end of file");
	struct stat fst;
	check(fstat(fd, &fst) == 0 && fst.st_size == st.st_size, "fstat");
	unsigned char *mapped = mmap(0, (size_t)st.st_size, PROT_READ,
		MAP_PRIVATE, fd, 0);
	check(mapped != MAP_FAILED && memcmp(mapped, head, 4) == 0, "mmap a file");
	check(munmap(mapped, (size_t)st.st_size) == 0, "release private file copy");
	check(close(fd) == 0 && read(fd, head, 1) < 0 && errno == EBADF,
	      "close");
	FILE *f = fopen("/tls/roots.der", "rb");
	check(f && fgetc(f) == 0x30 && fclose(f) == 0, "stdio fopen");
	DIR *dir = opendir("/tls");
	int found = 0;
	struct dirent *ent;
	while (dir && (ent = readdir(dir)))
		if (!strcmp(ent->d_name, "roots.der") && ent->d_type == DT_REG)
			found = 1;
	check(dir && found && closedir(dir) == 0, "opendir and readdir");
	check(open("/motd.txt", O_RDONLY) < 0 && errno == EACCES,
	      "a file outside the scope is denied");
	check(open("/tls/roots.der", O_WRONLY) < 0 && errno == EACCES,
	      "writing needs a write scope");

	/* File times (docs/self-hosting.md: make needs real mtimes): a write
	 * sets mtime and ctime to the wall clock; the volume's inode number,
	 * type and link count come through fstat and stat. */
	if (mkdir("/libc-check", 0755) < 0 && errno != EEXIST)
		check(0, "make the writable directory");
	int wfd = open("/libc-check/times", O_CREAT | O_TRUNC | O_RDWR | O_CLOEXEC, 0644);
	struct timespec before, after;
	struct stat wst, nst;
	clock_gettime(CLOCK_REALTIME, &before);
	check(wfd >= 3 && write(wfd, "stamp", 5) == 5, "create and write a file");
	clock_gettime(CLOCK_REALTIME, &after);
	check(fstat(wfd, &wst) == 0 && S_ISREG(wst.st_mode) && wst.st_size == 5 &&
	      wst.st_nlink == 1 && wst.st_ino > 2, "fstat: type, size, links, inode");
	/* Whole seconds on ext2: the write's second, within the call. */
	check(wst.st_mtim.tv_sec >= before.tv_sec && wst.st_mtim.tv_sec <= after.tv_sec &&
	      wst.st_ctim.tv_sec == wst.st_mtim.tv_sec,
	      "a write sets mtime and ctime to the wall clock");
	say("libc-check: mtime %ld realtime %ld..%ld\n", (long)wst.st_mtim.tv_sec,
	    (long)before.tv_sec, (long)after.tv_sec);
	check(close(wfd) == 0 && stat("/libc-check/times", &nst) == 0 &&
	      nst.st_ino == wst.st_ino && nst.st_mtim.tv_sec == wst.st_mtim.tv_sec &&
	      nst.st_size == 5, "stat by name agrees with fstat");
	check(stat("/libc-check", &nst) == 0 && S_ISDIR(nst.st_mode) &&
	      nst.st_mtim.tv_sec >= before.tv_sec, "the directory's mtime follows its entries");
	char link[8];
	check(readlink("/libc-check/times", link, sizeof link) < 0 && errno == EINVAL &&
	      readlink("/libc-check/absent", link, sizeof link) < 0 && errno == ENOENT,
	      "readlink: no symbolic links (EINVAL), missing names ENOENT");
	char *real = realpath("/libc-check/times", 0);
	check(real && !strcmp(real, "/libc-check/times"), "realpath");
	free(real);
	check(access("/libc-check/times", R_OK | W_OK) == 0 &&
	      access("/libc-check", W_OK) == 0 &&
	      access("/tls/roots.der", R_OK) == 0 &&
	      access("/tls/roots.der", W_OK) < 0 && errno == EACCES,
	      "access: W_OK follows the write scope");
	int tfd = open("/libc-check/times", O_RDWR | O_CLOEXEC);
	char grown[9] = { 1 };
	check(tfd >= 3 && ftruncate(tfd, 2) == 0 && fstat(tfd, &nst) == 0 && nst.st_size == 2 &&
	      ftruncate(tfd, 9) == 0 && pread(tfd, grown, 9, 0) == 9 &&
	      !memcmp(grown, "st\0\0\0\0\0\0\0", 9) && close(tfd) == 0 &&
	      truncate("/libc-check/times", 0) == 0 && stat("/libc-check/times", &nst) == 0 &&
	      nst.st_size == 0, "ftruncate and truncate shrink and zero-fill");
	int sp[2];
	fd_set rset, wset;
	struct timeval zero = { 0, 0 }, wait10 = { 0, 10000 };
	check(pipe(sp) == 0, "pipe for select");
	FD_ZERO(&rset); FD_SET(sp[0], &rset);
	FD_ZERO(&wset); FD_SET(sp[1], &wset);
	check(select(sp[1] + 1, &rset, &wset, 0, &zero) == 1 &&
	      !FD_ISSET(sp[0], &rset) && FD_ISSET(sp[1], &wset),
	      "select: empty pipe writable, not readable");
	FD_ZERO(&rset); FD_SET(sp[0], &rset);
	check(write(sp[1], "s", 1) == 1 && select(sp[0] + 1, &rset, 0, 0, &wait10) == 1 &&
	      FD_ISSET(sp[0], &rset) && close(sp[0]) == 0 && close(sp[1]) == 0,
	      "select: readable after a write");
	check(unlink("/libc-check/times") == 0, "remove the file");

	/* Working directory and directory-relative names (self-hosting
	 * item 2): relative names start from the working directory, ".."
	 * leaves it, and the *at calls start from an open directory. */
	char here[64];
	check(getcwd(here, sizeof here) && !strcmp(here, "/"), "getcwd starts at /");
	check(chdir("/libc-check") == 0 && getcwd(here, sizeof here) &&
	      !strcmp(here, "/libc-check"), "chdir and getcwd");
	int rfd = open("relative", O_CREAT | O_TRUNC | O_WRONLY | O_CLOEXEC, 0644);
	check(rfd >= 3 && write(rfd, "r", 1) == 1 && close(rfd) == 0 &&
	      stat("/libc-check/relative", &nst) == 0 && nst.st_size == 1,
	      "a relative name starts from the working directory");
	check(stat("../tls/roots.der", &nst) == 0 && S_ISREG(nst.st_mode) &&
	      stat("./../libc-check/./relative", &nst) == 0,
	      "dot and dot-dot components");
	check(chdir("relative") < 0 && errno == ENOTDIR &&
	      chdir("absent") < 0 && errno == ENOENT &&
	      getcwd(here, sizeof here) && !strcmp(here, "/libc-check"),
	      "chdir refuses files and missing names and stays put");
	int dfd = open("/libc-check", O_RDONLY | O_DIRECTORY | O_CLOEXEC);
	check(dfd >= 3 && mkdirat(dfd, "sub", 0755) == 0 &&
	      (rfd = openat(dfd, "sub/inner", O_CREAT | O_WRONLY | O_CLOEXEC, 0644)) >= 3 &&
	      close(rfd) == 0 && fstatat(dfd, "sub/inner", &nst, 0) == 0 &&
	      faccessat(dfd, "sub/inner", W_OK, 0) == 0 &&
	      renameat(dfd, "sub/inner", dfd, "sub/moved") == 0 &&
	      unlinkat(dfd, "sub/moved", 0) == 0 && unlinkat(dfd, "sub", AT_REMOVEDIR) == 0,
	      "mkdirat, openat, fstatat, faccessat, renameat and unlinkat");
	check(chdir("/") < 0 && errno == EACCES && getcwd(here, sizeof here) &&
	      !strcmp(here, "/libc-check"),
	      "chdir to a directory it may not read is refused");
	check(chdir("/tls") == 0 && fchdir(dfd) == 0 && getcwd(here, sizeof here) &&
	      !strcmp(here, "/libc-check") && close(dfd) == 0, "fchdir");
	check(openat(1, "x", O_RDONLY) < 0 && errno == ENOTDIR &&
	      openat(999, "x", O_RDONLY) < 0 && errno == EBADF,
	      "openat with a non-directory or closed descriptor");
	check(unlink("relative") == 0 && stat("/../../tls/roots.der", &nst) == 0 &&
	      stat("../../../tls/roots.der", &nst) == 0, "dot-dot stops at the root");

	say("%s\n", failures ? "LIBC: FAIL" : "LIBC: PASS");
	return failures != 0;
}
