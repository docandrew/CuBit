/* Native checks for the CuBit libc (userspace/libc): stdio, malloc,
 * pthreads, thread-local storage, time and formatting. */
#define _GNU_SOURCE
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <errno.h>
#include <sys/mman.h>
#include <sys/stat.h>
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
	for (long i = 0; i < 8; i++) pthread_create(&t[i], 0, worker, (void *)(i + 1));
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
	check(open("/tls/roots.der", O_WRONLY) < 0 && errno == EROFS,
	      "files are read-only for now");

	say("%s\n", failures ? "LIBC: FAIL" : "LIBC: PASS");
	return failures != 0;
}
