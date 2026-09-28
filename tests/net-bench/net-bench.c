/*
 * CuBit network benchmark client. The same source runs on CuBit (musl over
 * netstack) and on Linux, each in the same QEMU configuration, against
 * tests/net-bench/server.py on the host (10.0.2.2 through QEMU user
 * networking). Sockets only; no platform-specific calls.
 *
 * Workloads, each repeated ROUNDS times:
 *   download  receive DOWNLOAD_BYTES from the host (port 18480)
 *   upload    send UPLOAD_BYTES to the host, which acknowledges (18481)
 *   rr        one-byte request/response round trips (18482)
 *   connect   connect, receive one byte, close (18483)
 *   serve     the guest as server: listen on port 8080, tell the host
 *             (18484) to connect in through QEMU's forward (host port
 *             18486), accept SERVE_CONNECTS connections sending one byte
 *             each, then send SERVE_BYTES on one more
 */
#include <arpa/inet.h>
#include <errno.h>
#include <netinet/in.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <time.h>
#include <unistd.h>
#ifdef CUBIT
#include <cubit/debug.h>
/* The kernel's scheduling trace around one round-trip run (CuBit only):
 * reset before, summary (ready-to-run latency histogram) after. */
static inline void trace_op(unsigned long n, unsigned long a)
{
	unsigned long ret;
	__asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(n), "D"(a)
		: "rcx", "r11", "memory");
	(void)ret;
}
#define TRACE_RESET() trace_op(82, 0)
#define TRACE_SUMMARY() trace_op(83, 0)
/* Event timeline of a few round trips (kernels built with LATENCY_TRACE=1). */
#define TRACE_TIMELINE_START() trace_op(83, 1)
#define TRACE_TIMELINE_FREEZE() trace_op(83, 2)
#define TRACE_TIMELINE_DUMP() trace_op(83, 3)
#else
#define TRACE_TIMELINE_START() ((void)0)
#define TRACE_TIMELINE_FREEZE() ((void)0)
#define TRACE_TIMELINE_DUMP() ((void)0)
#define TRACE_RESET() ((void)0)
#define TRACE_SUMMARY() ((void)0)
#endif

#define HOST "10.0.2.2"
#define ROUNDS 3
#define DOWNLOAD_BYTES (64ull << 20)
#define UPLOAD_BYTES (64ull << 20)
#define ROUND_TRIPS 2000
#ifndef CONNECTS
#define CONNECTS 200
#endif
#define SERVE_PORT 8080
#define SERVE_CONNECTS 200
#define SERVE_BYTES (64ull << 20)

static unsigned char buf[1 << 16];

/* Results go to the serial console: on CuBit the debug console (stdout is
 * a stream with no subscriber here), on Linux stdout (the console). */
static void report(const char *fmt, ...)
{
	char line[256];
	va_list ap;
	va_start(ap, fmt);
	int n = vsnprintf(line, sizeof line, fmt, ap);
	va_end(ap);
	if (n < 0)
		return;
	if ((size_t)n >= sizeof line)
		n = sizeof line - 1;
#ifdef CUBIT
	cubit_debug_write(line, (size_t)n);
#else
	fputs(line, stdout);
	fflush(stdout);
#endif
}

static double now_ms(void)
{
	struct timespec t;
	clock_gettime(CLOCK_MONOTONIC, &t);
	return t.tv_sec * 1e3 + t.tv_nsec / 1e6;
}

static int dial(int port)
{
	struct sockaddr_in a;
	memset(&a, 0, sizeof a);
	a.sin_family = AF_INET;
	a.sin_port = htons(port);
	inet_pton(AF_INET, HOST, &a.sin_addr);
	int fd = socket(AF_INET, SOCK_STREAM, 0);
	if (fd < 0) {
		report("net-bench: socket errno=%d\n", errno);
		return -1;
	}
	if (connect(fd, (struct sockaddr *)&a, sizeof a) < 0) {
		report("net-bench: connect port=%d errno=%d\n", port, errno);
		close(fd);
		return -1;
	}
	return fd;
}

static int send_all(int fd, const void *p, size_t n)
{
	const unsigned char *c = p;
	while (n) {
		ssize_t k = send(fd, c, n, 0);
		if (k <= 0)
			return -1;
		c += k;
		n -= (size_t)k;
	}
	return 0;
}

static int recv_all(int fd, void *p, size_t n)
{
	unsigned char *c = p;
	while (n) {
		ssize_t k = recv(fd, c, n, 0);
		if (k <= 0)
			return -1;
		c += k;
		n -= (size_t)k;
	}
	return 0;
}

static void header(unsigned char h[8], uint64_t v)
{
	for (int i = 0; i < 8; i++)
		h[i] = (unsigned char)(v >> (8 * i));
}

static int download(int round)
{
	unsigned char h[8];
	int fd = dial(18480);
	if (fd < 0)
		return -1;
	header(h, DOWNLOAD_BYTES);
	double t0 = now_ms();
	if (send_all(fd, h, 8) < 0) {
		close(fd);
		return -1;
	}
	uint64_t got = 0;
	int traced = -1;   /* round 1: a traced snapshot mid-transfer */
	while (got < DOWNLOAD_BYTES) {
		ssize_t k = recv(fd, buf, sizeof buf, 0);
		if (k <= 0)
			break;
		got += (uint64_t)k;
		if (round == 1 && traced < 0 && got >= DOWNLOAD_BYTES / 2) {
			TRACE_TIMELINE_START();
			traced = 0;
		} else if (traced >= 0 && traced < 8 && ++traced == 8) {
			TRACE_TIMELINE_FREEZE();
		}
	}
	double ms = now_ms() - t0;
	if (traced >= 0) TRACE_TIMELINE_DUMP();
	close(fd);
	if (got != DOWNLOAD_BYTES)
		return -1;
	report("net-bench: download round=%d bytes=%llu ms=%.0f Mbps=%.1f\n", round,
	       (unsigned long long)got, ms, got * 8 / (ms * 1e3));
	return 0;
}

static int upload(int round)
{
	unsigned char h[8];
	int fd = dial(18481);
	if (fd < 0)
		return -1;
	header(h, UPLOAD_BYTES);
	for (size_t i = 0; i < sizeof buf; i++)
		buf[i] = (unsigned char)i;
	double t0 = now_ms();
	if (send_all(fd, h, 8) < 0) {
		close(fd);
		return -1;
	}
	uint64_t sent = 0;
	while (sent < UPLOAD_BYTES) {
		size_t n = sizeof buf;
		if (UPLOAD_BYTES - sent < n)
			n = (size_t)(UPLOAD_BYTES - sent);
		if (send_all(fd, buf, n) < 0) {
			close(fd);
			return -1;
		}
		sent += n;
	}
	/* The host acknowledges once every byte has arrived. */
	int ok = recv_all(fd, h, 8);
	double ms = now_ms() - t0;
	close(fd);
	if (ok < 0)
		return -1;
	report("net-bench: upload round=%d bytes=%llu ms=%.0f Mbps=%.1f\n", round,
	       (unsigned long long)sent, ms, sent * 8 / (ms * 1e3));
	return 0;
}

static int round_trips(int round)
{
	unsigned char c = 'x';
	int fd = dial(18482);
	if (fd < 0)
		return -1;
	double t0 = now_ms();
	for (int i = 0; i < ROUND_TRIPS; i++) {
		if (send_all(fd, &c, 1) < 0 || recv_all(fd, &c, 1) < 0) {
			close(fd);
			return -1;
		}
	}
	double ms = now_ms() - t0;
	close(fd);
	report("net-bench: rr round=%d trips=%d ms=%.0f us_per_trip=%.1f\n", round,
	       ROUND_TRIPS, ms, ms * 1e3 / ROUND_TRIPS);
	return 0;
}

static int connects(int round)
{
	unsigned char c;
	double t0 = now_ms();
	for (int i = 0; i < CONNECTS; i++) {
		int fd = dial(18483);
		if (fd < 0)
			return -1;
		int ok = recv_all(fd, &c, 1);
		close(fd);
		if (ok < 0)
			return -1;
	}
	double ms = now_ms() - t0;
	report("net-bench: connect round=%d connections=%d ms=%.0f per_second=%.0f\n",
	       round, CONNECTS, ms, CONNECTS * 1e3 / ms);
	return 0;
}

/* The guest as a server: every connection accepted through listen. */
static int serve(int listener, int round)
{
	unsigned char h[16], c = 'x';
	header(h, SERVE_CONNECTS);
	header(h + 8, SERVE_BYTES);
	int ready = dial(18484);
	if (ready < 0 || send_all(ready, h, sizeof h) < 0) {
		if (ready >= 0) close(ready);
		return -1;
	}
	double t0 = now_ms();
	for (int i = 0; i < SERVE_CONNECTS; i++) {
		int fd = accept(listener, 0, 0);
		if (fd < 0) {
			report("net-bench: accept errno=%d\n", errno);
			close(ready);
			return -1;
		}
		int ok = send_all(fd, &c, 1);
		close(fd);
		if (ok < 0) { close(ready); return -1; }
	}
	double ms = now_ms() - t0;
	report("net-bench: serve-accept round=%d connections=%d ms=%.0f per_second=%.0f\n",
	       round, SERVE_CONNECTS, ms, SERVE_CONNECTS * 1e3 / ms);
	int fd = accept(listener, 0, 0);
	if (fd < 0) { close(ready); return -1; }
	t0 = now_ms();
	for (uint64_t left = SERVE_BYTES; left; ) {
		size_t n = left < sizeof buf ? (size_t)left : sizeof buf;
		if (send_all(fd, buf, n) < 0) { close(fd); close(ready); return -1; }
		left -= n;
	}
	/* Delivered when the host, having read it all, closes. */
	shutdown(fd, SHUT_WR);
	while (recv(fd, &c, 1, 0) > 0) {}
	ms = now_ms() - t0;
	close(fd);
	close(ready);
	report("net-bench: serve-download round=%d bytes=%llu ms=%.0f mbit_per_s=%.0f\n",
	       round, (unsigned long long)SERVE_BYTES, ms, SERVE_BYTES * 8 / (ms * 1e3));
	return 0;
}

static int open_listener(void)
{
	int one = 1;
	struct sockaddr_in a;
	memset(&a, 0, sizeof a);
	a.sin_family = AF_INET;
	a.sin_port = htons(SERVE_PORT);
	a.sin_addr.s_addr = htonl(INADDR_ANY);
	int fd = socket(AF_INET, SOCK_STREAM, 0);
	if (fd < 0) return -1;
	setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one);
	if (bind(fd, (struct sockaddr *)&a, sizeof a) < 0 || listen(fd, 16) < 0) {
		report("net-bench: listen errno=%d\n", errno);
		close(fd);
		return -1;
	}
	return fd;
}

int main(void)
{
	int failures = 0;
	report("net-bench: start\n");
	int listener = open_listener();
	if (listener < 0) failures++;
	for (int r = 1; r <= ROUNDS; r++) {
#ifndef CONNECT_ONLY   /* -DCONNECT_ONLY: just the connection workload (debugging) */
		if (download(r) < 0) { report("net-bench: download FAIL\n"); failures++; }
		if (upload(r) < 0) { report("net-bench: upload FAIL\n"); failures++; }
		if (r == 1)
			TRACE_RESET();
		if (round_trips(r) < 0) { report("net-bench: rr FAIL\n"); failures++; }
		if (r == 1)
			TRACE_SUMMARY();
		if (r == 1) {
			/* A short traced burst after warm-up. */
			int fd = dial(18482);
			unsigned char c = 'x';
			if (fd >= 0) {
				for (int i = 0; i < 20; i++) {
					send_all(fd, &c, 1);
					recv_all(fd, &c, 1);
				}
				TRACE_TIMELINE_START();
				for (int i = 0; i < 5; i++) {
					send_all(fd, &c, 1);
					recv_all(fd, &c, 1);
				}
				TRACE_TIMELINE_FREEZE();
				TRACE_TIMELINE_DUMP();
				close(fd);
			}
		}
#endif
		if (connects(r) < 0) { report("net-bench: connect FAIL\n"); failures++; }
		if (listener >= 0 && serve(listener, r) < 0) {
			report("net-bench: serve FAIL\n");
			failures++;
		}
	}
	if (listener >= 0) close(listener);
	report(failures ? "net-bench: FAIL\n" : "net-bench: done\n");
	return failures != 0;
}
