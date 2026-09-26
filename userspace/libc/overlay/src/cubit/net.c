/*
 * CuBit libc: TCP sockets over netstack (docs/servo-port.md).
 *
 * A socket is a netstack channel reached through the program's network
 * endpoint (fixed slot 11), which exists only if the manifest requested
 * network access and the launch approved it; netstack checks every
 * connection against that scope. Nothing here adds authority.
 *
 * Names are not resolved here (lookup_name.c): a host name gets a
 * placeholder IPv4 address, and connecting to it opens
 * "@net:tcp:<name>:<port>", so netstack resolves the name inside the scope.
 *
 * Each socket lends netstack one 64 KiB buffer: the first half receives,
 * the second half sends. A reader thread per socket opens the channel and
 * keeps one READ outstanding whenever the program has consumed the previous
 * one (the netstack reply is deferred until data or end of stream);
 * readiness goes through the descriptor futex (fd.c). Writers send
 * synchronously; replies are thread-addressed, so reads and writes from
 * different threads do not interfere.
 *
 * First version: one thread per socket; no listening sockets, no UDP.
 */
#define _GNU_SOURCE
#include <arpa/inet.h>
#include <errno.h>
#include <netinet/in.h>
#include <poll.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <sys/uio.h>
#include "lock.h"
#include "cubit_fd.h"

#define SLOT_NETWORK 11

enum {
	SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY = 41,
	SYSCALL_REVOKE_SHARED_MEMORY_GRANT = 103,
	SYSCALL_CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY = 106,
	SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION = 108,
};

enum { OP_NET_OPEN = 0x0420, OP_NET_WRITE = 0x0421, OP_NET_READ = 0x0422,
       OP_NET_SHUT = 0x0423 };
enum { REPLY_OK = 0xF000, REPLY_EOF = 0xF006 };

#define BUF_PAGES 16
#define BUF_BYTES (BUF_PAGES * 4096UL)
#define RX_BYTES (BUF_BYTES / 2)
#define TX_OFFSET RX_BYTES
#define TX_BYTES (BUF_BYTES - RX_BYTES)

struct message {
	uint32_t label;
	uint8_t length, flags;
	uint16_t reserved;
	uint64_t authority;
	uint64_t words[4];
};

static inline unsigned long cubit(unsigned long n, unsigned long a,
	unsigned long b, unsigned long c, unsigned long d)
{
	unsigned long ret;
	register unsigned long r10 __asm__("r10") = d;
	__asm__ __volatile__ ("syscall" : "=a"(ret)
		: "a"(n), "D"(a), "S"(b), "d"(c), "r"(r10)
		: "rcx", "r11", "memory");
	return ret;
}

static uint32_t call(uint32_t label, uint8_t length, uint64_t w0, uint64_t w1,
	uint64_t w2, uint64_t w3, uint64_t *reply0)
{
	struct message m = { label, length, 0, 0, 0, { w0, w1, w2, w3 } };
	unsigned long tag = cubit(SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY,
		SLOT_NETWORK, (unsigned long)&m, 0, 0);
	if (tag == (unsigned long)-1) return 0;
	if (reply0) *reply0 = m.words[0];
	return (uint32_t)tag;
}

/* --- names ----------------------------------------------------------------- */

/* Placeholder addresses 100.100.0.1 .. 100.100.255.255 stand for names. */
#define NAME_NET 0x64640000u
#define MAX_NAMES 65535

static volatile int names_lock[1];
static char **names;
static unsigned names_count;

hidden uint32_t __cubit_name_address(const char *name)
{
	uint32_t index = 0;
	LOCK(names_lock);
	for (unsigned i = 0; i < names_count; i++)
		if (!strcasecmp(names[i], name)) { index = i + 1; break; }
	if (!index && names_count < MAX_NAMES) {
		if (!(names_count & 63)) {
			char **grown = realloc(names, (names_count + 64) * sizeof *names);
			if (grown) names = grown;
			else { UNLOCK(names_lock); return 0; }
		}
		char *copy = strdup(name);
		if (copy) {
			names[names_count++] = copy;
			index = names_count;
		}
	}
	UNLOCK(names_lock);
	return index ? htonl(NAME_NET | index) : 0;
}

/* The name a placeholder address stands for, or NULL. */
static const char *address_name(uint32_t be)
{
	uint32_t a = ntohl(be);
	if ((a & 0xFFFF0000u) != NAME_NET || !(a & 0xFFFF)) return 0;
	const char *name = 0;
	LOCK(names_lock);
	if ((a & 0xFFFF) <= names_count) name = names[(a & 0xFFFF) - 1];
	UNLOCK(names_lock);
	return name;
}

/* --- sockets -------------------------------------------------------------- */

enum tcp_state { TCP_IDLE, TCP_CONNECTING, TCP_OPEN, TCP_FAILED };

struct cubit_tcp {
	volatile int lock[1];
	volatile int write_lock[1];
	enum tcp_state state;
	int error;                      /* pending SO_ERROR */
	int eof;                        /* no more data will arrive */
	int closing, shut_sent, write_shut;
	int refs;                       /* the descriptor and the reader */
	uint64_t channel;
	unsigned char *buf;
	uint64_t slot, generation;
	size_t rx_len, rx_off;          /* unread bytes in buf[rx_off, rx_len) */
	struct sockaddr_in peer;
	char target[300];
	size_t target_len;
};

static void release(struct cubit_tcp *t)
{
	LOCK(t->lock);
	int last = --t->refs == 0;
	UNLOCK(t->lock);
	if (!last) return;
	if (t->slot) cubit(SYSCALL_REVOKE_SHARED_MEMORY_GRANT, t->slot, 0, 0, 0);
	/* The buffer came from the heap-growing mmap; it is not returned. */
	free(t);
}

static void send_shut(struct cubit_tcp *t)
{
	LOCK(t->lock);
	int send = t->channel && !t->shut_sent;
	t->shut_sent = 1;
	uint64_t channel = t->channel;
	UNLOCK(t->lock);
	if (send) call(OP_NET_SHUT, 3, channel, 0, 0, 0, 0);
}

static void *reader(void *arg)
{
	struct cubit_tcp *t = arg;
	uint64_t value = 0;

	memcpy(t->buf, t->target, t->target_len);
	uint32_t label = call(OP_NET_OPEN, (uint8_t)t->target_len, t->slot,
		BUF_BYTES, t->target_len, t->generation, &value);
	LOCK(t->lock);
	if (label == REPLY_OK) {
		t->channel = value;
		t->state = TCP_OPEN;
	} else {
		t->state = TCP_FAILED;
		t->error = ECONNREFUSED;
		t->eof = 1;
	}
	int closing = t->closing;
	UNLOCK(t->lock);
	__cubit_readiness_changed();
	if (label == REPLY_OK && closing) send_shut(t);

	while (label == REPLY_OK) {
		/* Wait until the program has consumed the previous read. */
		for (;;) {
			int seq = __cubit_readiness_seq();
			LOCK(t->lock);
			int done = t->closing;
			int consumed = t->rx_off >= t->rx_len;
			UNLOCK(t->lock);
			if (done) goto out;
			if (consumed) break;
			__cubit_readiness_wait(seq, ~0UL);
		}
		label = call(OP_NET_READ, 3, t->channel, 0, RX_BYTES, 0, &value);
		LOCK(t->lock);
		if (label == REPLY_OK && value <= RX_BYTES) {
			t->rx_off = 0;
			t->rx_len = (size_t)value;
		} else {
			if (label != REPLY_EOF && !t->closing) t->error = ECONNRESET;
			t->eof = 1;
			label = 0;
		}
		UNLOCK(t->lock);
		__cubit_readiness_changed();
	}
out:
	release(t);
	return 0;
}

hidden struct cubit_tcp *__cubit_tcp_new(void)
{
	struct cubit_tcp *t = calloc(1, sizeof *t);
	if (t) t->refs = 1;
	return t;
}

hidden long __cubit_tcp_connect(struct cubit_tcp *t, const struct sockaddr *sa,
	socklen_t len, int nonblock)
{
	if (!sa || len < sizeof(struct sockaddr_in)) return -EINVAL;
	if (sa->sa_family != AF_INET) return -EAFNOSUPPORT;
	LOCK(t->lock);
	enum tcp_state state = t->state;
	UNLOCK(t->lock);
	if (state == TCP_CONNECTING) return -EALREADY;
	if (state == TCP_OPEN) return -EISCONN;
	if (state == TCP_FAILED) return -t->error;

	const struct sockaddr_in *sin = (const void *)sa;
	const char *name = address_name(sin->sin_addr.s_addr);
	char host[INET_ADDRSTRLEN];
	if (!name) name = inet_ntop(AF_INET, &sin->sin_addr, host, sizeof host);
	int n = snprintf(t->target, sizeof t->target, "@net:tcp:%s:%u",
		name, (unsigned)ntohs(sin->sin_port));
	if (n <= 0 || n > 255) return -ENAMETOOLONG;
	t->target_len = (size_t)n;
	t->peer = *sin;

	/* The buffer lent to netstack; fails without a network capability. */
	void *buf = mmap(0, BUF_BYTES, PROT_READ | PROT_WRITE,
		MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	if (buf == MAP_FAILED) return -ENOMEM;
	unsigned long slot = cubit(SYSCALL_CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY,
		SLOT_NETWORK, (unsigned long)buf, BUF_PAGES, 1);
	if (slot == (unsigned long)-1) return -EACCES;
	unsigned long gen = cubit(SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION,
		slot, 0, 0, 0);
	if (gen == (unsigned long)-1 || !gen) {
		cubit(SYSCALL_REVOKE_SHARED_MEMORY_GRANT, slot, 0, 0, 0);
		return -EACCES;
	}
	t->buf = buf;
	t->slot = slot;
	t->generation = gen;

	LOCK(t->lock);
	t->state = TCP_CONNECTING;
	t->refs++;
	UNLOCK(t->lock);
	pthread_attr_t attr;
	pthread_t thread;
	pthread_attr_init(&attr);
	pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
	pthread_attr_setstacksize(&attr, 64 * 1024);
	int r = pthread_create(&thread, &attr, reader, t);
	pthread_attr_destroy(&attr);
	if (r) {
		LOCK(t->lock);
		t->state = TCP_FAILED;
		t->error = r;
		t->refs--;
		UNLOCK(t->lock);
		return -r;
	}
	if (nonblock) return -EINPROGRESS;
	for (;;) {
		int seq = __cubit_readiness_seq();
		LOCK(t->lock);
		state = t->state;
		UNLOCK(t->lock);
		if (state == TCP_OPEN) return 0;
		if (state == TCP_FAILED) return -t->error;
		__cubit_readiness_wait(seq, ~0UL);
	}
}

hidden long __cubit_tcp_read(struct cubit_tcp *t, void *out, size_t n, int nonblock)
{
	for (;;) {
		int seq = __cubit_readiness_seq();
		LOCK(t->lock);
		if (t->state == TCP_IDLE) { UNLOCK(t->lock); return -ENOTCONN; }
		if (t->rx_off < t->rx_len) {
			size_t got = t->rx_len - t->rx_off;
			if (got > n) got = n;
			memcpy(out, t->buf + t->rx_off, got);
			t->rx_off += got;
			int drained = t->rx_off >= t->rx_len;
			UNLOCK(t->lock);
			if (drained) __cubit_readiness_changed();   /* the reader reads on */
			return (long)got;
		}
		if (t->eof) {
			int err = t->error;
			t->error = 0;
			UNLOCK(t->lock);
			return err && err != ECONNREFUSED ? -err : 0;
		}
		UNLOCK(t->lock);
		if (!n) return 0;
		if (nonblock) return -EAGAIN;
		__cubit_readiness_wait(seq, ~0UL);
	}
}

hidden long __cubit_tcp_write(struct cubit_tcp *t, const struct iovec *iov, int n,
	int nonblock)
{
	for (;;) {
		int seq = __cubit_readiness_seq();
		LOCK(t->lock);
		enum tcp_state state = t->state;
		int shut = t->write_shut || t->closing;
		UNLOCK(t->lock);
		if (state == TCP_IDLE) return -ENOTCONN;
		if (state == TCP_FAILED || shut) return -EPIPE;
		if (state == TCP_OPEN) break;
		if (nonblock) return -EAGAIN;
		__cubit_readiness_wait(seq, ~0UL);
	}
	long total = 0;
	LOCK(t->write_lock);
	for (int i = 0; i < n; i++) {
		const unsigned char *p = iov[i].iov_base;
		size_t left = iov[i].iov_len;
		while (left) {
			size_t chunk = left > TX_BYTES ? TX_BYTES : left;
			uint64_t sent = 0;
			memcpy(t->buf + TX_OFFSET, p, chunk);
			uint32_t label = call(OP_NET_WRITE, 3, t->channel, TX_OFFSET,
				chunk, 0, &sent);
			if (label != REPLY_OK || sent != chunk) {
				UNLOCK(t->write_lock);
				return total ? total : -EPIPE;
			}
			p += chunk;
			left -= chunk;
			total += (long)chunk;
		}
	}
	UNLOCK(t->write_lock);
	return total;
}

hidden short __cubit_tcp_poll(struct cubit_tcp *t, short events)
{
	short re = 0;
	LOCK(t->lock);
	switch (t->state) {
	case TCP_IDLE:
		re = POLLHUP;
		break;
	case TCP_CONNECTING:
		break;
	case TCP_OPEN:
		if (!t->write_shut) re |= events & (POLLOUT | POLLWRNORM);
		if (t->rx_off < t->rx_len || t->eof)
			re |= events & (POLLIN | POLLRDNORM);
		if (t->eof) re |= POLLRDHUP & events;
		if (t->eof && t->error) re |= POLLERR | POLLHUP;
		break;
	case TCP_FAILED:
		re = POLLERR | POLLHUP | (events & (POLLOUT | POLLIN));
		break;
	}
	UNLOCK(t->lock);
	return re;
}

hidden long __cubit_tcp_so_error(struct cubit_tcp *t)
{
	LOCK(t->lock);
	int err = t->state == TCP_FAILED ? t->error : 0;
	UNLOCK(t->lock);
	return err;
}

hidden long __cubit_tcp_peer(struct cubit_tcp *t, struct sockaddr *sa, socklen_t *len)
{
	LOCK(t->lock);
	enum tcp_state state = t->state;
	UNLOCK(t->lock);
	if (state != TCP_OPEN) return -ENOTCONN;
	socklen_t n = *len < sizeof t->peer ? *len : sizeof t->peer;
	memcpy(sa, &t->peer, n);
	*len = sizeof t->peer;
	return 0;
}

hidden long __cubit_tcp_shutdown(struct cubit_tcp *t, int how)
{
	if (how == SHUT_WR || how == SHUT_RDWR) {
		LOCK(t->lock);
		t->write_shut = 1;
		UNLOCK(t->lock);
	}
	/* netstack's SHUT releases the whole channel; a half-close stays local
	 * until close (HTTP clients rarely half-close). */
	__cubit_readiness_changed();
	return 0;
}

hidden void __cubit_tcp_close(struct cubit_tcp *t)
{
	LOCK(t->lock);
	t->closing = 1;
	int open = t->state == TCP_OPEN;
	UNLOCK(t->lock);
	if (open) send_shut(t);     /* ends the reader's outstanding READ */
	__cubit_readiness_changed();
	release(t);
}
