/*
 * CuBit libc: TCP sockets over netstack (docs/servo-port.md).
 *
 * A socket is a netstack channel reached through one of the program's
 * network endpoints, which exist only for the scopes its manifest
 * requested and the launch approved. On first use the libc asks netstack
 * what each endpoint's scope allows (OP_NET_SCOPE) and routes each
 * connection or listener to one that permits it; netstack checks again.
 * Nothing here adds authority.
 *
 * Names are not resolved here (lookup_name.c): a host name gets a
 * placeholder IPv4 address, and connecting to it opens
 * "@net:tcp:<name>:<port>", so netstack resolves the name inside the scope.
 *
 * Each socket's channel lives in a buffer of a channel arena: memory lent
 * to netstack once and cut into buffers of a header page, a send ring and
 * a receive ring (cubit_net_channel.h; docs/netstack-redesign.md, "Async
 * channels" and "Channel arenas"). Arenas are lent on demand, ARENA_BUFFERS
 * sockets each, so sockets cost no grant of their own. Reads and writes move bytes through the rings without any
 * IPC while netstack keeps up; the ring indices go through the proved
 * CuBit.Channel_Rings (compiled into this libc). IPC is needed only to wake
 * an idle side:
 * - a one-way KICK when netstack asked for one (it drained the send ring,
 *   or waits for receive ring space);
 * - a WAIT, submitted asynchronously, which netstack completes when a
 *   socket some thread waits on becomes ready. Waiting threads register
 *   their sockets (the WAIT's interest mask); one thread at a time (the
 *   waiter) blocks for completions, the others on the descriptor futex
 *   (fd.c), which the waiter bumps. A socket added while a WAIT is out
 *   ends that WAIT so the next one includes it.
 * OPEN is submitted asynchronously too; its completion carries the handle.
 *
 * Listening: bind records the address; listen opens a netstack listener
 * ("@net:tcp-listen:<address>:<port>", checked against the manifest's
 * tcp-listen scope) in an arena buffer, and keeps OFFERS sockets offered
 * to it, each with its own buffer and wait bit. A connection arrives open
 * in one of them (an arrival record in the listener's receive ring);
 * accept hands that socket out and offers another. No IPC per accept
 * beyond the kick for a new offer, and only when netstack asked for one.
 *
 * No UDP yet.
 */
#define _GNU_SOURCE
#include <arpa/inet.h>
#include <errno.h>
#include <netinet/in.h>
#include <poll.h>
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
#include "cubit_net_channel.h"

/* Scopes (CuBit.Network_Authority) and how to find their endpoints. */
enum { SCOPE_CONNECT_TCP = 1, SCOPE_LISTEN_TCP = 2, SCOPE_CONNECT_UDP = 3 };
enum { SYSCALL_GETPID = 6, SYSCALL_INFO = 15, SYSCALL_INSPECT_CAPABILITY = 84 };
#define SYSINFO_REGISTERED_DRIVER 2000
#define DRIVER_NETSTACK 3
#define CAPABILITY_SLOTS 64
#define CAPABILITY_ENDPOINT 1
#define MAX_SCOPES 16

enum {
	SYSCALL_WAIT_COMPLETION = 24,
	SYSCALL_POLL_COMPLETION = 25,
	SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY = 41,
	SYSCALL_SUBMIT_VIA_ENDPOINT_CAPABILITY = 42,
	SYSCALL_REVOKE_SHARED_MEMORY_GRANT = 103,
	SYSCALL_CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY = 106,
	SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION = 108,
};

enum { OP_NET_OPEN = 0x0420, OP_NET_SHUT = 0x0423 };
enum { REPLY_OK = 0xF000 };

/* Each direction's ring; the grant is the header page and both rings. */
#define RING_BYTES (64 * 1024UL)
#define BUF_BYTES (NET_HEADER_BYTES + 2 * RING_BYTES)
/* Sockets per arena; enough arenas for every wait bit. */
#define ARENA_BUFFERS 16
/* Sockets a listener keeps offered for arriving connections. */
#define OFFERS 4
#define MAX_ARENAS ((NET_MAXIMUM_WAIT_BIT + 1) / ARENA_BUFFERS)

#define NO_COMPLETION_TOKEN (~0UL)
/* A WAIT's token; an OPEN's is (serial << 8) | wait bit. */
#define WAIT_TOKEN 0xFFUL
/* SHUT completions: (serial << 8 | wait bit) with this bit set. */
#define SHUT_TOKEN (1UL << 63)

struct message {
	uint32_t label;
	uint8_t length, flags;
	uint16_t reserved;
	uint64_t authority;
	uint64_t words[4];
};

/* The kernel's completion entry (process.ads CompletionEntry). */
struct completion {
	uint64_t request_id, token;
	struct message msg;
	uint32_t from, pad0;
	uint64_t status;
	uint8_t valid, pad[7];
};
_Static_assert(sizeof(struct message) == 48, "IPC message ABI");
_Static_assert(sizeof(struct completion) == 88, "completion entry ABI");

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

/* Call netstack through the endpoint in slot; reply gets the reply's
 * words (if not NULL). */
static uint32_t call(unsigned slot, uint32_t label, uint8_t length, uint64_t w0,
	uint64_t w1, uint64_t w2, uint64_t w3, uint64_t reply[4])
{
	struct message m = { label, length, 0, 0, 0, { w0, w1, w2, w3 } };
	unsigned long tag = cubit(SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY,
		slot, (unsigned long)&m, 0, 0);
	if (tag == (unsigned long)-1) return 0;
	if (reply) memcpy(reply, m.words, sizeof m.words);
	return (uint32_t)tag;
}

/* Submit to netstack: its reply becomes a completion with token, or with
 * NO_COMPLETION_TOKEN a one-way message with no reply. 1 if queued. */
static int submit(unsigned slot, uint32_t label, uint8_t length, uint64_t w0,
	uint64_t w1, uint64_t w2, uint64_t w3, uint64_t token)
{
	unsigned long ret;
	uint64_t tag = (uint64_t)label | (uint64_t)length << 32;
	register uint64_t r10 __asm__("r10") = w1;
	register uint64_t r8 __asm__("r8") = w2;
	register uint64_t r9 __asm__("r9") = w3;
	register uint64_t r12 __asm__("r12") = token;
	__asm__ __volatile__ ("syscall"
		: "=a"(ret), "+r"(r10), "+r"(r8), "+r"(r9), "+r"(r12)
		: "a"((unsigned long)SYSCALL_SUBMIT_VIA_ENDPOINT_CAPABILITY),
		  "D"((unsigned long)slot), "S"(tag), "d"(w0)
		: "rcx", "r11", "memory");
	return ret == 1;
}

/* --- names ----------------------------------------------------------------- */

/* Placeholder addresses 100.100.0.1 .. 100.100.255.255 stand for names. */
/* The program's network scopes, found once: each netstack endpoint among
 * its capabilities, and what netstack says its scope allows. */
static struct scope {
	unsigned slot, action, prefix;  /* prefix over the 128 bits */
	unsigned char network[NET_ADDRESS_BYTES];   /* IPv4 mapped */
	uint16_t first, last;           /* ports */
	int resolve;                    /* may name hosts */
} scopes[MAX_SCOPES];

/* The single address type (CuBit.Net_Address): IPv4 as ::ffff:a.b.c.d.
 * be is the IPv4 address in network order, as in sin_addr. */
static void mapped(unsigned char a[NET_ADDRESS_BYTES], uint32_t be)
{
	memset(a, 0, 10);
	a[10] = a[11] = 0xff;
	memcpy(a + 12, &be, 4);
}

/* a lies in network/prefix. */
static int matches(const unsigned char *a, const unsigned char *network,
	unsigned prefix)
{
	for (unsigned i = 0; i < NET_ADDRESS_BYTES; i++) {
		unsigned bits = prefix > 8 * i ? prefix - 8 * i : 0;
		unsigned char mask = bits >= 8 ? 0xff : (unsigned char)(0xff << (8 - bits));
		if ((a[i] & mask) != (network[i] & mask)) return 0;
	}
	return 1;
}
static int scope_count, scopes_known;
static volatile int scopes_lock[1];

static void discover_scopes(void)
{
	LOCK(scopes_lock);
	if (!scopes_known) {
		unsigned long self = cubit(SYSCALL_GETPID, 0, 0, 0, 0);
		unsigned long netstack = cubit(SYSCALL_INFO, SYSINFO_REGISTERED_DRIVER,
			DRIVER_NETSTACK, 0, 0);
		for (unsigned slot = 0; slot < CAPABILITY_SLOTS && scope_count < MAX_SCOPES; slot++) {
			uint64_t info[6] = { 0 }, reply[4];
			if (cubit(SYSCALL_INSPECT_CAPABILITY, self, slot, (unsigned long)info, 0) != 1 ||
			    info[0] != CAPABILITY_ENDPOINT || info[3] != netstack ||
			    call(slot, OP_NET_SCOPE, 0, 0, 0, 0, 0, reply) != REPLY_OK)
				continue;
			uint64_t d = reply[2];
			struct scope *c = &scopes[scope_count++];
			*c = (struct scope){
				.slot = slot,
				.first = (uint16_t)d, .last = (uint16_t)(d >> 16),
				.prefix = (unsigned)(d >> 32) & 0xff,
				.action = (unsigned)(d >> 40) & 0xff,
				.resolve = (int)(d >> 48) & 1 };
			memcpy(c->network, reply, NET_ADDRESS_BYTES);  /* words 0 and 1 */
			if (c->prefix > 8 * NET_ADDRESS_BYTES) scope_count--;
		}
		scopes_known = 1;
	}
	UNLOCK(scopes_lock);
}

/* The endpoint of a scope for action on address (NULL with a name) and
 * port, or -1. */
static int scope_slot(unsigned action, const unsigned char *address,
	uint16_t port)
{
	discover_scopes();
	for (int i = 0; i < scope_count; i++) {
		const struct scope *c = &scopes[i];
		if (c->action == action && port >= c->first && port <= c->last &&
		    (address ? matches(address, c->network, c->prefix) : c->resolve))
			return (int)c->slot;
	}
	return -1;
}

/* An endpoint for the process's own requests (arenas, WAIT, KICK). */
static int any_slot(void)
{
	discover_scopes();
	return scope_count ? (int)scopes[0].slot : -1;
}

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
	volatile int write_lock[1];     /* one producer of the send ring */
	volatile int read_lock[1];      /* one consumer of the receive ring */
	enum tcp_state state;
	int error;                      /* pending SO_ERROR */
	int closing, write_shut;
	int refs;                       /* the descriptor, and an OPEN in flight */
	int bit;                        /* wait bit, or -1 */
	int slot;                       /* its scope's endpoint, or -1 */
	uint64_t channel, open_token, shut_token;
	unsigned char *buf;
	int arena, index;               /* its arena buffer, or arena -1 */
	struct cubit_ring tx, rx;       /* we produce tx, consume rx */
	struct sockaddr_in peer;
	struct sockaddr_in local;       /* bind's address */
	int bound, listening;
	struct cubit_tcp *offered[OFFERS];   /* a listener's offers */
	char target[300];
	size_t target_len;
};

static volatile int net_lock[1];
static struct cubit_tcp *by_bit[NET_MAXIMUM_WAIT_BIT + 1];
static uint64_t open_serial;
static int opens_outstanding;
static int wait_outstanding;            /* a WAIT is at netstack */
static unsigned long wait_deadline;     /* ... when it gives up */
static uint64_t wait_mask;              /* ... and the sockets it is for */
static volatile int waiter;             /* a thread blocks for completions */
/* Sockets that waiting threads want to hear about, counted per wait bit. */
static unsigned interest_count[NET_MAXIMUM_WAIT_BIT + 1];
static uint64_t interest;

static void add_interest(uint64_t mask)
{
	for (int b = 0; b <= NET_MAXIMUM_WAIT_BIT; b++)
		if (mask >> b & 1 && !interest_count[b]++) interest |= 1UL << b;
}

static void drop_interest(uint64_t mask)
{
	for (int b = 0; b <= NET_MAXIMUM_WAIT_BIT; b++)
		if (mask >> b & 1 && !--interest_count[b]) interest &= ~(1UL << b);
}

static void end_wait(void)
{
	int slot = any_slot();
	if (slot >= 0)
		submit((unsigned)slot, OP_NET_KICK, 2, 0, NET_END_WAIT, 0, 0,
		       NO_COMPLETION_TOKEN);
}

#define barrier() __asm__ __volatile__ ("" ::: "memory")
#define fence() __asm__ __volatile__ ("mfence" ::: "memory")

static inline volatile uint32_t *word(struct cubit_tcp *t, unsigned offset)
{
	return (volatile uint32_t *)(t->buf + offset);
}

static inline unsigned char *send_ring(struct cubit_tcp *t)
{
	return t->buf + NET_HEADER_BYTES;
}

static inline unsigned char *receive_ring(struct cubit_tcp *t)
{
	return t->buf + NET_HEADER_BYTES + RING_BYTES;
}

static void kick(struct cubit_tcp *t, uint64_t flags)
{
	submit((unsigned)t->slot, OP_NET_KICK, 2, 1UL << t->bit, flags, 0, 0,
	       NO_COMPLETION_TOKEN);
}

static int status_error(uint32_t status)
{
	switch (status) {
	case NET_STATUS_RESET: return ECONNRESET;
	case NET_STATUS_TIMED_OUT: return ETIMEDOUT;
	case NET_STATUS_UNREACHABLE: return EHOSTUNREACH;
	case NET_STATUS_PROTOCOL_ERROR: return EPROTO;
	default: return 0;
	}
}

/* Channel arenas lent to netstack, and which of their buffers sockets
 * hold. Memory and grants are kept for the process's lifetime (it holds
 * at most MAX_ARENAS). A buffer comes back only after netstack released
 * its channel (SHUT is synchronous; a failed OPEN releases it). */
static struct arena {
	unsigned char *base;
	uint64_t handle;
	uint32_t used;                  /* a bit per buffer */
} arenas[MAX_ARENAS];
static int arena_count;
static volatile int arena_lock[1];

/* Lend netstack a new arena (a call: once per ARENA_BUFFERS sockets). */
static int lend_arena(void)
{
	int endpoint = any_slot();
	if (endpoint < 0) return -EACCES;
	size_t bytes = (size_t)ARENA_BUFFERS * BUF_BYTES;
	unsigned char *mem = mmap(0, bytes, PROT_READ | PROT_WRITE,
		MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	if (mem == MAP_FAILED) return -ENOMEM;
	unsigned long slot = cubit(SYSCALL_CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY,
		(unsigned long)endpoint, (unsigned long)mem, bytes / 4096, 1);
	if (slot == (unsigned long)-1) {
		munmap(mem, bytes);
		return -EACCES;
	}
	unsigned long gen = cubit(SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION,
		slot, 0, 0, 0);
	uint64_t reply[4];
	if (gen == (unsigned long)-1 || !gen ||
	    call((unsigned)endpoint, OP_NET_ARENA, 4, slot, gen,
		 RING_BYTES | (uint64_t)RING_BYTES << 32, ARENA_BUFFERS,
		 reply) != REPLY_OK) {
		cubit(SYSCALL_REVOKE_SHARED_MEMORY_GRANT, slot, 0, 0, 0);
		munmap(mem, bytes);
		return -EACCES;
	}
	arenas[arena_count++] = (struct arena){ mem, reply[0], 0 };
	return 0;
}

/* Take a free buffer for t, lending a new arena if all are held. */
static int take_buffer(struct cubit_tcp *t)
{
	int err = 0;
	LOCK(arena_lock);
	for (;;) {
		for (int a = 0; a < arena_count; a++) {
			if (arenas[a].used == (1u << ARENA_BUFFERS) - 1) continue;
			int i = __builtin_ctz(~arenas[a].used);
			arenas[a].used |= 1u << i;
			t->arena = a;
			t->index = i;
			t->buf = arenas[a].base + (size_t)i * BUF_BYTES;
			UNLOCK(arena_lock);
			memset(t->buf, 0, NET_HEADER_BYTES);  /* indices at 0 */
			return 0;
		}
		if (arena_count == MAX_ARENAS) { err = -ENOBUFS; break; }
		if ((err = lend_arena())) break;
	}
	UNLOCK(arena_lock);
	return err;
}

static void give_buffer(struct cubit_tcp *t)
{
	if (t->arena < 0) return;
	LOCK(arena_lock);
	arenas[t->arena].used &= ~(1u << t->index);
	UNLOCK(arena_lock);
	t->arena = -1;
}

static void release(struct cubit_tcp *t)
{
	LOCK(t->lock);
	int last = --t->refs == 0;
	UNLOCK(t->lock);
	if (!last) return;
	LOCK(net_lock);
	if (t->bit >= 0) by_bit[t->bit] = 0;
	UNLOCK(net_lock);
	give_buffer(t);
	free(t);
}

/* SHUT and wait for it (a listener's close must know no more arrive). */
static void send_shut_now(struct cubit_tcp *t)
{
	call((unsigned)t->slot, OP_NET_SHUT, 1, t->channel, 0, 0, 0, 0);
}

/* SHUT without waiting: the socket keeps its buffer and wait bit (a
 * reference) until the completion says netstack released the channel. */
static void send_shut(struct cubit_tcp *t)
{
	LOCK(net_lock);
	uint64_t token = SHUT_TOKEN | (++open_serial << 8) | (uint64_t)t->bit;
	UNLOCK(net_lock);
	LOCK(t->lock);
	t->shut_token = token;
	t->refs++;
	UNLOCK(t->lock);
	if (submit((unsigned)t->slot, OP_NET_SHUT, 1, t->channel, 0, 0, 0, token)) return;
	LOCK(t->lock);
	t->shut_token = 0;
	t->refs--;
	UNLOCK(t->lock);
	send_shut_now(t);
}

/* An OPEN finished: the socket is open or failed; a socket closed while
 * connecting gives its channel back. */
static void opened(struct cubit_tcp *t, const struct completion *c)
{
	int ok = c->status == 0 && c->msg.label == REPLY_OK;
	LOCK(t->lock);
	t->open_token = 0;
	if (ok) {
		t->channel = c->msg.words[0];
		t->state = TCP_OPEN;
	} else {
		t->state = TCP_FAILED;
		t->error = ECONNREFUSED;
	}
	int closing = t->closing;
	UNLOCK(t->lock);
	if (ok && closing) send_shut(t);
	release(t);     /* the OPEN's reference */
}

static void dispatch(const struct completion *c)
{
	if (c->token == WAIT_TOKEN) {
		LOCK(net_lock);
		wait_outstanding = 0;
		UNLOCK(net_lock);
		return;
	}
	unsigned bit = c->token & 0xFF;
	if (bit > NET_MAXIMUM_WAIT_BIT) return;
	if (c->token & SHUT_TOKEN) {
		LOCK(net_lock);
		struct cubit_tcp *s = by_bit[bit];
		int done = s && s->shut_token == c->token;
		UNLOCK(net_lock);
		if (done) {
			s->shut_token = 0;
			release(s);     /* the SHUT's reference */
		}
		return;
	}
	LOCK(net_lock);
	struct cubit_tcp *t = by_bit[bit];
	int match = t && t->open_token == c->token;
	if (match) opens_outstanding--;
	UNLOCK(net_lock);
	if (match) opened(t, c);
}

/* Take the completions that have arrived; with block, wait for one. */
static void drain(int block)
{
	struct completion c[8];
	unsigned long n = 0;
	if (block) {
		n = cubit(SYSCALL_WAIT_COMPLETION, (unsigned long)c, 8, 1, 0);
		if (n > 8) n = 0;
	} else {
		while (n < 8 && cubit(SYSCALL_POLL_COMPLETION, (unsigned long)&c[n], 0, 0, 0) == 1)
			n++;
	}
	for (unsigned long i = 0; i < n; i++) dispatch(&c[i]);
	if (n) __cubit_readiness_changed();
}

/* Collect finished OPENs and WAITs without blocking, if any are out and
 * no waiter is collecting them. */
static void collect(void)
{
	if (!waiter && (opens_outstanding || wait_outstanding)) drain(0);
}

/* A local event (fd.c) while a thread blocks for netstack: end its WAIT so
 * it looks again. */
hidden void __cubit_net_interrupt(void)
{
	if (waiter && wait_outstanding) end_wait();
}

hidden uint64_t __cubit_tcp_mask(struct cubit_tcp *t)
{
	return t->bit >= 0 ? 1UL << t->bit : 0;
}

/* Block until netstack reports one of the sockets in mask (or finishes an
 * OPEN), the descriptor futex moves on from seq, or the deadline (kernel
 * milliseconds; ~0 = forever) passes. */
hidden void __cubit_net_wait(int seq, unsigned long deadline, uint64_t mask)
{
	LOCK(net_lock);
	add_interest(mask);
	if (waiter) {
		/* The waiter's WAIT must cover our sockets: if not, end it so
		 * the next one does. */
		int stale = wait_outstanding && (interest & ~wait_mask);
		UNLOCK(net_lock);
		if (stale) end_wait();
		__cubit_readiness_wait(seq, deadline);
		LOCK(net_lock);
		drop_interest(mask);
		UNLOCK(net_lock);
		return;
	}
	waiter = 1;
	int fresh = !wait_outstanding;
	int stale = !fresh && ((interest & ~wait_mask) || deadline < wait_deadline);
	uint64_t wanted = interest;
	if (fresh) {
		wait_outstanding = 1;
		wait_mask = wanted;
		wait_deadline = deadline;
	}
	UNLOCK(net_lock);
	int endpoint = fresh ? any_slot() : -1;
	if (fresh && (endpoint < 0 ||
		      !submit((unsigned)endpoint, OP_NET_WAIT, 3, 0, deadline, wanted, 0,
			      WAIT_TOKEN))) {
		LOCK(net_lock);
		wait_outstanding = 0;
		waiter = 0;
		drop_interest(mask);
		UNLOCK(net_lock);
		__cubit_readiness_wait(seq, deadline);
		return;
	}
	/* The outstanding WAIT misses a socket or outlasts our deadline: end
	 * it, and the caller's next wait submits the right one. */
	if (stale) end_wait();
	if (__cubit_readiness_seq() == seq) drain(1);
	else drain(0);
	LOCK(net_lock);
	waiter = 0;
	drop_interest(mask);
	UNLOCK(net_lock);
}

hidden struct cubit_tcp *__cubit_tcp_new(void)
{
	struct cubit_tcp *t = calloc(1, sizeof *t);
	if (t) {
		t->refs = 1;
		t->bit = -1;
		t->slot = -1;
		t->arena = -1;
	}
	return t;
}

/* Give t a buffer and a wait bit, and lay out the channel header netstack
 * reads when a channel opens there. */
static int prepare_channel(struct cubit_tcp *t)
{
	int err = take_buffer(t);
	if (err == -ENOBUFS) {
		collect();      /* closed sockets whose SHUT completed give theirs back */
		err = take_buffer(t);
	}
	if (err) return err;
	t->tx = (struct cubit_ring){ RING_BYTES, 0, 0 };
	t->rx = (struct cubit_ring){ RING_BYTES, 0, 0 };
	LOCK(net_lock);
	int bit = -1;
	for (int i = 0; i <= NET_MAXIMUM_WAIT_BIT && bit < 0; i++)
		if (!by_bit[i]) bit = i;
	if (bit >= 0) {
		by_bit[bit] = t;
		t->bit = bit;
	}
	UNLOCK(net_lock);
	if (bit < 0) {
		collect();
		LOCK(net_lock);
		for (int i = 0; i <= NET_MAXIMUM_WAIT_BIT && bit < 0; i++)
			if (!by_bit[i]) bit = i;
		if (bit >= 0) {
			by_bit[bit] = t;
			t->bit = bit;
		}
		UNLOCK(net_lock);
	}
	if (bit < 0) return -ENOBUFS;
	*word(t, NET_TX_SIZE_AT) = RING_BYTES;
	*word(t, NET_RX_SIZE_AT) = RING_BYTES;
	*word(t, NET_WAIT_BIT_AT) = (uint32_t)bit;
	return 0;
}

/* Submit OPEN for t's target (prepared) and, unless nonblock, wait for
 * its completion. */
static long open_channel(struct cubit_tcp *t, int nonblock)
{
	LOCK(net_lock);
	t->open_token = (++open_serial << 8) | (uint64_t)t->bit;
	opens_outstanding++;
	UNLOCK(net_lock);
	memcpy(t->buf + NET_TARGET_AT, t->target, t->target_len);

	LOCK(t->lock);
	t->state = TCP_CONNECTING;
	t->refs++;
	UNLOCK(t->lock);
	if (!submit((unsigned)t->slot, OP_NET_OPEN, (uint8_t)t->target_len,
		    arenas[t->arena].handle, (uint64_t)t->index, 0, 0,
		    t->open_token)) {
		LOCK(net_lock);
		opens_outstanding--;
		UNLOCK(net_lock);
		LOCK(t->lock);
		t->state = TCP_FAILED;
		t->error = EAGAIN;
		t->open_token = 0;
		t->refs--;
		UNLOCK(t->lock);
		return -EAGAIN;
	}
	if (nonblock) return -EINPROGRESS;
	for (;;) {
		int seq = __cubit_readiness_seq();
		collect();
		LOCK(t->lock);
		enum tcp_state state = t->state;
		UNLOCK(t->lock);
		if (state == TCP_OPEN) return 0;
		if (state == TCP_FAILED) return -t->error;
		__cubit_net_wait(seq, ~0UL, 0);
	}
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
	if (n <= 0 || n > NET_TARGET_MAXIMUM) return -ENAMETOOLONG;
	t->target_len = (size_t)n;
	t->peer = *sin;
	unsigned char target[NET_ADDRESS_BYTES];
	mapped(target, sin->sin_addr.s_addr);
	t->slot = scope_slot(SCOPE_CONNECT_TCP,
		address_name(sin->sin_addr.s_addr) ? 0 : target, ntohs(sin->sin_port));
	if (t->slot < 0) return -EACCES;    /* no scope allows it */

	/* A buffer in an arena lent to netstack (lending one fails without
	 * a network capability). */
	int err = prepare_channel(t);
	if (err) return err;
	return open_channel(t, nonblock);
}

/* Receive-ring bytes available now, as the ring rules allow (0 if netstack
 * broke them). Caller holds read_lock. */
static uint32_t receivable(struct cubit_tcp *t)
{
	uint32_t produced = *word(t, NET_RX_PRODUCED_AT);
	barrier();
	if (!__cubit_ring_accept_produced(&t->rx, produced)) return 0;
	return t->rx.count;
}

/* Copy up to n received bytes out and release them. */
static size_t ring_read(struct cubit_tcp *t, unsigned char *out, size_t n)
{
	uint32_t first, l1, l2;
	/* Data seen before may already cover the request: then netstack's
	 * produced index, on a line it writes, is not read again. */
	if (t->rx.count < n) receivable(t);
	if (!t->rx.count) return 0;
	__cubit_ring_data_slices(&t->rx, &first, &l1, &l2);
	size_t take = n < (size_t)l1 + l2 ? n : (size_t)l1 + l2;
	size_t c1 = take < l1 ? take : l1;
	memcpy(out, receive_ring(t) + first, c1);
	memcpy(out + c1, receive_ring(t), take - c1);
	__cubit_ring_consume(&t->rx, (uint32_t)take);
	barrier();
	*word(t, NET_RX_CONSUMED_AT) = t->rx.own;
	if (take) {
		fence();
		if (*word(t, NET_KICK_WANTED_AT) & NET_KICK_ON_RECEIVE) kick(t, 0);
	}
	return take;
}

/* Arm a notification, then look again: netstack may have moved before it
 * could see the flag. */
static void want(struct cubit_tcp *t, uint32_t flags)
{
	uint32_t old = *word(t, NET_WANT_AT);
	if ((old & flags) != flags) *word(t, NET_WANT_AT) = old | flags;
	fence();
}

hidden long __cubit_tcp_read(struct cubit_tcp *t, void *out, size_t n, int nonblock)
{
	for (;;) {
		int seq = __cubit_readiness_seq();
		collect();
		LOCK(t->lock);
		enum tcp_state state = t->state;
		int err = t->error;
		UNLOCK(t->lock);
		if (state == TCP_IDLE) return -ENOTCONN;
		if (state == TCP_FAILED) return err && err != ECONNREFUSED ? -err : 0;
		if (state == TCP_OPEN) {
			LOCK(t->read_lock);
			size_t got = ring_read(t, out, n);
			uint32_t status = *word(t, NET_STATUS_AT);
			if (!got && status >= NET_STATUS_PEER_FINISHED)
				got = ring_read(t, out, n);   /* the last bytes precede the status */
			UNLOCK(t->read_lock);
			if (got || !n) return (long)got;
			if (status == NET_STATUS_PEER_FINISHED) return 0;
			if (status_error(status)) return -status_error(status);
			if (nonblock) return -EAGAIN;
			want(t, NET_WANT_READABLE);
			LOCK(t->read_lock);
			uint32_t ready = receivable(t);
			UNLOCK(t->read_lock);
			if (ready || *word(t, NET_STATUS_AT) >= NET_STATUS_PEER_FINISHED) continue;
		} else if (nonblock) {
			return -EAGAIN;
		}
		__cubit_net_wait(seq, ~0UL, __cubit_tcp_mask(t));
	}
}

/* Send-ring free space, as the ring rules allow. Caller holds write_lock. */
static uint32_t sendable(struct cubit_tcp *t)
{
	uint32_t consumed = *word(t, NET_TX_CONSUMED_AT);
	if (!__cubit_ring_accept_consumed(&t->tx, consumed)) return 0;
	return t->tx.size - t->tx.count;
}

/* Copy up to n bytes into the send ring and publish them. */
static size_t ring_write(struct cubit_tcp *t, const unsigned char *p, size_t n)
{
	uint32_t first, l1, l2;
	/* Space seen before may already fit the request: then netstack's
	 * consumed index is not read (a stale view only under-reports). */
	if (t->tx.size - t->tx.count < n && !sendable(t)) return 0;
	if (t->tx.count == t->tx.size) return 0;
	__cubit_ring_free_slices(&t->tx, &first, &l1, &l2);
	size_t put = n < (size_t)l1 + l2 ? n : (size_t)l1 + l2;
	size_t c1 = put < l1 ? put : l1;
	memcpy(send_ring(t) + first, p, c1);
	memcpy(send_ring(t), p + c1, put - c1);
	__cubit_ring_commit(&t->tx, (uint32_t)put);
	barrier();
	*word(t, NET_TX_PRODUCED_AT) = t->tx.own;
	if (put) {
		fence();
		if (*word(t, NET_KICK_WANTED_AT) & NET_KICK_ON_SEND) kick(t, 0);
	}
	return put;
}

hidden long __cubit_tcp_write(struct cubit_tcp *t, const struct iovec *iov, int n,
	int nonblock)
{
	for (;;) {
		int seq = __cubit_readiness_seq();
		collect();
		LOCK(t->lock);
		enum tcp_state state = t->state;
		int shut = t->write_shut || t->closing;
		UNLOCK(t->lock);
		if (state == TCP_IDLE) return -ENOTCONN;
		if (state == TCP_FAILED || shut) return -EPIPE;
		if (state == TCP_OPEN) break;
		if (nonblock) return -EAGAIN;
		__cubit_net_wait(seq, ~0UL, 0);
	}
	long total = 0;
	LOCK(t->write_lock);
	for (int i = 0; i < n; i++) {
		const unsigned char *p = iov[i].iov_base;
		size_t left = iov[i].iov_len;
		while (left) {
			int seq = __cubit_readiness_seq();
			uint32_t status = *word(t, NET_STATUS_AT);
			if (status_error(status)) {
				UNLOCK(t->write_lock);
				return total ? total : -EPIPE;
			}
			size_t put = ring_write(t, p, left);
			p += put;
			left -= put;
			total += (long)put;
			if (!left) break;
			if (put) continue;
			if (nonblock) {
				UNLOCK(t->write_lock);
				return total ? total : -EAGAIN;
			}
			want(t, NET_WANT_WRITABLE);
			if (sendable(t)) continue;
			__cubit_net_wait(seq, ~0UL, __cubit_tcp_mask(t));
			collect();
		}
	}
	UNLOCK(t->write_lock);
	return total;
}

hidden short __cubit_tcp_poll(struct cubit_tcp *t, short events)
{
	short re = 0;
	collect();
	LOCK(t->lock);
	enum tcp_state state = t->state;
	int write_shut = t->write_shut;
	UNLOCK(t->lock);
	switch (state) {
	case TCP_IDLE:
		return POLLHUP;
	case TCP_CONNECTING:
		return 0;
	case TCP_FAILED:
		return POLLERR | POLLHUP | (events & (POLLOUT | POLLIN));
	case TCP_OPEN:
		break;
	}
	if (t->listening) {
		/* Readable when a connection has arrived. */
		for (int look = 0; look < 2; look++) {
			LOCK(t->read_lock);
			uint32_t arrived = receivable(t);
			UNLOCK(t->read_lock);
			re = arrived ? events & (POLLIN | POLLRDNORM) : 0;
			if (re || look || !(events & (POLLIN | POLLRDNORM))) break;
			want(t, NET_WANT_READABLE);
		}
		return re;
	}
	for (int look = 0; look < 2; look++) {
		uint32_t status = *word(t, NET_STATUS_AT);
		LOCK(t->read_lock);
		uint32_t readable = receivable(t);
		UNLOCK(t->read_lock);
		LOCK(t->write_lock);
		uint32_t writable = sendable(t);
		UNLOCK(t->write_lock);
		int final = status >= NET_STATUS_PEER_FINISHED;
		re = 0;
		if (!write_shut && (writable || status_error(status)))
			re |= events & (POLLOUT | POLLWRNORM);
		if (readable || final) re |= events & (POLLIN | POLLRDNORM);
		if (final) re |= POLLRDHUP & events;
		if (status_error(status)) re |= POLLERR | POLLHUP;
		if (re || look) break;
		/* Not ready: ask netstack to tell this process's waiter. */
		uint32_t flags = 0;
		if (events & (POLLIN | POLLRDNORM | POLLRDHUP)) flags |= NET_WANT_READABLE;
		if (events & (POLLOUT | POLLWRNORM)) flags |= NET_WANT_WRITABLE;
		if (!flags) break;
		want(t, flags);
	}
	return re;
}

hidden long __cubit_tcp_bind(struct cubit_tcp *t, const struct sockaddr *sa,
	socklen_t len)
{
	if (!sa || len < sizeof(struct sockaddr_in)) return -EINVAL;
	if (sa->sa_family != AF_INET) return -EAFNOSUPPORT;
	LOCK(t->lock);
	int busy = t->state != TCP_IDLE || t->bound;
	if (!busy) {
		t->local = *(const struct sockaddr_in *)sa;
		t->bound = 1;
	}
	UNLOCK(t->lock);
	return busy ? -EINVAL : 0;
}

/* Offer a new socket to listener l for its next arriving connection:
 * one arrival record's worth of the listener's send ring. 0 if offered. */
static int offer(struct cubit_tcp *l, int slot)
{
	struct cubit_tcp *t = __cubit_tcp_new();
	if (!t) return -ENOMEM;
	t->slot = l->slot;                 /* accepted channels are the listen scope's */
	int err = prepare_channel(t);
	if (err) {
		release(t);
		return err;
	}
	unsigned char item[NET_OFFER_BYTES];
	uint64_t arena = arenas[t->arena].handle;
	uint32_t index = (uint32_t)t->index;
	memcpy(item + NET_OFFER_ARENA_AT, &arena, sizeof arena);
	memcpy(item + NET_OFFER_BUFFER_AT, &index, sizeof index);
	LOCK(l->write_lock);
	int put = sendable(l) || l->tx.count == 0
		? __cubit_datagram_put(&l->tx, send_ring(l), item, sizeof item) : 0;
	if (put == 1) {
		barrier();
		*word(l, NET_TX_PRODUCED_AT) = l->tx.own;
		fence();
		if (*word(l, NET_KICK_WANTED_AT) & NET_KICK_ON_SEND) kick(l, 0);
	}
	UNLOCK(l->write_lock);
	if (put != 1) {
		release(t);
		return -ENOBUFS;
	}
	LOCK(l->lock);
	l->offered[slot] = t;
	UNLOCK(l->lock);
	return 0;
}

static void refill_offers(struct cubit_tcp *l)
{
	for (int i = 0; i < OFFERS; i++) {
		LOCK(l->lock);
		int empty = !l->offered[i];
		UNLOCK(l->lock);
		if (empty && offer(l, i)) return;
	}
}

hidden long __cubit_tcp_listen(struct cubit_tcp *t, int backlog)
{
	(void)backlog;   /* netstack keeps the backlog; OFFERS bound what is taken */
	LOCK(t->lock);
	enum tcp_state state = t->state;
	int bound = t->bound, listening = t->listening;
	UNLOCK(t->lock);
	if (listening) return 0;
	if (state != TCP_IDLE) return -EINVAL;
	/* A listener needs the port its scope names: no ephemeral port. */
	if (!bound || !t->local.sin_port) return -EOPNOTSUPP;
	/* A listener names one address, the one its tcp-listen scope names;
	 * INADDR_ANY takes that. */
	uint16_t port = ntohs(t->local.sin_port);
	struct in_addr address = t->local.sin_addr;
	if (address.s_addr == INADDR_ANY) {
		discover_scopes();
		for (int i = 0; i < scope_count; i++)
			if (scopes[i].action == SCOPE_LISTEN_TCP &&
			    port >= scopes[i].first && port <= scopes[i].last)
				memcpy(&address.s_addr, scopes[i].network + 12, 4);
	}
	unsigned char listen_at[NET_ADDRESS_BYTES];
	mapped(listen_at, address.s_addr);
	t->slot = scope_slot(SCOPE_LISTEN_TCP, listen_at, port);
	if (t->slot < 0) return -EACCES;    /* no scope allows it */
	char host[INET_ADDRSTRLEN];
	inet_ntop(AF_INET, &address, host, sizeof host);
	int n = snprintf(t->target, sizeof t->target, "@net:tcp-listen:%s:%u",
		host, (unsigned)ntohs(t->local.sin_port));
	if (n <= 0 || n > NET_TARGET_MAXIMUM) return -ENAMETOOLONG;
	t->target_len = (size_t)n;
	long err = prepare_channel(t);
	if (err) return err;
	t->listening = 1;
	err = open_channel(t, 0);
	if (err) return err == -ECONNREFUSED ? -EADDRINUSE : err;
	refill_offers(t);
	return 0;
}

/* The next arrival on listener l: the offered socket it opened in. 1 if
 * one was taken, 0 if none, negative on a broken ring. */
static int take_arrival(struct cubit_tcp *l, struct cubit_tcp **out)
{
	unsigned char a[NET_ARRIVAL_BYTES];
	uint32_t got = 0;
	int cut = 0, r = 0;
	LOCK(l->read_lock);
	if (receivable(l)) {
		r = __cubit_datagram_take(&l->rx, receive_ring(l), a, sizeof a, &got, &cut);
		barrier();
		*word(l, NET_RX_CONSUMED_AT) = l->rx.own;
	}
	UNLOCK(l->read_lock);
	if (r != 1) return r < 0 ? -EPROTO : 0;
	if (got != NET_ARRIVAL_BYTES || cut) return -EPROTO;
	uint64_t channel, arena;
	uint32_t index;
	unsigned char address[NET_ADDRESS_BYTES], ipv4[NET_ADDRESS_BYTES];
	uint16_t port;
	memcpy(&channel, a + NET_ARRIVAL_CHANNEL_AT, sizeof channel);
	memcpy(&arena, a + NET_ARRIVAL_ARENA_AT, sizeof arena);
	memcpy(&index, a + NET_ARRIVAL_BUFFER_AT, sizeof index);
	memcpy(address, a + NET_ARRIVAL_ADDRESS_AT, sizeof address);
	memcpy(&port, a + NET_ARRIVAL_PORT_AT, sizeof port);
	mapped(ipv4, 0);
	if (!matches(address, ipv4, 96)) return -EPROTO;  /* IPv6: not yet here */
	struct cubit_tcp *t = 0;
	LOCK(l->lock);
	for (int i = 0; i < OFFERS && !t; i++) {
		struct cubit_tcp *o = l->offered[i];
		if (o && arenas[o->arena].handle == arena && (uint32_t)o->index == index) {
			t = o;
			l->offered[i] = 0;
		}
	}
	UNLOCK(l->lock);
	if (!t) return -EPROTO;         /* netstack named a buffer never offered */
	LOCK(t->lock);
	t->channel = channel;
	t->state = TCP_OPEN;
	t->peer = (struct sockaddr_in){ .sin_family = AF_INET, .sin_port = htons(port) };
	memcpy(&t->peer.sin_addr.s_addr, address + 12, 4);
	UNLOCK(t->lock);
	*out = t;
	return 1;
}

hidden long __cubit_tcp_accept(struct cubit_tcp *l, struct cubit_tcp **out,
	struct sockaddr *sa, socklen_t *len, int nonblock)
{
	LOCK(l->lock);
	int ok = l->listening && l->state == TCP_OPEN;
	UNLOCK(l->lock);
	if (!ok) return -EINVAL;
	for (;;) {
		int seq = __cubit_readiness_seq();
		collect();
		int r = take_arrival(l, out);
		if (r < 0) return r;
		if (r) {
			refill_offers(l);
			if (sa && len) {
				socklen_t n = *len < sizeof (*out)->peer ? *len : sizeof (*out)->peer;
				memcpy(sa, &(*out)->peer, n);
				*len = sizeof (*out)->peer;
			}
			return 0;
		}
		refill_offers(l);               /* after a failed refill */
		if (nonblock) return -EAGAIN;
		want(l, NET_WANT_READABLE);
		LOCK(l->read_lock);
		uint32_t ready = receivable(l);
		UNLOCK(l->read_lock);
		if (ready) continue;
		__cubit_net_wait(seq, ~0UL, __cubit_tcp_mask(l));
	}
}

hidden long __cubit_tcp_local(struct cubit_tcp *t, struct sockaddr *sa, socklen_t *len)
{
	struct sockaddr_in local = { .sin_family = AF_INET };
	LOCK(t->lock);
	if (t->bound) local = t->local;
	UNLOCK(t->lock);
	socklen_t n = *len < sizeof local ? *len : sizeof local;
	memcpy(sa, &local, n);
	*len = sizeof local;
	return 0;
}

hidden long __cubit_tcp_so_error(struct cubit_tcp *t)
{
	LOCK(t->lock);
	int err = t->state == TCP_FAILED ? t->error : 0;
	int open = t->state == TCP_OPEN;
	UNLOCK(t->lock);
	if (open) err = status_error(*word(t, NET_STATUS_AT));
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
		int first = !t->write_shut;
		t->write_shut = 1;
		int open = t->state == TCP_OPEN;
		UNLOCK(t->lock);
		/* FIN once netstack has sent what the ring holds. */
		if (first && t->buf) {
			*word(t, NET_SHUT_WRITE_AT) = 1;
			if (open) kick(t, 0);
		}
	}
	return 0;
}

hidden void __cubit_tcp_close(struct cubit_tcp *t)
{
	LOCK(t->lock);
	t->closing = 1;
	int open = t->state == TCP_OPEN;
	UNLOCK(t->lock);
	if (t->listening && open) {
		/* Once SHUT returns no more arrive: close those that did, and
		 * take back the offers (netstack dropped them with the
		 * listener). */
		send_shut_now(t);
		struct cubit_tcp *arrived;
		while (take_arrival(t, &arrived) == 1) __cubit_tcp_close(arrived);
		for (int i = 0; i < OFFERS; i++) {
			if (t->offered[i]) release(t->offered[i]);
			t->offered[i] = 0;
		}
		release(t);
		return;
	}
	/* SHUT takes what is left in the send ring, then sends FIN; an OPEN
	 * still in flight is shut when it completes. */
	if (open) send_shut(t);
	release(t);
}
