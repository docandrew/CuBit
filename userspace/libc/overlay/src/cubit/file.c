/*
 * CuBit libc: files through the filesystem service (docs/servo-port.md).
 *
 * A client of filesystem.svc's typed protocol
 * (userspace/runtime/gnat/cubit-filesystems.ads), over the capability in
 * the fixed filesystem slot. The service checks every path against the
 * program's filesystem scopes (its manifest's `filesystem-scope` entries);
 * this library adds no authority and no policy of its own.
 *
 * Names: a CuBit path ("@nvme:0/fonts/a.ttf") is used as is. A POSIX
 * absolute path names the same place on the system volume
 * ("/fonts/a.ttf" -> "@nvme:0/fonts/a.ttf"); relative paths start at the
 * system volume's root (there is no working directory). The system
 * volume is named by its device today; CuBit is volume-first with
 * machine-config aliases (@nvme0 = @mydisk), so this should become an
 * alias, "@system", once aliases are resolved (not implemented yet).
 *
 * Open, positioned read and write, flush and close go through a request
 * queue and a transfer arena lent to the service once (cubit_fs_queue.h,
 * docs/filesystem-data-plane.md): no message per request, and a KICK
 * only when the service's wake word shows it asleep. Other operations
 * (directories, seek) are messages through one bounce buffer. Requests
 * are serialized; there is no unlink or mkdir yet, which the protocol
 * does not have.
 */
#define _GNU_SOURCE
#include <errno.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/mman.h>
#include "lock.h"
#include "cubit_fd.h"
#include "cubit_fs_queue.h"
#include <cubit/debug.h>

#define SLOT_FILESYSTEM 1
#define SYSTEM_VOLUME "@nvme:0"        /* TODO: "@system" once aliases resolve */

enum {
	SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY = 41,
	SYSCALL_SUBMIT_VIA_ENDPOINT_CAPABILITY = 42,
	SYSCALL_CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY = 106,
	SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION = 108,
};

enum {
	OP_OPEN = 0x0001, OP_CLOSE = 0x0002, OP_OPEN_DIRECTORY = 0x0005,
	OP_SEEK = 0x0006, OP_READ_DIRECTORY_PAGE = 0x0007,
	OP_RENAME = 0x0008, OP_CLOSE_DIRECTORY = 0x0009, OP_FLUSH_FILE = 0x000C, OP_READ_AT = 0x000D,
	OP_WRITE_AT = 0x000E, OP_UNLINK = 0x0010, OP_MKDIR = 0x0011,
	OP_RMDIR = 0x0012,
};

enum {
	REPLY_OK = 0xF000, REPLY_NO_SPACE = 0xF002, REPLY_READ_ONLY = 0xF003,
	REPLY_OUT_OF_RANGE = 0xF004, REPLY_ACCESS_DENIED = 0xF007,
	REPLY_WRONG_OBJECT_TYPE = 0xF009, REPLY_ALREADY_EXISTS = 0xF00A,
	REPLY_NOT_FOUND = 0xF00B, REPLY_SHARING_VIOLATION = 0xF00F,
	REPLY_NOT_EMPTY = 0xF010,
};

#define PROTOCOL_VERSION 1
#define SEEK_FROM_END 2
#define BOUNCE_PAGES 64
#define BOUNCE_BYTES (BOUNCE_PAGES * 4096UL)
#define MAX_PATH 1024

/* CuBit.Messages.Message: 48 bytes. */
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

static volatile int fs_lock[1];
static unsigned char *bounce;
static uint64_t grant_slot, grant_generation;

/* Caller holds fs_lock. */
static int lend_bounce(void)
{
	if (bounce) return 0;
	void *p = mmap(0, BOUNCE_BYTES, PROT_READ | PROT_WRITE,
		MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	if (p == MAP_FAILED) return -ENOMEM;
	unsigned long slot = cubit(SYSCALL_CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY,
		SLOT_FILESYSTEM, (unsigned long)p, BOUNCE_PAGES, 1);
	if (slot == (unsigned long)-1) return -EACCES;  /* no filesystem capability */
	unsigned long gen = cubit(SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION,
		slot, 0, 0, 0);
	if (gen == (unsigned long)-1 || gen == 0) return -EACCES;
	grant_slot = slot;
	grant_generation = gen;
	bounce = p;
	return 0;
}

static uint32_t call(uint32_t label, uint8_t length, uint64_t w0, uint64_t w1,
	uint64_t w2, uint64_t w3, uint64_t *reply0)
{
	struct message m = { label, length, 0, 0, 0, { w0, w1, w2, w3 } };
	unsigned long tag = cubit(SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY,
		SLOT_FILESYSTEM, (unsigned long)&m, 0, 0);
	if (reply0) *reply0 = m.words[0];
	return (uint32_t)tag;
}

/* --- the request queue ----------------------------------------------------- */

#define ARENA_PAGES 256
#define ARENA_BYTES (ARENA_PAGES * 4096UL)
#define QUEUE_PAGES (FS_QUEUE_BYTES / 4096)
#define NO_COMPLETION_TOKEN (~0UL)
/* Waiting for an answer: spin briefly (the service is on another CPU and
 * answers within microseconds), then yield the CPU between looks (it may
 * share ours and need it to answer), then block in WAIT. */
#define ANSWER_SPINS 256
#define ANSWER_YIELDS 64
#define SYSCALL_YIELD 118
#define barrier() __asm__ __volatile__ ("" ::: "memory")

static unsigned char *queue, *arena, *dirty;

/* Directory pages fetched ahead (__cubit_dir_read_page). */
#define DIRECTORY_PAGE_BYTES 4096
#define DIRECTORY_BATCH_PAGES 64
#define DIRECTORY_PAGE_FLAGS_AT 8
#define DIRECTORY_PAGE_END 1
static struct {
	uint64_t handle;                /* 0: empty */
	unsigned count, next;
	unsigned char pages[DIRECTORY_BATCH_PAGES][DIRECTORY_PAGE_BYTES];
} dir_batch;
static int queue_refused;             /* the service gave none: messages */
static uint32_t q_produced, q_taken, q_reaped, q_answered, q_kicked;
static uint64_t q_token;

static inline volatile uint32_t *qword(unsigned offset)
{
	return (volatile uint32_t *)(queue + offset);
}

/* KICK (one-way, no reply): the submit syscall takes the tag (label, and
 * length in the high half) and the words in registers, as net.c's
 * submit() does, and a token for the completion (none here). */
static void kick(void)
{
	unsigned long ret;
	uint64_t tag = (uint64_t)OP_FS_KICK;
	register uint64_t r10 __asm__("r10") = 0;
	register uint64_t r8 __asm__("r8") = 0;
	register uint64_t r9 __asm__("r9") = 0;
	register uint64_t r12 __asm__("r12") = NO_COMPLETION_TOKEN;
	__asm__ __volatile__ ("syscall"
		: "=a"(ret), "+r"(r10), "+r"(r8), "+r"(r9), "+r"(r12)
		: "a"((unsigned long)SYSCALL_SUBMIT_VIA_ENDPOINT_CAPABILITY),
		  "D"((unsigned long)SLOT_FILESYSTEM), "S"(tag), "d"(0UL)
		: "rcx", "r11", "memory");
	(void)ret;
}

/* A grant of pages at p to the filesystem, in wire form; 0 on failure. */
static uint64_t lend(void *p, unsigned long pages)
{
	unsigned long slot = cubit(SYSCALL_CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY,
		SLOT_FILESYSTEM, (unsigned long)p, pages, 1);
	if (slot == (unsigned long)-1) return 0;
	unsigned long gen = cubit(SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION,
		slot, 0, 0, 0);
	if (gen == (unsigned long)-1 || gen == 0) return 0;
	return ((uint64_t)gen << 32) | slot;
}

/* Lend the queue and arena once; 1 if the queue is usable. Caller holds
 * fs_lock. */
static int queue_ready(void)
{
	if (queue) return 1;
	if (queue_refused) return 0;
	queue_refused = 1;
	unsigned char *q = mmap(0, FS_QUEUE_BYTES, PROT_READ | PROT_WRITE,
		MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	unsigned char *a = mmap(0, ARENA_BYTES, PROT_READ | PROT_WRITE,
		MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	unsigned char *d = mmap(0, FS_DIRTY_ARENA_BYTES, PROT_READ | PROT_WRITE,
		MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	if (q == MAP_FAILED || a == MAP_FAILED) return 0;
	uint64_t qref = lend(q, QUEUE_PAGES), aref = lend(a, ARENA_PAGES);
	/* Without a dirty arena there are no write delegations; still fine. */
	uint64_t dref = d == MAP_FAILED ? 0 : lend(d, FS_DIRTY_ARENA_BYTES / 4096);
	if (!qref || !aref ||
	    call(OP_FS_QUEUE, 4, qref, aref, ARENA_BYTES, dref, 0) != REPLY_OK)
		return 0;
	queue = q;
	arena = a;
	/* Touch the arenas once now, so no write or read later takes a first-
	 * touch fault on them (as Linux's page cache never faults a write). */
	memset(a, 0, ARENA_BYTES);
	if (dref) {
		memset(d, 0, FS_DIRTY_ARENA_BYTES);
		dirty = d;
	}
	queue_refused = 0;
	return 1;
}

/* Requests whose answers nobody waits for (close): their tokens carry
 * this bit, and their answers are dropped as they are reaped. */
#define ASYNC_TOKEN (1UL << 63)
static unsigned q_async;               /* such answers still to come */
/* An early write-back of the dirty arena under way (its token), and one
 * finished since the dirty map was last rebuilt. */
static uint64_t writeback_token;
static int writeback_done;
/* The last answer reaped: its token (without ASYNC_TOKEN) and, for an
 * open, the rights its handle carries (FS_RIGHTS_*). */
static uint64_t q_last_token;
static uint32_t q_last_rights;

/* Take the next answer, waiting for it: its token, status, value, spare. */
static uint64_t q_reap(uint32_t *status, uint64_t *value, uint64_t *spare)
{
	for (unsigned looks = 0;; looks++) {
		uint32_t produced = *qword(FS_COMPLETIONS_AT + FS_PRODUCED_AT);
		barrier();              /* the answer after its count */
		if (produced - q_reaped <= FS_SLOTS &&
		    produced - q_reaped >= q_answered - q_reaped)
			q_answered = produced;
		if (q_answered != q_reaped) break;
		if (looks < ANSWER_SPINS) {
			__builtin_ia32_pause();
		} else if (looks < ANSWER_SPINS + ANSWER_YIELDS) {
			cubit(SYSCALL_YIELD, 0, 0, 0, 0);
		} else {
			call(OP_FS_WAIT, 0, 0, 0, 0, 0, 0);   /* returns once one waits */
			looks = 0;
		}
	}
	const unsigned char *a = queue + FS_ANSWERS_AT +
		(size_t)(q_reaped & (FS_SLOTS - 1)) * FS_ANSWER_BYTES;
	uint64_t token;
	memcpy(&token, a + FS_TOKEN_AT, 8);
	memcpy(status, a + FS_STATUS_AT, 4);
	memcpy(&q_last_rights, a + FS_RIGHTS_AT, 4);
	if (value) memcpy(value, a + FS_VALUE_AT, 8);
	if (spare) memcpy(spare, a + FS_VALUE_AT + 8, 8);
	q_reaped++;
	barrier();                      /* copied out before the slot goes back */
	*qword(FS_COMPLETIONS_AT + FS_CONSUMED_AT) = q_reaped;
	if (token & ASYNC_TOKEN) q_async--;
	q_last_token = token & ~ASYNC_TOKEN;
	if (token == writeback_token) {  /* entries freed: our map is stale */
		writeback_token = 0;
		writeback_done = 1;
	}
	return token;
}

/* Reap answers already there, without waiting (only async ones can be:
 * callers hold fs_lock between a request and its answer). */
static void q_reap_ready(void)
{
	while (q_async) {
		uint32_t produced = *qword(FS_COMPLETIONS_AT + FS_PRODUCED_AT);
		barrier();
		if (produced == q_reaped) return;
		uint32_t status;
		q_reap(&status, 0, 0);
	}
}

/* Requests written while set are handed over with the next one written
 * while clear (so the service takes them in one pass). */
static int q_hold;

/* Put one request on the queue; its token. Room is made first: at most
 * FS_SLOTS - 1 answers are ever outstanding. */
static uint64_t q_submit(uint32_t op, uint32_t options, uint64_t handle,
	uint64_t position, uint64_t length, int async)
{
	while (q_produced - q_reaped >= FS_SLOTS - 1) {
		uint32_t status;
		q_reap(&status, 0, 0);          /* only async answers can be out */
	}
	uint64_t token = ++q_token | (async ? ASYNC_TOKEN : 0);
	if (async) q_async++;
	/* The service's consumed count: accepted if it releases nothing we
	 * did not write. */
	uint32_t consumed = *qword(FS_SUBMISSIONS_AT + FS_CONSUMED_AT);
	if (consumed - q_taken <= q_produced - q_taken) q_taken = consumed;
	unsigned char *e = queue + FS_REQUESTS_AT +
		(size_t)(q_produced & (FS_SLOTS - 1)) * FS_REQUEST_BYTES;
	uint64_t arena_offset = 0;
	memset(e, 0, FS_REQUEST_BYTES);
	memcpy(e + FS_TOKEN_AT, &token, 8);
	memcpy(e + FS_OPERATION_AT, &op, 4);
	memcpy(e + FS_OPTIONS_AT, &options, 4);
	memcpy(e + FS_HANDLE_AT, &handle, 8);
	memcpy(e + FS_POSITION_AT, &position, 8);
	memcpy(e + FS_LENGTH_AT, &length, 8);
	memcpy(e + FS_ARENA_OFFSET_AT, &arena_offset, 8);
	barrier();                      /* the entry before the count */
	q_produced++;
	if (q_hold) return token;
	*qword(FS_SUBMISSIONS_AT + FS_PRODUCED_AT) = q_produced;
	__atomic_thread_fence(__ATOMIC_SEQ_CST);  /* the count before the wake word */
	uint32_t wake = *qword(FS_SUBMISSIONS_AT + FS_WAKE_AT);
	if (wake && wake != q_kicked) {
		q_kicked = wake;
		kick();
	}
	return token;
}

/* One request through the queue, waiting for its answer: the answer's
 * status (a reply label), value and spare word. Caller holds fs_lock.
 * Answers come in request order; those of async requests before it are
 * dropped on the way. */
static uint32_t q_call(uint32_t op, uint32_t options, uint64_t handle,
	uint64_t position, uint64_t length, uint64_t *value, uint64_t *spare)
{
	uint64_t token = q_submit(op, options, handle, position, length, 0);
	uint32_t status = 0;
	uint64_t got;
	do {
		got = q_reap(&status, value, spare);
	} while (got != token && (got & ASYNC_TOKEN));
	if (got != token) status = 0;
	return status;
}

/* --- the client cache ------------------------------------------------------
 * File pages cached in this process while the service delegates reads to
 * a handle (cubit_fs_queue.h, "Read delegations"): no other handle can
 * write the file, and the service clears the delegation's valid word
 * before anything changes it. A cached read checks the word before and
 * after copying; if it went to zero meanwhile, the read goes to the
 * service instead. Pages are kept per file and version, so a reopen of an
 * unchanged file finds them. Caller holds fs_lock throughout. */

#define PAGE_BYTES 4096UL
#define CACHE_MAX_PAGES 32768           /* 128 MiB, allocated as used */
#define CACHE_CHUNK_PAGES 512           /* grown 2 MiB at a time */
#define HASH_BUCKETS 65536              /* a power of two */
#define CACHED_FILES 4096              /* files with pages here */
#define FILE_BUCKETS 8192               /* a power of two */
#define READAHEAD_MIN_PAGES 1           /* grows while reads are sequential */
#define READAHEAD_MAX_PAGES (ARENA_BYTES / PAGE_BYTES)
#define NO_PAGE (-1)

struct cfile {                  /* a file with pages here */
	uint64_t inode;         /* volume << 32 | inode; 0: unused */
	uint64_t version;       /* the version its pages hold */
	uint32_t epoch;         /* pages of other epochs are stale */
	int32_t next;           /* hash chain; -1 ends it */
};
struct cpage {
	int32_t file;           /* index into cfiles; -1: free */
	uint32_t epoch;
	uint64_t page;          /* page number within the file */
	int32_t next;           /* hash chain */
	uint8_t referenced;
	unsigned char *data;
};
struct chandle {                /* per service handle slot */
	uint64_t handle;        /* 0: none */
	int32_t file;           /* -1: not cached */
	uint64_t size;
	uint64_t next_offset;   /* where a sequential reader reads next */
	uint32_t readahead;     /* pages */
	int write;              /* a write delegation: writes stay here */
	/* Its open's name (cubit_name), kept to park it on close; 0: none. */
	char *name;
	uint32_t name_len;
	uint32_t generation;    /* the namespace generation before its open */
	uint64_t name_hash;
	/* Parked (closed, kept for a reopen): links as slot + 1, 0 ends. */
	int parked;
	uint32_t lru_prev, lru_next, chain;
	uint64_t park_token;    /* its PARK request */
	int service_parked;     /* PARK sent: the service keeps it read-only */
};

static struct cfile cfiles[CACHED_FILES];
static int cfile_clock, cfile_count;
static int32_t file_buckets[FILE_BUCKETS];
static struct cpage cpages[CACHE_MAX_PAGES];
static int cpage_count, cpage_clock;
static int32_t buckets[HASH_BUCKETS];
static int buckets_ready;
static struct chandle chandles[FS_MAXIMUM_DELEGATIONS];

static inline volatile uint32_t *delegation_valid(unsigned slot)
{
	return qword(FS_DELEGATIONS_AT + slot * FS_DELEGATION_BYTES +
		     FS_DELEGATION_VALID_AT);
}

static inline uint64_t delegation_long(unsigned slot, unsigned at)
{
	return *(volatile uint64_t *)(queue + FS_DELEGATIONS_AT +
		slot * FS_DELEGATION_BYTES + at);
}

/* A handle's slot, or -1 for one this cache does not track. */
static int slot_of(uint64_t handle)
{
	uint64_t code = handle & 0xFFFFFFFFUL;
	if (code == 0 || code > FS_MAXIMUM_DELEGATIONS) return -1;
	return (int)code - 1;
}

static inline unsigned bucket_of(int32_t file, uint64_t page)
{
	uint64_t h = (uint64_t)file * 0x9E3779B97F4A7C15UL ^ page * 0xC2B2AE3D27D4EB4FUL;
	return (unsigned)(h >> 40) & (HASH_BUCKETS - 1);
}

static void unlink_page(int i)
{
	unsigned b = bucket_of(cpages[i].file, cpages[i].page);
	for (int32_t *p = &buckets[b]; *p != NO_PAGE; p = &cpages[*p].next)
		if (*p == i) { *p = cpages[i].next; break; }
	cpages[i].file = -1;
}

static void cache_init(void)
{
	for (int b = 0; b < HASH_BUCKETS; b++) buckets[b] = NO_PAGE;
	for (int i = 0; i < CACHE_MAX_PAGES; i++) cpages[i].file = -1;
	for (int b = 0; b < FILE_BUCKETS; b++) file_buckets[b] = -1;
	buckets_ready = 1;
}

static struct cpage *find_page(int32_t file, uint64_t page)
{
	if (!buckets_ready) return 0;
	for (int32_t i = buckets[bucket_of(file, page)]; i != NO_PAGE; i = cpages[i].next)
		if (cpages[i].file == file && cpages[i].page == page) {
			if (cpages[i].epoch != cfiles[file].epoch) return 0;
			cpages[i].referenced = 1;
			return &cpages[i];
		}
	return 0;
}

/* A page for (file, page): free, stale, or taken from the clock. */
/* Pages of an older epoch (a truncated or changed file) are taken before
 * the cache grows: new memory costs a kernel allocation per page, a stale
 * page nothing. The clock is probed briefly; after a probe finds none,
 * probing pauses until an epoch moves again. */
#define STALE_PROBE_PAGES 16
static int stale_possible;

static int take_stale(void)
{
	if (!stale_possible || cpage_count == 0) return -1;
	for (int n = 0; n < STALE_PROBE_PAGES; n++) {
		int i = cpage_clock;
		cpage_clock = (cpage_clock + 1) % cpage_count;
		struct cpage *c = &cpages[i];
		if (c->file < 0 || c->epoch != cfiles[c->file].epoch) {
			if (c->file >= 0) unlink_page(i);
			return i;
		}
	}
	stale_possible = 0;
	return -1;
}

static struct cpage *new_page(int32_t file, uint64_t page)
{
	if (!buckets_ready) cache_init();
	int i = take_stale();
	if (i < 0 && cpage_count < CACHE_MAX_PAGES) {
		if (cpage_count % CACHE_CHUNK_PAGES == 0) {
			unsigned char *chunk = mmap(0, CACHE_CHUNK_PAGES * PAGE_BYTES,
				PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
			if (chunk != MAP_FAILED) {
				memset(chunk, 0, CACHE_CHUNK_PAGES * PAGE_BYTES);  /* no faults later */
				for (int k = 0; k < CACHE_CHUNK_PAGES; k++)
					cpages[cpage_count + k].data = chunk + k * PAGE_BYTES;
			}
		}
		if (cpages[cpage_count].data) i = cpage_count++;
	}
	if (i < 0) {
		if (cpage_count == 0) return 0;
		for (;;) {
			struct cpage *c = &cpages[cpage_clock];
			int stale = c->file < 0 || c->epoch != cfiles[c->file].epoch;
			if (stale || !c->referenced) { i = cpage_clock; break; }
			c->referenced = 0;
			cpage_clock = (cpage_clock + 1) % cpage_count;
		}
		cpage_clock = (cpage_clock + 1) % cpage_count;
		if (cpages[i].file >= 0) unlink_page(i);
	}
	cpages[i].file = file;
	cpages[i].epoch = cfiles[file].epoch;
	cpages[i].page = page;
	cpages[i].referenced = 1;
	unsigned b = bucket_of(file, page);
	cpages[i].next = buckets[b];
	buckets[b] = i;
	return &cpages[i];
}

static inline unsigned file_bucket(uint64_t inode)
{
	return (unsigned)((inode * 0x9E3779B97F4A7C15ULL) >> 40) & (FILE_BUCKETS - 1);
}

/* A cached file some open handle uses: never given to another inode. */
static int file_in_use(int32_t file)
{
	for (unsigned s = 0; s < FS_MAXIMUM_DELEGATIONS; s++)
		if (chandles[s].handle && chandles[s].file == file) return 1;
	return 0;
}

/* The cached file for inode at version; pages of another version go. */
static int32_t file_for(uint64_t inode, uint64_t version)
{
	if (!buckets_ready) cache_init();
	unsigned b = file_bucket(inode);
	for (int32_t i = file_buckets[b]; i >= 0; i = cfiles[i].next)
		if (cfiles[i].inode == inode) {
			if (cfiles[i].version != version) {
				cfiles[i].epoch++;
				stale_possible = 1;
				cfiles[i].version = version;
			}
			return i;
		}
	int32_t f;
	if (cfile_count < CACHED_FILES) {
		f = cfile_count++;
	} else {                        /* reuse one no handle uses; its pages go stale */
		do {
			f = cfile_clock;
			cfile_clock = (cfile_clock + 1) % CACHED_FILES;
		} while (file_in_use(f));
		int32_t *link = &file_buckets[file_bucket(cfiles[f].inode)];
		while (*link != f) link = &cfiles[*link].next;
		*link = cfiles[f].next;
	}
	cfiles[f].inode = inode;
	cfiles[f].version = version;
	cfiles[f].epoch++;
	stale_possible = 1;
	cfiles[f].next = file_buckets[b];
	file_buckets[b] = f;
	return f;
}

static void cache_delegated(struct chandle *c, int slot);

/* After open: cache under the handle's delegation, if it has one. */
static void cache_opened(uint64_t handle, uint64_t size)
{
	int slot = slot_of(handle);
	if (slot < 0) return;
	struct chandle *c = &chandles[slot];
	*c = (struct chandle){ .handle = handle, .file = -1, .size = size,
			       .readahead = READAHEAD_MIN_PAGES };
	cache_delegated(c, slot);
}

/* The handle's delegation, if it has one: its file's cache, size, mode. */
static void cache_delegated(struct chandle *c, int slot)
{
	if (!*delegation_valid((unsigned)slot)) return;
	barrier();
	uint64_t inode = delegation_long((unsigned)slot, FS_DELEGATION_INODE_AT);
	uint64_t version = delegation_long((unsigned)slot, FS_DELEGATION_VERSION_AT);
	uint64_t dsize = delegation_long((unsigned)slot, FS_DELEGATION_SIZE_AT);
	barrier();
	if (!*delegation_valid((unsigned)slot) || !inode) return;
	uint32_t mode = *qword(FS_DELEGATIONS_AT + (unsigned)slot * FS_DELEGATION_BYTES +
			       FS_DELEGATION_MODE_AT);
	c->file = file_for(inode, version);
	c->size = dsize;
	c->write = mode == FS_WRITE_DELEGATION && dirty != 0;
}

static struct chandle *cached(uint64_t handle)
{
	int slot = slot_of(handle);
	if (slot < 0 || !queue) return 0;
	struct chandle *c = &chandles[slot];
	if (c->handle != handle || c->file < 0) return 0;
	if (!*delegation_valid((unsigned)slot)) { c->file = -1; return 0; }
	return c;
}

/* Read through the cache: bytes read, or -1 to go to the service. */
static long cache_read(struct chandle *c, int slot, char *buf, size_t count,
	uint64_t offset)
{
	if (offset >= c->size) return 0;
	if (count > c->size - offset) count = (size_t)(c->size - offset);
	int sequential = offset == c->next_offset;
	c->readahead = sequential
		? (c->readahead * 2 > READAHEAD_MAX_PAGES ? READAHEAD_MAX_PAGES : c->readahead * 2)
		: READAHEAD_MIN_PAGES;
	size_t done = 0;
	while (done < count) {
		uint64_t at = offset + done;
		uint64_t page = at / PAGE_BYTES;
		size_t in = (size_t)(at % PAGE_BYTES);
		size_t take = PAGE_BYTES - in;
		if (take > count - done) take = count - done;
		struct cpage *p = find_page(c->file, page);
		if (!p) {
			/* Fill from the service: this page and the readahead after it. */
			uint64_t first = page * PAGE_BYTES;
			uint64_t want = (uint64_t)c->readahead * PAGE_BYTES;
			uint64_t rest = offset + count - first;   /* this request's pages */
			if (want < rest) want = rest;
			if (want > ARENA_BYTES) want = ARENA_BYTES;
			if (want > c->size - first) want = c->size - first;
			uint64_t got = 0;
			uint32_t label = q_call(FS_QUEUE_READ_AT, 0, c->handle, first, want, &got, 0);
			if (label != REPLY_OK || got < (at - first) + take || got > want) return -1;
			for (uint64_t off = 0; off < got; off += PAGE_BYTES) {
				struct cpage *n = find_page(c->file, page + off / PAGE_BYTES);
				if (!n) n = new_page(c->file, page + off / PAGE_BYTES);
				if (!n) return -1;
				uint64_t len = got - off < PAGE_BYTES ? got - off : PAGE_BYTES;
				memcpy(n->data, arena + off, (size_t)len);
				if (len < PAGE_BYTES) memset(n->data + len, 0, PAGE_BYTES - len);
			}
			p = find_page(c->file, page);
			if (!p) return -1;
		}
		memcpy(buf + done, p->data + in, take);
		done += take;
	}
	barrier();                      /* the copies before the second look */
	if (!*delegation_valid((unsigned)slot)) { c->file = -1; return -1; }
	c->next_offset = offset + done;
	return (long)done;
}

/* Our own write through a delegated handle: cached pages follow it. */
static void cache_wrote(uint64_t handle, const char *buf, size_t count,
	uint64_t offset)
{
	struct chandle *c = cached(handle);
	if (!c) return;
	int slot = slot_of(handle);
	for (size_t done = 0; done < count; ) {
		uint64_t at = offset + done;
		size_t in = (size_t)(at % PAGE_BYTES);
		size_t take = PAGE_BYTES - in;
		if (take > count - done) take = count - done;
		struct cpage *p = find_page(c->file, at / PAGE_BYTES);
		/* A whole page written is cached as it is, as Linux's page
		 * cache holds what was written. */
		if (!p && in == 0 && take == PAGE_BYTES)
			p = new_page(c->file, at / PAGE_BYTES);
		if (p) memcpy(p->data + in, buf + done, take);
		done += take;
	}
	if (offset + count > c->size) c->size = offset + count;
	/* The service moved the version on for this write; our pages have it. */
	cfiles[c->file].version = delegation_long((unsigned)slot, FS_DELEGATION_VERSION_AT);
}

/* --- buffered writes under a write delegation (cubit_fs_queue.h, "dirty
 * arena"). A written page is kept whole in the private cache and copied to
 * a dirty entry the service harvests on close, flush and write-back. An
 * entry is taken by compare-and-swap (sequence made odd) and released by
 * making it even; the delegation is checked again once it is held, so a
 * recall that raced the write is noticed and the write goes to the service
 * instead. */

#define DIRTY_MAP (FS_DIRTY_ENTRIES * 2)   /* a power of two */
static int32_t dirty_map[DIRTY_MAP];       /* (slot, page) -> entry + 1 */
static uint32_t dirty_cursor;
static unsigned dirty_tombstones;           /* map slots of harvested entries */
static unsigned dirty_used;                 /* entries we hold, as of our last look */

static inline volatile uint32_t *dword(unsigned e, unsigned at)
{
	return (volatile uint32_t *)(dirty + e * FS_DIRTY_ENTRY_BYTES + at);
}

/* A handle's dirty-entry tag (FS_DIRTY_SLOT_AT): its slot, and the low
 * bits of its generation above it. */
static inline uint32_t dirty_tag(uint64_t handle, int slot)
{
	uint32_t generation = (uint32_t)(handle >> 32) & ((1u << FS_TAG_GENERATION_BITS) - 1);
	return generation << FS_TAG_SLOT_BITS | (uint32_t)slot;
}

static inline unsigned dirty_hash(unsigned slot, uint32_t page)
{
	uint64_t h = ((uint64_t)slot << 32 | page) * 0x9E3779B97F4A7C15UL;
	return (unsigned)(h >> 40) & (DIRTY_MAP - 1);
}

/* The entry holding (slot, page) as we left it, or -1. */
static int dirty_find(unsigned slot, uint32_t page)
{
	for (unsigned i = dirty_hash(slot, page), n = 0; n < DIRTY_MAP;
	     i = (i + 1) & (DIRTY_MAP - 1), n++) {
		int32_t v = dirty_map[i];
		if (v == 0) return -1;
		if (v < 0) continue;
		unsigned e = (unsigned)v - 1;
		if (*dword(e, FS_DIRTY_SLOT_AT) == slot && *dword(e, FS_DIRTY_PAGE_AT) == page) {
			if (*dword(e, FS_DIRTY_SEQUENCE_AT) != 0) return (int)e;
			dirty_map[i] = -1;          /* the service took it */
			dirty_tombstones++;
			return -1;
		}
	}
	return -1;
}

static void dirty_remember(unsigned slot, uint32_t page, unsigned e)
{
	for (unsigned i = dirty_hash(slot, page), n = 0; n < DIRTY_MAP;
	     i = (i + 1) & (DIRTY_MAP - 1), n++)
		if (dirty_map[i] <= 0) {
			if (dirty_map[i] < 0) dirty_tombstones--;
			dirty_map[i] = (int32_t)e + 1;
			return;
		}
}

/* Rebuild the map from the entries still live in the arena: after the
 * service harvested them (write-back, flush), or when harvested entries'
 * tombstones would make lookups long. */
static void dirty_rebuild(void)
{
	memset(dirty_map, 0, sizeof dirty_map);
	dirty_tombstones = 0;
	dirty_used = 0;
	writeback_done = 0;
	for (unsigned e = 0; e < FS_DIRTY_ENTRIES; e++)
		if (*dword(e, FS_DIRTY_SEQUENCE_AT) != 0) {
			dirty_remember(*dword(e, FS_DIRTY_SLOT_AT), *dword(e, FS_DIRTY_PAGE_AT), e);
			dirty_used++;
		}
}

/* Take a free entry (sequence 0 -> 1); -1 if none is free. */
static int dirty_take_free(void)
{
	for (unsigned n = 0; n < FS_DIRTY_ENTRIES; n++) {
		unsigned e = dirty_cursor;
		dirty_cursor = (dirty_cursor + 1) % FS_DIRTY_ENTRIES;
		uint32_t zero = 0;
		if (*dword(e, FS_DIRTY_SEQUENCE_AT) == 0 &&
		    __atomic_compare_exchange_n(dword(e, FS_DIRTY_SEQUENCE_AT), &zero, 1,
				0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST))
			return (int)e;
	}
	return -1;
}

/* Write count bytes through the dirty arena; bytes written, or -1 to write
 * through the service instead (recalled, or nothing written yet). */
static long cache_write_back(struct chandle *c, int slot, const char *buf,
	size_t count, uint64_t offset)
{
	size_t done = 0;
	int written_back = 0;           /* asked for write-back since an entry */
	while (done < count) {
		uint64_t at = offset + done;
		uint32_t page = (uint32_t)(at / PAGE_BYTES);
		size_t in = (size_t)(at % PAGE_BYTES);
		size_t take = PAGE_BYTES - in;
		if (take > count - done) take = count - done;
		/* The page as it is now, whole, in the private cache. */
		struct cpage *p = find_page(c->file, page);
		if (!p) {
			uint64_t first = (uint64_t)page * PAGE_BYTES;
			if (in == 0 && take == PAGE_BYTES) {
				p = new_page(c->file, page);
			} else if (first >= c->size) {
				p = new_page(c->file, page);
				if (p) memset(p->data, 0, PAGE_BYTES);
			} else {
				uint64_t got = 0;
				if (q_call(FS_QUEUE_READ_AT, 0, c->handle, first, PAGE_BYTES,
					   &got, 0) != REPLY_OK || got > PAGE_BYTES)
					break;
				p = new_page(c->file, page);
				if (p) {
					memcpy(p->data, arena, (size_t)got);
					memset(p->data + got, 0, PAGE_BYTES - (size_t)got);
				}
			}
			if (!p) break;
		}
		/* An entry for the page, held (odd). Half the arena in use: the
		 * service starts writing it back while we go on (Linux's
		 * background write-back); a full arena still waits for it. */
		q_reap_ready();
		if (writeback_done || dirty_tombstones > DIRTY_MAP / 4) dirty_rebuild();
		if (!writeback_token && dirty_used >= FS_DIRTY_ENTRIES / 2)
			writeback_token = q_submit(FS_QUEUE_WRITEBACK, 0, c->handle, 0, 0, 1);
		uint32_t tag = dirty_tag(c->handle, slot);
		int e = dirty_find(tag, page);
		int fresh = 0;
		uint32_t held = 0;
		if (e >= 0) {
			uint32_t even = *dword((unsigned)e, FS_DIRTY_SEQUENCE_AT);
			if (even % 2 || !__atomic_compare_exchange_n(dword((unsigned)e,
					FS_DIRTY_SEQUENCE_AT), &even, even + 1, 0,
					__ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST))
				e = -1;                 /* taken meanwhile: a new one */
			else
				held = even + 1;
		}
		if (e < 0) {
			e = dirty_take_free();
			if (e < 0 && !written_back) {
				/* Full: the service writes back all our buffered pages,
				 * then this page gets an entry. */
				written_back = 1;
				if (q_call(FS_QUEUE_WRITEBACK, 0, c->handle, 0, 0, 0, 0) != REPLY_OK)
					break;
				dirty_rebuild();
				continue;
			}
			if (e < 0) break;       /* still full: the rest goes through */
			written_back = 0;
			fresh = 1;
			held = 1;
			dirty_used++;
		}
		if (!*delegation_valid((unsigned)slot)) {   /* recalled: undo */
			*dword((unsigned)e, FS_DIRTY_SEQUENCE_AT) = fresh ? 0 : held - 1;
			c->file = -1;
			break;
		}
		memcpy(p->data + in, buf + done, take);
		uint64_t end = at + take > c->size ? at + take : c->size;
		uint64_t first = (uint64_t)page * PAGE_BYTES;
		uint32_t stop = end - first >= PAGE_BYTES ? PAGE_BYTES : (uint32_t)(end - first);
		memcpy(dirty + FS_DIRTY_PAGES_AT + (size_t)e * FS_DIRTY_PAGE_BYTES, p->data, stop);
		*dword((unsigned)e, FS_DIRTY_SLOT_AT) = tag;
		*dword((unsigned)e, FS_DIRTY_PAGE_AT) = page;
		*(volatile uint16_t *)(dirty + (unsigned)e * FS_DIRTY_ENTRY_BYTES + FS_DIRTY_START_AT) = 0;
		*(volatile uint16_t *)(dirty + (unsigned)e * FS_DIRTY_ENTRY_BYTES + FS_DIRTY_STOP_AT) = (uint16_t)stop;
		if (fresh) dirty_remember(tag, page, (unsigned)e);
		__atomic_store_n(dword((unsigned)e, FS_DIRTY_SEQUENCE_AT), held + 1, __ATOMIC_RELEASE);
		if (end > c->size) c->size = end;
		done += take;
	}
	return done ? (long)done : -1;
}

/* --- parked handles (docs/filesystem-data-plane.md, "Metadata operations")
 * A closed file handle that may read, under a valid delegation, is kept
 * open ("parked", keyed by its normalized name) instead of closed: the
 * service drops its rights to reading (FS_QUEUE_PARK, answered
 * asynchronously). A write delegation stays, with its buffered pages,
 * which the service harvests later (write-back, commit, another open,
 * release), as Linux writes back after close; an unlink of the name
 * drops them unwritten if the file dies with it. A writable open of the
 * name closes our parked handle first, so it can be the file's only
 * handle (and so have a write delegation). A
 * later read-only open of the same name takes it back with no request
 * while (1) the queue's namespace generation still equals the one read
 * before the handle's open (the service moves it on before any unlink,
 * rename, mkdir, rmdir or change to this client's access policy), and
 * (2) its delegation is still valid for the same file (the service clears
 * it before any other handle writes the file, and when the handle is
 * released, as on revocation). At most PARKED_MAX are kept, least
 * recently parked closed first (async CLOSE). Caller holds fs_lock. */
#ifndef PARKED_MAX
#define PARKED_MAX 1024                /* 0: never park (A/B runs) */
#endif
#define PARK_BUCKETS 2048               /* a power of two */
#define NO_LINK 0                       /* links are slot + 1 */
static uint32_t park_buckets[PARK_BUCKETS];
static uint32_t park_newest, park_oldest;
static unsigned parked_count;

static uint64_t name_hash(const char *name, size_t len)
{
	uint64_t h = 0xCBF29CE484222325UL;     /* FNV-1a */
	for (size_t i = 0; i < len; i++) h = (h ^ (unsigned char)name[i]) * 0x100000001B3UL;
	return h;
}

static uint32_t *park_bucket(uint64_t hash)
{
	return &park_buckets[(hash >> 40) & (PARK_BUCKETS - 1)];
}

/* The parked handle for name, as a slot, or -1. */
static int park_find(const char *name, size_t len, uint64_t hash)
{
	for (uint32_t l = *park_bucket(hash); l != NO_LINK; l = chandles[l - 1].chain) {
		struct chandle *c = &chandles[l - 1];
		if (c->name_hash == hash && c->name_len == len && memcmp(c->name, name, len) == 0)
			return (int)l - 1;
	}
	return -1;
}

static void park_insert(int slot)
{
	struct chandle *c = &chandles[slot];
	uint32_t *b = park_bucket(c->name_hash);
	c->parked = 1;
	c->chain = *b;
	*b = (uint32_t)slot + 1;
	c->lru_prev = NO_LINK;
	c->lru_next = park_newest;
	if (park_newest != NO_LINK) chandles[park_newest - 1].lru_prev = (uint32_t)slot + 1;
	park_newest = (uint32_t)slot + 1;
	if (park_oldest == NO_LINK) park_oldest = park_newest;
	parked_count++;
}

static void park_remove(int slot)
{
	struct chandle *c = &chandles[slot];
	uint32_t *l = park_bucket(c->name_hash);
	while (*l != (uint32_t)slot + 1) l = &chandles[*l - 1].chain;
	*l = c->chain;
	if (c->lru_prev != NO_LINK) chandles[c->lru_prev - 1].lru_next = c->lru_next;
	else park_newest = c->lru_next;
	if (c->lru_next != NO_LINK) chandles[c->lru_next - 1].lru_prev = c->lru_prev;
	else park_oldest = c->lru_prev;
	c->parked = 0;
	parked_count--;
}

/* Forget slot's name and close its handle (async, as every close). */
static void handle_close(int slot)
{
	struct chandle *c = &chandles[slot];
	uint64_t handle = c->handle;
	if (c->parked) park_remove(slot);
	free(c->name);
	c->name = 0;
	c->handle = 0;
	q_submit(FS_QUEUE_CLOSE, 0, handle, 0, 0, 1);
}

/* Close the parked handle for name, if any (before the name changes).
 * With hold, the close goes with the caller's next queue request. */
static void park_drop(const char *name, size_t len, int hold)
{
	int slot = park_find(name, len, name_hash(name, len));
	if (slot < 0) return;
	q_hold = hold;
	handle_close(slot);
	q_hold = 0;
}

/* Park slot's handle at close; 0 if it cannot be. */
static int park(int slot)
{
	struct chandle *c = &chandles[slot];
	if (PARKED_MAX == 0 || !c->name || c->file < 0 ||
	    !*delegation_valid((unsigned)slot)) return 0;
	int other = park_find(c->name, c->name_len, c->name_hash);
	if (other >= 0) handle_close(other);    /* the newer handle is kept */
	if (parked_count >= PARKED_MAX) handle_close((int)park_oldest - 1);
	/* Parked before and reused since (read-only, still delegated): the
	 * service's side is unchanged, so no request. */
	if (!c->service_parked) {
		c->park_token = q_submit(FS_QUEUE_PARK, 0, c->handle, 0, 0, 1) & ~ASYNC_TOKEN;
		c->service_parked = 1;
	}
	park_insert(slot);
	return 1;
}

/* A read-only open of name through a parked handle: 1 with its handle and
 * size, or 0 to open it through the service. A parked handle that fails
 * the checks is closed. */
static int unpark(const char *name, size_t len, uint64_t *handle, uint64_t *size)
{
	if (!parked_count) return 0;
	int slot = park_find(name, len, name_hash(name, len));
	if (slot < 0) return 0;
	struct chandle *c = &chandles[slot];
	/* Its PARK answered first: the delegation is then a reader's. */
	while (q_last_token < c->park_token) {
		uint32_t status;
		q_reap(&status, 0, 0);
	}
	uint32_t generation = *qword(FS_SUBMISSIONS_AT + FS_NAMESPACE_GENERATION_AT);
	barrier();
	uint64_t inode = delegation_long((unsigned)slot, FS_DELEGATION_INODE_AT);
	barrier();
	if (generation != c->generation || !*delegation_valid((unsigned)slot) ||
	    c->file < 0 || inode != cfiles[c->file].inode) {
		handle_close(slot);
		return 0;
	}
	park_remove(slot);
	uint64_t written_size = c->size;
	c->next_offset = 0;
	c->readahead = READAHEAD_MIN_PAGES;
	c->file = -1;
	cache_delegated(c, slot);
	if (c->file < 0) {              /* revoked meanwhile */
		handle_close(slot);
		return 0;
	}
	/* Still a write delegation: pages we wrote may not have reached the
	 * service yet (they are harvested later), so our size is the file's. */
	if (c->write && written_size > c->size) c->size = written_size;
	*handle = c->handle;
	*size = c->size;
	return 1;
}

/* At exit: parked handles are closed, and every async answer is waited
 * for, so the service has handled them before the process goes. */
__attribute__((destructor)) static void file_fini(void)
{
	LOCK(fs_lock);
	if (queue) {
		while (park_oldest != NO_LINK) handle_close((int)park_oldest - 1);
		while (q_async) {
			uint32_t status;
			q_reap(&status, 0, 0);
		}
	}
	UNLOCK(fs_lock);
}

static long to_errno(uint32_t label)
{
	switch (label) {
	case REPLY_NOT_FOUND:         return -ENOENT;
	case REPLY_ACCESS_DENIED:     return -EACCES;
	case REPLY_WRONG_OBJECT_TYPE: return -ENOTDIR;
	case REPLY_ALREADY_EXISTS:    return -EEXIST;
	case REPLY_READ_ONLY:         return -EROFS;
	case REPLY_NO_SPACE:          return -ENOSPC;
	case REPLY_OUT_OF_RANGE:      return -EINVAL;
	case REPLY_NOT_EMPTY:         return -ENOTEMPTY;
	case REPLY_SHARING_VIOLATION: return -EBUSY;
	default:                      return -EIO;
	}
}

/* The CuBit name for a path; its length, or -errno. */
static long cubit_name(const char *path, char *out)
{
	size_t n = 0;
	if (!path || !*path) return -ENOENT;
	if (path[0] != '@') {
		memcpy(out, SYSTEM_VOLUME, sizeof SYSTEM_VOLUME - 1);
		n = sizeof SYSTEM_VOLUME - 1;
		out[n++] = '/';
	}
	for (const char *p = path; *p; ) {
		/* Drop empty and "." components; the service rejects "..". */
		if (*p == '/') { p++; continue; }
		const char *end = strchrnul(p, '/');
		size_t len = (size_t)(end - p);
		if (!(len == 1 && p[0] == '.')) {
			if (out[n - 1] != '/' && p != path) out[n++] = '/';
			if (n + len >= MAX_PATH) return -ENAMETOOLONG;
			memcpy(out + n, p, len);
			n += len;
		}
		p = end;
	}
	return (long)n;
}

hidden long __cubit_file_open(const char *path, int directory,
	uint64_t options, uint64_t *handle, uint64_t *size)
{
	char name[MAX_PATH];
	long len = cubit_name(path, name);
	if (len < 0) return len;
	LOCK(fs_lock);
	uint64_t h = 0, sz = 0;
	if (queue_ready()) {
		if (!directory && options == FS_OPEN_READ_ONLY &&
		    unpark(name, (size_t)len, &h, &sz)) {
			UNLOCK(fs_lock);
			*handle = h;
			if (size) *size = sz;
			return 0;
		}
		/* A writable open: our own parked handle for the name goes
		 * first (in the same pass), or the file would have another
		 * handle and this one no write delegation. */
		if (!directory && options != FS_OPEN_READ_ONLY)
			park_drop(name, (size_t)len, 1);
		/* Read before the open: a change to the namespace after it
		 * resolves the name leaves this generation behind. */
		uint32_t generation = *qword(FS_SUBMISSIONS_AT + FS_NAMESPACE_GENERATION_AT);
		barrier();
		memcpy(arena, name, (size_t)len);
		uint32_t label = directory
			? q_call(FS_QUEUE_OPEN_DIRECTORY, 0, 0, 0, (uint64_t)len, &h, 0)
			: q_call(FS_QUEUE_OPEN, (uint32_t)options, 0, 0, (uint64_t)len, &h, &sz);
		if (label == REPLY_OK && !directory) {
			cache_opened(h, sz);
			int slot = slot_of(h);
			/* A handle that may read can be parked at close. */
			if (slot >= 0 && (q_last_rights & FS_RIGHTS_READ) &&
			    (chandles[slot].name = malloc((size_t)len))) {
				memcpy(chandles[slot].name, name, (size_t)len);
				chandles[slot].name_len = (uint32_t)len;
				chandles[slot].name_hash = name_hash(name, (size_t)len);
				chandles[slot].generation = generation;
			}
		}
		UNLOCK(fs_lock);
		if (label != REPLY_OK) return to_errno(label);
		*handle = h;
		if (size) *size = sz;
		return 0;
	}
	long r = lend_bounce();
	if (r) { UNLOCK(fs_lock); return r; }
	memcpy(bounce, name, (size_t)len);
	uint32_t label = directory
		? call(OP_OPEN_DIRECTORY, 3, grant_slot, (uint64_t)len, grant_generation, 0, &h)
		: call(OP_OPEN, 4, grant_slot, (uint64_t)len, options, grant_generation, &h);
	if (label != REPLY_OK) {
		UNLOCK(fs_lock);
		if (label == REPLY_ACCESS_DENIED) {
			/* A request for authority the program lacks: say which
			 * (diagnostics console). */
			static const char pre[] = "cubit-libc: denied: ";
			cubit_debug_write(pre, sizeof pre - 1);
			cubit_debug_write(name, (size_t)len);
			cubit_debug_write("\n", 1);
		}
		return to_errno(label);
	}
	if (!directory) {
		label = call(OP_SEEK, 3, h, 0, SEEK_FROM_END, 0, &sz);
		if (label != REPLY_OK) {
			call(OP_CLOSE, 1, h, 0, 0, 0, 0);
			UNLOCK(fs_lock);
			return to_errno(label);
		}
	}
	UNLOCK(fs_lock);
	*handle = h;
	if (size) *size = sz;
	return 0;
}

hidden void __cubit_file_close(uint64_t handle, int directory)
{
	LOCK(fs_lock);
	if (directory && dir_batch.handle == handle) dir_batch.handle = 0;
	if (directory && queue) {
		q_submit(FS_QUEUE_CLOSE_DIRECTORY, 0, handle, 0, 0, 1);   /* no wait */
	} else if (!directory && queue) {
		/* Nobody waits for a close (Linux reports write-back errors at
		 * fsync, not close): its answer is dropped when reaped. */
		int slot = slot_of(handle);
		if (slot >= 0 && chandles[slot].handle == handle) {
			if (chandles[slot].parked || !park(slot)) handle_close(slot);
		} else {
			q_submit(FS_QUEUE_CLOSE, 0, handle, 0, 0, 1);
		}
	}
	else
		call(directory ? OP_CLOSE_DIRECTORY : OP_CLOSE, 1, handle, 0, 0, 0, 0);
	UNLOCK(fs_lock);
}

hidden long __cubit_file_read_at(uint64_t handle, void *buf, size_t count,
	uint64_t offset)
{
	size_t done = 0;
	LOCK(fs_lock);
	struct chandle *c = cached(handle);
	if (c) {
		long n = cache_read(c, slot_of(handle), buf, count, offset);
		if (n >= 0) { UNLOCK(fs_lock); return n; }
	}
	if (queue_ready()) {
		while (done < count) {
			size_t want = count - done > ARENA_BYTES ? ARENA_BYTES : count - done;
			uint64_t got = 0;
			uint32_t label = q_call(FS_QUEUE_READ_AT, 0, handle, offset + done,
				want, &got, 0);
			if (got > want) got = want;
			memcpy((char *)buf + done, arena, got);
			done += got;
			if (label != REPLY_OK) {
				UNLOCK(fs_lock);
				return done ? (long)done : to_errno(label);
			}
			if (got < want) break;          /* end of file */
		}
		UNLOCK(fs_lock);
		return (long)done;
	}
	long r = lend_bounce();
	if (r) { UNLOCK(fs_lock); return r; }
	while (done < count) {
		size_t want = count - done > BOUNCE_BYTES ? BOUNCE_BYTES : count - done;
		uint64_t got = 0;
		uint64_t ref = (grant_generation << 32) | grant_slot;
		uint32_t label = call(OP_READ_AT, 4, handle, ref, want, offset + done, &got);
		if (label != REPLY_OK) {
			if (got && got <= want) {
				memcpy((char *)buf + done, bounce, got);
				done += got;
			}
			UNLOCK(fs_lock);
			return done ? (long)done : to_errno(label);
		}
		if (got > want) got = want;
		memcpy((char *)buf + done, bounce, got);
		done += got;
		if (got < want) break;          /* end of file */
	}
	UNLOCK(fs_lock);
	return (long)done;
}

/* Positioned write of count bytes through the bounce buffer; the bytes
 * written, or -errno when nothing was. */
hidden long __cubit_file_write_at(uint64_t handle, const void *buf, size_t count,
	uint64_t offset)
{
	size_t done = 0;
	LOCK(fs_lock);
	struct chandle *c = cached(handle);
	if (c && c->write) {
		long n = cache_write_back(c, slot_of(handle), buf, count, offset);
		if (n >= 0 && (size_t)n == count) { UNLOCK(fs_lock); return n; }
		if (n > 0) done = (size_t)n;
	}
	if (queue_ready()) {
		while (done < count) {
			size_t want = count - done > ARENA_BYTES ? ARENA_BYTES : count - done;
			uint64_t put = 0;
			memcpy(arena, (const char *)buf + done, want);
			uint32_t label = q_call(FS_QUEUE_WRITE_AT, 0, handle, offset + done,
				want, &put, 0);
			if (put > want) put = want;
			cache_wrote(handle, (const char *)buf + done, (size_t)put, offset + done);
			done += put;
			if (label != REPLY_OK) {
				UNLOCK(fs_lock);
				return done ? (long)done : to_errno(label);
			}
			if (put < want) break;
		}
		UNLOCK(fs_lock);
		return (long)done;
	}
	long r = lend_bounce();
	if (r) { UNLOCK(fs_lock); return r; }
	while (done < count) {
		size_t want = count - done > BOUNCE_BYTES ? BOUNCE_BYTES : count - done;
		uint64_t put = 0;
		uint64_t ref = (grant_generation << 32) | grant_slot;
		memcpy(bounce, (const char *)buf + done, want);
		uint32_t label = call(OP_WRITE_AT, 4, handle, ref, want, offset + done, &put);
		if (put > want) put = want;
		done += put;
		if (label != REPLY_OK) {
			UNLOCK(fs_lock);
			return done ? (long)done : to_errno(label);
		}
		if (put < want) break;
	}
	UNLOCK(fs_lock);
	return (long)done;
}

/* Completed writes and metadata to the device's flush completion. */
hidden long __cubit_file_flush(uint64_t handle)
{
	LOCK(fs_lock);
	uint32_t label = queue
		? q_call(FS_QUEUE_FLUSH, 0, handle, 0, 0, 0, 0)
		: call(OP_FLUSH_FILE, 1, handle, 0, 0, 0, 0);
	if (dirty) dirty_rebuild();     /* the flush harvested this handle's */
	UNLOCK(fs_lock);
	return label == REPLY_OK ? 0 : to_errno(label);
}

/* Remove a file or an empty directory, or make a directory, by name. */
static long path_op(const char *path, uint32_t queue_op, uint32_t message_op)
{
	char name[MAX_PATH];
	long len = cubit_name(path, name);
	if (len < 0) return len;
	LOCK(fs_lock);
	uint32_t label;
	if (queue_ready()) {
		/* A parked handle for the name goes with the request (the name
		 * is about to change, and an unlinked file held open would
		 * linger): UNLINK closes it, dropping its buffered pages if the
		 * file dies with it; before RMDIR it is closed. */
		uint64_t parked = 0;
		if (queue_op == FS_QUEUE_UNLINK) {
			int slot = park_find(name, (size_t)len, name_hash(name, (size_t)len));
			if (slot >= 0) {
				parked = chandles[slot].handle;
				park_remove(slot);
				free(chandles[slot].name);
				chandles[slot].name = 0;
				chandles[slot].handle = 0;
			}
		} else if (queue_op == FS_QUEUE_RMDIR) {
			park_drop(name, (size_t)len, 1);
		}
		memcpy(arena, name, (size_t)len);
		label = q_call(queue_op, 0, parked, 0, (uint64_t)len, 0, 0);
	} else {
		long r = lend_bounce();
		if (r) { UNLOCK(fs_lock); return r; }
		memcpy(bounce, name, (size_t)len);
		label = call(message_op, 4, grant_slot, (uint64_t)len, 0, grant_generation, 0);
	}
	UNLOCK(fs_lock);
	return label == REPLY_OK ? 0 : to_errno(label);
}

hidden long __cubit_path_remove(const char *path, int kind)
{
	return kind == CUBIT_REMOVE_DIRECTORY
		? path_op(path, FS_QUEUE_RMDIR, OP_RMDIR)
		: path_op(path, FS_QUEUE_UNLINK, OP_UNLINK);
}

hidden long __cubit_path_mkdir(const char *path)
{
	return path_op(path, FS_QUEUE_MKDIR, OP_MKDIR);
}

/* Rename a file or directory (the service refuses an existing target:
 * -EEXIST, not POSIX replacement). By message: it is rare. */
hidden long __cubit_path_rename(const char *from, const char *to)
{
	char old_name[MAX_PATH], new_name[MAX_PATH];
	long old_len = cubit_name(from, old_name);
	if (old_len < 0) return old_len;
	long new_len = cubit_name(to, new_name);
	if (new_len < 0) return new_len;
	LOCK(fs_lock);
	if (queue) {                    /* the names are about to change */
		park_drop(old_name, (size_t)old_len, 0);
		park_drop(new_name, (size_t)new_len, 0);
	}
	long r = lend_bounce();
	if (r) { UNLOCK(fs_lock); return r; }
	memcpy(bounce, old_name, (size_t)old_len);
	memcpy(bounce + old_len, new_name, (size_t)new_len);
	uint32_t label = call(OP_RENAME, 4, grant_slot, (uint64_t)old_len,
		(uint64_t)new_len, grant_generation, 0);
	UNLOCK(fs_lock);
	return label == REPLY_OK ? 0 : to_errno(label);
}

/* One Directory.Page.V1 into page (4096 bytes). Through the queue, pages
 * come in batches (up to DIRECTORY_BATCH_PAGES per request) and are handed
 * out from a buffer, one per call, until the batch that ends the
 * directory is used up. */

hidden long __cubit_dir_read_page(uint64_t handle, void *page)
{
	LOCK(fs_lock);
	if (dir_batch.handle == handle && dir_batch.next < dir_batch.count) {
		memcpy(page, dir_batch.pages[dir_batch.next++], DIRECTORY_PAGE_BYTES);
		UNLOCK(fs_lock);
		return 0;
	}
	if (queue_ready()) {
		uint64_t filled = 0;
		uint32_t label = q_call(FS_QUEUE_READ_DIRECTORY, 0, handle, 0,
			DIRECTORY_BATCH_PAGES * DIRECTORY_PAGE_BYTES, &filled, 0);
		if (label == REPLY_OK && filled >= 1 && filled <= DIRECTORY_BATCH_PAGES) {
			memcpy(dir_batch.pages, arena, (size_t)filled * DIRECTORY_PAGE_BYTES);
			dir_batch.handle = handle;
			dir_batch.count = (unsigned)filled;
			dir_batch.next = 1;
			memcpy(page, dir_batch.pages[0], DIRECTORY_PAGE_BYTES);
			UNLOCK(fs_lock);
			return 0;
		}
		UNLOCK(fs_lock);
		return label == REPLY_OK ? -EIO : to_errno(label);
	}
	long r = lend_bounce();
	if (r) { UNLOCK(fs_lock); return r; }
	uint32_t label = call(OP_READ_DIRECTORY_PAGE, 4, handle, grant_slot,
		PROTOCOL_VERSION, grant_generation, 0);
	if (label == REPLY_OK) memcpy(page, bounce, 4096);
	UNLOCK(fs_lock);
	return label == REPLY_OK ? 0 : to_errno(label);
}
