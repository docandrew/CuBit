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
	OP_CLOSE_DIRECTORY = 0x0009, OP_FLUSH_FILE = 0x000C, OP_READ_AT = 0x000D,
	OP_WRITE_AT = 0x000E,
};

enum {
	REPLY_OK = 0xF000, REPLY_NO_SPACE = 0xF002, REPLY_READ_ONLY = 0xF003,
	REPLY_OUT_OF_RANGE = 0xF004, REPLY_ACCESS_DENIED = 0xF007,
	REPLY_WRONG_OBJECT_TYPE = 0xF009, REPLY_ALREADY_EXISTS = 0xF00A,
	REPLY_NOT_FOUND = 0xF00B,
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
/* Answers usually come within microseconds: look this many times before
 * blocking in WAIT. */
#define ANSWER_SPINS 20000
#define barrier() __asm__ __volatile__ ("" ::: "memory")

static unsigned char *queue, *arena;
static int queue_refused;             /* the service gave none: messages */
static uint32_t q_produced, q_taken, q_reaped, q_answered, q_kicked;
static uint64_t q_token;

static inline volatile uint32_t *qword(unsigned offset)
{
	return (volatile uint32_t *)(queue + offset);
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
	if (q == MAP_FAILED || a == MAP_FAILED) return 0;
	uint64_t qref = lend(q, QUEUE_PAGES), aref = lend(a, ARENA_PAGES);
	if (!qref || !aref ||
	    call(OP_FS_QUEUE, 3, qref, aref, ARENA_BYTES, 0, 0) != REPLY_OK)
		return 0;
	queue = q;
	arena = a;
	queue_refused = 0;
	return 1;
}

/* One request through the queue, waiting for its answer: the answer's
 * status (a reply label), value and spare word. Caller holds fs_lock and
 * the queue is ready; requests are serialized, so the answer is the next. */
/* TEMPORARY diagnostics (fs-bench): cycles per operation, kicks, waits. */
static uint64_t diag_cycles[8], diag_count[8], diag_kicks, diag_waits, diag_calls;
static void diag_report(void)
{
	char line[256];
	int n = snprintf(line, sizeof line,
		"fs-queue: calls=%lu kicks=%lu waits=%lu open=%lu/%lu close=%lu/%lu read=%lu/%lu write=%lu/%lu\n",
		(unsigned long)diag_calls, (unsigned long)diag_kicks, (unsigned long)diag_waits,
		(unsigned long)diag_count[1], (unsigned long)(diag_count[1] ? diag_cycles[1] / diag_count[1] : 0),
		(unsigned long)diag_count[2], (unsigned long)(diag_count[2] ? diag_cycles[2] / diag_count[2] : 0),
		(unsigned long)diag_count[3], (unsigned long)(diag_count[3] ? diag_cycles[3] / diag_count[3] : 0),
		(unsigned long)diag_count[4], (unsigned long)(diag_count[4] ? diag_cycles[4] / diag_count[4] : 0));
	if (n > 0) cubit_debug_write(line, (size_t)n);
}

static uint32_t q_call(uint32_t op, uint32_t options, uint64_t handle,
	uint64_t position, uint64_t length, uint64_t *value, uint64_t *spare)
{
	uint64_t diag_start = __builtin_ia32_rdtsc();
	uint64_t token = ++q_token;
	/* The service's consumed count: accepted if it releases nothing we
	 * did not write. One request is out at a time, so there is room. */
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
	*qword(FS_SUBMISSIONS_AT + FS_PRODUCED_AT) = q_produced;
	__atomic_thread_fence(__ATOMIC_SEQ_CST);  /* the count before the wake word */
	uint32_t wake = *qword(FS_SUBMISSIONS_AT + FS_WAKE_AT);
	if (wake && wake != q_kicked) {
		diag_kicks++;
		q_kicked = wake;
		struct message m = { OP_FS_KICK, 0, 0, 0, 0, { 0, 0, 0, 0 } };
		cubit(SYSCALL_SUBMIT_VIA_ENDPOINT_CAPABILITY, SLOT_FILESYSTEM,
		      (unsigned long)&m, NO_COMPLETION_TOKEN, 0);
	}
	for (unsigned spins = 0;; spins++) {
		uint32_t produced = *qword(FS_COMPLETIONS_AT + FS_PRODUCED_AT);
		barrier();              /* the answer after its count */
		if (produced - q_reaped <= FS_SLOTS &&
		    produced - q_reaped >= q_answered - q_reaped)
			q_answered = produced;
		if (q_answered != q_reaped) break;
		if (spins >= ANSWER_SPINS) {
			diag_waits++;
			call(OP_FS_WAIT, 0, 0, 0, 0, 0, 0);   /* returns once one waits */
			spins = 0;
		} else {
			__builtin_ia32_pause();
		}
	}
	const unsigned char *a = queue + FS_ANSWERS_AT +
		(size_t)(q_reaped & (FS_SLOTS - 1)) * FS_ANSWER_BYTES;
	uint64_t got_token;
	uint32_t status;
	memcpy(&got_token, a + FS_TOKEN_AT, 8);
	memcpy(&status, a + FS_STATUS_AT, 4);
	if (value) memcpy(value, a + FS_VALUE_AT, 8);
	if (spare) memcpy(spare, a + FS_VALUE_AT + 8, 8);
	q_reaped++;
	barrier();                      /* copied out before the slot goes back */
	*qword(FS_COMPLETIONS_AT + FS_CONSUMED_AT) = q_reaped;
	if (op < 8) {
		diag_cycles[op] += __builtin_ia32_rdtsc() - diag_start;
		diag_count[op]++;
	}
	if (++diag_calls % 2048 == 0) diag_report();
	return got_token == token ? status : 0;
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
#define CACHED_FILES 256
#define READAHEAD_MIN_PAGES 1           /* grows while reads are sequential */
#define READAHEAD_MAX_PAGES (ARENA_BYTES / PAGE_BYTES)
#define NO_PAGE (-1)

struct cfile {                  /* a file with pages here */
	uint64_t inode;         /* volume << 32 | inode; 0: unused */
	uint64_t version;       /* the version its pages hold */
	uint32_t epoch;         /* pages of other epochs are stale */
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
	/* TEMPORARY diagnostics (fs-bench): reads served, filled, bypassed. */
	uint32_t hits, fills, bypassed, delegated;
};

static struct cfile cfiles[CACHED_FILES];
static int cfile_clock;
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
static struct cpage *new_page(int32_t file, uint64_t page)
{
	if (!buckets_ready) {
		for (int b = 0; b < HASH_BUCKETS; b++) buckets[b] = NO_PAGE;
		for (int i = 0; i < CACHE_MAX_PAGES; i++) cpages[i].file = -1;
		buckets_ready = 1;
	}
	int i = -1;
	if (cpage_count < CACHE_MAX_PAGES) {
		if (cpage_count % CACHE_CHUNK_PAGES == 0) {
			unsigned char *chunk = mmap(0, CACHE_CHUNK_PAGES * PAGE_BYTES,
				PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
			if (chunk != MAP_FAILED)
				for (int k = 0; k < CACHE_CHUNK_PAGES; k++)
					cpages[cpage_count + k].data = chunk + k * PAGE_BYTES;
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

/* The cached file for inode at version; pages of another version go. */
static int32_t file_for(uint64_t inode, uint64_t version)
{
	int free = -1;
	for (int i = 0; i < CACHED_FILES; i++) {
		if (cfiles[i].inode == inode) {
			if (cfiles[i].version != version) {
				cfiles[i].epoch++;
				cfiles[i].version = version;
			}
			return i;
		}
		if (free < 0 && cfiles[i].inode == 0) free = i;
	}
	if (free < 0) {                 /* reuse one; its pages go stale */
		free = cfile_clock;
		cfile_clock = (cfile_clock + 1) % CACHED_FILES;
	}
	cfiles[free].inode = inode;
	cfiles[free].version = version;
	cfiles[free].epoch++;
	return free;
}

/* After open: cache under the handle's delegation, if it has one. */
static void cache_opened(uint64_t handle, uint64_t size)
{
	int slot = slot_of(handle);
	if (slot < 0) return;
	struct chandle *c = &chandles[slot];
	*c = (struct chandle){ .handle = handle, .file = -1, .size = size,
			       .readahead = READAHEAD_MIN_PAGES };
	if (!*delegation_valid((unsigned)slot)) return;
	c->delegated = 1;
	barrier();
	uint64_t inode = delegation_long((unsigned)slot, FS_DELEGATION_INODE_AT);
	uint64_t version = delegation_long((unsigned)slot, FS_DELEGATION_VERSION_AT);
	uint64_t dsize = delegation_long((unsigned)slot, FS_DELEGATION_SIZE_AT);
	barrier();
	if (!*delegation_valid((unsigned)slot) || !inode) return;
	c->file = file_for(inode, version);
	c->size = dsize;
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
		if (p) c->hits++; else c->fills++;
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
	if (!directory && queue_ready()) {
		memcpy(arena, name, (size_t)len);
		uint32_t label = q_call(FS_QUEUE_OPEN, (uint32_t)options, 0, 0,
			(uint64_t)len, &h, &sz);
		if (label == REPLY_OK) cache_opened(h, sz);
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
	if (!directory && queue) {
		int slot = slot_of(handle);
		if (slot >= 0 && chandles[slot].handle == handle) {
			struct chandle *c = &chandles[slot];
			if (c->hits + c->fills + c->bypassed >= 256) {   /* TEMPORARY */
				char line[128];
				int n = snprintf(line, sizeof line,
					"fs-cache: delegated=%u hits=%u fills=%u bypassed=%u\n",
					c->delegated, c->hits, c->fills, c->bypassed);
				if (n > 0) cubit_debug_write(line, (size_t)n);
			}
			chandles[slot].handle = 0;
		}
		q_call(FS_QUEUE_CLOSE, 0, handle, 0, 0, 0, 0);
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
	{
		int slot = slot_of(handle);
		if (slot >= 0 && chandles[slot].handle == handle) chandles[slot].bypassed++;
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
	UNLOCK(fs_lock);
	return label == REPLY_OK ? 0 : to_errno(label);
}

/* One Directory.Page.V1 into page (4096 bytes). */
hidden long __cubit_dir_read_page(uint64_t handle, void *page)
{
	LOCK(fs_lock);
	long r = lend_bounce();
	if (r) { UNLOCK(fs_lock); return r; }
	uint32_t label = call(OP_READ_DIRECTORY_PAGE, 4, handle, grant_slot,
		PROTOCOL_VERSION, grant_generation, 0);
	if (label == REPLY_OK) memcpy(page, bounce, 4096);
	UNLOCK(fs_lock);
	return label == REPLY_OK ? 0 : to_errno(label);
}
