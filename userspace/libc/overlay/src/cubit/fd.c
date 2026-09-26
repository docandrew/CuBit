/*
 * CuBit libc: descriptors are a local table of CuBit objects.
 *
 * There are no implicit Unix descriptors. A descriptor exists only for a
 * CuBit object the program has (docs/servo-port.md):
 *
 *   1, 2   the program's stdout and stderr streams (CuBit.Streams, typed
 *          text lines). A stream is created on first write; subscribers
 *          (a shell, a log collector) see it only if the program's manifest
 *          declares it, e.g. (stream stdout text 4). With no subscriber the
 *          records are dropped (the stream's DROP_OLDEST policy).
 *   0      none (no input stream is granted yet).
 *   3...   files and directories opened through filesystem.svc (file.c),
 *          read-only for now; pipes, which are in-process objects (a ring
 *          shared by both ends; threads use them to wake each other, e.g.
 *          mio's waker); connected socket pairs, two such rings (tokio's
 *          signal driver wakes itself through one; CuBit has no signals);
 *          TCP sockets over netstack (net.c).
 *
 * Streams are not terminals. The process's mailbox is owned by the libc:
 * a dispatcher thread receives every message and serves stream
 * subscriptions (and, later, file and socket replies).
 */
#define _GNU_SOURCE
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <stddef.h>
#include <stdlib.h>
#include <pthread.h>
#include <stdint.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include "lock.h"
#include "atomic.h"
#include "cubit_fd.h"

/* From userspace/c/cubit_streams.c (compiled into this libc). */
void cubit_stream_create(uint16_t stream_id, unsigned pages, uint16_t type_tag);
uint32_t cubit_stream_write(uint16_t stream_id, const void *data,
	uint32_t len, uint16_t type_tag);
int cubit_stream_handle_message(long from, const void *msg);
extern int cubit_stream_poll_on_write;

#define STREAM_STDOUT 2
#define STREAM_STDERR 3
#define TYPE_TEXT_LINE 1
#define STREAM_PAGES 4
#define MAX_RECORD 4096

enum fd_kind { FD_NONE, FD_STREAM_OUT, FD_FILE, FD_DIR, FD_PIPE_R, FD_PIPE_W, FD_PAIR, FD_TCP };

/* A pipe: an in-process ring shared by its read and write ends. */
#define PIPE_BYTES 65536
struct pipe_obj {
	volatile int lock[1];
	unsigned char buf[PIPE_BYTES];
	size_t head, len;
	int readers, writers;
};

/* A directory being listed: one Directory.Page.V1 and the next entry. */
struct dir_page {
	unsigned char page[4096];
	unsigned next, count;
	int loaded, ended;
};

struct fd_entry {
	enum fd_kind kind;
	uint16_t stream;
	int created;
	uint64_t handle;                /* filesystem.svc handle */
	uint64_t offset, size;
	int cloexec, flags;
	struct dir_page *dir;
	struct pipe_obj *pipe;          /* the ring this end reads (FD_PAIR) */
	struct pipe_obj *peer;          /* FD_PAIR: the ring this end writes */
	struct cubit_tcp *tcp;          /* FD_TCP */
};

#define MAX_FDS 1024
static struct fd_entry fds[MAX_FDS] = {
	[0] = { FD_NONE },
	[1] = { FD_STREAM_OUT, STREAM_STDOUT },
	[2] = { FD_STREAM_OUT, STREAM_STDERR },
};

/* Serializes descriptor allocation and each file's offset. */
static volatile int table_lock[1];

/* Serializes the stream table: application writers and the dispatcher. */
static volatile int stream_lock[1];

/* --- the mailbox dispatcher ---------------------------------------------- */

struct cubit_message {
	uint64_t tag;
	uint64_t authority;
	uint64_t words[4];
};

static void *dispatcher(void *unused)
{
	(void)unused;
	for (;;) {
		struct cubit_message m;
		unsigned long from;
		memset(&m, 0, sizeof m);
		__asm__ __volatile__ ("syscall" : "=a"(from)
			: "a"(17UL), "D"(&m) : "rcx", "r11", "memory");
		LOCK(stream_lock);
		cubit_stream_handle_message((long)from, &m);
		UNLOCK(stream_lock);
		/* Anything else is not addressed to this libc yet: dropped. */
	}
	return 0;
}

static void start_dispatcher(void)
{
	static volatile int started;
	if (a_swap(&started, 1)) return;
	pthread_attr_t attr;
	pthread_t t;
	pthread_attr_init(&attr);
	pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
	pthread_attr_setstacksize(&attr, 64 * 1024);
	pthread_create(&t, &attr, dispatcher, 0);
	pthread_attr_destroy(&attr);
}

/* Caller holds stream_lock. */
static void ensure_stream(struct fd_entry *e)
{
	if (e->created) return;
	cubit_stream_poll_on_write = 0;
	cubit_stream_create(e->stream, STREAM_PAGES, TYPE_TEXT_LINE);
	e->created = 1;
}

/* --- operations ----------------------------------------------------------- */

static struct fd_entry *lookup(int fd)
{
	if (fd < 0 || fd >= MAX_FDS) return 0;
	if (fds[fd].kind == FD_NONE) return 0;
	return &fds[fd];
}

/* --- readiness ------------------------------------------------------------ */

/* Bumped whenever a descriptor may have become ready (a pipe written, read
 * or closed); poll and blocking reads/writes wait on it as a futex. */
static volatile int fd_events;

static void readiness_changed(void);
hidden void __cubit_readiness_changed(void) { readiness_changed(); }
hidden int __cubit_readiness_seq(void) { return fd_events; }

static void readiness_changed(void)
{
	unsigned long r;
	a_inc(&fd_events);
	__asm__ __volatile__ ("syscall" : "=a"(r)
		: "a"(93UL), "D"(&fd_events), "S"(~0UL) : "rcx", "r11", "memory");
}

/* Wait until fd_events moves on from seq, or the deadline (kernel
 * milliseconds; ~0 = forever) passes. */
static void wait_readiness(int seq, unsigned long deadline)
{
	unsigned long r;
	register unsigned long r10 __asm__("r10") = 0;
	__asm__ __volatile__ ("syscall" : "=a"(r)
		: "a"(92UL), "D"(&fd_events), "S"((unsigned long)(unsigned)seq),
		  "d"(deadline), "r"(r10) : "rcx", "r11", "memory");
}

hidden void __cubit_readiness_wait(int seq, unsigned long deadline)
{
	wait_readiness(seq, deadline);
}

static unsigned long now_ms(void)
{
	unsigned long r;
	__asm__ __volatile__ ("syscall" : "=a"(r) : "a"(27UL) : "rcx", "r11", "memory");
	return r;
}

/* The lowest free descriptor, claimed as kind; -EMFILE if none. */
static int allocate(enum fd_kind kind)
{
	LOCK(table_lock);
	for (int fd = 3; fd < MAX_FDS; fd++) {
		if (fds[fd].kind == FD_NONE) {
			memset(&fds[fd], 0, sizeof fds[fd]);
			fds[fd].kind = kind;
			UNLOCK(table_lock);
			return fd;
		}
	}
	UNLOCK(table_lock);
	return -EMFILE;
}

hidden long __cubit_fd_pipe(int out[2], int flags)
{
	struct pipe_obj *p = calloc(1, sizeof *p);
	if (!p) return -ENOMEM;
	int r = allocate(FD_PIPE_R);
	if (r < 0) { free(p); return r; }
	int w = allocate(FD_PIPE_W);
	if (w < 0) { fds[r].kind = FD_NONE; free(p); return w; }
	p->readers = p->writers = 1;
	for (int i = 0; i < 2; i++) {
		struct fd_entry *e = &fds[i ? w : r];
		e->pipe = p;
		e->flags = (flags & O_NONBLOCK) | (i ? O_WRONLY : O_RDONLY);
		e->cloexec = (flags & O_CLOEXEC) != 0;
	}
	out[0] = r;
	out[1] = w;
	return 0;
}

static long pipe_write(struct fd_entry *e, const struct iovec *iov, int n)
{
	struct pipe_obj *p = e->kind == FD_PAIR ? e->peer : e->pipe;
	size_t want = 0, done = 0;
	for (int i = 0; i < n; i++) want += iov[i].iov_len;
	if (!want) return 0;
	for (;;) {
		int seq = fd_events;
		LOCK(p->lock);
		if (!p->readers) { UNLOCK(p->lock); return -EPIPE; }
		/* Copy what fits, in order; a short write is allowed. */
		for (int i = 0; i < n; i++) {
			const unsigned char *src = iov[i].iov_base;
			size_t off = 0;
			for (; off < iov[i].iov_len && p->len < PIPE_BYTES; off++) {
				p->buf[(p->head + p->len) % PIPE_BYTES] = src[off];
				p->len++;
				done++;
			}
			if (off < iov[i].iov_len) break;
		}
		UNLOCK(p->lock);
		if (done) { readiness_changed(); return (long)done; }
		if (e->flags & O_NONBLOCK) return -EAGAIN;
		wait_readiness(seq, ~0UL);
	}
}

static long pipe_read(struct fd_entry *e, void *buf, size_t n)
{
	struct pipe_obj *p = e->pipe;
	if (!n) return 0;
	for (;;) {
		int seq = fd_events;
		LOCK(p->lock);
		if (p->len) {
			size_t got = n < p->len ? n : p->len;
			for (size_t i = 0; i < got; i++)
				((unsigned char *)buf)[i] = p->buf[(p->head + i) % PIPE_BYTES];
			p->head = (p->head + got) % PIPE_BYTES;
			p->len -= got;
			UNLOCK(p->lock);
			readiness_changed();
			return (long)got;
		}
		int ended = !p->writers;
		UNLOCK(p->lock);
		if (ended) return 0;
		if (e->flags & O_NONBLOCK) return -EAGAIN;
		wait_readiness(seq, ~0UL);
	}
}

hidden long __cubit_fd_socket_tcp(int flags)
{
	struct cubit_tcp *t = __cubit_tcp_new();
	if (!t) return -ENOMEM;
	int fd = allocate(FD_TCP);
	if (fd < 0) { __cubit_tcp_close(t); return fd; }
	fds[fd].tcp = t;
	fds[fd].flags = (flags & O_NONBLOCK) | O_RDWR;
	fds[fd].cloexec = (flags & O_CLOEXEC) != 0;
	return fd;
}

/* The socket behind a descriptor, or NULL. */
hidden struct cubit_tcp *__cubit_fd_tcp(int fd, int *nonblock)
{
	struct fd_entry *e = lookup(fd);
	if (!e || e->kind != FD_TCP) return 0;
	if (nonblock) *nonblock = (e->flags & O_NONBLOCK) != 0;
	return e->tcp;
}

hidden int __cubit_fd_is_socket(int fd)
{
	struct fd_entry *e = lookup(fd);
	return e && (e->kind == FD_TCP || e->kind == FD_PAIR);
}

/* socketpair(AF_UNIX, SOCK_STREAM): end 0 reads ring a and writes ring b,
 * end 1 the reverse. */
hidden long __cubit_fd_socketpair(int out[2], int flags)
{
	struct pipe_obj *a = calloc(1, sizeof *a), *b = calloc(1, sizeof *b);
	if (!a || !b) { free(a); free(b); return -ENOMEM; }
	int x = allocate(FD_PAIR);
	if (x < 0) { free(a); free(b); return x; }
	int y = allocate(FD_PAIR);
	if (y < 0) { fds[x].kind = FD_NONE; free(a); free(b); return y; }
	a->readers = a->writers = b->readers = b->writers = 1;
	fds[x].pipe = a; fds[x].peer = b;
	fds[y].pipe = b; fds[y].peer = a;
	for (int i = 0; i < 2; i++) {
		struct fd_entry *e = &fds[i ? y : x];
		e->flags = (flags & O_NONBLOCK) | O_RDWR;
		e->cloexec = (flags & O_CLOEXEC) != 0;
	}
	out[0] = x;
	out[1] = y;
	return 0;
}

static void release_ring(struct pipe_obj *p, int reader)
{
	LOCK(p->lock);
	if (reader) p->readers--; else p->writers--;
	int last = !p->readers && !p->writers;
	UNLOCK(p->lock);
	if (last) free(p);
}

hidden long __cubit_fd_open(const char *path, int flags)
{
	if ((flags & O_ACCMODE) != O_RDONLY || (flags & (O_CREAT | O_TRUNC)))
		return -EROFS;              /* read-only for now */
	int directory = (flags & O_DIRECTORY) != 0;
	uint64_t handle, size = 0;
	long r = __cubit_file_open(path, directory, &handle, &size);
	if (r == -ENOTDIR && !directory && !(flags & O_NOFOLLOW)) {
		/* open(2) of a directory without O_DIRECTORY also succeeds. */
		directory = 1;
		r = __cubit_file_open(path, 1, &handle, &size);
	}
	if (r) return r;
	struct dir_page *page = 0;
	if (directory && !(page = calloc(1, sizeof *page))) {
		__cubit_file_close(handle, 1);
		return -ENOMEM;
	}
	int fd = allocate(directory ? FD_DIR : FD_FILE);
	if (fd < 0) {
		__cubit_file_close(handle, directory);
		free(page);
		return fd;
	}
	fds[fd].handle = handle;
	fds[fd].size = size;
	fds[fd].flags = flags;
	fds[fd].cloexec = (flags & O_CLOEXEC) != 0;
	fds[fd].dir = page;
	return fd;
}

/* stat(2) by name: open it, describe it, close it. */
hidden long __cubit_path_stat(const char *path, struct stat *st)
{
	uint64_t handle, size = 0;
	int directory = 0;
	long r = __cubit_file_open(path, 0, &handle, &size);
	if (r == -ENOTDIR) {
		directory = 1;
		r = __cubit_file_open(path, 1, &handle, &size);
	}
	if (r) return r;
	__cubit_file_close(handle, directory);
	memset(st, 0, sizeof *st);
	st->st_mode = directory ? (S_IFDIR | 0555) : (S_IFREG | 0444);
	st->st_nlink = 1;
	st->st_size = (off_t)size;
	st->st_blksize = 4096;
	st->st_blocks = (blkcnt_t)((size + 511) / 512);
	return 0;
}

hidden long __cubit_fd_writev(int fd, const struct iovec *iov, int n)
{
	struct fd_entry *e = lookup(fd);
	if (!e) return -EBADF;
	if (e->kind == FD_PIPE_W || e->kind == FD_PAIR) return pipe_write(e, iov, n);
	if (e->kind == FD_TCP)
		return __cubit_tcp_write(e->tcp, iov, n, (e->flags & O_NONBLOCK) != 0);
	if (e->kind != FD_STREAM_OUT) return e->kind == FD_DIR ? -EISDIR : -EBADF;
	long total = 0;
	int first = 0;
	LOCK(stream_lock);
	if (!e->created) first = 1;
	ensure_stream(e);
	for (int i = 0; i < n; i++) {
		const char *p = iov[i].iov_base;
		size_t left = iov[i].iov_len;
		while (left) {
			uint32_t chunk = left > MAX_RECORD ? MAX_RECORD : (uint32_t)left;
			cubit_stream_write(e->stream, p, chunk, TYPE_TEXT_LINE);
			p += chunk;
			left -= chunk;
		}
		total += (long)iov[i].iov_len;
	}
	UNLOCK(stream_lock);
	if (first) start_dispatcher();
	return total;
}

hidden long __cubit_fd_pread(int fd, void *buf, size_t n, off_t offset)
{
	struct fd_entry *e = lookup(fd);
	if (!e) return -EBADF;
	if (e->kind == FD_DIR) return -EISDIR;
	if (e->kind != FD_FILE) return -EINVAL;     /* output streams */
	if (offset < 0) return -EINVAL;
	if (!n || (uint64_t)offset >= e->size) return 0;
	return __cubit_file_read_at(e->handle, buf, n, (uint64_t)offset);
}

hidden long __cubit_fd_read(int fd, void *buf, size_t n)
{
	struct fd_entry *e = lookup(fd);
	if (!e) return -EBADF;
	if (e->kind == FD_PIPE_R || e->kind == FD_PAIR) return pipe_read(e, buf, n);
	if (e->kind == FD_TCP)
		return __cubit_tcp_read(e->tcp, buf, n, (e->flags & O_NONBLOCK) != 0);
	if (e->kind != FD_FILE) return __cubit_fd_pread(fd, buf, n, 0);
	LOCK(table_lock);
	uint64_t at = e->offset;
	UNLOCK(table_lock);
	long got = __cubit_fd_pread(fd, buf, n, (off_t)at);
	if (got > 0) {
		LOCK(table_lock);
		e->offset = at + (uint64_t)got;
		UNLOCK(table_lock);
	}
	return got;
}

hidden long __cubit_fd_lseek(int fd, off_t offset, int whence)
{
	struct fd_entry *e = lookup(fd);
	if (!e) return -EBADF;
	if (e->kind == FD_STREAM_OUT) return -ESPIPE;
	if (e->kind == FD_DIR) {
		if (offset || whence != SEEK_SET) return -EINVAL;
		return -ENOSYS;                 /* rewinddir: reopen instead */
	}
	LOCK(table_lock);
	int64_t base = whence == SEEK_SET ? 0
		: whence == SEEK_CUR ? (int64_t)e->offset
		: whence == SEEK_END ? (int64_t)e->size : -1;
	if (base < 0 || base + offset < 0) {
		UNLOCK(table_lock);
		return -EINVAL;
	}
	e->offset = (uint64_t)(base + offset);
	UNLOCK(table_lock);
	return (long)(base + offset);
}

hidden long __cubit_fd_close(int fd)
{
	struct fd_entry *e = lookup(fd);
	if (!e) return -EBADF;
	if (e->kind == FD_FILE || e->kind == FD_DIR)
		__cubit_file_close(e->handle, e->kind == FD_DIR);
	if (e->kind == FD_PIPE_R || e->kind == FD_PIPE_W) {
		release_ring(e->pipe, e->kind == FD_PIPE_R);
		e->pipe = 0;
		readiness_changed();
	}
	if (e->kind == FD_TCP) {
		__cubit_tcp_close(e->tcp);
		e->tcp = 0;
	}
	if (e->kind == FD_PAIR) {
		release_ring(e->pipe, 1);
		release_ring(e->peer, 0);
		e->pipe = e->peer = 0;
		readiness_changed();
	}
	free(e->dir);
	LOCK(table_lock);
	e->dir = 0;
	e->kind = FD_NONE;
	UNLOCK(table_lock);
	return 0;
}

hidden long __cubit_fd_fstat(int fd, struct stat *st)
{
	struct fd_entry *e = lookup(fd);
	if (!e) return -EBADF;
	memset(st, 0, sizeof *st);
	st->st_nlink = 1;
	st->st_blksize = 4096;
	switch (e->kind) {
	case FD_FILE:
		st->st_mode = S_IFREG | 0444;
		st->st_size = (off_t)e->size;
		st->st_blocks = (blkcnt_t)((e->size + 511) / 512);
		st->st_ino = e->handle;
		break;
	case FD_DIR:
		st->st_mode = S_IFDIR | 0555;
		st->st_ino = e->handle;
		break;
	case FD_PIPE_R:
	case FD_PIPE_W:
		st->st_mode = S_IFIFO | 0600;
		break;
	case FD_PAIR:
	case FD_TCP:
		st->st_mode = S_IFSOCK | 0600;
		break;
	default:
		st->st_mode = S_IFIFO | 0200;   /* a write-only stream */
	}
	return 0;
}

/* dup: a second descriptor for the same object, at the lowest free
 * number >= min (or exactly at target when target >= 0). Streams, pipes
 * and socket pairs share their object; files and directories cannot be
 * duplicated yet (their filesystem.svc handle has one owner). */
hidden long __cubit_fd_dup(int fd, int min, int target, int cloexec)
{
	struct fd_entry *e = lookup(fd);
	if (!e) return -EBADF;
	if (e->kind == FD_FILE || e->kind == FD_DIR || e->kind == FD_TCP)
		return -EOPNOTSUPP;
	if (target >= MAX_FDS || min >= MAX_FDS) return -EINVAL;
	if (target >= 0 && target == fd) return fd;
	if (target >= 0 && fds[target].kind != FD_NONE) __cubit_fd_close(target);
	LOCK(table_lock);
	int to = target;
	if (to < 0)
		for (to = min < 0 ? 0 : min; to < MAX_FDS && fds[to].kind != FD_NONE; to++) ;
	if (to >= MAX_FDS || fds[to].kind != FD_NONE) {
		UNLOCK(table_lock);
		return -EMFILE;
	}
	fds[to] = *e;
	fds[to].cloexec = cloexec;
	struct pipe_obj *rings[2] = { e->pipe, e->kind == FD_PAIR ? e->peer : 0 };
	UNLOCK(table_lock);
	for (int i = 0; i < 2; i++) {
		if (!rings[i]) continue;
		LOCK(rings[i]->lock);
		if (e->kind == FD_PIPE_R || (e->kind == FD_PAIR && i == 0)) rings[i]->readers++;
		else rings[i]->writers++;
		UNLOCK(rings[i]->lock);
	}
	return to;
}

hidden long __cubit_fd_fcntl(int fd, int cmd, long arg)
{
	struct fd_entry *e = lookup(fd);
	if (!e) return -EBADF;
	switch (cmd) {
	case F_GETFD: return e->cloexec ? FD_CLOEXEC : 0;
	case F_SETFD: e->cloexec = (arg & FD_CLOEXEC) != 0; return 0;
	case F_GETFL: return e->kind == FD_STREAM_OUT ? O_WRONLY : (e->flags & ~O_CLOEXEC);
	case F_SETFL:                       /* only pipes block */
		e->flags = (e->flags & ~O_NONBLOCK) | ((int)arg & O_NONBLOCK);
		return 0;
	case F_DUPFD:         return __cubit_fd_dup(fd, (int)arg, -1, 0);
	case F_DUPFD_CLOEXEC: return __cubit_fd_dup(fd, (int)arg, -1, 1);
	default:
		report_unsupported("fcntl", cmd);
		return -EINVAL;
	}
}

/* getdents64 over Directory.Page.V1 (cubit-filesystems.ads). */
struct page_entry {
	uint64_t object_hint, size;
	uint16_t name_length;
	uint8_t kind, flags;
	uint32_t reserved;
	char name[256];
};

struct linux_dirent64 {
	uint64_t d_ino;
	int64_t d_off;
	unsigned short d_reclen;
	unsigned char d_type;
	char d_name[];
};

hidden long __cubit_fd_getdents(int fd, void *buf, size_t count)
{
	struct fd_entry *e = lookup(fd);
	if (!e) return -EBADF;
	if (e->kind != FD_DIR) return -ENOTDIR;
	struct dir_page *d = e->dir;
	size_t used = 0;
	for (;;) {
		if (!d->loaded || d->next >= d->count) {
			if (d->ended) break;
			long r = __cubit_dir_read_page(e->handle, d->page);
			if (r) return used ? (long)used : r;
			uint16_t header = *(uint16_t *)(d->page + 2);
			uint16_t entry = *(uint16_t *)(d->page + 4);
			d->count = *(uint16_t *)(d->page + 6);
			uint32_t flags = *(uint32_t *)(d->page + 8);
			if (header != 32 || entry != 280 || d->count > 14) return -EIO;
			d->next = 0;
			d->loaded = 1;
			d->ended = (flags & 1) != 0;
			if (!d->count) continue;
		}
		const struct page_entry *p =
			(const void *)(d->page + 32 + d->next * 280);
		size_t len = p->name_length > 255 ? 255 : p->name_length;
		size_t reclen = (offsetof(struct linux_dirent64, d_name) + len + 1 + 7) & ~7UL;
		if (used + reclen > count) {
			if (!used) return -EINVAL;
			break;
		}
		struct linux_dirent64 *out = (void *)((char *)buf + used);
		out->d_ino = p->object_hint ? p->object_hint : 1;
		out->d_off = (int64_t)(used + reclen);
		out->d_reclen = (unsigned short)reclen;
		out->d_type = p->kind == 1 ? DT_REG : p->kind == 2 ? DT_DIR
			: p->kind == 3 ? DT_LNK : DT_UNKNOWN;
		memcpy(out->d_name, p->name, len);
		out->d_name[len] = 0;
		used += reclen;
		d->next++;
	}
	return (long)used;
}

/* Readiness of the descriptors' CuBit objects. An output stream never
 * blocks (it drops the oldest records instead), so it is always writable;
 * a file is always readable; a pipe end is ready when it has data (or room)
 * or its other end is closed; a descriptor with no object is invalid. */
static long scan(struct pollfd *p, unsigned long n)
{
	long ready = 0;
	for (unsigned long i = 0; i < n; i++) {
		short re = 0;
		struct fd_entry *e = p[i].fd < 0 ? 0 : lookup(p[i].fd);
		if (p[i].fd < 0) re = 0;
		else if (!e) re = POLLNVAL;
		else if (e->kind == FD_STREAM_OUT)
			re = p[i].events & (POLLOUT | POLLWRNORM);
		else if (e->kind == FD_PIPE_R) {
			struct pipe_obj *q = e->pipe;
			LOCK(q->lock);
			if (q->len) re = p[i].events & (POLLIN | POLLRDNORM);
			if (!q->writers) re |= POLLHUP;
			UNLOCK(q->lock);
		} else if (e->kind == FD_PIPE_W) {
			struct pipe_obj *q = e->pipe;
			LOCK(q->lock);
			if (q->len < PIPE_BYTES) re = p[i].events & (POLLOUT | POLLWRNORM);
			if (!q->readers) re |= POLLERR;
			UNLOCK(q->lock);
		} else if (e->kind == FD_TCP) {
			re = __cubit_tcp_poll(e->tcp, p[i].events);
		} else if (e->kind == FD_PAIR) {
			struct pipe_obj *in = e->pipe, *out = e->peer;
			LOCK(in->lock);
			if (in->len) re = p[i].events & (POLLIN | POLLRDNORM);
			if (!in->writers) re |= POLLHUP | (p[i].events & (POLLIN | POLLRDHUP));
			UNLOCK(in->lock);
			LOCK(out->lock);
			if (out->len < PIPE_BYTES && out->readers)
				re |= p[i].events & (POLLOUT | POLLWRNORM);
			UNLOCK(out->lock);
		} else
			re = p[i].events & (POLLIN | POLLRDNORM);  /* files never block */
		p[i].revents = re;
		if (re) ready++;
	}
	return ready;
}

/* poll: wait until a descriptor is ready or the deadline passes (kernel
 * milliseconds; ~0 = forever, 0 = do not wait). */
hidden long __cubit_fd_poll(struct pollfd *p, unsigned long n, unsigned long deadline)
{
	for (;;) {
		int seq = fd_events;
		long ready = scan(p, n);
		if (ready || !deadline || (deadline != ~0UL && now_ms() >= deadline))
			return ready;
		wait_readiness(seq, deadline);
	}
}
