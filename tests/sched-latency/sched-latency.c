/*
 * Scheduler latency benchmark, modelled on Con Kolivas's interbench
 * (docs/scheduler.md, "Benchmark before implementation"). The same source
 * runs on CuBit (musl over CuBit syscalls) and on Linux (static musl).
 *
 * Each simulated latency-sensitive workload runs alone against each
 * background load in turn. Everything here is an ordinary application
 * thread: nothing is pinned, and nothing asks for a scheduling class.
 *
 * Workloads (the probe):
 *   wake         a thread blocked in FUTEX_WAIT is woken by another thread
 *                at most every WAKE_PERIOD_US; latency is the waker's
 *                timestamp (just before FUTEX_WAKE) to the waiter running
 *                again. The waker waits for each wake to be observed, and
 *                wake and interactive pacing skip grid points already past.
 *   interactive  a request/response round trip every INTERACTIVE_PERIOD_US
 *                to another process: on CuBit a synchronous IPC call to
 *                bench-ipc-server (its echo), on Linux a pipe ping-pong
 *                with a forked child
 *   frame        a FRAME_PERIOD_US loop: sleep to the period start, do
 *                FRAME_WORK_US of calibrated work; latency is the wakeup's
 *                lateness against the period start
 *   audio        the same with AUDIO_PERIOD_US and AUDIO_WORK_US
 *
 * Background loads:
 *   none
 *   burn         CPUs + 1 threads each spinning on calibrated work
 *   spam         CPUs / 2 thread pairs, each a futex ping-pong with
 *                SPAM_SPIN_US of work per turn (a wake and a sleep every
 *                few microseconds)
 *   poll         one thread busy-polling a flag for POLL_SPIN_US, then
 *                sched_yield, forever (a polling service)
 *   io           one thread: IO_BLOCK_BYTES pwrites over IO_SPAN_BLOCKS
 *                blocks of one file, fsync every IO_FSYNC_EVERY writes
 *
 * One line per (workload, load):
 *   sched-latency: workload=W load=L samples=N p50_us= p99_us= max_us=
 *                  missed= bg_rate= bg_unit=
 * then "sched-latency: done". "missed" is, for wake and interactive, the
 * samples over MISS_LATENCY_US (plus failed echoes); for frame and audio, periods whose work did not finish before
 * the next period start (late periods are not dropped: the next one starts
 * late, and its lateness is a sample).
 */
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <sched.h>
#include <stdarg.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/syscall.h>
#include <time.h>
#include <unistd.h>
#ifdef CUBIT
#include <cubit/debug.h>
#define DEFAULT_IO_PATH "@nvme:0/sched-latency/io.dat"
#else
#include <sys/wait.h>
#define DEFAULT_IO_PATH "/tmp/sched-latency-io.dat"
#endif

/* --- parameters ------------------------------------------------------------ */

#define US_PER_MS 1000
#define US_PER_S 1000000.0
#define NS_PER_US 1000
#define NS_PER_S 1000000000L
#define MS_PER_S 1000

/* -DRUN_DIVISOR=N shortens every run (and the warmup) N times: smoke tests. */
#ifndef RUN_DIVISOR
#define RUN_DIVISOR 1
#endif
#define WARMUP_MS (250 / RUN_DIVISOR)  /* load running before the probe starts */

#define WAKE_PERIOD_US 1000
#define WAKE_RUN_MS (2000 / RUN_DIVISOR)
#define INTERACTIVE_PERIOD_US 1000
#define INTERACTIVE_RUN_MS (2000 / RUN_DIVISOR)
#define FRAME_PERIOD_US 16000       /* 62.5 Hz: whole milliseconds for CuBit */
#define FRAME_WORK_US 2000
#define FRAME_RUN_MS (4000 / RUN_DIVISOR)
#define AUDIO_PERIOD_US 5000
#define AUDIO_WORK_US 500
#define AUDIO_RUN_MS (2000 / RUN_DIVISOR)
#define MISS_LATENCY_US 1000.0      /* docs/input-latency.md's 1 ms target */
#define MAX_SAMPLES 4096
#define WAKE_WAIT_TIMEOUT_MS 100

#define MAX_CPUS 16
#define BURN_CHUNK_UNITS 1000
#define SPAM_SPIN_US 2
#define SPAM_WAIT_TIMEOUT_MS 10
#define POLL_SPIN_US 50
#define IO_BLOCK_BYTES 4096
#define IO_SPAN_BLOCKS 64
#define IO_FSYNC_EVERY 16
#define IO_FILE_MODE 0644

#define CALIBRATE_MS 1000
#define WORK_CALIBRATE_UNITS 2000000
#define WORK_CALIBRATE_ROUNDS 5
#define PERCENT_MEDIAN 50
#define PERCENT_TAIL 99
#define PEER_WAIT_MS 5000
#define PEER_RETRY_MS 10
#define LINE_BYTES 320
#define CACHE_LINE 64

/* --- output ---------------------------------------------------------------- */

/* The serial console: CuBit's debug console, Linux's stdout (the console). */
static void report(const char *fmt, ...)
{
	char line[LINE_BYTES];
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

/* --- time ------------------------------------------------------------------ */

static int compare_double(const void *a, const void *b)
{
	double x = *(const double *)a, y = *(const double *)b;
	return (x > y) - (x < y);
}


/*
 * The pacing grid: targets are anchor_us + k * period (whole milliseconds).
 *
 * On CuBit, clock_gettime and sleeps count the kernel's millisecond clock.
 * CPU 0's timer ticks advance it from the TSC, lazily and sometimes by more
 * than one, and sleep deadlines expire at those same ticks. So timestamps
 * here are TSC readings, and the millisecond clock's TSC origin and rate
 * are fitted from many observed edges: the clock never runs ahead of the
 * TSC, so the TSC at which a millisecond value is first seen is an upper
 * bound on when that millisecond truly began, and the lower envelope of
 * (first seen - m * rate) is the origin. A target's millisecond is then
 * exact, and lateness counts from when the millisecond truly began, so it
 * includes the clock's own update lag (reported as ms_lag_us).
 */
static double anchor_us;
static long anchor_ms;

#ifdef CUBIT
#define MAX_EDGES 2048
#define ANCHOR_MS 100               /* the origin refit before each pair */
#define FIT_WINDOW_DIVISOR 4        /* rate: the first and last quarter */

static double tsc_per_us;
static double tsc_origin;           /* TSC at millisecond 0 */
static long edge_ms[MAX_EDGES];
static double edge_tsc[MAX_EDGES];
static int edges;

static long monotonic_ms(void)
{
	struct timespec t;
	clock_gettime(CLOCK_MONOTONIC, &t);
	return t.tv_sec * MS_PER_S + t.tv_nsec / (NS_PER_S / MS_PER_S);
}

static uint64_t tsc(void)
{
	uint32_t lo, hi;
	__asm__ __volatile__ ("lfence; rdtsc; lfence" : "=a"(lo), "=d"(hi) :: "memory");
	return (uint64_t)hi << 32 | lo;
}

static double now_us(void)
{
	return (double)tsc() / tsc_per_us;
}

/* Records the TSC at which each new millisecond value is first seen. */
static void observe_edges(long window_ms)
{
	long previous = monotonic_ms(), end = previous + window_ms;
	edges = 0;
	while (edges < MAX_EDGES) {
		long ms = monotonic_ms();
		if (ms == previous)
			continue;
		edge_tsc[edges] = (double)tsc();
		edge_ms[edges++] = ms;
		previous = ms;
		if (ms >= end)
			break;
	}
}

/* The smallest first-seen - m * rate over edges [from, to); its index. */
static double envelope(int from, int to, double ticks_per_ms, int *at)
{
	double best = edge_tsc[from] - edge_ms[from] * ticks_per_ms;
	*at = from;
	for (int i = from + 1; i < to; i++) {
		double v = edge_tsc[i] - edge_ms[i] * ticks_per_ms;
		if (v < best) {
			best = v;
			*at = i;
		}
	}
	return best;
}

static void set_origin(void)
{
	int at;
	tsc_origin = envelope(0, edges, tsc_per_us * US_PER_MS, &at);
	anchor_ms = edge_ms[edges - 1];
	anchor_us = (tsc_origin + anchor_ms * tsc_per_us * US_PER_MS) / tsc_per_us;
}

static void calibrate_clock(void)
{
	observe_edges(CALIBRATE_MS);
	double rate = (edge_tsc[edges - 1] - edge_tsc[0]) / (edge_ms[edges - 1] - edge_ms[0]);
	int quarter = edges / FIT_WINDOW_DIVISOR > 0 ? edges / FIT_WINDOW_DIVISOR : 1;
	int first, last;
	double early = envelope(0, quarter, rate, &first);
	double late = envelope(edges - quarter, edges, rate, &last);
	rate += (late - early) / (edge_ms[last] - edge_ms[first]);
	tsc_per_us = rate / US_PER_MS;
	set_origin();
	/* How far each first sighting trails the fitted boundary. */
	static double lag[MAX_EDGES];
	for (int i = 0; i < edges; i++)
		lag[i] = (edge_tsc[i] - tsc_origin) / tsc_per_us - (double)edge_ms[i] * US_PER_MS;
	qsort(lag, (size_t)edges, sizeof lag[0], compare_double);
	report("sched-latency: clock tsc ticks_per_us=%.3f ms_edges=%d ms_lag_us p50=%.1f"
		" p99=%.1f max=%.1f\n", tsc_per_us, edges, lag[edges / 2],
		lag[(edges * PERCENT_TAIL) / 100], lag[edges - 1]);
}

/* Refits the origin (the rate stays), before each pair's load starts. */
static void set_anchor(void)
{
	observe_edges(ANCHOR_MS);
	set_origin();
}

static void sleep_until(double target_us)
{
	long ms = anchor_ms + (long)((target_us - anchor_us) / US_PER_MS + 0.5);
	struct timespec t = { ms / MS_PER_S, (ms % MS_PER_S) * (NS_PER_S / MS_PER_S) };
	while (clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &t, 0) == EINTR) {}
}
#else
static double now_us(void)
{
	struct timespec t;
	clock_gettime(CLOCK_MONOTONIC, &t);
	return t.tv_sec * US_PER_S + (double)t.tv_nsec / NS_PER_US;
}

static void calibrate_clock(void)
{
	report("sched-latency: clock clock_gettime(CLOCK_MONOTONIC)\n");
}

static void set_anchor(void)
{
	anchor_us = now_us();
	anchor_ms = (long)(anchor_us / US_PER_MS);
}

static void sleep_until(double target_us)
{
	long ns = (long)(target_us * NS_PER_US);
	struct timespec t = { ns / NS_PER_S, ns % NS_PER_S };
	while (clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &t, 0) == EINTR) {}
}
#endif

static void sleep_ms(long ms)
{
	struct timespec t = { ms / MS_PER_S, (ms % MS_PER_S) * (NS_PER_S / MS_PER_S) };
	while (nanosleep(&t, &t) == EINTR) {}
}

/* --- calibrated work --------------------------------------------------------- */

static double units_per_us;

/* CPU work that the compiler cannot remove: a linear congruential chain. */
static uint64_t work(uint64_t units, uint64_t seed)
{
	volatile uint64_t sink;
	uint64_t x = seed;
	for (uint64_t i = 0; i < units; i++)
		x = x * 6364136223846793005ull + 1442695040888963407ull;
	sink = x;
	return sink;
}

/* The fastest of several runs, with nothing else running. */
static void calibrate_work(void)
{
	double best = 0;
	for (int r = 0; r < WORK_CALIBRATE_ROUNDS; r++) {
		double t0 = now_us();
		work(WORK_CALIBRATE_UNITS, (uint64_t)r);
		double rate = WORK_CALIBRATE_UNITS / (now_us() - t0);
		if (rate > best)
			best = rate;
	}
	units_per_us = best;
	report("sched-latency: work units_per_us=%.1f\n", units_per_us);
}

static void work_us(double us)
{
	work((uint64_t)(us * units_per_us), 1);
}

static void spin_us(double us)
{
	double end = now_us() + us;
	while (now_us() < end) {}
}

/* --- futexes ----------------------------------------------------------------- */

#define FUTEX_WAIT_OP 0
#define FUTEX_WAKE_OP 1
#define FUTEX_PRIVATE_FLAG 128

static void futex_wait(atomic_uint *word, unsigned expected, long timeout_ms)
{
	struct timespec t = { timeout_ms / MS_PER_S,
		(timeout_ms % MS_PER_S) * (NS_PER_S / MS_PER_S) };
	syscall(SYS_futex, word, FUTEX_WAIT_OP | FUTEX_PRIVATE_FLAG, expected, &t, 0, 0);
}

static void futex_wake(atomic_uint *word, int count)
{
	syscall(SYS_futex, word, FUTEX_WAKE_OP | FUTEX_PRIVATE_FLAG, count, 0, 0, 0);
}

/* --- samples ------------------------------------------------------------------ */

struct result {
	double samples[MAX_SAMPLES];
	int count;
	long missed;
};

static void add_sample(struct result *r, double us)
{
	if (r->count < MAX_SAMPLES)
		r->samples[r->count++] = us;
}

/* Nearest rank. */
static double percentile(const double *sorted, int n, int percent)
{
	if (n == 0)
		return 0;
	int rank = (n * percent + 99) / 100;
	return sorted[rank < 1 ? 0 : rank - 1];
}

/* --- background loads ------------------------------------------------------ */

struct counter {
	_Alignas(CACHE_LINE) atomic_ulong value;
};

static int cpus;
static atomic_int bg_stop;
static struct counter bg_counts[MAX_CPUS * 2 + 2];
static int bg_thread_count;         /* pool workers 1.. run the load */

struct spam_pair {
	_Alignas(CACHE_LINE) atomic_uint turn;
};
static struct spam_pair spam_pairs[MAX_CPUS];

static const char *io_path = DEFAULT_IO_PATH;
static int io_fd = -1;

static void *burn_thread(void *arg)
{
	struct counter *c = arg;
	uint64_t x = 1;
	while (!atomic_load_explicit(&bg_stop, memory_order_relaxed)) {
		x = work(BURN_CHUNK_UNITS, x);
		atomic_fetch_add_explicit(&c->value, 1, memory_order_relaxed);
	}
	return 0;
}

/* Thread index i of a pair owns turn value i % 2. */
static void *spam_thread(void *arg)
{
	long index = (long)arg;
	struct spam_pair *p = &spam_pairs[index / 2];
	unsigned mine = (unsigned)(index % 2), other = 1 - mine;
	struct counter *c = &bg_counts[index];
	while (!atomic_load(&bg_stop)) {
		unsigned turn = atomic_load(&p->turn);
		if (turn != mine) {
			futex_wait(&p->turn, turn, SPAM_WAIT_TIMEOUT_MS);
			continue;
		}
		spin_us(SPAM_SPIN_US);
		atomic_store(&p->turn, other);
		futex_wake(&p->turn, 1);
		atomic_fetch_add_explicit(&c->value, 1, memory_order_relaxed);
	}
	return 0;
}

static atomic_int poll_flag;

static void *poll_thread(void *arg)
{
	struct counter *c = arg;
	while (!atomic_load_explicit(&bg_stop, memory_order_relaxed)) {
		double end = now_us() + POLL_SPIN_US;
		while (now_us() < end && !atomic_load_explicit(&poll_flag, memory_order_relaxed)) {}
		sched_yield();
		atomic_fetch_add_explicit(&c->value, 1, memory_order_relaxed);
	}
	return 0;
}

static unsigned char io_block[IO_BLOCK_BYTES];

static void *io_thread(void *arg)
{
	struct counter *c = arg;
	unsigned long n = 0;
	while (!atomic_load_explicit(&bg_stop, memory_order_relaxed)) {
		io_block[0] = (unsigned char)n;
		off_t at = (off_t)(n % IO_SPAN_BLOCKS) * IO_BLOCK_BYTES;
		if (pwrite(io_fd, io_block, IO_BLOCK_BYTES, at) != IO_BLOCK_BYTES) {
			report("sched-latency: io load pwrite failed errno=%d\n", errno);
			break;
		}
		n++;
		if (n % IO_FSYNC_EVERY == 0)
			fsync(io_fd);
		atomic_fetch_add_explicit(&c->value, 1, memory_order_relaxed);
	}
	return 0;
}

/* spam runs last: on the 2026-09-29 kernel it can panic the guest. */
enum load_kind { LOAD_NONE, LOAD_BURN, LOAD_POLL, LOAD_IO, LOAD_SPAM, LOAD_COUNT };

static const char *const load_names[LOAD_COUNT] = { "none", "burn", "poll", "io", "spam" };
static const char *const load_units[LOAD_COUNT] = {
	"-", "kunits/s", "yields/s", "writes/s", "handoffs/s" };

/*
 * A fixed pool of threads, created once: background loads and the waker run
 * on them. Creating and joining threads for every pair is not what this
 * measures, and on the 2026-09-29 kernel THREAD_CREATE began refusing after
 * about 36 create/join cycles in some runs (see README).
 * Worker 0 is the waker; workers 1.. run load threads.
 */
#define POOL_WAIT_MS 1000
#define POOL_SIZE (MAX_CPUS + 2)

struct worker {
	_Alignas(CACHE_LINE) atomic_uint go, busy;
	void *(*fn)(void *);
	void *arg;
};
static struct worker pool[POOL_SIZE];
static int pool_size;

static void *worker_main(void *p)
{
	struct worker *w = p;
	unsigned seen = 0;
	for (;;) {
		unsigned go;
		while ((go = atomic_load(&w->go)) == seen)
			futex_wait(&w->go, seen, POOL_WAIT_MS);
		seen = go;
		w->fn(w->arg);
		atomic_store(&w->busy, 0);
		futex_wake(&w->busy, 1);
	}
	return 0;
}

static int start_pool(int size)
{
	for (pool_size = 0; pool_size < size; pool_size++) {
		pthread_t t;
		int error = pthread_create(&t, 0, worker_main, &pool[pool_size]);
		if (error) {
			report("sched-latency: FAIL pool pthread_create error=%d at %d\n",
				error, pool_size);
			return 0;
		}
		pthread_detach(t);
	}
	return 1;
}

static void run_on(int index, void *(*fn)(void *), void *arg)
{
	struct worker *w = &pool[index];
	w->fn = fn;
	w->arg = arg;
	atomic_store(&w->busy, 1);
	atomic_fetch_add(&w->go, 1);
	futex_wake(&w->go, 1);
}

static void join_worker(int index)
{
	struct worker *w = &pool[index];
	unsigned busy;
	while ((busy = atomic_load(&w->busy)) != 0)
		futex_wait(&w->busy, busy, POOL_WAIT_MS);
}

static void spawn(void *(*fn)(void *), void *arg)
{
	if (bg_thread_count + 1 >= pool_size) {
		report("sched-latency: FAIL load needs more than %d threads\n", pool_size - 1);
		return;
	}
	run_on(++bg_thread_count, fn, arg);
}

/* Starts a load; false if it cannot run here. */
static int start_load(enum load_kind kind)
{
	atomic_store(&bg_stop, 0);
	atomic_store(&poll_flag, 0);
	bg_thread_count = 0;
	for (size_t i = 0; i < sizeof bg_counts / sizeof bg_counts[0]; i++)
		atomic_store(&bg_counts[i].value, 0);
	switch (kind) {
	case LOAD_NONE:
	case LOAD_COUNT:
		break;
	case LOAD_BURN:
		for (int i = 0; i < cpus + 1; i++)
			spawn(burn_thread, &bg_counts[i]);
		break;
	case LOAD_SPAM:
		for (int p = 0; p < (cpus / 2 > 0 ? cpus / 2 : 1); p++) {
			atomic_store(&spam_pairs[p].turn, 0);
			spawn(spam_thread, (void *)(long)(2 * p));
			spawn(spam_thread, (void *)(long)(2 * p + 1));
		}
		break;
	case LOAD_POLL:
		spawn(poll_thread, &bg_counts[0]);
		break;
	case LOAD_IO:
		if (io_fd < 0) {
			io_fd = open(io_path, O_RDWR | O_CREAT, IO_FILE_MODE);
			if (io_fd < 0) {
				report("sched-latency: io load unavailable: open %s errno=%d\n",
					io_path, errno);
				return 0;
			}
		}
		spawn(io_thread, &bg_counts[0]);
		break;
	}
	return 1;
}

static unsigned long load_progress(void)
{
	unsigned long sum = 0;
	for (size_t i = 0; i < sizeof bg_counts / sizeof bg_counts[0]; i++)
		sum += atomic_load(&bg_counts[i].value);
	return sum;
}

static void stop_load(void)
{
	atomic_store(&bg_stop, 1);
	atomic_store(&poll_flag, 1);
	for (int p = 0; p < MAX_CPUS; p++) {
		atomic_store(&spam_pairs[p].turn, 2);
		futex_wake(&spam_pairs[p].turn, 2);
	}
	for (int i = 1; i <= bg_thread_count; i++)
		join_worker(i);
	bg_thread_count = 0;
}

/* --- workloads ------------------------------------------------------------- */

/* The first pacing grid point after now. A pacer that wakes late (CuBit's
 * millisecond timer) skips the grid points it missed rather than firing
 * them back to back. */
static double next_grid(long period_us)
{
	double k = (double)(long)((now_us() - anchor_us) / period_us) + 1;
	return anchor_us + k * period_us;
}

/* wake: the main thread waits; a waker thread paces the wakes, and waits
 * for each to be observed before pacing the next, so every wake is a
 * sample. */
static atomic_uint wake_seq, wake_ack, wake_done;
static _Atomic double wake_stamp;

static void *waker_thread(void *arg)
{
	(void)arg;
	double next = next_grid(WAKE_PERIOD_US);
	double end = next + (double)WAKE_RUN_MS * US_PER_MS;
	unsigned k = 0;
	while (next < end) {
		sleep_until(next);
		k++;
		atomic_store(&wake_stamp, now_us());
		atomic_store(&wake_seq, k);
		futex_wake(&wake_seq, 1);
		unsigned ack;
		while ((ack = atomic_load(&wake_ack)) != k)
			futex_wait(&wake_ack, ack, WAKE_WAIT_TIMEOUT_MS);
		next = next_grid(WAKE_PERIOD_US);
	}
	atomic_store(&wake_done, 1);
	atomic_store(&wake_seq, k + 1);
	futex_wake(&wake_seq, 1);
	return 0;
}

static void run_wake(struct result *r)
{
	atomic_store(&wake_seq, 0);
	atomic_store(&wake_ack, 0);
	atomic_store(&wake_done, 0);
	run_on(0, waker_thread, 0);
	unsigned last = 0;
	for (;;) {
		unsigned seq = atomic_load(&wake_seq);
		if (seq == last) {
			futex_wait(&wake_seq, last, WAKE_WAIT_TIMEOUT_MS);
			continue;
		}
		double latency = now_us() - atomic_load(&wake_stamp);
		if (atomic_load(&wake_done))
			break;
		if (latency > MISS_LATENCY_US)
			r->missed++;
		add_sample(r, latency);
		last = seq;
		atomic_store(&wake_ack, seq);
		futex_wake(&wake_ack, 1);
	}
	join_worker(0);
}

/* interactive: a paced request/response round trip to another process. */
#ifdef CUBIT
/* The ipc-test service (bench-ipc-server's echo): the manifest's fixed
 * binding slot and the server's protocol (userspace/apps/bench-ipc-*). */
#define SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY 41
#define SLOT_IPC_TEST 18
#define OP_BENCH_ECHO 0x0910
#define REPLY_OK 0xF000
#define ECHO_WORDS 1
#define XOR_MAGIC 0xC0B17000BE11ull

struct message {
	uint32_t label;
	uint8_t length, flags;
	uint16_t reserved;
	uint64_t authority;
	uint64_t words[4];
};

static unsigned long cubit_call(unsigned long n, unsigned long a, unsigned long b)
{
	unsigned long ret;
	__asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(n), "D"(a), "S"(b)
		: "rcx", "r11", "memory");
	return ret;
}

static int echo(uint64_t value)
{
	struct message m = { .label = OP_BENCH_ECHO, .length = ECHO_WORDS };
	m.words[0] = value;
	unsigned long tag = cubit_call(SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY,
		SLOT_IPC_TEST, (unsigned long)&m);
	return tag != (unsigned long)-1 && (uint32_t)tag == REPLY_OK &&
		m.words[0] == value && m.words[1] == (value ^ XOR_MAGIC);
}

static int start_peer(void)
{
	for (long waited = 0; waited < PEER_WAIT_MS; waited += PEER_RETRY_MS) {
		if (echo(waited + 1))
			return 1;
		sleep_ms(PEER_RETRY_MS);
	}
	return 0;
}

static void stop_peer(void) {}
#else
static int to_peer[2], from_peer[2];
static pid_t peer;

static int echo(uint64_t value)
{
	uint64_t back;
	if (write(to_peer[1], &value, sizeof value) != sizeof value)
		return 0;
	if (read(from_peer[0], &back, sizeof back) != sizeof back)
		return 0;
	return back == value;
}

/* A forked child echoing 8-byte requests over a pipe pair. */
static int start_peer(void)
{
	if (pipe(to_peer) || pipe(from_peer))
		return 0;
	peer = fork();
	if (peer < 0)
		return 0;
	if (peer == 0) {
		uint64_t v;
		close(to_peer[1]);
		close(from_peer[0]);
		while (read(to_peer[0], &v, sizeof v) == sizeof v)
			if (write(from_peer[1], &v, sizeof v) != sizeof v)
				break;
		_exit(0);
	}
	close(to_peer[0]);
	close(from_peer[1]);
	return echo(1);
}

static void stop_peer(void)
{
	close(to_peer[1]);
	close(from_peer[0]);
	waitpid(peer, 0, 0);
}
#endif

static int echo_failures;

static void run_interactive(struct result *r)
{
	double next = next_grid(INTERACTIVE_PERIOD_US);
	double end = next + (double)INTERACTIVE_RUN_MS * US_PER_MS;
	for (uint64_t k = 1; next < end; k++) {
		sleep_until(next);
		double t0 = now_us();
		int ok = echo(k);
		double latency = now_us() - t0;
		next = next_grid(INTERACTIVE_PERIOD_US);
		if (!ok) {
			echo_failures++;
			r->missed++;
			continue;
		}
		if (latency > MISS_LATENCY_US)
			r->missed++;
		add_sample(r, latency);
	}
}

/* frame and audio: periodic work; samples are wakeup lateness. */
static void run_periodic(struct result *r, long period_us, double work, long run_ms)
{
	long periods = run_ms * US_PER_MS / period_us;
	double first = next_grid(period_us);
	for (long k = 0; k < periods; k++) {
		double start = first + (double)k * period_us;
		sleep_until(start);
		double woke = now_us();
		add_sample(r, woke - start);
		work_us(work);
		/* Late work is not dropped: the next period then starts late
		 * and its lateness is recorded too. */
		if (now_us() > start + period_us)
			r->missed++;
	}
}

enum workload_kind { WORK_WAKE, WORK_INTERACTIVE, WORK_FRAME, WORK_AUDIO, WORK_COUNT };
static const char *const workload_names[WORK_COUNT] = { "wake", "interactive", "frame", "audio" };

static struct result result;

static void run_pair(enum workload_kind w, enum load_kind l)
{
	memset(&result, 0, sizeof result);
	set_anchor();                       /* before the load disturbs it */
	if (!start_load(l)) {
		report("sched-latency: workload=%s load=%s unavailable\n",
			workload_names[w], load_names[l]);
		return;
	}
	sleep_ms(WARMUP_MS);
	unsigned long p0 = load_progress();
	double t0 = now_us();
	switch (w) {
	case WORK_WAKE:
		run_wake(&result);
		break;
	case WORK_INTERACTIVE:
		run_interactive(&result);
		break;
	case WORK_FRAME:
		run_periodic(&result, FRAME_PERIOD_US, FRAME_WORK_US, FRAME_RUN_MS);
		break;
	case WORK_AUDIO:
		run_periodic(&result, AUDIO_PERIOD_US, AUDIO_WORK_US, AUDIO_RUN_MS);
		break;
	case WORK_COUNT:
		break;
	}
	double seconds = (now_us() - t0) / US_PER_S;
	unsigned long progress = load_progress() - p0;
	stop_load();

	double rate = progress / seconds;
	if (l == LOAD_BURN)
		rate = rate * BURN_CHUNK_UNITS / 1000.0;   /* thousands of units */
	qsort(result.samples, (size_t)result.count, sizeof result.samples[0], compare_double);
	report("sched-latency: workload=%s load=%s samples=%d p50_us=%.1f p99_us=%.1f"
		" max_us=%.1f missed=%ld bg_rate=%.0f bg_unit=%s\n",
		workload_names[w], load_names[l], result.count,
		percentile(result.samples, result.count, PERCENT_MEDIAN),
		percentile(result.samples, result.count, PERCENT_TAIL),
		result.count ? result.samples[result.count - 1] : 0.0,
		result.missed, l == LOAD_NONE ? 0.0 : rate, load_units[l]);
}

int main(int argc, char **argv)
{
	if (argc > 1)
		io_path = argv[1];
	long n = sysconf(_SC_NPROCESSORS_ONLN);
	cpus = n < 1 ? 1 : n > MAX_CPUS ? MAX_CPUS : (int)n;
	report("sched-latency: start cpus=%d\n", cpus);
	calibrate_clock();
	calibrate_work();
	if (!start_peer()) {             /* first: Linux forks it */
		report("sched-latency: FAIL interactive peer unavailable\n");
		return 1;
	}
	report("sched-latency: peer ready\n");
	if (!start_pool(cpus + 2))
		return 1;
	for (int l = 0; l < LOAD_COUNT; l++)
		for (int w = 0; w < WORK_COUNT; w++)
			run_pair((enum workload_kind)w, (enum load_kind)l);
	stop_peer();
	if (io_fd >= 0)
		close(io_fd);
	if (echo_failures)
		report("sched-latency: FAIL %d interactive echoes failed\n", echo_failures);
	report("sched-latency: done\n");
	return 0;
}
