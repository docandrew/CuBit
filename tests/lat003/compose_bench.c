/* LAT-003 hosted microbenchmark: where a CPU compositor's per-pixel time
 * goes when it writes into a scanout buffer the display driver flushes.
 *
 * A 3840x2160 (4K) 32-bit frame; each "frame" composes one damage area
 * (by default 1200x625, about 750k px, as the laptop's Metrics animation):
 * a wallpaper copy, then two alpha-blended layers (reads of the target),
 * like Desktop's CPU renderer. Destinations:
 *   warm       : the same buffer, its lines left in cache (image 25: Desktop
 *                composing into its own buffer)
 *   flushed    : clflush of the area's full rows after each frame, as
 *                intel-gpu does on Present (Submit flushes First_Row..Rows)
 *                -> the lent path today
 *   staged-nt  : compose into a private warm buffer, then stream the area to
 *                the flushed scanout with non-temporal stores (+ sfence)
 *   staged-cpy : the same with ordinary stores (memcpy-like)
 * Also: pure streaming writes (no blending) into warm vs flushed lines.
 * The host is not the N150 (bigger caches, faster DRAM): compare ratios.
 * UC/WC destinations need a kernel mapping and are not measured here.
 */
#define _GNU_SOURCE
#include <immintrin.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

enum { W = 3840, H = 2160, PITCH = W };
static int AW = 1200, AH = 625, AX = 400, AY = 300, FRAMES = 60;

static double now_ns(void) {
    struct timespec t; clock_gettime(CLOCK_MONOTONIC, &t);
    return t.tv_sec * 1e9 + t.tv_nsec;
}
static uint32_t *alloc_frame(void) {
    uint32_t *p = aligned_alloc(64, (size_t)PITCH * H * 4);
    for (size_t i = 0; i < (size_t)PITCH * H; i++) p[i] = 0xFF202020u + (uint32_t)i;
    return p;
}
static inline uint32_t blend(uint32_t s, uint32_t d) {
    uint32_t a = s >> 24, ia = 255 - a;
    uint32_t rb = (((s & 0xFF00FF) * a + (d & 0xFF00FF) * ia) >> 8) & 0xFF00FF;
    uint32_t g  = (((s & 0x00FF00) * a + (d & 0x00FF00) * ia) >> 8) & 0x00FF00;
    return 0xFF000000u | rb | g;
}
/* Wallpaper copy, then two blended layers over the area. */
static void compose(uint32_t *dst, const uint32_t *wall, uint32_t frame) {
    for (int y = AY; y < AY + AH; y++) {
        uint32_t *row = dst + (size_t)y * PITCH;
        memcpy(row + AX, wall + (size_t)y * PITCH + AX, (size_t)AW * 4);
        uint32_t l1 = 0x80000000u | (frame * 2654435761u >> 8);
        uint32_t l2 = 0x40FFFFFFu;
        for (int x = AX; x < AX + AW; x++) row[x] = blend(l1, row[x]);
        for (int x = AX + 100; x < AX + AW - 100; x++) row[x] = blend(l2, row[x]);
    }
}
static void stream_fill(uint32_t *dst, uint32_t v) {
    for (int y = AY; y < AY + AH; y++) {
        uint32_t *row = dst + (size_t)y * PITCH;
        for (int x = AX; x < AX + AW; x++) row[x] = v;
    }
}
/* intel-gpu's Present: clflush the presented rows, full pitch. */
static void flush_rows(uint32_t *dst) {
    char *p = (char *)(dst + (size_t)AY * PITCH);
    size_t bytes = (size_t)AH * PITCH * 4;
    for (size_t i = 0; i < bytes; i += 64) _mm_clflush(p + i);
    _mm_mfence();
}
static void copy_nt(uint32_t *dst, const uint32_t *src) {
    for (int y = AY; y < AY + AH; y++) {
        __m128i *d = (__m128i *)(dst + (size_t)y * PITCH + AX);
        const __m128i *s = (const __m128i *)(src + (size_t)y * PITCH + AX);
        for (int i = 0; i < AW / 4; i++) _mm_stream_si128(d + i, _mm_load_si128(s + i));
    }
    _mm_sfence();
}
static void copy_plain(uint32_t *dst, const uint32_t *src) {
    for (int y = AY; y < AY + AH; y++)
        memcpy(dst + (size_t)y * PITCH + AX, src + (size_t)y * PITCH + AX, (size_t)AW * 4);
}
static double px(void) { return (double)AW * AH; }

int main(int argc, char **argv) {
    if (argc > 2) { AW = atoi(argv[1]); AH = atoi(argv[2]); }
    if (AW % 4) AW -= AW % 4;
    uint32_t *wall = alloc_frame(), *scan[3] = {alloc_frame(), alloc_frame(), alloc_frame()};
    uint32_t *priv = alloc_frame();
    double t, best;
    printf("LAT-003 compose bench: area %dx%d (%.0fk px), %d frames, 4K frames\n", AW, AH, px() / 1000, FRAMES);
#define RUN(name, body, after) do { best = 1e30; double sum = 0; \
    for (int f = 0; f < FRAMES; f++) { uint32_t *d = scan[f % 3]; (void)d; \
        t = now_ns(); body; double e = now_ns() - t; after; sum += e; if (e < best) best = e; } \
    printf("  %-34s p50~mean %6.3f ns/px  best %6.3f ns/px  (%5.2f ms/frame)\n", name, sum / FRAMES / px(), best / px(), sum / FRAMES / 1e6); } while (0)
    RUN("compose, warm own buffer (copy path)", compose(priv, wall, f), (void)0);
    RUN("compose, lent + clflush (3 bufs)", compose(d, wall, f), flush_rows(d));
    RUN("staged: compose warm + NT stream", (compose(priv, wall, f), copy_nt(d, priv)), flush_rows(d));
    RUN("staged: compose warm + plain copy", (compose(priv, wall, f), copy_plain(d, priv)), flush_rows(d));
    RUN("stream fill, warm", stream_fill(priv, f), (void)0);
    RUN("stream fill, clflushed lines", stream_fill(d, f), flush_rows(d));
    RUN("NT copy only (flushed dest)", copy_nt(d, priv), flush_rows(d));
    RUN("intel clflush of the rows only", flush_rows(d), (void)0);
    return 0;
}
