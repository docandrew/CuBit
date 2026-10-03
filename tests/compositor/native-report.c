#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <cubit/debug.h>
int compositor_baseline_main(void);
void compositor_target_retirement_report(void)
{
    const char *msg = "COMPOSITOR-TARGETS: PASS 32 retire/reimport cycles, changed stride, preserved source, exact pixels\n";
    cubit_debug_write(msg, strlen(msg));
}
void compositor_test_mismatch(uint32_t frame, uint32_t index,
                              uint32_t actual, uint32_t expected)
{
    char text[160];
    int n=snprintf(text,sizeof text,
        "COMPOSITOR-NATIVE: frame=%u pixel=%u actual=%08x expected=%08x\n",
        frame,index,actual,expected);
    cubit_debug_write(text,(size_t)n);
}
void compositor_test_report(uint32_t result)
{
    if(result) {
        const char *msg="COMPOSITOR-NATIVE: FAIL ";
        cubit_debug_write(msg,strlen(msg));
        char n[4]={'0'+result/10,'0'+result%10,'\n',0};
        cubit_debug_write(n,3);
    } else {
        const char *msg="COMPOSITOR-NATIVE: PASS imported pixels, 192 draws, 3 contexts\n";
        cubit_debug_write(msg,strlen(msg));
        compositor_baseline_main();
    }
}

void compositor_pool_report(uint32_t pixels)
{
    char msg[180];
    int n=snprintf(msg,sizeof msg,
        "COMPOSITOR-POOL: PASS 96 frames, 3 imported targets, partial repair, held pixels stable; repair_pixels=%u full_pixels=98304\n", pixels);
    cubit_debug_write(msg,(size_t)n);
}
void compositor_affine_report(void)
{
    const char *msg="COMPOSITOR-AFFINE: PASS 128 draws, 3 scales, signed offsets, 4 rotations, source phase and padding exact, 13 rejected descriptors\n";
    cubit_debug_write(msg,strlen(msg));
}

void compositor_output_report(void)
{
    const char *msg="COMPOSITOR-OUTPUT: PASS 384 cached copy/premultiplied/straight requests, 8 damage clips, 2 output slots, exact copy/premultiplied and <=1 straight blend tolerance, target mismatch rejected without writes\n";
    cubit_debug_write(msg,strlen(msg));
}

void compositor_glyph_report(void)
{
    const char *msg="COMPOSITOR-GLYPH: PASS 12 density masks, exact existing raster equivalence, padding and short-buffer rejection\n";
    cubit_debug_write(msg,strlen(msg));
}
void compositor_mask_report(uint32_t shared)
{
    const char *msg=shared?
        "COMPOSITOR-SHARED-MASK: PASS 320 oracle draws, one context for color and masks, retained imports, target retirement and complete shutdown\n":
        "COMPOSITOR-MASK: PASS 320 tinted A8 draws, 4 scales, 4 rotations, damage, retained-source mutation, padding and format rejection; fixed arena, cache leases and final charge zero\n";
    cubit_debug_write(msg,strlen(msg));
}

void compositor_batch_report(void)
{
    const char *msg="COMPOSITOR-MASK-BATCH: PASS 132 batches, lengths0..32, high-precision blend oracle and bounded serial rounding, 4 rotations, 32 retained leases, padding/source guards and rejection before writes\n";
    cubit_debug_write(msg,strlen(msg));
}

void compositor_placement_report(uint32_t count)
{
    char text[180];
    int n=snprintf(text,sizeof text,"COMPOSITOR-GLYPH-PLACEMENT: PASS %u cases, six DPI ratios, four rotations, exact texel placement, software parity, damage, clipping and storage guards\n",count);
    cubit_debug_write(text,(size_t)n);
}

void compositor_glyph_owner_report(void)
{
    const char *msg="COMPOSITOR-GLYPH-OWNER: PASS 260 Mesa + 260 retained-software raster oracles/eviction, 32 pending leases/cancel, pinned budget/rounded-arena exhaustion and recovery, final charge zero\n";
    cubit_debug_write(msg,strlen(msg));
}

void compositor_front_report(void)
{
    const char msg[] = "COMPOSITOR-FRONT: PASS 64 simulated latches, native Mesa writes, held front/pending pixels stable, 3-target backpressure, 252 ready replacements\n";
    cubit_debug_write(msg, sizeof msg - 1);
}
