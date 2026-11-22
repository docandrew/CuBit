/* Linux-hosted upstream ISL oracle, NOT GPU execution or a CuBit provider. */
#include <assert.h>
#include <inttypes.h>
#include <stdio.h>
#include "intel/dev/intel_device_info.h"
#include "intel/isl/isl.h"

int main(void)
{
   struct intel_device_info info;
   assert(intel_get_device_info_from_pci_id(0x46d2, &info));
   assert(info.verx10 == 120);
   struct isl_device dev;
   isl_device_init(&dev, &info);
   struct isl_surf surf;
   bool created = isl_surf_init(&dev, &surf,
      .dim = ISL_SURF_DIM_2D, .format = ISL_FORMAT_B8G8R8A8_UNORM,
      .width = 64, .height = 64, .depth = 1, .levels = 1,
      .array_len = 1, .samples = 1, .row_pitch_B = 256,
      .usage = ISL_SURF_USAGE_RENDER_TARGET_BIT,
      .tiling_flags = ISL_TILING_LINEAR_BIT);
   assert(created);
   assert(surf.tiling == ISL_TILING_LINEAR && surf.row_pitch_B == 256);
   assert(surf.size_B <= 16384 && surf.alignment_B <= 4096);
   struct isl_view view = {
      .usage = ISL_SURF_USAGE_RENDER_TARGET_BIT,
      .format = ISL_FORMAT_B8G8R8A8_UNORM,
      .levels = 1, .array_len = 1, .swizzle = ISL_SWIZZLE_IDENTITY,
   };
   uint32_t state[16] = {0};
   /* MOCS here is an explicit encoding fixture, not a claim about a live GPU. */
   isl_surf_fill_state(&dev, state, .surf = &surf, .view = &view,
      .address = 0x202000, .mocs = 2, .aux_usage = ISL_AUX_USAGE_NONE);
   assert((state[0] >> 29) == 1); /* SURFTYPE_2D */
   assert(!(state[0] & (1u << 27))); /* TGL PRM reserved MBZ */
   assert(state[1] & (1u << 31)); /* PRM: Unorm path must remain enabled */
   assert(((state[0] >> 12) & 3) == 0); /* linear */
   assert((state[2] & 0x3fff) == 63 && ((state[2] >> 16) & 0x3fff) == 63);
   assert((state[3] & 0x3ffff) == 255);
   assert((state[6] & 7) == 0 && !(state[7] & (1u << 30)));
   assert(state[8] == 0x202000 && state[9] == 0);
   printf("ISL offscreen layout: bytes=%" PRIu64 " pitch=%u alignment=%u\n",
          surf.size_B, surf.row_pitch_B, surf.alignment_B);
   for (unsigned i = 0; i < 16; i++) printf("DW%02u=%08x\n", i, state[i]);
   /* Exercise the actual upstream no-attachment branch, not a reconstruction
    * of its individual packets. Cache policy is still only an encoding fixture.
    * Poison surrounding storage so skipped writes and overruns are visible. */
   uint32_t null_ds[64];
   for (unsigned i = 0; i < 64; i++) null_ds[i] = 0xdeadbeef;
   assert(dev.ds.size % 4 == 0 && dev.ds.size <= 62 * sizeof(uint32_t));
   isl_emit_depth_stencil_hiz(&dev, null_ds + 1,
      .depth_surf = NULL, .stencil_surf = NULL, .hiz_surf = NULL,
      .hiz_usage = ISL_AUX_USAGE_NONE, .mocs = 6);
   assert(null_ds[0] == 0xdeadbeef);
   for (unsigned i = 1 + dev.ds.size / 4; i < 64; i++)
      assert(null_ds[i] == 0xdeadbeef);
   printf("ISL null depth/stencil/HiZ bytes=%u\n", dev.ds.size);
   for (unsigned i = 0; i < dev.ds.size / 4; i++)
      printf("DS%02u=%08x\n", i, null_ds[i + 1]);
   const uint32_t null_expected[24] = {
      0x78050006, 0xe1000000, 0, 0, 0, 6, 0, 0,
      0x78060006, 0xe0000000, 0, 0, 0, 6, 0, 0,
      0x78070003, 0x0c000000, 0, 0, 0,
      0x78040001, 0, 0,
   };
   assert(dev.ds.size == sizeof(null_expected));
   for (unsigned i = 0; i < 24; i++)
      assert(null_ds[i + 1] == null_expected[i]);
   /* The driver admits an even encoded MOCS in [2,126], not an index.
    * Only policy fields may vary in this no-attachment sequence. */
   for (unsigned mocs = 2; mocs <= 126; mocs += 2) {
      for (unsigned i = 0; i < 64; i++) null_ds[i] = 0xdeadbeef;
      isl_emit_depth_stencil_hiz(&dev, null_ds + 1,
         .hiz_usage = ISL_AUX_USAGE_NONE, .mocs = mocs);
      assert(null_ds[0] == 0xdeadbeef);
      for (unsigned i = 25; i < 64; i++) assert(null_ds[i] == 0xdeadbeef);
      for (unsigned i = 0; i < 24; i++) {
         uint32_t expected = null_expected[i];
         if (i == 5 || i == 13) expected = mocs;
         if (i == 17) expected = mocs << 25;
         assert(null_ds[i + 1] == expected);
      }
   }
   puts("PASS: actual ISL null depth/stencil/HiZ/clear sequence, 63 MOCS encodings and guards");
   puts("PASS: hosted layout/encoding only; no GPU execution");
}
