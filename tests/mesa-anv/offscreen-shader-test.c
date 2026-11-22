/* Linux-hosted compiler oracle only: no device opened or batch submitted. */
#define GFX_VER 12
#define GFX_VERx10 120
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>

static void compiler_log(void *opaque, unsigned *id, const char *format, ...)
{
   (void)opaque;
   (void)id;
   va_list args;
   va_start(args, format);
   vfprintf(stderr, format, args);
   va_end(args);
}
#include "compiler/nir/nir_builder.h"
#include "intel/compiler/brw/brw_compiler.h"
#include "intel/compiler/brw/brw_nir.h"
#include "intel/dev/intel_debug.h"
#include "intel/common/intel_l3_config.h"
#define __gen_address_type uint64_t
#define __gen_user_data void
static uint64_t __gen_combine_address(void *data, void *location,
                                      uint64_t address, uint32_t delta)
{
   (void)data;
   (void)location;
   return address + delta;
}
#include "gen120_pack.h"
#include "intel/common/intel_genX_state_brw.h"
/* Match Mesa's release header layout, but keep this harness's checks active. */
#undef NDEBUG
#include <assert.h>

static void save_code(const char *directory, const char *stage,
                      const unsigned *code, unsigned bytes)
{
   char path[4096];
   int length = snprintf(path, sizeof(path), "%s/%s.bin", directory, stage);
   assert(length > 0 && (size_t)length < sizeof(path));
   FILE *file = fopen(path, "wb");
   assert(file);
   /* Explicit little-endian bytes, independent of the compiler host. */
   for (unsigned i = 0; i < bytes / 4; i++) {
      for (unsigned shift = 0; shift < 32; shift += 8)
         assert(fputc((code[i] >> shift) & 255, file) != EOF);
   }
   assert(fclose(file) == 0);
}

static void compile_vertex(void *ctx, struct brw_compiler *compiler,
                           const char *directory)
{
   nir_builder b = nir_builder_init_simple_shader(MESA_SHADER_VERTEX,
      &compiler->nir_options[MESA_SHADER_VERTEX], "CuBit position passthrough");
   ralloc_steal(ctx, b.shader);
   nir_variable *position = nir_variable_create(b.shader, nir_var_shader_in,
                                               glsl_vec4_type(), "position");
   position->data.location = VERT_ATTRIB_GENERIC0;
   position->data.driver_location = 0;
   nir_variable *output = nir_variable_create(b.shader, nir_var_shader_out,
                                             glsl_vec4_type(), "gl_Position");
   output->data.location = VARYING_SLOT_POS;
   nir_store_var(&b, output, nir_load_var(&b, position), 0xf);
   struct brw_nir_compiler_opts opts = {0};
   brw_preprocess_nir(compiler, b.shader, &opts);
   nir_shader_gather_info(b.shader, nir_shader_get_entrypoint(b.shader));
   struct brw_vs_prog_data data = {0};
   data.inputs_read = b.shader->info.inputs_read;
   brw_compute_vue_map(compiler->devinfo, &data.base.vue_map,
      b.shader->info.outputs_written, b.shader->info.separate_shader, 1);
   struct brw_vs_prog_key key = {0};
   struct brw_compile_vs_params params = {
      .base = {.mem_ctx = ctx, .nir = b.shader, .key = &key.base,
               .prog_data = &data.base.base},
   };
   const unsigned *code = brw_compile(compiler, &params.base);
   if (!code) {
      fprintf(stderr, "Vertex compiler failed: %s\n", params.base.error_str);
      abort();
   }
   assert(data.base.base.program_size && data.base.base.program_size % 4 == 0);
   assert(data.base.base.total_scratch == 0 && data.base.base.num_relocs == 0);
   for (unsigned i = 0; i < 4; i++)
      assert(data.base.base.push_sizes[i] == 0);
   assert(!data.base.base.has_ubo_pull);
   assert(data.inputs_read == BITFIELD64_BIT(VERT_ATTRIB_GENERIC0));
   assert(!data.uses_vertexid && !data.uses_instanceid);
   assert(data.base.dispatch_mode == INTEL_DISPATCH_MODE_SIMD8);
   assert(data.base.base.dispatch_grf_start_reg == 2 && data.base.urb_read_length == 1);
   assert(data.base.clip_distance_mask == 0 && data.base.cull_distance_mask == 0);
   printf("VS hardware thread limit=%u\n", compiler->devinfo->max_vs_threads);
   assert(data.base.vue_map.varying_to_slot[VARYING_SLOT_POS] >= 0);
   printf("VS bytes=%u scratch=%u GRF-start=%u dispatch=%u URB-read=%u entry-size=%u\n",
      data.base.base.program_size, data.base.base.total_scratch,
      data.base.base.dispatch_grf_start_reg, data.base.dispatch_mode,
      data.base.urb_read_length, data.base.urb_entry_size);
   printf("VS input-mask=%016llx VUE-slots=%u position-slot=%d\n",
      (unsigned long long)data.inputs_read, data.base.vue_map.num_slots,
      data.base.vue_map.varying_to_slot[VARYING_SLOT_POS]);
   printf("VS component-packing=%08x,%08x,%08x,%08x\n",
      data.vf_component_packing[0], data.vf_component_packing[1],
      data.vf_component_packing[2], data.vf_component_packing[3]);
   const struct intel_device_info *dev = compiler->devinfo;
   const struct intel_l3_config *l3 = intel_get_default_l3_config(dev);
   struct intel_urb_config urb = { .size = {1, 1, 1, 1} };
   urb.size[MESA_SHADER_VERTEX] = data.base.urb_entry_size;
   bool constrained = false;
   intel_get_urb_config(dev, l3, false, false, &urb, &constrained);
   printf("URB PCI-default oracle: slices=%u banks=%u compute=%u capacity-KiB=%u push-KiB=%u constrained=%u deref=%u\n",
      dev->num_slices, dev->l3_banks, dev->has_compute_engine,
      intel_get_l3_config_urb_size(dev, l3), dev->max_constant_urb_size_kb,
      constrained, urb.deref_block_size);
   for (unsigned stage = 0; stage < 4; stage++) {
      printf("URB stage=%u entries=%u size-64B=%u start-8KiB=%u min=%u max=%u\n",
         stage, urb.entries[stage], urb.size[stage], urb.start[stage],
         dev->urb.min_entries[stage], dev->urb.max_entries[stage]);
   }
   for (unsigned i = 0; i < data.base.base.program_size / 4; i++)
      printf("%08x%c", code[i], i % 4 == 3 ? '\n' : ' ');
   save_code(directory, "vertex", code, data.base.base.program_size);
}

int main(int argc, char **argv)
{
   assert(argc == 2);
   process_intel_debug_variable();
   struct intel_device_info dev;
   assert(intel_get_device_info_from_pci_id(0x46d2, &dev));
   void *ctx = ralloc_context(NULL);
   glsl_type_singleton_init_or_ref();
   struct brw_compiler *compiler = brw_compiler_create(ctx, &dev);
   assert(compiler);
   compiler->shader_debug_log = compiler_log;
   compiler->shader_perf_log = compiler_log;
   compile_vertex(ctx, compiler, argv[1]);
   nir_builder b = nir_builder_init_simple_shader(MESA_SHADER_FRAGMENT,
      &compiler->nir_options[MESA_SHADER_FRAGMENT], "CuBit fixed red fragment");
   ralloc_steal(ctx, b.shader);
   b.shader->info.fs.origin_upper_left = true;
   nir_variable *color = nir_variable_create(b.shader, nir_var_shader_out,
                                            glsl_vec4_type(), "color");
   color->data.location = FRAG_RESULT_COLOR;
   nir_store_var(&b, color, nir_imm_vec4(&b, 1, 0, 0, 1), 0xf);
   nir_shader_gather_info(b.shader, nir_shader_get_entrypoint(b.shader));
   struct brw_nir_compiler_opts opts = {0};
   brw_preprocess_nir(compiler, b.shader, &opts);
   nir_shader_gather_info(b.shader, nir_shader_get_entrypoint(b.shader));
   struct brw_fs_prog_data data = {0};
   struct brw_fs_prog_key key = {0};
   key.multisample_fbo = INTEL_NEVER;
   key.nr_color_regions = 1;
   struct brw_compile_fs_params params = {
      .base = {.mem_ctx = ctx, .nir = b.shader, .key = &key.base,
               .prog_data = &data.base},
      .max_polygons = 1,
   };
   const unsigned *code = brw_compile(compiler, &params.base);
   if (!code) {
      fprintf(stderr, "Compiler failed: %s\n", params.base.error_str);
      return 1;
   }
   fprintf(stderr, "Compiled bytes=%u SIMD=%u/%u/%u\n", data.base.program_size,
           data.dispatch_8, data.dispatch_16, data.dispatch_32);
   assert(data.base.program_size && data.base.program_size % 4 == 0);
   assert(data.dispatch_8 || data.dispatch_16 || data.dispatch_32);
   assert(data.base.total_scratch == 0 && data.base.num_relocs == 0);
   for (unsigned i = 0; i < 4; i++)
      assert(data.base.push_sizes[i] == 0);
   assert(!data.base.has_ubo_pull);
   struct GFX12_3DSTATE_PS ps = {GFX12_3DSTATE_PS_header};
   intel_set_ps_dispatch_state(&ps, &dev, &data, 1, 0);
   assert(ps._8PixelDispatchEnable && ps._16PixelDispatchEnable && !ps._32PixelDispatchEnable);
   assert(brw_wm_state_simd_width_for_ksp(ps, 0) == 8);
   assert(brw_wm_state_simd_width_for_ksp(ps, 1) == 0);
   assert(brw_wm_state_simd_width_for_ksp(ps, 2) == 16);
   ps.KernelStartPointer0 = 256 + brw_fs_prog_data_prog_offset(&data, ps, 0);
   ps.KernelStartPointer1 = 256 + brw_fs_prog_data_prog_offset(&data, ps, 1);
   ps.KernelStartPointer2 = 256 + brw_fs_prog_data_prog_offset(&data, ps, 2);
   ps.DispatchGRFStartRegisterForConstantSetupData0 = brw_fs_prog_data_dispatch_grf_start_reg(&data, ps, 0);
   ps.DispatchGRFStartRegisterForConstantSetupData1 = brw_fs_prog_data_dispatch_grf_start_reg(&data, ps, 1);
   ps.DispatchGRFStartRegisterForConstantSetupData2 = brw_fs_prog_data_dispatch_grf_start_reg(&data, ps, 2);
   ps.MaximumNumberofThreadsPerPSD = dev.max_threads_per_psd - 1;
   ps.VectorMaskEnable = data.uses_vmask;
   assert(data.num_varying_inputs == 0);
   uint32_t ps_words[12];
   GFX12_3DSTATE_PS_pack(NULL, ps_words, &ps);
   printf("PS threads-per-PSD=%u slots=8,unused,16 vector-mask=%u varying-inputs=%u\n",
          dev.max_threads_per_psd, data.uses_vmask, data.num_varying_inputs);
   for (unsigned i = 0; i < 12; i++)
      printf("PS %08x\n", ps_words[i]);
   assert(!data.persample_dispatch && !data.computed_depth_mode && !data.computed_stencil);
   assert(!data.coarse_pixel_dispatch && !data.pulls_bary && !data.has_side_effects);
   assert(data.barycentric_interp_modes == 0 && !data.early_fragment_tests);
   assert(data.flat_inputs == 0 && data.num_varying_inputs == 0);
   assert(!data.uses_kill && !data.uses_omask && !data.uses_src_depth && !data.uses_src_w);
   assert(!data.uses_depth_w_coefficients && !data.uses_pc_bary_coefficients &&
          !data.uses_npc_bary_coefficients && !data.uses_sample_offsets &&
          !data.uses_sample_mask);
   struct GFX12_3DSTATE_PS_EXTRA psx = {GFX12_3DSTATE_PS_EXTRA_header};
   psx.PixelShaderValid = true;
   psx.AttributeEnable = data.num_varying_inputs > 0;
   psx.PixelShaderIsPerSample = data.persample_dispatch;
   psx.PixelShaderComputedDepthMode = data.computed_depth_mode;
   psx.PixelShaderComputesStencil = data.computed_stencil;
   uint32_t psx_words[2];
   GFX12_3DSTATE_PS_EXTRA_pack(NULL, psx_words, &psx);
   assert(psx_words[0] == 0x784f0000 && psx_words[1] == 0x80000000);
   printf("PS_EXTRA %08x %08x\n", psx_words[0], psx_words[1]);
   printf("FS bytes=%u scratch=%u SIMD8=%u SIMD16=%u SIMD32=%u\n",
      data.base.program_size, data.base.total_scratch,
      data.dispatch_8, data.dispatch_16, data.dispatch_32);
   printf("FS GRF starts=%u,%u,%u offsets=0,%u,%u\n",
      data.base.dispatch_grf_start_reg, data.dispatch_grf_start_reg_16,
      data.dispatch_grf_start_reg_32, data.prog_offset_16, data.prog_offset_32);
   for (unsigned i = 0; i < data.base.program_size / 4; i++)
      printf("%08x%c", code[i], i % 4 == 3 ? '\n' : ' ');
   save_code(argv[1], "fragment", code, data.base.program_size);
   ralloc_free(ctx);
   glsl_type_singleton_decref();
   puts("ADL-N vertex/fragment compiler PASS; NOT executed on GPU");
   return 0;
}
