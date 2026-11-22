/* TGL PRM Vol2a1256-1265. Encoding oracle, not an executable batch. */
#include <stdint.h>
#include <stdio.h>
#include <assert.h>
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
int main(void)
{
   /* Initial RCS pipeline selection; Mesa Gen12 unknown-mode flush policy. */
   uint32_t initial[7];
   const struct GFX12_PIPE_CONTROL mode_flush = {
      GFX12_PIPE_CONTROL_header,
      .HDCPipelineFlushEnable = true,
      .RenderTargetCacheFlushEnable = true,
      .DepthCacheFlushEnable = true,
      .DepthStallEnable = true, /* Wa_1409600907 */
      .CommandStreamerStallEnable = true,
   };
   const struct GFX12_PIPELINE_SELECT select = {
      GFX12_PIPELINE_SELECT_header,
      .PipelineSelection = _3D,
      .MaskBits = 0x13,
      .MediaSamplerDOPClockGateEnable = true,
   };
   GFX12_PIPE_CONTROL_pack(NULL, initial, &mode_flush);
   GFX12_PIPELINE_SELECT_pack(NULL, initial + 6, &select);
   assert(initial[0] == 0x7a000204 && initial[1] == 0x00103001);
   assert(initial[6] == 0x69041310);
   for (unsigned i = 2; i < 6; i++)
      assert(initial[i] == 0);
   uint32_t before[6], after[6];
   const struct GFX12_PIPE_CONTROL pre = {
      GFX12_PIPE_CONTROL_header,
      .HDCPipelineFlushEnable = true,
      .RenderTargetCacheFlushEnable = true,
      .CommandStreamerStallEnable = true,
   };
   const struct GFX12_PIPE_CONTROL post = {
      GFX12_PIPE_CONTROL_header,
      .StateCacheInvalidationEnable = true,
      .ConstantCacheInvalidationEnable = true,
      .TextureCacheInvalidationEnable = true,
      .InstructionCacheInvalidateEnable = true,
      .CommandCacheInvalidateEnable = true,
   };
   GFX12_PIPE_CONTROL_pack(NULL, before, &pre);
   GFX12_PIPE_CONTROL_pack(NULL, after, &post);
   assert(before[0] == 0x7a000204 && before[1] == 0x00101000);
   assert(after[0] == 0x7a000004 && after[1] == 0x20000c0c);
   for (unsigned i = 2; i < 6; i++)
      assert(before[i] == 0 && after[i] == 0);
   uint32_t words[22] = {0};
   const struct GFX12_STATE_BASE_ADDRESS base = {
      GFX12_STATE_BASE_ADDRESS_header,
      .GeneralStateBaseAddressModifyEnable = true, .GeneralStateMOCS = 6,
      .StatelessDataPortAccessMOCS = 6,
      .SurfaceStateBaseAddressModifyEnable = true, .SurfaceStateMOCS = 6,
      .SurfaceStateBaseAddress = 0x206000,
      .DynamicStateBaseAddressModifyEnable = true, .DynamicStateMOCS = 6,
      .DynamicStateBaseAddress = 0x206000,
      .IndirectObjectBaseAddressModifyEnable = true, .IndirectObjectMOCS = 6,
      .InstructionBaseAddressModifyEnable = true, .InstructionMOCS = 6,
      .InstructionBaseAddress = 0x207000,
      .GeneralStateBufferSizeModifyEnable = true,
      .DynamicStateBufferSizeModifyEnable = true, .DynamicStateBufferSize = 1,
      .IndirectObjectBufferSizeModifyEnable = true,
      .InstructionBuffersizeModifyEnable = true, .InstructionBufferSize = 1,
      .BindlessSurfaceStateBaseAddressModifyEnable = true, .BindlessSurfaceStateMOCS = 6,
      .BindlessSamplerStateBaseAddressModifyEnable = true, .BindlessSamplerStateMOCS = 6,
   };
   GFX12_STATE_BASE_ADDRESS_pack(NULL, words, &base);
   const uint32_t expected[22] = {
      0x61010014, 0x61, 0, 0x60000, 0x206061, 0, 0x206061, 0,
      0x61, 0, 0x207061, 0, 1, 0x1001, 1, 0x1001,
      0x61, 0, 0, 0x61, 0, 0,
   };
   for (unsigned i = 0; i < 7; i++)
      printf("%08x\n", initial[i]);
   for (unsigned i = 0; i < 6; i++)
      printf("%08x\n", before[i]);
   for (unsigned i = 0; i < 22; i++) {
      assert(words[i] == expected[i]);
      printf("%08x\n", words[i]);
   }
   for (unsigned i = 0; i < 6; i++)
      printf("%08x\n", after[i]);
   puts("State setup encoding PASS; RCS-only, streamer-disabled entry and pointer reissue required");
   /* VS-only URB configuration reported by the compiler/allocator oracle. */
   uint32_t urb[8];
   const struct GFX12_3DSTATE_URB_VS vs = {
      GFX12_3DSTATE_URB_VS_header,
      .VSNumberofURBEntries = 3576, .VSURBStartingAddress = 4,
   };
   const struct GFX12_3DSTATE_URB_HS hs = {
      GFX12_3DSTATE_URB_HS_header, .HSURBStartingAddress = 4,
   };
   const struct GFX12_3DSTATE_URB_DS ds = {
      GFX12_3DSTATE_URB_DS_header, .DSURBStartingAddress = 4,
   };
   const struct GFX12_3DSTATE_URB_GS gs = {
      GFX12_3DSTATE_URB_GS_header, .GSURBStartingAddress = 4,
   };
   GFX12_3DSTATE_URB_VS_pack(NULL, urb, &vs);
   GFX12_3DSTATE_URB_HS_pack(NULL, urb + 2, &hs);
   GFX12_3DSTATE_URB_DS_pack(NULL, urb + 4, &ds);
   GFX12_3DSTATE_URB_GS_pack(NULL, urb + 6, &gs);
   const uint32_t expected_urb[] = {
      0x78300000, 0x08000df8, 0x78310000, 0x08000000,
      0x78320000, 0x08000000, 0x78330000, 0x08000000,
   };
   for (unsigned i = 0; i < 8; i++) {
      assert(urb[i] == expected_urb[i]);
      printf("URB %08x\n", urb[i]);
   }
   puts("URB encoding PASS; usable hardware capacity must be admitted separately");
   uint32_t constants[12];
   for (unsigned stage = 0; stage < 5; stage++) {
      struct GFX12_3DSTATE_PUSH_CONSTANT_ALLOC_VS alloc = {
         GFX12_3DSTATE_PUSH_CONSTANT_ALLOC_VS_header,
      };
      alloc._3DCommandSubOpcode = 0x12 + stage;
      GFX12_3DSTATE_PUSH_CONSTANT_ALLOC_VS_pack(NULL, constants + stage * 2, &alloc);
      assert(constants[stage * 2] == 0x79120000 + (stage << 16));
      assert(constants[stage * 2 + 1] == 0);
   }
   const struct GFX12_3DSTATE_CONSTANT_ALL clear = {
      GFX12_3DSTATE_CONSTANT_ALL_header,
      .ShaderUpdateEnable = 31, .MOCS = 6,
   };
   GFX12_3DSTATE_CONSTANT_ALL_pack(NULL, constants + 10, &clear);
   assert(constants[10] == 0x786d1f00 && constants[11] == 6);
   puts("Constant reset encoding PASS; no intervening commit; pointer reissue required");
   uint32_t vertex[9];
   const struct GFX12_3DSTATE_VS vs_state = {
      GFX12_3DSTATE_VS_header,
      .Enable = true, .StatisticsEnable = true, .SIMD8DispatchEnable = true,
      .MaximumNumberofThreads = 545,
      .VertexURBEntryReadLength = 1, .DispatchGRFStartRegisterForURBData = 2,
   };
   GFX12_3DSTATE_VS_pack(NULL, vertex, &vs_state);
   const uint32_t expected_vertex[9] = {
      0x78100007, 0, 0, 0, 0, 0, 0x00200800, 0x88400405, 0,
   };
   for (unsigned i = 0; i < 9; i++) {
      assert(vertex[i] == expected_vertex[i]);
      printf("VS %08x\n", vertex[i]);
   }
   puts("VS encoding PASS; fixed kernel at instruction offset zero, no varying attributes");
   const struct GFX12_SF_CLIP_VIEWPORT viewport = {
      .ViewportMatrixElementm00 = 32, .ViewportMatrixElementm11 = 32,
      .ViewportMatrixElementm22 = 1, .ViewportMatrixElementm30 = 32,
      .ViewportMatrixElementm31 = 32, .ViewportMatrixElementm32 = 0,
      .XMinClipGuardband = -1, .XMaxClipGuardband = 1,
      .YMinClipGuardband = -1, .YMaxClipGuardband = 1,
      .XMinViewPort = 0, .XMaxViewPort = 63, .YMinViewPort = 0, .YMaxViewPort = 63,
   };
   uint32_t vp[16], cc[2], pointers[4];
   const uint32_t expected_vp[16] = {
      0x42000000, 0x42000000, 0x3f800000, 0x42000000, 0x42000000, 0, 0, 0,
      0xbf800000, 0x3f800000, 0xbf800000, 0x3f800000, 0, 0x427c0000, 0, 0x427c0000,
   };
   GFX12_SF_CLIP_VIEWPORT_pack(NULL, vp, &viewport);
   for (unsigned i = 0; i < 16; i++) assert(vp[i] == expected_vp[i]);
   const struct GFX12_CC_VIEWPORT depth = {.MinimumDepth = 0, .MaximumDepth = 1};
   GFX12_CC_VIEWPORT_pack(NULL, cc, &depth);
   assert(cc[0] == 0 && cc[1] == 0x3f800000);
   const struct GFX12_3DSTATE_VIEWPORT_STATE_POINTERS_SF_CLIP sf_ptr = {
      GFX12_3DSTATE_VIEWPORT_STATE_POINTERS_SF_CLIP_header, .SFClipViewportPointer = 512,
   };
   const struct GFX12_3DSTATE_VIEWPORT_STATE_POINTERS_CC cc_ptr = {
      GFX12_3DSTATE_VIEWPORT_STATE_POINTERS_CC_header, .CCViewportPointer = 576,
   };
   GFX12_3DSTATE_VIEWPORT_STATE_POINTERS_SF_CLIP_pack(NULL, pointers, &sf_ptr);
   GFX12_3DSTATE_VIEWPORT_STATE_POINTERS_CC_pack(NULL, pointers + 2, &cc_ptr);
   assert(pointers[0] == 0x78210000 && pointers[1] == 512);
   assert(pointers[2] == 0x78230000 && pointers[3] == 576);
   puts("Viewport encoding PASS: 64x64, depth[0,1], aligned dynamic-state pointers");
   const struct GFX12_3DSTATE_CLIP clip = {
      GFX12_3DSTATE_CLIP_header,
      .StatisticsEnable = true, .ClipEnable = true,
      .ViewportXYClipTestEnable = true, .ClipMode = CLIPMODE_NORMAL,
      .ForceZeroRTAIndexEnable = true,
      .MinimumPointWidth = 0.125f, .MaximumPointWidth = 255.875f,
   };
   uint32_t clip_words[4];
   GFX12_3DSTATE_CLIP_pack(NULL, clip_words, &clip);
   assert(clip_words[0] == 0x78120002 && clip_words[1] == 0x400);
   assert(clip_words[2] == 0x90000000 && clip_words[3] == 0x3ffe0);
   puts("Clip encoding PASS: perspective divide, viewport0, layer0, normal XY clipping");
   const struct GFX12_3DSTATE_RASTER raster = {
      GFX12_3DSTATE_RASTER_header,
      .CullMode = CULLMODE_NONE, .FrontWinding = CounterClockwise,
      .ViewportZNearClipTestEnable = true, .ViewportZFarClipTestEnable = true,
   };
   uint32_t raster_words[5];
   GFX12_3DSTATE_RASTER_pack(NULL, raster_words, &raster);
   assert(raster_words[0] == 0x78500003 && raster_words[1] == 0x04210001);
   for (unsigned i = 2; i < 5; i++) assert(raster_words[i] == 0);
   puts("Raster encoding PASS: solid triangles, no culling, near/far clipping, no depth bias");
   for (unsigned small = 0; small < 2; small++) {
      const struct GFX12_3DSTATE_SF sf = {
         GFX12_3DSTATE_SF_header,
         .ViewportTransformEnable = true, .StatisticsEnable = true,
         .LineWidth = 1.0f, .PointWidth = 1.0f, .PointWidthSource = State,
         .AALineDistanceMode = AALINEDISTANCE_TRUE,
         .DerefBlockSize = small ? PerPolyDerefMode : BlockDerefSize32,
      };
      uint32_t sf_words[4];
      GFX12_3DSTATE_SF_pack(NULL, sf_words, &sf);
      assert(sf_words[0] == 0x78130002 && sf_words[1] == 0x80402);
      assert(sf_words[2] == (small ? 0x20000000 : 0) && sf_words[3] == 0x4808);
   }
   puts("SF encoding PASS: viewport transform, statistics, both VS dereference modes");
   const struct GFX12_3DSTATE_WM wm = {
      GFX12_3DSTATE_WM_header, .StatisticsEnable = true,
   };
   uint32_t wm_words[2];
   GFX12_3DSTATE_WM_pack(NULL, wm_words, &wm);
   assert(wm_words[0] == 0x78140000 && wm_words[1] == 0x80000000);
   puts("WM encoding PASS: normal dispatch/kill/depth, no interpolated shader inputs");
   struct GFX12_3DSTATE_SBE sbe = {
      GFX12_3DSTATE_SBE_header,
      .VertexURBEntryReadOffset = 1, .VertexURBEntryReadLength = 1,
      .ForceVertexURBEntryReadOffset = true, .ForceVertexURBEntryReadLength = true,
   };
   for (unsigned i = 0; i < 32; i++)
      sbe.AttributeActiveComponentFormat[i] = ACTIVE_COMPONENT_XYZW;
   uint32_t sbe_words[6];
   GFX12_3DSTATE_SBE_pack(NULL, sbe_words, &sbe);
   assert(sbe_words[0] == 0x781f0004 && sbe_words[1] == 0x30000820);
   assert(sbe_words[2] == 0 && sbe_words[3] == 0);
   assert(sbe_words[4] == 0xffffffff && sbe_words[5] == 0xffffffff);
   puts("SBE encoding PASS: zero attributes, legal nonzero URB read, swizzle off");
   const struct GFX12_3DSTATE_MULTISAMPLE multisample = {
      GFX12_3DSTATE_MULTISAMPLE_header,
      .NumberofMultisamples = 0, .PixelLocation = CENTER,
   };
   const struct GFX12_3DSTATE_SAMPLE_MASK sample_mask = {
      GFX12_3DSTATE_SAMPLE_MASK_header, .SampleMask = 1,
   };
   uint32_t sampling_words[4];
   GFX12_3DSTATE_MULTISAMPLE_pack(NULL, sampling_words, &multisample);
   GFX12_3DSTATE_SAMPLE_MASK_pack(NULL, sampling_words + 2, &sample_mask);
   assert(sampling_words[0] == 0x780d0000 && sampling_words[1] == 0);
   assert(sampling_words[2] == 0x78180000 && sampling_words[3] == 1);
   puts("sampling encoding PASS: one sample, center location, coverage enabled");
   const struct GFX12_3DSTATE_PS_BLEND ps_blend = {
      GFX12_3DSTATE_PS_BLEND_header, .HasWriteableRT = true,
   };
   const struct GFX12_3DSTATE_BLEND_STATE_POINTERS blend_pointer = {
      GFX12_3DSTATE_BLEND_STATE_POINTERS_header,
      .BlendStatePointerValid = true, .BlendStatePointer = 640,
   };
   uint32_t blend_words[4];
   GFX12_3DSTATE_PS_BLEND_pack(NULL, blend_words, &ps_blend);
   GFX12_3DSTATE_BLEND_STATE_POINTERS_pack(NULL, blend_words + 2, &blend_pointer);
   assert(blend_words[0] == 0x784d0000 && blend_words[1] == 0x40000000);
   assert(blend_words[2] == 0x78240000 && blend_words[3] == 0x281);
   const struct GFX12_BLEND_STATE blend_common = {0};
   uint32_t blend_state[17];
   GFX12_BLEND_STATE_pack(NULL, blend_state, &blend_common);
   assert(blend_state[0] == 0);
   for (unsigned rt = 0; rt < 8; rt++) {
      const struct GFX12_BLEND_STATE_ENTRY entry = {
         .WriteDisableBlue = rt != 0, .WriteDisableGreen = rt != 0,
         .WriteDisableRed = rt != 0, .WriteDisableAlpha = rt != 0,
         .PreBlendColorClampEnable = true, .PostBlendColorClampEnable = true,
         .ColorClampRange = COLORCLAMP_RTFORMAT,
      };
      GFX12_BLEND_STATE_ENTRY_pack(NULL, blend_state + 1 + rt * 2, &entry);
      assert(blend_state[1 + rt * 2] == (rt == 0 ? 0 : 15));
      assert(blend_state[2 + rt * 2] == 11);
   }
   puts("blend encoding PASS: RT0 writable, other channels disabled, blending/alpha test off");
   const struct GFX12_3DSTATE_WM_DEPTH_STENCIL depth_stencil = {
      GFX12_3DSTATE_WM_DEPTH_STENCIL_header,
   };
   const struct GFX12_3DSTATE_DEPTH_BOUNDS depth_bounds = {
      GFX12_3DSTATE_DEPTH_BOUNDS_header,
      .DepthBoundsTestMinValue = 0.0f, .DepthBoundsTestMaxValue = 1.0f,
   };
   uint32_t depth_words[8];
   GFX12_3DSTATE_WM_DEPTH_STENCIL_pack(NULL, depth_words, &depth_stencil);
   GFX12_3DSTATE_DEPTH_BOUNDS_pack(NULL, depth_words + 4, &depth_bounds);
   const uint32_t expected_depth[8] = {
      0x784e0002, 0, 0, 0, 0x78710002, 0, 0, 0x3f800000,
   };
   for (unsigned i = 0; i < 8; i++)
      assert(depth_words[i] == expected_depth[i]);
   puts("depth/stencil encoding PASS: tests/writes off, all state updates enabled");
   const struct GFX12_3DSTATE_STREAMOUT streamout = {
      GFX12_3DSTATE_STREAMOUT_header,
   };
   const struct GFX12_3DSTATE_TE tessellation = {
      GFX12_3DSTATE_TE_header,
   };
   uint32_t stream_words[5], tess_words[5];
   GFX12_3DSTATE_STREAMOUT_pack(NULL, stream_words, &streamout);
   GFX12_3DSTATE_TE_pack(NULL, tess_words, &tessellation);
   assert(stream_words[0] == 0x781e0003 && tess_words[0] == 0x781c0003);
   for (unsigned i = 1; i < 5; i++) {
      assert(stream_words[i] == 0);
      assert(tess_words[i] == 0);
   }
   puts("passthrough encoding PASS: SO off/rendering on, TE off (HS/DS must also be off)");
   const struct GFX12_3DSTATE_HS hull_shader = {
      GFX12_3DSTATE_HS_header,
   };
   uint32_t hull_words[9];
   GFX12_3DSTATE_HS_pack(NULL, hull_words, &hull_shader);
   assert(hull_words[0] == 0x781b0007);
   for (unsigned i = 1; i < 9; i++)
      assert(hull_words[i] == 0);
   puts("hull shader encoding PASS: disabled, no UAV access (TE/DS must also be off)");
   const struct GFX12_3DSTATE_DS domain_shader = {
      GFX12_3DSTATE_DS_header,
   };
   uint32_t domain_words[11];
   GFX12_3DSTATE_DS_pack(NULL, domain_words, &domain_shader);
   assert(domain_words[0] == 0x781d0009);
   for (unsigned i = 1; i < 11; i++)
      assert(domain_words[i] == 0);
   puts("domain shader encoding PASS: disabled, no UAV access; HS/TE/DS all disabled");
   const struct GFX12_3DSTATE_GS geometry_shader = {
      GFX12_3DSTATE_GS_header,
   };
   uint32_t geometry_words[10];
   GFX12_3DSTATE_GS_pack(NULL, geometry_words, &geometry_shader);
   assert(geometry_words[0] == 0x78110008);
   for (unsigned i = 1; i < 10; i++)
      assert(geometry_words[i] == 0);
   puts("geometry shader encoding PASS: disabled pass-through, no UAV access");
   struct GFX12_3DSTATE_PRIMITIVE_REPLICATION replication = {
      GFX12_3DSTATE_PRIMITIVE_REPLICATION_header,
   };
   uint32_t replication_words[6];
   GFX12_3DSTATE_PRIMITIVE_REPLICATION_pack(NULL, replication_words, &replication);
   assert(replication_words[0] == 0x786c0004);
   for (unsigned i = 1; i < 6; i++)
      assert(replication_words[i] == 0);
   /* Nonzero layout fixture only; not submitted as a rendering state. */
   replication.ReplicationCount = 15;
   replication.ReplicaMask = 0xa55a;
   for (unsigned i = 0; i < 16; i++) {
      replication.ViewportOffset[i] = i;
      replication.RTAIOffset[i] = 15 - i;
   }
   GFX12_3DSTATE_PRIMITIVE_REPLICATION_pack(NULL, replication_words, &replication);
   const uint32_t expected_replication[6] = {
      0x786c0004, 0xa55a000f, 0x76543210, 0xfedcba98, 0x89abcdef, 0x01234567,
   };
   for (unsigned i = 0; i < 6; i++)
      assert(replication_words[i] == expected_replication[i]);
   puts("replication encoding PASS: disabled plus independent offset layout fixture");
   struct GFX12_3DSTATE_DRAWING_RECTANGLE rectangle = {
      GFX12_3DSTATE_DRAWING_RECTANGLE_header,
      .ClippedDrawingRectangleXMax = 63,
      .ClippedDrawingRectangleYMax = 63,
   };
   uint32_t rectangle_words[4];
   GFX12_3DSTATE_DRAWING_RECTANGLE_pack(NULL, rectangle_words, &rectangle);
   assert(rectangle_words[0] == 0x79000002 && rectangle_words[1] == 0);
   assert(rectangle_words[2] == 0x003f003f && rectangle_words[3] == 0);
   rectangle.DrawingRectangleOriginX = -16384;
   rectangle.DrawingRectangleOriginY = 16383;
   GFX12_3DSTATE_DRAWING_RECTANGLE_pack(NULL, rectangle_words, &rectangle);
   assert(rectangle_words[3] == 0x3fffc000);
   puts("drawing rectangle PASS: inclusive 64x64 bounds and signed origin layout");
   {
      const struct GFX12_3DSTATE_BINDING_TABLE_POINTERS_VS ptr = { GFX12_3DSTATE_BINDING_TABLE_POINTERS_VS_header };
      uint32_t words[2];
      GFX12_3DSTATE_BINDING_TABLE_POINTERS_VS_pack(NULL, words, &ptr);
      assert(words[0] == 0x78260000 && words[1] == 0);
   }
   {
      const struct GFX12_3DSTATE_SAMPLER_STATE_POINTERS_VS ptr = { GFX12_3DSTATE_SAMPLER_STATE_POINTERS_VS_header };
      uint32_t words[2];
      GFX12_3DSTATE_SAMPLER_STATE_POINTERS_VS_pack(NULL, words, &ptr);
      assert(words[0] == 0x782b0000 && words[1] == 0);
   }
   {
      const struct GFX12_3DSTATE_BINDING_TABLE_POINTERS_HS ptr = { GFX12_3DSTATE_BINDING_TABLE_POINTERS_HS_header };
      uint32_t words[2];
      GFX12_3DSTATE_BINDING_TABLE_POINTERS_HS_pack(NULL, words, &ptr);
      assert(words[0] == 0x78270000 && words[1] == 0);
   }
   {
      const struct GFX12_3DSTATE_SAMPLER_STATE_POINTERS_HS ptr = { GFX12_3DSTATE_SAMPLER_STATE_POINTERS_HS_header };
      uint32_t words[2];
      GFX12_3DSTATE_SAMPLER_STATE_POINTERS_HS_pack(NULL, words, &ptr);
      assert(words[0] == 0x782c0000 && words[1] == 0);
   }
   {
      const struct GFX12_3DSTATE_BINDING_TABLE_POINTERS_DS ptr = { GFX12_3DSTATE_BINDING_TABLE_POINTERS_DS_header };
      uint32_t words[2];
      GFX12_3DSTATE_BINDING_TABLE_POINTERS_DS_pack(NULL, words, &ptr);
      assert(words[0] == 0x78280000 && words[1] == 0);
   }
   {
      const struct GFX12_3DSTATE_SAMPLER_STATE_POINTERS_DS ptr = { GFX12_3DSTATE_SAMPLER_STATE_POINTERS_DS_header };
      uint32_t words[2];
      GFX12_3DSTATE_SAMPLER_STATE_POINTERS_DS_pack(NULL, words, &ptr);
      assert(words[0] == 0x782d0000 && words[1] == 0);
   }
   {
      const struct GFX12_3DSTATE_BINDING_TABLE_POINTERS_GS ptr = { GFX12_3DSTATE_BINDING_TABLE_POINTERS_GS_header };
      uint32_t words[2];
      GFX12_3DSTATE_BINDING_TABLE_POINTERS_GS_pack(NULL, words, &ptr);
      assert(words[0] == 0x78290000 && words[1] == 0);
   }
   {
      const struct GFX12_3DSTATE_SAMPLER_STATE_POINTERS_GS ptr = { GFX12_3DSTATE_SAMPLER_STATE_POINTERS_GS_header };
      uint32_t words[2];
      GFX12_3DSTATE_SAMPLER_STATE_POINTERS_GS_pack(NULL, words, &ptr);
      assert(words[0] == 0x782e0000 && words[1] == 0);
   }
   {
      const struct GFX12_3DSTATE_BINDING_TABLE_POINTERS_PS ptr = { GFX12_3DSTATE_BINDING_TABLE_POINTERS_PS_header };
      uint32_t words[2];
      GFX12_3DSTATE_BINDING_TABLE_POINTERS_PS_pack(NULL, words, &ptr);
      assert(words[0] == 0x782a0000 && words[1] == 0);
   }
   {
      const struct GFX12_3DSTATE_SAMPLER_STATE_POINTERS_PS ptr = { GFX12_3DSTATE_SAMPLER_STATE_POINTERS_PS_header };
      uint32_t words[2];
      GFX12_3DSTATE_SAMPLER_STATE_POINTERS_PS_pack(NULL, words, &ptr);
      assert(words[0] == 0x782f0000 && words[1] == 0);
   }
   puts("state pointers PASS: all five binding and sampler offsets reissued as zero");
   for (unsigned mocs = 2; mocs <= 126; mocs += 2) {
      const struct GFX12_3DSTATE_BINDING_TABLE_POOL_ALLOC pool = {
         GFX12_3DSTATE_BINDING_TABLE_POOL_ALLOC_header,
         .MOCS = mocs,
      };
      uint32_t words[4];
      GFX12_3DSTATE_BINDING_TABLE_POOL_ALLOC_pack(NULL, words, &pool);
      assert(words[0] == 0x79190002 && words[1] == mocs);
      assert(words[2] == 0 && words[3] == 0);
   }
   puts("binding pool disable PASS: all encoded even MOCS policies, no address or pages");
   {
      /* TGL Vol2c1265-1266: bit9 reserved. Do not select the generic
       * gfx12 full-way mode used by other devices. tgl_l3_configs[0,1]. */
      const struct GFX12_L3ALLOC render = {
         .URBAllocation = 32, .AllAllocation = 88,
      };
      const struct GFX12_L3ALLOC defaults = {
         .URBAllocation = 16, .AllAllocation = 104,
      };
      uint32_t word;
      GFX12_L3ALLOC_pack(NULL, &word, &render);
      assert(word == 0xb0000040);
      GFX12_L3ALLOC_pack(NULL, &word, &defaults);
      assert(word == 0xd0000020);
   }
   puts("TGL L3 allocations PASS: URB32/ALL88 and URB16/ALL104, bit9 clear");
   {
      const struct GFX12_CPS_STATE disabled = { .CoarsePixelShadingMode = CPS_MODE_NONE };
      uint32_t words[8];
      for (unsigned i = 0; i < 8; i++) words[i] = 0xdeadbeef;
      GFX12_CPS_STATE_pack(NULL, words, &disabled);
      for (unsigned i = 0; i < 8; i++) assert(words[i] == 0);
      const struct GFX12_3DSTATE_CPS_POINTERS ptr = {
         GFX12_3DSTATE_CPS_POINTERS_header,
         .CoarsePixelShadingStateArrayPointer = 736,
      };
      GFX12_3DSTATE_CPS_POINTERS_pack(NULL, words, &ptr);
      assert(words[0] == 0x78220000 && words[1] == 736);
   }
   puts("CPS disabled PASS: eight zero state words and aligned dynamic pointer");
   {
      const struct GFX12_COLOR_CALC_STATE cc = { 0 };
      uint32_t words[6];
      for (unsigned i = 0; i < 6; i++) words[i] = 0xdeadbeef;
      GFX12_COLOR_CALC_STATE_pack(NULL, words, &cc);
      for (unsigned i = 0; i < 6; i++) assert(words[i] == 0);
      const struct GFX12_3DSTATE_CC_STATE_POINTERS ptr = {
         GFX12_3DSTATE_CC_STATE_POINTERS_header,
         .ColorCalcStatePointer = 1280,
         .ColorCalcStatePointerValid = true,
      };
      GFX12_3DSTATE_CC_STATE_POINTERS_pack(NULL, words, &ptr);
      assert(words[0] == 0x780e0000 && words[1] == 0x501);
   }
   puts("color calc PASS: zero alpha/blend constants and valid aligned pointer");
   {
      const struct GFX12_3DSTATE_SAMPLE_PATTERN sp = {
         GFX12_3DSTATE_SAMPLE_PATTERN_header,
         ._1xSample0XOffset = 8.0f / 16.0f, ._1xSample0YOffset = 8.0f / 16.0f,
         ._2xSample0XOffset = 12.0f / 16.0f, ._2xSample0YOffset = 12.0f / 16.0f,
         ._2xSample1XOffset = 4.0f / 16.0f, ._2xSample1YOffset = 4.0f / 16.0f,
         ._4xSample0XOffset = 6.0f / 16.0f, ._4xSample0YOffset = 2.0f / 16.0f,
         ._4xSample1XOffset = 14.0f / 16.0f, ._4xSample1YOffset = 6.0f / 16.0f,
         ._4xSample2XOffset = 2.0f / 16.0f, ._4xSample2YOffset = 10.0f / 16.0f,
         ._4xSample3XOffset = 10.0f / 16.0f, ._4xSample3YOffset = 14.0f / 16.0f,
         ._8xSample0XOffset = 9.0f / 16.0f, ._8xSample0YOffset = 5.0f / 16.0f,
         ._8xSample1XOffset = 7.0f / 16.0f, ._8xSample1YOffset = 11.0f / 16.0f,
         ._8xSample2XOffset = 13.0f / 16.0f, ._8xSample2YOffset = 9.0f / 16.0f,
         ._8xSample3XOffset = 5.0f / 16.0f, ._8xSample3YOffset = 3.0f / 16.0f,
         ._8xSample4XOffset = 3.0f / 16.0f, ._8xSample4YOffset = 13.0f / 16.0f,
         ._8xSample5XOffset = 1.0f / 16.0f, ._8xSample5YOffset = 7.0f / 16.0f,
         ._8xSample6XOffset = 11.0f / 16.0f, ._8xSample6YOffset = 15.0f / 16.0f,
         ._8xSample7XOffset = 15.0f / 16.0f, ._8xSample7YOffset = 1.0f / 16.0f,
         ._16xSample0XOffset = 9.0f / 16.0f, ._16xSample0YOffset = 9.0f / 16.0f,
         ._16xSample1XOffset = 7.0f / 16.0f, ._16xSample1YOffset = 5.0f / 16.0f,
         ._16xSample2XOffset = 5.0f / 16.0f, ._16xSample2YOffset = 10.0f / 16.0f,
         ._16xSample3XOffset = 12.0f / 16.0f, ._16xSample3YOffset = 7.0f / 16.0f,
         ._16xSample4XOffset = 3.0f / 16.0f, ._16xSample4YOffset = 6.0f / 16.0f,
         ._16xSample5XOffset = 10.0f / 16.0f, ._16xSample5YOffset = 13.0f / 16.0f,
         ._16xSample6XOffset = 13.0f / 16.0f, ._16xSample6YOffset = 11.0f / 16.0f,
         ._16xSample7XOffset = 11.0f / 16.0f, ._16xSample7YOffset = 3.0f / 16.0f,
         ._16xSample8XOffset = 6.0f / 16.0f, ._16xSample8YOffset = 14.0f / 16.0f,
         ._16xSample9XOffset = 8.0f / 16.0f, ._16xSample9YOffset = 1.0f / 16.0f,
         ._16xSample10XOffset = 4.0f / 16.0f, ._16xSample10YOffset = 2.0f / 16.0f,
         ._16xSample11XOffset = 2.0f / 16.0f, ._16xSample11YOffset = 12.0f / 16.0f,
         ._16xSample12XOffset = 0.0f / 16.0f, ._16xSample12YOffset = 8.0f / 16.0f,
         ._16xSample13XOffset = 15.0f / 16.0f, ._16xSample13YOffset = 4.0f / 16.0f,
         ._16xSample14XOffset = 14.0f / 16.0f, ._16xSample14YOffset = 15.0f / 16.0f,
         ._16xSample15XOffset = 1.0f / 16.0f, ._16xSample15YOffset = 0.0f / 16.0f,
      };
      const uint32_t expected[9] = { 0x791c0007, 0xc75a7599, 0xb3dbad36, 0x2c42816e, 0x10eff408, 0xf1bf173d, 0x53d97b95, 0xae2ae662, 0x8844cc };
      uint32_t words[9];
      GFX12_3DSTATE_SAMPLE_PATTERN_pack(NULL, words, &sp);
      for (unsigned i = 0; i < 9; i++) assert(words[i] == expected[i]);
   }
   puts("sample pattern PASS: standard 1/2/4/8/16x positions");
   {
      const struct GFX12_PIPE_CONTROL pc = {
         GFX12_PIPE_CONTROL_header,
         .CommandStreamerStallEnable = true,
         .RenderTargetCacheFlushEnable = true,
         .StallAtPixelScoreboard = true,
         .PostSyncOperation = 1,
         .Address = 0x201008,
      };
      const uint32_t expected[6] = { 0x7a000004, 0x00105002, 0x201008, 0, 0, 0 };
      uint32_t words[6];
      GFX12_PIPE_CONTROL_pack(NULL, words, &pc);
      for (unsigned i = 0; i < 6; i++) assert(words[i] == expected[i]);
   }
   puts("stencil post-sync PASS: private PPGTT QWORD scratch, not completion marker");
   {
      const struct GFX12_MI_LOAD_REGISTER_IMM lri = {
         GFX12_MI_LOAD_REGISTER_IMM_header,
         .RegisterOffset = 0xb134, .DataDWord = 0xb0000040,
      };
      const struct GFX12_MI_STORE_REGISTER_MEM srm = {
         GFX12_MI_STORE_REGISTER_MEM_header,
         .RegisterAddress = 0xb134, .MemoryAddress = 0x201010,
      };
      const struct GFX12_MI_STORE_REGISTER_MEM parameters = {
         GFX12_MI_STORE_REGISTER_MEM_header,
         .RegisterAddress = 0xb164, .MemoryAddress = 0x201014,
      };
      uint32_t words[11];
      const uint32_t expected[11] = {
         0x11000001, 0xb134, 0xb0000040, 0x12000002, 0xb134, 0x201010, 0,
         0x12000002, 0xb164, 0x201014, 0,
      };
      GFX12_MI_LOAD_REGISTER_IMM_pack(NULL, words, &lri);
      GFX12_MI_STORE_REGISTER_MEM_pack(NULL, words + 3, &srm);
      GFX12_MI_STORE_REGISTER_MEM_pack(NULL, words + 7, &parameters);
      for (unsigned i = 0; i < 11; i++) assert(words[i] == expected[i]);
   }
   puts("L3 command encoding PASS: allocation write and private PPGTT readback");
}
