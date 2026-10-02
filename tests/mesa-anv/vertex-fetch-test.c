/* TGL PRM Vol2a pp143-146, Vol2d pp1141-1147, checked against Mesa. */
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
   uint32_t words[27] = {0};
   const struct GFX12_3DSTATE_VERTEX_BUFFERS buffers = {
      GFX12_3DSTATE_VERTEX_BUFFERS_header, .DWordLength = 3,
   };
   const struct GFX12_VERTEX_BUFFER_STATE buffer = {
      .BufferPitch = 16, .AddressModifyEnable = true, .MOCS = 6,
      .L3BypassDisable = true, .BufferStartingAddress = 0x206100,
      .BufferSize = 48,
   };
   const struct GFX12_3DSTATE_VERTEX_ELEMENTS elements = {
      GFX12_3DSTATE_VERTEX_ELEMENTS_header, .DWordLength = 1,
   };
   const struct GFX12_VERTEX_ELEMENT_STATE element = {
      .Valid = true, .SourceElementFormat = 0, /* R32G32B32A32_FLOAT */
      .Component0Control = VFCOMP_STORE_SRC,
      .Component1Control = VFCOMP_STORE_SRC,
      .Component2Control = VFCOMP_STORE_SRC,
      .Component3Control = VFCOMP_STORE_SRC,
   };
   GFX12_3DSTATE_VERTEX_BUFFERS_pack(NULL, words, &buffers);
   GFX12_VERTEX_BUFFER_STATE_pack(NULL, words + 1, &buffer);
   GFX12_3DSTATE_VERTEX_ELEMENTS_pack(NULL, words + 5, &elements);
   GFX12_VERTEX_ELEMENT_STATE_pack(NULL, words + 6, &element);
   const struct GFX12_3DSTATE_VF vf = { GFX12_3DSTATE_VF_header };
   GFX12_3DSTATE_VF_pack(NULL, words + 8, &vf);
   const struct GFX12_3DSTATE_VF_INSTANCING instancing = {
      GFX12_3DSTATE_VF_INSTANCING_header,
   };
   const struct GFX12_3DSTATE_VF_SGVS sgvs = { GFX12_3DSTATE_VF_SGVS_header };
   const struct GFX12_3DSTATE_VF_SGVS_2 sgvs2 = { GFX12_3DSTATE_VF_SGVS_2_header };
   GFX12_3DSTATE_VF_INSTANCING_pack(NULL, words + 10, &instancing);
   GFX12_3DSTATE_VF_SGVS_pack(NULL, words + 13, &sgvs);
   GFX12_3DSTATE_VF_SGVS_2_pack(NULL, words + 15, &sgvs2);
   const struct GFX12_3DSTATE_VF_TOPOLOGY topology = {
      GFX12_3DSTATE_VF_TOPOLOGY_header, .PrimitiveTopologyType = _3DPRIM_TRILIST,
   };
   const struct GFX12_3DPRIMITIVE draw = {
      GFX12_3DPRIMITIVE_header, .VertexCountPerInstance = 3, .InstanceCount = 1,
   };
   GFX12_3DSTATE_VF_TOPOLOGY_pack(NULL, words + 18, &topology);
   GFX12_3DPRIMITIVE_pack(NULL, words + 20, &draw);
   const uint32_t expected[27] = {
      0x78080003, 0x02064010, 0x00206100, 0, 48,
      0x78090001, 0x02000000, 0x11110000,
      0x780c0000, 0,
      0x78490001, 0, 0, 0x784a0000, 0, 0x78560001, 0, 0,
      0x784b0000, 4, 0x7b000005, 0, 3, 0, 1, 0, 0,
   };
   for (unsigned i = 0; i < 27; i++) {
      assert(words[i] == expected[i]);
      printf("%08x\n", words[i]);
   }
   puts("Vertex fetch pack PASS; not a complete or submitted pipeline");
}
