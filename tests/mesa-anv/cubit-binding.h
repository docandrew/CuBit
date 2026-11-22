/* CuBit platform boundary for Mesa ANV; no DRM handles or physical addresses. */
#pragma once
#include <stdbool.h>
#include <stdint.h>

struct cubit_anv_binding_range {
   uint64_t gpu_raw48;
   uint64_t bo_offset;
   uint64_t bytes;
};

/* Prepare a full-page buffer slice for the CuBit VM binding request.
 * This validates representation and bounds, NOT ownership or authority.
 * The service must independently validate the caller's BO and VM handles.
 * Output is cleared on rejection. The initial port supports 4KiB bindings;
 * sparse/NULL binds and UNBIND_ALL require separate request semantics.
 */
bool cubit_anv_prepare_binding(uint64_t canonical_gpu_address,
                              uint64_t bo_size, uint64_t bo_offset,
                              uint64_t bytes,
                              struct cubit_anv_binding_range *out);

/* ANV gem_mmap receives a page-adjusted offset but may pass a non-page-sized
 * length (anv_allocator.c). Translate it to whole grant pages without exposing
 * bytes beyond the actual BO allocation. Retain the resulting length with the
 * mapping: Vulkan's requested length is not the grant retirement length.
 * This is CPU grant geometry only, not GPU VA binding or cache synchronization.
 */
bool cubit_anv_prepare_cpu_map(uint64_t allocation_bytes, uint64_t offset,
                              uint64_t requested_bytes, uint64_t *grant_bytes);
