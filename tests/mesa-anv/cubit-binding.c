#include "cubit-binding.h"

bool
cubit_anv_prepare_cpu_map(uint64_t allocation_bytes, uint64_t offset,
                          uint64_t requested_bytes, uint64_t *grant_bytes)
{
   if (!grant_bytes)
      return false;
   *grant_bytes = 0;
   if (((allocation_bytes | offset) & UINT64_C(4095)) != 0 ||
       requested_bytes == 0 || offset > allocation_bytes ||
       requested_bytes > allocation_bytes - offset ||
       requested_bytes > UINT64_MAX - UINT64_C(4095))
      return false;
   const uint64_t rounded = (requested_bytes + UINT64_C(4095)) & ~UINT64_C(4095);
   if (rounded > allocation_bytes - offset)
      return false;
   *grant_bytes = rounded;
   return true;
}

bool
cubit_anv_prepare_binding(uint64_t address, uint64_t bo_size,
                          uint64_t offset, uint64_t bytes,
                          struct cubit_anv_binding_range *out)
{
   const uint64_t limit = UINT64_C(1) << 48;
   const uint64_t raw = address & (limit - 1);
   const uint64_t upper = (raw & (UINT64_C(1) << 47)) ? UINT64_C(0xffff) : 0;
   if (!out)
      return false;
   *out = (struct cubit_anv_binding_range){0};

   /* Validate before removing sign extension. A raw high-half address is
    * not interchangeable with the canonical representation used by ANV. */
   if ((address >> 48) != upper || raw == 0 ||
       ((raw | offset | bytes) & UINT64_C(4095)) != 0 || bytes == 0 ||
       offset > bo_size || bytes > bo_size - offset || bytes > limit - raw)
      return false;

   *out = (struct cubit_anv_binding_range){raw, offset, bytes};
   return true;
}
