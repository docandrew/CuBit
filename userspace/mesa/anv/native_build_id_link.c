#include "native_build_id.h"

extern const unsigned char __cubit_mesa_build_id_begin[];
extern const unsigned char __cubit_mesa_build_id_end[];
extern const unsigned char __cubit_mesa_code_begin[];
extern const unsigned char __cubit_mesa_code_end[];

const void *
cubit_mesa_build_id_for_address(const void *address)
{
   uintptr_t begin = (uintptr_t)__cubit_mesa_build_id_begin;
   uintptr_t end = (uintptr_t)__cubit_mesa_build_id_end;
   if (end < begin)
      return NULL;
   return cubit_mesa_build_id_in_image((uintptr_t)address,
      (uintptr_t)__cubit_mesa_code_begin, (uintptr_t)__cubit_mesa_code_end,
      __cubit_mesa_build_id_begin, end - begin);
}
