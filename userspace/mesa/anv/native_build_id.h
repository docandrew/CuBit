#ifndef CUBIT_MESA_NATIVE_BUILD_ID_H
#define CUBIT_MESA_NATIVE_BUILD_ID_H

#include <stddef.h>
#include <stdint.h>

/* Locate the real linker-generated GNU SHA-1 note for code in this static
 * executable. No dynamic-loader emulation and no manufactured cache identity.
 * Bounds come from the native linker; the parser is also independently tested.
 * Returned storage remains owned by the immutable executable mapping. */
const void *cubit_mesa_build_id_in_image(uintptr_t address,
   uintptr_t code_begin, uintptr_t code_end,
   const unsigned char *notes, size_t notes_size);

#endif
