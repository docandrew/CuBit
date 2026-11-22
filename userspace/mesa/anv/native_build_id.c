#include "native_build_id.h"
#include <string.h>

const void *
cubit_mesa_build_id_in_image(uintptr_t address,
   uintptr_t code_begin, uintptr_t code_end,
   const unsigned char *notes, size_t notes_size)
{
   if (code_begin >= code_end || address < code_begin || address >= code_end ||
       !notes)
      return NULL;

   size_t offset = 0;
   const void *found = NULL;
   while (offset < notes_size) {
      /* ELF64 uses three 32-bit words for each note header too. memcpy avoids
       * unaligned loads and effective-type violations in the hosted parser. */
      uint32_t header[3];
      if (notes_size - offset < sizeof(header))
         return NULL;
      memcpy(header, notes + offset, sizeof(header));
      size_t name_size = header[0], data_size = header[1];
      if (name_size > SIZE_MAX - 3 || data_size > SIZE_MAX - 3)
         return NULL;
      size_t name_extent = (name_size + 3) & ~(size_t)3;
      size_t data_extent = (data_size + 3) & ~(size_t)3;
      size_t payload = offset + sizeof(header);
      if (name_extent > notes_size - payload ||
          data_extent > notes_size - payload - name_extent)
         return NULL;
      if (header[2] == 3 && name_size == 4 &&
          memcmp(notes + payload, "GNU", 4) == 0) {
         /* This native link contract is --build-id=sha1. Reject duplicates,
          * malformed/other algorithms, and all-zero placeholder identities. */
         if (found || data_size != 20)
            return NULL;
         unsigned nonzero = 0;
         for (size_t i = 0; i < data_size; ++i)
            nonzero |= notes[payload + name_extent + i];
         if (!nonzero)
            return NULL;
         found = notes + offset;
      }
      offset = payload + name_extent + data_extent;
   }
   return found;
}
