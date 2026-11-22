#include "native_build_id.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>

#ifdef CUBIT_TEST_LINKED_BUILD_ID
extern const void *cubit_mesa_build_id_for_address(const void *);
#endif

static void *find(unsigned char *notes, size_t size)
{
   return (void *)cubit_mesa_build_id_in_image(0x1000, 0x1000, 0x2000,
                                             notes, size);
}

int main(void)
{
   unsigned char storage[73] = {0};
   unsigned char *note = storage + 1; /* Deliberately unaligned. */
   uint32_t header[3] = {4, 20, 3};
   memcpy(note, header, sizeof header);
   memcpy(note + 12, "GNU", 4);
   for (unsigned i = 0; i < 20; ++i) note[16 + i] = i + 1;
   assert(find(note, 36) == note);
   for (size_t size = 0; size < 36; ++size) assert(!find(note, size));
   assert(!cubit_mesa_build_id_in_image(0xfff, 0x1000, 0x2000, note, 36));
   assert(!cubit_mesa_build_id_in_image(0x2000, 0x1000, 0x2000, note, 36));
   assert(!cubit_mesa_build_id_in_image(0x1000, 0x2000, 0x1000, note, 36));
   assert(!find(NULL, 36));
   memcpy(note + 36, note, 36);
   assert(!find(note, 72)); /* Ambiguous duplicate. */
   note[12] = 'X'; assert(!find(note, 36)); note[12] = 'G';
   header[1] = UINT32_MAX; memcpy(note, header, sizeof header);
   assert(!find(note, 36));
   header[1] = 19; memcpy(note, header, sizeof header);
   assert(!find(note, 36));
   header[1] = 20; header[0] = UINT32_MAX;
   memcpy(note, header, sizeof header); assert(!find(note, 36));
   header[0] = 4; memcpy(note, header, sizeof header);
   memset(note + 16, 0, 20); assert(!find(note, 36));
#ifdef CUBIT_TEST_LINKED_BUILD_ID
   const unsigned char *linked = cubit_mesa_build_id_for_address(main);
   assert(linked);
   memcpy(header, linked, sizeof header);
   assert(header[0] == 4 && header[1] == 20 && header[2] == 3);
   assert(memcmp(linked + 12, "GNU", 4) == 0);
   assert(!cubit_mesa_build_id_for_address(storage));
   assert(!cubit_mesa_build_id_for_address(NULL));
#endif
   puts("native build-ID parser: PASS (bounds, identity, truncation, duplicates)");
}
