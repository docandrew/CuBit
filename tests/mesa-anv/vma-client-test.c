/* Hosted common-Mesa VA policy, not GPU execution or native retirement. */
#include "anv_private.h"
#include "util/log.h"
#include <assert.h>
#include <stdio.h>
static unsigned logs;
void mesa_log(enum mesa_log_level level, const char *tag, const char *format, ...)
{ (void)level; (void)tag; (void)format; logs++; }
#include "actual-vma.inc"
int main(void)
{
   struct anv_device d={0};
   pthread_mutex_init(&d.vma_mutex,NULL);
   util_vma_heap_init(&d.vma_hi,UINT64_C(1)<<47,65536);
   util_vma_heap_init(&d.vma_dynamic_visible,0x200000,65536);
   util_vma_heap_init(&d.vma_lo,0x10000,65536);
   for(unsigned dynamic=0;dynamic<2;dynamic++) {
      enum anv_bo_alloc_flags flags=ANV_BO_ALLOC_CLIENT_VISIBLE_ADDRESS |
         (dynamic ? ANV_BO_ALLOC_DYNAMIC_VISIBLE_POOL : 0);
      struct util_vma_heap *heap=NULL;
      uint64_t raw=dynamic ? 0x202000 : (UINT64_C(1)<<47)+8192;
      uint64_t a=anv_vma_alloc(&d,4096,4096,flags,raw,&heap);
      assert(a==intel_canonical_address(raw));
      assert(heap==(dynamic ? &d.vma_dynamic_visible : &d.vma_hi));
      assert(!anv_vma_alloc(&d,4096,4096,flags,raw,&heap));
      /* Explicit reservation cannot spill into another heap. */
      assert(!anv_vma_alloc(&d,4096,4096,flags,0x10000,&heap));
      anv_vma_free(&d,heap,a,4096);
      uint64_t b=anv_vma_alloc(&d,4096,4096,flags,raw,&heap);
      assert(b==a);
      uint64_t automatic=anv_vma_alloc(&d,4096,4096,flags,0,&heap);
      assert(automatic && automatic!=b && !(automatic&4095));
      assert(heap->alloc_high);
      anv_vma_free(&d,heap,b,4096);
      anv_vma_free(&d,heap,automatic,4096);
   }
   struct util_vma_heap *heap=NULL;
   uint64_t low=anv_vma_alloc(&d,4096,4096,ANV_BO_ALLOC_32BIT_ADDRESS,0,&heap);
   assert(low && low+4096<=UINT64_C(0x100000000) && heap==&d.vma_lo);
   anv_vma_free(&d,heap,low,4096);
   assert(logs==4);
   util_vma_heap_finish(&d.vma_hi);
   util_vma_heap_finish(&d.vma_dynamic_visible);
   util_vma_heap_finish(&d.vma_lo);
   pthread_mutex_destroy(&d.vma_mutex);
   puts("Common ANV VA PASS: explicit address, collision, no spill, release/re-reserve, automatic address, canonical high VA, low heap; hosted only");
}
