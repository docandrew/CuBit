/* Host regression of Mesa's unchanged pure engine helpers; no GPU queries. */
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "intel/common/intel_engine.h"

int main(void)
{
   const enum intel_engine_class classes[] = {
      INTEL_ENGINE_CLASS_RENDER, INTEL_ENGINE_CLASS_COPY,
      INTEL_ENGINE_CLASS_VIDEO, INTEL_ENGINE_CLASS_VIDEO_ENHANCE,
      INTEL_ENGINE_CLASS_COMPUTE,
   };
   const char *names[] = {"render", "copy", "video", "video-enh", "compute"};
   struct intel_query_engine_info *info = calloc(1, sizeof(*info) +
      8 * sizeof(info->engines[0]));
   assert(info);
   for (unsigned c = 0; c < 5; ++c) {
      assert(strcmp(intel_engines_class_to_string(classes[c]), names[c]) == 0);
      /* Every subset of eight instances, with noncontiguous IDs/GTs. */
      for (unsigned mask = 0; mask < 256; ++mask) {
         unsigned count = 0;
         for (unsigned n = 0; n < 8; ++n) {
            info->engines[n].engine_class = (mask & (1u << n)) ?
               classes[c] : classes[(c + 1) % 5];
            info->engines[n].engine_instance = n * 7;
            info->engines[n].gt_id = n % 2;
         }
         for (unsigned length = 0; length <= 8; ++length) {
            info->num_engines = length;
            if (length && (mask & (1u << (length - 1)))) ++count;
            assert(intel_engines_count(info, classes[c]) == (int)count);
         }
      }
   }
   assert(strcmp(intel_engines_class_to_string((enum intel_engine_class)99),
                 "unknown") == 0);
   free(info);
   puts("Engine helpers: 11520 count cases PASS; no engine discovery/execution");
   return 0;
}
