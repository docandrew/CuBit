#include <assert.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include "intel/perf/intel_perf.h"
#include "intel/perf/intel_perf_regs.h"

int main(void)
{
   struct intel_perf_query_result result;
   memset(&result, 0xa5, sizeof(result));
   intel_perf_query_result_clear(&result);
   assert(result.hw_id == INTEL_PERF_INVALID_CTX_ID);
   assert(result.reports_accumulated == 0);
   assert(result.begin_timestamp == 0 && result.end_timestamp == 0);
   struct intel_perf_query_info query = {.perfcnt_offset = 4};
   for (uint64_t n = 0; n < 4096; ++n) {
      uint64_t start[2] = {n, PERF_CNT_VALUE_MASK - n};
      uint64_t end[2] = {n + 1, n};
      intel_perf_query_result_read_perfcnts(&result, &query, start, end);
      assert(result.accumulator[4] == 1);
      assert(result.accumulator[5] == 2 * n + 1);
   }
   struct intel_device_info info = {.ver = 12, .verx10 = 120};
   struct intel_perf_config perf = {.devinfo = &info, .oa_timestamp_shift = 1};
   query.perf = &perf;
   uint32_t report[64] = {0};
   for (uint32_t n = 0; n < 65536; ++n) {
      report[1] = n;
      assert(intel_perf_report_timestamp(&query, &info, report) == n / 2);
   }
   puts("Mesa counter results PASS: clear, 4096 wrap cases, 65536 timestamps (host only)");
}
