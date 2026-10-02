#!/usr/bin/env python3
"""Check optional Linux services are absent, while shared math stays native."""
from pathlib import Path
import subprocess
import sys

build = Path(sys.argv[1]).resolve()
perf = build / "src/intel/vulkan/libanv_common.a.p/anv_perf.c.o"
common = build / "src/intel/common/libintel_common.a.p/intel_common.c.o"

def symbols(path, option):
    return {line.split()[-1] for line in subprocess.check_output(
        ["nm", option, str(path)], text=True).splitlines() if line.split()}

defined = symbols(perf, "--defined-only")
undefined = symbols(perf, "--undefined-only")
assert {"anv_device_perf_init", "anv_device_perf_close", "anv_perf_write_pass_results"} <= defined
assert not any(name.startswith(("intel_perf_stream_", "intel_bind_timeline_")) for name in undefined)
assert not {"anv_AcquireProfilingLockKHR", "anv_InitializePerformanceApiINTEL",
            "anv_EnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR"} & defined
defined = symbols(common, "--defined-only")
undefined = symbols(common, "--undefined-only")
assert {"intel_compute_engine_async_threads_limit",
        "intel_compute_threads_group_dispatch_size"} <= defined
assert "intel_common_update_device_info" not in defined
assert not {"intel_engine_get_info", "intel_engines_supported_count"} & undefined
print("Native optional-service symbols PASS: shared helpers retained; Linux providers absent")
