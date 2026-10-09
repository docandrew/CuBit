#!/usr/bin/env python3
"""Exercise the actual CPU timing accumulator; no Vulkan/hardware claim."""
from pathlib import Path
import subprocess
import tempfile

source = Path(__file__).with_name("render.h").read_text()
start = source.index("    if(!submit_start || !consumer_start || !finished")
end = source.index("    previous_finish=finished;", start)
body = source[start:end] + "    previous_finish=finished;\n"
assert "peak_frame_ns=peak_work_ns=peak_present_ns=peak_gap_ns=0" in source
prefix = """
#include <stdint.h>
#include <assert.h>
static uint64_t previous_finish=100,submit_ns,consumer_ns;
static uint64_t peak_frame_ns,peak_work_ns,peak_present_ns,peak_gap_ns;
static unsigned peak_frame;
static int timing_valid=1;
static void sample(uint64_t submit_start,uint64_t consumer_start,
                   uint64_t finished,unsigned frame) {
"""
suffix = """
}
int main(void) {
 sample(110,150,200,0);
 assert(peak_frame==1 && peak_frame_ns==100 && peak_work_ns==40 &&
        peak_present_ns==50 && peak_gap_ns==10);
 sample(210,280,285,1); /* Larger work, but not the largest whole frame. */
 assert(peak_frame==1 && peak_work_ns==40 && peak_present_ns==50);
 sample(300,310,450,2);
 assert(peak_frame==3 && peak_frame_ns==165 && peak_work_ns==10 &&
        peak_present_ns==140 && peak_gap_ns==15);
 assert(peak_frame_ns==peak_work_ns+peak_present_ns+peak_gap_ns);
 assert(submit_ns==120 && consumer_ns==195);
 sample(449,460,470,3); /* Backward clock: invalidate, never unsigned-wrap. */
 assert(!timing_valid && peak_frame==3);
 timing_valid=1; previous_finish=470; submit_ns=UINT64_MAX;
 sample(471,472,473,4);
 assert(!timing_valid); /* Accumulator overflow suppresses reporting. */
 return 0;
}
"""
with tempfile.TemporaryDirectory(prefix="cubit-gallery-timing-") as directory:
    path = Path(directory)
    for label, code in (("actual", body), ("negative", body.replace(
            "peak_work_ns=work;peak_present_ns=present;",
            "peak_work_ns=0;peak_present_ns=0;"))):
        unit = path / (label + ".c")
        executable = path / label
        unit.write_text(prefix + code + suffix)
        subprocess.run(["cc", "-std=c11", "-Wall", "-Wextra", "-Werror",
                        str(unit), "-o", str(executable)], check=True)
        result = subprocess.run([str(executable)], capture_output=True)
        if (result.returncode == 0) != (label == "actual"):
            raise SystemExit(f"{label}: unexpected exit {result.returncode}")
print("Gallery timing PASS: correlated peak components, clock/overflow rejection, negative control")
