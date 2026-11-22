from pathlib import Path
import runpy
check = runpy.run_path(str(Path(__file__).with_name("check-frame-trace.py")))["check"]
a = "COMPOSITOR-FRAME: output=1 session=3 frame=2 submit_us=10 complete_us=20\n"
b = "COMPOSITOR-FRAME: output=0 session=1 frame=1 submit_us=5 complete_us=25\n"
c = "COMPOSITOR-FRAME: output=0 session=1 frame=3 submit_us=30 complete_us=40\n"
end = "COMPOSITOR-FRAME-STATS: count=3 invalid=0 dropped=0\n"
good = a + b + c + end
assert len(check(good)["records"]) == 3  # Different outputs may finish out of order.
bad = [good.replace("frame=3", "frame=1"), good.replace("submit_us=30", "submit_us=24"),
       good.replace("complete_us=40", "complete_us=29"), good.replace("count=3", "count=2"),
       good.replace("invalid=0", "invalid=1"), good.replace("dropped=0", "dropped=1"),
       good.replace("output=1", "output=2"), good.replace("session=3", "session=0"),
       good.replace("submit_us=10", "submit_us=10 submit_us=11"),
       good.replace("complete_us=20", "complete_us=18446744073709551615"), a + b + c, ""]
for text in bad:
    try:
        check(text)
    except ValueError:
        pass
    else:
        raise AssertionError("accepted invalid frame trace")
print(f"frame trace evidence: PASS out-of-order outputs and {len(bad)} negative controls")
