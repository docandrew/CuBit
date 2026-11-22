"""Validate actual device storage with resource-factory faults, under Nix."""
from pathlib import Path
import hashlib,json,os,subprocess,tempfile
ROOT=Path(__file__).resolve().parents[2]
assert os.environ.get("IN_NIX_SHELL")
work=Path(tempfile.mkdtemp(prefix="checker-group-",dir=ROOT/"tests/compositor/build"));print(work,flush=True)
inputs={}
paths=list((ROOT/"userspace/lib/compositor").glob("*.h"))
paths += [ROOT/n for n in ("userspace/lib/compositor/vulkan_device_storage.c", "userspace/mesa/service-device.h",
                           "tests/compositor/checker_device_group_tests.c", "tests/compositor/vulkan_device_source_metadata_test.c")]
for p in paths:
    data=p.read_bytes();target=work/p.relative_to(ROOT);target.parent.mkdir(parents=True,exist_ok=True);target.write_bytes(data);inputs[str(p.relative_to(ROOT))]=hashlib.sha256(data).hexdigest()
(work/"inputs.json").write_text(json.dumps(inputs,indent=2)+"\n")
log=[]
for test,args in (("checker_device_group_tests",list(map(str,range(6)))),("vulkan_device_source_metadata_test",[None])):
    subprocess.run(["cc","-std=c11","-O2","-Wall","-Wextra","-Werror","-I",str(work/"userspace/lib/compositor"),str(work/"tests/compositor"/(test+".c")),str(work/"userspace/lib/compositor/vulkan_device_storage.c"),"-o",str(work/test)],check=True)
    for arg in args:
        run=subprocess.run([str(work/test)]+([] if arg is None else [arg]),text=True,capture_output=True)
        log.append(run.stdout+run.stderr);(work/"tests.log").write_text("".join(log));print(log[-1],end="",flush=True);run.check_returncode()
for n,h in inputs.items():assert hashlib.sha256((ROOT/n).read_bytes()).hexdigest()==h,n
(work/"result.json").write_text(json.dumps({"status":"PASS","scope":"hosted real device adapter with mocked resource factories; no GPU execution"})+"\n")
