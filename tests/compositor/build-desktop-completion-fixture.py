"""Build a private, instrumented native Desktop; never stage it.

Run in Nix under coordination/build.lock. The generated main changes only the
begin/completion observations and inserts assertions around the real output pump.
This tests Desktop control flow, not GPU fence truth or physical scanout.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import tempfile
import source_retirement_fixture
import output_retirement_fixture
import async_lease_fixture

ROOT = Path(__file__).resolve().parents[2]
DESKTOP = ROOT / "userspace/services/desktop"


def replace_once(source, old, new):
    if source.count(old) != 1:
        raise RuntimeError(f"fixture anchor must occur exactly once: {old!r}")
    return source.replace(old, new, 1)


def instrument(source, declarations, unsafe, retry=False):
    source = replace_once(source, "   procedure pumpOutput (Output : Output_Index) is",
        declarations.replace("@UNSAFE@", "True" if unsafe else "False") +
        "   procedure pumpOutput (Output : Output_Index) is")
    for poll in ("True", "False"):
        old = f"Desktop_Compositor.Complete_Output (P.Buffer, Output = 1, {poll}, Completion);"
        source = replace_once(source, old, old.replace("Desktop_Compositor.Complete_Output", "Probe_Completion"))
    source = replace_once(source,
        "Desktop_Compositor.Begin_Output (P.Buffer, P.Geometry, Output = 1, Started);",
        "Probe_Begin (P.Buffer, P.Geometry, Output = 1, Started);")
    source = replace_once(source, "when Desktop_Compositor.Deferred => return;",
        "when Desktop_Compositor.Deferred =>\n                     Probe_Deferred (Output);\n                     return;")
    source = replace_once(source,
        "               activity := Wait_For_Activity_Until (Unsigned_64'Last);",
        "               Probe_Idle;\n               activity := Wait_For_Activity_Until (Unsigned_64'Last);")
    if retry:
        source = replace_once(source,
            "            P.Buffer := P.Targets (Next_Writer.Buffer).Address;\n            return;",
            "            P.Buffer := P.Targets (Next_Writer.Buffer).Address;\n            Probe_Retry_Restored (Output);\n            return;")
    return replace_once(source, "         Compositor_Damage.Clear (P.Frame_Damage);",
        "         Compositor_Damage.Clear (P.Frame_Damage);\n         Probe_Published (Output);")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mesa", type=Path)
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--unsafe", action="store_true")
    mode.add_argument("--retry", action="store_true")
    mode.add_argument("--source-retirement", action="store_true")
    mode.add_argument("--output-retirement", choices=("scaled", "shutdown", "partial"))
    mode.add_argument("--async-lease", choices=("scaled", "shutdown", "partial"))
    args = parser.parse_args()
    mesa = args.mesa.resolve()
    out = ROOT / "tests/compositor/build"
    out.mkdir(exist_ok=True)
    work = Path(tempfile.mkdtemp(prefix="desktop-completion-", dir=out))
    print(work, flush=True)
    inputs = {}

    def remember(path):
        data = path.read_bytes()
        inputs[str(path)] = hashlib.sha256(data).hexdigest()
        return data

    # Record exact source inputs, imported library artifacts, runtime and link seeds.
    for relative in ("userspace/services/desktop", "userspace/lib/compositor",
                     "userspace/lib/display", "userspace/lib/theme", "userspace/lib/ui",
                     "userspace/ccl/src", "userspace/allocator/src", "userspace/runtime/gnat"):
        base = ROOT / relative
        for path in base.rglob("*"):
            if path.is_file() and path.suffix in (".ads", ".adb", ".gpr") and not any(
                    part.startswith("build") for part in path.relative_to(base).parts):
                remember(path)
    for path in (ROOT / "userspace/runtime/adalib").iterdir():
        if path.is_file():
            remember(path)
    seed_paths = [DESKTOP / "build" / name for name in
                  ("manifest.o", "wallpaper.o", "wallpaper_cubie.o")]
    seed_paths += [ROOT / "userspace/rust/build/font-native/libcubit_fonts.a"]
    for path in seed_paths:
        remember(path)
    declarations = remember(ROOT / "tests/compositor" / ("desktop_retry_fixture.inc" if args.retry else "desktop_completion_fixture.inc")).decode()
    remember(Path(__file__).resolve())
    original = remember(DESKTOP / "main.adb").decode()
    remember(Path(source_retirement_fixture.__file__))
    remember(Path(output_retirement_fixture.__file__))
    remember(Path(async_lease_fixture.__file__))
    generated = (async_lease_fixture.instrument(original, args.async_lease) if args.async_lease
                 else output_retirement_fixture.instrument(original, args.output_retirement) if args.output_retirement
                 else source_retirement_fixture.instrument(original) if args.source_retirement
                 else instrument(original, declarations, args.unsafe, args.retry))
    (work / "main.adb").write_text(generated)
    (work / "fixture.gpr").write_text(f'''project Fixture extends "{DESKTOP / 'desktop.gpr'}" is
   for Runtime ("Ada") use "{ROOT / 'userspace/runtime'}";
   for Source_Dirs use (".");
   for Object_Dir use "obj";
   for Exec_Dir use ".";
end Fixture;
''')
    env = {**os.environ, "CUBIT_STACK_SIZE": "16777216", "CUBIT_COMPOSITOR": "mesa",
           "CUBIT_COMPOSITOR_METRICS": "off", "CUBIT_COMPOSITOR_TIMING": "off",
           "CUBIT_COMPOSITOR_STORAGE": "production", "CUBIT_DISPLAY_TEST_MODE": "production"}

    def run(command, cwd=ROOT):
        subprocess.run(list(map(str, command)), cwd=cwd, env=env, check=True)

    result = {"scope": "instrumented native software Desktop; no GPU or physical timing claim",
              "mode": "lease-" + args.async_lease if args.async_lease else "output-" + args.output_retirement if args.output_retirement else "source-retirement" if args.source_retirement else ("retry" if args.retry else ("unsafe" if args.unsafe else "delayed")), "status": "INCOMPLETE"}
    try:
        softpipe = work / "softpipe.o"
        run(["python3", ROOT / "tests/mesa-software/compile-native-probe.py", mesa,
             ROOT / "userspace/lib/compositor/softpipe.c", softpipe])
        run(["alr", "exec", "--", "gprbuild", "-p", "-c", "-b", "-P", work / "fixture.gpr"], ROOT / "kernel")
        obj = work / "obj"
        bexch = (obj / "main.bexch").read_text()
        objects = bexch.split("[BOUND OBJECT FILES]\n")[1].split("\n[")[0].splitlines()
        libs = ["src/gallium/drivers/softpipe/libsoftpipe.a", "src/gallium/auxiliary/libgallium.a",
                "src/compiler/nir/libnir.a", "src/compiler/libcompiler.a", "src/util/libmesa_util.a",
                "src/util/blake3/libblake3.a", "src/util/libmesa_util_clflush.a",
                "src/util/libmesa_util_clflushopt.a", "src/util/libmesa_util_simd.a",
                "src/c11/impl/libmesa_util_c11.a"]
        for name in libs:
            remember(mesa / name)
        run([ROOT / "userspace/libc/cubit-c++", obj / "b__main.o", *objects, softpipe,
             *seed_paths[1:], "-Wl,--start-group", *(mesa / name for name in libs),
             ROOT / "userspace/runtime/adalib/libgnat-user.a", "-Wl,--end-group",
             "--manifest", seed_paths[0], "-o", work / "desktop.svc"])
        for path, expected in inputs.items():
            if hashlib.sha256(Path(path).read_bytes()).hexdigest() != expected:
                raise RuntimeError(f"input changed during build: {path}")
        result.update(status="BUILT", binary_sha256=hashlib.sha256((work / "desktop.svc").read_bytes()).hexdigest(),
                      generated_main_sha256=hashlib.sha256((work / "main.adb").read_bytes()).hexdigest())
        print("BUILT native completion fixture:", work, flush=True)
    finally:
        (work / "inputs.json").write_text(json.dumps(inputs, indent=2) + "\n")
        (work / "result.json").write_text(json.dumps(result, indent=2) + "\n")


if __name__ == "__main__":
    main()
