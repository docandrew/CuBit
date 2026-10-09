"""Build the declared Vulkan Desktop variant and publish its verified executable.

Called under build.lock in Nix. The source must contain the complete dispatcher
and optional-render manifest. This does not select the default or stage an image.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
from desktop_build_variant import directory
from ensure_native_mesa import ensure
from verify_desktop_vulkan_compositor import verify


def software_build(bundle, mesa_source, explicit=None):
    """Resolve the one recorded native build for this exact Mesa source."""
    if explicit is not None:
        return explicit.resolve()
    inventory = json.loads((bundle / 'inputs.json').read_text())['inputs_sha256']
    matches = []
    for name, expected in inventory.items():
        if not name.endswith('/meson-info/meson-info.json'):
            continue
        path = Path(name)
        if hashlib.sha256(path.read_bytes()).hexdigest() != expected:
            raise ValueError('Changed Mesa build metadata')
        directories = json.loads(path.read_text())['directories']
        if Path(directories['source']).resolve() == mesa_source.resolve():
            build = Path(directories['build']).resolve()
            if path.resolve() != build / 'meson-info/meson-info.json':
                raise ValueError('Mesa metadata location mismatch')
            matches.append(build)
    if len(matches) != 1:
        raise ValueError('Expected one recorded combined Mesa native build')
    return matches[0]


def main():
    root = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("bundle", type=Path, nargs="?",
                        default=Path(os.environ["CUBIT_MESA_BUNDLE"]) if os.environ.get("CUBIT_MESA_BUNDLE") else None)
    parser.add_argument("mesa_source", type=Path, nargs="?",
                        default=Path(os.environ["CUBIT_MESA_SOURCE"]) if os.environ.get("CUBIT_MESA_SOURCE") else None)
    parser.add_argument("--timing", choices=("off", "on"),
                        default=os.environ.get("CUBIT_COMPOSITOR_TIMING", "off"))
    parser.add_argument("--metrics", choices=("off", "on"), required=True)
    parser.add_argument("--input-overlay", choices=("off", "on"),
                        default=os.environ.get("CUBIT_INPUT_OVERLAY", "off"))
    parser.add_argument("--source-root", type=Path, default=root)
    parser.add_argument("--manifest-compiler", type=Path,
                        default=root / "userspace/ccl/build/manifest/ccl-manifest")
    parser.add_argument("--schema", type=Path,
                        default=root / "userspace/ccl/interfaces/executable-manifest.ccl")
    parser.add_argument("--software-mesa-build", type=Path,
                        default=Path(os.environ["CUBIT_MESA_BUILD"]) if os.environ.get("CUBIT_MESA_BUILD") else None)
    args = parser.parse_args()
    source = args.source_root.resolve()
    if not os.environ.get("IN_NIX_SHELL"):
        parser.error("Use the pinned Nix environment")
    scenario = dict(os.environ, CUBIT_COMPOSITOR="vulkan", CUBIT_INPUT_OVERLAY=args.input_overlay,
                    CUBIT_COMPOSITOR_TIMING=args.timing)
    for key, value in (("CUBIT_COMPOSITOR_STORAGE", "production"),
                       ("CUBIT_DISPLAY_TEST_MODE", "production")):
        if scenario.get(key, value) != value:
            parser.error(key + " is not supported by the verified Vulkan builder")
    if bool(args.bundle) != bool(args.mesa_source):
        parser.error("Provide both Mesa bundle and source, or neither for automatic dependency selection")
    if args.bundle is None:
        cache = Path(os.environ.get("CUBIT_MESA_CACHE") or str(root / "userspace/mesa/build/combined"))
        dependency = ensure(root, cache)
        args.bundle, args.mesa_source = dependency / "bundle", dependency / "source"
        if args.software_mesa_build is not None and args.software_mesa_build.resolve() != dependency / "native":
            parser.error("Explicit CUBIT_MESA_BUILD conflicts with automatic combined Mesa selection")
        args.software_mesa_build = dependency / "native"
    variant = directory(args.metrics, scenario)
    native = software_build(args.bundle.resolve(), args.mesa_source.resolve(), args.software_mesa_build)
    # Keep the clean build outside the input tree. Failed artifacts retain their
    # provenance; only a fully verified binary replaces the scenario output.
    work = Path(tempfile.mkdtemp(prefix="cubit-vulkan-desktop-", dir=os.environ.get("TMPDIR")))
    artifact = work / "artifact"
    subprocess.run(["python3", str(root / "tools/build_desktop_vulkan_compositor.py"),
        "--source-root", str(source), "--toolchain-root", str(root),
        "--bundle", str(args.bundle.resolve()), "--mesa-source", str(args.mesa_source.resolve()),
        "--software-mesa-build", str(native),
        "--manifest-compiler", str(args.manifest_compiler.resolve()),
        "--schema", str(args.schema.resolve()), "--catalog",
        str(root / "userspace/ccl/catalogs/native-runtime-services.ccl"),
        "--output", str(artifact), "--metrics", args.metrics, "--timing", args.timing,
        "--input-overlay", args.input_overlay], check=True)
    binary = verify(artifact)
    record = json.loads((artifact / "compositor-result.json").read_text())
    if record.get("software_renderer") != "mesa-softpipe":
        raise ValueError("Normal Desktop build must include software Mesa")
    if record.get("build_variant") != {"software_fault": "none", "input_overlay": args.input_overlay, "timing": args.timing, "metrics": args.metrics,
                                     "storage": "production"}:
        raise ValueError("Built variant does not match requested output")
    destination = source / "userspace/services/desktop" / variant
    destination.mkdir(parents=True, exist_ok=True)
    temporary = destination / "desktop.svc.pending"
    try:
        shutil.copyfile(binary, temporary)
        if hashlib.sha256(temporary.read_bytes()).hexdigest() != record["binary_sha256"]:
            raise ValueError("Copied executable changed")
        os.replace(temporary, destination / "desktop.svc")
    finally:
        temporary.unlink(missing_ok=True)
    (destination / "compositor-build.json").write_text(json.dumps(
        {"artifact": str(artifact), "binary_sha256": record["binary_sha256"],
         "build_variant": record["build_variant"]}, indent=2) + "\n")
    print(destination / "desktop.svc")


if __name__ == "__main__":
    main()
