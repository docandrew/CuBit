"""Retain small native-test evidence; discard reproducible VM images by default."""
import atexit
import hashlib
import json
from pathlib import Path


class NativeArtifacts:
    def __init__(self, directory, keep_images=False):
        self.directory = Path(directory).resolve()
        self.keep_images = keep_images
        self.vm = None
        atexit.register(self.cleanup)

    def cleanup(self):
        # Only the process explicitly attached by this fixture may be stopped.
        if self.vm is not None and self.vm.poll() is None:
            self.vm.terminate()
            try:
                self.vm.wait(timeout=5)
            except Exception:
                self.vm.kill()
                self.vm.wait()
        if not self.directory.is_dir():
            return
        records = {}
        candidates = []
        for path in self.directory.iterdir():
            if path.is_symlink() or not path.is_file():
                continue
            numbered = any(path.name.startswith(prefix) and path.name[len(prefix):].isdigit()
                           for prefix in ("payload-", "verified-"))
            converted_screenshot = path.suffix == ".ppm" and path.with_suffix(".png").is_file()
            if path.name not in {"base.img", "desktop.img", "boot.iso", "verified-kernel"} and not numbered and not converted_screenshot:
                continue
            with path.open("rb") as stream:
                digest = hashlib.file_digest(stream, "sha256").hexdigest()
            records[path.name] = {"sha256": digest, "bytes": path.stat().st_size}
            candidates.append(path)
        if not records:
            return
        (self.directory / "artifact-manifest.json").write_text(json.dumps({
            "large_artifacts_retained": self.keep_images,
            "artifacts": records,
        }, indent=2) + "\n")
        if not self.keep_images:
            for path in candidates:
                path.unlink()
