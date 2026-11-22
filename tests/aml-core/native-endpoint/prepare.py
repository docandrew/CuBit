"""Copy actual runtime ABI declarations; hosted tests do not prove stamping."""
from pathlib import Path
import hashlib
import json
root = Path(__file__).resolve().parents[3]
runtime = root / "userspace/runtime/gnat"
source_path = runtime / "cubit-messages.ads"
source = source_path.read_text()
start = source.index("   type MessageTag is record")
end = source.index("   COMPLETION_QUEUE_SIZE", start)
out = root / "tests/aml-core/build/native-endpoint/abi"
out.mkdir(parents=True, exist_ok=True)
inputs = [source_path, Path(__file__).resolve()]
for name in ("cubit.ads", "cubit-process_ids.ads", "cubit-process_ids.adb"):
    path = runtime / name
    inputs.append(path)
    (out / name).write_bytes(path.read_bytes())
(out / "cubit-messages.ads").write_text(
    "with Interfaces; use Interfaces;\nwith CuBit.Process_IDs;\n"
    "package CuBit.Messages is\n" + source[start:end] + "end CuBit.Messages;\n")
(out / "abi-inputs.json").write_text(json.dumps(
    {str(path.relative_to(root)): hashlib.sha256(path.read_bytes()).hexdigest()
     for path in inputs}, indent=2) + "\n")
