"""Extract actual runtime message declarations; never emulate kernel stamping."""
from pathlib import Path
root = Path(__file__).resolve().parents[3]
source = (root / "userspace/runtime/gnat/cubit-messages.ads").read_text()
start = source.index("   type MessageTag is record")
end = source.index("   subtype ProcessID", start)
out = root / "tests/aml-core/build/native-endpoint/abi"
out.mkdir(parents=True, exist_ok=True)
(out / "cubit.ads").write_text("package CuBit is end CuBit;\n")
(out / "cubit-messages.ads").write_text(
    "with Interfaces; use Interfaces;\npackage CuBit.Messages is\n"
    + source[start:end] + "end CuBit.Messages;\n")
