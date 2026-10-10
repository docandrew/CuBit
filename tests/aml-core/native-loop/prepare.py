"""Extract current runtime ABI declarations; scripts do not establish authentication."""
from pathlib import Path
import hashlib
import json

root = Path(__file__).resolve().parents[3]
runtime = root / "userspace/runtime/gnat"
source_path = runtime / "cubit-messages.ads"
source = source_path.read_text()
start = source.index("   type MessageTag is record")
end = source.index("   COMPLETION_QUEUE_SIZE", start)
out = root / "tests/aml-core/build/native-loop/abi"
out.mkdir(parents=True, exist_ok=True)
inputs = [source_path, Path(__file__).resolve()]
for directory, names in ((runtime, ("cubit.ads", "cubit-process_ids.ads", "cubit-process_ids.adb")),
                         (root / "tests/aml-core/native-blocks",
                          ("cubit-memory_grants.ads", "cubit-memory_grants.adb"))):
    for name in names:
        path = directory / name
        inputs.append(path)
        (out / name).write_bytes(path.read_bytes())

def declaration(prefix):
    """Copy a whole declaration, including semicolons inside formal parameters."""
    begin = source.index(prefix)
    depth = 0
    for index in range(begin, len(source)):
        character = source[index]
        if character == "(":
            depth += 1
        elif character == ")":
            depth -= 1
        elif character == ";" and depth == 0:
            return source[begin:index + 1] + "\n"
    raise ValueError("Unterminated runtime declaration: " + prefix)

prefixes = ("   subtype CapabilitySlot", "   SYSCALL_GETTIME ",
            "   type Activity_Result", "   procedure Poll_Any_Ipc\n",
            "   function Poll_Completion\n", "   function Wait_For_Activity_Until ",
            "   function syscall\n", "   function reply\n")
(out / "cubit-messages.ads").write_text(
    "with Interfaces; use Interfaces;\nwith System;\nwith CuBit.Process_IDs;\n"
    "package CuBit.Messages is\n" + source[start:end]
    + "".join(declaration(prefix) for prefix in prefixes) + """   type Script is array (Positive range 1 .. 8) of Message;
   Incoming, Replies : Script := [others => NULL_MESSAGE];
   -- Opaque identities sharing low bits detect truncation and generation loss.
   First_Sender : constant Process_ID := CuBit.Process_IDs.From_Word (16#1234_5678_0000_004D#);
   Second_Sender : constant Process_ID := CuBit.Process_IDs.From_Word (16#FEDC_BA98_0000_004D#);
   type Sender_Script is array (Positive range 1 .. 8) of Process_ID;
   Senders : constant Sender_Script :=
     [1 | 3 | 5 | 7 => First_Sender, others => Second_Sender];
   Used, Next, Sent, Waits, Polls : Natural := 0;
   Now, Last_Deadline : Unsigned_64 := 0;
   Completion_Ready, Fail_Reply, Repair_On_Wait : Boolean := False;
end CuBit.Messages;
""")
(out / "abi-inputs.json").write_text(json.dumps(
    {str(path.relative_to(root)): hashlib.sha256(path.read_bytes()).hexdigest()
     for path in inputs}, indent=2) + "\n")
