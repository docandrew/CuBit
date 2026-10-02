"""Hosted IPC script around actual runtime message declarations, not authentication."""
from pathlib import Path
root = Path(__file__).resolve().parents[3]
source = (root / "userspace/runtime/gnat/cubit-messages.ads").read_text()
start = source.index("   type MessageTag is record")
end = source.index("   COMPLETION_QUEUE_SIZE", start)
out = root / "tests/aml-core/build/native-loop/abi"
out.mkdir(parents=True, exist_ok=True)
(out / "cubit.ads").write_text("package CuBit is end CuBit;\n")
for name in ("cubit-memory_grants.ads", "cubit-memory_grants.adb"):
    (out / name).write_text((root / "tests/aml-core/native-blocks" / name).read_text())
def declaration(prefix):
    begin = source.index(prefix)
    return source[begin:source.index(";", begin) + 1] + "\n"
(out / "cubit-messages.ads").write_text(
    "with Interfaces; use Interfaces;\nwith System;\npackage CuBit.Messages is\n"
    + source[start:end]
    + declaration("   subtype CapabilitySlot")
    + declaration("   SYSCALL_GETTIME ")
    + declaration("   type Activity_Result") + """
   procedure Poll_Any_Ipc (From : out ProcessID; Msg : out Message; Found : out Boolean);
   function Poll_Completion (Address : System.Address) return Unsigned_64;
   function Wait_For_Activity_Until (Deadline : Unsigned_64) return Activity_Result;
   function syscall (Call : Unsigned_64) return Unsigned_64;
   function reply (Target : ProcessID; Msg : Message) return Unsigned_64;
   type Script is array (Positive range 1 .. 8) of Message;
   Incoming, Replies : Script := [others => NULL_MESSAGE];
   Used, Next, Sent, Waits, Polls : Natural := 0;
   Now, Last_Deadline : Unsigned_64 := 0;
   Completion_Ready, Fail_Reply, Repair_On_Wait : Boolean := False;
end CuBit.Messages;
""")
