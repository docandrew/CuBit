#!/usr/bin/env python3
"""Native live-metadata wait/reentry with mocked async storage, not GPU execution."""
from pathlib import Path
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = (root / "userspace/services/intel-gpu/main.adb").read_text()
start = source.index("   function Live_Metadata_Owner return Boolean is")
owner = source[start:source.index("   function Context_Metadata_Index", start)]
start = source.index("   procedure Advance_Live_Metadata is")
body = source[start:source.index("   procedure Advance_Directory_Metadata is", start)]
prefix = """
with Interfaces; use Interfaces;
procedure Live_Metadata_Test is
   Mode, Steps, Resumes, Failures, Replies : Natural := 0;
   Live_Metadata_Pending, In_Place_Active : Boolean := True;
   Update_Held : Boolean := False;
   Update_Pending : Unsigned_64 := 0;
   Update_Index : Natural := 1;
   Live_Metadata_Epoch : Unsigned_64 := 7;
   Owned : Boolean := True;
   function Update_Owner return Boolean is (Owned and Update_Index = 1);
   type Context is record Source : Unsigned_64 := 7; end record;
   Private_Contexts : array (1 .. 1) of Context;
   package Application_VM is
      function Revision (Object : Unsigned_64) return Unsigned_64 is (Object);
   end;
   Context_Metadata : array (1 .. 1) of Natural := [0];
   package Context_Metadata_Growth is
      type Phase is (Idle, Checking, Failed);
      procedure Step (Object : in out Natural);
      function State (Object : Natural) return Phase is
        (if Mode = 1 then Checking elsif Mode = 2 then Failed else Idle);
   end;
   use type Context_Metadata_Growth.Phase;
   package body Context_Metadata_Growth is
      procedure Step (Object : in out Natural) is
      begin
         Steps := Steps + 1;
         case Mode is
            when 4 => Owned := False;
            when 6 => Private_Contexts (1).Source := 8;
            when 8 => Update_Held := True;
            when others => null;
         end case;
      end;
   end;
   type Tag_Type is array (1 .. 4) of Unsigned_64;
   type Message is record tag, words : Tag_Type := [others => 0]; end record;
   NULL_MESSAGE : constant Message := (others => <>);
   Update_Request : Message := NULL_MESSAGE;
   subtype ProcessID is Unsigned_64;
   Update_Sender : constant Unsigned_64 := 42;
   Application_Reply_Slot : constant := 62;
   package Application_Binding is Update_Label : constant := 123; end;
   package Application_Buffers is Unavailable : constant := 3; end;
   procedure Handle_VM_Update (From : ProcessID; Msg : Message; Saved_Reply : Boolean) is
   begin
      pragma Assert (From = 42 and Saved_Reply and Msg = Update_Request);
      pragma Assert (not Live_Metadata_Pending and not In_Place_Active and not Update_Held);
      pragma Assert (Update_Index = 1 and Owned and Private_Contexts (1).Source = 7);
      Resumes := Resumes + 1;
   end;
   procedure Fail_Update is begin Failures := Failures + 1; end;
   function replyCap (Slot : Natural; Reply : Message) return Unsigned_64 is
   begin
      pragma Assert (Slot = 62 and Reply.tag = [123, 4, 0, 0] and Reply.words = [3, 1, 0, 0]);
      Replies := Replies + 1; return 1;
   end;
"""
suffix = """
begin
   for M in 0 .. 11 loop
      Mode := M; Steps := 0; Resumes := 0; Failures := 0; Replies := 0;
      Update_Index := 1; Update_Pending := (if M = 9 then 1 else 0);
      Owned := M /= 3; Update_Held := M = 7;
      Private_Contexts (1).Source := (if M = 5 then 8 else 7);
      In_Place_Active := M /= 10; Live_Metadata_Pending := M /= 11;
      Advance_Live_Metadata;
      pragma Assert (Steps = (if M in 0 .. 2 | 4 | 6 | 8 then 1 else 0));
      pragma Assert (Resumes = (if M = 0 then 1 else 0));
      pragma Assert (Failures = (if M in 0 | 1 | 11 then 0 else 1) and Replies = Failures);
      if Failures = 1 then
         pragma Assert (Update_Index = 0 and not Live_Metadata_Pending and not In_Place_Active and not Update_Held);
      end if;
   end loop;
end Live_Metadata_Test;
"""
variants = {
    "actual": body,
    "omit-post-owner": body.replace("if not Live_Metadata_Owner then null;", "if False then null;"),
    "lose-saved-reply": body.replace("Saved_Reply => True", "Saved_Reply => False"),
}
out = Path(tempfile.mkdtemp(prefix="cubit-live-metadata."))
for name, code in variants.items():
    assert name == "actual" or code != body
    case = out / name
    case.mkdir()
    (case / "live_metadata_test.adb").write_text(prefix + owner + code + suffix)
    with (case / "build.log").open("w") as log:
        subprocess.run(["gnatmake", "-q", "-gnat2022", "-gnata", "-gnato", "live_metadata_test.adb"],
                       cwd=case, stdout=log, stderr=log, check=True)
    with (case / "run.log").open("w") as log:
        run = subprocess.run([str(case / "live_metadata_test")], stdout=log, stderr=log)
    assert (run.returncode == 0) == (name == "actual"), (name, out)
print("Native live metadata wait PASS12 plus two negative controls:", out)
