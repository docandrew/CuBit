#!/usr/bin/env python3
"""Actual native pre-hold metadata continuation; modeled admission/IPC endpoints."""
from pathlib import Path
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = root / "userspace/services/intel-gpu/main.adb"
original = source.read_bytes()
text = original.decode()
start = text.index("   procedure Advance_Directory_Metadata is")
body = text[start:text.index("   end Advance_Directory_Metadata;", start) + len("   end Advance_Directory_Metadata;")]
prefix = r'''with Ada.Text_IO;
with Interfaces; use Interfaces;
procedure Resume_Test is
   subtype ProcessID is Unsigned_64;
   type Words is array (0 .. 3) of Unsigned_64;
   type Tag is record Label, Length, Flags, Reserved : Natural; end record;
   type Message is record Tag : Resume_Test.Tag; Words : Resume_Test.Words; end record;
   NULL_MESSAGE : constant Message := ((0, 0, 0, 0), [others => 0]);
   Update_Request : Message := ((1, 4, 0, 0), [1, 2, 3, 4]);
   Update_Sender : Unsigned_64 := 42;
   Directory_Metadata_Pending, In_Place_Active, Update_Held : Boolean;
   Update_Index : Natural;
   Directory_Metadata_Epoch : Unsigned_64 := 7;
   Application_Reply_Slot : constant := 62;
   package Application_Binding is Bind_Label : constant := 1; Update_Label : constant := 2; end;
   package Application_Buffers is Unavailable : constant := 3; end;
   package Application_VM is
      function Revision (Source : Unsigned_64) return Unsigned_64 is (Source);
   end;
   type Item is record Source : Unsigned_64; end record;
   Private_Contexts : array (1 .. 1) of Item := [(Source => 7)];
   Owner : Boolean;
   function Update_Owner return Boolean is (Owner and Update_Index = 1);
   Calls, Replies, Failures, Steps : Natural;
   Fault : Natural;
   package Directory_Metadata_Growth is
      type Phase is (Idle, Committing, Failed);
      type View is record State : Phase; end record;
      function Snapshot (Object : Phase) return View is ((State => Object));
      procedure Step (Object : in out Phase);
   end;
   use type Directory_Metadata_Growth.Phase;
   package body Directory_Metadata_Growth is
      procedure Step (Object : in out Phase) is
      begin Steps := Steps + 1;
         Object := (if Fault = 3 then Failed elsif Fault = 4 and Steps = 1 then Committing else Idle);
         if Fault = 7 then Owner := False; end if;
         if Fault = 8 then Private_Contexts (1).Source := 8; end if;
         if Fault = 9 then Update_Held := True; end if;
      end;
   end;
   Directory_Metadata : array (1 .. 1) of Directory_Metadata_Growth.Phase;
   Directory_Metadata_Target : constant Positive := 3;
   function Directory_Link_Capacity return Positive is (if Fault = 6 then 2 else 3);
   procedure Handle_VM_Update (From : ProcessID; Msg : Message; Saved_Reply : Boolean := False) is
   begin
      pragma Assert (From = 42 and Msg = Update_Request and Saved_Reply);
      pragma Assert (not Update_Held and not Directory_Metadata_Pending and not In_Place_Active);
      Calls := Calls + 1;
   end;
   procedure Fail_Update is begin Failures := Failures + 1; end;
   function replyCap (Slot : Natural; Msg : Message) return Unsigned_64 is
   begin
      pragma Assert (Slot = 62 and Msg.Words (0) = 3 and Failures = 1);
      Replies := Replies + 1; return 1;
   end;
'''
suffix = r'''
begin
   for F in 0 .. 9 loop
      Fault := F; Steps := 0; Calls := 0; Replies := 0; Failures := 0;
      Directory_Metadata_Pending := True; In_Place_Active := True;
      Update_Held := F = 5; Owner := F /= 1; Update_Index := 1;
      Private_Contexts (1).Source := (if F = 2 then 8 else 7);
      Directory_Metadata (1) := Directory_Metadata_Growth.Committing;
      Advance_Directory_Metadata;
      if F = 4 then
         pragma Assert (Calls = 0 and Replies = 0 and Directory_Metadata_Pending);
         Advance_Directory_Metadata;
      end if;
      if F in 0 | 4 then
         pragma Assert (Calls = 1 and Replies = 0 and Failures = 0);
      else
         pragma Assert (Calls = 0 and Replies = 1 and Failures = 1 and Update_Index = 0);
      end if;
      pragma Assert (not Directory_Metadata_Pending and not In_Place_Active);
      Advance_Directory_Metadata;
      pragma Assert (Calls + Replies = 1);
   end loop;
   Ada.Text_IO.Put_Line ("Metadata resume PASS10: saved reply, pending yield, owner/epoch/hold/failure exclusion, no replay");
end Resume_Test;
'''
out = Path(tempfile.mkdtemp(prefix="cubit-directory-resume."))
(out / "test.gpr").write_text('''project Test is
for Source_Dirs use (".");
for Object_Dir use "obj";
for Main use ("resume_test.adb");
package Compiler is
for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato");
end Compiler;
end Test;''')
variants = {"native": body, "resave-reply": body.replace("Saved_Reply => True", "Saved_Reply => False"),
    "omit-capacity": body.replace("Directory_Link_Capacity >= Directory_Metadata_Target", "True"),
    "omit-epoch": body.replace("Application_VM.Revision (Private_Contexts (Update_Index).Source) = Directory_Metadata_Epoch", "True").replace("Application_VM.Revision (Private_Contexts (Update_Index).Source) /= Directory_Metadata_Epoch", "False")}
for name, variant in variants.items():
    assert name == "native" or variant != body
    (out / "resume_test.adb").write_text(prefix + variant + suffix)
    build = subprocess.run(["gprbuild", "-f", "-p", "-P", str(out / "test.gpr")], capture_output=True, text=True)
    assert build.returncode == 0, (build.stderr, out)
    run = subprocess.run([str(out / "obj/resume_test")], capture_output=True, text=True)
    (out / f"{name}.log").write_text(run.stdout + run.stderr)
    assert (run.returncode == 0) == (name == "native"), (name, run.stdout, run.stderr, out)
    print(name, run.returncode, run.stdout.strip())
assert source.read_bytes() == original
print("Evidence:", out)
