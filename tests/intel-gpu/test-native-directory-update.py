#!/usr/bin/env python3
"""Exercise the actual native directory-update dispatcher; hardware is modeled."""
from pathlib import Path
import subprocess
import tempfile
import hashlib
import json

root = Path(__file__).resolve().parents[2]
source = root / "userspace/services/intel-gpu/main.adb"
original = source.read_bytes()
text = original.decode()
start = text.index("   procedure Finish_Directory_Update (")
end = text.index("   end Finish_Directory_Update;", start) + len("   end Finish_Directory_Update;")
body = text[start:end]
prefix = r'''with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Table_Provenance;
with Intel_GPU_Table_References;
procedure Native_Directory_Test is
   Fault, Publications, Invalidations, Finishes, Leaves, Replies, Failures : Natural := 0;
   Owner : Boolean := True;
   package Application_VM is
      type Image is record Used : Natural := 4; Revision : Natural := 1; end record;
      type Data_Pages is array (Positive range <>) of Unsigned_64;
      function Root_DMA (Source : Image) return Unsigned_64 is (4096);
   end Application_VM;
   package Directory_Writer is
      type State is record Tried, Written, Adopted : Boolean := False; end record;
      function Attempted (Receipt : State) return Boolean is (Receipt.Tried);
      procedure Rearm (Receipt : in out State; Source : Application_VM.Image;
                      Root : Unsigned_64; OK : out Boolean);
      generic
         with function Read_Page (Ordinal : Positive) return Unsigned_64;
      procedure Start_From_Pages (Receipt : in out State; Source : Application_VM.Image;
                       GPU, Bytes, Root : Unsigned_64; Page_Count : Natural;
                       OK : out Boolean);
      procedure Step (Receipt : in out State; Source : Application_VM.Image);
      function Pending (Receipt : State) return Boolean is (False);
      function Published (Receipt : State) return Boolean is (Receipt.Written);
      procedure Commit (Receipt : in out State; Source : in out Application_VM.Image;
                        OK : out Boolean);
   end Directory_Writer;
   package Intel_GPU_Buffer_Reply is
      type Backing is record Ready : Boolean := True; Bytes : Unsigned_64 := 12288; end record;
      function Valid (Item : Backing) return Boolean is (Item.Ready);
   end Intel_GPU_Buffer_Reply;
   package Application_Buffers is
      type Words is array (Natural range 0 .. 3) of Unsigned_64;
      Unavailable : constant Unsigned_64 := 3;
      function Ticket_Slot (Ticket : Unsigned_64) return Positive is (Positive (Ticket));
      procedure Finish_Private (Object : Natural; Ticket : Unsigned_64; Consumed : out Boolean);
   end Application_Buffers;
   package body Application_Buffers is
      procedure Finish_Private (Object : Natural; Ticket : Unsigned_64; Consumed : out Boolean) is
      begin
         pragma Assert (Ticket = 56); Finishes := Finishes + 1; Consumed := Fault /= 8;
      end;
   end Application_Buffers;
   Application_Buffer_State, Table_Backing_Registry : Natural := 0;
   package Table_Allocations is
      type Allocation_Role is (Incremental_Tables);
      procedure Install (Registry : Natural; Slot : Positive; Session, Ticket : Unsigned_64;
                         Role : Allocation_Role; Backing : Intel_GPU_Buffer_Reply.Backing;
                         OK : out Boolean);
   end Table_Allocations;
   package body Table_Allocations is
      procedure Install (Registry : Natural; Slot : Positive; Session, Ticket : Unsigned_64;
                         Role : Allocation_Role; Backing : Intel_GPU_Buffer_Reply.Backing;
                         OK : out Boolean) is
      begin
         pragma Assert (Slot = 56 and Session = 7 and Ticket = 56 and Backing.Bytes = 12288);
         OK := Fault /= 2;
      end;
   end Table_Allocations;
   package Application_State is
      package Table_References is new Intel_GPU_Table_References (8);
   end;
   package R renames Application_State.Table_References;
   type Item is limited record
      Source : Application_VM.Image;
      Growth : Directory_Writer.State;
      Table_Owners : Intel_GPU_Table_Provenance.Ledger;
      Table_Generation : Unsigned_64 := 1;
      Table_IDs : R.Map;
   end record;
   type Item_Access is access Item;
   Private_Contexts : array (1 .. 1) of Item_Access;
   procedure Resolve (Session, Ticket, Offset : Unsigned_64;
                      CPU, DMA : out Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := Session = 7 and Ticket in 55 .. 56 and not (Fault = 3 and Ticket = 56);
      CPU := 16#100000# + Ticket * 16#100000# + Offset; DMA := CPU;
   end Resolve;
   package Table_Authority is new Intel_GPU_Table_Provenance.Authority (Resolve);
   use type Table_Authority.Append_Phase;
   type Append_Access is access Table_Authority.Append_State;
   Table_Appends : array (1 .. 1) of Append_Access;
   package Application_Images is
      package Tables is
         type Page_Mapping is record CPU, DMA : Unsigned_64 := 4096; end record;
      end Tables;
      function Retained_Root (State : Natural) return Tables.Page_Mapping is
        ((4096, (if Fault = 10 then 8192 else 4096)));
   end Application_Images;
   Application_Images_State : array (1 .. 1) of Natural := [others => 0];
   Live_VM_States : array (1 .. 1) of Natural := [others => 1];
   type Tag_Record is record label, length, flags, reserved : Unsigned_64 := 0; end record;
   type Request_Record is record tag : Tag_Record; words : Application_Buffers.Words := [0, 1, 2 ** 39, 4096]; end record;
   Update_Request : Request_Record;
   type Directory_Phase is
     (No_Directory_Update, Allocate_Directories, Register_Directories,
      Start_Directories, Publish_Directories, Invalidate_Directories, Commit_Directories);
   Directory_Update : Directory_Phase;
   Directory_First_ID, Directory_Previous_Used, Update_Table_Pages, Update_Index : Natural;
   Directory_Invalidated, In_Place_Active : Boolean := False;
   Update_Pending, Update_Session, Update_Sender, Update_Stamp : Unsigned_64;
   function Directory_Exclusive return Boolean is (Owner and Fault /= 1);
   function Directory_Owned (DMA : Unsigned_64) return Boolean is (DMA = 4096);
   procedure Invalidate_Update (OK : out Boolean) is
   begin
      pragma Assert (Publications = 1 and not In_Place_Active);
      Invalidations := Invalidations + 1; OK := Fault /= 6;
   end;
   package body Directory_Writer is
      procedure Rearm (Receipt : in out State; Source : Application_VM.Image;
                      Root : Unsigned_64; OK : out Boolean) is
      begin OK := False; end;
      procedure Start_From_Pages (Receipt : in out State; Source : Application_VM.Image;
                       GPU, Bytes, Root : Unsigned_64; Page_Count : Natural;
                       OK : out Boolean) is
      begin
         pragma Assert (not Directory_Invalidated and Directory_First_ID = 65);
         pragma Assert (Page_Count = 3 and Read_Page (1) /= 0 and Read_Page (3) /= 0);
         Receipt.Tried := True; OK := Fault /= 4;
      end;
      procedure Step (Receipt : in out State; Source : Application_VM.Image) is
      begin
         pragma Assert (Receipt.Tried and Invalidations = 0);
         Publications := Publications + 1; Receipt.Written := Fault /= 5;
      end;
      procedure Commit (Receipt : in out State; Source : in out Application_VM.Image;
                        OK : out Boolean) is
      begin
         pragma Assert (Directory_Invalidated and Invalidations = 1 and Receipt.Written);
         pragma Assert (R.Get (Private_Contexts (1).Table_IDs, 1, 5) = 0);
         OK := Fault /= 7;
         if OK then Receipt.Adopted := True; Source.Used := 7; Source.Revision := 2; end if;
      end;
   end Directory_Writer;
   procedure Publish_Snapshot (Message : String) is begin null; end;
   procedure Fail_Update is begin Failures := Failures + 1; Owner := False; end;
   procedure Reply_In_Place (Words : Application_Buffers.Words) is
   begin
      pragma Assert (Words (0) = Application_Buffers.Unavailable);
      Replies := Replies + 1; Owner := False; In_Place_Active := False;
   end;
   procedure Begin_Insertion (Object : Natural; Source : Application_VM.Image; State : Natural;
     Session, Sender, Stamp, Label, Length, Flags, Reserved : Unsigned_64;
     Request : Application_Buffers.Words; Words : out Application_Buffers.Words;
     Started : out Boolean) is
   begin
      pragma Assert (Source.Used = 7 and Source.Revision = 2 and State = 1);
      pragma Assert (Finishes = 1 and In_Place_Active and Update_Pending = 0);
      pragma Assert (Directory_Update = No_Directory_Update and Update_Table_Pages = 0);
      for P in 5 .. 7 loop
         pragma Assert (R.Get (Private_Contexts (1).Table_IDs, 1, P) = P + 60);
      end loop;
      pragma Assert (Invalidations = 1 and Private_Contexts (1).Growth.Adopted);
      Leaves := Leaves + 1; Started := Fault /= 9;
      Words := [Application_Buffers.Unavailable, 1, 0, 0];
   end;
'''
# The native array owns records directly; the fixture allocates fresh limited
# records for each fault case. Explicit dereferences preserve native operations.
body = body.replace("Table_Appends (Update_Index)", "Table_Appends (Update_Index).all")
suffix = r'''
   type Bytes is array (1 .. 4096) of Unsigned_8;
   Metadata : Bytes := [others => 0] with Alignment => 4096;
   References : Bytes := [others => 0] with Alignment => 4096;
   OK : Boolean;
   Backing : Intel_GPU_Buffer_Reply.Backing;
begin
   for Case_ID in 0 .. 13 loop
      Fault := 0;
      Private_Contexts (1) := new Item;
      if Case_ID /= 12 then
         R.Extend (Private_Contexts (1).Table_IDs,
           Unsigned_64 (To_Integer (References'Address)), 4096, OK);
         pragma Assert (OK);
      end if;
      for P in 1 .. 4 loop
         R.Put (Private_Contexts (1).Table_IDs, 1, P, P, OK);
         pragma Assert (OK);
      end loop;
      if Case_ID = 13 then
         R.Reopen (Private_Contexts (1).Table_IDs, 1, 2, OK);
         pragma Assert (OK);
      end if;
      Table_Appends (1) := new Table_Authority.Append_State;
      Intel_GPU_Table_Provenance.Extend (Private_Contexts (1).Table_Owners,
        Unsigned_64 (To_Integer (Metadata'Address)), 4096, OK); pragma Assert (OK);
      Table_Authority.Begin_Append (Table_Appends (1).all, Private_Contexts (1).Table_Owners,
        7, 1, 55, 0, 64, OK); pragma Assert (OK);
      for P in 1 .. 64 loop
         Table_Authority.Step (Table_Appends (1).all, Private_Contexts (1).Table_Owners);
      end loop;
      Fault := Case_ID; Owner := True; Publications := 0; Invalidations := 0;
      Finishes := 0; Leaves := 0; Replies := 0; Failures := 0;
      Directory_Update := Allocate_Directories; Directory_First_ID := 0;
      Directory_Previous_Used := 4; Update_Table_Pages := 3; Update_Index := 1;
      Directory_Invalidated := False; In_Place_Active := False;
      Update_Pending := 56; Update_Session := 7; Update_Sender := 8; Update_Stamp := 9;
      Backing := (True, (if Fault = 11 then 4096 else 12288));
      for Turn in 1 .. 20 loop
         Finish_Directory_Update (Backing);
         exit when Directory_Update = No_Directory_Update;
      end loop;
      pragma Assert (Directory_Update = No_Directory_Update and Update_Pending = 0);
      pragma Assert (Leaves = (if Fault in 0 | 9 then 1 else 0));
      pragma Assert (Replies = (if Fault = 0 then 0 else 1));
      pragma Assert (Failures = (if Fault in 0 | 9 then 0 else 1));
      if Fault = 0 then pragma Assert (In_Place_Active and Finishes = 1); end if;
      if Fault in 12 .. 13 then
         pragma Assert (Publications = 1 and Invalidations = 1);
         pragma Assert (Private_Contexts (1).Growth.Adopted);
         pragma Assert (not In_Place_Active and Finishes = 1 and not Owner);
         pragma Assert (R.Get (Private_Contexts (1).Table_IDs, 1, 5) = 0);
      end if;
      declare Before : constant Natural := Publications + Finishes + Leaves + Replies; begin
         Finish_Directory_Update (Backing);
         pragma Assert (Publications + Finishes + Leaves + Replies = Before);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Native directory dispatcher PASS: 14 scenarios; retained failure, directory TLB, stable IDs, failed map capacity/generation blocks leaf handoff");
end Native_Directory_Test;
'''
out = Path(tempfile.mkdtemp(prefix="cubit-native-directories."))
project = out / "test.gpr"
project.write_text(f'''project Test is
for Source_Dirs use ("{out}", "{root / 'userspace/services/intel-gpu'}");
for Object_Dir use "obj";
for Main use ("native_directory_test.adb");
package Compiler is
for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato", "-O2");
end Compiler;
end Test;
''')
variants = {
    "native": body,
    "omit-invalidation": body.replace("Invalidate_Update (Directory_Invalidated);", "Directory_Invalidated := True;"),
    "wrong-image-ids": body.replace("Directory_First_ID + P - 1, OK);", "P, OK);"),
    "ignore-put-failure": body.replace(
        "if OK then\n                  Application_Buffers.Finish_Private",
        "if True then\n                  Application_Buffers.Finish_Private"),
}
assert all(variant != body for name, variant in variants.items() if name != "native")
for name, variant in variants.items():
    (out / "native_directory_test.adb").write_text(prefix + variant + suffix)
    built = subprocess.run(["gprbuild", "-f", "-p", "-P", str(project)], capture_output=True, text=True)
    (out / f"{name}-build.log").write_text(built.stdout + built.stderr)
    if built.returncode:
        raise SystemExit(f"compile failed: {out}/{name}-build.log\n{built.stdout}{built.stderr}")
    run = subprocess.run([str(out / "obj/native_directory_test")], capture_output=True, text=True)
    (out / f"{name}-run.log").write_text(run.stdout + run.stderr)
    assert (run.returncode == 0) == (name == "native"), (name, run.stdout, run.stderr, out)
    print(name, run.returncode, run.stdout.strip())
assert source.read_bytes() == original
(out / "evidence.json").write_text(json.dumps({"source_sha256": hashlib.sha256(original).hexdigest(), "variants": list(variants)}, indent=2))
print("Evidence:", out)
