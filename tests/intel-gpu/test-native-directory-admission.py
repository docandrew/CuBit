#!/usr/bin/env python3
"""Native directory allocation choice against the real VM topology planner."""
from pathlib import Path
import hashlib
import json
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = root / "userspace/services/intel-gpu/main.adb"
original = source.read_bytes()
text = original.decode()
start = text.index("                  declare\n                     Needed : constant Application_Topology.Requirements", text.index("   procedure Handle_VM_Update"))
end = text.index("                  if Update_Pending /= 0 then", start)
body = text[start:end]
prefix = r'''with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Growth;
with Intel_GPU_ADLN_PPGTT;
with Intel_GPU_Table_Provenance;
procedure Native_Directory_Admission_Test is
   package Application_VM is new Intel_GPU_VM_Image (8, 4, Bootstrap_Descriptors => 4);
   package Application_Topology is new Application_VM.Growth;
   type Item is limited record
      Source : Application_VM.Image;
      Table_Owners : Intel_GPU_Table_Provenance.Ledger;
      Table_IDs : Natural := 0;
   end record;
   type Item_Access is access Item;
   Private_Contexts : array (1 .. 1) of Item_Access;
   Current_Table_Ticket : array (1 .. 1) of Unsigned_64 := [others => 0];
   Update_Index : Natural := 1;
   Update_Session : Unsigned_64 := 7;
   Update_Pending : Unsigned_64 := 0;
   Update_Table_Pages, Directory_First_ID, Directory_Previous_Used : Natural := 0;
   Directory_Invalidated : Boolean := False;
   type Directory_Phase is (No_Directory_Update, Allocate_Directories);
   Directory_Update : Directory_Phase := No_Directory_Update;
   Application_Buffer_State, Reserves : Natural := 0;
   package Application_Buffers is
      Unavailable : constant := 3;
      type Private_Table_Kind is (Replacement_Tables, Incremental_Tables);
      procedure Reserve_Private (Object : Natural; Session : Unsigned_64; ID : out Unsigned_64;
        Reclaimable : Boolean := False; Kind : Private_Table_Kind := Replacement_Tables);
   end Application_Buffers;
   use type Application_Buffers.Private_Table_Kind;
   Selected : Application_Buffers.Private_Table_Kind;
   package body Application_Buffers is
      procedure Reserve_Private (Object : Natural; Session : Unsigned_64; ID : out Unsigned_64;
        Reclaimable : Boolean := False; Kind : Private_Table_Kind := Replacement_Tables) is
      begin
         pragma Assert (Session = 7 and Reclaimable);
         Reserves := Reserves + 1; Selected := Kind; ID := 56;
      end;
   end Application_Buffers;
   type Word_Array is array (0 .. 3) of Unsigned_64;
   type Tag is record Label, Length, Flags, Reserved : Natural; end record;
   type Message is record words : Word_Array; tag : Native_Directory_Admission_Test.Tag; end record;
   Msg, Update_Request, Response : Message;
   Started, Update_Held, In_Place_Active, Directory_Metadata_Pending : Boolean := False;
   Delivered, Directory_Metadata_Epoch : Unsigned_64 := 0;
   Directory_Metadata_Target : Positive := 1;
   Live_Metadata_Pending : Boolean := False;
   Live_Metadata_Epoch : Unsigned_64 := 0;
   Stored : constant := 1;
   Available_Links : Positive := 8;
   Requests, Saves : Natural := 0;
   Saved : Boolean := False;
   function Directory_Link_Capacity return Positive is (Available_Links);
   package Application_State is
      Table_Pages : constant := 8;
      package Table_References is
         function Capacity (Object : Natural) return Positive is (8);
      end;
   end;
   package Application_Binding is Update_Label : constant := 2; end;
   function Save_Update_Reply return Boolean is
   begin Saves := Saves + 1; Saved := True; return True; end;
   function Send_Update_Reply (Value : Message) return Unsigned_64 is
   begin pragma Assert (False); return 0; end;
   procedure Request_Context_Metadata (Index, Tables, Records : Positive; OK : out Boolean) is
   begin
      pragma Assert (Saved and Live_Metadata_Pending and In_Place_Active and not Update_Held);
      pragma Assert (Reserves = 0 and Update_Pending = 0 and Index = 1 and Tables = 7);
      pragma Assert (Records = (if Current_Table_Ticket (1) = 0 then 19 else 1));
      pragma Assert (Live_Metadata_Epoch = Application_VM.Revision (Private_Contexts (1).Source));
      Requests := Requests + 1; OK := True;
   end;
   package Directory_Metadata_Growth is
      type Phase is (Empty, Idle, Opening);
      type View is record State : Phase; end record;
      function Snapshot (Object : Phase) return View is ((State => Object));
      procedure Configure (Object : in out Phase; Bytes : Unsigned_64; Quota : Positive; OK : out Boolean);
      procedure Request (Object : in out Phase; Records : Positive; OK : out Boolean);
   end;
   use type Directory_Metadata_Growth.Phase;
   package body Directory_Metadata_Growth is
      procedure Configure (Object : in out Phase; Bytes : Unsigned_64; Quota : Positive; OK : out Boolean) is
      begin Object := Idle; OK := True; end;
      procedure Request (Object : in out Phase; Records : Positive; OK : out Boolean) is
      begin
         pragma Assert (Saved and Object = Idle and Records = 3 and Reserves = 0 and not Update_Held);
         Requests := Requests + 1; Object := Opening; OK := True;
      end;
   end;
   Directory_Metadata : array (1 .. 1) of Directory_Metadata_Growth.Phase;
   procedure Resolve (Session, Ticket, Offset : Unsigned_64;
                      CPU, DMA : out Unsigned_64; Accepted : out Boolean) is
   begin CPU := 16#100000# + Offset; DMA := CPU; Accepted := True; end;
   package A is new Intel_GPU_Table_Provenance.Authority (Resolve);
   procedure Select_Allocation is
   begin
'''
suffix = r'''
   end Select_Allocation;
   type Bytes is array (1 .. 4096) of Unsigned_8;
   Metadata : Bytes := [others => 0] with Alignment => 4096;
   Descriptors : Bytes := [others => 0] with Alignment => 4096;
   Mirrors : array (1 .. 4) of Bytes := [others => [others => 0]] with Alignment => 4096;
   Backing : Application_VM.Backing_Pages :=
     [for I in Application_VM.Page_Number => (if I <= 4 then Unsigned_64 (I) * 4096 else 0)];
   OK : Boolean;
   GPU : Unsigned_64;
begin
   for Case_ID in 0 .. 9 loop
      Available_Links := (if Case_ID = 8 then 2 else 8);
      Directory_Metadata := [others => Directory_Metadata_Growth.Empty];
      Requests := 0; Saves := 0; Saved := False; Directory_Metadata_Pending := False;
      Live_Metadata_Pending := False;
      Update_Index := 1; In_Place_Active := False; Update_Held := False;
      Private_Contexts (1) := new Item;
      if Case_ID /= 9 then
         Application_VM.Extend_Descriptors (Private_Contexts (1).Source,
           Unsigned_64 (To_Integer (Descriptors'Address)), 4096, OK); pragma Assert (OK);
         Application_VM.Extend_Metadata (Private_Contexts (1).Source,
           Unsigned_64 (To_Integer (Mirrors'Address)), 16384, OK); pragma Assert (OK);
      end if;
      Application_VM.Initialize (Private_Contexts (1).Source, Backing, OK, Backing_Count => 4); pragma Assert (OK);
      Application_VM.Map_Page (Private_Contexts (1).Source, 4096, 16#100000#,
        Intel_GPU_ADLN_PPGTT.Write_Back, Intel_GPU_ADLN_PPGTT.Read_Write, OK); pragma Assert (OK);
      Application_VM.Seal (Private_Contexts (1).Source, OK); pragma Assert (OK);
      if Case_ID /= 4 then
         Intel_GPU_Table_Provenance.Extend (Private_Contexts (1).Table_Owners,
           Unsigned_64 (To_Integer (Metadata'Address)), 4096, OK); pragma Assert (OK);
      else
         -- Exhaust bootstrap ledger metadata without changing VM table quota.
         for I in 1 .. 16 loop
            A.Install (Private_Contexts (1).Table_Owners, 7, 1, I, 55,
              Unsigned_64 (I - 1) * 4096, OK); pragma Assert (OK);
         end loop;
      end if;
      Current_Table_Ticket (1) := (if Case_ID in 3 | 9 then 55 else 0);
      GPU := (case Case_ID is when 0 => 2 ** 21, when 1 => 2 ** 30,
        when 5 => 4096, when 6 => 3, when others => 2 ** 39);
      Msg.words := [0, 1, GPU, (if Case_ID = 7 then 2 ** 40 else 4096)];
      Update_Pending := 0; Update_Table_Pages := 999; Reserves := 0;
      Directory_Update := Allocate_Directories; Directory_First_ID := 99; Directory_Invalidated := True;
      Select_Allocation;
      pragma Assert (Directory_First_ID = 0 and not Directory_Invalidated);
      if Case_ID in 4 | 9 then
         pragma Assert (Reserves = 0 and Update_Pending = 0 and Update_Table_Pages = 0);
         pragma Assert (Live_Metadata_Pending and In_Place_Active and not Update_Held);
         pragma Assert (Requests = 1 and Saves = 1);
      elsif Case_ID = 8 then
         pragma Assert (Reserves = 0 and Update_Pending = 0 and Update_Table_Pages = 0);
         pragma Assert (Directory_Metadata_Pending and In_Place_Active and not Update_Held);
         pragma Assert (Requests = 1 and Saves = 1 and Directory_Metadata_Target = 3);
         pragma Assert (Directory_Metadata_Epoch = Application_VM.Revision (Private_Contexts (1).Source));
      elsif Case_ID in 5 .. 7 then
         pragma Assert (Reserves = 0 and Update_Pending = 0 and Update_Table_Pages = 0);
         pragma Assert (Directory_Update = No_Directory_Update);
      else
         pragma Assert (Reserves = 1 and Update_Pending = 56);
         if Case_ID <= 2 then
            pragma Assert (Selected = Application_Buffers.Incremental_Tables);
            pragma Assert (Update_Table_Pages = Case_ID + 1 and Directory_Update = Allocate_Directories);
         else
            pragma Assert (Selected = Application_Buffers.Replacement_Tables);
            pragma Assert (Update_Table_Pages = 7 and Directory_Update = No_Directory_Update);
         end if;
      end if;
   end loop;
   Ada.Text_IO.Put_Line ("Native directory admission PASS10: actual topology, incremental/replacement metadata demand, exact size/role and rejected ranges");
end Native_Directory_Admission_Test;
'''
out = Path(tempfile.mkdtemp(prefix="cubit-native-directory-admission."))
project = out / "test.gpr"
project.write_text(f'''project Test is
for Source_Dirs use ("{out}", "{root / 'userspace/services/intel-gpu'}");
for Object_Dir use "obj";
for Main use ("native_directory_admission_test.adb");
package Compiler is
for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato", "-O2");
end Compiler;
end Test;
''')
variants = {
    "native": body,
    "oversized-incremental": body.replace("Update_Table_Pages := Needed.Additional_Tables;", "Update_Table_Pages := Directory_Previous_Used + Needed.Additional_Tables;"),
    "wrong-ticket-role": body.replace("Application_Buffers.Incremental_Tables else", "Application_Buffers.Replacement_Tables else"),
}
for name, variant in variants.items():
    (out / "native_directory_admission_test.adb").write_text(prefix + variant + suffix)
    built = subprocess.run(["gprbuild", "-f", "-p", "-P", str(project)], capture_output=True, text=True)
    (out / f"{name}-build.log").write_text(built.stdout + built.stderr)
    if built.returncode:
        raise SystemExit(f"compile failed: {out}/{name}-build.log\n{built.stdout}{built.stderr}")
    run = subprocess.run([str(out / "obj/native_directory_admission_test")], capture_output=True, text=True)
    (out / f"{name}-run.log").write_text(run.stdout + run.stderr)
    assert (run.returncode == 0) == (name == "native"), (name, run.stdout, run.stderr, out)
    print(name, run.returncode, run.stdout.strip())
assert source.read_bytes() == original
(out / "evidence.json").write_text(json.dumps({"source_sha256": hashlib.sha256(original).hexdigest(), "variants": list(variants)}, indent=2))
print("Evidence:", out)
