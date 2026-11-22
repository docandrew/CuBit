#!/usr/bin/env python3
"""Native deferred offline bind finalizer with real service, VM and provenance."""
from pathlib import Path
import subprocess
import tempfile
import hashlib
import json

root = Path(__file__).resolve().parents[2]
source = root / "userspace/services/intel-gpu/main.adb"
original = source.read_bytes()
text = original.decode()
start = text.index("   procedure Finish_Offline_Bind (")
end = text.index("   end Finish_Offline_Bind;", start) + len("   end Finish_Offline_Bind;")
body = text[start:end].replace("Table_Appends (Update_Index)", "Table_Appends (Update_Index).all")
prefix = r'''with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Growth;
with Intel_GPU_Table_Provenance;
with Intel_GPU_Table_References;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Requests.Binding;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Buffer_Backing;
procedure Native_Offline_Test is
   Fault, Replies, Failures, Installs : Natural := 0;
   Owner : Boolean := True;
   function Owner_Ready return Boolean is (Owner);
   function Session_Of (Sender, Stamp : Unsigned_64) return Unsigned_64 is
     (if Owner and Sender = 42 and Stamp = 99 then 99 else 0);
   package Application_VM is new Intel_GPU_VM_Image (8);
   package Application_Topology is new Application_VM.Growth;
   package Application_Buffers is new Intel_GPU_Buffer_Requests (Session_Of, Owner_Ready);
   package Application_Binding is new Application_Buffers.Binding (Application_VM);
   type Service_Access is access Application_Buffers.Service;
   Service : Service_Access;
   -- A rename inside Run gives the extracted procedure a fresh real service.
   package Application_State is
      package Table_References is new Intel_GPU_Table_References (8);
   end;
   package R renames Application_State.Table_References;
   type Item is limited record
      Source : Application_VM.Image;
      Table_Owners : Intel_GPU_Table_Provenance.Ledger;
      Table_Generation : Unsigned_64 := 1;
      Table_IDs : R.Map;
   end record;
   type Item_Access is access Item;
   Private_Contexts : array (1 .. 1) of Item_Access;
   Update_Index, Update_Table_Pages : Natural;
   Update_Pending : Application_Buffers.Ticket;
   Update_Session, Update_Sender, Update_Stamp, Offline_Epoch : Unsigned_64;
   type Tag_Type is record label : Unsigned_32 := Application_Binding.Bind_Label;
      length, flags : Unsigned_8 := 0; reserved : Unsigned_16 := 0; end record;
   type Message is record tag : Tag_Type; words : Application_Buffers.Words := [others => 0]; end record;
   NULL_MESSAGE : constant Message := (others => <>);
   Update_Request, Last_Reply : Message;
   Application_Reply_Slot : constant := 62;
   type Offline_Bind_Phase is (No_Offline_Bind, Grow_Offline_Metadata, Allocate_Offline, Register_Offline, Commit_Offline);
   Offline_Bind_State : Offline_Bind_Phase;
   Context_Metadata : array (1 .. 1) of Natural := [0];
   package Context_Metadata_Growth is
      type Phase is (Idle, Checking, Failed);
      procedure Step (Object : in out Natural) is null;
      function State (Object : Natural) return Phase is (Idle);
   end Context_Metadata_Growth;
   function Ledger_Capacity return Positive is
     (Intel_GPU_Table_Provenance.Capacity (Private_Contexts (1).Table_Owners));
   Buffer_Pool : Natural := 0;
   package Buffer_Memory is
      procedure Start (Pool : Natural; Slot, Pages : Positive; OK : out Boolean);
   end Buffer_Memory;
   package body Buffer_Memory is
      procedure Start (Pool : Natural; Slot, Pages : Positive; OK : out Boolean) is
      begin
         pragma Assert (Offline_Bind_State = Allocate_Offline and Pages = 3);
         pragma Assert (Slot = Application_Buffers.Ticket_Slot (Update_Pending));
         pragma Assert (Installs = 0 and Replies = 0);
         OK := True;
      end;
   end Buffer_Memory;
   procedure Resolve (Session, Ticket, Offset : Unsigned_64;
      CPU, DMA : out Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := Owner and Session = 99 and not (Fault = 3 and Ticket = Update_Pending);
      DMA := (if Ticket = 55 then 4096 else 16#300000#) + Offset;
      CPU := DMA + 16#10000000#;
   end Resolve;
   package Table_Authority is new Intel_GPU_Table_Provenance.Authority (Resolve);
   use type Table_Authority.Append_Phase;
   type Append_Access is access Table_Authority.Append_State;
   Table_Appends : array (1 .. 1) of Append_Access;
   Table_Backing_Registry : Natural := 0;
   package Table_Allocations is
      type Allocation_Role is (Incremental_Tables);
      procedure Install (Registry : Natural; Slot : Positive; Session, Ticket : Unsigned_64;
        Role : Allocation_Role; Backing : Intel_GPU_Buffer_Reply.Backing; OK : out Boolean);
   end Table_Allocations;
   package body Table_Allocations is
      procedure Install (Registry : Natural; Slot : Positive; Session, Ticket : Unsigned_64;
        Role : Allocation_Role; Backing : Intel_GPU_Buffer_Reply.Backing; OK : out Boolean) is
      begin
         Installs := Installs + 1;
         pragma Assert (Ticket = Update_Pending and Session = 99 and
           Slot = Application_Buffers.Ticket_Slot (Ticket) and Backing.Bytes = 12288);
         OK := Fault /= 2;
      end;
   end Table_Allocations;
   function Offline_Bind_Owner return Boolean is
      Status : Application_Binding.Preparation_Result;
      use type Application_Binding.Preparation_Result;
   begin
      if not Owner or Fault = 1 then return False; end if;
      Application_Binding.Check_Offline_Bind_Request (Service.all,
        Private_Contexts (1).Source, 99, Offline_Epoch, 42, 99,
        Update_Request.tag.label, 4, 0, 0, Update_Request.words, Status);
      return Status = Application_Binding.Eligible;
   end Offline_Bind_Owner;
   procedure Reject_Offline_Bind is begin Failures := Failures + 1; Owner := False; end;
   procedure Publish_Snapshot (Value : String) is begin null; end;
   function replyCap (Slot : Natural; Reply : Message) return Unsigned_64 is
   begin
      pragma Assert (Slot = 62); Replies := Replies + 1; Last_Reply := Reply;
      return (if Fault = 7 then 0 else 1);
   end;
   procedure Run is
      Application_Buffer_State : Application_Buffers.Service renames Service.all;
'''
suffix = r'''
      OK, Consumed : Boolean;
      Ticket : Application_Buffers.Ticket;
      Words : Application_Buffers.Words;
      Backing : Intel_GPU_Buffer_Reply.Backing;
      Prior : Unsigned_64;
      type Bytes is array (1 .. 4096) of Unsigned_8;
      Reference_Metadata : Bytes := [others => 0] with Alignment => 4096;
   begin
      if Fault /= 8 then
         R.Extend (Private_Contexts (1).Table_IDs,
           Unsigned_64 (To_Integer (Reference_Metadata'Address)), 4096, OK);
         pragma Assert (OK);
      end if;
      for P in 1 .. 4 loop
         R.Put (Private_Contexts (1).Table_IDs, 1, P, P, OK);
         pragma Assert (OK);
      end loop;
      if Fault = 9 then
         R.Reopen (Private_Contexts (1).Table_IDs, 1, 2, OK);
         pragma Assert (OK);
      end if;
      Application_Buffers.Handle (Service.all, 42, 99, Application_Buffers.Label, 4, 0, 0,
        [1, Application_Buffers.Create, 8192, 0], Words, Ticket);
      pragma Assert (Ticket /= 0);
      Application_Buffers.Complete (Service.all, Ticket, Intel_GPU_Buffer_Reply.From_Linear
        (16#900000#, Intel_GPU_Buffer_Backing.CPU_Base, 8192, 16#900000#), Words, OK);
      pragma Assert (OK);
      Update_Request := ((Application_Binding.Bind_Label, 4, 0, 0),
        [1 + 2 ** 32, Words (2), 2 ** 39, 4096]);
      Application_VM.Initialize (Private_Contexts (1).Source,
        [1 => 4096, 2 => 8192, 3 => 12288, 4 => 16384, others => 0], OK, Backing_Count => 4);
      pragma Assert (OK);
      Application_VM.Map_Page (Private_Contexts (1).Source, 4096, 16#100000#,
        Write_Back, Read_Write, OK); pragma Assert (OK);
      Table_Authority.Begin_Append (Table_Appends (1).all, Private_Contexts (1).Table_Owners,
        99, 1, 55, 0, 4, OK); pragma Assert (OK);
      for I in 1 .. 4 loop Table_Authority.Step (Table_Appends (1).all, Private_Contexts (1).Table_Owners); end loop;
      Application_Buffers.Reserve_Private (Service.all, 99, Update_Pending,
        Reclaimable => True, Kind => Application_Buffers.Incremental_Tables);
      pragma Assert (Update_Pending /= 0);
      Update_Index := 1; Update_Table_Pages := 3; Update_Session := 99; Update_Sender := 42; Update_Stamp := 99;
      Offline_Epoch := Application_VM.Revision (Private_Contexts (1).Source);
      Prior := Offline_Epoch; Offline_Bind_State := Grow_Offline_Metadata;
      Backing := Intel_GPU_Buffer_Reply.From_Linear
        (16#300000#, Intel_GPU_Buffer_Backing.CPU_Base + 16#200000#, 12288, 16#100000#);
      pragma Assert (Intel_GPU_Buffer_Reply.Valid (Backing));
      if Fault = 4 then Backing := (Ready => False); end if;
      for Turn in 1 .. 12 loop
         if Fault = 5 and Offline_Bind_State = Register_Offline then Offline_Epoch := 999; end if;
         if Fault = 6 and Offline_Bind_State = Commit_Offline then
            Application_Buffers.Finish_Private (Service.all, Update_Pending, Consumed);
         end if;
         Finish_Offline_Bind (Backing);
         Ada.Text_IO.Put_Line ("case" & Fault'Image & " turn" & Turn'Image &
           " phase " & Offline_Bind_State'Image & " failures" & Failures'Image);
         exit when Offline_Bind_State = No_Offline_Bind;
         pragma Assert (Replies = 0);
      end loop;
      pragma Assert (Offline_Bind_State = No_Offline_Bind and Update_Pending = 0 and Update_Index = 0);
      pragma Assert (Replies = 1);
      if Fault in 0 | 7 then
         Ada.Text_IO.Put_Line ("reply" & Last_Reply.words (0)'Image);
         pragma Assert (Last_Reply.words (0) = Application_Buffers.OK);
         pragma Assert (Failures = (if Fault = 7 then 1 else 0));
         pragma Assert (Application_VM.Revision (Private_Contexts (1).Source) = Prior + 1);
         pragma Assert (Application_VM.Used (Private_Contexts (1).Source) = 7);
         pragma Assert (Application_VM.Lookup (Private_Contexts (1).Source, 2 ** 39) =
           Encode_Leaf (16#901000#, Write_Back, Read_Write));
         for P in 5 .. 7 loop
            pragma Assert (R.Get (Private_Contexts (1).Table_IDs, 1, P) = P);
         end loop;
      else
         pragma Assert (Last_Reply.words (0) /= Application_Buffers.OK and Failures = 1);
         if Fault in 8 .. 9 then
            -- Appended private backing is retained after Put fails; it must
            -- not lead to leaf binding, epoch adoption or successful reply.
            pragma Assert (Application_VM.Backed_Tables (Private_Contexts (1).Source) = 7);
            pragma Assert (Application_VM.Lookup (Private_Contexts (1).Source, 2 ** 39) = 0);
            pragma Assert (Offline_Epoch = Prior);
            pragma Assert (R.Get (Private_Contexts (1).Table_IDs, 1, 5) = 0);
         end if;
      end if;
      Finish_Offline_Bind (Backing); pragma Assert (Replies = 1);
   end Run;
begin
   for Case_ID in 0 .. 9 loop
      Fault := Case_ID; Owner := True; Replies := 0; Failures := 0; Installs := 0;
      Update_Pending := 0;
      Service := new Application_Buffers.Service;
      Private_Contexts (1) := new Item;
      Table_Appends (1) := new Table_Authority.Append_State;
      Run;
   end loop;
   Ada.Text_IO.Put_Line ("Native offline bind PASS10: real service/VM/provenance/reference map, deferred reply, revalidation, failed ID capacity/generation, retained failure and reply loss");
end Native_Offline_Test;
'''
out = Path(tempfile.mkdtemp(prefix="cubit-native-offline-bind."))
project = out / "test.gpr"
project.write_text(f'''project Test is
for Source_Dirs use ("{out}", "{root / 'userspace/services/intel-gpu'}");
for Object_Dir use "obj";
for Main use ("native_offline_test.adb");
package Compiler is
for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato", "-O2");
end Compiler;
end Test;
''')
variants = {"native": body, "omit-reply-loss": body.replace("if Delivered /= 1 then Reject_Offline_Bind; end if;", "null;"),
            "wrong-id": body.replace("First + P - 1, ID + P - 1, OK);", "First + P - 1, 1, OK);"),
            "ignore-put-failure": body.replace("if OK then\n                        Offline_Epoch :=", "if True then\n                        Offline_Epoch :=")}
assert all(variant != body for name, variant in variants.items() if name != "native")
for name, variant in variants.items():
    (out / "native_offline_test.adb").write_text(prefix + variant + suffix)
    built = subprocess.run(["gprbuild", "-f", "-p", "-P", str(project)], capture_output=True, text=True)
    (out / f"{name}-build.log").write_text(built.stdout + built.stderr)
    if built.returncode:
        raise SystemExit(f"compile failed: {out}/{name}-build.log\n{built.stdout}{built.stderr}")
    run = subprocess.run([str(out / "obj/native_offline_test")], capture_output=True, text=True)
    (out / f"{name}-run.log").write_text(run.stdout + run.stderr)
    assert (run.returncode == 0) == (name == "native"), (name, run.stdout, run.stderr, out)
    print(name, run.returncode, run.stdout.strip())
assert source.read_bytes() == original
(out / "evidence.json").write_text(json.dumps({"source_sha256": hashlib.sha256(original).hexdigest(), "variants": list(variants)}, indent=2))
print("Evidence:", out)
