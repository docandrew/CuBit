#!/usr/bin/env python3
"""Actual offline admission function; real binding, topology and ticket service."""
from pathlib import Path
import subprocess
import tempfile
import hashlib
import json

root = Path(__file__).resolve().parents[2]
source = root / "userspace/services/intel-gpu/main.adb"
original = source.read_bytes()
text = original.decode()
start = text.index("   function Try_Offline_Bind_Growth (From : ProcessID; Msg : Message) return Boolean is")
end = text.index("   end Try_Offline_Bind_Growth;", start) + len("   end Try_Offline_Bind_Growth;")
body = text[start:end]
prefix = r'''with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Growth;
with Intel_GPU_Table_Provenance;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Requests.Binding;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_ADLN_PPGTT;
procedure Offline_Admission is
   Fault, Starts, Saves, Rejects, Finishes : Natural := 0;
   function Owner return Boolean is (True);
   function Application_Session (Sender, Stamp : Unsigned_64) return Unsigned_64 is
     (if Sender = 42 and Stamp = 99 then 99 else 0);
   subtype ProcessID is Unsigned_64;
   package Application_VM is new Intel_GPU_VM_Image (8, 4, Bootstrap_Descriptors => 4);
   package Application_Topology is new Application_VM.Growth;
   package Application_Buffers is new Intel_GPU_Buffer_Requests (Application_Session, Owner);
   package Application_Binding is new Application_Buffers.Binding (Application_VM);
   type Service_Access is access Application_Buffers.Service;
   Service : Service_Access;
   package Application_Lifetime is
      type Phase is (Offline, Published);
   end Application_Lifetime;
   use type Application_Lifetime.Phase;
   type Item is limited record
      Source : Application_VM.Image;
      Table_Owners : Intel_GPU_Table_Provenance.Ledger;
      Life : Application_Lifetime.Phase := Application_Lifetime.Offline;
   end record;
   type Item_Access is access Item;
   Private_Contexts : array (1 .. 1) of Item_Access;
   package Intel_GPU_Render_Sessions is
      subtype Slot_Index is Natural range 0 .. 1;
   end Intel_GPU_Render_Sessions;
   Render_Admission : Natural := 0;
   package Intel_GPU_Render_Control is
      function Storage_Index (Object : Natural; Session : Unsigned_64) return Natural is
        (if Session = 99 then 1 else 0);
      function Recipient_Identity (Object : Natural; Sender, Stamp : Unsigned_64) return Unsigned_64 is
        (if Application_Session (Sender, Stamp) = 99 then 1234 else 0);
   end Intel_GPU_Render_Control;
   type Tag_Type is record label : Unsigned_32; length, flags : Unsigned_8; reserved : Unsigned_16; end record;
   type Message is record tag : Tag_Type; words : Application_Buffers.Words; authorityTag : Unsigned_64; end record;
   Update_Request : Message;
   Update_Index, Update_Table_Pages : Natural := 0;
   Update_Session, Update_Sender, Update_Stamp, Update_Identity, Offline_Epoch : Unsigned_64;
   Update_Pending, Application_Pending, Private_Pending, Buffer_Retirement_Pending : Unsigned_64 := 0;
   In_Place_Active, Table_Ledger_Busy : Boolean := False;
   type Offline_Phase is (No_Offline_Bind, Grow_Offline_Metadata, Allocate_Offline);
   Offline_Bind_State : Offline_Phase := No_Offline_Bind;
   Application_Reply_Slot : constant := 62;
   Buffer_Pool : Natural := 0;
   package Buffer_Memory is
      function Pending (Pool : Natural) return Boolean is (Fault = 9);
      procedure Start (Pool : Natural; Slot, Pages : Positive; Started : out Boolean);
   end Buffer_Memory;
   package body Buffer_Memory is
      procedure Start (Pool : Natural; Slot, Pages : Positive; Started : out Boolean) is
      begin
         Starts := Starts + 1;
         pragma Assert (Saves = 1 and Slot = Application_Buffers.Ticket_Slot (Update_Pending));
         pragma Assert (Pages = (if Fault = 1 then 1 elsif Fault = 2 then 2 else 3));
         pragma Assert (Update_Session = 99 and Update_Sender = 42 and Update_Stamp = 99 and Update_Identity = 1234);
         pragma Assert (Offline_Epoch = Application_VM.Revision (Private_Contexts (1).Source));
         Started := Fault /= 12;
      end Start;
   end Buffer_Memory;
   procedure Request_Context_Metadata (Index, Tables, Records : Positive; OK : out Boolean) is
   begin
      Starts := Starts + 1;
      pragma Assert (Saves = 1 and Index = 1 and Offline_Bind_State = Grow_Offline_Metadata);
      pragma Assert (Tables = (if Fault = 1 then 5 elsif Fault = 2 then 6 else 7));
      pragma Assert (Update_Table_Pages = (if Fault = 1 then 1 elsif Fault = 2 then 2 else 3));
      pragma Assert (Records = Intel_GPU_Table_Provenance.Count (Private_Contexts (1).Table_Owners) +
        (if Fault = 1 then 1 elsif Fault = 2 then 2 else 3));
      pragma Assert (Update_Session = 99 and Update_Sender = 42 and Update_Stamp = 99 and Update_Identity = 1234);
      OK := Fault /= 12;
   end Request_Context_Metadata;
   function Offline_Bind_Owner return Boolean is (Fault /= 10);
   function saveReplyCap (Slot : Unsigned_64) return Unsigned_64 is
   begin pragma Assert (Slot = 62); Saves := Saves + 1; return (if Fault = 11 then 0 else 1); end;
   procedure Reject_Offline_Bind is begin Rejects := Rejects + 1; end;
   procedure Finish_Offline_Bind (Backing : Intel_GPU_Buffer_Reply.Backing) is
      Consumed : Boolean;
   begin
      pragma Assert (not Backing.Ready);
      Finishes := Finishes + 1;
      Application_Buffers.Finish_Private (Service.all, Update_Pending, Consumed);
      pragma Assert (Consumed);
      Update_Pending := 0; Offline_Bind_State := No_Offline_Bind;
   end Finish_Offline_Bind;
   procedure Resolve (Session, Ticket, Offset : Unsigned_64; CPU, DMA : out Unsigned_64; OK : out Boolean) is
   begin CPU := 16#100000# + Offset; DMA := CPU; OK := True; end;
   package Ledger_Authority is new Intel_GPU_Table_Provenance.Authority (Resolve);
   procedure Run is
      Application_Buffer_State : Application_Buffers.Service renames Service.all;
'''
suffix = r'''
      Msg : Message;
      Words : Application_Buffers.Words;
      Ticket : Application_Buffers.Ticket;
      OK, Taken, Consumed : Boolean;
      GPU : Unsigned_64;
   begin
      Application_Buffers.Handle (Service.all, 42, 99, Application_Buffers.Label, 4, 0, 0,
        [1, Application_Buffers.Create, 8192, 0], Words, Ticket);
      pragma Assert (Ticket /= 0);
      Application_Buffers.Complete (Service.all, Ticket, Intel_GPU_Buffer_Reply.From_Linear
        (16#900000#, Intel_GPU_Buffer_Backing.CPU_Base, 8192, 16#900000#), Words, OK);
      pragma Assert (OK);
      Application_VM.Initialize (Private_Contexts (1).Source,
        [1 => 4096, 2 => 8192, 3 => 12288, 4 => 16384, others => 0], OK, Backing_Count => 4);
      pragma Assert (OK);
      Application_VM.Map_Page (Private_Contexts (1).Source, 4096, 16#100000#,
        Intel_GPU_ADLN_PPGTT.Write_Back, Intel_GPU_ADLN_PPGTT.Read_Write, OK); pragma Assert (OK);
      GPU := (if Fault = 1 then 2 ** 21 elsif Fault = 2 then 2 ** 30
        elsif Fault = 13 then 8192 elsif Fault = 14 then 3 else 2 ** 39);
      Msg := ((Application_Binding.Bind_Label, 4, 0, 0), [1, Words (2), GPU, 4096], 99);
      if Fault in 0 .. 2 then
         pragma Assert (not Application_Topology.Inspect_Offline
           (Private_Contexts (1).Source, GPU, 4096).Topology.Fits_Reserved);
         pragma Assert (Application_Topology.Inspect_Offline
           (Private_Contexts (1).Source, GPU, 4096).Topology.Fits_Quota);
      end if;
      case Fault is
         when 3 => Msg.authorityTag := 7;
         when 4 => Msg.tag.length := 3;
         when 5 => Update_Pending := 77;
         when 6 => Application_Pending := 77;
         when 7 => In_Place_Active := True;
         when 8 => Table_Ledger_Busy := True;
         when 15 => Private_Contexts (1).Life := Application_Lifetime.Published;
         when 16 =>
            for I in 1 .. Intel_GPU_Table_Provenance.Capacity (Private_Contexts (1).Table_Owners) loop
               Ledger_Authority.Install (Private_Contexts (1).Table_Owners, 99, 1, I,
                 55, Unsigned_64 (I - 1) * 4096, OK); pragma Assert (OK);
            end loop;
         when 17 => Private_Pending := 77;
         when 18 => Buffer_Retirement_Pending := 77;
         when 19 => Msg.words (1) := 0;
         when others => null;
      end case;
      Taken := Try_Offline_Bind_Growth (42, Msg);
      if Fault in 0 .. 2 or else Fault = 16 then
         pragma Assert (Taken and Starts = 1 and Saves = 1 and Rejects = 0 and Finishes = 0);
         pragma Assert (Application_Buffers.Is_Table_Allocation (Service.all, 99, Update_Pending,
           Application_Buffers.Incremental_Tables));
         Application_Buffers.Finish_Private (Service.all, Update_Pending, Consumed);
         pragma Assert (Consumed);
      elsif Fault in 10 .. 11 then
         pragma Assert (not Taken and Starts = 0 and Rejects = 1 and Finishes = 0);
         pragma Assert (Saves = (if Fault = 10 then 0 else 1));
         pragma Assert (Update_Pending = 0 and Update_Index = 0 and Update_Table_Pages = 0);
      elsif Fault = 12 then
         pragma Assert (Taken and Starts = 1 and Saves = 1 and Finishes = 1 and Update_Pending = 0);
      else
         pragma Assert (not Taken and Starts = 0 and Saves = 0 and Rejects = 0 and Finishes = 0);
      end if;
   end Run;
begin
   for Case_ID in 0 .. 19 loop
      Fault := Case_ID; Starts := 0; Saves := 0; Rejects := 0; Finishes := 0;
      Service := new Application_Buffers.Service; Private_Contexts (1) := new Item;
      Update_Pending := 0; Private_Pending := 0; Application_Pending := 0; Buffer_Retirement_Pending := 0;
      In_Place_Active := False; Table_Ledger_Busy := False; Offline_Bind_State := No_Offline_Bind;
      Run;
   end loop;
   Ada.Text_IO.Put_Line ("Offline admission PASS20: real preflight/topology/tickets, exact sizing, serialization and failed handoff");
end Offline_Admission;
'''
out = Path(tempfile.mkdtemp(prefix="cubit-offline-admission."))
(out / "test.gpr").write_text(f'''project Test is
for Source_Dirs use (".", "{root / 'userspace/services/intel-gpu'}");
for Object_Dir use "obj";
for Main use ("offline_admission.adb");
package Compiler is
for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato");
end Compiler;
end Test;
''')
variants = {"native": body,
    "wrong-page-count": body.replace("Update_Table_Pages := Needed.Additional_Backing;", "Update_Table_Pages := 8;"),
    "omit-save-failure": body.replace("saveReplyCap (Unsigned_64 (Application_Reply_Slot)) /= 1", "saveReplyCap (Unsigned_64 (Application_Reply_Slot)) = 99"),
    "wrong-role": body.replace("Kind => Application_Buffers.Incremental_Tables", "Kind => Application_Buffers.Replacement_Tables")}
for name, variant in variants.items():
    assert name == "native" or variant != body
    (out / "offline_admission.adb").write_text(prefix + variant + suffix)
    build = subprocess.run(["gprbuild", "-f", "-p", "-P", str(out / "test.gpr")], capture_output=True, text=True)
    (out / f"{name}-build.log").write_text(build.stdout + build.stderr)
    assert build.returncode == 0, (name, build.stderr, out)
    run = subprocess.run([str(out / "obj/offline_admission")], capture_output=True, text=True)
    (out / f"{name}-run.log").write_text(run.stdout + run.stderr)
    assert (run.returncode == 0) == (name == "native"), (name, run.stdout, run.stderr, out)
    print(name, run.returncode, run.stdout.strip())
assert source.read_bytes() == original
(out / "evidence.json").write_text(json.dumps({"source_sha256": hashlib.sha256(original).hexdigest(), "variants": list(variants)}, indent=2))
print("Evidence:", out)
