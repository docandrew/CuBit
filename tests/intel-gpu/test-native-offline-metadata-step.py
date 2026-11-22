#!/usr/bin/env python3
"""Extract the native metadata-to-backing handoff; mock asynchronous dependencies."""
from pathlib import Path
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = (root / "userspace/services/intel-gpu/main.adb").read_text()
start = source.index("      if Offline_Bind_State = Grow_Offline_Metadata and then Offline_Bind_Owner then")
body = source[start:source.index("      if Offline_Bind_Owner and then Intel_GPU_Buffer_Reply.Valid", start)]
prefix = """
with Interfaces; use Interfaces;
procedure Offline_Metadata_Step is
   Mode, Starts, Steps, Failures : Natural := 0;
   Owned : Boolean := True;
   type Offline_Bind_Phase is (Grow_Offline_Metadata, Allocate_Offline);
   Offline_Bind_State : Offline_Bind_Phase := Grow_Offline_Metadata;
   function Offline_Bind_Owner return Boolean is (Owned);
   Update_Index : constant := 1;
   Update_Table_Pages : constant := 3;
   Update_Pending : constant Unsigned_64 := 99;
   type Context is record Source, Table_Owners : Natural := 0; end record;
   Private_Contexts : array (1 .. 1) of Context;
   type Message_Words is array (0 .. 3) of Unsigned_64;
   type Message is record words : Message_Words := [0, 0, 2 ** 39, 4096]; end record;
   Update_Request : Message;
   Context_Metadata : array (1 .. 1) of Natural := [0];
   package Context_Metadata_Growth is
      type Phase is (Idle, Checking, Failed);
      procedure Step (Object : in out Natural);
      function State (Object : Natural) return Phase is
        (if Mode = 1 then Checking elsif Mode = 2 then Failed else Idle);
   end Context_Metadata_Growth;
   package body Context_Metadata_Growth is
      procedure Step (Object : in out Natural) is
      begin
         Steps := Steps + 1;
         if Mode = 3 then Owned := False; end if;
      end Step;
   end Context_Metadata_Growth;
   package Application_Topology is
      type Plan_Status is (Ready, Invalid_Range);
      type Requirements is record Status : Plan_Status; Fits_Reserved : Boolean; end record;
      type Offline_Requirements is record Topology : Requirements; Additional_Backing : Natural; end record;
      function Inspect_Offline (Source : Natural; GPU, Bytes : Unsigned_64) return Offline_Requirements;
   end Application_Topology;
   package body Application_Topology is
      function Inspect_Offline (Source : Natural; GPU, Bytes : Unsigned_64) return Offline_Requirements is
      begin
         pragma Assert (GPU = 2 ** 39 and Bytes = 4096);
         return ((Status => (if Mode = 8 then Invalid_Range else Ready),
                  Fits_Reserved => Mode /= 4), (if Mode = 5 then 4 else 3));
      end;
   end Application_Topology;
   function Ledger_Capacity return Positive is (if Mode = 6 then 4 else 8);
   package Intel_GPU_Table_Provenance is
      function Count (Object : Natural) return Natural is (4);
   end Intel_GPU_Table_Provenance;
   package Application_Buffers is
      function Ticket_Slot (Ticket : Unsigned_64) return Positive is (1);
   end Application_Buffers;
   Buffer_Pool : Natural := 0;
   package Buffer_Memory is
      procedure Start (Pool : Natural; Slot, Pages : Positive; OK : out Boolean);
   end Buffer_Memory;
   package body Buffer_Memory is
      procedure Start (Pool : Natural; Slot, Pages : Positive; OK : out Boolean) is
      begin
         pragma Assert (Steps = 1 and Owned and Slot = 1 and Pages = 3);
         pragma Assert (Offline_Bind_State = Allocate_Offline);
         Starts := Starts + 1; OK := Mode /= 7;
      end;
   end Buffer_Memory;
   procedure Advance is
      OK : Boolean := False;
   begin
"""
suffix = """
      Failures := Failures + 1;
   end Advance;
begin
   for M in 0 .. 9 loop
      Mode := M; Owned := M /= 9; Starts := 0; Steps := 0; Failures := 0;
      Offline_Bind_State := Grow_Offline_Metadata;
      Advance;
      pragma Assert (Steps = (if M = 9 then 0 else 1));
      pragma Assert (Starts = (if M in 0 | 7 then 1 else 0));
      pragma Assert (Failures = (if M in 0 | 1 then 0 else 1));
      pragma Assert (Offline_Bind_State = (if M = 0 then Allocate_Offline else Grow_Offline_Metadata));
   end loop;
end Offline_Metadata_Step;
"""
variants = {
    "actual": body,
    "omit-post-owner": body.replace("if Offline_Bind_Owner then", "if True then"),
    "omit-metadata-fit": body.replace("Needed.Topology.Fits_Reserved and then", "True and then"),
    "omit-size-recheck": body.replace("Needed.Additional_Backing = Update_Table_Pages and then", "True and then"),
}
out = Path(tempfile.mkdtemp(prefix="cubit-offline-metadata-step."))
for name, code in variants.items():
    assert name == "actual" or code != body
    case = out / name
    case.mkdir()
    (case / "offline_metadata_step.adb").write_text(prefix + code + suffix)
    with (case / "build.log").open("w") as log:
        subprocess.run(["gnatmake", "-q", "-gnat2022", "-gnata", "-gnato", "offline_metadata_step.adb"],
                       cwd=case, stdout=log, stderr=log, check=True)
    with (case / "run.log").open("w") as log:
        run = subprocess.run([str(case / "offline_metadata_step")], stdout=log, stderr=log)
    assert (run.returncode == 0) == (name == "actual"), (name, out)
print("Native offline metadata handoff PASS10 plus three negative controls:", out)
