#!/usr/bin/env python3
"""Compile the native parent completion with fault-injected boundary adapters."""
from pathlib import Path
import hashlib
import json
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = root / "userspace/services/intel-gpu/main.adb"
original = source.read_bytes()
text = original.decode()
start = text.index("   procedure Finish_Context_Retirement is")
end = text.index("   end Finish_Context_Retirement;", start) + len("   end Finish_Context_Retirement;")
body = text[start:end]
helper_start = text.index("   procedure Recycle_Context_Ledger (")
helper_end = text.index("   end Recycle_Context_Ledger;", helper_start) + len("   end Recycle_Context_Ledger;")
helper = text[helper_start:helper_end].replace("Recycle_Context_Ledger", "Native_Recycle_Context_Ledger")
gate_start = text.index("   function Context_Group_Exclusion (Owner : Unsigned_64) return Boolean is")
gate_end = text.index("   end Context_Group_Exclusion;", gate_start) + len("   end Context_Group_Exclusion;")
group_gate = text[gate_start:gate_end]
begin_start = text.index("   procedure Begin_Context_Recycling (")
begin_end = text.index("   end Begin_Context_Recycling;", begin_start) + len("   end Begin_Context_Recycling;")
begin_group = text[begin_start:begin_end]
prefix = '''with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Table_Provenance;
with Intel_GPU_Table_Provenance.Retirement;
with Intel_GPU_Table_Provenance.Retirement.Dispatcher;
with Intel_GPU_Table_References;
procedure Completion_Test is
   package P renames Intel_GPU_Table_Provenance;
   package Application_State is
      package Table_References is new Intel_GPU_Table_References (64);
   end;
   package R renames Application_State.Table_References;
   Fault, Steps, Cancels, Quarantines, Case_Number : Natural := 0;
   Retired_Source : Boolean := False;
   Ledger_Recycled : Boolean := False;
   Mixed_Group, Child_Finalized : Boolean := False;
   Submissions : Natural := 0;
   Runtime_Fault : Boolean := False;
   function Context_Owner return Boolean is (Fault /= 3);
   package Application_Lifetime is
      type Phase is (Published, Retired);
   end Application_Lifetime;
   use type Application_Lifetime.Phase;
   type Backing is record Ready : Boolean := True; end record;
   type Item is limited record
      Life : Application_Lifetime.Phase := Application_Lifetime.Retired;
      Parent_Ticket : Unsigned_64 := 55;
      Parent, Context, Tables, Scratch : Backing;
      Source : Natural := 1;
      Table_Owners : P.Ledger;
      Table_Generation : Unsigned_64 := 1;
      Table_IDs : R.Map;
   end record;
   type Item_Access is access Item;
   Private_Contexts : array (1 .. 2) of Item_Access := [others => new Item];
   procedure Resolve (Session, Ticket, Offset : Unsigned_64;
     CPU, DMA : out Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := Session = 101 and (Ticket = 55 or (Mixed_Group and Ticket = 56));
      CPU := 16#100000# + Offset + (if Ticket = 56 then 16#100000# else 0);
      DMA := 16#200000# + Offset + (if Ticket = 56 then 16#100000# else 0);
   end;
   package Authority is new P.Authority (Resolve);
   Installed : Boolean;
   type Bytes is array (1 .. 4096) of Unsigned_8;
   Metadata : Bytes := [others => 0] with Alignment => 4096;
   Reference_Metadata : Bytes := [others => 0] with Alignment => 4096;
   package Intel_GPU_Render_Sessions is
      Tag_Base : constant Unsigned_64 := 100;
   end Intel_GPU_Render_Sessions;
   Render_Admission : Boolean := True;
   package Intel_GPU_Render_Control is
      function Issued_Tag (Object : Boolean; Index : Natural) return Unsigned_64 is
        (if not Object or Index not in 1 .. 2 or Fault in 16 .. 17 then 0
         else 100 + Unsigned_64 (Index));
   end Intel_GPU_Render_Control;
   Contexts, Application_Map_State, Application_Buffer_State, Buffer_Pool : Boolean := True;
   Buffer_Retirement_Pending : Unsigned_64 := 55;
   Buffer_Retirement_Session : Unsigned_64 := 101;
   Buffer_Retirement_Is_Context : Boolean := True;
   Context_Retirement_Index : Natural := 1;
   Context_Retirement_Revision : Unsigned_64 := 4;
   type Mapping is record CPU, DMA : Unsigned_64; end record;
   Context_Retirement_Root : Mapping := (8192, 12288);
   Image_Retirement_Addresses : array (1 .. 2) of Unsigned_64 := [others => 4096];
   Application_Images_State : array (1 .. 2) of Natural := [others => 1];
   package Context_Drain is
      type Retirement_State is (Deregistered, Uncertain);
      function Observe (Object : Boolean; Session : Unsigned_64) return Retirement_State is
        (if Fault = 7 then Uncertain else Deregistered);
   end Context_Drain;
   package Application_Maps is
      type Retirement_State is (Clear, Uncertain);
      function Observe_Retirement (Object : Boolean; Session : Unsigned_64) return Retirement_State is
        (if Fault = 8 then Uncertain else Clear);
   end Application_Maps;
   package Context_Tickets is
      function Can_Retire (Object : Boolean; Session, Ticket : Unsigned_64) return Boolean is (Fault /= 9);
      procedure Acknowledge (Object : in out Boolean; Session, Ticket : Unsigned_64;
        Evidence : Boolean; Accepted : out Boolean);
   end Context_Tickets;
   package body Context_Tickets is
      procedure Acknowledge (Object : in out Boolean; Session, Ticket : Unsigned_64;
        Evidence : Boolean; Accepted : out Boolean) is
      begin
         pragma Assert (Steps = 3 and Ledger_Recycled and Evidence and Session = 101 and Ticket = 55);
         Steps := 4; Accepted := Fault /= 13;
      end Acknowledge;
   end Context_Tickets;
   package Application_Buffers is
      function Ticket_Slot (Ticket : Unsigned_64) return Natural is (3);
      function Ticket_Generation (Ticket : Unsigned_64) return Unsigned_32 is (7);
      procedure Quarantine (Object : in out Boolean);
   end Application_Buffers;
   package body Application_Buffers is
      procedure Quarantine (Object : in out Boolean) is
      begin Quarantines := Quarantines + 1; end Quarantine;
   end Application_Buffers;
   package Buffer_Memory is
      function Retirement_Confirmed (Object : Boolean; Slot : Natural; Generation : Unsigned_32) return Boolean is
        (Slot = 3 and Generation = 7 and Fault /= 10);
      procedure Cancel (Object : in out Boolean);
   end Buffer_Memory;
   package body Buffer_Memory is
      procedure Cancel (Object : in out Boolean) is
      begin Cancels := Cancels + 1; end Cancel;
   end Buffer_Memory;
   package Live_Snapshots is
      function Retired (Object : Natural) return Boolean is (Retired_Source);
      procedure Forget_Retired (Object : in out Natural; Revision, Root : Unsigned_64;
        Evidence : Boolean; Accepted : out Boolean);
   end Live_Snapshots;
   package body Live_Snapshots is
      procedure Forget_Retired (Object : in out Natural; Revision, Root : Unsigned_64;
        Evidence : Boolean; Accepted : out Boolean) is
      begin
         pragma Assert (Steps = 0 and Revision = 4 and Root = 12288 and Evidence);
         Steps := 1; Accepted := Fault /= 11;
      end Forget_Retired;
   end Live_Snapshots;
   package Image_Retirement is
      procedure Forget_Backing_Receipt (Object : in out Natural; GPU : Unsigned_64;
        Root : Mapping; Evidence : Boolean; Accepted : out Boolean);
   end Image_Retirement;
   package body Image_Retirement is
      procedure Forget_Backing_Receipt (Object : in out Natural; GPU : Unsigned_64;
        Root : Mapping; Evidence : Boolean; Accepted : out Boolean) is
      begin
         pragma Assert (Steps = 1 and GPU = 4096 and Root = Context_Retirement_Root and Evidence);
         Steps := 2; Accepted := Fault /= 12;
      end Forget_Backing_Receipt;
   end Image_Retirement;
   procedure Publish_Snapshot (Value : String) is null;
   Recycle_Context_Index : Natural := 1;
   Recycle_Session, Recycle_Ticket : Unsigned_64 := 0;
   Recycle_Slot : Positive := 3;
   Table_Release_Ticket : Unsigned_64 := 0;
   Group_Fault : Natural := 0;
   function Current_Table_Ticket (Index : Positive) return Unsigned_64 is
     (if Group_Fault = 3 then 55 else 0);
   package Application_VM is
      function Sealed (Object : Natural) return Boolean is (True);
      function Revision (Object : Natural) return Unsigned_64 is (if Group_Fault = 1 then 5 else 4);
      function Root_DMA (Object : Natural) return Unsigned_64 is (if Group_Fault = 2 then 16384 else 12288);
   end Application_VM;
   function Context_Group_Exclusion (Owner : Unsigned_64) return Boolean;
   function Recycle_Exclusion_Ready (Owner : Unsigned_64) return Boolean is
     (Context_Group_Exclusion (Owner));
   function Released (Session : Unsigned_64) return Boolean is (Context_Group_Exclusion (Session));
   function Confirmed (Session, Ticket : Unsigned_64) return Boolean is
     (Session = 101 and (Ticket = 55 or (Mixed_Group and Ticket = 56)));
   package Recycling is new P.Retirement (Released, Confirmed, Confirmed);
   procedure Prepare (Session, Ticket : Unsigned_64; Complete, Failed : out Boolean) is
   begin Complete := Confirmed (Session, Ticket); Failed := not Complete; end;
   procedure Submit (Session, Ticket : Unsigned_64; Accepted : out Boolean) is
   begin
      Submissions := Submissions + 1;
      pragma Assert (Ticket = (if Mixed_Group and Submissions = 1 then 56 else 55));
      if Ticket = 55 and Mixed_Group then pragma Assert (Child_Finalized); end if;
      Accepted := Confirmed (Session, Ticket);
   end;
   procedure Poll (Session, Ticket : Unsigned_64; Complete, Failed : out Boolean) renames Prepare;
   procedure Finalize (Session, Ticket : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := Confirmed (Session, Ticket);
      if Accepted and Ticket = 56 then Child_Finalized := True; end if;
   end;
   package Table_Recycle_Dispatch is new Recycling.Dispatcher (Prepare, Submit, Poll, Finalize);
   -- Failed controllers cannot be reset. Each fault scenario is independent.
   Table_Recycle_Controls : array (0 .. 19) of Table_Recycle_Dispatch.Controller;
   use type Table_Recycle_Dispatch.State;
'''
wrapper = '''
   procedure Recycle_Context_Ledger (Index : Positive; Accepted : out Boolean) is
   begin
      pragma Assert (Steps = 2 and Index = 1);
      Steps := 3;
      -- Drive the real dispatcher to completion using modeled hardware and
      -- allocator receipts; native Reopen must consume that completed group.
      Begin_Context_Recycling (Index, Accepted);
      if Accepted then
         for Turn in 1 .. 64 loop
            exit when Table_Recycle_Dispatch.Status (Table_Recycle_Control) /= Table_Recycle_Dispatch.Running;
            Table_Recycle_Dispatch.Step (Table_Recycle_Control, Private_Contexts (Index).Table_Owners);
         end loop;
         pragma Assert (Table_Recycle_Dispatch.Status (Table_Recycle_Control) = Table_Recycle_Dispatch.Done);
         pragma Assert (Submissions = (if Mixed_Group then 2 else 1));
      end if;
      Native_Recycle_Context_Ledger (Index, Accepted);
      Ledger_Recycled := Accepted;
      if Accepted then
         pragma Assert (P.Count (Private_Contexts (Index).Table_Owners) = 0);
         pragma Assert (P.Generation (Private_Contexts (Index).Table_Owners) = 2);
         pragma Assert (Private_Contexts (Index).Table_Generation = 2);
         pragma Assert (R.Generation (Private_Contexts (Index).Table_IDs) = 2);
         pragma Assert (for all I in 1 .. 64 =>
           R.Get (Private_Contexts (Index).Table_IDs, 1, I) = 0 and
           R.Get (Private_Contexts (Index).Table_IDs, 2, I) = 0);
         pragma Assert (Authority.Lookup (Private_Contexts (Index).Table_Owners, 101, 1, 1).Ticket = 0);
      end if;
   end;
'''
suffix = '''
begin
   for Case_ID in 0 .. 19 loop
      Case_Number := Case_ID;
      Retired_Source := Case_ID = 14;
      Mixed_Group := Case_ID = 15; Child_Finalized := False; Submissions := 0;
      Fault := (if Retired_Source or Mixed_Group then 0 else Case_ID);
      Steps := (if Retired_Source then 1 else 0); Cancels := 0; Quarantines := 0;
      Ledger_Recycled := False;
      Runtime_Fault := Fault = 2;
      Context_Retirement_Index := (if Fault = 1 then 0 else 1);
      Buffer_Retirement_Pending := 55;
      Buffer_Retirement_Session := (if Fault = 16 then 0 elsif Fault = 6 then 102 else 101);
      Buffer_Retirement_Is_Context := True;
      Recycle_Context_Index := 1; Recycle_Session := 101; Recycle_Ticket := 55;
      Private_Contexts := [others => new Item];
      Metadata := [others => 0];
      Reference_Metadata := [others => 0];
      R.Extend (Private_Contexts (1).Table_IDs,
                Unsigned_64 (To_Integer (Reference_Metadata'Address)), 4096, Installed);
      pragma Assert (Installed);
      P.Extend (Private_Contexts (1).Table_Owners,
                Unsigned_64 (To_Integer (Metadata'Address)), 4096, Installed);
      pragma Assert (Installed);
      for I in 1 .. 64 loop
         Authority.Install (Private_Contexts (1).Table_Owners, 101, 1, I, 55,
                            Unsigned_64 (I - 1) * 4096, Installed);
         pragma Assert (Installed);
         R.Put (Private_Contexts (1).Table_IDs, 1, I, I, Installed);
         pragma Assert (Installed);
      end loop;
      if Mixed_Group then
         Authority.Install (Private_Contexts (1).Table_Owners, 101, 1, 65, 56, 0, Installed);
         pragma Assert (Installed);
      end if;
      if Fault = 18 then Private_Contexts (1).Table_Generation := 2; end if;
      if Fault = 19 then
         -- Inject an inconsistent map epoch: ledger can retire, map cannot
         -- accept the captured epoch. This must not acknowledge parent reuse.
         R.Reopen (Private_Contexts (1).Table_IDs, 1, 2, Installed);
         pragma Assert (Installed);
      end if;
      if Fault = 4 then Private_Contexts (1).Life := Application_Lifetime.Published; end if;
      if Fault = 5 then Private_Contexts (1).Parent_Ticket := 56; end if;
      if Case_ID = 0 then
         for Inject in 1 .. 3 loop
            Group_Fault := Inject;
            pragma Assert (not Context_Group_Exclusion (101));
         end loop;
         Group_Fault := 0;
         pragma Assert (Context_Group_Exclusion (101));
      end if;
      Finish_Context_Retirement;
      pragma Assert (Buffer_Retirement_Pending = 0 and not Buffer_Retirement_Is_Context and Context_Retirement_Index = 0);
      pragma Assert (Runtime_Fault = (Fault /= 0));
      pragma Assert (Cancels = (if Fault = 0 then 0 else 1) and Quarantines = Cancels);
      pragma Assert (Steps = (if Fault = 0 or Fault = 13 then 4 elsif Fault in 18 .. 19 then 3 elsif Fault = 12 then 2 elsif Fault = 11 then 1 else 0));
      pragma Assert (Ledger_Recycled = (Fault = 0 or Fault = 13));
      pragma Assert (P.Count (Private_Contexts (1).Table_Owners) =
                     (if Ledger_Recycled or Fault = 19 then 0 else 64));
      if Fault = 19 then
         pragma Assert (P.Generation (Private_Contexts (1).Table_Owners) = 2);
         pragma Assert (Private_Contexts (1).Table_Generation = 1);
         pragma Assert (Recycle_Context_Index = 1); -- no successful recycle acknowledgement
      end if;
      pragma Assert (Private_Contexts (1).Parent.Ready = (Fault /= 0));
      pragma Assert (Private_Contexts (1).Context.Ready = (Fault /= 0));
      pragma Assert (Private_Contexts (1).Tables.Ready = (Fault /= 0));
      pragma Assert (Private_Contexts (1).Scratch.Ready = (Fault /= 0));
      pragma Assert (Private_Contexts (2).Parent.Ready);
   end loop;
   Ada.Text_IO.Put_Line ("Native parent completion PASS20 paths: native group start, child before parent, exact receipt and ledger/map recycle order, failed map reopen, quarantine and identity rejection");
end Completion_Test;
'''
helper, begin_group, wrapper, body = (
    part.replace('Table_Recycle_Control', 'Table_Recycle_Controls (Case_Number)')
    for part in (helper, begin_group, wrapper, body))
work = Path(tempfile.mkdtemp(prefix="context-parent-completion."))
(work / "test.gpr").write_text('''project Test is
 for Source_Dirs use (".", "''' + str(root / 'userspace/services/intel-gpu') + '''");
 for Main use ("completion_test.adb");
 package Compiler is
  for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato", "-O0");
 end Compiler;
end Test;
''')
(work / "completion_test.adb").write_text(prefix + group_gate + begin_group + helper + wrapper + body + suffix)
subprocess.run(["gprbuild", "-f", "-P", "test.gpr"], cwd=work, check=True)
result = subprocess.run([str(work / "completion_test")], capture_output=True, text=True)
assert result.returncode == 0 and "PASS20" in result.stdout, (result.stdout, result.stderr)
negative = body.replace("            Recycle_Context_Ledger (Index, Accepted);",
                        "            null; -- negative: omit provenance retirement")
assert negative != body
(work / "completion_test.adb").write_text(prefix + group_gate + begin_group + helper + wrapper + negative + suffix)
subprocess.run(["gprbuild", "-f", "-P", "test.gpr"], cwd=work, check=True)
rejected = subprocess.run([str(work / "completion_test")], capture_output=True, text=True)
assert rejected.returncode != 0, "missing ledger retirement escaped the fixture"
wrong_order = begin_group.replace("Last_Ticket => Recycle_Ticket", "Last_Ticket => 0")
assert wrong_order != begin_group
(work / "completion_test.adb").write_text(prefix + group_gate + wrong_order + helper + wrapper + body + suffix)
subprocess.run(["gprbuild", "-f", "-P", "test.gpr"], cwd=work, check=True)
order_rejected = subprocess.run([str(work / "completion_test")], capture_output=True, text=True)
assert order_rejected.returncode != 0, "parent-first retirement escaped the fixture"
ignored_map_failure = helper.replace('         if not Accepted then return; end if;',
                                     '         Accepted := True; -- negative: ignore map failure')
assert ignored_map_failure != helper
(work / "completion_test.adb").write_text(prefix + group_gate + begin_group + ignored_map_failure + wrapper + body + suffix)
subprocess.run(["gprbuild", "-f", "-P", "test.gpr"], cwd=work, check=True)
map_rejected = subprocess.run([str(work / "completion_test")], capture_output=True, text=True)
assert map_rejected.returncode != 0, "ignored map retirement failure escaped the fixture"
(work / "completion_test.adb").write_text(prefix + group_gate + begin_group + helper + wrapper + body + suffix)
subprocess.run(["gprbuild", "-f", "-P", "test.gpr"], cwd=work, check=True)
assert source.read_bytes() == original, "native source changed during fixture"
(work / "result.json").write_text(json.dumps({"source_sha256": hashlib.sha256(original).hexdigest(),
    "body_sha256": hashlib.sha256(body.encode()).hexdigest(),
    "helper_sha256": hashlib.sha256(helper.encode()).hexdigest(),
    "group_gate_sha256": hashlib.sha256(group_gate.encode()).hexdigest(),
    "group_start_sha256": hashlib.sha256(begin_group.encode()).hexdigest(),
    "order_negative_stderr": order_rejected.stderr,
    "map_negative_stderr": map_rejected.stderr,
    "negative_control_stderr": rejected.stderr, "stdout": result.stdout}, indent=2) + "\n")
print(result.stdout, "evidence", work)
