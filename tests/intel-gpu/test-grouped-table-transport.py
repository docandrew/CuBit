#!/usr/bin/env python3
"""Execute native grouped retirement callbacks with modeled supervisor receipts.

Registry code and extracted callbacks are real; GPU exclusion, allocator and
request-slot transitions are fixtures, not evidence of hardware retirement.
"""
from pathlib import Path
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = (root / "userspace/services/intel-gpu/main.adb").read_text()
body = source.split("   -- BEGIN GROUPED TABLE TRANSPORT\n", 1)[1].split(
    "   -- END GROUPED TABLE TRANSPORT", 1)[0]
receipt_start = source.index("   function Table_Allocation_Retired (Session, Ticket : Unsigned_64) return Boolean is")
receipt_body = source[receipt_start:source.index("   procedure Select_Table_Slice", receipt_start)]
prefix = '''with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Table_Allocations;
with Intel_GPU_Table_Provenance;
with Intel_GPU_Table_Provenance.Retirement;
procedure Transport_Test is
begin
 for Fault in 0 .. 11 loop
  declare
   Anchor : constant Unsigned_64 := 257;
   Child : constant Unsigned_64 := 258;
   Recycle_Ticket : constant Unsigned_64 := Anchor;
   Recycle_Slot : constant Positive := 1;
   Table_Release_Ticket : Unsigned_64 := 0;
   Buffer_Retirement_Is_Closed_Table : Boolean := Fault = 1;
   Buffer_Retirement_Is_Context : Boolean := Fault >= 10;
   Buffer_Retirement_Is_Private : Boolean := Fault /= 1 and Fault < 10;
   Buffer_Retirement_Pending : Unsigned_64 := Anchor;
   Buffer_Retirement_Session : Unsigned_64 := 42;
   Runtime_Fault : Boolean := False;
   Context_Owner : Boolean := True;
   Application_Buffer_State, Buffer_Pool : Natural := 0;
   Held : Boolean := True;
   Child_Reusable : Boolean := False;
   In_Flight : Boolean := False;
   Receipt_Slot : Positive := 1;
   Receipt_Generation : Unsigned_64 := 0;
   Submitted_Slot : Positive := 1;
   Submitted_Generation : Unsigned_64 := 0;
   Calls : Natural := 0;
   type Checkpoint is (Retirement_Submitted, Retirement_Rejected);
   Last_Table_Retirement : Checkpoint := Retirement_Rejected;
   package Application_Buffers is
    type Private_Table_Kind is (Replacement_Tables, Incremental_Tables);
    function Ticket_Slot (ID : Unsigned_64) return Positive is
      (Positive (ID mod 256));
    function Ticket_Generation (ID : Unsigned_64) return Unsigned_64 is (ID / 256 + 1);
    function Is_Table_Allocation (Ignored : Natural; Owner, ID : Unsigned_64;
      Kind : Private_Table_Kind) return Boolean is
      (Owner = 42 and ID = Child and Kind = Incremental_Tables and
       not Child_Reusable and Fault /= 3);
    procedure Acknowledge_Private_Retirement (Ignored : in out Natural;
      Owner, ID : Unsigned_64; Swept : Boolean; Accepted : out Boolean);
   end Application_Buffers;
   package body Application_Buffers is
    procedure Acknowledge_Private_Retirement (Ignored : in out Natural;
      Owner, ID : Unsigned_64; Swept : Boolean; Accepted : out Boolean) is
    begin
      pragma Assert (Owner = 42 and ID = Child and Swept);
      Accepted := Fault /= 7;
      if Accepted then Child_Reusable := True; end if;
    end;
   end Application_Buffers;
   package Closed_Table_Tickets is
    function Can_Retire (Ignored : Natural; Owner, ID : Unsigned_64) return Boolean is
      (Owner = 42 and ID = Child and not Child_Reusable);
    procedure Acknowledge (Ignored : in out Natural; Owner, ID : Unsigned_64;
      Swept : Boolean; Accepted : out Boolean);
   end Closed_Table_Tickets;
   package body Closed_Table_Tickets is
    procedure Acknowledge (Ignored : in out Natural; Owner, ID : Unsigned_64;
      Swept : Boolean; Accepted : out Boolean) is
    begin
      pragma Assert (Buffer_Retirement_Is_Closed_Table or Buffer_Retirement_Is_Context);
      Application_Buffers.Acknowledge_Private_Retirement (Ignored, Owner, ID, Swept, Accepted);
    end;
   end Closed_Table_Tickets;
   package Context_Tickets is
    function Can_Retire (Ignored : Natural; Owner, ID : Unsigned_64) return Boolean is
      (Owner = 42 and ID = Anchor and Fault /= 11);
   end Context_Tickets;
   package Buffer_Memory is
    function Pending (Ignored : Natural) return Boolean is (In_Flight);
    function Retirement_Confirmed (Ignored : Natural; Slot : Positive;
      Generation : Unsigned_64) return Boolean is
      (not In_Flight and Slot = Receipt_Slot and Generation = Receipt_Generation);
    procedure Retire (Ignored : in out Natural; Slot : Positive;
      Generation : Unsigned_64; Retired : Boolean; Accepted : out Boolean);
   end Buffer_Memory;
   package body Buffer_Memory is
    procedure Retire (Ignored : in out Natural; Slot : Positive;
      Generation : Unsigned_64; Retired : Boolean; Accepted : out Boolean) is
    begin
      pragma Assert (Retired and not In_Flight);
      Calls := Calls + 1; Submitted_Slot := Slot; Submitted_Generation := Generation;
      Accepted := Fault /= 6;
      In_Flight := Accepted;
    end;
   end Buffer_Memory;
   function Admitted (Owner, ID : Unsigned_64; Slot : Positive) return Boolean is
     (Owner = 42 and ID in Anchor .. Child and
      Slot = Application_Buffers.Ticket_Slot (ID));
@RECEIPT_BODY@
   package Table_Allocations is new Intel_GPU_Table_Allocations (Admitted, Table_Allocation_Retired);
   Table_Backing_Registry : Table_Allocations.Registry;
   function Recycle_Exclusion_Ready (Owner : Unsigned_64) return Boolean is
     (Held and Owner = 42);
'''
prefix = prefix.replace("@RECEIPT_BODY@", receipt_body)
suffix = '''
   Backing : constant Intel_GPU_Buffer_Reply.Backing :=
     Intel_GPU_Buffer_Reply.From_Linear (16#200000#,
       Intel_GPU_Buffer_Reply.Layout.CPU_Base, 4096, 16#200000#);
   OK, Complete, Failed : Boolean;
   Before : Natural;
  begin
   if not Buffer_Retirement_Is_Context then
     Table_Allocations.Install (Table_Backing_Registry, 1, 42, Anchor,
       Table_Allocations.Replacement_Image, Backing, OK); pragma Assert (OK);
   end if;
   Table_Allocations.Install (Table_Backing_Registry, 2, 42, Child,
     (if Fault = 2 then Table_Allocations.Replacement_Image else Table_Allocations.Incremental_Tables),
     Backing, OK); pragma Assert (OK);
   if Fault = 9 then
     Table_Allocations.Revoke (Table_Backing_Registry, 2, 42, Child, OK);
     pragma Assert (OK);
   end if;
   Submit_Table_Retirement (42, 0, OK); pragma Assert (not OK and Calls = 0);
   Submit_Table_Retirement (43, Child, OK); pragma Assert (not OK and Calls = 0);
   Submit_Table_Retirement (42, 514, OK); pragma Assert (not OK and Calls = 0);
   Submit_Table_Retirement (42, Child, OK);
   if Fault in 2 | 3 then
     pragma Assert (not OK and Calls = 0 and Table_Release_Ticket = 0);
   elsif Fault = 6 then
     pragma Assert (not OK and Calls = 1 and Table_Release_Ticket = Child);
     Submit_Table_Retirement (42, Child, OK); pragma Assert (not OK and Calls = 1);
   else
     pragma Assert (OK and Calls = 1 and Table_Release_Ticket = Child);
     pragma Assert (Submitted_Slot = 2 and Submitted_Generation = 2);
     Poll_Table_Receipt (42, Child, Complete, Failed);
     pragma Assert (not Complete and not Failed and not Child_Reusable);
     Submit_Table_Retirement (42, Anchor, OK); pragma Assert (not OK and Calls = 1);
     Receipt_Slot := (if Fault = 4 then 1 else 2);
     Receipt_Generation := (if Fault = 5 then 1 else 2);
     In_Flight := False;
     if Fault = 8 then Held := False; end if;
     Poll_Table_Receipt (42, Child, Complete, Failed);
     if Fault in 4 | 5 | 8 then
       pragma Assert (not Complete and Failed);
       Finalize_Table_Receipt (42, Child, OK); pragma Assert (not OK and not Child_Reusable);
       pragma Assert (Table_Allocations.Retained_At (Table_Backing_Registry, 2).Present);
     else
       pragma Assert (Complete and not Failed);
       Finalize_Table_Receipt (42, Child, OK);
       if Fault = 7 then
         pragma Assert (not OK and not Child_Reusable and Table_Release_Ticket = Child);
         Submit_Table_Retirement (42, Anchor, OK); pragma Assert (not OK and Calls = 1);
       else
         pragma Assert (OK and Child_Reusable and Table_Release_Ticket = 0);
         pragma Assert (not Table_Allocations.Retained_At (Table_Backing_Registry, 2).Present);
         Finalize_Table_Receipt (42, Child, OK); pragma Assert (not OK);
         Submit_Table_Retirement (42, Anchor, OK);
         if Fault = 11 then
           pragma Assert (not OK and Calls = 1 and Table_Release_Ticket = 0);
         else
         pragma Assert (OK and Calls = 2);
         pragma Assert (Submitted_Slot = 1 and Submitted_Generation = 2);
         Receipt_Slot := 1; Receipt_Generation := 2; In_Flight := False;
         Poll_Table_Receipt (42, Anchor, Complete, Failed); pragma Assert (Complete and not Failed);
         Finalize_Table_Receipt (42, Anchor, OK); pragma Assert (OK and Table_Release_Ticket = 0);
         pragma Assert (not Table_Allocations.Retained_At (Table_Backing_Registry, 1).Present);
         end if;
       end if;
     end if;
   end if;
  end;
 end loop;
 Ada.Text_IO.Put_Line ("Native grouped transport PASS12: context parent without registry entry, parent identity rejection, child/anchor slots and receipt failures");
end Transport_Test;
'''
work = Path(tempfile.mkdtemp(prefix="cubit-grouped-table-transport."))
(work / "test.gpr").write_text(f'''project Test is
 for Source_Dirs use (".", "{root}/userspace/services/intel-gpu");
 for Main use ("transport_test.adb");
 package Compiler is
  for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato");
 end Compiler;
end Test;
''')
variants = {"actual": body,
            "ignore_context_identity": body.replace(
                "return Context_Tickets.Can_Retire (Application_Buffer_State, Owner, ID);",
                "return True;"),
            "anchor_slot": body.replace("Buffer_Memory.Retire (Buffer_Pool, Application_Buffers.Ticket_Slot (ID)",
                                        "Buffer_Memory.Retire (Buffer_Pool, Recycle_Slot"),
            "ignore_receipt_generation": body.replace("Application_Buffers.Ticket_Generation (ID)))",
                                                     "Receipt_Generation))")}
for name, block in variants.items():
    if name != "actual":
        assert block != body
    (work / "transport_test.adb").write_text(prefix + block + suffix)
    subprocess.run(["alr", "exec", "--", "gprbuild", "-f", "-q", "-p", "-P", str(work / "test.gpr")],
                   cwd=root / "kernel", check=True)
    result = subprocess.run([str(work / "transport_test")], text=True, capture_output=True)
    (work / f"{name}.log").write_text(result.stdout + result.stderr)
    if name == "actual":
        assert result.returncode == 0, result.stdout + result.stderr
        print(result.stdout.strip())
    else:
        assert result.returncode != 0 and "ASSERTION_ERROR" in result.stderr, result.stdout + result.stderr
        print(f"{name}: negative control rejected")
print(f"Hosted adapter evidence (NOT hardware): {work}")
