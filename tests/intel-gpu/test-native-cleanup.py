#!/usr/bin/env python3
"""Compile exact native coordinator blocks with real registries and mock grants.

Run inside nix develop. This is hosted lifecycle coverage, not GPU evidence.
Generated sources/build outputs are retained in a fresh workspace directory.
"""
from pathlib import Path
import re
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
driver = root / "userspace/services/intel-gpu"
fixture = root / "tests/mesa-anv/memory-fixture"
main = (driver / "main.adb").read_text()

def section(start, end):
    first = main.index(start)
    return main[first:main.index(end, first)]

state = section("   type Cleanup_Phase is", "   Private_Pending :")
actions = section("   procedure Retire_Application_Resources (Session : Unsigned_64) is",
                  "   procedure Handle_Close_Own")
deferred = section("   function Deferred_Application_Work return Boolean is",
                   "   function Application_Work_Drained (Session : Unsigned_64) return Boolean is")
# Integration guards: a swept flag cannot replace existing physical evidence.
for name in ("Image_Retirement_Owner", "Context_Group_Exclusion",
             "Recycle_Context_Ledger", "Finish_Context_Retirement",
             "Finish_Closed_Table_Retirement", "Teardown_Buffer_Owner"):
    body = re.search(r"   (?:function|procedure) " + name + r"\b.*?   end " + name + ";", main, re.S)
    assert body and "Cleanup_Swept (" in body[0], name
query = section("   procedure Handle_Retirement_Query", "   function Session_Healthy")
assert "Facts.Work_Pending := not Cleanup_Swept (Session)" in query
assert "Deferred_Application_Work or else Application_Buffers.Pending_For" in query
drain = section("   function Application_Work_Drained (Session : Unsigned_64) return Boolean is",
                "   procedure Handle_Application_Submission")
assert "Runtime_Fault or else Deferred_Application_Work" in drain
assert "Advance_Application_Cleanup;\n         Application_Maps.Poll" in main
idle = section("         if not Metadata_Busy and then not Update_Image_Pending", "   end loop;\nend Main;")
assert "and then not Cleanup_Work_Ready" in idle
assert "Wait_For_Activity_Until" in idle

source = '''with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages;
with CuBit.Memory_Grants;
with Intel_GPU_Render_Control;
with Intel_GPU_Render_Sessions;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Requests.Sharing;
with Intel_GPU_Buffer_Reply;
procedure Native_Cleanup_Test is
   package Control renames Intel_GPU_Render_Control;
   package G renames CuBit.Memory_Grants;
   Render_Admission : Control.Controller;
   Runtime_Fault : Boolean := False;
   function Owner return Boolean is (not Runtime_Fault);
   function Resolve (Sender, Stamp : Unsigned_64) return Unsigned_64 is
     (Control.Resolve (Render_Admission, Sender, Stamp));
   procedure Recipient (Sender, Stamp : Unsigned_64;
     Slot : out CuBit.Messages.CapabilitySlot; Identity : out Unsigned_64) is
   begin
      Slot := 7;
      Identity := Control.Recipient_Identity (Render_Admission, Sender, Stamp);
   end Recipient;
   package Application_Buffers is new Intel_GPU_Buffer_Requests (Resolve, Owner);
   package Application_Maps is new Application_Buffers.Sharing (Recipient);
   use type Application_Maps.Retirement_State;
   Application_Buffer_State : Application_Buffers.Service;
   Application_Map_State : Application_Maps.Mapping_Table;
   package Application_Lifetime is
      type Phase is (Active, Retired);
   end Application_Lifetime;
   type Context is record
      Life : Application_Lifetime.Phase := Application_Lifetime.Active;
   end record;
   Private_Contexts : array (1 .. Intel_GPU_Render_Sessions.Capacity) of Context;
   Context_Closes : Natural := 0;
   procedure Retire_Application_Context (Session : Unsigned_64) is
   begin
      pragma Assert (Session /= 0);
      Context_Closes := Context_Closes + 1;
   end Retire_Application_Context;
''' + state + actions + '''
   Buffer_Retirement_Pending, Selected_Index, Preparing_Index,
     Application_Pending, Private_Pending, Update_Pending : Unsigned_64 := 0;
   In_Place_Active, Table_Ledger_Busy : Boolean := False;
''' + deferred + '''
   Reply : Application_Buffers.Words;
   Response : Control.Words;
   Sessions : array (1 .. 4) of Unsigned_64;
   Tickets : array (1 .. 65) of Application_Buffers.Ticket;
   Pending, Other_Ticket : Application_Buffers.Ticket;
   Mapping : Application_Maps.Mapping_ID;
   Reference, Handle_ID : Unsigned_64;
   OK, Consumed : Boolean;
   type Storage is array (0 .. 8191) of Unsigned_64;
   Ticket_Memory, Handle_Memory : Storage := [others => 0] with Alignment => 4096;
   Base : constant Unsigned_64 := Intel_GPU_Buffer_Reply.Layout.CPU_Base;
   procedure Round is
   begin
      for I in Cleanups'Range loop Advance_Application_Cleanup; end loop;
   end Round;
   procedure Close (S : Unsigned_64) is
   begin
      Control.Close_Own (Render_Admission, 42, S, Control.Close_Own_Label,
        4, 0, 0, [Control.Version, 0, 0, 0], Response);
      pragma Assert (Response (0) = Control.OK);
      Retire_Application_Resources (S);
   end Close;
begin
   -- Exact shared predicate: every independent publisher, plus all combinations.
   for Mask in Unsigned_64 range 0 .. 255 loop
      Buffer_Retirement_Pending := Mask and 1;
      Selected_Index := Mask and 2;
      Preparing_Index := Mask and 4;
      Application_Pending := Mask and 8;
      Private_Pending := Mask and 16;
      Update_Pending := Mask and 32;
      In_Place_Active := (Mask and 64) /= 0;
      Table_Ledger_Busy := (Mask and 128) /= 0;
      pragma Assert (Deferred_Application_Work = (Mask /= 0));
   end loop;
   Buffer_Retirement_Pending := 0; Selected_Index := 0; Preparing_Index := 0;
   Application_Pending := 0; Private_Pending := 0; Update_Pending := 0;
   In_Place_Active := False; Table_Ledger_Busy := False;
   Ada.Text_IO.Put_Line ("PASS exact deferred publisher predicate: 256 combinations");
   Control.Bind (Render_Admission, 1, 9);
   for I in Sessions'Range loop
      Control.Handle (Render_Admission, 1, 9, True, Control.Label, 4, 0, 0,
        [1, 7 * 2 ** 32 + 42, 0, Control.Reserve], Response);
      pragma Assert (Response (0) = Control.OK);
      Sessions (I) := Response (2);
      Control.Handle (Render_Admission, 1, 9, True, Control.Label, 4, 0, 0,
        [1, 7 * 2 ** 32 + 42, Sessions (I), Control.Activate], Response, True);
      pragma Assert (Response (0) = Control.OK);
   end loop;
   Application_Buffers.Configure_Client_Budgets (Application_Buffer_State, 512 * 1024, OK);
   pragma Assert (OK);
   Application_Buffers.Extend_Tickets (Application_Buffer_State,
     Unsigned_64 (To_Integer (Ticket_Memory'Address)), 65536, OK); pragma Assert (OK);
   Application_Buffers.Extend_Handles (Application_Buffer_State,
     Unsigned_64 (To_Integer (Handle_Memory'Address)), 65536, OK); pragma Assert (OK);
   Application_Buffers.Admit_Slots (Application_Buffer_State, 128, (others => 128), OK);
   pragma Assert (OK);
   for I in Tickets'Range loop
      Application_Buffers.Handle (Application_Buffer_State, 42, Sessions (1),
        Application_Buffers.Label, 4, 0, 0, [1, 0, 4096, 0], Reply, Tickets (I));
      declare Offset : constant Unsigned_64 := Unsigned_64 (I) * 4096; begin
         Application_Buffers.Complete (Application_Buffer_State, Tickets (I),
           Intel_GPU_Buffer_Reply.From_Linear (16#10000000# + Offset, Base + Offset,
             4096, 16#10000000#), Reply, Consumed);
      end;
      pragma Assert (Consumed and Reply (0) = Application_Buffers.OK);
   end loop;
   Handle_ID := Reply (2);
   G.Expected_Slot := 7; G.Expected_Offset := Base + 65 * 4096;
   G.Expected_Bytes := 4096; G.Expected_Access := G.Read_Access;
   G.Expected_Reference := (slot => 8, generation => 9); G.Gone := False;
   for I in 1 .. 49 loop
      Application_Maps.Map (Application_Buffer_State, Application_Map_State,
        42, Sessions (1), Handle_ID, 0, 4096, False, Mapping, Reference);
      pragma Assert (Mapping /= 0);
   end loop;
   Application_Buffers.Handle (Application_Buffer_State, 42, Sessions (2),
     Application_Buffers.Label, 4, 0, 0, [1, 0, 4096, 0], Reply, Other_Ticket);
   Application_Buffers.Complete (Application_Buffer_State, Other_Ticket,
     Intel_GPU_Buffer_Reply.From_Linear (16#10000000# + 66 * 4096,
       Base + 66 * 4096, 4096, 16#10000000#), Reply, Consumed);
   pragma Assert (Consumed and Reply (0) = Application_Buffers.OK);
   Application_Buffers.Handle (Application_Buffer_State, 42, Sessions (1),
     Application_Buffers.Label, 4, 0, 0, [1, 0, 4096, 0], Reply, Pending);
   pragma Assert (Pending /= 0);
   Close (Sessions (1));
   Retire_Application_Resources (Sessions (1));
   pragma Assert (Context_Closes = 1 and Cleanup_Pending (Sessions (1)));
   pragma Assert (Pending_Cleanups = 1 and Cleanup_Work_Ready);
   Application_Buffers.Complete (Application_Buffer_State, Pending,
     Intel_GPU_Buffer_Reply.From_Linear (16#30000000#, Base, 4096, 16#30000000#), Reply, Consumed);
   pragma Assert (Consumed and Reply (0) = Application_Buffers.Denied);
   for Turn in 1 .. 3 loop
      pragma Assert (not Cleanup_Swept (Sessions (1)));
      Round;
      Retire_Application_Resources (Sessions (1));
      for I in 1 .. 64 loop
         pragma Assert (Application_Buffers.Can_Retire
           (Application_Buffer_State, Sessions (1), Tickets (I)) = (I <= Turn * 32));
      end loop;
      pragma Assert (G.Revokes = 0 and Context_Closes = 1);
      pragma Assert (Pending_Cleanups = 1 and Cleanup_Work_Ready);
   end loop;
   Close (Sessions (2)); -- another pending session must also advance
   pragma Assert (Pending_Cleanups = 2 and Cleanup_Work_Ready);
   for Turn in 1 .. 3 loop Round; end loop;
   pragma Assert (Cleanups (1).Phase = Closing_Maps and G.Revokes = 0);
   for Turn in 1 .. 4 loop
      for I in Cleanups'Range loop
         declare Before : constant Natural := G.Revokes; begin
            Advance_Application_Cleanup;
            pragma Assert (G.Revokes - Before <= Application_Maps.Poll_Budget);
         end;
      end loop;
      pragma Assert (G.Revokes = Natural'Min (Turn * 16, 49));
      pragma Assert (Cleanup_Swept (Sessions (1)) = (Turn = 4));
      pragma Assert (Application_Maps.Observe_Retirement
        (Application_Map_State, Sessions (1)) = Application_Maps.Outstanding);
   end loop;
   pragma Assert (not Cleanup_Swept (Sessions (2)));
   pragma Assert (Pending_Cleanups = 1 and Cleanup_Work_Ready);
   for I in 1 .. 3 loop Round; end loop;
   pragma Assert (Cleanup_Swept (Sessions (2)) and Context_Closes = 2);
   pragma Assert (Pending_Cleanups = 0 and not Cleanup_Work_Ready);
   pragma Assert (not Application_Buffers.Can_Retire
     (Application_Buffer_State, Sessions (1), Tickets (65)));
   G.Gone := True;
   for I in 1 .. 4 loop
      Application_Maps.Poll (Application_Buffer_State, Application_Map_State);
   end loop;
   pragma Assert (Application_Maps.Observe_Retirement
     (Application_Map_State, Sessions (1)) = Application_Maps.Clear);
   pragma Assert (Application_Buffers.Client_Usage
     (Application_Buffer_State, Sessions (1)).Charged = 66 * 4096);
   Retire_Application_Resources (Sessions (1)); Round;
   pragma Assert (Context_Closes = 2 and G.Revokes = 49 and not Runtime_Fault);
   pragma Assert (Pending_Cleanups = 0 and not Cleanup_Work_Ready);
   -- A fault with queued work must not create an idle-loop busy spin.
   Close (Sessions (3));
   pragma Assert (Pending_Cleanups = 1 and Cleanup_Work_Ready);
   -- A known slot is not evidence that its admission was closed. Deliberately
   -- violate the caller precondition: no cursor/context transition is allowed.
   Retire_Application_Resources (Sessions (4));
   pragma Assert (Runtime_Fault and Context_Closes = 3);
   pragma Assert (Cleanups (4).Phase = Not_Started and Pending_Cleanups = 1);
   pragma Assert (Control.Resolve (Render_Admission, 42, Sessions (4)) = 0);
   Retire_Application_Resources (1); pragma Assert (Runtime_Fault);
   pragma Assert (not Cleanup_Work_Ready);
   Ada.Text_IO.Put_Line ("Native cleanup coordinator hosted PASS: exact extracted code, bounded sweeps, duplicates, two sessions, late completion, held grants, retained charges, unknown identity");
end Native_Cleanup_Test;
'''
work = Path(tempfile.mkdtemp(prefix="native-cleanup-", dir=root / ".vm-intel-o4CEuh"))
(work / "native_cleanup_test.adb").write_text(source)
files = re.search(r'for Source_Files use \((.*?)\);', (fixture / "sharing.gpr").read_text(), re.S)[1]
files = files.replace('"sharing_tests.adb"', '"native_cleanup_test.adb"')
files += ', "intel_gpu_render_control.ads", "intel_gpu_render_control.adb", "intel_gpu_render_sessions.ads", "intel_gpu_render_sessions.adb"'
(work / "cleanup.gpr").write_text(f'''project Cleanup is
   for Source_Dirs use (".", "{fixture}", "{driver}", "{root / 'userspace/runtime/gnat'}");
   for Source_Files use ({files});
   for Object_Dir use "obj";
   for Main use ("native_cleanup_test.adb");
   package Naming is
      for Spec ("CuBit.Messages") use "messages_mock.ads";
      for Body ("CuBit.Messages") use "messages_mock.adb";
      for Spec ("CuBit.Memory_Grants") use "memory_mock.ads";
      for Body ("CuBit.Memory_Grants") use "memory_mock.adb";
   end Naming;
   package Compiler is
      for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato", "-O2");
   end Compiler;
end Cleanup;
''')
print(work, flush=True)
subprocess.run(["gprbuild", "-p", "-P", str(work / "cleanup.gpr")], check=True)
subprocess.run([str(work / "obj/native_cleanup_test")], check=True)
