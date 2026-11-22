#!/usr/bin/env python3
"""Driver-level fixture for intel-gpu session recycling (Linux-hosted).

Extracts the native driver's session-end and recycling procedures from
userspace/services/intel-gpu/main.adb (End_Client_Session,
Handle_Channel_Close, Handle_Grant_Event, Try_Recycle,
Recycle_Retired_Sessions) and runs them against the production session
table and admission controller, with every hardware and resource
observation modelled. Checks that a client's end (queue close or death)
retires its session once; that a slot is recycled only when the GuC
acknowledged deregistration, the image GGTT claim is released and every
release fact holds; that recycling then releases the context ID, the queue,
the metadata reservations and the client account and resets the slot, in
that order; and that a stuck session is reported, not recycled.
Hosted decision coverage of the glue, not Intel hardware evidence.
"""
from pathlib import Path
import os
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
# RECYCLING_SRC: a mutated copy of the driver sources (mutations.sh).
source = Path(os.environ.get("RECYCLING_SRC", root / "userspace/services/intel-gpu"))
main = (source / "main.adb").read_text()


def extract(start, end):
    a = main.index(start)
    b = main.index(end, a) + len(end)
    return main[a:b]


glue = (extract("   procedure End_Client_Session (S : Session_Slot; Why : String) is",
                "   end Handle_Grant_Event;") + "\n" +
        extract("   procedure Try_Recycle (S : Session_Slot) is", "   end Try_Recycle;") + "\n" +
        extract("   procedure Recycle_Retired_Sessions is", "   end Recycle_Retired_Sessions;"))

prefix = r'''
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Intel_GPU_Render_Control;
with Intel_GPU_Render_Sessions;
with Intel_GPU_Client_Budgets;
procedure Recycle_Glue_Tests is
   package RS renames Intel_GPU_Render_Sessions;
   subtype Session_Slot is RS.Slot;
   subtype Session_Count is RS.Slot_Count;
   type Process_ID is new Unsigned_64;
   type Message is record Words : Unsigned_64 := 0; end record;
   function To_Word (P : Process_ID) return Unsigned_64 is (Unsigned_64 (P));
   type Controller_Access is access Intel_GPU_Render_Control.Controller;
   Render_Admission : Intel_GPU_Render_Control.Controller
     renames Controller_Access'(new Intel_GPU_Render_Control.Controller).all;
   Slots_In_Use : constant := 4;
   function Grown_Sessions return Session_Count is
     (Intel_GPU_Render_Control.Grown (Render_Admission));
   Trace : Unbounded_String;
   procedure Note (Event : String) is
   begin
      Append (Trace, Event & ";");
   end Note;
   package CuBit is
      package Log_Records is
         type Severity is (Information, Warning, Error);
      end Log_Records;
      package Control_Events is
         type Event is record Slot : Natural := 0; end record;
      end Control_Events;
      package Channels is
         type Channel is record Active : Boolean := False; Number : Natural := 0; end record;
         function Ended (C : Channel; E : Control_Events.Event) return Boolean is
           (C.Active and then E.Slot = C.Number);
         function Number_Of (M : Message) return Unsigned_64 is (M.Words);
         function Closed_By (C : Channel; From : Process_ID; M : Message) return Boolean is
           (C.Active and then Unsigned_64 (C.Number) = M.Words);
      end Channels;
   end CuBit;
   procedure Publish_Snapshot (Text : String;
     Level : CuBit.Log_Records.Severity := CuBit.Log_Records.Information) is
   begin
      Note ("log:" & CuBit.Log_Records.Severity'Image (Level));
   end Publish_Snapshot;
   Queue_Links : array (Session_Slot range 1 .. Slots_In_Use) of CuBit.Channels.Channel;
   procedure End_Session_Queue (S : Positive) is
   begin
      Note ("end-queue" & Natural'Image (S));
      Queue_Links (S).Active := False;
   end End_Session_Queue;
   Retired_Count : Natural := 0;
   procedure Retire_Application_Resources (Session : Unsigned_64) is
   begin
      Retired_Count := Retired_Count + 1;
      Note ("retire");
   end Retire_Application_Resources;
   -- Modelled observations per slot.
   type Context_State is (Absent, Deregistered, Pending, Quarantined);
   type Slot_Model is record
      Context : Context_State := Absent;
      Image_Released, Grants_Clear, Queue_Idle, Unreferenced : Boolean := True;
      Parent_Ready : Boolean := False;
      Charged : Unsigned_64 := 0;
      Phase_Swept : Boolean := True;
   end record;
   Models : array (Session_Slot range 1 .. Slots_In_Use) of Slot_Model;
   package Context_Drain is
      type Retirement_State is (No_Context, Admission_Open, Pending, Disabled, Deregistered, Uncertain);
   end Context_Drain;
   function Context_Retirement (Session : Unsigned_64) return Context_Drain.Retirement_State is
     (case Models (Intel_GPU_Render_Control.Storage_Index (Render_Admission, Session)).Context is
        when Absent => Context_Drain.No_Context,
        when Deregistered => Context_Drain.Deregistered,
        when Pending => Context_Drain.Pending,
        when Quarantined => Context_Drain.Uncertain);
   package Image_Retirement is
      type Result is (Rejected, Quarantined, Address_Released);
   end Image_Retirement;
   function Image_Retirement_Results (S : Session_Slot) return Image_Retirement.Result is
     (if Models (S).Image_Released then Image_Retirement.Address_Released
      else Image_Retirement.Rejected);
   type Backing is record Ready : Boolean := False; end record;
   type Context_Record is record Parent : Backing; end record;
   function Private_Contexts (S : Session_Slot) return Context_Record is
     ((Parent => (Ready => Models (S).Parent_Ready)));
   Application_Buffer_State, Application_Map_State, Queue_State, Contexts : Natural := 0;
   package Application_Buffers is
      function Client_Usage (Object : Natural; Session : Unsigned_64)
        return Intel_GPU_Client_Budgets.Usage;
      procedure Forget_Client_Account (Object : in out Natural; Session : Unsigned_64; Accepted : out Boolean);
   end Application_Buffers;
   package body Application_Buffers is
      function Client_Usage (Object : Natural; Session : Unsigned_64)
        return Intel_GPU_Client_Budgets.Usage is
        ((Known => True, Closed => True, Limit => 4096 * 64,
          Charged => Models (Intel_GPU_Render_Control.Storage_Index (Render_Admission, Session)).Charged));
      procedure Forget_Client_Account (Object : in out Natural; Session : Unsigned_64; Accepted : out Boolean) is
      begin
         Note ("forget-account"); Accepted := True;
      end Forget_Client_Account;
   end Application_Buffers;
   package Application_Maps is
      type Retirement_State is (Clear, Outstanding, Uncertain);
      function Observe_Retirement (Object : Natural; Session : Unsigned_64) return Retirement_State is
        (if Models (Intel_GPU_Render_Control.Storage_Index (Render_Admission, Session)).Grants_Clear
         then Clear else Outstanding);
   end Application_Maps;
   package Queue_Service is
      function Live (T : Natural; S : Session_Slot) return Boolean is (False);
      function Session_Idle (T : Natural; S : Session_Slot) return Boolean is (Models (S).Queue_Idle);
      procedure Forget_Session (T : in out Natural; S : Session_Slot; Forgotten : out Boolean);
   end Queue_Service;
   package body Queue_Service is
      procedure Forget_Session (T : in out Natural; S : Session_Slot; Forgotten : out Boolean) is
      begin
         Note ("forget-queue"); Forgotten := True;
      end Forget_Session;
   end Queue_Service;
   function Slot_Unreferenced (S : Session_Slot; Session : Unsigned_64) return Boolean is
     (Models (S).Unreferenced);
   function Slot_Of_ID (ID : Unsigned_32) return Session_Slot is
     (Session_Slot (ID - RS.Context_ID (1) + 1));
   package Context_Pool is
      function In_Use (Object : Natural; ID : Unsigned_32) return Boolean is
        (Models (Slot_Of_ID (ID)).Context /= Absent);
      function Releasable (Object : Natural; ID : Unsigned_32) return Boolean is
        (Models (Slot_Of_ID (ID)).Context = Deregistered);
      procedure Release (Object : in out Natural; ID : Unsigned_32; Accepted : out Boolean);
   end Context_Pool;
   package body Context_Pool is
      procedure Release (Object : in out Natural; ID : Unsigned_32; Accepted : out Boolean) is
      begin
         Accepted := Releasable (Object, ID);
         if Accepted then Models (Slot_Of_ID (ID)).Context := Absent; end if;
         Note ("release-context");
      end Release;
   end Context_Pool;
   type Bundle is null record;
   Context_Metadata, Directory_Metadata : array (Session_Slot range 1 .. Slots_In_Use) of Bundle;
   package Context_Metadata_Release is
      procedure Release (Object : in out Bundle; Accepted : out Boolean);
   end Context_Metadata_Release;
   package body Context_Metadata_Release is
      procedure Release (Object : in out Bundle; Accepted : out Boolean) is
      begin Note ("release-context-metadata"); Accepted := True; end Release;
   end Context_Metadata_Release;
   package Directory_Metadata_Release is
      procedure Release (Object : in out Bundle; Accepted : out Boolean);
   end Directory_Metadata_Release;
   package body Directory_Metadata_Release is
      procedure Release (Object : in out Bundle; Accepted : out Boolean) is
      begin Note ("release-directory-metadata"); Accepted := True; end Release;
   end Directory_Metadata_Release;
   procedure Reset_Session_Slot (S : Session_Slot) is
   begin
      Note ("reset" & Natural'Image (S));
      Models (S) := (others => <>);
   end Reset_Session_Slot;
   Waits : Natural := 0;
   procedure Report_Recycle_Wait (S : Session_Slot; Session : Unsigned_64; Facts : RS.Release_Facts) is
   begin
      Waits := Waits + 1;
   end Report_Recycle_Wait;
   type Cleanup_Phase is (Not_Started, Closing_Buffers, Closing_Maps, Swept);
   type Cleanup_State is record Phase : Cleanup_Phase := Swept; end record;
   function Cleanups (S : Session_Slot) return Cleanup_State is
     ((Phase => (if Models (S).Phase_Swept then Swept else Closing_Maps)));
   Quiet : Boolean := True;
   function Recycling_Quiet return Boolean is (Quiet);
   Runtime_Fault : Boolean := False;
   Retiring_Sessions : Session_Count := 0;
   Recycled_Sessions : Unsigned_64 := 0;
'''

suffix = r'''
   Failures : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      if not Condition then Failures := Failures + 1; Put_Line ("FAIL: " & What); end if;
   end Check;
   Broker : constant := 42; Broker_Tag : constant := 99;
   Reply : Intel_GPU_Render_Control.Words;
   function Admit (Identity : Unsigned_64) return Unsigned_64 is
      use Intel_GPU_Render_Control;
   begin
      Handle (Render_Admission, Broker, Broker_Tag, True, Label, 4, 0, 0,
              [Version, Identity, 0, Reserve], Reply);
      Handle (Render_Admission, Broker, Broker_Tag, True, Label, 4, 0, 0,
              [Version, Identity, Reply (2), Activate], Reply);
      return Reply (2);
   end Admit;
   Tag : Unsigned_64;
   S : Session_Slot;
   Notice : CuBit.Control_Events.Event;
begin
   Intel_GPU_Render_Control.Bind (Render_Admission, Broker, Broker_Tag);
   Intel_GPU_Render_Control.Grow (Render_Admission, Slots_In_Use);
   -- The client dies: its queue's grant ends; the session retires once.
   Tag := Admit (16#1_0000_0001#);
   S := Intel_GPU_Render_Control.Storage_Index (Render_Admission, Tag);
   Queue_Links (S) := (Active => True, Number => S);
   Intel_GPU_Render_Control.Hold (Render_Admission, S, RS.GuC_Context);
   Intel_GPU_Render_Control.Hold (Render_Admission, S, RS.Context_Image);
   Models (S) := (Context => Pending, Image_Released => False, others => <>);
   Notice := (Slot => S);
   Handle_Grant_Event (Notice);
   Check (Retired_Count = 1 and Intel_GPU_Render_Control.Retired (Render_Admission, S),
          "client death retires the session");
   Handle_Grant_Event (Notice);
   Handle_Channel_Close (16#1_0000_0001#, (Words => Unsigned_64 (S)));
   Check (Retired_Count = 1, "a second end notice changes nothing");
   Retiring_Sessions := 1;
   -- Nothing is recycled until the GuC acknowledges and everything is back.
   for Missing in 1 .. 8 loop
      Models (S) := (Context => (if Missing = 1 then Pending else Deregistered),
                     Image_Released => Missing /= 2, Parent_Ready => Missing = 3,
                     Charged => (if Missing = 4 then 4096 else 0),
                     Grants_Clear => Missing /= 5, Queue_Idle => Missing /= 6,
                     Unreferenced => Missing /= 7, Phase_Swept => Missing /= 8);
      if Missing = 1 then
         -- Image released but the context not deregistered.
         Models (S).Image_Released := True;
      end if;
      -- Each case starts from a session holding both (test only).
      Intel_GPU_Render_Control.Hold (Render_Admission, S, RS.GuC_Context);
      Intel_GPU_Render_Control.Hold (Render_Admission, S, RS.Context_Image);
      Trace := Null_Unbounded_String; Waits := 0;
      Recycle_Retired_Sessions;
      Check (Intel_GPU_Render_Control.Issued_Tag (Render_Admission, S) = Tag,
             "not recycled while anything is held (case" & Natural'Image (Missing) & ")");
      Check (Index (Trace, "release") = 0 and Index (Trace, "reset") = 0,
             "nothing released before recycling (case" & Natural'Image (Missing) & ")");
      Check (Missing = 8 or else Waits = 1, "a waiting session is reported (case" & Natural'Image (Missing) & ")");
      Check (Missing /= 1 or else
             Intel_GPU_Render_Control.Holds (Render_Admission, S, RS.GuC_Context),
             "the GuC context hold stays until the GuC answers");
   end loop;
   -- Not while other application work is pending.
   Models (S) := (Context => Deregistered, others => <>);
   Quiet := False;
   Recycle_Retired_Sessions;
   Check (Intel_GPU_Render_Control.Issued_Tag (Render_Admission, S) = Tag, "waits for quiet");
   Quiet := True;
   Trace := Null_Unbounded_String;
   Recycle_Retired_Sessions;
   Check (Intel_GPU_Render_Control.Issued_Tag (Render_Admission, S) = 0 and
          Intel_GPU_Render_Control.Recycled (Render_Admission, Tag), "recycled when all is released");
   Check (To_String (Trace) = "release-context;forget-queue;release-context-metadata;" &
          "release-directory-metadata;forget-account;reset" & Natural'Image (S) & ";log:INFORMATION;",
          "releases in order: context ID, queue, metadata, account, then reset: " & To_String (Trace));
   Check (Retiring_Sessions = 0 and Recycled_Sessions = 1, "counters");
   -- The slot is reused under a fresh tag; Mesa's late close of the old
   -- tag is answered as finished.
   Check (Admit (16#1_0000_0002#) /= Tag, "fresh tag");
   Check (Intel_GPU_Render_Control.Storage_Index (Render_Admission, Reply (2)) = S, "same slot reused");
   -- A session closed by its queue (OP_CLOSE) retires the same way.
   declare T2 : constant Unsigned_64 := Admit (16#1_0000_0003#);
           S2 : constant Session_Slot := Intel_GPU_Render_Control.Storage_Index (Render_Admission, T2);
   begin
      Queue_Links (S2) := (Active => True, Number => S2);
      Handle_Channel_Close (16#1_0000_0003#, (Words => Unsigned_64 (S2)));
      Check (Retired_Count = 2 and Intel_GPU_Render_Control.Retired (Render_Admission, S2),
             "queue close retires");
   end;
   if Failures = 0 then
      Put_Line ("Recycle glue PASS: client death and queue close retire once; recycling waits for every release and acknowledgement; releases in order");
   else
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Recycle_Glue_Tests;
'''

work = Path(tempfile.mkdtemp(prefix="cubit-recycle-glue."))
(work / "recycle_glue_tests.adb").write_text(prefix + glue + suffix)
(work / "test.gpr").write_text(f'''project Test is
   for Source_Dirs use (".", "{source}");
   for Object_Dir use "obj";
   for Main use ("recycle_glue_tests.adb");
   package Compiler is
      for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato", "-gnatwn");
   end Compiler;
end Test;
''')
subprocess.run(["gprbuild", "-q", "-p", "-P", str(work / "test.gpr")], cwd=work, check=True)
result = subprocess.run([str(work / "obj" / "recycle_glue_tests")], capture_output=True, text=True)
print(result.stdout, end="")
if result.returncode != 0 or "PASS" not in result.stdout:
    raise SystemExit(1)
