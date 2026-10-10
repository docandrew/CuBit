with Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.GPU_Queues;
with CuBit.GPU_Queue_Clients;
with Intel_GPU_ADLN_Context_Init;
with Intel_GPU_GuC_Submission_Policy;
with Intel_GPU_Native_Queue_Ring;
with Intel_GPU_Queue_Service;
with Intel_GPU_Ring_Reservation;
-- The production native ring writer (Intel_GPU_Native_Queue_Ring) under the
-- queue service, on real memory laid out as a context backing (saved tail at
-- +4124, ring at +64 KiB), Linux-hosted. A model GPU executes the ring as
-- written: from its head it skips MI_NOOP padding and runs each 96-word
-- segment, taking the timeline value from the segment's own breadcrumb
-- words. So a misplaced, overwritten or torn segment shows up as a wrong
-- timeline. Each GPU step also saves the context as Gen12 hardware does
-- (TGL PRM RING_HEAD/RING_TAIL): RING_HEAD gets the engine's dword offset
-- and its wrap count in bits 21..31, RING_TAIL the tail register, which the
-- model may rewrite with nonzero reserved high bits. The writer must accept
-- both and still refuse a tail whose offset field is not its own.
-- Not hardware evidence.
procedure Ring_Exhaustion_Submit_Tests is
   package Q renames CuBit.GPU_Queues;
   package Clients renames CuBit.GPU_Queue_Clients;
   package Policy renames Intel_GPU_GuC_Submission_Policy;
   use type Q.Timeline_Value;

   Backing_Bytes_Value : constant := 81_920;
   type Backing is array (0 .. Backing_Bytes_Value - 1) of Unsigned_8 with Alignment => 4096;
   Memory : access Backing := new Backing'(others => 0);
   Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Memory.all'Address));
   Ring_At : constant Unsigned_64 := 65_536;
   Tail_At : constant Unsigned_64 := 4_124;
   Ring_Bytes : constant := 16_384;
   Segment_Words : constant := 96;

   Owner : Boolean := True;
   Coherent_Mapping : Boolean := True;
   -- Engine wraps of the ring, saved in RING_HEAD's wrap count.
   Head_Wraps : Unsigned_32 := 0;
   -- Reserved RING_TAIL bits the context save leaves set (tolerated).
   Saved_Tail_Noise : Unsigned_32 := 0;
   Quarantines, Calls_OK, Calls_Failed, Bad_Values : Natural := 0;
   Last_Call : Unsigned_64 := 0;
   Head : Unsigned_32 := 384;      -- the setup segment has run
   Timeline : Unsigned_64 := 1;
   Kicked : Boolean := False;
   Clock : Unsigned_64 := 1_000_000;

   function Word (Offset : Unsigned_32) return Unsigned_32 is
      W : constant Unsigned_32 with Import, Volatile,
        Address => To_Address (Integer_Address (Base + Ring_At + Unsigned_64 (Offset)));
   begin
      return W;
   end Word;
   Head_At : constant Unsigned_64 := 4_116;
   -- The tail field (QWORD offset, bits 3..20) of the saved RING_TAIL.
   function Saved_Tail return Unsigned_32 is
      W : constant Unsigned_32 with Import, Volatile,
        Address => To_Address (Integer_Address (Base + Tail_At));
   begin
      return W and 16#001F_FFF8#;
   end Saved_Tail;
   -- A context save: RING_HEAD with the wrap count, RING_TAIL rewritten.
   procedure Save_Context is
      H : Unsigned_32 with Import, Volatile,
        Address => To_Address (Integer_Address (Base + Head_At));
      W : Unsigned_32 with Import, Volatile,
        Address => To_Address (Integer_Address (Base + Tail_At));
      Tail : constant Unsigned_32 := W and 16#001F_FFF8#;
   begin
      H := Shift_Left (Head_Wraps and 16#7FF#, 21) or Head;
      W := Tail or Saved_Tail_Noise;
   end Save_Context;

   -- One segment per turn, in ring order, as far as the saved tail.
   procedure GPU_Step is
      Value : Unsigned_64;
   begin
      if not Kicked then return; end if;
      while Head /= Saved_Tail and then Word (Head) = 0 loop
         if Head + 4 = Ring_Bytes then
            Head_Wraps := Head_Wraps + 1;
         end if;
         Head := (Head + 4) mod Ring_Bytes;   -- MI_NOOP padding
      end loop;
      if Head = Saved_Tail then return; end if;
      Value := Unsigned_64 (Word (Head + Unsigned_32 (Intel_GPU_ADLN_Context_Init.Breadcrumb_Low) * 4)) or
        Shift_Left (Unsigned_64 (Word (Head + Unsigned_32 (Intel_GPU_ADLN_Context_Init.Breadcrumb_High) * 4)), 32);
      if Value /= Timeline + 1 then
         Bad_Values := Bad_Values + 1;
      end if;
      Timeline := Value;
      if Head + Segment_Words * 4 >= Ring_Bytes then
         Head_Wraps := Head_Wraps + 1;
      end if;
      Head := (Head + Segment_Words * 4) mod Ring_Bytes;
      Save_Context;
   end GPU_Step;

   subtype Session_Id is Positive range 1 .. 1;
   function CPU_Base return Unsigned_64 is (Base);
   function Bytes return Unsigned_64 is (Backing_Bytes_Value);
   function Owned return Boolean is (Owner);
   function Coherent return Boolean is (Coherent_Mapping);
   package Native is new Intel_GPU_Native_Queue_Ring (CPU_Base, Bytes, Owned, Coherent);
   Last_Report : Native.Report;
   use type Native.Outcome;

   function Select_Context (S : Session_Id; C : Q.Context_Index) return Boolean is
     (C = 0 and then Owner);
   function Batch_Ready (Handle, GPU, Offset, Size : Unsigned_64) return Boolean is (True);
   function Resident return Boolean is (Kicked);
   procedure Write_Segment
     (Operation : Q.Opcode; V, GPU : Unsigned_64; Plan : Intel_GPU_Ring_Reservation.Plan;
      Expected_Tail : Unsigned_32; OK : out Boolean)
   is
      pragma Unreferenced (Operation);
      Segment : constant Intel_GPU_ADLN_Context_Init.Segment :=
        Intel_GPU_ADLN_Context_Init.Build_Batch (True, 0, V, GPU);
      Words : Native.Word_Array := [others => 0];
   begin
      for I in Segment.Words'Range loop Words (I) := Segment.Words (I); end loop;
      Native.Write (Words, Segment.Words'Length, Plan, Expected_Tail, Last_Report);
      OK := Native."=" (Last_Report.Result, Native.Written);
   end Write_Segment;
   function Segment_Bytes (Operation : Q.Opcode) return Unsigned_32 is (Segment_Words * 4);
   procedure Kick (Enable : Boolean; Result : out Policy.Kick_Result) is
      pragma Unreferenced (Enable);
   begin
      Kicked := True;
      Result := Policy.Kick_Queued;
   end Kick;
   procedure Read_Timeline (V : out Unsigned_64; OK : out Boolean) is
   begin
      V := Timeline; OK := True;
   end Read_Timeline;
   function Now_Us return Unsigned_64 is (Clock);
   procedure Quarantine (S : Session_Id; Why : Q.Fault_Reason) is
   begin
      Quarantines := Quarantines + 1;
   end Quarantine;
   procedure Call_Finished (S : Session_Id; V : Unsigned_64; OK : Boolean) is
   begin
      if OK then Calls_OK := Calls_OK + 1; else Calls_Failed := Calls_Failed + 1; end if;
      Last_Call := V;
   end Call_Finished;
   procedure Answer_Wake (S : Session_Id; Result : Q.Wake_Result) is null;
   type Region is array (1 .. 2 * 4096) of Unsigned_8 with Alignment => 4096;
   Client_Memory, Driver_Memory : access Region := new Region'(others => 0);
   function Client_Region (S : Session_Id) return System.Address is (Client_Memory.all'Address);
   function Driver_Region (S : Session_Id) return System.Address is (Driver_Memory.all'Address);
   package Service is new Intel_GPU_Queue_Service
     (Session_Id, Select_Context, Owned, Batch_Ready, Resident, Owned, Write_Segment,
      Segment_Bytes, Kick, Read_Timeline, Now_Us, Quarantine, Call_Finished, Answer_Wake,
      Client_Region, Driver_Region, 1_000_000);
   use type Service.Call_Result;
   T : access Service.Table := new Service.Table;
   Client : access Clients.Client := new Clients.Client;
   Result : Service.Call_Result;
   Opened : Boolean;
   Failures : Natural := 0;
   procedure Check (Condition : Boolean; Label : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Label);
      end if;
   end Check;
   procedure Turn is
   begin
      Clock := Clock + 1_000;
      GPU_Step;
      Service.Turn (T.all);
   end Turn;
   Wraps : Natural := 0;
   Previous_Tail : Unsigned_32;
begin
   -- Registration left the setup segment (value 1) and the saved tail at 384.
   declare
      W : Unsigned_32 with Import, Volatile, Address => To_Address (Integer_Address (Base + Tail_At));
   begin
      W := 384;
   end;
   Service.Open_Context (T.all, 1, 0, 1, 384, Opened);
   Check (Opened, "open context");
   -- The synchronous wrapper, one job at a time, around the ring many times.
   for N in 1 .. 4_096 loop
      Previous_Tail := Saved_Tail;
      Service.Submit_Call (T.all, 1, 0, 7, 16#20_0000#, 0, 4096, 1_000_000, Result);
      Check (Result = Service.Submitted, "call submitted");
      if Saved_Tail < Previous_Tail then Wraps := Wraps + 1; end if;
      for Step in 1 .. 3 loop Turn; end loop;
   end loop;
   Check (Calls_OK = 4_096 and Calls_Failed = 0 and Last_Call = 4_097, "4096 calls completed");
   -- The queue: 32 jobs in flight in the same ring, around it again.
   Service.Open_Queue (T.all, 1, Opened);
   Check (Opened, "open queue");
   Clients.Attach (Client.all, Client_Memory.all'Address, Driver_Memory.all'Address);
   declare
      Tag : Clients.Token;
      Signal : Q.Timeline_Value;
      OK, Kick_Wanted, Got : Boolean;
      Item : Clients.Q.Completion;
      Submitted, Reaped : Natural := 0;
      Job : constant Clients.Job :=
        (Operation => Q.Execute, Context => 0, Handle => 7, GPU => 16#20_0000#, Offset => 0,
         Bytes => 4096, First => (others => <>), Second => (others => <>),
         Deadline => Q.No_Deadline);
   begin
      for Step in 1 .. 20_000 loop
         while Submitted < 4_096 and then Submitted - Reaped < 32 loop
            Previous_Tail := Saved_Tail;
            Clients.Submit (Client.all, Job, Tag, Signal, OK, Kick_Wanted);
            exit when not OK;
            Submitted := Submitted + 1;
         end loop;
         Previous_Tail := Saved_Tail;
         Turn;
         if Saved_Tail < Previous_Tail then Wraps := Wraps + 1; end if;
         loop
            Clients.Reap (Client.all, Item, Got);
            exit when not Got;
            Check (Item.Answer.Status = Q.Completion_Status'Enum_Rep (Q.Completed),
                   "queue job completed");
            Reaped := Reaped + 1;
         end loop;
         exit when Reaped = 4_096;
      end loop;
      Check (Reaped = 4_096, "4096 queue jobs completed");
      Check (Service.Stats (T.all).In_Flight_Peak >= 30, "many in flight on the native ring");
   end;
   Check (Bad_Values = 0, "the GPU read every breadcrumb in order: nothing misplaced or overwritten");
   Check (Timeline = 8_193, "the timeline reached the last value");
   Check (Wraps >= 100, "the ring wrapped" & Natural'Image (Wraps) & " times");
   Check (Quarantines = 0, "no quarantine");
   Check (Head_Wraps > 0, "saved heads carried a wrap count");
   -- Reserved RING_TAIL bits from a context save are not the tail: the
   -- writer compares the offset field only.
   Saved_Tail_Noise := 16#8020_0000#;
   Save_Context;
   Calls_OK := 0;
   for N in 1 .. 64 loop
      Service.Submit_Call (T.all, 1, 0, 7, 16#20_0000#, 0, 4096, 1_000_000, Result);
      Check (Result = Service.Submitted, "call submitted over a noisy saved tail");
      for Step in 1 .. 3 loop Turn; end loop;
   end loop;
   Check (Calls_OK = 64 and Bad_Values = 0, "noisy saved tail: 64 calls completed in order");
   Saved_Tail_Noise := 0;
   -- No coherent mapping of this context: refused before any read or write.
   declare
      W : constant Unsigned_32 with Import, Volatile,
        Address => To_Address (Integer_Address (Base + Tail_At));
      Kept : constant Unsigned_32 := W;
      Words : constant Native.Word_Array := [others => 0];
      Plan : constant Intel_GPU_Ring_Reservation.Plan :=
        Intel_GPU_Ring_Reservation.Reserve (0, Saved_Tail, Segment_Words * 4);
   begin
      Coherent_Mapping := False;
      Native.Write (Words, Segment_Words, Plan, Saved_Tail, Last_Report);
      Check (Last_Report.Result = Native.Not_Owned and W = Kept, "not coherent: Not_Owned, nothing written");
      Coherent_Mapping := True;
   end;
   -- A tail field that is not the window's (another writer) is never
   -- overwritten, and the raw dwords are reported.
   declare
      W : Unsigned_32 with Import, Volatile, Address => To_Address (Integer_Address (Base + Tail_At));
      Kept : constant Unsigned_32 := W;
   begin
      W := Kept + 8;
      Service.Submit_Call (T.all, 1, 0, 7, 16#20_0000#, 0, 4096, 1_000_000, Result);
      Check (Result = Service.Faulted and Quarantines = 1, "tail mismatch: the session is lost");
      Check (W = Kept + 8, "tail mismatch: nothing written");
      Check (Last_Report.Result = Native.Tail_Mismatch and Last_Report.Raw_Tail = Kept + 8,
             "tail mismatch: reported with the raw saved tail");
   end;
   if Failures = 0 then
      Ada.Text_IO.Put_Line
        ("Ring exhaustion PASS: 4096 wrapper calls and 4096 queue jobs (32 in flight) through the" &
         " native ring writer," & Natural'Image (Wraps) &
         " wraps, every breadcrumb executed in order; a foreign tail is never overwritten (model GPU)");
   else
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Ring_Exhaustion_Submit_Tests;
