with Interfaces;
-- Count and elapsed-time admission between presentation opportunities.
-- The caller starts exactly two input phases per turn, before and after the
-- request phase. No admission policy bounds the execution of one handler.
package Compositor_Dispatch_Budget with Pure, SPARK_Mode is
   subtype Tick is Interfaces.Unsigned_64;
   use type Tick;
   Unavailable : constant Tick := Tick'Last;
   Event_Limit : constant := 64;
   Frame_Request_Limit : constant := 32;
   Idle_Request_Limit : constant := 96;
   Input_Phase_Us : constant Tick := 500;
   Request_Phase_Us : constant Tick := 1_000;
   subtype Event_Count is Natural range 0 .. Event_Limit;
   subtype Phase_Count is Natural range 0 .. 2;
   subtype Request_Count is Natural range 0 .. Idle_Request_Limit;

   function Within (Started, Now, Budget : Tick) return Boolean is
     (Started /= Unavailable and then Now /= Unavailable and then
      Now >= Started and then Now - Started < Budget);

   type Input_Batch is private;
   function Events (S : Input_Batch) return Event_Count;
   function Phases (S : Input_Batch) return Phase_Count;
   function Phase_Used (S : Input_Batch) return Boolean;
   function Input_Start (S : Input_Batch) return Tick;
   function New_Input return Input_Batch
     with Post => Events (New_Input'Result) = 0 and Phases (New_Input'Result) = 0;
   procedure Begin_Input_Phase (S : in out Input_Batch; Now : Tick)
     with Pre => Phases (S) < Phase_Count'Last,
       Post => Events (S) = Events (S'Old) and not Phase_Used (S) and
         Input_Start (S) = Now and Phases (S) = Phases (S'Old) + 1;
   function Can_Input (S : Input_Batch; Now : Tick) return Boolean
     with Post => Can_Input'Result =
       (Phases (S) > 0 and then Events (S) < Event_Limit and then
        (not Phase_Used (S) or else Within (Input_Start (S), Now, Input_Phase_Us)));
   procedure Charge_Input (S : in out Input_Batch)
     with Pre => Phases (S) > 0 and Events (S) < Event_Limit,
       Post => Events (S) = Events (S'Old) + 1 and Phase_Used (S) and
         Input_Start (S) = Input_Start (S'Old) and Phases (S) = Phases (S'Old);

   type Request_Batch is private;
   function Requests (S : Request_Batch) return Request_Count;
   function Request_Start (S : Request_Batch) return Tick;
   function New_Requests (Now : Tick) return Request_Batch
     with Post => Requests (New_Requests'Result) = 0 and
       Request_Start (New_Requests'Result) = Now;
   function Can_Request
     (S : Request_Batch; Now : Tick; Frame_Pending : Boolean) return Boolean
     with Post => Can_Request'Result =
       (Requests (S) < (if Frame_Pending then Frame_Request_Limit else Idle_Request_Limit)
        and then (Requests (S) = 0 or else
          Within (Request_Start (S), Now, Request_Phase_Us)));
   procedure Charge_Request (S : in out Request_Batch)
     with Pre => Requests (S) < Idle_Request_Limit,
       Post => Requests (S) = Requests (S'Old) + 1 and
         Request_Start (S) = Request_Start (S'Old);
   Completion_Limit : constant := 64;
   Completion_Phase_Us : constant Tick := 500;
   subtype Completion_Count is Natural range 0 .. Completion_Limit;
   type Completion_Batch is private;
   function Completions (S : Completion_Batch) return Completion_Count;
   function Completion_Start (S : Completion_Batch) return Tick;
   function New_Completions (Now : Tick) return Completion_Batch
     with Post => Completions (New_Completions'Result) = 0 and
       Completion_Start (New_Completions'Result) = Now;
   function Can_Complete (S : Completion_Batch; Now : Tick) return Boolean
     with Post => Can_Complete'Result =
       (Completions (S) < Completion_Limit and then
        (Completions (S) = 0 or else Within (Completion_Start (S), Now, Completion_Phase_Us)));
   procedure Charge_Completion (S : in out Completion_Batch)
     with Pre => Completions (S) < Completion_Limit,
       Post => Completions (S) = Completions (S'Old) + 1 and
         Completion_Start (S) = Completion_Start (S'Old);

private
   type Input_Batch is record
      Used : Event_Count := 0;
      Charged : Boolean := False;
      Start : Tick := Unavailable;
      Opened : Phase_Count := 0;
   end record;
   function Events (S : Input_Batch) return Event_Count is (S.Used);
   function Phases (S : Input_Batch) return Phase_Count is (S.Opened);
   function Phase_Used (S : Input_Batch) return Boolean is (S.Charged);
   function Input_Start (S : Input_Batch) return Tick is (S.Start);
   type Request_Batch is record
      Used : Request_Count := 0;
      Start : Tick := Unavailable;
   end record;
   function Requests (S : Request_Batch) return Request_Count is (S.Used);
   function Request_Start (S : Request_Batch) return Tick is (S.Start);
   type Completion_Batch is record
      Used : Completion_Count := 0;
      Start : Tick := Unavailable;
   end record;
   function Completions (S : Completion_Batch) return Completion_Count is (S.Used);
   function Completion_Start (S : Completion_Batch) return Tick is (S.Start);
end Compositor_Dispatch_Budget;
