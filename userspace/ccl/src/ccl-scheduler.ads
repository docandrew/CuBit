with Interfaces;
with CCL.VM;

package CCL.Scheduler with
   SPARK_Mode => On
is
   use Interfaces;

   MAX_ISOLATES : constant := 4;
   subtype Isolate_Index is Natural range 0 .. MAX_ISOLATES - 1;

   type Isolate_Status is
     (Empty, Runnable, Waiting, Finished, Failed);

   type Event_Kind is
     (No_Event, Host_Request, Isolate_Completed, Isolate_Failed);

   type Scheduler_Event is record
      Kind      : Event_Kind := No_Event;
      Isolate   : Isolate_Index := 0;
      Token     : Unsigned_64 := 0;
      Import    : CCL.VM.Import_Index := 0;
      Authority : CCL.VM.Authority_Class := CCL.VM.No_Authority;
      Binding   : Unsigned_32 := 0;
      Argument  : CCL.VM.Value := (others => <>);
      Has_Value : Boolean := False;
      Value     : CCL.VM.Value := (others => <>);
      Failure   : CCL.VM.Execution_Status := CCL.VM.No_Result;
   end record;

   type Scheduler_State is private;

   procedure Initialize (State : out Scheduler_State);

   procedure Start
     (State   : in out Scheduler_State;
      Program : CCL.VM.Validated_Program;
      Fuel    : Natural;
      Started : out Boolean;
      Isolate : out Isolate_Index)
   with Pre => CCL.VM.Is_Valid (Program);

   procedure Dispatch_One
     (State : in out Scheduler_State;
      Event : out Scheduler_Event);

   procedure Complete
     (State    : in out Scheduler_State;
      Token    : Unsigned_64;
      Response : CCL.VM.Value;
      Accepted : Boolean;
      Matched  : out Boolean);

   function Status
     (State : Scheduler_State;
      Item  : Isolate_Index) return Isolate_Status;

private
   type Isolate_Record is record
      Status     : Isolate_Status := Empty;
      Generation : Unsigned_32 := 0;
      Token      : Unsigned_64 := 0;
      Program    : CCL.VM.Validated_Program;
      Machine    : CCL.VM.Machine_State;
   end record;

   type Isolate_Array is array (Isolate_Index) of Isolate_Record;

   type Scheduler_State is record
      Isolates : Isolate_Array;
      Next     : Isolate_Index := 0;
   end record;
end CCL.Scheduler;
