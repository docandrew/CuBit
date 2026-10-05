with Interfaces;
with CCL.Host_Values;
with CCL.Streams;

--  Host calls of one evaluation, kept so the evaluation can run again
--  without making them twice (docs/control-language.md, Task<T>). The
--  interpreter cannot suspend inside (wait t): when t is pending it stops
--  (Waiting_On_Task), and once t completes the session runs the entry again.
--  Calls the first run made are answered from the log in order; a call
--  that differs from the logged one (the environment changed meanwhile)
--  is refused and marks the log Diverged. Calls past the log go to the
--  host and are logged; past MAX_CALLS they still go to the host but the
--  log is Overflowed and the entry cannot be run again safely.
--  Stream views are not logged: they read the current state, and a
--  completed task's result never changes.
generic
   type Host_Context is limited private;
   with procedure Invoke
     (Context : in out Host_Context; Binding : Interfaces.Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result);
   with procedure Read_Stream
     (Context : in out Host_Context; Request : CCL.Streams.View_Request;
      Reply : in out CCL.Streams.View_Reply) is null;
package CCL.Host_Replay with SPARK_Mode => On is
   MAX_CALLS : constant := 8;
   subtype Call_Count is Natural range 0 .. MAX_CALLS;

   type Log is private;
   function Overflowed (Item : Log) return Boolean;
   function Diverged (Item : Log) return Boolean;
   function Length (Item : Log) return Call_Count;
   --  Start a run that answers the logged calls first.
   procedure Rewind (Item : in out Log);

   type Context is limited record
      Inner : Host_Context;
      Calls : Log;
   end record;
   --  Forget every call: the next run records afresh.
   procedure Clear (Item : in out Context);
   procedure Invoke_Logged
     (Item : in out Context; Binding : Interfaces.Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result);
   procedure Read_Logged
     (Item : in out Context; Request : CCL.Streams.View_Request;
      Reply : in out CCL.Streams.View_Reply);
private
   subtype Call_Index is Positive range 1 .. MAX_CALLS;
   type Call is record
      Binding : Interfaces.Unsigned_32 := 0;
      Argument : CCL.Host_Values.Value;
      Reply : CCL.Host_Values.Call_Result;
   end record;
   type Call_Array is array (Call_Index) of Call;
   type Log is record
      Calls : Call_Array;
      Count : Call_Count := 0;
      --  Calls answered so far in this run.
      Next : Call_Count := 0;
      Overflow, Divergence : Boolean := False;
   end record
     with Dynamic_Predicate => Next <= Count;
   function Overflowed (Item : Log) return Boolean is (Item.Overflow);
   function Diverged (Item : Log) return Boolean is (Item.Divergence);
   function Length (Item : Log) return Call_Count is (Item.Count);
end CCL.Host_Replay;
