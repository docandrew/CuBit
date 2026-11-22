pragma Ada_2022;
with AML_Delays;
with AML_Events;
generic
   with procedure Perform (Item : AML_Delays.Request; Result : out AML_Delays.Outcome);
-- Quiescent local events only: the caller has already checked the event count.
-- Completed from Perform must certify at least the requested elapsed duration.
-- This adapter cannot receive signals, admit a window, or extend its deadline.
package ACPI_Event_Waits with SPARK_Mode is
   use type AML_Events.Timeout_Outcome;
   Finite_Timeout_Maximum : constant := AML_Events.Wait_Forever - 1;
   Chunk_Milliseconds : constant := AML_Delays.Sleep_Milliseconds'Last;
   Maximum_Chunks : constant :=
     (Finite_Timeout_Maximum + Chunk_Milliseconds - 1) / Chunk_Milliseconds;
   type Chunk_Count is range 0 .. Maximum_Chunks;
   type Report is record
      Code : AML_Events.Timeout_Outcome := AML_Events.Unavailable;
      Completed_Milliseconds : AML_Events.Timeout := 0;
      Attempts : Chunk_Count := 0;
   end record;
   procedure Run (Milliseconds : AML_Events.Timeout; Result : out Report)
     with Post => Result.Completed_Milliseconds <= Milliseconds
       and then (if Result.Code = AML_Events.Elapsed then
         Milliseconds in 1 .. Finite_Timeout_Maximum
         and then Result.Completed_Milliseconds = Milliseconds
         and then Result.Attempts > 0)
       and then (if Milliseconds in AML_Events.Poll_Only | AML_Events.Wait_Forever then
         Result.Code = AML_Events.Unavailable
         and then Result.Completed_Milliseconds = 0 and then Result.Attempts = 0);
end ACPI_Event_Waits;
