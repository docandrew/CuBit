pragma Ada_2022;
-- Each caller's first refusal must be atomic and side-effect free. The retry
-- flag requires the caller to collect using its original complete root census
-- before the second allocation. Never instantiate around firmware IO or an
-- entire effectful AML instruction. Callbacks return normally, no reentry.
generic
   with procedure Attempt (Collect_Before : Boolean);
   with function Capacity_Refused return Boolean;
package AML_Allocation_Retry with Pure, SPARK_Mode is
   subtype Attempt_Count is Positive range 1 .. 2;
   type Report is record Attempts : Attempt_Count := 1; Retried : Boolean := False; end record;
   procedure Run (Result : out Report)
     with Post => Result.Retried = (Result.Attempts = 2);
end AML_Allocation_Retry;
