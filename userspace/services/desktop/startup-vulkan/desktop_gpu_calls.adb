--  Thin binding to the glue's exported C symbols; nothing to prove here.
package body Desktop_GPU_Calls with SPARK_Mode => Off is
   procedure Bound_Calls (Milliseconds : Interfaces.Unsigned_64)
     with Import, Convention => C, External_Name => "cubit_intel_bound_calls";
   function Call_Timeouts return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "cubit_intel_call_timeouts";
   function Last_Label return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "cubit_intel_last_timed_out_label";

   procedure Bound (Milliseconds : Interfaces.Unsigned_64) is
   begin
      Bound_Calls (Milliseconds);
   end Bound;

   procedure Read (Timeouts, Label : out Interfaces.Unsigned_32) is
   begin
      Timeouts := Call_Timeouts;
      Label := Last_Label;
   end Read;
end Desktop_GPU_Calls;
