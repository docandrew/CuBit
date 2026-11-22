with CuBit.Console_Route;
with CuBit.Log;
with CuBit.Monotonic;
with CuBit.Waits;
package body Intel_GPU_Diagnostics is
   use Interfaces;
   --  How often a stopping driver looks for logstore's introduction.
   Flush_Poll_Ms : constant := 10;

   procedure Capture (Text : String;
     Level : CuBit.Log_Records.Severity := CuBit.Log_Records.Information) is
   begin
      CuBit.Log.Write (Level, Text);
   end Capture;

   function Flush (Limit_Ms : Unsigned_64) return Boolean is
      Started : constant Unsigned_64 := CuBit.Monotonic.Milliseconds;
      Ignore : Unsigned_64;
   begin
      while CuBit.Console_Route.Awaiting
        and then CuBit.Monotonic.Milliseconds - Started < Limit_Ms
      loop
         Ignore := CuBit.Waits.Sleep_For (Flush_Poll_Ms);
      end loop;
      CuBit.Log.Flush;
      return CuBit.Console_Route.Routed and then not CuBit.Console_Route.Awaiting;
   end Flush;
end Intel_GPU_Diagnostics;
