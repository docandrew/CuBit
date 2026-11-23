pragma Ada_2022;
with Interfaces; use Interfaces;
with TextIO; use TextIO;
with PerCPUData;
with Time;
with x86;

package body Clock_Stall_Probe is

   -- The TSC value CPU 0 skips its clock work until (0: no stall).
   Until_Counter : Unsigned_64 := 0 with Atomic;
   -- CPU 0 announced the stall it is in.
   Announced : Boolean := False with Atomic;

   procedure Handle_Request
     (Caller : Process.ProcessID; Text : String; Handled : out Boolean)
   is
      pragma Unreferenced (Caller);
   begin
      Handled := Text = Stall_Request;
      if Handled then
         Announced := False;
         Until_Counter := x86.readOrderedTSC +
           Time.tscPerDuration * Stall_Ms * Time.Milliseconds;
      end if;
   end Handle_Request;

   function Skips_Clock_Work return Boolean is
      Limit : constant Unsigned_64 := Until_Counter;
   begin
      if Limit = 0 or else PerCPUData.getCPUNumber /= 0 then
         return False;
      elsif x86.readOrderedTSC < Limit then
         if not Announced then
            Announced := True;
            println ("CLOCK-PROBE: CPU 0 runs no timer work for" &
                     Natural'Image (Stall_Ms) & " ms");
         end if;
         return True;
      end if;
      Until_Counter := 0;
      println ("CLOCK-PROBE: CPU 0 runs its timer work again");
      return False;
   end Skips_Clock_Work;
end Clock_Stall_Probe;
