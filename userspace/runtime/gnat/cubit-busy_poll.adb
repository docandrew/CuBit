with System.Machine_Code; use System.Machine_Code;
with CuBit.Messages; use CuBit.Messages;

package body CuBit.Busy_Poll is

   Ticks_Per_Microsecond : Unsigned_64 := 1;
   Calibration_Ms        : constant := 10;
   Microseconds_Per_Ms   : constant := 1_000;

   function Now return Unsigned_64 is
      Low, High : Unsigned_32;
   begin
      Asm ("rdtsc",
           Outputs  => (Unsigned_32'Asm_Output ("=a", Low),
                        Unsigned_32'Asm_Output ("=d", High)),
           Volatile => True);
      return Shift_Left (Unsigned_64 (High), 32) or Unsigned_64 (Low);
   end Now;

   --  Count TSC ticks across Calibration_Ms whole milliseconds, starting
   --  at a millisecond boundary.
   procedure Calibrate is
      Start_Ms, Current : Unsigned_64;
      Start_Ticks : Unsigned_64;
   begin
      Start_Ms := syscall (SYSCALL_GETTIME);
      loop
         Current := syscall (SYSCALL_GETTIME);
         exit when Current /= Start_Ms;
      end loop;
      Start_Ms := Current;
      Start_Ticks := Now;
      loop
         Current := syscall (SYSCALL_GETTIME);
         exit when Current >= Start_Ms + Calibration_Ms;
      end loop;
      Ticks_Per_Microsecond := Unsigned_64'Max
        (1, (Now - Start_Ticks) /
            ((Current - Start_Ms) * Microseconds_Per_Ms));
   end Calibrate;

   function Within (Since, Micros : Unsigned_64) return Boolean is
     (Now - Since < Micros * Ticks_Per_Microsecond);

   procedure Relax is
   begin
      Asm ("pause", Volatile => True);
   end Relax;

end CuBit.Busy_Poll;
