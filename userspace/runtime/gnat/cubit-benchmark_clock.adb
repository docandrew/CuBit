with System.Machine_Code; use System.Machine_Code;
with CuBit.Messages; use CuBit.Messages;

package body CuBit.Benchmark_Clock is
   function Read_Counter return Unsigned_64 is
      Low, High : Unsigned_32;
   begin
      Asm ("lfence; rdtsc; lfence",
           Outputs => (Unsigned_32'Asm_Output ("=a", Low),
                       Unsigned_32'Asm_Output ("=d", High)),
           Clobber => "memory", Volatile => True);
      return Shift_Left (Unsigned_64 (High), 32) or Unsigned_64 (Low);
   end Read_Counter;

   procedure Print_Value (Value : Unsigned_64) is
   begin
      debugPrint (Unsigned_64'Image (Value));
   end Print_Value;

   procedure Calibrate (Ticks_Per_Millisecond : out Unsigned_64) is
      Start_Ms, End_Ms, Start_Ticks, End_Ticks, Ignored : Unsigned_64;
      Minimum : Unsigned_64 := Unsigned_64'Last;
      Maximum : Unsigned_64 := 0;
      Rate : Unsigned_64;
   begin
      Ticks_Per_Millisecond := 0;
      for Attempt in 1 .. 3 loop
         Start_Ms := syscall (SYSCALL_GETTIME);
         Start_Ticks := Read_Counter;
         Ignored := syscall (SYSCALL_SLEEP, 200);
         End_Ticks := Read_Counter;
         End_Ms := syscall (SYSCALL_GETTIME);
         if End_Ms <= Start_Ms or else End_Ticks <= Start_Ticks then
            return;
         end if;
         Rate := (End_Ticks - Start_Ticks) / (End_Ms - Start_Ms);
         Minimum := Unsigned_64'Min (Minimum, Rate);
         Maximum := Unsigned_64'Max (Maximum, Rate);
      end loop;
      if Minimum > 0 and then Maximum - Minimum <= Minimum / 50 then
         Ticks_Per_Millisecond := Minimum + (Maximum - Minimum) / 2;
      end if;
      debugPrint
        ("TIMING: calibration ticks_per_ms=" &
         Unsigned_64'Image (Ticks_Per_Millisecond) &
         " min=" & Unsigned_64'Image (Minimum) &
         " max=" & Unsigned_64'Image (Maximum) & ASCII.LF);
   end Calibrate;

   procedure Report
     (Name : String; H : CuBit.Timing_Histograms.Histogram)
   is
      use CuBit.Timing_Histograms;
   begin
      debugPrint
        ("TIMING: " & Name & " count=" & Sample_Count'Image (Count (H)) &
         " min_ticks=" &
         Unsigned_64'Image (Minimum (H)) &
         " p50_le_ticks=" & Unsigned_64'Image (Quantile_Upper (H, 50)) &
         " p95_le_ticks=" & Unsigned_64'Image (Quantile_Upper (H, 95)) &
         " p99_le_ticks=" & Unsigned_64'Image (Quantile_Upper (H, 99)) &
         " max_ticks=" & Unsigned_64'Image (Maximum (H)) & ASCII.LF);
   end Report;
end CuBit.Benchmark_Clock;
