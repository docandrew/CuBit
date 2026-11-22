package body Boot_Timer_Rates with SPARK_Mode is
   function From_CPUID (Denominator, Numerator, Crystal_Hz : Unsigned_32)
     return Frequency
   is
      Rate : Unsigned_64;
   begin
      if Denominator = 0 or Numerator = 0 or Crystal_Hz = 0 then
         return 0;
      end if;
      -- Two 32-bit factors always fit in 64 bits without modular wrap.
      Rate := Unsigned_64 (Crystal_Hz) * Unsigned_64 (Numerator) /
        Unsigned_64 (Denominator);
      if Rate not in Valid_Frequency then return 0; end if;
      return Rate;
   end From_CPUID;

   function LAPIC_Per_Millisecond
     (Countdown_Ticks : Unsigned_32; Elapsed_TSC : Unsigned_64;
      TSC_Hz : Valid_Frequency) return Unsigned_32
   is
      -- With the admitted frequency bound this product fits in 64 bits.
      Scaled : constant Unsigned_64 :=
        Unsigned_64 (Countdown_Ticks) * (TSC_Hz / 1000);
      Rate : Unsigned_64;
   begin
      if Elapsed_TSC = 0 then return 0; end if;
      Rate := Scaled / Elapsed_TSC;
      if Rate = 0 or Rate > Unsigned_64 (Unsigned_32'Last) then return 0; end if;
      return Unsigned_32 (Rate);
   end LAPIC_Per_Millisecond;
end Boot_Timer_Rates;
