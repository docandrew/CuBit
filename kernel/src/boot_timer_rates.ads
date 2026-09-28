with Interfaces; use Interfaces;

-- Arithmetic only. Hardware reports and rate stability remain trusted inputs.
package Boot_Timer_Rates with SPARK_Mode, Pure is
   Minimum_Hz : constant := 1_000_000;
   Maximum_Hz : constant := 100_000_000_000;
   subtype Frequency is Unsigned_64 range 0 .. Maximum_Hz;
   subtype Valid_Frequency is Frequency range Minimum_Hz .. Maximum_Hz;
   -- Zero means absent/unsupported. Do not guess a crystal from CPU model.
   function From_CPUID (Denominator, Numerator, Crystal_Hz : Unsigned_32)
     return Frequency
     with Post => From_CPUID'Result = 0 or else
                  From_CPUID'Result in Valid_Frequency;
   function LAPIC_Per_Millisecond
     (Countdown_Ticks : Unsigned_32; Elapsed_TSC : Unsigned_64;
      TSC_Hz : Valid_Frequency) return Unsigned_32;
end Boot_Timer_Rates;
