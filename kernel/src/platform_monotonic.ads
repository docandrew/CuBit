with Interfaces;
with System;
-- x86 implementation boundary. Common consumers use Time.Read_Monotonic.
package Platform_Monotonic with SPARK_Mode => Off is
   -- BSP only, before AP startup. Base names at least 16#500# mapped device
   -- bytes, aligned to eight bytes, under exclusive kernel ownership.
   procedure Initialize_HPET (Base : System.Address; Success : out Boolean);
   procedure Read (Microseconds : out Interfaces.Unsigned_64;
                   Success : out Boolean);
   function Startup_Diagnostic return String;
   function Diagnostic (Detail : Interfaces.Unsigned_64)
     return Interfaces.Unsigned_64;
   -- Read-only boot evidence: 0=startup code (0 uninitialized, 1 invalid
   -- base, otherwise 2+HPET_Clock.Startup_Status position), 1=capabilities,
   -- 2=period in femtoseconds; 3/4/5=last comparator offset/before/after.
   -- Unknown detail returns all ones. No physical or virtual addresses.
   -- Numeric microsecond resolution is NOT a certified physical error bound.
end Platform_Monotonic;
