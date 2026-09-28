with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Boot_Timer_Rates; use Boot_Timer_Rates;
procedure Main is
   type Inputs is array (Positive range <>) of Unsigned_32;
   Edges : constant Inputs := [0, 1, 2, 88, 999_999, 1_000_000,
     24_000_000, 38_400_000, 2_000_000_000, Unsigned_32'Last];
   Expected : Unsigned_64;
   Count : Natural := 0;
begin
   pragma Assert (From_CPUID (2, 88, 38_400_000) = 1_689_600_000);
   pragma Assert (From_CPUID (1, 1, 999_999) = 0);
   for D of Edges loop
      for N of Edges loop
         for C of Edges loop
            Expected := 0;
            if D /= 0 then
               Expected := Unsigned_64 (N) * Unsigned_64 (C) / Unsigned_64 (D);
               if Expected not in Valid_Frequency then Expected := 0; end if;
            end if;
            pragma Assert (From_CPUID (D, N, C) = Expected);
            Count := Count + 1;
         end loop;
      end loop;
   end loop;
   -- N95 crystal-fed /16 APIC example over 10ms: 24,000 counter ticks.
   pragma Assert (LAPIC_Per_Millisecond (24_000, 16_896_000, 1_689_600_000) = 2400);
   pragma Assert (LAPIC_Per_Millisecond (0, 10, 1_000_000) = 0);
   pragma Assert (LAPIC_Per_Millisecond (1, 0, 1_000_000) = 0);
   pragma Assert (LAPIC_Per_Millisecond (Unsigned_32'Last, 1, Maximum_Hz) = 0);
   pragma Assert (LAPIC_Per_Millisecond (Unsigned_32'Last, 100_000_000, Maximum_Hz)
     = Unsigned_32'Last);
   pragma Assert (LAPIC_Per_Millisecond (1, Unsigned_64'Last, Minimum_Hz) = 0);
   Put_Line ("PASS boot timer rates:" & Count'Image & " CPUID combinations; LAPIC bounds");
end Main;
