with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with ACPI;
with Boot_Diagnostics;
with Boot_Panel;
with BuddyAllocator;
with CPUID;
with Firmware_Tables.HPET;
with InterruptNumbers;
with Mem_mgr;
with TextIO;
with Virtmem;
with X86;
with Platform_Monotonic;

package body Boot_Timer_Setup with SPARK_Mode => Off is
   function Map_IO is new Mem_mgr.mapIOFrame (BuddyAllocator.allocFrame);
   HPET_Before, HPET_After, Comparator_Zero : Unsigned_32 := 0;
   LAPIC_Before, LAPIC_After : Unsigned_32 := 0;
   HPET_Present, LAPIC_Present : Boolean := False;

   function Hex (V : Unsigned_32) return String is
      Alphabet : constant String := "0123456789ABCDEF";
      S : String (1 .. 8);
      Rest : Unsigned_32 := V;
   begin
      for I in reverse S'Range loop
         S (I) := Alphabet (Natural (Rest and 15) + 1);
         Rest := Shift_Right (Rest, 4);
      end loop;
      return S;
   end Hex;

   procedure Report is
      Line : constant String :=
        (if HPET_Present then "HPET=" & Hex (HPET_Before) & ">" & Hex (HPET_After) &
          " T0=" & Hex (Comparator_Zero) else "HPET absent") &
        (if LAPIC_Present then " LVTT=" & Hex (LAPIC_Before) & ">" & Hex (LAPIC_After)
         else " LVTT not sampled");
   begin
      Boot_Diagnostics.Set_Evidence (Boot_Panel.Firmware_Evidence, Line);
      TextIO.println (Line);
   end Report;

   -- Separate non-inlined boundary permits QEMU/GDB to inject inherited HPET
   -- hardware state AFTER mapping but BEFORE any observation or repair.
   procedure Quiesce_HPET (Base : Address) with No_Inline;
   procedure Quiesce_HPET (Base : Address) is
      Capabilities : Unsigned_32 with Import, Volatile_Full_Access, Address => Base;
      Period_FS : Unsigned_32 with Import, Volatile_Full_Access, Address => Base + 4;
      Configuration : Unsigned_32 with Import, Volatile_Full_Access, Address => Base + 16#10#;
      Timer_Zero : Unsigned_32 with Import, Volatile_Full_Access, Address => Base + 16#100#;
      ID : constant Unsigned_32 := Capabilities;
      Period : constant Unsigned_32 := Period_FS;
   begin
      if (ID and 255) = 0 or else ID = Unsigned_32'Last or else
        Period = 0 or else Period > 100_000_000
      then raise Program_Error with "HPET hardware identity invalid"; end if;
      HPET_Present := True;
      HPET_Before := Configuration;
      Comparator_Zero := Timer_Zero;
      Configuration := Firmware_Tables.HPET.Quiescent_Config (HPET_Before);
      HPET_After := Configuration;
      Report;
      if HPET_After /= Firmware_Tables.HPET.Quiescent_Config (HPET_Before) then
         raise Program_Error with "HPET firmware timer takeover failed";
      end if;
   end Quiesce_HPET;

   procedure Quiesce_LAPIC is
      APIC_Base : Unsigned_64;
      Physical : Virtmem.PhysAddress;
      Extended : Boolean;
      Timer_Offset : constant := 16#320#;
      Initial_Count_Offset : constant := 16#380#;
      Masked : constant Unsigned_32 := 16#1_0000#;
      function Read (Offset : Natural) return Unsigned_32 is
      begin
         if Extended then
            return Unsigned_32 (X86.rdmsr (X86.MSR (16#800# + Offset / 16)) and 16#FFFF_FFFF#);
         else
            declare
               V : Unsigned_32 with Import, Volatile_Full_Access,
                 Address => Virtmem.P2Va (Physical) + Storage_Offset (Offset);
            begin return V; end;
         end if;
      end Read;
      procedure Write (Offset : Natural; Value : Unsigned_32) is
      begin
         if Extended then
            X86.wrmsr (X86.MSR (16#800# + Offset / 16), Unsigned_64 (Value));
         else
            declare
               V : Unsigned_32 with Import, Volatile_Full_Access,
                 Address => Virtmem.P2Va (Physical) + Storage_Offset (Offset);
            begin V := Value; end;
         end if;
      end Write;
   begin
      if not CPUID.leaf1edx.hasAPIC then return; end if;
      APIC_Base := X86.rdmsr (X86.MSRs.APIC_BASE);
      if (APIC_Base and X86.MSRs.APIC_BASE_FLAG_GLOBAL) = 0 then return; end if;
      Extended := (APIC_Base and X86.MSRs.APIC_BASE_FLAG_X2APIC) /= 0;
      Physical := Virtmem.PhysAddress (APIC_Base and X86.MSRs.APIC_BASE_MASK);
      if not Extended and then not Map_IO (Physical) then
         raise Program_Error with "Cannot map firmware LAPIC timer";
      end if;
      LAPIC_Present := True;
      LAPIC_Before := Read (Timer_Offset);
      -- Use a legal vector while masking. Preserve the timer mode and all
      -- unrelated fields; do not send speculative EOIs or alter LINT routing.
      Write (Timer_Offset, (LAPIC_Before and not Unsigned_32'(255)) or
             Masked or Unsigned_32 (InterruptNumbers.TIMER));
      Write (Initial_Count_Offset, 0);
      LAPIC_After := Read (Timer_Offset);
      Report;
      if (LAPIC_After and Masked) = 0 then
         raise Program_Error with "LAPIC firmware timer masking failed";
      end if;
   end Quiesce_LAPIC;

   procedure Take_Over is
      Counter_Ready : Boolean := False;
      Page : constant Virtmem.PhysAddress := ACPI.hpetAddr and not Virtmem.PhysAddress'(4095);
      Counter_Mapped : Boolean := False;
   begin
      X86.cli;
      if ACPI.hpetAddr /= 0 then
         if not Map_IO (ACPI.hpetAddr and not Virtmem.PhysAddress'(4095)) then
            raise Program_Error with "Cannot map firmware HPET timer";
         end if;
         Quiesce_HPET (Virtmem.P2Va (ACPI.hpetAddr));
         -- Up to 32 comparator configurations span 16#500# bytes. ACPI
         -- permits 1KiB alignment, so the counter adapter can need two pages.
         Counter_Mapped := True;
         if ACPI.hpetAddr - Page > 4096 - 16#500# then
            Counter_Mapped := Page <= Virtmem.PhysAddress'Last - 4096;
            if Counter_Mapped then Counter_Mapped := Map_IO (Page + 4096); end if;
         end if;
      end if;
      Quiesce_LAPIC;
      Report;
      if Counter_Mapped then
         Platform_Monotonic.Initialize_HPET
           (Virtmem.P2Va (ACPI.hpetAddr), Counter_Ready);
      end if;
      if Counter_Ready then
         TextIO.println ("monotonic: HPET counter-only ready (scheduler unchanged)");
      else
         TextIO.println ("monotonic: high-resolution backend unavailable");
      end if;
      TextIO.println ("monotonic: HPET startup=" & Platform_Monotonic.Startup_Diagnostic);
   end Take_Over;
end Boot_Timer_Setup;
