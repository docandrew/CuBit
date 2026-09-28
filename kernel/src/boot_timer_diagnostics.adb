with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;
with Boot_Diagnostics;
with Boot_Panel;
with Boot_Timer_Probe;
with BuddyAllocator;
with CPUID;
with Mem_mgr;
with PIC;
with TextIO;
with Time;
with Timer_PIT;
with X86;
with Virtmem;

package body Boot_Timer_Diagnostics with SPARK_Mode => Off is
   function Hex (Value : Unsigned_64; Width : Positive := 8) return String is
      Alphabet : constant String := "0123456789ABCDEF";
      Result : String (1 .. Width);
      Rest : Unsigned_64 := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Alphabet (Natural (Rest and 15) + 1);
         Rest := Shift_Right (Rest, 4);
      end loop;
      return Result;
   end Hex;

   procedure Evidence (R : Boot_Panel.Evidence_Row; Text : String) is
   begin
      Boot_Diagnostics.Set_Evidence (R, Text);
      TextIO.println (Text);
   end Evidence;

   procedure Controller_Snapshot is
      APIC_Base : Unsigned_64;
      Physical : Virtmem.PhysAddress;
      Extended : Boolean;
      function Map_IO is new Mem_mgr.mapIOFrame (BuddyAllocator.allocFrame);
      -- Intel architectural register offsets; reads never acknowledge IRQs
      -- or change the controller. x2APIC exposes the same banks through MSRs.
      type Register is (Priority, Spurious, Timer_Vector, Legacy_Input);
      for Register use (Priority => 16#A0#, Spurious => 16#F0#,
                        Timer_Vector => 16#320#, Legacy_Input => 16#350#);
      ISR_Bank : constant Unsigned_32 := 16#100#;
      IRR_Bank : constant Unsigned_32 := 16#200#;

      function Read_Offset (Offset : Unsigned_32) return Unsigned_32 is
      begin
         if Extended then
            return Unsigned_32 (X86.rdmsr
              (X86.MSR (16#800# + Offset / 16)) and 16#FFFF_FFFF#);
         else
            declare
               Value : Unsigned_32 with Import, Volatile_Full_Access,
                 Address => Virtmem.P2Va (Physical) + Storage_Offset (Offset);
            begin return Value; end;
         end if;
      end Read_Offset;
      function Read (R : Register) return String is
        (Hex (Unsigned_64 (Read_Offset (Register'Enum_Rep (R)))));
      function Highest (Bank : Unsigned_32) return Unsigned_32 is
         Bits : Unsigned_32;
      begin
         for Word in reverse 0 .. 7 loop
            Bits := Read_Offset (Bank + Unsigned_32 (Word * 16));
            for Bit in reverse 0 .. 31 loop
               if (Bits and Shift_Left (Unsigned_32'(1), Bit)) /= 0 then
                  return Unsigned_32 (Word * 32 + Bit);
               end if;
            end loop;
         end loop;
         return Unsigned_32'Last;
      end Highest;
   begin
      if not CPUID.leaf1edx.hasAPIC then
         Evidence (Boot_Panel.Controller_Evidence, "APIC not advertised");
         return;
      end if;
      APIC_Base := X86.rdmsr (X86.MSRs.APIC_BASE);
      if (APIC_Base and X86.MSRs.APIC_BASE_FLAG_GLOBAL) = 0 then
         Evidence (Boot_Panel.Controller_Evidence, "APIC globally disabled");
         return;
      end if;
      Extended := (APIC_Base and X86.MSRs.APIC_BASE_FLAG_X2APIC) /= 0;
      Physical := Virtmem.PhysAddress (APIC_Base and X86.MSRs.APIC_BASE_MASK);
      if not Extended and then not Map_IO (Physical) then
         Evidence (Boot_Panel.Controller_Evidence, "APIC diagnostic mapping unavailable");
         return;
      end if;
      Evidence (Boot_Panel.Controller_Evidence,
        (if Extended then "x2 " else "x1 ") &
        "SVR=" & Read (Spurious) & " LINT0=" & Read (Legacy_Input) &
        " LVTT=" & Read (Timer_Vector) & " ISR=" & Hex (Unsigned_64 (Highest (ISR_Bank))) &
        " IRR=" & Hex (Unsigned_64 (Highest (IRR_Bank))) & " PPR=" & Read (Priority));
   end Controller_Snapshot;

   function Status return Unsigned_8 is
      Value : Unsigned_8;
      Latch_Channel_Zero_Status : constant Unsigned_8 := 16#E2#;
   begin
      X86.out8 (Timer_PIT.TimerModeReg, Latch_Channel_Zero_Status);
      X86.in8 (Timer_PIT.Timer0, Value);
      return Value;
   end Status;
   function Counter return Unsigned_16 is
      Low, High : Unsigned_8;
   begin
      --  Channel zero count latch: coherent two-byte sample without
      --  reprogramming the timer or relying on interrupt delivery.
      X86.out8 (Timer_PIT.TimerModeReg, 0);
      X86.in8 (Timer_PIT.Timer0, Low);
      X86.in8 (Timer_PIT.Timer0, High);
      return Unsigned_16 (Low) or Shift_Left (Unsigned_16 (High), 8);
   end Counter;

   procedure Wait_For_Ticks (Count : Unsigned_64) is
      --  Work/cycle budgets, NOT calibrated wall-clock deadlines. Either can
      --  stop the wait independently; no functioning timer is assumed.
      Poll_Budget : constant := 100_000_000;
      Cycle_Budget : constant Unsigned_64 := 20_000_000_000;
      Sample_Interval : constant := 65_536;
      Start_Ticks : constant Unsigned_64 := Time.msTicks;
      Start_TSC : constant Unsigned_64 := X86.rdtsc;
      First_Count : constant Unsigned_16 := Counter;
      Last_Count : Unsigned_16 := First_Count;
      Minimum, Maximum : Unsigned_16 := First_Count;
      First_Status : constant Unsigned_8 := Status;
      Last_Status : Unsigned_8;
      Previous_Ticks : Unsigned_64 := Start_Ticks;
      Observed_Ticks : Unsigned_64;
      First_Tick, Last_Tick, Span : Unsigned_64 := 0;
      Cycle_Limited : Boolean := False;
      Denominator, Numerator, Crystal_Hz, Unused : Unsigned_32 := 0;
      Moved : Boolean := False;
      Enabled : Boolean;
      Mask, Pending, In_Service : Unsigned_8;
      Delivered : Unsigned_64;
      Interrupts_Were_Enabled : constant Boolean := X86.getFlags.interrupt;
   begin
      X86.cli;
      Boot_Timer_Probe.Start;
      if Interrupts_Were_Enabled then X86.sti; end if;
      for Poll in 1 .. Poll_Budget loop
         Observed_Ticks := Time.msTicks;
         if Observed_Ticks - Start_Ticks >= Count then
            X86.cli;
            Boot_Timer_Probe.Stop;
            if Interrupts_Were_Enabled then X86.sti; end if;
            return;
         end if;
         if Observed_Ticks /= Previous_Ticks then
            Last_Tick := X86.rdtsc - Start_TSC;
            if Previous_Ticks = Start_Ticks then First_Tick := Last_Tick; end if;
            Previous_Ticks := Observed_Ticks;
         end if;
         if Poll mod Sample_Interval = 0 then
            Last_Count := Counter;
            Minimum := Unsigned_16'Min (Minimum, Last_Count);
            Maximum := Unsigned_16'Max (Maximum, Last_Count);
            Moved := Moved or Last_Count /= First_Count;
            Cycle_Limited := X86.rdtsc - Start_TSC >= Cycle_Budget;
            exit when Cycle_Limited;
         end if;
         System.Machine_Code.Asm ("pause", Volatile => True);
      end loop;

      Enabled := X86.getFlags.interrupt;
      X86.cli;
      Boot_Timer_Probe.Stop;
      Span := X86.rdtsc - Start_TSC;
      Delivered := Time.msTicks - Start_Ticks;
      Last_Count := Counter;
      Moved := Moved or Last_Count /= First_Count;
      X86.in8 (PIC.PIC1Data, Mask);
      X86.out8 (PIC.PIC1Command, 16#0A#); -- OCW3: read pending requests
      X86.in8 (PIC.PIC1Command, Pending);
      X86.out8 (PIC.PIC1Command, 16#0B#); -- OCW3: read in-service requests
      X86.in8 (PIC.PIC1Command, In_Service);
      X86.out8 (PIC.PIC1Command, 16#0A#); -- leave conventional read selection

      Last_Status := Status;
      if CPUID.getMaxStandardFunction >= 16#15# then
         CPUID.cpuid (16#15#, Denominator, Numerator, Crystal_Hz, Unused);
      end if;
      Evidence (Boot_Panel.Timing_Evidence,
        "span=" & Hex (Span, 16) & " first=" & Hex (First_Tick, 16) &
        " last=" & Hex (Last_Tick, 16) &
        (if Cycle_Limited then " budget=cycles" else " budget=polls"));
      Evidence (Boot_Panel.Timer_Evidence,
        "status=" & Hex (Unsigned_64 (First_Status), 2) & "/" &
        Hex (Unsigned_64 (Last_Status), 2) & " min=" & Hex (Unsigned_64 (Minimum), 4) &
        " max=" & Hex (Unsigned_64 (Maximum), 4) & " CPUID15 D=" &
        Hex (Unsigned_64 (Denominator)) & " N=" & Hex (Unsigned_64 (Numerator)) &
        " Hz=" & Hex (Unsigned_64 (Crystal_Hz)));
      Controller_Snapshot;
      Evidence (Boot_Panel.IRQ_Timing_Evidence,
        "gap-min=" & Hex (Boot_Timer_Probe.Minimum_Gap, 16) &
        " gap-max=" & Hex (Boot_Timer_Probe.Maximum_Gap, 16) &
        " handler-max=" & Hex (Boot_Timer_Probe.Maximum_Handler, 16));
      Evidence (Boot_Panel.IRQ_Source_Evidence,
        "entries=" & Hex (Boot_Timer_Probe.Entries) &
        " returned=" & Hex (Boot_Timer_Probe.Completed) &
        " pic0=" & Hex (Boot_Timer_Probe.PIC_IRQ0) &
        " isr-or=" & Hex (Unsigned_64 (Boot_Timer_Probe.ISR_Bits), 2));

      TextIO.print ("PIT counter first="); TextIO.print (First_Count);
      TextIO.print (" last="); TextIO.println (Last_Count);
      --  Fits the 96-column panel. Panic freezes this last completed line,
      --  so it remains readable even without a serial console.
      TextIO.print ("PIT moved="); TextIO.print ((if Moved then 'Y' else 'N'));
      TextIO.print (" ticks="); TextIO.print (Delivered);
      TextIO.print (" mask="); TextIO.print (Mask);
      TextIO.print (" irr="); TextIO.print (Pending);
      TextIO.print (" isr="); TextIO.print (In_Service);
      TextIO.print (" IF="); TextIO.print ((if Enabled then '1' else '0'));
      TextIO.println;

      -- Keep the essential probe readings near the top as well. This final
      -- complete line is retained in Latest Diagnostic on the failure panel.
      TextIO.println
        ("IRQ2 n=" & Hex (Boot_Timer_Probe.Entries) &
         " ret=" & Hex (Boot_Timer_Probe.Completed) &
         " pic=" & Hex (Boot_Timer_Probe.PIC_IRQ0) &
         " h=" & Hex (Boot_Timer_Probe.Maximum_Handler, 16) &
         " gap=" & Hex (Boot_Timer_Probe.Maximum_Gap, 16));

      if not Enabled then
         raise Program_Error with "PIT calibration stalled: CPU interrupts disabled";
      elsif (Mask and 1) /= 0 then
         raise Program_Error with "PIT calibration stalled: IRQ0 masked";
      elsif Delivered /= 0 then
         raise Program_Error with "PIT calibration stalled: incomplete tick delivery";
      elsif (Pending and 1) /= 0 then
         raise Program_Error with "PIT calibration stalled: IRQ0 pending, not delivered";
      elsif Moved then
         raise Program_Error with "PIT calibration stalled: counter moves, no IRQ0 delivery";
      else
         raise Program_Error with "PIT calibration stalled: no counter movement or IRQ0 delivery";
      end if;
   end Wait_For_Ticks;
end Boot_Timer_Diagnostics;
