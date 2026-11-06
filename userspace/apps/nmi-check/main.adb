with Interfaces; use Interfaces;
with System.Machine_Code; use System.Machine_Code;
with CuBit.Messages; use CuBit.Messages;

--  NMI survival check (SEC-023): spin in user mode while the headless test
--  injects NMIs through the QEMU monitor. The kernel's NMI path must not
--  change the page tables (CR3) or the GS base it returns with: an NMI that
--  landed here and came back with the kernel-only page tables would leave
--  this process without its mappings, and it would be killed for a fault it
--  never caused. The headless test nmi looks for the kernel's NMI line and
--  for this program finishing every round.
procedure Main is
   use ASCII;
   --  Rounds reported, and the timestamp-counter ticks each round spins:
   --  long enough for the injected NMIs to land while this process runs.
   Rounds          : constant := 20;
   Ticks_Per_Round : constant Unsigned_64 := 400_000_000;

   function Read_TSC return Unsigned_64 is
      Low, High : Unsigned_32;
   begin
      Asm ("rdtsc", Outputs => (Unsigned_32'Asm_Output ("=a", Low),
                                Unsigned_32'Asm_Output ("=d", High)),
           Volatile => True);
      return Shift_Left (Unsigned_64 (High), 32) or Unsigned_64 (Low);
   end Read_TSC;

   Sum : Unsigned_64 := 0 with Volatile;
begin
   debugPrint ("nmi-check: spinning" & LF);
   for Round in 1 .. Rounds loop
      declare
         Start : constant Unsigned_64 := Read_TSC;
      begin
         while Read_TSC - Start < Ticks_Per_Round loop
            Sum := Sum + 1;
         end loop;
      end;
      debugPrint ("nmi-check: round" & Integer'Image (Round) & LF);
   end loop;
   debugPrint ("nmi-check: done" & LF);
end Main;
