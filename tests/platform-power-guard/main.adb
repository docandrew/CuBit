pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Tables.Power_Registers;
with Platform_Power_Port_Guard; use Platform_Power_Port_Guard;
--  KERN-004: user port writes that would start a sleep or a reset are refused;
--  every other write, other bits of the same registers included, passes.
procedure Main is
   package PR renames Firmware_Tables.Power_Registers;
   Checks, Failures : Natural := 0;
   procedure Check (Ok : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Ok then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("POWER-GUARD: FAIL " & What);
      end if;
   end Check;

   function IO (Address : Unsigned_64; Bytes : PR.Register_Bytes) return PR.Register is
     (Present => True, Space => PR.System_IO, Address => Address, Bytes => Bytes);

   --  An ordinary PC: PM1a control at 0x1804, reset at 0xCF9, SMI_CMD at
   --  0xB2 (a separate port).
   PC : constant PR.Description :=
     (Model => PR.PM1A_Only, PM1A_Control => IO (16#1804#, 2), PM1B_Control => PR.No_Register,
      Sleep_Control => PR.No_Register, Reset => IO (16#CF9#, 1), Reset_Value => 6,
      SMI_Command => 16#B2#);
   --  The IdeaPad Slim 3i's real FADT (captured facp.dat): PM1a at 0x1804,
   --  and the reset register IS the SMI command port: 0xFB to 0xB2.
   IP : constant PR.Description :=
     (Model => PR.PM1A_Only, PM1A_Control => IO (16#1804#, 2), PM1B_Control => PR.No_Register,
      Sleep_Control => PR.No_Register, Reset => IO (16#B2#, 1), Reset_Value => 16#FB#,
      SMI_Command => 16#B2#);
   --  A PM1a + PM1b machine.
   AB : constant PR.Description :=
     (Model => PR.PM1A_And_B, PM1A_Control => IO (16#404#, 2), PM1B_Control => IO (16#408#, 2),
      Sleep_Control => PR.No_Register, Reset => PR.No_Register, Reset_Value => 0,
      SMI_Command => 0);
   --  Hardware-reduced: a byte sleep control register.
   HR : constant PR.Description :=
     (Model => PR.Hardware_Reduced, PM1A_Control => PR.No_Register, PM1B_Control => PR.No_Register,
      Sleep_Control => IO (16#610#, 1), Reset => PR.No_Register, Reset_Value => 0,
      SMI_Command => 0);
   --  Memory-mapped registers are not judged on the port path.
   MM : constant PR.Description :=
     (Model => PR.PM1A_Only,
      PM1A_Control => (Present => True, Space => PR.System_Memory, Address => 16#FE00_1804#, Bytes => 2),
      PM1B_Control => PR.No_Register, Sleep_Control => PR.No_Register, Reset => PR.No_Register,
      Reset_Value => 0, SMI_Command => 0);
begin
   Check (PR.Consistent (PC) and PR.Consistent (AB) and PR.Consistent (HR) and PR.Consistent (IP),
          "fixtures consistent");
   --  Reset through a software SMI (the IdeaPad): only the reset command is
   --  refused at the SMI port; ACPI enable (0xA0) and other SMI commands pass.
   Check (Refuses (IP, 16#B2#, 1, 16#FB#), "IdeaPad: reset SMI 0xFB to 0xB2 refused");
   Check (not Refuses (IP, 16#B2#, 1, 16#A0#), "IdeaPad: ACPI-enable SMI 0xA0 to 0xB2 passes");
   Check (not Refuses (IP, 16#B2#, 1, 16#A1#), "IdeaPad: ACPI-disable SMI 0xA1 passes");
   Check (Refuses (IP, 16#B1#, 2, 16#FB00#), "IdeaPad: 16-bit write putting 0xFB at 0xB2 refused");
   Check (not Refuses (IP, 16#B1#, 2, 16#A000#), "IdeaPad: 16-bit write putting 0xA0 at 0xB2 passes");
   Check (Refuses (IP, 16#1804#, 2, 16#3C01#), "IdeaPad: S5 with SLP_EN still refused");
   --  An ordinary PC: SMI_CMD is its own port, never the reset register.
   Check (not Refuses (PC, 16#B2#, 1, 16#FB#), "PC: 0xFB to its separate SMI port passes");
   --  PM1a: SLP_EN (bit 13) at any width and offset.
   Check (Refuses (PC, 16#1804#, 2, 16#3C01#), "16-bit S5 with SLP_EN refused");
   Check (not Refuses (PC, 16#1804#, 2, 16#1C01#), "16-bit SLP_TYP without SLP_EN passes");
   Check (not Refuses (PC, 16#1804#, 2, 16#0001#), "SCI_EN write passes");
   Check (Refuses (PC, 16#1805#, 1, 16#20#), "8-bit SLP_EN at the high byte refused");
   Check (not Refuses (PC, 16#1805#, 1, 16#1C#), "8-bit SLP_TYP at the high byte passes");
   Check (not Refuses (PC, 16#1804#, 1, 16#FF#), "8-bit low byte passes (no SLP_EN there)");
   Check (Refuses (PC, 16#1804#, 4, 16#0000_2001#), "32-bit write covering SLP_EN refused");
   Check (Refuses (PC, 16#1802#, 4, 16#2000_0000#), "32-bit write from below reaching SLP_EN refused");
   Check (not Refuses (PC, 16#1802#, 4, 16#1C00_FFFF#), "32-bit write from below, SLP_EN clear, passes");
   --  Reset register.
   Check (Refuses (PC, 16#CF9#, 1, 6), "reset value at 0xCF9 refused");
   Check (Refuses (PC, 16#CF9#, 1, 0), "any byte at the reset register refused");
   Check (Refuses (PC, 16#CF8#, 2, 16#0600#), "16-bit write reaching 0xCF9 refused");
   Check (not Refuses (PC, 16#CF8#, 4, 16#8000_0000#), "dword CONFIG_ADDRESS at 0xCF8 passes");
   Check (not Refuses (PC, 16#CFC#, 4, 16#FFFF_FFFF#), "CONFIG_DATA passes");
   Check (not Refuses (PC, 16#CF8#, 1, 0), "byte at 0xCF8 passes");
   --  PM1b.
   Check (Refuses (AB, 16#409#, 1, 16#20#), "PM1b SLP_EN refused");
   Check (Refuses (AB, 16#405#, 1, 16#20#), "PM1a SLP_EN refused on an A+B machine");
   Check (not Refuses (AB, 16#408#, 2, 16#1C00#), "PM1b SLP_TYP passes");
   --  Hardware-reduced.
   Check (Refuses (HR, 16#610#, 1, 16#3C#), "reduced SLP_EN refused");
   Check (not Refuses (HR, 16#610#, 1, 16#1C#), "reduced SLP_TYP passes");
   Check (Refuses (HR, 16#60F#, 2, 16#2000#), "16-bit write reaching the reduced register's SLP_EN refused");
   --  Elsewhere, memory-mapped, and wrapping.
   Check (not Refuses (PC, 16#60#, 1, 16#FF#), "unrelated port passes");
   Check (not Refuses (MM, 16#1805#, 1, 16#20#), "memory-mapped registers are not port-judged");
   Check (Refuses (PC, 16#FFFE#, 4, 0), "write wrapping past 0xFFFF refused");
   Check (not Refuses (PR.No_Description, 16#CF9#, 1, 6), "no FADT: nothing to guard");
   if Failures = 0 then
      Ada.Text_IO.Put_Line ("POWER-GUARD: PASS checks=" & Checks'Image);
   else
      Ada.Text_IO.Put_Line ("POWER-GUARD: FAIL" & Failures'Image & " of" & Checks'Image);
      raise Program_Error;
   end if;
end Main;
