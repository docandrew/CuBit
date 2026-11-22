pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with PCI_Power_Control; use PCI_Power_Control;
--  PCI-BAR-001: finding the PM capability and the PMCSR write for D0.
procedure Main is
   Checks, Failures : Natural := 0;
   procedure Check (Ok : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Ok then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("PCI-POWER: FAIL " & What);
      end if;
   end Check;

   --  A type-0 function with a capability list: vendor, status bit 4.
   function Base return Configuration is
      C : Configuration := [others => 0];
   begin
      C (0) := 16#86#; C (1) := 16#80#;  --  Intel
      C (6) := 16#10#;                   --  capabilities list
      return C;
   end Base;

   C : Configuration;
   L : Location;
begin
   --  LPSS-like: MSI-X at 0x80 -> PM at 0x90 -> end.
   C := Base; C (16#34#) := 16#80#;
   C (16#80#) := 16#11#; C (16#81#) := 16#90#;
   C (16#90#) := 16#01#; C (16#91#) := 16#00#; C (16#92#) := 16#03#;
   L := Locate (C);
   Check (L.Status = Found and then L.Offset = 16#90#, "PM after MSI-X");

   C := Base; C (16#34#) := 16#00#;
   Check (Locate (C).Status = Absent, "empty list");
   C := Base; C (6) := 0;
   Check (Locate (C).Status = Absent, "no capability list");
   C := Base; C (0) := 16#FF#; C (1) := 16#FF#;
   Check (Locate (C).Status = Absent, "no function");
   C := Base; C (16#0E#) := 1;
   Check (Locate (C).Status = Malformed, "type-1 header refused");
   --  A cycle: 0x80 -> 0x90 -> 0x80.
   C := Base; C (16#34#) := 16#80#; C (16#80#) := 16#05#; C (16#81#) := 16#90#;
   C (16#90#) := 16#05#; C (16#91#) := 16#80#;
   Check (Locate (C).Status = Malformed, "cycle refused");
   --  Two PM capabilities.
   C := Base; C (16#34#) := 16#80#; C (16#80#) := 1; C (16#81#) := 16#90#;
   C (16#90#) := 1; C (16#91#) := 0;
   Check (Locate (C).Status = Malformed, "duplicate PM refused");
   --  Misaligned pointer, and one below 0x40.
   C := Base; C (16#34#) := 16#82#;
   Check (Locate (C).Status = Malformed, "misaligned pointer refused");
   C := Base; C (16#34#) := 16#20#;
   Check (Locate (C).Status = Malformed, "pointer into the header refused");
   --  PM at 0xFC cannot hold its PMCSR (offset + 4 past 0xFF).
   C := Base; C (16#34#) := 16#FC#; C (16#FC#) := 1; C (16#FD#) := 0;
   Check (Locate (C).Status = Malformed, "PM too high refused");

   --  PMCSR decisions.
   Check (State_Of (16#0003#) = D3_Hot, "D3hot decoded");
   Check (To_D0 (16#0003#) = 0, "D3hot to D0");
   Check (To_D0 (16#8103#) = 16#0100#, "PME_En kept, PME_Status not written");
   Check (To_D0 (16#000B#) = 16#0008#, "No_Soft_Reset bit (read-only) passes through");
   Check (Resets_On_D0 (16#0003#), "no No_Soft_Reset: resets on D0");
   Check (not Resets_On_D0 (16#000B#), "No_Soft_Reset set: keeps state");

   if Failures = 0 then
      Ada.Text_IO.Put_Line ("PCI-POWER: PASS checks=" & Checks'Image);
   else
      Ada.Text_IO.Put_Line ("PCI-POWER: FAIL" & Failures'Image & " of" & Checks'Image);
      raise Program_Error;
   end if;
end Main;
