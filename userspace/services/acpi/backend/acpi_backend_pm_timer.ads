pragma Ada_2022;
with Interfaces;
with ACPI_FADT.Registers;
with ACPI_Backend_Windows;
with ACPI_Region_Policy;
-- Trusted startup planner, not a public address-registration operation.
-- Firmware supplies geometry only. The catalog must still bind and retain
-- the actual enforcing window handle, authorize Stamp, Add and Seal.
package ACPI_Backend_PM_Timer with SPARK_Mode is
   package F renames ACPI_FADT;
   package R renames ACPI_FADT.Registers;
   package W renames ACPI_Backend_Windows;
   package P renames ACPI_Region_Policy;
   type Status is (Ready, Timer_Unavailable, Admission_Refused);
   type Result (Code : Status := Timer_Unavailable) is record
      Block_Status : R.Status := R.Absent;
      Admission : W.Outcome := W.Invalid_Record;
      case Code is
         when Ready =>
            Configuration : P.Configuration;
            Source : R.Address_Source;
            Extended_Rejected : Boolean;
         when others => null;
      end case;
   end record;
   -- PM_TMR is one read-only 32-bit transaction even on a 24-bit counter.
   -- GAS access-size does not select a different transaction here. Native
   -- alignment restrictions remain explicit; no outward rounding or fallback
   -- to another window/address after admission refusal is attempted.
   function Prepare
     (Item : F.Description; Window : W.Granted_Window;
      Stamp, Register_ID : Interfaces.Unsigned_64;
      Delegable : Boolean := False) return Result;
end ACPI_Backend_PM_Timer;
