pragma Ada_2022;
with Interfaces;
with ACPI_SCI_Registers;
-- Pure geometry only. The caller must supply the retained SCI layout and
-- actual EC port identity, and independently establish provider ownership.
package ACPI_SCI_Exclusion with SPARK_Mode, Pure is
   type Outcome is (Disjoint, Missing_PM1a, Malformed_Block, Overlapping_Ports);
   function Check (Layout : ACPI_SCI_Registers.Layout;
      EC_Active : Boolean; EC_Control, EC_Data : Interfaces.Unsigned_64) return Outcome
      with Global => null;
end ACPI_SCI_Exclusion;
