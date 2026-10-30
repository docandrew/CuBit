pragma Ada_2022;
with Interfaces;
-- Shared service badge, minted only by the trusted launcher from declared use.
-- Classify only a kernel-stamped received-message authorityTag, never words/PID.
package ACPI_Protocol with Pure, SPARK_Mode is
   type Role_Tag is new Interfaces.Unsigned_64;
   Observer_Tag : constant Role_Tag := 16#4143_5049_4F42_5356#; -- ACPIOBSV
   function Is_Observer (Stamp : Role_Tag) return Boolean is
     (Stamp = Observer_Tag);
end ACPI_Protocol;
