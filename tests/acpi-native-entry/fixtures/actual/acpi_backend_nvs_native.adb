pragma Ada_2022;
with ACPI_Backend_Memory_Access;
package body ACPI_Backend_NVS_Native is
   procedure Transfer
     (Virtual : Interfaces.Unsigned_64; Width : ACPI_FADT.Transactions.Access_Width;
      Value : out Interfaces.Unsigned_64; Completed : out Boolean) is
   begin
      ACPI_Backend_Memory_Access.Transfer (Virtual, Width, False, 0, Value, Completed);
   end Transfer;
end ACPI_Backend_NVS_Native;
