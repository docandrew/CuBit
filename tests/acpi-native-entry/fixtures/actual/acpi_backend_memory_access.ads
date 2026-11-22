pragma Ada_2022;
with Interfaces;
with ACPI_FADT.Transactions;
-- Privileged backend only, one current borrowed binding view per call. Kernel
-- mapping attributes, physical device semantics and backing lifetime are trusted
-- integration obligations. This unit does not resolve a caller's physical address.
package ACPI_Backend_Memory_Access is
   procedure Transfer
     (Virtual : Interfaces.Unsigned_64; Width : ACPI_FADT.Transactions.Access_Width;
      For_Write : Boolean; Input : Interfaces.Unsigned_64;
      Output : out Interfaces.Unsigned_64; Completed : out Boolean);
end ACPI_Backend_Memory_Access;
