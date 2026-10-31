pragma Ada_2022;
with Interfaces;
with CuBit.Handles;
with ACPI_FADT.Transactions;
package ACPI_Backend_Port_Access is
   procedure Transfer
     (Authority : CuBit.Handles.IO_Ports; Port : Interfaces.Unsigned_16;
      Width : ACPI_FADT.Transactions.Access_Width; For_Write : Boolean;
      Input : Interfaces.Unsigned_64; Output : out Interfaces.Unsigned_64; Completed : out Boolean);
end ACPI_Backend_Port_Access;
