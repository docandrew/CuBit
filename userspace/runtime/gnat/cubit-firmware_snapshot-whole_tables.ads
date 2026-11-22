------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A whole firmware table from the sealed snapshot, by signature: the
--  caller sizes its buffer from the table's own extent (Find), then copies
--  every chunk of it (Copy). Nothing is cut: a table longer than
--  Maximum_Table_Bytes is Too_Large.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.ACPI_Fixed_Windows;

package CuBit.Firmware_Snapshot.Whole_Tables is

   subtype Byte_Array is CuBit.ACPI_Fixed_Windows.Byte_Array;

   --  The named ceiling on one table (1 MiB).
   Maximum_Table_Bytes : constant := CuBit.ACPI_Fixed_Windows.Maximum_Table_Bytes;
   subtype Table_Bytes is Natural range 0 .. Maximum_Table_Bytes;

   type Read_Status is (Found, Absent, Too_Large, Read_Failed);

   --  The first table named Signature (four characters): its number and
   --  whole extent.
   procedure Find
     (Tables : Authority; Signature : String;
      Table : out Table_ID; Bytes : out Table_Bytes; Status : out Read_Status)
   with Pre => Signature'Length = Signature_Length;

   --  All of Table into Into, whose length is the extent Find gave.
   procedure Copy
     (Tables : Authority; Table : Table_ID; Into : out Byte_Array; Status : out Read_Status)
   with Pre => Into'First = 1 and then Into'Length <= Maximum_Table_Bytes;

end CuBit.Firmware_Snapshot.Whole_Tables;
