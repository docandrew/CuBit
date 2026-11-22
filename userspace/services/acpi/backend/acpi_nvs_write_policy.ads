pragma Ada_2022;
with Interfaces;
with ACPI_FADT.Transactions;
package ACPI_NVS_Write_Policy with Pure, SPARK_Mode is
   subtype Word is Interfaces.Unsigned_64;
   function Permitted (Address : Word; Width : ACPI_FADT.Transactions.Access_Width;
      FACS_Address : Word) return Boolean;
   -- Bounds/authority/actual WB NVS class remain the retained Records owner's
   -- duty. Generic AML data writes cannot modify the physical FACS page,
   -- including an NVS-classified FACS page. Zero means no FACS was declared.
end ACPI_NVS_Write_Policy;
