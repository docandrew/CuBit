pragma Ada_2022;
with ACPI_EC_Boot_Table;
package ACPI_EC_Boot_Observations with SPARK_Mode, Pure is
   -- Local diagnostic policy, not an ACPI table-format or grant-size limit.
   type Inspection_Budget is range 1 .. 65_536;
   Default_Inspection_Bytes : constant Inspection_Budget := 4_096;
   type Catalog_Position is new Natural;
   type Byte_Extent is new Natural;
   type Match_Count is range 0 .. 2;
   type Outcome is (Catalog_Not_Ready, Missing_ECDT, Multiple_ECDT,
                    Inspection_Limit, Inspected);
   type Report is record
      Status : Outcome := Catalog_Not_Ready;
      Matches : Match_Count := 0;
      Table : Catalog_Position := 0;
      Required_Bytes : Byte_Extent := 0;
      Description : ACPI_EC_Boot_Table.Description;
      Path : ACPI_EC_Boot_Table.Path_Result;
      Registers : ACPI_EC_Boot_Table.Register_Status := ACPI_EC_Boot_Table.Invalid_Description;
   end record;
   -- Copied descriptions only. No namespace/UID association, reservation,
   -- Global Lock fact, mapped register, GPE routing or provider is established.
end ACPI_EC_Boot_Observations;
