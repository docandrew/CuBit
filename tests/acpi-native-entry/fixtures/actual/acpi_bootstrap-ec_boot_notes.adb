pragma Ada_2022;
with ACPI_EC_Boot_Table;
with Firmware_Tables;
package body ACPI_Bootstrap.EC_Boot_Notes with SPARK_Mode is
   procedure Observe (Session : State;
      Result : out ACPI_EC_Boot_Observations.Report;
      Limit : ACPI_EC_Boot_Observations.Inspection_Budget :=
        ACPI_EC_Boot_Observations.Default_Inspection_Bytes) is
      package O renames ACPI_EC_Boot_Observations;
      use type O.Match_Count;
   begin
      Result := (others => <>);
      if Session.Stage /= Complete then return; end if;
      for Index in 1 .. ACPI_Service.Table_Count (Session.Core) loop
         if ACPI_Service.Table_Info (Session.Core, Index).Signature = "ECDT" then
            Result.Matches := Result.Matches + 1;
            if Result.Matches = O.Match_Count'Last then
               Result.Status := O.Multiple_ECDT;
               Result.Table := 0;
               return;
            end if;
            Result.Table := O.Catalog_Position (Index);
         end if;
      end loop;
      if Result.Matches = 0 then Result.Status := O.Missing_ECDT; return; end if;
      declare
         Index : constant Positive := Positive (Result.Table);
         Extent : constant Natural := ACPI_Service.Table_Info (Session.Core, Index).Extent;
      begin
         Result.Required_Bytes := O.Byte_Extent (Extent);
         if Extent > Natural (Limit) then
            Result.Status := O.Inspection_Limit;
            return;
         end if;
         declare
            Data : Firmware_Tables.Bytes (1 .. Extent);
         begin
            for Offset in Data'Range loop
               Data (Offset) := ACPI_Service.Table_Byte (Session.Core, Index, Offset - Data'First);
            end loop;
            Result.Description := ACPI_EC_Boot_Table.Decode (Data);
            Result.Path := ACPI_EC_Boot_Table.Read_Path (Data);
            Result.Registers := ACPI_EC_Boot_Table.Check_Registers (Data);
            Result.Status := O.Inspected;
         end;
      end;
   end Observe;
end ACPI_Bootstrap.EC_Boot_Notes;
