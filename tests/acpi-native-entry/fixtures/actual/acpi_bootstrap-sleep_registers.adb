pragma Ada_2022;
package body ACPI_Bootstrap.Sleep_Registers with SPARK_Mode is
   procedure Observe (Session : State; Result : out ACPI_FADT.Sleep_Registers.Source_Report) is
      package P renames ACPI_FADT.Sleep_Registers;
   begin
      Result := (others => <>);
      -- Fixed-register descriptions depend on the immutable completed catalog,
      -- not on _REG/_INI or hardware-provider readiness. No AML or I/O runs here.
      if Session.Stage /= Complete then return; end if;
      for Index in 1 .. ACPI_Service.Table_Count (Session.Core) loop
         if ACPI_Service.Table_Info (Session.Core, Index).Signature = "FACP" then
            Result.Matches := Result.Matches + 1;
            if Result.Matches > 1 then Result.Status := P.Multiple_FADT; Result.Table := 0; return; end if;
            Result.Table := Index;
         end if;
      end loop;
      if Result.Matches = 0 then Result.Status := P.Missing_FADT; return; end if;
      declare
         Item : constant ACPI_FADT.Result := ACPI_Service.Fixed_Description (Session.Core, Result.Table);
      begin
         if not Item.Valid then Result.Status := P.Invalid_FADT; return; end if;
         Result.Data := P.Describe (Item.Value);
         Result.Status := P.Classified;
      end;
   end Observe;
end ACPI_Bootstrap.Sleep_Registers;
