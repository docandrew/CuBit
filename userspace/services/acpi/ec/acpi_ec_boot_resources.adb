pragma Ada_2022;
package body ACPI_EC_Boot_Resources with SPARK_Mode is
   package B renames ACPI_EC_Boot_Table;
   package R renames ACPI_EC_Resources;
   function Compare (Boot, Resources : Firmware_Tables.Bytes) return Report is
      Table : constant B.Description := B.Decode (Boot);
      Claims : constant R.Result := R.Describe (Resources);
      Result : Report;
   begin
      Result.Boot_Status := Table.Status;
      Result.Namespace_Status := Claims.Outcome;
      if Table.Status /= B.Decoded then return Result; end if;
      Result.Boot_Registers := B.Check_Registers (Boot);
      Result.Status := Unsupported_Boot_Registers;
      if Result.Boot_Registers /= B.Byte_Registers_Described then return Result; end if;
      Result.Status := Invalid_Namespace_Resources;
      if Claims.Outcome /= R.Described then return Result; end if;
      --  Validate both role/address pairs before reviewing extra namespace
      --  semantics,so review outcomes cannot conceal a mismatched second role.
      for Role in R.Register_Role loop
         Result.Register_Examined := True; Result.At_Register := Role;
         Result.Boot_Address := (if Role = R.Data_Register then Table.Data else Table.Control);
         Result.Namespace_Claim := Claims.Registers (Role);
         if Result.Boot_Address.Space /= Result.Namespace_Claim.Address.Space then
            Result.Status := Space_Mismatch; return Result;
         elsif Result.Boot_Address.Address /= Result.Namespace_Claim.Address.Address then
            Result.Status := Address_Mismatch; return Result;
         end if;
      end loop;
      for Role in R.Register_Role loop
         Result.At_Register := Role;
         Result.Boot_Address := (if Role = R.Data_Register then Table.Data else Table.Control);
         Result.Namespace_Claim := Claims.Registers (Role);
         if Result.Namespace_Claim.IO_Decode_10 then
            Result.Status := Decode_Review_Required; return Result;
         elsif Result.Namespace_Claim.Writeability_Declared
           and then not Result.Namespace_Claim.Declared_Writeable
         then Result.Status := Read_Only_Review_Required; return Result;
         end if;
      end loop;
      Result.Status := Claims_Agree;
      return Result;
   end Compare;
end ACPI_EC_Boot_Resources;
