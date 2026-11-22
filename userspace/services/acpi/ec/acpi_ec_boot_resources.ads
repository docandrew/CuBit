pragma Ada_2022;
with Firmware_Tables;
with ACPI_FADT;
with ACPI_EC_Boot_Table;
with ACPI_EC_Resources;
with Interfaces;
--  Comparison of copied firmware claims only. No namespace identity,provider
--  readiness,interrupt equivalence,ownership or register capability is created.
package ACPI_EC_Boot_Resources with SPARK_Mode, Pure is
   type Outcome is (Claims_Agree, Invalid_Boot_Table, Unsupported_Boot_Registers,
      Invalid_Namespace_Resources, Space_Mismatch, Address_Mismatch,
      Decode_Review_Required, Read_Only_Review_Required);
   type Report is record
      Status : Outcome := Invalid_Boot_Table;
      Boot_Status : ACPI_EC_Boot_Table.Decode_Status := ACPI_EC_Boot_Table.Invalid_Table;
      Boot_Registers : ACPI_EC_Boot_Table.Register_Status := ACPI_EC_Boot_Table.Invalid_Description;
      Namespace_Status : ACPI_EC_Resources.Status := ACPI_EC_Resources.Bad_Template;
      Register_Examined : Boolean := False;
      At_Register : ACPI_EC_Resources.Register_Role := ACPI_EC_Resources.Data_Register;
      Boot_Address : ACPI_FADT.Generic_Address;
      Namespace_Claim : ACPI_EC_Resources.Register_Claim;
   end record;
   use type Interfaces.Unsigned_8, Interfaces.Unsigned_64;
   use type ACPI_EC_Boot_Table.Decode_Status, ACPI_EC_Boot_Table.Register_Status, ACPI_EC_Resources.Status;
   use type ACPI_EC_Resources.Register_Role;
   --  Ghost specification over decoded descriptions. This says nothing about
   --  parser correctness,physical backing or the provenance of the byte arrays.
   function Descriptions_Agree
     (Boot : ACPI_EC_Boot_Table.Description; Namespace : ACPI_EC_Resources.Result)
      return Boolean is
     (Boot.Status = ACPI_EC_Boot_Table.Decoded
      and then Namespace.Outcome = ACPI_EC_Resources.Described
      and then (for all Role in ACPI_EC_Resources.Register_Role =>
        (if Role = ACPI_EC_Resources.Data_Register then Boot.Data.Space else Boot.Control.Space)
          = Namespace.Registers (Role).Address.Space
        and then (if Role = ACPI_EC_Resources.Data_Register then Boot.Data.Address else Boot.Control.Address)
          = Namespace.Registers (Role).Address.Address
        and then not Namespace.Registers (Role).IO_Decode_10
        and then (not Namespace.Registers (Role).Writeability_Declared
          or else Namespace.Registers (Role).Declared_Writeable)))
     with Ghost, Global => null;
   --  Re-decodes both exact copied byte sequences. ECDT control/data ordering
   --  differs from _CRS data/control ordering; compare by role,not array order.
   --  Access_Size0/1 is equivalent after byte-register compatibility validation.
   --  IO10-bit decode or declared read-only memory needs separate platform
   --  review even if addresses match. Neither condition is silently discarded.
   --  GPIO/IRQ/_GPE,_UID and GlobalLock are deliberately separate observations.
   function Compare (Boot, Resources : Firmware_Tables.Bytes) return Report
     with Global => null,
       Post =>
         ((Compare'Result.Status = Claims_Agree) =
           (ACPI_EC_Boot_Table.Check_Registers (Boot) = ACPI_EC_Boot_Table.Byte_Registers_Described
            and then Descriptions_Agree (ACPI_EC_Boot_Table.Decode (Boot), ACPI_EC_Resources.Describe (Resources))))
         and then (if Compare'Result.Status = Space_Mismatch then
           Compare'Result.Register_Examined
           and then Compare'Result.Boot_Address.Space /= Compare'Result.Namespace_Claim.Address.Space)
         and then (if Compare'Result.Status = Address_Mismatch then
           Compare'Result.Register_Examined
           and then Compare'Result.Boot_Address.Space = Compare'Result.Namespace_Claim.Address.Space
           and then Compare'Result.Boot_Address.Address /= Compare'Result.Namespace_Claim.Address.Address)
         and then (if Compare'Result.Status = Decode_Review_Required then
           Compare'Result.Register_Examined and then Compare'Result.Namespace_Claim.IO_Decode_10)
         and then (if Compare'Result.Status = Read_Only_Review_Required then
           Compare'Result.Register_Examined
           and then not Compare'Result.Namespace_Claim.IO_Decode_10
           and then Compare'Result.Namespace_Claim.Writeability_Declared
           and then not Compare'Result.Namespace_Claim.Declared_Writeable);
end ACPI_EC_Boot_Resources;
