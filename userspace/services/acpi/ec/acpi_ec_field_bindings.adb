pragma Ada_2022;
with ACPI_EC_Types;
with ACPI_FADT.Transactions;
package body ACPI_EC_Field_Bindings with SPARK_Mode is
   package L renames AML_Field_Layout;
   use type Catalog.Register_ID, Catalog.Epoch_ID;
   use type ACPI_EC_Types.Byte, AML_Decode.Byte;
   function Build
     (Item : Catalog.Admission; Expected_Owner : Catalog.Owner_ID;
      Region : L.Region_Geometry) return Result
   is
      Cell : constant AML_Decode.Integer_Value := AML_Decode.Integer_Value (Item.Cell);
      Descriptor : Provider.Descriptor;
   begin
      if Item.Register = 0 or else Item.Epoch = 0 or else Item.Owner = 0
        or else Expected_Owner = 0
        or else (not Item.Readable and not Item.Writable)
        or else (Item.Writable and not Item.Exact_Write_Safe)
        or else (not Item.Writable and Item.Write_Mask /= 0)
      then return (Status => Invalid_Admission); end if;
      if Item.Owner /= Expected_Owner then return (Status => Owner_Mismatch); end if;
      if Region.Space /= L.Embedded_Control_Space or else Region.Length = 0
        or else Region.Base > L.EC_Last_Cell
        or else Region.Length - 1 > L.EC_Last_Cell - Region.Base
      then return (Status => Invalid_Region); end if;
      if Cell < Region.Base or else Cell - Region.Base >= Region.Length then
         return (Status => Cell_Outside_Region);
      end if;
      Descriptor :=
        (Register => AML_Field_Access.Register_ID (Item.Register),
         Epoch => AML_Field_Access.Counter (Item.Epoch),
         Geometry => (Region => Region, Byte_Offset => Cell - Region.Base,
                      Width => ACPI_FADT.Transactions.Byte_Access),
         Readable => Item.Readable, Writable => Item.Writable,
         Exact_Write_Safe => Item.Exact_Write_Safe,
         Write_Mask => Interfaces.Unsigned_64 (Item.Write_Mask));
      if not Provider.Valid (Descriptor) then return (Status => Invalid_Descriptor); end if;
      return (Status => Ready, Item => Descriptor);
   end Build;
end ACPI_EC_Field_Bindings;
