pragma Ada_2022;
with ACPI_EC_Registers;
with ACPI_Field_Remote_Provider;
with AML_Field_Layout;
with AML_Field_Access;
with AML_Decode;
with Interfaces;
--  Trusted controller-side association, not an admission or wire interface.
--  Item must already describe independently admitted cell rights. This helper
--  neither installs that admission nor establishes backing/protocol ownership.
generic
   with package Catalog is new ACPI_EC_Registers (<>);
   with package Provider is new ACPI_Field_Remote_Provider (<>);
package ACPI_EC_Field_Bindings with SPARK_Mode is
   use type Catalog.Owner_ID, AML_Field_Access.Register_ID;
   use type AML_Decode.Integer_Value, AML_Field_Layout.Region_Geometry;
   type Outcome is (Ready, Invalid_Admission, Owner_Mismatch,
                    Invalid_Region, Cell_Outside_Region, Invalid_Descriptor);
   type Result (Status : Outcome := Invalid_Admission) is record
      case Status is
         when Ready => Item : Provider.Descriptor;
         when others => null;
      end case;
   end record;
   --  Expected_Owner is trusted authenticated process identity, never a word
   --  supplied by AML. Preserve the complete64-bit ID/epoch/owner domains.
   --  The EC cell must equal Region.Base + the returned byte offset. No caller
   --  supplied geometry may silently associate a named ID with another cell.
   function Build
     (Item : Catalog.Admission; Expected_Owner : Catalog.Owner_ID;
      Region : AML_Field_Layout.Region_Geometry) return Result
     with Global => null,
       Post => (if Build'Result.Status = Ready then
         Provider.Valid (Build'Result.Item)
         and then Item.Owner = Expected_Owner
         and then Build'Result.Item.Register = AML_Field_Access.Register_ID (Item.Register)
         and then Build'Result.Item.Epoch = AML_Field_Access.Counter (Item.Epoch)
         and then Build'Result.Item.Geometry.Region = Region
         and then Build'Result.Item.Geometry.Byte_Offset + Region.Base =
           AML_Decode.Integer_Value (Item.Cell)
         and then Build'Result.Item.Readable = Item.Readable
         and then Build'Result.Item.Writable = Item.Writable
         and then Build'Result.Item.Exact_Write_Safe = Item.Exact_Write_Safe
         and then Build'Result.Item.Write_Mask = Interfaces.Unsigned_64 (Item.Write_Mask));
end ACPI_EC_Field_Bindings;
