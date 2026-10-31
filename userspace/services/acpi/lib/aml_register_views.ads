pragma Ada_2022;
with AML_Field_Layout;
with AML_Decode;
with ACPI_FADT.Transactions;
-- Geometry only. A region view never installs or enlarges backend authority.
-- It associates the exact same scalar transaction in the same address space.
package AML_Register_Views with SPARK_Mode, Pure is
   package L renames AML_Field_Layout;
   package T renames ACPI_FADT.Transactions;
   use type AML_Decode.Integer_Value, AML_Decode.Byte;
   use type L.Region_Geometry, L.Field_Form, T.Access_Width;
   subtype Register_Byte_Count is AML_Decode.Integer_Value range 1 .. 8;
   function Octets (Width : T.Access_Width) return Register_Byte_Count is
     (case Width is when T.Byte_Access => 1, when T.Word_Access => 2,
       when T.Dword_Access => 4, when T.Qword_Access => 8)
     with Post => Octets'Result = AML_Decode.Integer_Value (T.Octets (Width));
   function Valid_Region (Item : L.Region_Geometry) return Boolean is
     (Item.Space in L.Memory_Space | L.IO_Space | L.Embedded_Control_Space
      and then Item.Length /= 0
      and then Item.Length - 1 <= AML_Decode.Integer_Value'Last - Item.Base
      and then (if Item.Space = L.IO_Space then Item.Base <= L.IO_Last_Address
        and then Item.Length - 1 <= L.IO_Last_Address - Item.Base)
      and then (if Item.Space = L.Embedded_Control_Space then Item.Base <= L.EC_Last_Cell
        and then Item.Length - 1 <= L.EC_Last_Cell - Item.Base));
   function Valid_Register (Item : L.Register_Geometry) return Boolean is
     (Valid_Region (Item.Region) and then Item.Byte_Offset < Item.Region.Length
      and then Octets (Item.Width) <= Item.Region.Length - Item.Byte_Offset
      and then (if Item.Region.Space /= L.Memory_Space then Item.Width /= T.Qword_Access)
      and then (if Item.Region.Space = L.Embedded_Control_Space then Item.Width = T.Byte_Access)
      and then T.Aligned (Item.Region.Base + Item.Byte_Offset, Item.Width));
   function First_Address (Item : L.Register_Geometry) return AML_Decode.Integer_Value is
     (Item.Region.Base + Item.Byte_Offset) with Pre => Valid_Register (Item),
       Post => First_Address'Result >= Item.Region.Base
         and then First_Address'Result - Item.Region.Base = Item.Byte_Offset;
   function Last_Address (Item : L.Register_Geometry) return AML_Decode.Integer_Value
     with Pre => Valid_Register (Item),
       Post => Last_Address'Result = First_Address (Item) + (Octets (Item.Width) - 1)
         and then Last_Address'Result >= First_Address (Item)
         and then Last_Address'Result <= Item.Region.Base + (Item.Region.Length - 1);
   function Same_Transaction (Left, Right : L.Register_Geometry) return Boolean is
     (Valid_Register (Left) and then Valid_Register (Right)
      and then Left.Region.Space = Right.Region.Space and then Left.Width = Right.Width
      and then First_Address (Left) = First_Address (Right)
      and then Last_Address (Left) = Last_Address (Right));
   function Can_Rebase (Canonical : L.Register_Geometry; Region : L.Region_Geometry) return Boolean is
     (Valid_Register (Canonical) and then Valid_Region (Region)
      and then Canonical.Region.Space = Region.Space
      and then First_Address (Canonical) >= Region.Base
      and then First_Address (Canonical) - Region.Base < Region.Length
      and then Octets (Canonical.Width) <=
        Region.Length - (First_Address (Canonical) - Region.Base));
   type View (Allowed : Boolean := False) is record
      case Allowed is
         when True => Value : L.Register_Geometry;
         when False => null;
      end case;
   end record;
   function Rebase (Canonical : L.Register_Geometry; Region : L.Region_Geometry) return View
     with Post => Rebase'Result.Allowed = Can_Rebase (Canonical, Region)
       and then (if Rebase'Result.Allowed then
         Rebase'Result.Value.Region = Region
         and then Rebase'Result.Value.Width = Canonical.Width
         and then Same_Transaction (Canonical, Rebase'Result.Value));
   -- Preserve the existing field form/access/flags policy, using a verified
   -- metadata view instead of requiring identical enclosing region extents.
   function Admitted (Field : L.Field_Geometry; Canonical : L.Register_Geometry) return Boolean
     with Post => (if Admitted'Result then
       Field.Form = L.Ordinary_Field and then not Field.Has_Connection
       and then Field.Bit_Offset mod L.Bits_Per_Byte = 0
       and then Field.Bit_Length = AML_Decode.Integer_Value (L.Bits_Per_Byte * T.Octets (Canonical.Width))
       and then Same_Transaction (Canonical,
         (Field.Region, Field.Bit_Offset / L.Bits_Per_Byte, Canonical.Width)));
end AML_Register_Views;
