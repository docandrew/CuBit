pragma Ada_2022;
package body AML_Register_Views with SPARK_Mode is
   function Last_Address (Item : L.Register_Geometry) return AML_Decode.Integer_Value is
      Size : constant Register_Byte_Count := Octets (Item.Width);
      Relative_Last : AML_Decode.Integer_Value;
   begin
      pragma Assert (Size <= Item.Region.Length - Item.Byte_Offset);
      Relative_Last := Item.Byte_Offset + (Size - 1);
      pragma Assert (Relative_Last < Item.Region.Length);
      pragma Assert (Item.Region.Base <= AML_Decode.Integer_Value'Last - Relative_Last);
      return Item.Region.Base + Relative_Last;
   end Last_Address;
   function Rebase (Canonical : L.Register_Geometry; Region : L.Region_Geometry) return View is
      Offset : AML_Decode.Integer_Value;
      Size : constant Register_Byte_Count := Octets (Canonical.Width);
   begin
      if not Valid_Register (Canonical) or else not Valid_Region (Region)
        or else Canonical.Region.Space /= Region.Space or else Canonical.Region.PCI /= Region.PCI
        or else First_Address (Canonical) < Region.Base then return (Allowed => False); end if;
      Offset := First_Address (Canonical) - Region.Base;
      if Offset >= Region.Length or else Size > Region.Length - Offset then return (Allowed => False); end if;
      declare
         Local : constant L.Register_Geometry :=
           (Region, Offset, Canonical.Width);
      begin
         pragma Assert (Local.Region.Base + Local.Byte_Offset = First_Address (Canonical));
         pragma Assert (Valid_Register (Local));
         return (Allowed => True, Value => Local);
      end;
   end Rebase;
   function Admitted (Field : L.Field_Geometry; Canonical : L.Register_Geometry;
      Allow_Locked_Read : Boolean := False) return Boolean is
      Local : constant View := Rebase (Canonical, Field.Region);
   begin
      return Local.Allowed and then L.Admitted (Field, Local.Value, Allow_Locked_Read);
   end Admitted;
end AML_Register_Views;
