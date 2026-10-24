------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with Interfaces;
with CCL.Setting_Codecs.Display_Mode;

package body Desktop_Display_Mode_Settings is
   use type Interfaces.Unsigned_32;
   package DD renames CCL.Interfaces.Desktop_Display;
   package Codec renames CCL.Setting_Codecs.Display_Mode;
   use type Codec.Display_Mode_Choice;

   --  The schema's member and field names (DD.MODE_TYPE_SOURCE), for Encode.
   Auto_Member : constant String := "Auto";
   Fixed_Member : constant String := "Fixed";
   Choice_Type : constant String := "Display_Mode_Choice";
   Width_Field : constant String := "width";
   Height_Field : constant String := "height";
   Refresh_Field : constant String := "refresh_millihertz";

   pragma Compile_Time_Error
     (DD.MAXIMUM_REFRESH_MILLIHERTZ /= Long_Long_Integer (OM.Millihertz'Last)
      or else Long_Long_Integer (Codec.Refresh_Millihertz_Value'First) /= Long_Long_Integer (OM.Millihertz'First)
      or else Long_Long_Integer (Codec.Refresh_Millihertz_Value'Last) /= Long_Long_Integer (OM.Millihertz'Last)
      or else Long_Long_Integer (Codec.Mode_Extent_Value'First) /= Long_Long_Integer (OM.OD.Extent'First)
      or else Long_Long_Integer (Codec.Mode_Extent_Value'Last) /= Long_Long_Integer (OM.OD.Extent'Last),
      "the CCL mode bounds must be the output protocol's");

   Hex_Digits : constant String := "0123456789ABCDEF";
   Hex_Base : constant := 16;
   function Hex (Value : Interfaces.Unsigned_32; Width : Positive) return String is
      Result : String (1 .. Width);
      Rest : Interfaces.Unsigned_32 := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Hex_Digits (Natural (Rest mod Hex_Base) + 1);
         Rest := Rest / Hex_Base;
      end loop;
      return Result;
   end Hex;
   Product_Digits : constant := 4;
   Serial_Digits : constant := 8;

   function Member_For (Monitor : OM.Monitor_Identity) return String is
     (Monitor.Vendor (1) & Monitor.Vendor (2) & Monitor.Vendor (3) & "-" &
      Hex (Interfaces.Unsigned_32 (Monitor.Product), Product_Digits) & "-" &
      Hex (Monitor.Serial, Serial_Digits));

   procedure Decode (Source : String; Item : out Preference; Success : out Boolean) is
      Setting : Codec.Display_Mode_Value;
   begin
      Item := Default;
      Codec.Decode (Source, Setting, Success);
      if Success and then Setting.Choice = Codec.Fixed then
         Item := (Auto => False,
                  Size => (OM.OD.Extent (Setting.Fixed_Payload.width), OM.OD.Extent (Setting.Fixed_Payload.height)),
                  Refresh => OM.Millihertz (Setting.Fixed_Payload.refresh_millihertz));
      end if;
   end Decode;

   function Image (Value : Long_Long_Integer) return String is
      Text : constant String := Long_Long_Integer'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   function Encode (Item : Preference) return String is
     (if Item.Auto then DD.MODE_ROOT_TYPE_NAME & "." & Auto_Member
      else "(" & DD.MODE_ROOT_TYPE_NAME & "." & Fixed_Member & " (" & Choice_Type & " " &
           Width_Field & " => " & Image (Long_Long_Integer (Item.Size.Width)) & " " &
           Height_Field & " => " & Image (Long_Long_Integer (Item.Size.Height)) & " " &
           Refresh_Field & " => " & Image (Long_Long_Integer (Item.Refresh)) & "))");
end Desktop_Display_Mode_Settings;
