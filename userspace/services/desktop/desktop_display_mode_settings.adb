------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with CCL.Typed_Settings;
with CCL.Setting_Kinds;
with Interfaces;
with CCL.Objects;
with CCL.Objects.Views;

package body Desktop_Display_Mode_Settings is
   use type Interfaces.Unsigned_32;
   package Views renames CCL.Objects.Views;
   package TS renames CCL.Typed_Settings;
   package DD renames CCL.Interfaces.Desktop_Display;

   --  The schema's member and field names (DD.MODE_TYPE_SOURCE).
   Auto_Member : constant String := "Auto";
   Fixed_Member : constant String := "Fixed";
   Choice_Type : constant String := "Display_Mode_Choice";
   Width_Field : constant String := "width";
   Height_Field : constant String := "height";
   Refresh_Field : constant String := "refresh_millihertz";

   pragma Compile_Time_Error
     (DD.MAXIMUM_REFRESH_MILLIHERTZ /= Long_Long_Integer (OM.Millihertz'Last),
      "the CCL refresh bound must be the output protocol's");

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

   type Snapshot_Access is access Views.Snapshot;
   Work : Snapshot_Access;

   procedure Decode (Source : String; Item : out Preference; Success : out Boolean) is
      Read : Boolean;
   begin
      Item := Default;
      Success := False;
      if Work = null then
         Work := new Views.Snapshot;
      end if;
      TS.Read (CCL.Setting_Kinds.Display_Mode_Setting, Source, Work.all, Read);
      if not Read then
         return;
      end if;
      declare
         Root : constant Views.Cursor := Views.Root (Work.all);
         Member : constant String := TS.Alternative_Name (Work.all, Root);
      begin
         if Member = Auto_Member then
            Success := True;
         elsif Member = Fixed_Member then
            declare
               Choice : constant Views.Cursor := Views.Payload (Work.all, Root);
               function Field (Name : String) return Interfaces.Integer_64 is
                 (CCL.Objects.Integer_Of (Views.Scalar (Work.all, TS.Named (Work.all, Choice, Name))));
               W : constant Interfaces.Integer_64 := Field (Width_Field);
               H : constant Interfaces.Integer_64 := Field (Height_Field);
               R : constant Interfaces.Integer_64 := Field (Refresh_Field);
               subtype Extent_Value is Interfaces.Integer_64 range
                 Interfaces.Integer_64 (OM.OD.Extent'First) .. Interfaces.Integer_64 (OM.OD.Extent'Last);
               subtype Refresh_Value is Interfaces.Integer_64 range
                 Interfaces.Integer_64 (OM.Millihertz'First) .. Interfaces.Integer_64 (OM.Millihertz'Last);
            begin
               --  The schema's range types already bound all three.
               if W in Extent_Value and then H in Extent_Value and then R in Refresh_Value then
                  Item := (Auto => False, Size => (OM.OD.Extent (W), OM.OD.Extent (H)),
                           Refresh => OM.Millihertz (R));
                  Success := True;
               end if;
            end;
         end if;
      end;
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
