------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with CCL.Interfaces.Desktop_Display;
with CCL.Setting_Codecs.Display_Scale;

package body Desktop_Display_Scale_Settings is
   package DD renames CCL.Interfaces.Desktop_Display;
   package G renames CuBit.Display_Geometry;
   package Codec renames CCL.Setting_Codecs.Display_Scale;
   use type Codec.Display_Scale_Choice;

   --  The schema's member and field names (DD.TYPE_SOURCE), for Encode.
   Auto_Member : constant String := "Auto";
   Fixed_Member : constant String := "Fixed";
   Numerator_Field : constant String := "numerator";
   Denominator_Field : constant String := "denominator";

   pragma Compile_Time_Error
     (DD.MINIMUM_SCALE_PART /= Natural (G.Scale_Component'First) or else
      DD.MAXIMUM_SCALE_PART /= Natural (G.Scale_Component'Last) or else
      Long_Long_Integer (Codec.Scale_Part_Value'First) /= Long_Long_Integer (G.Scale_Component'First) or else
      Long_Long_Integer (Codec.Scale_Part_Value'Last) /= Long_Long_Integer (G.Scale_Component'Last),
      "the CCL scale part bounds must be the display geometry's");

   procedure Decode (Source : String; Item : out Preference; Success : out Boolean) is
      Setting : Codec.Display_Scale_Value;
   begin
      Item := Default;
      Codec.Decode (Source, Setting, Success);
      if Success and then Setting.Choice = Codec.Fixed then
         Item := (Auto => False, Scale => (G.Scale_Component (Setting.Fixed_Payload.numerator),
                                           G.Scale_Component (Setting.Fixed_Payload.denominator)));
      end if;
   end Decode;

   function Image (Value : G.Scale_Component) return String is
      Text : constant String := G.Scale_Component'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   function Encode (Item : Preference) return String is
     (if Item.Auto then DD.ROOT_TYPE_NAME & "." & Auto_Member
      else "(" & DD.ROOT_TYPE_NAME & "." & Fixed_Member & " (UI_Scale " &
           Numerator_Field & " => " & Image (Item.Scale.Numerator) & " " &
           Denominator_Field & " => " & Image (Item.Scale.Denominator) & "))");
end Desktop_Display_Scale_Settings;
