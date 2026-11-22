------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The desktop's display scale setting, read from and written to Config.
--
--  `desktop.display.scale` holds a typed CCL Display_Scale
--  (CCL.Interfaces.Desktop_Display), stored as its canonical source:
--
--    Display_Scale.Auto
--    (Display_Scale.Fixed (UI_Scale numerator => 5 denominator => 4))
--
--  Auto (also what an absent setting means) gives each output the scale its
--  size asks for (Desktop_Auto_Scale); Fixed is the user's choice in
--  Settings > Displays. Decode reads a stored value with the decoder generated
--  from its type (CCL.Setting_Codecs.Display_Scale); anything but the canonical, schema-valid form
--  is refused and the desktop keeps Auto. Encode writes the canonical form
--  (tests/display-scale checks it is what the CCL checker produces).
------------------------------------------------------------------------------
with CuBit.Display_Geometry;

package Desktop_Display_Scale_Settings is
   --  The longest stored value Decode accepts.

   type Preference (Auto : Boolean := True) is record
      case Auto is
         when True => null;
         when False => Scale : CuBit.Display_Geometry.UI_Scale;
      end case;
   end record;
   Default : constant Preference := (Auto => True);

   procedure Decode (Source : String; Item : out Preference; Success : out Boolean);
   --  The setting catalog's codec (CAP-003).
   subtype Value is Preference;
   function Encode (Item : Preference) return String;
end Desktop_Display_Scale_Settings;
