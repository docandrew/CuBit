------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The display mode chosen for one monitor (Settings > Monitor, DISP-016),
--  read from and written to Config.
--
--  `desktop.display.mode.<monitor>` holds a typed CCL Display_Mode
--  (CCL.Interfaces.Desktop_Display), stored as its canonical source:
--
--    Display_Mode.Auto
--    (Display_Mode.Fixed (Display_Mode_Choice width => 3840 height => 2160
--       refresh_millihertz => 60000))
--
--  The key names the monitor by its EDID identity (Key_For: vendor,
--  product, serial), so each monitor keeps its own choice. Auto (also what
--  an absent setting means) is the driver's best admitted mode; Fixed is
--  one of the monitor's listed modes. Decode reads a stored value back
--  through the typed path (CCL.Typed_Settings) and refuses anything but the
--  canonical, schema-valid form; Encode writes that form
--  (tests/display-scale checks it is what the CCL checker produces).
------------------------------------------------------------------------------
with CuBit.Output_Modes;
with CCL.Interfaces.Desktop_Display;
with CCL.Typed_Settings;

package Desktop_Display_Mode_Settings is
   package OM renames CuBit.Output_Modes;
   --  The longest stored value Decode accepts.
   Maximum_Source : constant := CCL.Typed_Settings.MAXIMUM_CANONICAL;

   --  The Config key for a monitor: the prefix, then vendor-product-serial
   --  (product and serial in upper-case hexadecimal, fixed width).
   function Key_For (Monitor : OM.Monitor_Identity) return String;

   type Preference (Auto : Boolean := True) is record
      case Auto is
         when True => null;
         when False =>
            Size : OM.Mode;
            Refresh : OM.Millihertz;
      end case;
   end record;
   Default : constant Preference := (Auto => True);

   --  The Set_Mode request a Fixed preference names.
   function Request_Of (Item : Preference) return OM.Mode_Request is
     (OM.Exact_Rate, Item.Size, Item.Refresh)
     with Pre => not Item.Auto;

   procedure Decode (Source : String; Item : out Preference; Success : out Boolean);
   function Encode (Item : Preference) return String;

   pragma Compile_Time_Error
     (CCL.Interfaces.Desktop_Display.MAXIMUM_MODE_EXTENT /= Natural (CuBit.Output_Modes.OD.Extent'Last),
      "the CCL mode extent bound must be the output protocol's");
end Desktop_Display_Mode_Settings;
