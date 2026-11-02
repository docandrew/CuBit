--  The desktop's display scale as CCL types: `desktop.display.scale` is one
--  Display_Scale, checked when the configuration is compiled
--  (CCL.Typed_Settings) and read back, with the decoder generated from
--  these declarations (CCL.Setting_Codecs), by the desktop
--  (Desktop_Display_Scale_Settings). Auto (the default, and what an absent
--  setting means) lets the desktop pick each output's scale from its size
--  (Desktop_Auto_Scale); Fixed is the user's choice in Settings > Displays,
--  numerator / denominator (5/4 is 125%). The part bounds are the display
--  geometry's (CuBit.Display_Geometry.Scale_Component): the type system
--  refuses anything else, so the desktop never clamps.
package CCL.Interfaces.Desktop_Display with SPARK_Mode, Pure is
   MINIMUM_SCALE_PART : constant := 1;
   MAXIMUM_SCALE_PART : constant := 16;
   --  The same bounds spelled for the CCL analyser (tests/display-scale
   --  checks the two agree).
   TYPE_SOURCE : constant String :=
     "(type Scale_Part (range 1 16)) " &
     "(type UI_Scale (record (numerator Scale_Part) (denominator Scale_Part))) " &
     "(type Display_Scale (variant (Auto) (Fixed UI_Scale)))";
   ROOT_TYPE_NAME : constant String := "Display_Scale";

   --  `desktop.display.modeset` is one Display_Modeset: when enabled, the
   --  desktop asks display.svc for each Intel output's best admitted mode
   --  (the monitor's native one when the hardware can drive it; DISP-006)
   --  instead of keeping the firmware's. Off by default until the modeset
   --  has passed its hardware soak (DISP-013); absent means off.
   MODESET_TYPE_SOURCE : constant String :=
     "(type Display_Modeset (record (enabled Boolean false)))";
   MODESET_ROOT_TYPE_NAME : constant String := "Display_Modeset";

   --  `desktop.display.mode.<monitor>` is one Display_Mode: the mode chosen
   --  for that monitor in Settings > Monitor (DISP-016), keyed by its EDID
   --  identity (Desktop_Display_Mode_Settings.Member_For: vendor, product and
   --  serial). Auto (also what an absent setting means) is the best mode the
   --  driver admits; Fixed is one of the monitor's listed modes, its size
   --  and the refresh rate the driver lists for it. The bounds are the
   --  output mode protocol's (CuBit.Output_Modes: extents, millihertz).
   MAXIMUM_MODE_EXTENT : constant := 65_535;
   MAXIMUM_REFRESH_MILLIHERTZ : constant := 4_294_967_295;
   MODE_TYPE_SOURCE : constant String :=
     "(type Mode_Extent (range 1 65535)) " &
     "(type Refresh_Millihertz (range 1 4294967295)) " &
     "(type Display_Mode_Choice (record (width Mode_Extent) (height Mode_Extent) " &
     "(refresh_millihertz Refresh_Millihertz))) " &
     "(type Display_Mode (variant (Auto) (Fixed Display_Mode_Choice)))";
   MODE_ROOT_TYPE_NAME : constant String := "Display_Mode";

   --  `desktop.appearance.v2` is one Appearance: what the desktop's outputs
   --  show, read back and written, with the decoder generated from these
   --  declarations (CCL.Setting_Codecs.Appearance), by the desktop
   --  (Desktop_Appearance_Settings). The backdrop is data, not a list of
   --  pictures: a solid colour, or a wallpaper named by its catalog name
   --  (CCL.Interfaces.Wallpapers) and how it is placed. A name the wallpaper
   --  catalog does not hold (a picture since removed) is still a well-typed
   --  value: the desktop shows its default wallpaper instead and says so.
   APPEARANCE_TYPE_SOURCE : constant String :=
     "(type Color_Scheme (enum Alloy_Light Alloy_Dark)) " &
     "(type Solid_Color (enum Slate Ocean)) " &
     "(type Placement (enum Fill Fit Center)) " &
     "(type Wallpaper_Choice (record (name String) (placement Placement Placement.Fill))) " &
     "(type Backdrop (variant (Image Wallpaper_Choice) (Solid Solid_Color))) " &
     "(type Appearance (record (scheme Color_Scheme Color_Scheme.Alloy_Light) (backdrop Backdrop)))";
   APPEARANCE_ROOT_TYPE_NAME : constant String := "Appearance";
end CCL.Interfaces.Desktop_Display;
