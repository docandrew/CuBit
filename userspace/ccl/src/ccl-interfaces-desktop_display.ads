--  The desktop's display scale as CCL types: `desktop.display.scale` is one
--  Display_Scale, checked when the configuration is compiled
--  (CCL.Typed_Settings) and read back the same way by the desktop
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
   DISPLAY_SCALE_KEY : constant String := "desktop.display.scale";

   --  `desktop.display.modeset` is one Display_Modeset: when enabled, the
   --  desktop asks display.svc for each Intel output's best admitted mode
   --  (the monitor's native one when the hardware can drive it; DISP-006)
   --  instead of keeping the firmware's. Off by default until the modeset
   --  has passed its hardware soak (DISP-013); absent means off.
   MODESET_TYPE_SOURCE : constant String :=
     "(type Display_Modeset (record (enabled Boolean false)))";
   MODESET_ROOT_TYPE_NAME : constant String := "Display_Modeset";
   MODESET_KEY : constant String := "desktop.display.modeset";
end CCL.Interfaces.Desktop_Display;
