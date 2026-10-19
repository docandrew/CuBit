------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The UI scale an output gets by default (desktop.display.scale = Auto).
--
--  @description
--  Without modesetting the desktop runs at the firmware (UEFI GOP) mode,
--  which on a 4K monitor is 3840 x 2160: at 100% the UI would be tiny.
--  Until the user picks a scale in Settings > Displays, each output's scale
--  follows its native size: a 4K-class panel (long side >= UHD_Long_Side)
--  is drawn at 200%, a QHD-class one (>= QHD_Long_Side) at 150%, anything
--  smaller at 100%. The long side is used so a portrait monitor gets the
--  same scale as the same panel in landscape. A scale that would leave the
--  output less than the arrangement minimum (CuBit.Display_Arrangement:
--  Minimum_Width x Minimum_Height logical pixels) steps down to the largest
--  smaller preset that leaves enough, down to 100%.
--
--  The display service does not yet report a monitor's physical size (GOP
--  carries none; virtio-gpu's EDID stays inside the driver), so this is by
--  pixels, not DPI.
------------------------------------------------------------------------------
with CuBit.Display_Geometry;
with CuBit.Display_Arrangement;

package Desktop_Auto_Scale with SPARK_Mode, Pure is
   package G renames CuBit.Display_Geometry;
   package D renames CuBit.Display_Arrangement;
   use type D.Scale_Preset, G.Pixel_Edge;

   --  Long sides from which the UI is drawn larger: 3840 (4K UHD) and 2560
   --  (QHD / WQXGA).
   UHD_Long_Side : constant := 3_840;
   QHD_Long_Side : constant := 2_560;

   function Long_Side (Width, Height : G.Physical_Extent) return G.Physical_Extent is
     (G.Physical_Extent'Max (Width, Height));

   --  The preset the output's size asks for.
   function By_Size (Width, Height : G.Physical_Extent) return D.Scale_Preset is
     (if Long_Side (Width, Height) >= UHD_Long_Side then D.Scale_200
      elsif Long_Side (Width, Height) >= QHD_Long_Side then D.Scale_150
      else D.Scale_100);

   --  One axis's logical extent at Preset: native pixels over the scale,
   --  rounded up (as CuBit.Display_Geometry.Bounds rounds).
   subtype Logical_Extent is Positive range 1 .. Natural (G.Physical_Extent'Last);
   function Logical (Native : G.Physical_Extent; Preset : D.Scale_Preset) return Logical_Extent is
     ((Natural (Native) * Natural (D.Factor (Preset).Denominator) +
         Natural (D.Factor (Preset).Numerator) - 1) / Natural (D.Factor (Preset).Numerator));

   --  Preset leaves an output of Width x Height (upright) at least the
   --  arrangement minimum.
   function Fits (Width, Height : G.Physical_Extent; Preset : D.Scale_Preset) return Boolean is
     (Logical (Width, Preset) >= D.Minimum_Width and then
      Logical (Height, Preset) >= D.Minimum_Height);

   --  The default scale of an output Width x Height pixels (upright): the
   --  largest preset no larger than By_Size that Fits, or 100%.
   function Choose (Width, Height : G.Physical_Extent) return D.Scale_Preset
     with Post =>
       Choose'Result <= By_Size (Width, Height) and then
       (Choose'Result = D.Scale_100 or else Fits (Width, Height, Choose'Result)) and then
       (for all P in D.Scale_Preset =>
          (if P > Choose'Result and then P <= By_Size (Width, Height) then
             not Fits (Width, Height, P)));
end Desktop_Auto_Scale;
