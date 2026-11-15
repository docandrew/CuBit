------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Chart colours (MET-002's app), one set per colour scheme: categorical
--  series slots in a fixed order (never cycled; colour follows the series,
--  not its rank), one sequential hue for magnitude (heatmaps), and the
--  status colours, reserved for meaning (over budget) and always shown
--  with a label.
--
--  @description
--  The categorical order and steps are the data-visualization reference
--  palette's, validated for colour-vision deficiency (adjacent pairs, both
--  schemes); the dark column is the same hues stepped for a dark surface.
--  Tints are precomputed blends over the plot's surface, never per-pixel
--  alpha.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Charts_Palette with SPARK_Mode, Pure is
   subtype Color is Unsigned_32 range 0 .. 16#FF_FFFF#;

   Series_Slots : constant := 8;
   subtype Slot is Positive range 1 .. Series_Slots;
   type Slot_Colors is array (Slot) of Color;

   --  Blue, orange, aqua, yellow, magenta, green, violet, red.
   Light_Series : constant Slot_Colors :=
     [16#2A78D6#, 16#EB6834#, 16#1BAF7A#, 16#EDA100#, 16#E87BA4#, 16#008300#, 16#6250D6#, 16#E34948#];
   Dark_Series : constant Slot_Colors :=
     [16#3987E5#, 16#D95926#, 16#199E70#, 16#C98500#, 16#D55181#, 16#008300#, 16#9085E9#, 16#E66767#];

   --  The sequential blue, near zero first: on the light surface it starts
   --  pale, on the dark one it starts dark (zero recedes into the surface).
   Ramp_Steps : constant := 9;
   subtype Ramp_Index is Positive range 1 .. Ramp_Steps;
   type Ramp is array (Ramp_Index) of Color;
   Light_Ramp : constant Ramp :=
     [16#DCEAFB#, 16#B7D3F6#, 16#9EC5F4#, 16#86B6EF#, 16#5598E7#, 16#3987E5#, 16#256ABF#, 16#184F95#, 16#0D366B#];
   Dark_Ramp : constant Ramp :=
     [16#1A3150#, 16#184F95#, 16#1C5CAB#, 16#256ABF#, 16#2A78D6#, 16#3987E5#, 16#5598E7#, 16#86B6EF#, 16#B7D3F6#];

   --  Status (fixed in both schemes).
   Good : constant Color := 16#0CA30C#;
   Warning : constant Color := 16#FAB219#;
   Serious : constant Color := 16#EC835A#;
   Critical : constant Color := 16#D03B3B#;

   --  Over Under, Alpha_Percent of the way (a precomputed tint).
   Percent : constant := 100;
   subtype Percentage is Natural range 0 .. Percent;
   function Mix (Over, Under : Color; Alpha_Percent : Percentage) return Color;

   --  The ramp step for Value of Top (zero: the first step; Top and above:
   --  the last).
   function Step_For (Value, Top : Unsigned_64) return Ramp_Index
     with Pre => Top >= 1;
end Charts_Palette;
