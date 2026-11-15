------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The value axis of a chart: nice upper bounds and tick steps (1, 2, 5
--  times a power of ten), and a value's pixel row.
--
--  @description
--  Every axis starts at zero (latencies, counts and sizes are never
--  negative, and a truncated axis exaggerates). Values above Value_Ceiling
--  are drawn at it.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Charts_Value_Axis with SPARK_Mode, Pure is
   --  2^48: 78 hours in microseconds, 256 TiB in bytes.
   Value_Ceiling : constant := 2 ** 48;
   subtype Value is Unsigned_64 range 0 .. Value_Ceiling;

   function Clamp (V : Unsigned_64) return Value is (if V > Value_Ceiling then Value_Ceiling else V);

   --  The smallest 1, 2 or 5 times a power of ten that is at least V (1 for
   --  zero), or Value_Ceiling.
   function Nice_Ceiling (V : Value) return Value
     with Post => Nice_Ceiling'Result >= 1 and then (Nice_Ceiling'Result >= V);

   --  A tick step for an axis from zero to High with at most Ticks ticks: a
   --  nice value no smaller than High / Ticks.
   Maximum_Ticks : constant := 64;
   subtype Tick_Count is Positive range 1 .. Maximum_Ticks;
   function Tick_Step (High : Value; Ticks : Tick_Count) return Value
     with Post => Tick_Step'Result >= 1;

   --  The axis's top: High rounded up to a whole number of tick steps.
   function Axis_Top (High : Value; Ticks : Tick_Count) return Value
     with Post => Axis_Top'Result >= 1;

   --  The plot's height in rows.
   Maximum_Rows : constant := 8_192;
   subtype Row_Count is Positive range 1 .. Maximum_Rows;

   --  V's row in a plot of Rows rows whose top row shows Top (row 0 at the
   --  top, Rows - 1 is zero). Values above Top are at row 0.
   function Row_Of (V : Unsigned_64; Top : Value; Rows : Row_Count) return Natural
     with Pre => Top >= 1, Post => Row_Of'Result < Rows;
end Charts_Value_Axis;
