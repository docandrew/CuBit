with CCL.Interfaces.Calc_Types;
with CCL.Typed_Settings;
with CCL_Calc_Options;

--  The Calculator's remembered mode and options (docs/calculator.md,
--  "Modes"): the typed setting calculator.options, a CCL Calc_Options
--  value (the same type calc.evaluate-with takes) stored as its canonical
--  source, e.g.
--
--    (Calc_Options mode => Calc_Mode.Programmer angle => Calc_Angle.Radians
--     word => Calc_Word.Bits_32 signed => false base => Calc_Base.Hexadecimal)
--
--  Read back through the typed path (CCL.Typed_Settings), never parsed by
--  hand: anything but the canonical, schema-valid form is refused and the
--  app starts in Standard mode.
package Calc_Settings is
   Maximum_Source : constant := CCL.Typed_Settings.MAXIMUM_CANONICAL;
   procedure Decode (Source : String; Item : out CCL_Calc_Options.Choice; Success : out Boolean);
   --  The setting catalog's codec (CAP-003).
   subtype Value is CCL_Calc_Options.Choice;
   function Encode (Item : Value) return String renames CCL_Calc_Options.Source;
end Calc_Settings;
