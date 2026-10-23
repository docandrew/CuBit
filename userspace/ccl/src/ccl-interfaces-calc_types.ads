--  The calculator interface's types as CCL source (docs/calculator.md,
--  "The interface"), without the interface's operations: what
--  CCL.Interfaces.Calc publishes and what the typed setting
--  calculator.options (CCL.Typed_Settings) is checked against. A spec only,
--  so a reader of the setting (devmgr's typed configuration) needs no CCL
--  VM or catalog.
package CCL.Interfaces.Calc_Types with SPARK_Mode, Pure is
   --  Calc_Number: numerator signed and denominator positive ("1" for an
   --  integer), of the value or, for an approximation, of its midpoint;
   --  decimal: the value at PLACES places (an approximation: its
   --  guaranteed digits); bound: the error bound of those digits ("" when
   --  exact); hexadecimal, octal, binary: in programmer mode, the word's
   --  bits ("" otherwise); wrapped: a step wrapped into the word.
   --  Calc_Options is also the app's remembered setting (OPTIONS_KEY).
   TYPE_SOURCE : constant String :=
     "(type Calc_Form (enum Integer_Form Terminating Repeating Scientific_Exact Rounded " &
     "Scientific_Rounded Approximate_Decimal Approximate_Scientific)) (type Calc_Number (record " &
     "(exact Boolean) (numerator String) (denominator String) (decimal String) (form Calc_Form) " &
     "(bound String) (hexadecimal String) (octal String) (binary String) (wrapped Boolean))) " &
     "(type Calc_Mode (enum Standard Scientific Programmer)) (type Calc_Angle (enum Radians " &
     "Degrees Gradians)) (type Calc_Word (enum Unbounded Bits_8 Bits_16 Bits_32 Bits_64)) (type " &
     "Calc_Base (enum Decimal Hexadecimal Octal Binary)) (type Calc_Options (record (mode " &
     "Calc_Mode Calc_Mode.Standard) (angle Calc_Angle Calc_Angle.Radians) (word Calc_Word " &
     "Calc_Word.Bits_64) (signed Boolean true) (base Calc_Base Calc_Base.Decimal))) (type " &
     "Calc_Request (record (expression String) (options Calc_Options)))";
   --  The Calculator's remembered options: a Calc_Options.
   ROOT_OPTIONS_NAME : constant String := "Calc_Options";
end CCL.Interfaces.Calc_Types;
