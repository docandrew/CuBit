with Calc_Engine;
with CCL.Interfaces.Calc;
with CCL.Objects.Views;

--  A Calc_Options value (CCL.Interfaces.Calc) as the calculator core's
--  evaluation options, and back as its canonical CCL source: one mapping
--  for calc.evaluate-with and for the app's remembered setting.
package CCL_Calc_Options is
   package Calc renames CCL.Interfaces.Calc;
   type Choice is record
      Mode : Calc.Mode_Member := Calc.Standard;
      Angle : Calc.Angle_Member := Calc.Radians;
      Word : Calc.Word_Member := Calc.Bits_64;
      Signed : Boolean := True;
      Base : Calc.Base_Member := Calc.Decimal;
   end record;

   --  The Calc_Options at Position of Object.
   function Read (Object : CCL.Objects.Views.Snapshot; Position : CCL.Objects.Views.Cursor) return Choice;
   --  What the core evaluates with: programmer mode keeps integers in the word.
   function Engine_Options (Item : Choice) return Calc_Engine.Options;
   --  Its canonical CCL source, every field named in declaration order.
   function Source (Item : Choice) return String;
end CCL_Calc_Options;
