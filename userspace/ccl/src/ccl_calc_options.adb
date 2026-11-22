with Interfaces;
with Calc_Balls;
with Calc_Words;
with CCL.Objects;

package body CCL_Calc_Options is
   package Views renames CCL.Objects.Views;
   use type Interfaces.Unsigned_64;
   use type Calc.Mode_Member;

   --  Fields of Calc_Options, in declaration order.
   MODE_FIELD : constant := 1;
   ANGLE_FIELD : constant := 2;
   WORD_FIELD : constant := 3;
   SIGNED_FIELD : constant := 4;
   BASE_FIELD : constant := 5;

   function Read (Object : Views.Snapshot; Position : Views.Cursor) return Choice is
      function Member (Field : Positive; Count : Positive) return Natural is
        (Natural'Min (Natural'Max (Views.Alternative (Object, Views.Field (Object, Position, Field)), 1), Count) - 1);
   begin
      return
        (Mode => Calc.Mode_Member'Val (Member (MODE_FIELD, Calc.Mode_Member'Pos (Calc.Mode_Member'Last) + 1)),
         Angle => Calc.Angle_Member'Val (Member (ANGLE_FIELD, Calc.Angle_Member'Pos (Calc.Angle_Member'Last) + 1)),
         Word => Calc.Word_Member'Val (Member (WORD_FIELD, Calc.Word_Member'Pos (Calc.Word_Member'Last) + 1)),
         Signed => Views.Scalar (Object, Views.Field (Object, Position, SIGNED_FIELD)).First /= 0,
         Base => Calc.Base_Member'Val (Member (BASE_FIELD, Calc.Base_Member'Pos (Calc.Base_Member'Last) + 1)));
   end Read;

   function Engine_Options (Item : Choice) return Calc_Engine.Options is
     ((Domain => (if Item.Mode = Calc.Programmer then Calc_Engine.Integers else Calc_Engine.Rationals),
       --  Words are programmer mode's; the other modes are unbounded.
       Word => (if Item.Mode /= Calc.Programmer then Calc_Words.Unbounded else
                (case Item.Word is
                   when Calc.Unbounded => Calc_Words.Unbounded, when Calc.Bits_8 => Calc_Words.Bits_8,
                   when Calc.Bits_16 => Calc_Words.Bits_16, when Calc.Bits_32 => Calc_Words.Bits_32,
                   when Calc.Bits_64 => Calc_Words.Bits_64)),
       Signed => Item.Signed,
       Angle => (case Item.Angle is
                    when Calc.Radians => Calc_Balls.Radians, when Calc.Degrees => Calc_Balls.Degrees,
                    when Calc.Gradians => Calc_Balls.Gradians)));

   function Source (Item : Choice) return String is
     ("(Calc_Options mode => Calc_Mode." &
        (case Item.Mode is
            when Calc.Standard => "Standard", when Calc.Scientific => "Scientific",
            when Calc.Programmer => "Programmer") &
      " angle => Calc_Angle." &
        (case Item.Angle is
            when Calc.Radians => "Radians", when Calc.Degrees => "Degrees", when Calc.Gradians => "Gradians") &
      " word => Calc_Word." &
        (case Item.Word is
            when Calc.Unbounded => "Unbounded", when Calc.Bits_8 => "Bits_8", when Calc.Bits_16 => "Bits_16",
            when Calc.Bits_32 => "Bits_32", when Calc.Bits_64 => "Bits_64") &
      " signed => " & (if Item.Signed then "true" else "false") &
      " base => Calc_Base." &
        (case Item.Base is
            when Calc.Decimal => "Decimal", when Calc.Hexadecimal => "Hexadecimal", when Calc.Octal => "Octal",
            when Calc.Binary => "Binary") & ")");
end CCL_Calc_Options;
