with CCL.Setting_Codecs.Calc_Options;

package body Calc_Settings is
   package Codec renames CCL.Setting_Codecs.Calc_Options;
   package Calc renames CCL_Calc_Options.Calc;

   --  The schema's members are CCL.Interfaces.Calc's, one to one.
   function Mode_Of (Member : Codec.Calc_Mode_Value) return Calc.Mode_Member is
     (case Member is
         when Codec.Standard => Calc.Standard, when Codec.Scientific => Calc.Scientific,
         when Codec.Programmer => Calc.Programmer);
   function Angle_Of (Member : Codec.Calc_Angle_Value) return Calc.Angle_Member is
     (case Member is
         when Codec.Radians => Calc.Radians, when Codec.Degrees => Calc.Degrees, when Codec.Gradians => Calc.Gradians);
   function Word_Of (Member : Codec.Calc_Word_Value) return Calc.Word_Member is
     (case Member is
         when Codec.Unbounded => Calc.Unbounded, when Codec.Bits_8 => Calc.Bits_8, when Codec.Bits_16 => Calc.Bits_16,
         when Codec.Bits_32 => Calc.Bits_32, when Codec.Bits_64 => Calc.Bits_64);
   function Base_Of (Member : Codec.Calc_Base_Value) return Calc.Base_Member is
     (case Member is
         when Codec.Decimal => Calc.Decimal, when Codec.Hexadecimal => Calc.Hexadecimal,
         when Codec.Octal => Calc.Octal, when Codec.Binary => Calc.Binary);

   procedure Decode (Source : String; Item : out CCL_Calc_Options.Choice; Success : out Boolean) is
      Setting : Codec.Calc_Options_Value;
   begin
      Item := (others => <>);
      Codec.Decode (Source, Setting, Success);
      if Success then
         Item := (Mode => Mode_Of (Setting.mode), Angle => Angle_Of (Setting.angle), Word => Word_Of (Setting.word),
                  Signed => Setting.signed, Base => Base_Of (Setting.base));
      end if;
   end Decode;
end Calc_Settings;
