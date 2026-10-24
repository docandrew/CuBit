--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog or a type.
pragma Ada_2022;

package body CCL.Setting_Codecs.Calc_Options with SPARK_Mode is
   package R renames CCL.Canonical_Readers;

   procedure Read_Calc_Mode
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Calc_Mode_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      Item := Calc_Mode_Value'First;
      if not OK then
         return;
      elsif R.Next_Is_Word (Source, Position, "Calc_Mode.Standard") then
         R.Expect_Word (Source, Position, OK, "Calc_Mode.Standard");
         Item := Standard;
      elsif R.Next_Is_Word (Source, Position, "Calc_Mode.Scientific") then
         R.Expect_Word (Source, Position, OK, "Calc_Mode.Scientific");
         Item := Scientific;
      elsif R.Next_Is_Word (Source, Position, "Calc_Mode.Programmer") then
         R.Expect_Word (Source, Position, OK, "Calc_Mode.Programmer");
         Item := Programmer;
      else
         OK := False;
      end if;
   end Read_Calc_Mode;

   procedure Read_Calc_Angle
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Calc_Angle_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      Item := Calc_Angle_Value'First;
      if not OK then
         return;
      elsif R.Next_Is_Word (Source, Position, "Calc_Angle.Radians") then
         R.Expect_Word (Source, Position, OK, "Calc_Angle.Radians");
         Item := Radians;
      elsif R.Next_Is_Word (Source, Position, "Calc_Angle.Degrees") then
         R.Expect_Word (Source, Position, OK, "Calc_Angle.Degrees");
         Item := Degrees;
      elsif R.Next_Is_Word (Source, Position, "Calc_Angle.Gradians") then
         R.Expect_Word (Source, Position, OK, "Calc_Angle.Gradians");
         Item := Gradians;
      else
         OK := False;
      end if;
   end Read_Calc_Angle;

   procedure Read_Calc_Word
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Calc_Word_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      Item := Calc_Word_Value'First;
      if not OK then
         return;
      elsif R.Next_Is_Word (Source, Position, "Calc_Word.Unbounded") then
         R.Expect_Word (Source, Position, OK, "Calc_Word.Unbounded");
         Item := Unbounded;
      elsif R.Next_Is_Word (Source, Position, "Calc_Word.Bits_8") then
         R.Expect_Word (Source, Position, OK, "Calc_Word.Bits_8");
         Item := Bits_8;
      elsif R.Next_Is_Word (Source, Position, "Calc_Word.Bits_16") then
         R.Expect_Word (Source, Position, OK, "Calc_Word.Bits_16");
         Item := Bits_16;
      elsif R.Next_Is_Word (Source, Position, "Calc_Word.Bits_32") then
         R.Expect_Word (Source, Position, OK, "Calc_Word.Bits_32");
         Item := Bits_32;
      elsif R.Next_Is_Word (Source, Position, "Calc_Word.Bits_64") then
         R.Expect_Word (Source, Position, OK, "Calc_Word.Bits_64");
         Item := Bits_64;
      else
         OK := False;
      end if;
   end Read_Calc_Word;

   procedure Read_Calc_Base
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Calc_Base_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      Item := Calc_Base_Value'First;
      if not OK then
         return;
      elsif R.Next_Is_Word (Source, Position, "Calc_Base.Decimal") then
         R.Expect_Word (Source, Position, OK, "Calc_Base.Decimal");
         Item := Decimal;
      elsif R.Next_Is_Word (Source, Position, "Calc_Base.Hexadecimal") then
         R.Expect_Word (Source, Position, OK, "Calc_Base.Hexadecimal");
         Item := Hexadecimal;
      elsif R.Next_Is_Word (Source, Position, "Calc_Base.Octal") then
         R.Expect_Word (Source, Position, OK, "Calc_Base.Octal");
         Item := Octal;
      elsif R.Next_Is_Word (Source, Position, "Calc_Base.Binary") then
         R.Expect_Word (Source, Position, OK, "Calc_Base.Binary");
         Item := Binary;
      else
         OK := False;
      end if;
   end Read_Calc_Base;

   procedure Read_Calc_Options
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Calc_Options_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      R.Expect (Source, Position, OK, "(Calc_Options");
      R.Expect (Source, Position, OK, " mode => ");
      Read_Calc_Mode (Source, Position, OK, Item.mode);
      R.Expect (Source, Position, OK, " angle => ");
      Read_Calc_Angle (Source, Position, OK, Item.angle);
      R.Expect (Source, Position, OK, " word => ");
      Read_Calc_Word (Source, Position, OK, Item.word);
      R.Expect (Source, Position, OK, " signed => ");
      R.Read_Boolean (Source, Position, OK, Item.signed);
      R.Expect (Source, Position, OK, " base => ");
      Read_Calc_Base (Source, Position, OK, Item.base);
      R.Expect (Source, Position, OK, ")");
   end Read_Calc_Options;

   procedure Decode (Source : String; Item : out Calc_Options_Value; Success : out Boolean) is
   begin
      Item := (others => <>);
      Success := R.Readable (Source) and then Source'Length in 1 .. R.Maximum_Canonical;
      if not Success then
         return;
      end if;
      declare
         Position : Positive := Source'First;
      begin
         Read_Calc_Options (Source, Position, Success, Item);
         R.Finish (Source, Position, Success);
      end;
      if not Success then
         Item := (others => <>);
      end if;
   end Decode;
end CCL.Setting_Codecs.Calc_Options;

