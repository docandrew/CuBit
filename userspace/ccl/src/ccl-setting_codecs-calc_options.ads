--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog or a type.
pragma Ada_2022;
with CCL.Canonical_Readers;

--  The typed setting calculator.options: a CCL Calc_Options (CCL.Interfaces.Calc_Types.TYPE_SOURCE),
--  as Ada types, and Decode, which reads its canonical source
--  (CCL.Typed_Settings renders it; CCL.Canonical_Readers).
package CCL.Setting_Codecs.Calc_Options with SPARK_Mode, Pure is
   subtype Text is CCL.Canonical_Readers.Text;
   type Calc_Mode_Value is (Standard, Scientific, Programmer);
   type Calc_Angle_Value is (Radians, Degrees, Gradians);
   type Calc_Word_Value is (Unbounded, Bits_8, Bits_16, Bits_32, Bits_64);
   type Calc_Base_Value is (Decimal, Hexadecimal, Octal, Binary);
   type Calc_Options_Value is record
      mode : Calc_Mode_Value := Calc_Mode_Value'First;
      angle : Calc_Angle_Value := Calc_Angle_Value'First;
      word : Calc_Word_Value := Calc_Word_Value'First;
      signed : Boolean := False;
      base : Calc_Base_Value := Calc_Base_Value'First;
   end record;

   --  Item is the value when Success: Source is exactly the canonical
   --  source of a Calc_Options. Otherwise Item is (others => <>).
   procedure Decode (Source : String; Item : out Calc_Options_Value; Success : out Boolean)
   with Global => null, Always_Terminates;
end CCL.Setting_Codecs.Calc_Options;

