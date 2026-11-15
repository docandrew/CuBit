pragma Ada_2022;
package body BCD_Binary_Lemmas with SPARK_Mode is
   procedure Insert_Empty_Digit (Value : Word; Number : Digit; Index : Digit_Index) is
   begin
      null;
   end Insert_Empty_Digit;
   procedure Preserve_Narrow (Value : Word; Number : Digit; Index : Narrow_Digit_Index) is
      pragma Unreferenced (Value, Index);
   begin
      null;
   end Preserve_Narrow;
end BCD_Binary_Lemmas;
