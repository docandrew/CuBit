with Ada.Unchecked_Conversion;
with Interfaces;
with System;
package Intel_GPU_Cursor_Control with SPARK_Mode is
   -- Intel IHD-OS-TGL-Vol 2c-12.21, CUR_CTL, pp309-313.
   -- TGL-family snapshot layout, not a universal Intel register layout.
   -- Numeric fields preserve reserved encodings; never overlay live MMIO.
   type Bits_1 is mod 2 ** 1 with Size => 1;
   type Bits_2 is mod 2 ** 2 with Size => 2;
   type Bits_3 is mod 2 ** 3 with Size => 3;
   type Bits_4 is mod 2 ** 4 with Size => 4;
   type Bits_6 is mod 2 ** 6 with Size => 6;
   type Control is record
      Mode_Select : Bits_6 := 0;
      Reserved_6 : Bits_2 := 0;
      Force_Alpha_Value : Bits_2 := 0;
      Force_Alpha_Plane_Select : Bits_2 := 0;
      Reserved_12 : Bits_3 := 0;
      Rotate_180 : Bits_1 := 0;
      CSC_Enable : Bits_1 := 0;
      Reserved_17 : Bits_1 := 0;
      Pre_CSC_Gamma_Enable : Bits_1 := 0;
      Reserved_19 : Bits_4 := 0;
      Allow_Update_Disable : Bits_1 := 0;
      Pipe_CSC_Enable : Bits_1 := 0;
      Reserved_25 : Bits_1 := 0;
      Gamma_Enable : Bits_1 := 0;
      Reserved_27 : Bits_1 := 0;
      Arbitration_Slots : Bits_3 := 0;
      Reserved_31 : Bits_1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Control use record
      Mode_Select at 0 range 0 .. 5;
      Reserved_6 at 0 range 6 .. 7;
      Force_Alpha_Value at 0 range 8 .. 9;
      Force_Alpha_Plane_Select at 0 range 10 .. 11;
      Reserved_12 at 0 range 12 .. 14;
      Rotate_180 at 0 range 15 .. 15;
      CSC_Enable at 0 range 16 .. 16;
      Reserved_17 at 0 range 17 .. 17;
      Pre_CSC_Gamma_Enable at 0 range 18 .. 18;
      Reserved_19 at 0 range 19 .. 22;
      Allow_Update_Disable at 0 range 23 .. 23;
      Pipe_CSC_Enable at 0 range 24 .. 24;
      Reserved_25 at 0 range 25 .. 25;
      Gamma_Enable at 0 range 26 .. 26;
      Reserved_27 at 0 range 27 .. 27;
      Arbitration_Slots at 0 range 28 .. 30;
      Reserved_31 at 0 range 31 .. 31;
   end record;
   function From_Word is new Ada.Unchecked_Conversion
     (Interfaces.Unsigned_32, Control);
   function To_Word is new Ada.Unchecked_Conversion
     (Control, Interfaces.Unsigned_32);
end Intel_GPU_Cursor_Control;
