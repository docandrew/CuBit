with Ada.Unchecked_Conversion;
with Interfaces;
with System;
package Intel_GPU_Plane_Control with SPARK_Mode is
   -- Intel IHD-OS-TGL-Vol 2c-12.21, PLANE_CTL, pp.748-753.
   -- TGL-family layout used by the ADL-N read-only collector, not a
   -- universal layout for older/newer Intel generations.
   -- All encodings are representable; unsupported values are rejected by
   -- the footprint decoder. Never overlay this type on live MMIO.
   type Bits_1 is mod 2 ** 1 with Size => 1;
   type Bits_2 is mod 2 ** 2 with Size => 2;
   type Bits_3 is mod 2 ** 3 with Size => 3;
   type Bits_5 is mod 2 ** 5 with Size => 5;
   type Control is record
      Rotation : Bits_2 := 0;
      Reserved_2 : Bits_1 := 0;
      Allow_Update_Disable : Bits_1 := 0;
      Media_Decompression : Bits_1 := 0;
      Reserved_5 : Bits_1 := 0;
      Stereo_Vblank_Mask : Bits_2 := 0;
      Horizontal_Flip : Bits_1 := 0;
      Async_Address_Update : Bits_1 := 0;
      Tiling : Bits_3 := 0;
      Clear_Color_Disable : Bits_1 := 0;
      Reserved_14 : Bits_1 := 0;
      Render_Decompression : Bits_1 := 0;
      YUV_Byte_Order : Bits_2 := 0;
      Reserved_18 : Bits_1 := 0;
      YUV420_Component : Bits_1 := 0;
      RGB_Order : Bits_1 := 0;
      Key_Enable : Bits_2 := 0;
      Pixel_Format : Bits_5 := 0;
      Arbitration_Slots : Bits_3 := 0;
      Enabled : Bits_1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Control use record
      Rotation at 0 range 0 .. 1;
      Reserved_2 at 0 range 2 .. 2;
      Allow_Update_Disable at 0 range 3 .. 3;
      Media_Decompression at 0 range 4 .. 4;
      Reserved_5 at 0 range 5 .. 5;
      Stereo_Vblank_Mask at 0 range 6 .. 7;
      Horizontal_Flip at 0 range 8 .. 8;
      Async_Address_Update at 0 range 9 .. 9;
      Tiling at 0 range 10 .. 12;
      Clear_Color_Disable at 0 range 13 .. 13;
      Reserved_14 at 0 range 14 .. 14;
      Render_Decompression at 0 range 15 .. 15;
      YUV_Byte_Order at 0 range 16 .. 17;
      Reserved_18 at 0 range 18 .. 18;
      YUV420_Component at 0 range 19 .. 19;
      RGB_Order at 0 range 20 .. 20;
      Key_Enable at 0 range 21 .. 22;
      Pixel_Format at 0 range 23 .. 27;
      Arbitration_Slots at 0 range 28 .. 30;
      Enabled at 0 range 31 .. 31;
   end record;
   function From_Word is new Ada.Unchecked_Conversion
     (Interfaces.Unsigned_32, Control);
   function To_Word is new Ada.Unchecked_Conversion
     (Control, Interfaces.Unsigned_32);
   -- Same PRM: PLANE_OFFSET p789, PLANE_SIZE pp830-831,
   -- PLANE_STRIDE pp835-836. Numeric fields preserve all bit patterns.
   type Bits_12 is mod 2 ** 12 with Size => 12;
   type Bits_13 is mod 2 ** 13 with Size => 13;
   type Bits_20 is mod 2 ** 20 with Size => 20;
   type Bits_8 is mod 2 ** 8 with Size => 8;
   -- PLANE_SURF pp840-841 and PLANE_SURFLIVE p848. SURF bit3
   -- identifies the flip source, not an address bit; SURFLIVE has no such
   -- field. Read whole MMIO words before interpreting these snapshots.
   type Surface_Register is record
      Reserved_0 : Bits_3 := 0;
      Ring_Flip_Source : Bits_1 := 0;
      Reserved_4 : Bits_8 := 0;
      Base_Page : Bits_20 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Surface_Register use record
      Reserved_0 at 0 range 0 .. 2;
      Ring_Flip_Source at 0 range 3 .. 3;
      Reserved_4 at 0 range 4 .. 11;
      Base_Page at 0 range 12 .. 31;
   end record;
   type Live_Surface_Register is record
      Reserved : Bits_12 := 0;
      Base_Page : Bits_20 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Live_Surface_Register use record
      Reserved at 0 range 0 .. 11;
      Base_Page at 0 range 12 .. 31;
   end record;
   function Surface_From_Word is new Ada.Unchecked_Conversion
     (Interfaces.Unsigned_32, Surface_Register);
   function Surface_To_Word is new Ada.Unchecked_Conversion
     (Surface_Register, Interfaces.Unsigned_32);
   function Live_Surface_From_Word is new Ada.Unchecked_Conversion
     (Interfaces.Unsigned_32, Live_Surface_Register);
   type Stride_Register is record
      Cache_Lines : Bits_12 := 0;
      Reserved : Bits_20 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Stride_Register use record
      Cache_Lines at 0 range 0 .. 11;
      Reserved at 0 range 12 .. 31;
   end record;
   type Size_Register is record
      Width_Minus_One : Bits_13 := 0;
      Reserved_13 : Bits_3 := 0;
      Height_Minus_One : Bits_13 := 0;
      Reserved_29 : Bits_3 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Size_Register use record
      Width_Minus_One at 0 range 0 .. 12;
      Reserved_13 at 0 range 13 .. 15;
      Height_Minus_One at 0 range 16 .. 28;
      Reserved_29 at 0 range 29 .. 31;
   end record;
   type Offset_Register is record
      Start_X : Bits_13 := 0;
      Reserved_13 : Bits_3 := 0;
      Start_Y : Bits_13 := 0;
      Reserved_29 : Bits_3 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Offset_Register use record
      Start_X at 0 range 0 .. 12;
      Reserved_13 at 0 range 13 .. 15;
      Start_Y at 0 range 16 .. 28;
      Reserved_29 at 0 range 29 .. 31;
   end record;
   function Stride_From_Word is new Ada.Unchecked_Conversion
     (Interfaces.Unsigned_32, Stride_Register);
   function Size_From_Word is new Ada.Unchecked_Conversion
     (Interfaces.Unsigned_32, Size_Register);
   function Offset_From_Word is new Ada.Unchecked_Conversion
     (Interfaces.Unsigned_32, Offset_Register);
end Intel_GPU_Plane_Control;
