with Interfaces; use Interfaces;
with System;
package Intel_GPU_ADLN_Null_Buffers with SPARK_Mode is
   -- TGL Vol2a24,42-50,56,125; Vol2d5,38-40,101-108.
   -- Fixed no-attachment sequence. These records describe raw fields, not
   -- admission of arbitrary non-null depth or stencil surfaces.
   type B1 is mod 2 ** 1 with Size => 1;
   type B2 is mod 2 ** 2 with Size => 2;
   type B3 is mod 2 ** 3 with Size => 3;
   type B4 is mod 2 ** 4 with Size => 4;
   type B6 is mod 2 ** 6 with Size => 6;
   type B7 is mod 2 ** 7 with Size => 7;
   type B11 is mod 2 ** 11 with Size => 11;
   type B14 is mod 2 ** 14 with Size => 14;
   type B15 is mod 2 ** 15 with Size => 15;
   type B17 is mod 2 ** 17 with Size => 17;
   type B18 is mod 2 ** 18 with Size => 18;
   type B26 is mod 2 ** 26 with Size => 26;
   type B31 is mod 2 ** 31 with Size => 31;
   type Depth_Control is record
      Pitch : B18 := 0;
      Reserved_18 : B1 := 0;
      Control_Surface : B1 := 0;
      Reserved_20 : B1 := 0;
      Compression : B1 := 0;
      HiZ_Enable : B1 := 0;
      Corner_Texel : B1 := 0;
      Surface_Format : B3 := 0;
      Null_Coherency : B1 := 0;
      Write_Enable : B1 := 0;
      Surface_Type : B3 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Depth_Control use record
      Pitch at 0 range 0 .. 17;
      Reserved_18 at 0 range 18 .. 18;
      Control_Surface at 0 range 19 .. 19;
      Reserved_20 at 0 range 20 .. 20;
      Compression at 0 range 21 .. 21;
      HiZ_Enable at 0 range 22 .. 22;
      Corner_Texel at 0 range 23 .. 23;
      Surface_Format at 0 range 24 .. 26;
      Null_Coherency at 0 range 27 .. 27;
      Write_Enable at 0 range 28 .. 28;
      Surface_Type at 0 range 29 .. 31;
   end record;
   function Encode (V : Depth_Control) return Unsigned_32 is
     (Unsigned_32 (V.Pitch) or
      Shift_Left (Unsigned_32 (V.Reserved_18), 18) or
      Shift_Left (Unsigned_32 (V.Control_Surface), 19) or
      Shift_Left (Unsigned_32 (V.Reserved_20), 20) or
      Shift_Left (Unsigned_32 (V.Compression), 21) or
      Shift_Left (Unsigned_32 (V.HiZ_Enable), 22) or
      Shift_Left (Unsigned_32 (V.Corner_Texel), 23) or
      Shift_Left (Unsigned_32 (V.Surface_Format), 24) or
      Shift_Left (Unsigned_32 (V.Null_Coherency), 27) or
      Shift_Left (Unsigned_32 (V.Write_Enable), 28) or
      Shift_Left (Unsigned_32 (V.Surface_Type), 29));

   type Stencil_Control is record
      Pitch : B17 := 0;
      Reserved_17 : B6 := 0;
      Corner_Texel : B1 := 0;
      Control_Surface : B1 := 0;
      Compression : B1 := 0;
      Reserved_26 : B1 := 0;
      Null_Coherency : B1 := 0;
      Write_Enable : B1 := 0;
      Surface_Type : B3 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Stencil_Control use record
      Pitch at 0 range 0 .. 16;
      Reserved_17 at 0 range 17 .. 22;
      Corner_Texel at 0 range 23 .. 23;
      Control_Surface at 0 range 24 .. 24;
      Compression at 0 range 25 .. 25;
      Reserved_26 at 0 range 26 .. 26;
      Null_Coherency at 0 range 27 .. 27;
      Write_Enable at 0 range 28 .. 28;
      Surface_Type at 0 range 29 .. 31;
   end record;
   function Encode (V : Stencil_Control) return Unsigned_32 is
     (Unsigned_32 (V.Pitch) or
      Shift_Left (Unsigned_32 (V.Reserved_17), 17) or
      Shift_Left (Unsigned_32 (V.Corner_Texel), 23) or
      Shift_Left (Unsigned_32 (V.Control_Surface), 24) or
      Shift_Left (Unsigned_32 (V.Compression), 25) or
      Shift_Left (Unsigned_32 (V.Reserved_26), 26) or
      Shift_Left (Unsigned_32 (V.Null_Coherency), 27) or
      Shift_Left (Unsigned_32 (V.Write_Enable), 28) or
      Shift_Left (Unsigned_32 (V.Surface_Type), 29));

   type Dimensions is record
      Reserved_0 : B1 := 0;
      Width_Minus_One : B14 := 0;
      Reserved_15 : B2 := 0;
      Height_Minus_One : B14 := 0;
      Reserved_31 : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Dimensions use record
      Reserved_0 at 0 range 0 .. 0;
      Width_Minus_One at 0 range 1 .. 14;
      Reserved_15 at 0 range 15 .. 16;
      Height_Minus_One at 0 range 17 .. 30;
      Reserved_31 at 0 range 31 .. 31;
   end record;
   function Encode (V : Dimensions) return Unsigned_32 is
     (Unsigned_32 (V.Reserved_0) or
      Shift_Left (Unsigned_32 (V.Width_Minus_One), 1) or
      Shift_Left (Unsigned_32 (V.Reserved_15), 15) or
      Shift_Left (Unsigned_32 (V.Height_Minus_One), 17) or
      Shift_Left (Unsigned_32 (V.Reserved_31), 31));

   type Array_Control is record
      MOCS : B7 := 0;
      Reserved_7 : B1 := 0;
      Minimum_Element : B11 := 0;
      Reserved_19 : B1 := 0;
      Depth_Minus_One : B11 := 0;
      Reserved_31 : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Array_Control use record
      MOCS at 0 range 0 .. 6;
      Reserved_7 at 0 range 7 .. 7;
      Minimum_Element at 0 range 8 .. 18;
      Reserved_19 at 0 range 19 .. 19;
      Depth_Minus_One at 0 range 20 .. 30;
      Reserved_31 at 0 range 31 .. 31;
   end record;
   function Encode (V : Array_Control) return Unsigned_32 is
     (Unsigned_32 (V.MOCS) or
      Shift_Left (Unsigned_32 (V.Reserved_7), 7) or
      Shift_Left (Unsigned_32 (V.Minimum_Element), 8) or
      Shift_Left (Unsigned_32 (V.Reserved_19), 19) or
      Shift_Left (Unsigned_32 (V.Depth_Minus_One), 20) or
      Shift_Left (Unsigned_32 (V.Reserved_31), 31));

   type Tiling_Control is record
      Reserved_0 : B26 := 0;
      Mip_Tail_Start : B4 := 0;
      Tiled_Mode : B2 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Tiling_Control use record
      Reserved_0 at 0 range 0 .. 25;
      Mip_Tail_Start at 0 range 26 .. 29;
      Tiled_Mode at 0 range 30 .. 31;
   end record;
   function Encode (V : Tiling_Control) return Unsigned_32 is
     (Unsigned_32 (V.Reserved_0) or
      Shift_Left (Unsigned_32 (V.Mip_Tail_Start), 26) or
      Shift_Left (Unsigned_32 (V.Tiled_Mode), 30));

   type View_Control is record
      QPitch_Div_4 : B15 := 0;
      Reserved_15 : B1 := 0;
      LOD : B4 := 0;
      Reserved_20 : B1 := 0;
      Extent_Minus_One : B11 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for View_Control use record
      QPitch_Div_4 at 0 range 0 .. 14;
      Reserved_15 at 0 range 15 .. 15;
      LOD at 0 range 16 .. 19;
      Reserved_20 at 0 range 20 .. 20;
      Extent_Minus_One at 0 range 21 .. 31;
   end record;
   function Encode (V : View_Control) return Unsigned_32 is
     (Unsigned_32 (V.QPitch_Div_4) or
      Shift_Left (Unsigned_32 (V.Reserved_15), 15) or
      Shift_Left (Unsigned_32 (V.LOD), 16) or
      Shift_Left (Unsigned_32 (V.Reserved_20), 20) or
      Shift_Left (Unsigned_32 (V.Extent_Minus_One), 21));

   type HiZ_Control is record
      Pitch : B17 := 0;
      Reserved_17 : B3 := 0;
      Write_Through : B1 := 0;
      Reserved_21 : B1 := 0;
      Tiled_Mode : B2 := 0;
      Reserved_24 : B1 := 0;
      MOCS : B7 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for HiZ_Control use record
      Pitch at 0 range 0 .. 16;
      Reserved_17 at 0 range 17 .. 19;
      Write_Through at 0 range 20 .. 20;
      Reserved_21 at 0 range 21 .. 21;
      Tiled_Mode at 0 range 22 .. 23;
      Reserved_24 at 0 range 24 .. 24;
      MOCS at 0 range 25 .. 31;
   end record;
   function Encode (V : HiZ_Control) return Unsigned_32 is
     (Unsigned_32 (V.Pitch) or
      Shift_Left (Unsigned_32 (V.Reserved_17), 17) or
      Shift_Left (Unsigned_32 (V.Write_Through), 20) or
      Shift_Left (Unsigned_32 (V.Reserved_21), 21) or
      Shift_Left (Unsigned_32 (V.Tiled_Mode), 22) or
      Shift_Left (Unsigned_32 (V.Reserved_24), 24) or
      Shift_Left (Unsigned_32 (V.MOCS), 25));

   type HiZ_QPitch is record
      QPitch_Div_4 : B15 := 0;
      Reserved_15 : B17 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for HiZ_QPitch use record
      QPitch_Div_4 at 0 range 0 .. 14;
      Reserved_15 at 0 range 15 .. 31;
   end record;
   function Encode (V : HiZ_QPitch) return Unsigned_32 is
     (Unsigned_32 (V.QPitch_Div_4) or
      Shift_Left (Unsigned_32 (V.Reserved_15), 15));

   type Clear_Control is record
      Valid : B1 := 0;
      Reserved_1 : B31 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Clear_Control use record
      Valid at 0 range 0 .. 0;
      Reserved_1 at 0 range 1 .. 31;
   end record;
   function Encode (V : Clear_Control) return Unsigned_32 is
     (Unsigned_32 (V.Valid) or
      Shift_Left (Unsigned_32 (V.Reserved_1), 1));

   type Words is array (Natural range 0 .. 23) of Unsigned_32;
   type Image is record
      Valid : Boolean := False;
      Data : Words := [others => 0];
   end record;
   -- Encoded MOCS, not table index; caller must separately admit live policy.
   -- A zero base address alone does NOT designate a null surface.
   -- Whole-batch integration must handle stepping workarounds separately:
   -- Vol2d101 documents an A-step post-sync PIPE_CONTROL after stencil
   -- surface-state changes. This builder does not submit or synchronize.
   function Build (MOCS : Unsigned_32) return Image
     with Post =>
       (Build'Result.Valid = (MOCS in 2 .. 126 and then MOCS mod 2 = 0) and then
        (if not Build'Result.Valid then
          (for all W of Build'Result.Data => W = 0)));
end Intel_GPU_ADLN_Null_Buffers;
