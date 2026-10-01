with Interfaces; use Interfaces;
with System;
package Intel_GPU_ADLN_Offscreen_Surface with SPARK_Mode is
   -- Fixed64x64 BGRA8 linear RGB surface; Intel TGL Vol2d RENDER_SURFACE_STATE.
   -- Numeric copied state, not MMIO. YUV/auxiliary/compressed interpretations
   -- are unsupported. Nothing here authorizes backing or publishes a surface.
   type Bits_1 is mod 2 ** 1 with Size => 1;
   type Bits_2 is mod 2 ** 2 with Size => 2;
   type Bits_3 is mod 2 ** 3 with Size => 3;
   type Bits_4 is mod 2 ** 4 with Size => 4;
   type Bits_5 is mod 2 ** 5 with Size => 5;
   type Bits_6 is mod 2 ** 6 with Size => 6;
   type Bits_7 is mod 2 ** 7 with Size => 7;
   type Bits_9 is mod 2 ** 9 with Size => 9;
   type Bits_11 is mod 2 ** 11 with Size => 11;
   type Bits_12 is mod 2 ** 12 with Size => 12;
   type Bits_14 is mod 2 ** 14 with Size => 14;
   type Bits_15 is mod 2 ** 15 with Size => 15;
   type Bits_18 is mod 2 ** 18 with Size => 18;
   type DW0_Fields is record
      Cube_Faces : Bits_6 := 0;
      Media_Boundary : Bits_2 := 0;
      Render_Cache_Mode : Bits_1 := 0;
      Sampler_Bypass_Disable : Bits_1 := 0;
      Line_Offset : Bits_1 := 0;
      Line_Stride : Bits_1 := 0;
      Tile_Mode : Bits_2 := 0;
      Horizontal_Alignment : Bits_2 := 0;
      Vertical_Alignment : Bits_2 := 0;
      Surface_Format : Bits_9 := 0;
      Reserved_27 : Bits_1 := 0;
      Surface_Array : Bits_1 := 0;
      Surface_Type : Bits_3 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for DW0_Fields use record
      Cube_Faces at 0 range 0 .. 5;
      Media_Boundary at 0 range 6 .. 7;
      Render_Cache_Mode at 0 range 8 .. 8;
      Sampler_Bypass_Disable at 0 range 9 .. 9;
      Line_Offset at 0 range 10 .. 10;
      Line_Stride at 0 range 11 .. 11;
      Tile_Mode at 0 range 12 .. 13;
      Horizontal_Alignment at 0 range 14 .. 15;
      Vertical_Alignment at 0 range 16 .. 17;
      Surface_Format at 0 range 18 .. 26;
      Reserved_27 at 0 range 27 .. 27;
      Surface_Array at 0 range 28 .. 28;
      Surface_Type at 0 range 29 .. 31;
   end record;
   function Encode (Value : DW0_Fields) return Unsigned_32 is
     (Unsigned_32 (Value.Cube_Faces) or
      Shift_Left (Unsigned_32 (Value.Media_Boundary), 6) or
      Shift_Left (Unsigned_32 (Value.Render_Cache_Mode), 8) or
      Shift_Left (Unsigned_32 (Value.Sampler_Bypass_Disable), 9) or
      Shift_Left (Unsigned_32 (Value.Line_Offset), 10) or
      Shift_Left (Unsigned_32 (Value.Line_Stride), 11) or
      Shift_Left (Unsigned_32 (Value.Tile_Mode), 12) or
      Shift_Left (Unsigned_32 (Value.Horizontal_Alignment), 14) or
      Shift_Left (Unsigned_32 (Value.Vertical_Alignment), 16) or
      Shift_Left (Unsigned_32 (Value.Surface_Format), 18) or
      Shift_Left (Unsigned_32 (Value.Reserved_27), 27) or
      Shift_Left (Unsigned_32 (Value.Surface_Array), 28) or
      Shift_Left (Unsigned_32 (Value.Surface_Type), 29));
   type DW1_Fields is record
      QPitch : Bits_15 := 0;
      Sample_Tap_Discard_Disable : Bits_1 := 0;
      Reserved_16 : Bits_1 := 0;
      Double_Fetch_Disable : Bits_1 := 0;
      Corner_Texel : Bits_1 := 0;
      Base_Mip_Level : Bits_5 := 0;
      MOCS : Bits_7 := 0;
      Unorm_Path : Bits_1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for DW1_Fields use record
      QPitch at 0 range 0 .. 14;
      Sample_Tap_Discard_Disable at 0 range 15 .. 15;
      Reserved_16 at 0 range 16 .. 16;
      Double_Fetch_Disable at 0 range 17 .. 17;
      Corner_Texel at 0 range 18 .. 18;
      Base_Mip_Level at 0 range 19 .. 23;
      MOCS at 0 range 24 .. 30;
      Unorm_Path at 0 range 31 .. 31;
   end record;
   function Encode (Value : DW1_Fields) return Unsigned_32 is
     (Unsigned_32 (Value.QPitch) or
      Shift_Left (Unsigned_32 (Value.Sample_Tap_Discard_Disable), 15) or
      Shift_Left (Unsigned_32 (Value.Reserved_16), 16) or
      Shift_Left (Unsigned_32 (Value.Double_Fetch_Disable), 17) or
      Shift_Left (Unsigned_32 (Value.Corner_Texel), 18) or
      Shift_Left (Unsigned_32 (Value.Base_Mip_Level), 19) or
      Shift_Left (Unsigned_32 (Value.MOCS), 24) or
      Shift_Left (Unsigned_32 (Value.Unorm_Path), 31));
   type DW2_Fields is record
      Width_Minus_One : Bits_14 := 0;
      Reserved_14 : Bits_2 := 0;
      Height_Minus_One : Bits_14 := 0;
      Reserved_30 : Bits_1 := 0;
      Depth_Stencil : Bits_1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for DW2_Fields use record
      Width_Minus_One at 0 range 0 .. 13;
      Reserved_14 at 0 range 14 .. 15;
      Height_Minus_One at 0 range 16 .. 29;
      Reserved_30 at 0 range 30 .. 30;
      Depth_Stencil at 0 range 31 .. 31;
   end record;
   function Encode (Value : DW2_Fields) return Unsigned_32 is
     (Unsigned_32 (Value.Width_Minus_One) or
      Shift_Left (Unsigned_32 (Value.Reserved_14), 14) or
      Shift_Left (Unsigned_32 (Value.Height_Minus_One), 16) or
      Shift_Left (Unsigned_32 (Value.Reserved_30), 30) or
      Shift_Left (Unsigned_32 (Value.Depth_Stencil), 31));
   type DW3_Fields is record
      Pitch_Minus_One : Bits_18 := 0;
      Reserved_18 : Bits_1 := 0;
      Standard_Tiling_Extensions : Bits_1 := 0;
      Tile_Address_Mode : Bits_1 := 0;
      Depth_Minus_One : Bits_11 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for DW3_Fields use record
      Pitch_Minus_One at 0 range 0 .. 17;
      Reserved_18 at 0 range 18 .. 18;
      Standard_Tiling_Extensions at 0 range 19 .. 19;
      Tile_Address_Mode at 0 range 20 .. 20;
      Depth_Minus_One at 0 range 21 .. 31;
   end record;
   function Encode (Value : DW3_Fields) return Unsigned_32 is
     (Unsigned_32 (Value.Pitch_Minus_One) or
      Shift_Left (Unsigned_32 (Value.Reserved_18), 18) or
      Shift_Left (Unsigned_32 (Value.Standard_Tiling_Extensions), 19) or
      Shift_Left (Unsigned_32 (Value.Tile_Address_Mode), 20) or
      Shift_Left (Unsigned_32 (Value.Depth_Minus_One), 21));
   type DW4_Fields is record
      Palette_Index : Bits_3 := 0;
      Multisamples : Bits_3 := 0;
      Multisample_Storage : Bits_1 := 0;
      View_Extent : Bits_11 := 0;
      Minimum_Array_Element : Bits_11 := 0;
      Rotation : Bits_2 := 0;
      Decompress_In_L3 : Bits_1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for DW4_Fields use record
      Palette_Index at 0 range 0 .. 2;
      Multisamples at 0 range 3 .. 5;
      Multisample_Storage at 0 range 6 .. 6;
      View_Extent at 0 range 7 .. 17;
      Minimum_Array_Element at 0 range 18 .. 28;
      Rotation at 0 range 29 .. 30;
      Decompress_In_L3 at 0 range 31 .. 31;
   end record;
   function Encode (Value : DW4_Fields) return Unsigned_32 is
     (Unsigned_32 (Value.Palette_Index) or
      Shift_Left (Unsigned_32 (Value.Multisamples), 3) or
      Shift_Left (Unsigned_32 (Value.Multisample_Storage), 6) or
      Shift_Left (Unsigned_32 (Value.View_Extent), 7) or
      Shift_Left (Unsigned_32 (Value.Minimum_Array_Element), 18) or
      Shift_Left (Unsigned_32 (Value.Rotation), 29) or
      Shift_Left (Unsigned_32 (Value.Decompress_In_L3), 31));
   type DW5_Fields is record
      Mip_Count : Bits_4 := 0;
      Minimum_LOD : Bits_4 := 0;
      Mip_Tail_Start : Bits_4 := 0;
      Reserved_12 : Bits_2 := 0;
      Coherency : Bits_1 := 0;
      Reserved_15 : Bits_3 := 0;
      Tiled_Resource_Mode : Bits_2 := 0;
      EWA_Disable : Bits_1 := 0;
      Y_Offset : Bits_3 := 0;
      Reserved_24 : Bits_1 := 0;
      X_Offset : Bits_7 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for DW5_Fields use record
      Mip_Count at 0 range 0 .. 3;
      Minimum_LOD at 0 range 4 .. 7;
      Mip_Tail_Start at 0 range 8 .. 11;
      Reserved_12 at 0 range 12 .. 13;
      Coherency at 0 range 14 .. 14;
      Reserved_15 at 0 range 15 .. 17;
      Tiled_Resource_Mode at 0 range 18 .. 19;
      EWA_Disable at 0 range 20 .. 20;
      Y_Offset at 0 range 21 .. 23;
      Reserved_24 at 0 range 24 .. 24;
      X_Offset at 0 range 25 .. 31;
   end record;
   function Encode (Value : DW5_Fields) return Unsigned_32 is
     (Unsigned_32 (Value.Mip_Count) or
      Shift_Left (Unsigned_32 (Value.Minimum_LOD), 4) or
      Shift_Left (Unsigned_32 (Value.Mip_Tail_Start), 8) or
      Shift_Left (Unsigned_32 (Value.Reserved_12), 12) or
      Shift_Left (Unsigned_32 (Value.Coherency), 14) or
      Shift_Left (Unsigned_32 (Value.Reserved_15), 15) or
      Shift_Left (Unsigned_32 (Value.Tiled_Resource_Mode), 18) or
      Shift_Left (Unsigned_32 (Value.EWA_Disable), 20) or
      Shift_Left (Unsigned_32 (Value.Y_Offset), 21) or
      Shift_Left (Unsigned_32 (Value.Reserved_24), 24) or
      Shift_Left (Unsigned_32 (Value.X_Offset), 25));
   type DW6_Fields is record
      Auxiliary_Mode : Bits_3 := 0;
      Auxiliary_Pitch : Bits_9 := 0;
      Reserved_12 : Bits_3 := 0;
      YUV_Interpolation : Bits_1 := 0;
      Auxiliary_QPitch : Bits_15 := 0;
      Separate_UV : Bits_1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for DW6_Fields use record
      Auxiliary_Mode at 0 range 0 .. 2;
      Auxiliary_Pitch at 0 range 3 .. 11;
      Reserved_12 at 0 range 12 .. 14;
      YUV_Interpolation at 0 range 15 .. 15;
      Auxiliary_QPitch at 0 range 16 .. 30;
      Separate_UV at 0 range 31 .. 31;
   end record;
   function Encode (Value : DW6_Fields) return Unsigned_32 is
     (Unsigned_32 (Value.Auxiliary_Mode) or
      Shift_Left (Unsigned_32 (Value.Auxiliary_Pitch), 3) or
      Shift_Left (Unsigned_32 (Value.Reserved_12), 12) or
      Shift_Left (Unsigned_32 (Value.YUV_Interpolation), 15) or
      Shift_Left (Unsigned_32 (Value.Auxiliary_QPitch), 16) or
      Shift_Left (Unsigned_32 (Value.Separate_UV), 31));
   type DW7_Fields is record
      Resource_Min_LOD : Bits_12 := 0;
      Reserved_12 : Bits_4 := 0;
      Channel_Alpha : Bits_3 := 0;
      Channel_Blue : Bits_3 := 0;
      Channel_Green : Bits_3 := 0;
      Channel_Red : Bits_3 := 0;
      Reserved_28 : Bits_2 := 0;
      Compression_Enable : Bits_1 := 0;
      Compression_Mode : Bits_1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for DW7_Fields use record
      Resource_Min_LOD at 0 range 0 .. 11;
      Reserved_12 at 0 range 12 .. 15;
      Channel_Alpha at 0 range 16 .. 18;
      Channel_Blue at 0 range 19 .. 21;
      Channel_Green at 0 range 22 .. 24;
      Channel_Red at 0 range 25 .. 27;
      Reserved_28 at 0 range 28 .. 29;
      Compression_Enable at 0 range 30 .. 30;
      Compression_Mode at 0 range 31 .. 31;
   end record;
   function Encode (Value : DW7_Fields) return Unsigned_32 is
     (Unsigned_32 (Value.Resource_Min_LOD) or
      Shift_Left (Unsigned_32 (Value.Reserved_12), 12) or
      Shift_Left (Unsigned_32 (Value.Channel_Alpha), 16) or
      Shift_Left (Unsigned_32 (Value.Channel_Blue), 19) or
      Shift_Left (Unsigned_32 (Value.Channel_Green), 22) or
      Shift_Left (Unsigned_32 (Value.Channel_Red), 25) or
      Shift_Left (Unsigned_32 (Value.Reserved_28), 28) or
      Shift_Left (Unsigned_32 (Value.Compression_Enable), 30) or
      Shift_Left (Unsigned_32 (Value.Compression_Mode), 31));
   -- TGL Vol2d printed998: SW-generated binding table,64-byte offsets.
   type Bits_26 is mod 2 ** 26 with Size => 26;
   type Binding_Entry is record
      Reserved : Bits_6 := 0;
      Surface_Offset_64 : Bits_26 := 1;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Binding_Entry use record
      Reserved at 0 range 0 .. 5;
      Surface_Offset_64 at 0 range 6 .. 31;
   end record;
   function Encode_Binding (Value : Binding_Entry) return Unsigned_32 is
     (Unsigned_32 (Value.Reserved) or Shift_Left (Unsigned_32 (Value.Surface_Offset_64), 6));
   type State_Words is array (Natural range 0 .. 15) of Unsigned_32;
   type Image is record
      Valid : Boolean := False;
      Words : State_Words := [others => 0];
   end record;
   -- MOCS is the encoded7-bit field, not an index. Caller must admit its
   -- cache policy against installed hardware state. No default policy is chosen.
   function Build (MOCS : Unsigned_32) return Image;
end Intel_GPU_ADLN_Offscreen_Surface;
