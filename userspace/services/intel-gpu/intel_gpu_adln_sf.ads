with Interfaces; use Interfaces;
with System;
package Intel_GPU_ADLN_SF with SPARK_Mode is
   -- TGL Vol2d94-98. VS-only final geometry stage; HS/DS/GS must be off.
   type B1 is mod 2 ** 1 with Size => 1;
   type B2 is mod 2 ** 2 with Size => 2;
   type B8 is mod 2 ** 8 with Size => 8;
   type B10 is mod 2 ** 10 with Size => 10;
   type B11 is mod 2 ** 11 with Size => 11;
   type B16 is mod 2 ** 16 with Size => 16;
   type B18 is mod 2 ** 18 with Size => 18;
   type Transform_Control is record
      Reserved_0 : B1 := 0;
      Viewport_Transform : B1 := 0;
      Reserved_2 : B8 := 0;
      Statistics : B1 := 0;
      Legacy_Depth_Bias : B1 := 0;
      Line_Width_128ths : B18 := 0;
      Reserved_30 : B2 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Transform_Control use record
      Reserved_0 at 0 range 0 .. 0;
      Viewport_Transform at 0 range 1 .. 1;
      Reserved_2 at 0 range 2 .. 9;
      Statistics at 0 range 10 .. 10;
      Legacy_Depth_Bias at 0 range 11 .. 11;
      Line_Width_128ths at 0 range 12 .. 29;
      Reserved_30 at 0 range 30 .. 31;
   end record;
   function Encode (V : Transform_Control) return Unsigned_32 is
     (Unsigned_32 (V.Reserved_0) or
      Shift_Left (Unsigned_32 (V.Viewport_Transform), 1) or
      Shift_Left (Unsigned_32 (V.Reserved_2), 2) or
      Shift_Left (Unsigned_32 (V.Statistics), 10) or
      Shift_Left (Unsigned_32 (V.Legacy_Depth_Bias), 11) or
      Shift_Left (Unsigned_32 (V.Line_Width_128ths), 12) or
      Shift_Left (Unsigned_32 (V.Reserved_30), 30));
   type Deref_Control is record
      Reserved_0 : B16 := 0;
      Line_Endcap_AA : B2 := 0;
      Reserved_18 : B11 := 0;
      Block_Size : B2 := 0;
      Reserved_31 : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Deref_Control use record
      Reserved_0 at 0 range 0 .. 15;
      Line_Endcap_AA at 0 range 16 .. 17;
      Reserved_18 at 0 range 18 .. 28;
      Block_Size at 0 range 29 .. 30;
      Reserved_31 at 0 range 31 .. 31;
   end record;
   function Encode (V : Deref_Control) return Unsigned_32 is
     (Unsigned_32 (V.Reserved_0) or
      Shift_Left (Unsigned_32 (V.Line_Endcap_AA), 16) or
      Shift_Left (Unsigned_32 (V.Reserved_18), 18) or
      Shift_Left (Unsigned_32 (V.Block_Size), 29) or
      Shift_Left (Unsigned_32 (V.Reserved_31), 31));
   type Point_Control is record
      Width_Eighths : B11 := 0;
      Width_From_State : B1 := 0;
      Subpixel_4Bit : B1 := 0;
      Smooth : B1 := 0;
      True_AA_Line_Distance : B1 := 0;
      Reserved_15 : B10 := 0;
      Fan_Provoking_Vertex : B2 := 0;
      Line_Provoking_Vertex : B2 := 0;
      Triangle_Provoking_Vertex : B2 := 0;
      Last_Pixel : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Point_Control use record
      Width_Eighths at 0 range 0 .. 10;
      Width_From_State at 0 range 11 .. 11;
      Subpixel_4Bit at 0 range 12 .. 12;
      Smooth at 0 range 13 .. 13;
      True_AA_Line_Distance at 0 range 14 .. 14;
      Reserved_15 at 0 range 15 .. 24;
      Fan_Provoking_Vertex at 0 range 25 .. 26;
      Line_Provoking_Vertex at 0 range 27 .. 28;
      Triangle_Provoking_Vertex at 0 range 29 .. 30;
      Last_Pixel at 0 range 31 .. 31;
   end record;
   function Encode (V : Point_Control) return Unsigned_32 is
     (Unsigned_32 (V.Width_Eighths) or
      Shift_Left (Unsigned_32 (V.Width_From_State), 11) or
      Shift_Left (Unsigned_32 (V.Subpixel_4Bit), 12) or
      Shift_Left (Unsigned_32 (V.Smooth), 13) or
      Shift_Left (Unsigned_32 (V.True_AA_Line_Distance), 14) or
      Shift_Left (Unsigned_32 (V.Reserved_15), 15) or
      Shift_Left (Unsigned_32 (V.Fan_Provoking_Vertex), 25) or
      Shift_Left (Unsigned_32 (V.Line_Provoking_Vertex), 27) or
      Shift_Left (Unsigned_32 (V.Triangle_Provoking_Vertex), 29) or
      Shift_Left (Unsigned_32 (V.Last_Pixel), 31));
   type Words is array (Natural range 0 .. 3) of Unsigned_32;
   type Image is record
      Valid : Boolean := False;
      Data : Words := [others => 0];
   end record;
   -- Entries must come from the admitted URB allocation, not PCI defaults.
   -- SF statistics matches our enabled CLIP statistics.
   function Build (VS_Entries : Natural) return Image;
end Intel_GPU_ADLN_SF;
