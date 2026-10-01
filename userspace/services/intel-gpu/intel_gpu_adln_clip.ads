with Interfaces; use Interfaces;
with System;
with Intel_GPU_ADLN_Vertex_Fetch;
package Intel_GPU_ADLN_Clip with SPARK_Mode is
   -- Intel TGL Vol2d6-11. Initial triangle probe, viewport0 and RT layer0.
   type B1 is mod 2 ** 1 with Size => 1;
   type B2 is mod 2 ** 2 with Size => 2;
   type B3 is mod 2 ** 3 with Size => 3;
   type B4 is mod 2 ** 4 with Size => 4;
   type B8 is mod 2 ** 8 with Size => 8;
   type B11 is mod 2 ** 11 with Size => 11;
   type Cull_Control is record
      User_Cull_Mask : B8 := 0;
      Reserved_8 : B2 := 0;
      Statistics : B2 := 0;
      Reserved_12 : B4 := 0;
      Force_Mode : B1 := 0;
      Force_Clip_Mask : B1 := 0;
      Early_Cull : B1 := 0;
      Subpixel_4Bit : B1 := 0;
      Force_Cull_Mask : B1 := 0;
      Reserved_21 : B11 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Cull_Control use record
      User_Cull_Mask at 0 range 0 .. 7;
      Reserved_8 at 0 range 8 .. 9;
      Statistics at 0 range 10 .. 11;
      Reserved_12 at 0 range 12 .. 15;
      Force_Mode at 0 range 16 .. 16;
      Force_Clip_Mask at 0 range 17 .. 17;
      Early_Cull at 0 range 18 .. 18;
      Subpixel_4Bit at 0 range 19 .. 19;
      Force_Cull_Mask at 0 range 20 .. 20;
      Reserved_21 at 0 range 21 .. 31;
   end record;
   function Encode (V : Cull_Control) return Unsigned_32 is
     (Unsigned_32 (V.User_Cull_Mask) or
      Shift_Left (Unsigned_32 (V.Reserved_8), 8) or
      Shift_Left (Unsigned_32 (V.Statistics), 10) or
      Shift_Left (Unsigned_32 (V.Reserved_12), 12) or
      Shift_Left (Unsigned_32 (V.Force_Mode), 16) or
      Shift_Left (Unsigned_32 (V.Force_Clip_Mask), 17) or
      Shift_Left (Unsigned_32 (V.Early_Cull), 18) or
      Shift_Left (Unsigned_32 (V.Subpixel_4Bit), 19) or
      Shift_Left (Unsigned_32 (V.Force_Cull_Mask), 20) or
      Shift_Left (Unsigned_32 (V.Reserved_21), 21));
   type Clip_Control is record
      Fan_Provoking_Vertex : B2 := 0;
      Line_Provoking_Vertex : B2 := 0;
      Triangle_Provoking_Vertex : B2 := 0;
      Reserved_6 : B2 := 0;
      Nonperspective_Bary : B1 := 0;
      Disable_Perspective_Divide : B1 := 0;
      Reserved_10 : B3 := 0;
      Mode : B3 := 0;
      User_Clip_Mask : B8 := 0;
      Reserved_24 : B2 := 0;
      Guardband_Test : B1 := 0;
      Reserved_27 : B1 := 0;
      Viewport_XY_Test : B1 := 0;
      Reserved_29 : B1 := 0;
      API_Mode : B1 := 0;
      Enable : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Clip_Control use record
      Fan_Provoking_Vertex at 0 range 0 .. 1;
      Line_Provoking_Vertex at 0 range 2 .. 3;
      Triangle_Provoking_Vertex at 0 range 4 .. 5;
      Reserved_6 at 0 range 6 .. 7;
      Nonperspective_Bary at 0 range 8 .. 8;
      Disable_Perspective_Divide at 0 range 9 .. 9;
      Reserved_10 at 0 range 10 .. 12;
      Mode at 0 range 13 .. 15;
      User_Clip_Mask at 0 range 16 .. 23;
      Reserved_24 at 0 range 24 .. 25;
      Guardband_Test at 0 range 26 .. 26;
      Reserved_27 at 0 range 27 .. 27;
      Viewport_XY_Test at 0 range 28 .. 28;
      Reserved_29 at 0 range 29 .. 29;
      API_Mode at 0 range 30 .. 30;
      Enable at 0 range 31 .. 31;
   end record;
   function Encode (V : Clip_Control) return Unsigned_32 is
     (Unsigned_32 (V.Fan_Provoking_Vertex) or
      Shift_Left (Unsigned_32 (V.Line_Provoking_Vertex), 2) or
      Shift_Left (Unsigned_32 (V.Triangle_Provoking_Vertex), 4) or
      Shift_Left (Unsigned_32 (V.Reserved_6), 6) or
      Shift_Left (Unsigned_32 (V.Nonperspective_Bary), 8) or
      Shift_Left (Unsigned_32 (V.Disable_Perspective_Divide), 9) or
      Shift_Left (Unsigned_32 (V.Reserved_10), 10) or
      Shift_Left (Unsigned_32 (V.Mode), 13) or
      Shift_Left (Unsigned_32 (V.User_Clip_Mask), 16) or
      Shift_Left (Unsigned_32 (V.Reserved_24), 24) or
      Shift_Left (Unsigned_32 (V.Guardband_Test), 26) or
      Shift_Left (Unsigned_32 (V.Reserved_27), 27) or
      Shift_Left (Unsigned_32 (V.Viewport_XY_Test), 28) or
      Shift_Left (Unsigned_32 (V.Reserved_29), 29) or
      Shift_Left (Unsigned_32 (V.API_Mode), 30) or
      Shift_Left (Unsigned_32 (V.Enable), 31));
   type Viewport_Control is record
      Maximum_Viewport_Index : B4 := 0;
      Reserved_4 : B1 := 0;
      Force_Zero_RT_Index : B1 := 0;
      Maximum_Point_Width_Eighths : B11 := 0;
      Minimum_Point_Width_Eighths : B11 := 0;
      Reserved_28 : B4 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Viewport_Control use record
      Maximum_Viewport_Index at 0 range 0 .. 3;
      Reserved_4 at 0 range 4 .. 4;
      Force_Zero_RT_Index at 0 range 5 .. 5;
      Maximum_Point_Width_Eighths at 0 range 6 .. 16;
      Minimum_Point_Width_Eighths at 0 range 17 .. 27;
      Reserved_28 at 0 range 28 .. 31;
   end record;
   function Encode (V : Viewport_Control) return Unsigned_32 is
     (Unsigned_32 (V.Maximum_Viewport_Index) or
      Shift_Left (Unsigned_32 (V.Reserved_4), 4) or
      Shift_Left (Unsigned_32 (V.Force_Zero_RT_Index), 5) or
      Shift_Left (Unsigned_32 (V.Maximum_Point_Width_Eighths), 6) or
      Shift_Left (Unsigned_32 (V.Minimum_Point_Width_Eighths), 17) or
      Shift_Left (Unsigned_32 (V.Reserved_28), 28));
   type Words is array (Natural range 0 .. 3) of Unsigned_32;
   -- Keep perspective division enabled and use normal clipping. Viewport XY
   -- test without guardband test clips at [-1,1]. No user clip distances.
   -- No early backface culling; RASTER culling is configured separately.
   Initial : constant Words :=
     [Intel_GPU_ADLN_Vertex_Fetch.Encode
        (Intel_GPU_ADLN_Vertex_Fetch.Header'
           (Length => 2, Subopcode => 16#12#, others => <>)),
      Encode (Cull_Control'(Statistics => 1, others => <>)),
      Encode (Clip_Control'(Enable => 1, Viewport_XY_Test => 1, others => <>)),
      Encode (Viewport_Control'(Force_Zero_RT_Index => 1,
         Minimum_Point_Width_Eighths => 1, Maximum_Point_Width_Eighths => 2047,
         others => <>))];
end Intel_GPU_ADLN_Clip;
