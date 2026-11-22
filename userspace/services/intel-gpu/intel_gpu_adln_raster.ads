with Interfaces; use Interfaces;
with System;
with Intel_GPU_ADLN_Vertex_Fetch;
package Intel_GPU_ADLN_Raster with SPARK_Mode is
   -- TGL Vol2d77-81. Fixed filled-triangle, single-sample probe.
   type B1 is mod 2 ** 1 with Size => 1;
   type B2 is mod 2 ** 2 with Size => 2;
   type B3 is mod 2 ** 3 with Size => 3;
   type B5 is mod 2 ** 5 with Size => 5;
   type Control is record
      Near_Clip : B1 := 0;
      Scissor : B1 := 0;
      Antialias_Lines : B1 := 0;
      Back_Fill : B2 := 0;
      Front_Fill : B2 := 0;
      Depth_Offset_Point : B1 := 0;
      Depth_Offset_Wire : B1 := 0;
      Depth_Offset_Solid : B1 := 0;
      MS_Mode : B2 := 0;
      MS_Enable : B1 := 0;
      Smooth_Point : B1 := 0;
      Force_MS : B1 := 0;
      Reserved_15 : B1 := 0;
      Cull_Mode : B2 := 0;
      Forced_Samples : B3 := 0;
      Front_CCW : B1 := 0;
      API_Mode : B2 := 0;
      Conservative : B1 := 0;
      Reserved_25 : B1 := 0;
      Far_Clip : B1 := 0;
      Reserved_27 : B5 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Control use record
      Near_Clip at 0 range 0 .. 0;
      Scissor at 0 range 1 .. 1;
      Antialias_Lines at 0 range 2 .. 2;
      Back_Fill at 0 range 3 .. 4;
      Front_Fill at 0 range 5 .. 6;
      Depth_Offset_Point at 0 range 7 .. 7;
      Depth_Offset_Wire at 0 range 8 .. 8;
      Depth_Offset_Solid at 0 range 9 .. 9;
      MS_Mode at 0 range 10 .. 11;
      MS_Enable at 0 range 12 .. 12;
      Smooth_Point at 0 range 13 .. 13;
      Force_MS at 0 range 14 .. 14;
      Reserved_15 at 0 range 15 .. 15;
      Cull_Mode at 0 range 16 .. 17;
      Forced_Samples at 0 range 18 .. 20;
      Front_CCW at 0 range 21 .. 21;
      API_Mode at 0 range 22 .. 23;
      Conservative at 0 range 24 .. 24;
      Reserved_25 at 0 range 25 .. 25;
      Far_Clip at 0 range 26 .. 26;
      Reserved_27 at 0 range 27 .. 31;
   end record;
   function Encode (V : Control) return Unsigned_32 is
     (Unsigned_32 (V.Near_Clip) or
      Shift_Left (Unsigned_32 (V.Scissor), 1) or
      Shift_Left (Unsigned_32 (V.Antialias_Lines), 2) or
      Shift_Left (Unsigned_32 (V.Back_Fill), 3) or
      Shift_Left (Unsigned_32 (V.Front_Fill), 5) or
      Shift_Left (Unsigned_32 (V.Depth_Offset_Point), 7) or
      Shift_Left (Unsigned_32 (V.Depth_Offset_Wire), 8) or
      Shift_Left (Unsigned_32 (V.Depth_Offset_Solid), 9) or
      Shift_Left (Unsigned_32 (V.MS_Mode), 10) or
      Shift_Left (Unsigned_32 (V.MS_Enable), 12) or
      Shift_Left (Unsigned_32 (V.Smooth_Point), 13) or
      Shift_Left (Unsigned_32 (V.Force_MS), 14) or
      Shift_Left (Unsigned_32 (V.Reserved_15), 15) or
      Shift_Left (Unsigned_32 (V.Cull_Mode), 16) or
      Shift_Left (Unsigned_32 (V.Forced_Samples), 18) or
      Shift_Left (Unsigned_32 (V.Front_CCW), 21) or
      Shift_Left (Unsigned_32 (V.API_Mode), 22) or
      Shift_Left (Unsigned_32 (V.Conservative), 24) or
      Shift_Left (Unsigned_32 (V.Reserved_25), 25) or
      Shift_Left (Unsigned_32 (V.Far_Clip), 26) or
      Shift_Left (Unsigned_32 (V.Reserved_27), 27));
   type Depth_Offset_State is record
      Constant_Bits : Unsigned_32 := 0;
      Scale_Bits : Unsigned_32 := 0;
      Clamp_Bits : Unsigned_32 := 0;
   end record with Size => 96, Bit_Order => System.Low_Order_First;
   for Depth_Offset_State use record
      Constant_Bits at 0 range 0 .. 31;
      Scale_Bits at 4 range 0 .. 31;
      Clamp_Bits at 8 range 0 .. 31;
   end record;
   type Offset_Words is array (Natural range 0 .. 2) of Unsigned_32;
   function Encode (V : Depth_Offset_State) return Offset_Words is
     [V.Constant_Bits, V.Scale_Bits, V.Clamp_Bits];
   type Words is array (Natural range 0 .. 4) of Unsigned_32;
   -- Cull_Mode zero rejects ALL triangles, so select NONE explicitly.
   -- Scissor is off for this contained fixed triangle; do not use this
   -- profile for arbitrary application geometry without scissor setup.
   Initial : constant Words :=
     [Intel_GPU_ADLN_Vertex_Fetch.Encode
        (Intel_GPU_ADLN_Vertex_Fetch.Header'
           (Length => 3, Subopcode => 16#50#, others => <>)),
      Encode (Control'(Cull_Mode => 1, Front_CCW => 1,
         Near_Clip => 1, Far_Clip => 1, others => <>)),
      0, 0, 0];
end Intel_GPU_ADLN_Raster;
