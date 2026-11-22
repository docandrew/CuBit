with Interfaces; use Interfaces;
with System;
with Intel_GPU_ADLN_Vertex_Fetch;
package Intel_GPU_ADLN_Passthrough with SPARK_Mode is
   -- Intel TGL Vol2d109-116. Fixed no-streamout, no-tessellation probe.
   type B1 is mod 2 ** 1 with Size => 1;
   type B2 is mod 2 ** 2 with Size => 2;
   type B4 is mod 2 ** 4 with Size => 4;
   type B5 is mod 2 ** 5 with Size => 5;
   type B8 is mod 2 ** 8 with Size => 8;
   type B12 is mod 2 ** 12 with Size => 12;
   type B23 is mod 2 ** 23 with Size => 23;
   type Stream_Control is record
      Reserved_0 : B23 := 0;
      Force_Rendering : B2 := 0;
      Statistics : B1 := 0;
      Reorder : B1 := 0;
      Render_Stream : B2 := 0;
      Reserved_29 : B1 := 0;
      Disable_Rendering : B1 := 0;
      Streamout_Enable : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Stream_Control use record
      Reserved_0 at 0 range 0 .. 22;
      Force_Rendering at 0 range 23 .. 24;
      Statistics at 0 range 25 .. 25;
      Reorder at 0 range 26 .. 26;
      Render_Stream at 0 range 27 .. 28;
      Reserved_29 at 0 range 29 .. 29;
      Disable_Rendering at 0 range 30 .. 30;
      Streamout_Enable at 0 range 31 .. 31;
   end record;
   function Encode (V : Stream_Control) return Unsigned_32 is
     (Unsigned_32 (V.Reserved_0) or
      Shift_Left (Unsigned_32 (V.Force_Rendering), 23) or
      Shift_Left (Unsigned_32 (V.Statistics), 25) or
      Shift_Left (Unsigned_32 (V.Reorder), 26) or
      Shift_Left (Unsigned_32 (V.Render_Stream), 27) or
      Shift_Left (Unsigned_32 (V.Reserved_29), 29) or
      Shift_Left (Unsigned_32 (V.Disable_Rendering), 30) or
      Shift_Left (Unsigned_32 (V.Streamout_Enable), 31));

   type Stream_Reads is record
      Length_0_Minus_One : B5 := 0;
      Offset_0 : B1 := 0;
      Reserved_6 : B2 := 0;
      Length_1_Minus_One : B5 := 0;
      Offset_1 : B1 := 0;
      Reserved_14 : B2 := 0;
      Length_2_Minus_One : B5 := 0;
      Offset_2 : B1 := 0;
      Reserved_22 : B2 := 0;
      Length_3_Minus_One : B5 := 0;
      Offset_3 : B1 := 0;
      Reserved_30 : B2 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Stream_Reads use record
      Length_0_Minus_One at 0 range 0 .. 4;
      Offset_0 at 0 range 5 .. 5;
      Reserved_6 at 0 range 6 .. 7;
      Length_1_Minus_One at 0 range 8 .. 12;
      Offset_1 at 0 range 13 .. 13;
      Reserved_14 at 0 range 14 .. 15;
      Length_2_Minus_One at 0 range 16 .. 20;
      Offset_2 at 0 range 21 .. 21;
      Reserved_22 at 0 range 22 .. 23;
      Length_3_Minus_One at 0 range 24 .. 28;
      Offset_3 at 0 range 29 .. 29;
      Reserved_30 at 0 range 30 .. 31;
   end record;
   function Encode (V : Stream_Reads) return Unsigned_32 is
     (Unsigned_32 (V.Length_0_Minus_One) or
      Shift_Left (Unsigned_32 (V.Offset_0), 5) or
      Shift_Left (Unsigned_32 (V.Reserved_6), 6) or
      Shift_Left (Unsigned_32 (V.Length_1_Minus_One), 8) or
      Shift_Left (Unsigned_32 (V.Offset_1), 13) or
      Shift_Left (Unsigned_32 (V.Reserved_14), 14) or
      Shift_Left (Unsigned_32 (V.Length_2_Minus_One), 16) or
      Shift_Left (Unsigned_32 (V.Offset_2), 21) or
      Shift_Left (Unsigned_32 (V.Reserved_22), 22) or
      Shift_Left (Unsigned_32 (V.Length_3_Minus_One), 24) or
      Shift_Left (Unsigned_32 (V.Offset_3), 29) or
      Shift_Left (Unsigned_32 (V.Reserved_30), 30));

   type Pitch_Pair is record
      First_Pitch : B12 := 0;
      Reserved_12 : B4 := 0;
      Second_Pitch : B12 := 0;
      Reserved_28 : B4 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Pitch_Pair use record
      First_Pitch at 0 range 0 .. 11;
      Reserved_12 at 0 range 12 .. 15;
      Second_Pitch at 0 range 16 .. 27;
      Reserved_28 at 0 range 28 .. 31;
   end record;
   function Encode (V : Pitch_Pair) return Unsigned_32 is
     (Unsigned_32 (V.First_Pitch) or
      Shift_Left (Unsigned_32 (V.Reserved_12), 12) or
      Shift_Left (Unsigned_32 (V.Second_Pitch), 16) or
      Shift_Left (Unsigned_32 (V.Reserved_28), 28));

   type Tessellation_Control is record
      Enable_TE : B1 := 0;
      Mode_TE : B2 := 0;
      Reserved_3 : B1 := 0;
      Domain_TE : B2 := 0;
      Reserved_6 : B2 := 0;
      Topology : B2 := 0;
      Reserved_10 : B2 := 0;
      Partitioning : B2 := 0;
      Reserved_14 : B5 := 0;
      Scale_Enable : B1 := 0;
      Factor_Format : B1 := 0;
      Reserved_21 : B1 := 0;
      Patch_Layout : B2 := 0;
      Reserved_24 : B8 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Tessellation_Control use record
      Enable_TE at 0 range 0 .. 0;
      Mode_TE at 0 range 1 .. 2;
      Reserved_3 at 0 range 3 .. 3;
      Domain_TE at 0 range 4 .. 5;
      Reserved_6 at 0 range 6 .. 7;
      Topology at 0 range 8 .. 9;
      Reserved_10 at 0 range 10 .. 11;
      Partitioning at 0 range 12 .. 13;
      Reserved_14 at 0 range 14 .. 18;
      Scale_Enable at 0 range 19 .. 19;
      Factor_Format at 0 range 20 .. 20;
      Reserved_21 at 0 range 21 .. 21;
      Patch_Layout at 0 range 22 .. 23;
      Reserved_24 at 0 range 24 .. 31;
   end record;
   function Encode (V : Tessellation_Control) return Unsigned_32 is
     (Unsigned_32 (V.Enable_TE) or
      Shift_Left (Unsigned_32 (V.Mode_TE), 1) or
      Shift_Left (Unsigned_32 (V.Reserved_3), 3) or
      Shift_Left (Unsigned_32 (V.Domain_TE), 4) or
      Shift_Left (Unsigned_32 (V.Reserved_6), 6) or
      Shift_Left (Unsigned_32 (V.Topology), 8) or
      Shift_Left (Unsigned_32 (V.Reserved_10), 10) or
      Shift_Left (Unsigned_32 (V.Partitioning), 12) or
      Shift_Left (Unsigned_32 (V.Reserved_14), 14) or
      Shift_Left (Unsigned_32 (V.Scale_Enable), 19) or
      Shift_Left (Unsigned_32 (V.Factor_Format), 20) or
      Shift_Left (Unsigned_32 (V.Reserved_21), 21) or
      Shift_Left (Unsigned_32 (V.Patch_Layout), 22) or
      Shift_Left (Unsigned_32 (V.Reserved_24), 24));

   type Words is array (Natural range 0 .. 4) of Unsigned_32;
   -- Streamout OFF is distinct from rendering OFF: preserve forwarding.
   -- Zero pitches designate unbound buffers; no streamout memory writes.
   Streamout_Disabled : constant Words :=
     [Intel_GPU_ADLN_Vertex_Fetch.Encode
        (Intel_GPU_ADLN_Vertex_Fetch.Header'
           (Length => 3, Subopcode => 16#1E#, others => <>)),
      Encode (Stream_Control'(others => <>)),
      Encode (Stream_Reads'(others => <>)),
      Encode (Pitch_Pair'(others => <>)),
      Encode (Pitch_Pair'(others => <>))];
   -- TE disabled makes every other TE field ignored, including float factors.
   -- HS and DS MUST also be disabled before drawing (Vol2d116).
   -- This fragment alone does NOT disable the complete tessellation pipeline.
   Tessellation_Disabled : constant Words :=
     [Intel_GPU_ADLN_Vertex_Fetch.Encode
        (Intel_GPU_ADLN_Vertex_Fetch.Header'
           (Length => 3, Subopcode => 16#1C#, others => <>)),
      Encode (Tessellation_Control'(others => <>)), 0, 0, 0];
end Intel_GPU_ADLN_Passthrough;
