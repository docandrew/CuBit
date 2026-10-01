with Interfaces; use Interfaces;
with System;
with Intel_GPU_ADLN_Vertex_Fetch;
package Intel_GPU_ADLN_Sample_Pattern with SPARK_Mode is
   -- Intel TGL Vol2a94-103. Coordinates are unsigned U0.4 (sixteenths).
   -- Standard Vulkan positions match Mesa vk_standard_sample_locations.c.
   type B4 is mod 2 ** 4 with Size => 4;
   type B8 is mod 2 ** 8 with Size => 8;
   type Four_Samples is record
      Y0 : B4 := 0;
      X0 : B4 := 0;
      Y1 : B4 := 0;
      X1 : B4 := 0;
      Y2 : B4 := 0;
      X2 : B4 := 0;
      Y3 : B4 := 0;
      X3 : B4 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Four_Samples use record
      Y0 at 0 range 0 .. 3;
      X0 at 0 range 4 .. 7;
      Y1 at 0 range 8 .. 11;
      X1 at 0 range 12 .. 15;
      Y2 at 0 range 16 .. 19;
      X2 at 0 range 20 .. 23;
      Y3 at 0 range 24 .. 27;
      X3 at 0 range 28 .. 31;
   end record;
   function Encode (V : Four_Samples) return Unsigned_32 is
     (Unsigned_32 (V.Y0) or
      Shift_Left (Unsigned_32 (V.X0), 4) or
      Shift_Left (Unsigned_32 (V.Y1), 8) or
      Shift_Left (Unsigned_32 (V.X1), 12) or
      Shift_Left (Unsigned_32 (V.Y2), 16) or
      Shift_Left (Unsigned_32 (V.X2), 20) or
      Shift_Left (Unsigned_32 (V.Y3), 24) or
      Shift_Left (Unsigned_32 (V.X3), 28));
   type Small_Modes is record
      Y2_0 : B4 := 0;
      X2_0 : B4 := 0;
      Y2_1 : B4 := 0;
      X2_1 : B4 := 0;
      Y1_0 : B4 := 0;
      X1_0 : B4 := 0;
      Reserved_24 : B8 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Small_Modes use record
      Y2_0 at 0 range 0 .. 3;
      X2_0 at 0 range 4 .. 7;
      Y2_1 at 0 range 8 .. 11;
      X2_1 at 0 range 12 .. 15;
      Y1_0 at 0 range 16 .. 19;
      X1_0 at 0 range 20 .. 23;
      Reserved_24 at 0 range 24 .. 31;
   end record;
   function Encode (V : Small_Modes) return Unsigned_32 is
     (Unsigned_32 (V.Y2_0) or
      Shift_Left (Unsigned_32 (V.X2_0), 4) or
      Shift_Left (Unsigned_32 (V.Y2_1), 8) or
      Shift_Left (Unsigned_32 (V.X2_1), 12) or
      Shift_Left (Unsigned_32 (V.Y1_0), 16) or
      Shift_Left (Unsigned_32 (V.X1_0), 20) or
      Shift_Left (Unsigned_32 (V.Reserved_24), 24));
   type Words is array (Natural range 0 .. 8) of Unsigned_32;
   Standard : constant Words :=
     [Intel_GPU_ADLN_Vertex_Fetch.Encode
        (Intel_GPU_ADLN_Vertex_Fetch.Header'
           (Length => 7, Opcode => 1, Subopcode => 16#1C#, others => <>)),
      Encode (Four_Samples'(Y0 => 9, X0 => 9, Y1 => 5, X1 => 7, Y2 => 10, X2 => 5, Y3 => 7, X3 => 12)),
      Encode (Four_Samples'(Y0 => 6, X0 => 3, Y1 => 13, X1 => 10, Y2 => 11, X2 => 13, Y3 => 3, X3 => 11)),
      Encode (Four_Samples'(Y0 => 14, X0 => 6, Y1 => 1, X1 => 8, Y2 => 2, X2 => 4, Y3 => 12, X3 => 2)),
      Encode (Four_Samples'(Y0 => 8, X0 => 0, Y1 => 4, X1 => 15, Y2 => 15, X2 => 14, Y3 => 0, X3 => 1)),
      Encode (Four_Samples'(Y0 => 13, X0 => 3, Y1 => 7, X1 => 1, Y2 => 15, X2 => 11, Y3 => 1, X3 => 15)),
      Encode (Four_Samples'(Y0 => 5, X0 => 9, Y1 => 11, X1 => 7, Y2 => 9, X2 => 13, Y3 => 3, X3 => 5)),
      Encode (Four_Samples'(Y0 => 2, X0 => 6, Y1 => 6, X1 => 14, Y2 => 10, X2 => 2, Y3 => 14, X3 => 10)),
      Encode (Small_Modes'(Y2_0 => 12, X2_0 => 12, Y2_1 => 4,
        X2_1 => 4, Y1_0 => 8, X1_0 => 8, Reserved_24 => 0))];
end Intel_GPU_ADLN_Sample_Pattern;
