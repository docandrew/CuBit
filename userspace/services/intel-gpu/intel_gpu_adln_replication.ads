with Interfaces; use Interfaces;
with System;
with Intel_GPU_ADLN_Vertex_Fetch;
package Intel_GPU_ADLN_Replication with SPARK_Mode is
   -- Intel TGL Vol2a65 / Vol2d52-55. No multi-viewport replication for
   -- the fixed single-position, single-render-target triangle probe.
   type B4 is mod 2 ** 4 with Size => 4;
   type B12 is mod 2 ** 12 with Size => 12;
   type B16 is mod 2 ** 16 with Size => 16;
   type Control is record
      Count : B4 := 0;
      Reserved_4 : B12 := 0;
      Replica_Mask : B16 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Control use record
      Count at 0 range 0 .. 3;
      Reserved_4 at 0 range 4 .. 15;
      Replica_Mask at 0 range 16 .. 31;
   end record;
   function Encode (V : Control) return Unsigned_32 is
     (Unsigned_32 (V.Count) or Shift_Left (Unsigned_32 (V.Reserved_4), 4) or
      Shift_Left (Unsigned_32 (V.Replica_Mask), 16));
   -- Each group holds eight consecutive viewport or render-target-array
   -- index offsets; the two groups cover replicas0..7 and8..15.
   type Offset_Group is record
      Offset_0 : B4 := 0;
      Offset_1 : B4 := 0;
      Offset_2 : B4 := 0;
      Offset_3 : B4 := 0;
      Offset_4 : B4 := 0;
      Offset_5 : B4 := 0;
      Offset_6 : B4 := 0;
      Offset_7 : B4 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Offset_Group use record
      Offset_0 at 0 range 0 .. 3;
      Offset_1 at 0 range 4 .. 7;
      Offset_2 at 0 range 8 .. 11;
      Offset_3 at 0 range 12 .. 15;
      Offset_4 at 0 range 16 .. 19;
      Offset_5 at 0 range 20 .. 23;
      Offset_6 at 0 range 24 .. 27;
      Offset_7 at 0 range 28 .. 31;
   end record;
   function Encode (V : Offset_Group) return Unsigned_32 is
     (Unsigned_32 (V.Offset_0) or
      Shift_Left (Unsigned_32 (V.Offset_1), 4) or
      Shift_Left (Unsigned_32 (V.Offset_2), 8) or
      Shift_Left (Unsigned_32 (V.Offset_3), 12) or
      Shift_Left (Unsigned_32 (V.Offset_4), 16) or
      Shift_Left (Unsigned_32 (V.Offset_5), 20) or
      Shift_Left (Unsigned_32 (V.Offset_6), 24) or
      Shift_Left (Unsigned_32 (V.Offset_7), 28));
   type Words is array (Natural range 0 .. 5) of Unsigned_32;
   -- Explicitly clear all inherited state, matching Mesa's disable packet.
   Disabled : constant Words :=
     [Intel_GPU_ADLN_Vertex_Fetch.Encode
        (Intel_GPU_ADLN_Vertex_Fetch.Header'
           (Length => 4, Subopcode => 16#6C#, others => <>)),
      Encode (Control'(others => <>)),
      Encode (Offset_Group'(others => <>)),
      Encode (Offset_Group'(others => <>)),
      Encode (Offset_Group'(others => <>)),
      Encode (Offset_Group'(others => <>))];
end Intel_GPU_ADLN_Replication;
