with Interfaces; use Interfaces;
with System;
with Intel_GPU_ADLN_Vertex_Fetch; use Intel_GPU_ADLN_Vertex_Fetch;
package Intel_GPU_ADLN_Triangle with SPARK_Mode is
   -- TGL Vol2a pp1-6 and Vol2d p139. Fixed non-indexed draw only.
   -- These packets do not supply or validate the rest of the pipeline.
   type B26 is mod 2 ** 26 with Size => 26;
   type Topology_Control is record
      Primitive : B6 := 4; -- TRILIST, selected here (not in 3DPRIMITIVE).
      Reserved : B26 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Topology_Control use record
      Primitive at 0 range 0 .. 5; Reserved at 0 range 6 .. 31;
   end record;
   type Draw_Header is record
      Length : B8 := 5;
      Predicate_Enable : B1 := 0;
      UAV_Coherency : B1 := 0;
      Indirect_Parameters : B1 := 0;
      Extended_Parameters : B1 := 0;
      POSH_Enable : B1 := 0;
      Reserved : B3 := 0;
      Subopcode : B8 := 0;
      Opcode : B3 := 3;
      Subtype_Code : B2 := 3;
      Command_Type : B3 := 3;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Draw_Header use record
      Length at 0 range 0 .. 7; Predicate_Enable at 0 range 8 .. 8;
      UAV_Coherency at 0 range 9 .. 9; Indirect_Parameters at 0 range 10 .. 10;
      Extended_Parameters at 0 range 11 .. 11; POSH_Enable at 0 range 12 .. 12;
      Reserved at 0 range 13 .. 15; Subopcode at 0 range 16 .. 23;
      Opcode at 0 range 24 .. 26; Subtype_Code at 0 range 27 .. 28;
      Command_Type at 0 range 29 .. 31;
   end record;
   type Draw_Control is record
      Ignored_Topology : B6 := 0;
      Reserved_Low : B2 := 0;
      Indexed_Access : B1 := 0;
      End_Offset_Enable : B1 := 0;
      Reserved_High : B22 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Draw_Control use record
      Ignored_Topology at 0 range 0 .. 5; Reserved_Low at 0 range 6 .. 7;
      Indexed_Access at 0 range 8 .. 8; End_Offset_Enable at 0 range 9 .. 9;
      Reserved_High at 0 range 10 .. 31;
   end record;
   function Encode (V : Topology_Control) return Unsigned_32 is
     (Unsigned_32 (V.Primitive) or Shift_Left (Unsigned_32 (V.Reserved), 6));
   function Encode (V : Draw_Header) return Unsigned_32 is
     (Unsigned_32 (V.Length) or Shift_Left (Unsigned_32 (V.Predicate_Enable), 8) or
      Shift_Left (Unsigned_32 (V.UAV_Coherency), 9) or Shift_Left (Unsigned_32 (V.Indirect_Parameters), 10) or
      Shift_Left (Unsigned_32 (V.Extended_Parameters), 11) or Shift_Left (Unsigned_32 (V.POSH_Enable), 12) or
      Shift_Left (Unsigned_32 (V.Reserved), 13) or Shift_Left (Unsigned_32 (V.Subopcode), 16) or
      Shift_Left (Unsigned_32 (V.Opcode), 24) or Shift_Left (Unsigned_32 (V.Subtype_Code), 27) or
      Shift_Left (Unsigned_32 (V.Command_Type), 29));
   function Encode (V : Draw_Control) return Unsigned_32 is
     (Unsigned_32 (V.Ignored_Topology) or Shift_Left (Unsigned_32 (V.Reserved_Low), 6) or
      Shift_Left (Unsigned_32 (V.Indexed_Access), 8) or Shift_Left (Unsigned_32 (V.End_Offset_Enable), 9) or
      Shift_Left (Unsigned_32 (V.Reserved_High), 10));
   type Packet is array (Natural range <>) of Unsigned_32;
   Topology : constant Packet :=
     [Intel_GPU_ADLN_Vertex_Fetch.Encode
        (Header'(Length => 0, Subopcode => 16#4B#, others => <>)),
      Encode (Topology_Control'(others => <>))];
   Draw : constant Packet :=
     [Encode (Draw_Header'(others => <>)), Encode (Draw_Control'(others => <>)),
      3, 0, 1, 0, 0]; -- vertices, start vertex, instances, start instance, base.
end Intel_GPU_ADLN_Triangle;
