with Interfaces; use Interfaces;
with System;
package Intel_GPU_ADLN_State_Pointers with SPARK_Mode is
   -- TGL Vol2a13-17/104-108; Vol2d2/83. Surface/dynamic-base relative,
   -- never CPU pointers. Binding pool MUST be disabled for this probe.
   -- Binding layout is the 32B-aligned software table interpretation.
   -- Zero offset is invariant under the documented alignment choices.
   type B1 is mod 2 ** 1 with Size => 1;
   type B2 is mod 2 ** 2 with Size => 2;
   type B3 is mod 2 ** 3 with Size => 3;
   type B5 is mod 2 ** 5 with Size => 5;
   type B7 is mod 2 ** 7 with Size => 7;
   type B8 is mod 2 ** 8 with Size => 8;
   type B11 is mod 2 ** 11 with Size => 11;
   type B16 is mod 2 ** 16 with Size => 16;
   type B27 is mod 2 ** 27 with Size => 27;
   type Header is record
      Length : B8 := 0;
      Reserved_8 : B7 := 0;
      POSH_Optimization : B1 := 0;
      Subopcode : B8 := 0;
      Opcode : B3 := 0;
      Command_Subtype : B2 := 3;
      Command_Type : B3 := 3;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Header use record
      Length at 0 range 0 .. 7;
      Reserved_8 at 0 range 8 .. 14;
      POSH_Optimization at 0 range 15 .. 15;
      Subopcode at 0 range 16 .. 23;
      Opcode at 0 range 24 .. 26;
      Command_Subtype at 0 range 27 .. 28;
      Command_Type at 0 range 29 .. 31;
   end record;
   function Encode (V : Header) return Unsigned_32 is
     (Unsigned_32 (V.Length) or
      Shift_Left (Unsigned_32 (V.Reserved_8), 8) or
      Shift_Left (Unsigned_32 (V.POSH_Optimization), 15) or
      Shift_Left (Unsigned_32 (V.Subopcode), 16) or
      Shift_Left (Unsigned_32 (V.Opcode), 24) or
      Shift_Left (Unsigned_32 (V.Command_Subtype), 27) or
      Shift_Left (Unsigned_32 (V.Command_Type), 29));
   type Binding_Pointer is record
      Reserved_0 : B5 := 0;
      Offset_Units : B11 := 0;
      Reserved_16 : B16 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Binding_Pointer use record
      Reserved_0 at 0 range 0 .. 4;
      Offset_Units at 0 range 5 .. 15;
      Reserved_16 at 0 range 16 .. 31;
   end record;
   function Encode (V : Binding_Pointer) return Unsigned_32 is
     (Unsigned_32 (V.Reserved_0) or
      Shift_Left (Unsigned_32 (V.Offset_Units), 5) or
      Shift_Left (Unsigned_32 (V.Reserved_16), 16));
   type Sampler_Pointer is record
      Reserved_0 : B5 := 0;
      Offset_Units : B27 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Sampler_Pointer use record
      Reserved_0 at 0 range 0 .. 4;
      Offset_Units at 0 range 5 .. 31;
   end record;
   function Encode (V : Sampler_Pointer) return Unsigned_32 is
     (Unsigned_32 (V.Reserved_0) or
      Shift_Left (Unsigned_32 (V.Offset_Units), 5));
   type Words is array (Natural range 0 .. 19) of Unsigned_32;
   -- Issue after SBA and constant-state updates, before drawing.
   -- VS has no resources; HS/DS/GS disabled. PS table is at surface-base
   -- offset0, where entry0 names the offscreen surface at offset64.
   -- All shaders have sampler count0 and contain no sampler instructions.
   -- A zero sampler offset alone does NOT disable sampler access.
   -- POSH optimization is a PS header field; MBZ at bit15 for other stages.
   Initial : constant Words :=
     [Encode (Header'(Subopcode => 16#26#, others => <>)),
      Encode (Binding_Pointer'(others => <>)),
      Encode (Header'(Subopcode => 16#27#, others => <>)),
      Encode (Binding_Pointer'(others => <>)),
      Encode (Header'(Subopcode => 16#28#, others => <>)),
      Encode (Binding_Pointer'(others => <>)),
      Encode (Header'(Subopcode => 16#29#, others => <>)),
      Encode (Binding_Pointer'(others => <>)),
      Encode (Header'(Subopcode => 16#2A#, others => <>)),
      Encode (Binding_Pointer'(others => <>)),
      Encode (Header'(Subopcode => 16#2B#, others => <>)),
      Encode (Sampler_Pointer'(others => <>)),
      Encode (Header'(Subopcode => 16#2C#, others => <>)),
      Encode (Sampler_Pointer'(others => <>)),
      Encode (Header'(Subopcode => 16#2D#, others => <>)),
      Encode (Sampler_Pointer'(others => <>)),
      Encode (Header'(Subopcode => 16#2E#, others => <>)),
      Encode (Sampler_Pointer'(others => <>)),
      Encode (Header'(Subopcode => 16#2F#, others => <>)),
      Encode (Sampler_Pointer'(others => <>))];
end Intel_GPU_ADLN_State_Pointers;
