with Interfaces; use Interfaces;
with System;
with Intel_GPU_ADLN_Vertex_Fetch; use Intel_GPU_ADLN_Vertex_Fetch;
with Intel_GPU_ADLN_Pipe_Control;
package Intel_GPU_ADLN_Pipeline with SPARK_Mode is
   -- TGL Vol2a pp1131-1133. Only RCS, not ComputeCS.
   -- Unmodified controls retain their old values: mask writes only bits0/1/4.
   type Select_Control is record
      Selection : B2 := 0;
      Render_Slice_Power_Gate : B1 := 0;
      Render_Sampler_Power_Gate : B1 := 0;
      Media_DOP_Clock_Gate : B1 := 1;
      Reserved_5 : B1 := 0;
      Media_Power_Clock_Gate_Disable : B1 := 0;
      Reserved_7 : B1 := 0;
      Mask_Bits : B8 := 16#13#;
      Subopcode : B8 := 4;
      Opcode : B3 := 1;
      Subtype_Code : B2 := 1;
      Command_Type : B3 := 3;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Select_Control use record
      Selection at 0 range 0 .. 1;
      Render_Slice_Power_Gate at 0 range 2 .. 2;
      Render_Sampler_Power_Gate at 0 range 3 .. 3;
      Media_DOP_Clock_Gate at 0 range 4 .. 4;
      Reserved_5 at 0 range 5 .. 5;
      Media_Power_Clock_Gate_Disable at 0 range 6 .. 6;
      Reserved_7 at 0 range 7 .. 7;
      Mask_Bits at 0 range 8 .. 15;
      Subopcode at 0 range 16 .. 23;
      Opcode at 0 range 24 .. 26;
      Subtype_Code at 0 range 27 .. 28;
      Command_Type at 0 range 29 .. 31;
   end record;
   function Encode (V : Select_Control) return Unsigned_32 is
     (Unsigned_32 (V.Selection) or
      Shift_Left (Unsigned_32 (V.Render_Slice_Power_Gate), 2) or
      Shift_Left (Unsigned_32 (V.Render_Sampler_Power_Gate), 3) or
      Shift_Left (Unsigned_32 (V.Media_DOP_Clock_Gate), 4) or
      Shift_Left (Unsigned_32 (V.Reserved_5), 5) or
      Shift_Left (Unsigned_32 (V.Media_Power_Clock_Gate_Disable), 6) or
      Shift_Left (Unsigned_32 (V.Reserved_7), 7) or
      Shift_Left (Unsigned_32 (V.Mask_Bits), 8) or
      Shift_Left (Unsigned_32 (V.Subopcode), 16) or
      Shift_Left (Unsigned_32 (V.Opcode), 24) or
      Shift_Left (Unsigned_32 (V.Subtype_Code), 27) or
      Shift_Left (Unsigned_32 (V.Command_Type), 29));
   type Packet is array (Natural range 0 .. 6) of Unsigned_32;
   -- Mesa Gen12 flush_pipeline_select for initial/unknown mode. Depth stall
   -- accompanies depth flush per Wa_1409600907. Deliberately omit media clear:
   -- Mesa reports hangs when it is emitted outside MEDIA mode. This is not a
   -- general mode-switch API, only the private RCS probe initialization.
   Initial_3D : constant Packet :=
     [Intel_GPU_ADLN_Pipe_Control.Encode
        (Intel_GPU_ADLN_Pipe_Control.Pipe_Header'(HDC_Flush => 1, others => <>)),
      Intel_GPU_ADLN_Pipe_Control.Encode
        (Intel_GPU_ADLN_Pipe_Control.Pipe_Flags'
           (CS_Stall => 1, Render_Target_Flush => 1, Depth_Flush => 1,
            Depth_Stall => 1, others => <>)),
      0, 0, 0, 0, Encode (Select_Control'(others => <>))];
end Intel_GPU_ADLN_Pipeline;
