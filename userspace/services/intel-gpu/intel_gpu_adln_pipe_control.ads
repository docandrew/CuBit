with Interfaces; use Interfaces;
with System;
with Intel_GPU_ADLN_Vertex_Fetch; use Intel_GPU_ADLN_Vertex_Fetch;
package Intel_GPU_ADLN_Pipe_Control with SPARK_Mode is
   -- TGL PRM Vol2a pp1120-1130. RCS 3D state-base transition only.
   -- Header bit10 and control22/27 remain reserved per this PRM, despite
   -- later/different-platform fields in Mesa's shared Gen12 schema.
   -- These encoders describe bits, not permission to submit arbitrary packets.
   type Pipe_Header is record
      Length : B8 := 4;
      Reserved_8 : B1 := 0;
      HDC_Flush : B1 := 0;
      Reserved_10_15 : B6 := 0;
      Subopcode : B8 := 0;
      Opcode : B3 := 2;
      Subtype_Code : B2 := 3;
      Command_Type : B3 := 3;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Pipe_Header use record
      Length at 0 range 0 .. 7;
      Reserved_8 at 0 range 8 .. 8;
      HDC_Flush at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      Subopcode at 0 range 16 .. 23;
      Opcode at 0 range 24 .. 26;
      Subtype_Code at 0 range 27 .. 28;
      Command_Type at 0 range 29 .. 31;
   end record;
   function Encode (V : Pipe_Header) return Unsigned_32 is
     (Unsigned_32 (V.Length) or
      Shift_Left (Unsigned_32 (V.Reserved_8), 8) or
      Shift_Left (Unsigned_32 (V.HDC_Flush), 9) or
      Shift_Left (Unsigned_32 (V.Reserved_10_15), 10) or
      Shift_Left (Unsigned_32 (V.Subopcode), 16) or
      Shift_Left (Unsigned_32 (V.Opcode), 24) or
      Shift_Left (Unsigned_32 (V.Subtype_Code), 27) or
      Shift_Left (Unsigned_32 (V.Command_Type), 29));
   type Pipe_Flags is record
      Depth_Flush : B1 := 0;
      Scoreboard_Stall : B1 := 0;
      State_Invalidate : B1 := 0;
      Constant_Invalidate : B1 := 0;
      VF_Invalidate : B1 := 0;
      DC_Flush : B1 := 0;
      Reserved_6 : B1 := 0;
      Pipe_Control_Flush : B1 := 0;
      Notify : B1 := 0;
      Indirect_Pointers_Disable : B1 := 0;
      Texture_Invalidate : B1 := 0;
      Instruction_Invalidate : B1 := 0;
      Render_Target_Flush : B1 := 0;
      Depth_Stall : B1 := 0;
      Post_Sync : B2 := 0;
      Media_Clear : B1 := 0;
      PSD_Sync : B1 := 0;
      TLB_Invalidate : B1 := 0;
      Snapshot_Reset : B1 := 0;
      CS_Stall : B1 := 0;
      Store_Index : B1 := 0;
      Reserved_22 : B1 := 0;
      LRI_Post_Sync : B1 := 0;
      Use_GGTT : B1 := 0;
      AMFS_Flush : B1 := 0;
      LLC_Flush : B1 := 0;
      Reserved_27 : B1 := 0;
      Tile_Flush : B1 := 0;
      Command_Invalidate : B1 := 0;
      L3_Fabric_Flush : B1 := 0;
      Reserved_31 : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Pipe_Flags use record
      Depth_Flush at 0 range 0 .. 0;
      Scoreboard_Stall at 0 range 1 .. 1;
      State_Invalidate at 0 range 2 .. 2;
      Constant_Invalidate at 0 range 3 .. 3;
      VF_Invalidate at 0 range 4 .. 4;
      DC_Flush at 0 range 5 .. 5;
      Reserved_6 at 0 range 6 .. 6;
      Pipe_Control_Flush at 0 range 7 .. 7;
      Notify at 0 range 8 .. 8;
      Indirect_Pointers_Disable at 0 range 9 .. 9;
      Texture_Invalidate at 0 range 10 .. 10;
      Instruction_Invalidate at 0 range 11 .. 11;
      Render_Target_Flush at 0 range 12 .. 12;
      Depth_Stall at 0 range 13 .. 13;
      Post_Sync at 0 range 14 .. 15;
      Media_Clear at 0 range 16 .. 16;
      PSD_Sync at 0 range 17 .. 17;
      TLB_Invalidate at 0 range 18 .. 18;
      Snapshot_Reset at 0 range 19 .. 19;
      CS_Stall at 0 range 20 .. 20;
      Store_Index at 0 range 21 .. 21;
      Reserved_22 at 0 range 22 .. 22;
      LRI_Post_Sync at 0 range 23 .. 23;
      Use_GGTT at 0 range 24 .. 24;
      AMFS_Flush at 0 range 25 .. 25;
      LLC_Flush at 0 range 26 .. 26;
      Reserved_27 at 0 range 27 .. 27;
      Tile_Flush at 0 range 28 .. 28;
      Command_Invalidate at 0 range 29 .. 29;
      L3_Fabric_Flush at 0 range 30 .. 30;
      Reserved_31 at 0 range 31 .. 31;
   end record;
   function Encode (V : Pipe_Flags) return Unsigned_32 is
     (Unsigned_32 (V.Depth_Flush) or
      Shift_Left (Unsigned_32 (V.Scoreboard_Stall), 1) or
      Shift_Left (Unsigned_32 (V.State_Invalidate), 2) or
      Shift_Left (Unsigned_32 (V.Constant_Invalidate), 3) or
      Shift_Left (Unsigned_32 (V.VF_Invalidate), 4) or
      Shift_Left (Unsigned_32 (V.DC_Flush), 5) or
      Shift_Left (Unsigned_32 (V.Reserved_6), 6) or
      Shift_Left (Unsigned_32 (V.Pipe_Control_Flush), 7) or
      Shift_Left (Unsigned_32 (V.Notify), 8) or
      Shift_Left (Unsigned_32 (V.Indirect_Pointers_Disable), 9) or
      Shift_Left (Unsigned_32 (V.Texture_Invalidate), 10) or
      Shift_Left (Unsigned_32 (V.Instruction_Invalidate), 11) or
      Shift_Left (Unsigned_32 (V.Render_Target_Flush), 12) or
      Shift_Left (Unsigned_32 (V.Depth_Stall), 13) or
      Shift_Left (Unsigned_32 (V.Post_Sync), 14) or
      Shift_Left (Unsigned_32 (V.Media_Clear), 16) or
      Shift_Left (Unsigned_32 (V.PSD_Sync), 17) or
      Shift_Left (Unsigned_32 (V.TLB_Invalidate), 18) or
      Shift_Left (Unsigned_32 (V.Snapshot_Reset), 19) or
      Shift_Left (Unsigned_32 (V.CS_Stall), 20) or
      Shift_Left (Unsigned_32 (V.Store_Index), 21) or
      Shift_Left (Unsigned_32 (V.Reserved_22), 22) or
      Shift_Left (Unsigned_32 (V.LRI_Post_Sync), 23) or
      Shift_Left (Unsigned_32 (V.Use_GGTT), 24) or
      Shift_Left (Unsigned_32 (V.AMFS_Flush), 25) or
      Shift_Left (Unsigned_32 (V.LLC_Flush), 26) or
      Shift_Left (Unsigned_32 (V.Reserved_27), 27) or
      Shift_Left (Unsigned_32 (V.Tile_Flush), 28) or
      Shift_Left (Unsigned_32 (V.Command_Invalidate), 29) or
      Shift_Left (Unsigned_32 (V.L3_Fabric_Flush), 30) or
      Shift_Left (Unsigned_32 (V.Reserved_31), 31));

   type Packet is array (Natural range 0 .. 5) of Unsigned_32;
   -- Mesa pre-SBA workaround Wa_18039438632 plus HDC flush and CS stall.
   Before_State_Base : constant Packet :=
     [Encode (Pipe_Header'(HDC_Flush => 1, others => <>)),
      Encode (Pipe_Flags'(Render_Target_Flush => 1, CS_Stall => 1, others => <>)),
      0, 0, 0, 0];
   -- State/texture/constant invalidate after SBA. Also invalidate instruction
   -- cache for freshly populated shader backing and command cache to cover
   -- SLICE_COMMON_ECO_CHICKEN1 state-cache redirect (PRM p1129).
   -- No flush/invalidate merging: invalidation occurs at parsing time whereas
   -- pre-SBA stalling flush must finish before changing the bases.
   After_State_Base : constant Packet :=
     [Encode (Pipe_Header'(others => <>)),
      Encode (Pipe_Flags'(State_Invalidate => 1, Constant_Invalidate => 1,
                         Texture_Invalidate => 1, Instruction_Invalidate => 1,
                         Command_Invalidate => 1, others => <>)),
      0, 0, 0, 0];
end Intel_GPU_ADLN_Pipe_Control;
