with Ada.Text_IO; use Ada.Text_IO;
with Intel_GPU_ADLN_L3_Commands;
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Viewport; use Intel_GPU_ADLN_Viewport;
with Intel_GPU_ADLN_Clip;
with Intel_GPU_ADLN_Raster;
with Intel_GPU_ADLN_SF;
with Intel_GPU_ADLN_Windower;
with Intel_GPU_ADLN_SBE;
with Intel_GPU_ADLN_Sampling;
with Intel_GPU_ADLN_Offscreen_Surface;
with Intel_GPU_ADLN_Pixel_Blend;
with Intel_GPU_ADLN_Depth_Stencil;
with Intel_GPU_ADLN_Null_Buffers;
with Intel_GPU_ADLN_Passthrough;
with Intel_GPU_ADLN_Hull_Shader;
with Intel_GPU_ADLN_Domain_Shader;
with Intel_GPU_ADLN_Geometry_Shader;
with Intel_GPU_ADLN_Replication;
with Intel_GPU_ADLN_Drawing_Rectangle;
with Intel_GPU_ADLN_State_Pointers;
with Intel_GPU_ADLN_Binding_Pool;
with Intel_GPU_ADLN_L3;
with Intel_GPU_ADLN_Coarse_Pixel;
with Intel_GPU_ADLN_Color_Calc;
with Intel_GPU_ADLN_Sample_Pattern;
with Intel_GPU_ADLN_Stencil_Sync;
with Intel_GPU_ADLN_Pipe_Control;
procedure Viewport_Tests is
   package L3_Commands renames Intel_GPU_ADLN_L3_Commands;
   use type L3_Commands.Words;
   function To_L3_Load is new Ada.Unchecked_Conversion
     (Unsigned_32, L3_Commands.Load_Header);
   function To_L3_Store is new Ada.Unchecked_Conversion
     (Unsigned_32, L3_Commands.Store_Header);
   function To_L3_Register is new Ada.Unchecked_Conversion
     (Unsigned_32, L3_Commands.Register_Offset);
   function To_L3_Address is new Ada.Unchecked_Conversion
     (Unsigned_64, L3_Commands.Memory_Address);
   package Stencil renames Intel_GPU_ADLN_Stencil_Sync;
   use type Intel_GPU_ADLN_Pipe_Control.Packet;
   function To_Stencil_Address is new Ada.Unchecked_Conversion
     (Unsigned_64, Stencil.Qword_Address);
   package Pattern renames Intel_GPU_ADLN_Sample_Pattern;
   use type Pattern.Words;
   function To_Pattern_Four is new Ada.Unchecked_Conversion (Unsigned_32, Pattern.Four_Samples);
   function To_Pattern_Small is new Ada.Unchecked_Conversion (Unsigned_32, Pattern.Small_Modes);
   package Calc renames Intel_GPU_ADLN_Color_Calc;
   use type Calc.Words;
   function To_Calc_Control is new Ada.Unchecked_Conversion (Unsigned_32, Calc.Control);
   function To_Calc_Reference is new Ada.Unchecked_Conversion (Unsigned_32, Calc.UNorm_Reference);
   function To_Calc_Pointer is new Ada.Unchecked_Conversion (Unsigned_32, Calc.Pointer_Control);
   package CPS renames Intel_GPU_ADLN_Coarse_Pixel;
   use type CPS.Words;
   function To_CPS_Header is new Ada.Unchecked_Conversion (Unsigned_32, CPS.Header);
   function To_CPS_Pointer is new Ada.Unchecked_Conversion (Unsigned_32, CPS.Pointer_Control);
   function To_CPS_Minimum is new Ada.Unchecked_Conversion (Unsigned_32, CPS.Minimum_Control);
   function To_CPS_Maximum is new Ada.Unchecked_Conversion (Unsigned_32, CPS.Maximum_Control);
   function To_CPS_Focal is new Ada.Unchecked_Conversion (Unsigned_32, CPS.Focal_Control);
   package L3 renames Intel_GPU_ADLN_L3;
   function To_L3_Fuse is new Ada.Unchecked_Conversion
     (Unsigned_32, L3.Fuse_Control);
   function To_L3_Allocation is new Ada.Unchecked_Conversion
     (Unsigned_32, L3.Allocation);
   function To_L3_Parameters is new Ada.Unchecked_Conversion
     (Unsigned_32, L3.Parameters);
   package Pool renames Intel_GPU_ADLN_Binding_Pool;
   use type Pool.Words;
   Pool_Image : Pool.Image;
   function To_Pool_Address is new Ada.Unchecked_Conversion
     (Unsigned_64, Pool.Address_Control);
   function To_Pool_Size is new Ada.Unchecked_Conversion
     (Unsigned_32, Pool.Size_Control);
   package State_Pointers renames Intel_GPU_ADLN_State_Pointers;
   function To_Pointer_Header is new Ada.Unchecked_Conversion
     (Unsigned_32, State_Pointers.Header);
   function To_Binding_Pointer is new Ada.Unchecked_Conversion
     (Unsigned_32, State_Pointers.Binding_Pointer);
   function To_Sampler_Pointer is new Ada.Unchecked_Conversion
     (Unsigned_32, State_Pointers.Sampler_Pointer);
   package Rectangle renames Intel_GPU_ADLN_Drawing_Rectangle;
   use type Rectangle.Words;
   Rectangle_Image : Rectangle.Image;
   function To_Rectangle_Header is new Ada.Unchecked_Conversion
     (Unsigned_32, Rectangle.Header);
   function To_Rectangle_Coordinates is new Ada.Unchecked_Conversion
     (Unsigned_32, Rectangle.Coordinates);
   function To_Rectangle_Origin is new Ada.Unchecked_Conversion
     (Unsigned_32, Rectangle.Origin);
   package Replication renames Intel_GPU_ADLN_Replication;
   use type Replication.Words;
   function To_Replication_Control is new Ada.Unchecked_Conversion
     (Unsigned_32, Replication.Control);
   function To_Replication_Offsets is new Ada.Unchecked_Conversion
     (Unsigned_32, Replication.Offset_Group);
   package GS renames Intel_GPU_ADLN_Geometry_Shader;
   use type GS.Words;
   use type GS.B1;
   function To_GS_Resource is new Ada.Unchecked_Conversion
     (Unsigned_32, GS.Resource_Control);
   function To_GS_Payload is new Ada.Unchecked_Conversion
     (Unsigned_32, GS.Payload_Control);
   function To_GS_Dispatch is new Ada.Unchecked_Conversion
     (Unsigned_32, GS.Dispatch_Control);
   function To_GS_Thread is new Ada.Unchecked_Conversion
     (Unsigned_32, GS.Thread_Control);
   package DS renames Intel_GPU_ADLN_Domain_Shader;
   use type DS.Words;
   use type DS.B1;
   function To_DS_Resource is new Ada.Unchecked_Conversion
     (Unsigned_32, DS.Resource_Control);
   function To_DS_Payload is new Ada.Unchecked_Conversion
     (Unsigned_32, DS.Payload_Control);
   function To_DS_Dispatch is new Ada.Unchecked_Conversion
     (Unsigned_32, DS.Dispatch_Control);
   function To_DS_Output is new Ada.Unchecked_Conversion
     (Unsigned_32, DS.Output_Control);
   package HS renames Intel_GPU_ADLN_Hull_Shader;
   use type HS.Words;
   use type HS.B1;
   function To_HS_Resource is new Ada.Unchecked_Conversion
     (Unsigned_32, HS.Resource_Control);
   function To_HS_Dispatch is new Ada.Unchecked_Conversion
     (Unsigned_32, HS.Dispatch_Control);
   function To_HS_Payload is new Ada.Unchecked_Conversion
     (Unsigned_32, HS.Payload_Control);
   function To_HS_Kernel is new Ada.Unchecked_Conversion
     (Unsigned_64, HS.Kernel_Address);
   function To_HS_Scratch is new Ada.Unchecked_Conversion
     (Unsigned_64, HS.Scratch_Address);
   package Clip renames Intel_GPU_ADLN_Clip;
   package Raster renames Intel_GPU_ADLN_Raster;
   package Setup renames Intel_GPU_ADLN_SF;
   package WM renames Intel_GPU_ADLN_Windower;
   package SBE renames Intel_GPU_ADLN_SBE;
   package Sampling renames Intel_GPU_ADLN_Sampling;
   package Blend renames Intel_GPU_ADLN_Pixel_Blend;
   package Depth renames Intel_GPU_ADLN_Depth_Stencil;
   package Null_Buffers renames Intel_GPU_ADLN_Null_Buffers;
   package Pass_Through renames Intel_GPU_ADLN_Passthrough;
   use type Pass_Through.Words;
   use type Pass_Through.B1;
   function To_Pass_Stream_Control is new Ada.Unchecked_Conversion
     (Unsigned_32, Pass_Through.Stream_Control);
   function To_Pass_Stream_Reads is new Ada.Unchecked_Conversion
     (Unsigned_32, Pass_Through.Stream_Reads);
   function To_Pass_Pitch_Pair is new Ada.Unchecked_Conversion
     (Unsigned_32, Pass_Through.Pitch_Pair);
   function To_Pass_Tessellation_Control is new Ada.Unchecked_Conversion
     (Unsigned_32, Pass_Through.Tessellation_Control);
   use type Null_Buffers.Words;
   Null_Image : Null_Buffers.Image;
   Expected_Null : Null_Buffers.Words;
   function To_Null_Depth_Control is new Ada.Unchecked_Conversion
     (Unsigned_32, Null_Buffers.Depth_Control);
   function To_Null_Stencil_Control is new Ada.Unchecked_Conversion
     (Unsigned_32, Null_Buffers.Stencil_Control);
   function To_Null_Dimensions is new Ada.Unchecked_Conversion
     (Unsigned_32, Null_Buffers.Dimensions);
   function To_Null_Array_Control is new Ada.Unchecked_Conversion
     (Unsigned_32, Null_Buffers.Array_Control);
   function To_Null_Tiling_Control is new Ada.Unchecked_Conversion
     (Unsigned_32, Null_Buffers.Tiling_Control);
   function To_Null_View_Control is new Ada.Unchecked_Conversion
     (Unsigned_32, Null_Buffers.View_Control);
   function To_Null_HiZ_Control is new Ada.Unchecked_Conversion
     (Unsigned_32, Null_Buffers.HiZ_Control);
   function To_Null_HiZ_QPitch is new Ada.Unchecked_Conversion
     (Unsigned_32, Null_Buffers.HiZ_QPitch);
   function To_Null_Clear_Control is new Ada.Unchecked_Conversion
     (Unsigned_32, Null_Buffers.Clear_Control);
   use type Depth.Words;
   function To_Depth_Stencil_Header is new Ada.Unchecked_Conversion
     (Unsigned_32, Depth.Stencil_Header);
   function To_Depth_Test_Control is new Ada.Unchecked_Conversion
     (Unsigned_32, Depth.Test_Control);
   function To_Depth_Masks is new Ada.Unchecked_Conversion
     (Unsigned_32, Depth.Masks);
   function To_Depth_References is new Ada.Unchecked_Conversion
     (Unsigned_32, Depth.References);
   function To_Depth_Bounds_Header is new Ada.Unchecked_Conversion
     (Unsigned_32, Depth.Bounds_Header);
   function To_Depth_Bounds_Control is new Ada.Unchecked_Conversion
     (Unsigned_32, Depth.Bounds_Control);
   use type Blend.Words;
   function To_PS_Blend is new Ada.Unchecked_Conversion (Unsigned_32, Blend.PS_Control);
   function To_Blend_Common is new Ada.Unchecked_Conversion (Unsigned_32, Blend.Common_Control);
   function To_Blend_Color is new Ada.Unchecked_Conversion (Unsigned_32, Blend.Entry_Color);
   function To_Blend_Clamp is new Ada.Unchecked_Conversion (Unsigned_32, Blend.Entry_Clamp);
   function To_Blend_Pointer is new Ada.Unchecked_Conversion (Unsigned_32, Blend.State_Pointer);
   package Surface renames Intel_GPU_ADLN_Offscreen_Surface;
   Surface_Image : constant Surface.Image := Surface.Build (6);
   function To_Surface_Samples is new Ada.Unchecked_Conversion
     (Unsigned_32, Surface.DW4_Fields);
   use type Sampling.Words;
   function To_Sampling is new Ada.Unchecked_Conversion
     (Unsigned_32, Sampling.Multisample_Control);
   function To_Coverage is new Ada.Unchecked_Conversion
     (Unsigned_32, Sampling.Coverage_Control);
   use type SBE.Words, SBE.Body_Words;
   function To_SBE is new Ada.Unchecked_Conversion (SBE.Body_Words, SBE.State);
   SB : SBE.Body_Words;
   use type WM.Words;
   function To_WM is new Ada.Unchecked_Conversion (Unsigned_32, WM.Control);
   use type Setup.Words;
   function To_Transform is new Ada.Unchecked_Conversion (Unsigned_32, Setup.Transform_Control);
   function To_Deref is new Ada.Unchecked_Conversion (Unsigned_32, Setup.Deref_Control);
   function To_Point is new Ada.Unchecked_Conversion (Unsigned_32, Setup.Point_Control);
   Setup_Image : Setup.Image;
   use type Raster.Words, Raster.Offset_Words;
   function To_Raster is new Ada.Unchecked_Conversion (Unsigned_32, Raster.Control);
   function To_Offsets is new Ada.Unchecked_Conversion (Raster.Offset_Words, Raster.Depth_Offset_State);
   O : Raster.Offset_Words;
   use type Clip.Words;
   function To_Cull is new Ada.Unchecked_Conversion (Unsigned_32, Clip.Cull_Control);
   function To_Clip is new Ada.Unchecked_Conversion (Unsigned_32, Clip.Clip_Control);
   function To_Clip_VP is new Ada.Unchecked_Conversion (Unsigned_32, Clip.Viewport_Control);
   function To_SF is new Ada.Unchecked_Conversion (SF_Words, SF_State);
   function To_CC is new Ada.Unchecked_Conversion (CC_Words, CC_State);
   function To_SF_Pointer is new Ada.Unchecked_Conversion (Unsigned_32, SF_Pointer);
   function To_CC_Pointer is new Ada.Unchecked_Conversion (Unsigned_32, CC_Pointer);
   S : SF_Words;
   C : CC_Words;
begin
   pragma Assert (Stencil.Packet = Intel_GPU_ADLN_Pipe_Control.Packet'
     [16#7A000004#, 16#00105002#, 16#201008#, 0, 0, 0]);
   for Bit in 0 .. 63 loop
      declare V : constant Unsigned_64 := Shift_Left (1, Bit); begin
         pragma Assert (Stencil.Encode (To_Stencil_Address (V)) = V);
      end;
   end loop;
   pragma Assert (Pattern.Standard = Pattern.Words'[16#791c0007#, 16#c75a7599#, 16#b3dbad36#, 16#2c42816e#, 16#10eff408#, 16#f1bf173d#, 16#53d97b95#, 16#ae2ae662#, 16#8844cc#]);
   pragma Assert (Calc.Pointer = Calc.Words'[16#780E0000#, 16#501#]);
   pragma Assert (for all W of Calc.State => W = 0);
   pragma Assert (CPS.Pointer = CPS.Words'[16#78220000#, 736]);
   pragma Assert (for all W of CPS.Initial_Array => W = 0);
   for Untagged in 0 .. 255 loop
      for Tagged_Count in 0 .. 255 loop
         pragma Assert (L3.Probe_URB_KiB (True, To_L3_Fuse (16#F0#),
           L3.Decode_Parameters (Unsigned_32 (Untagged * 256 + Tagged_Count)),
           L3.Render_Allocation) =
           (if Untagged in 16 .. 32 and Tagged_Count in 88 .. 104 and
               Untagged + Tagged_Count = 120 then 512 else 0));
      end loop;
   end loop;
   for Mask in 0 .. 255 loop
      declare
         F : constant L3.Fuse_Control := To_L3_Fuse (Unsigned_32 (Mask));
         Expected : Natural := 0;
      begin
         for Bit in 0 .. 7 loop
            if (Mask / (2 ** Bit)) mod 2 = 0 then Expected := Expected + 1; end if;
         end loop;
         pragma Assert (L3.Enabled_Banks (F) = Expected);
         pragma Assert (L3.Probe_URB_KiB (True, F,
           L3.Decode_Parameters (16#1068#), L3.Render_Allocation) =
           (if Mask = 16#F0# then 512 else 0));
      end;
   end loop;
   pragma Assert (L3.Probe_URB_KiB (False, To_L3_Fuse (16#F0#),
     L3.Decode_Parameters (16#1068#), L3.Render_Allocation) = 0);
   for Bit in 0 .. 31 loop
      declare
         V : constant Unsigned_32 := Shift_Left (1, Bit);
      begin
         pragma Assert (L3.Encode (To_L3_Fuse (V)) = V);
         pragma Assert (L3.Probe_URB_KiB (True, To_L3_Fuse (16#F0#),
           L3.Decode_Parameters (16#1068#),
           To_L3_Allocation (16#B0000040# xor V)) =
           (if Bit in 8 .. 10 then 512 else 0));
      end;
   end loop;
   pragma Assert (L3.Encode (L3.Render_Allocation) = 16#B0000040#);
   pragma Assert (L3.Encode (L3.Default_Allocation) = 16#D0000020#);
   pragma Assert (L3.Encode (L3.Decode_Parameters (16#ABCD1068#)) = 16#ABCD1068#);
   for Bit in 0 .. 31 loop
      declare V : constant Unsigned_32 := Shift_Left (1, Bit); begin
         pragma Assert (L3.Encode (To_L3_Allocation (V)) = V);
         pragma Assert (CPS.Encode (To_CPS_Pointer (V)) = V);
         pragma Assert (CPS.Encode (To_CPS_Header (V)) = V);
         pragma Assert (Calc.Encode (To_Calc_Control (V)) = V);
         pragma Assert (Calc.Encode (To_Calc_Reference (V)) = V);
         pragma Assert (Calc.Encode (To_Calc_Pointer (V)) = V);
         pragma Assert (Pattern.Encode (To_Pattern_Four (V)) = V);
         pragma Assert (Pattern.Encode (To_Pattern_Small (V)) = V);
         pragma Assert (CPS.Encode (To_CPS_Minimum (V)) = V);
         pragma Assert (CPS.Encode (To_CPS_Maximum (V)) = V);
         pragma Assert (CPS.Encode (To_CPS_Focal (V)) = V);
         pragma Assert (L3.Encode (To_L3_Parameters (V)) = V);
         pragma Assert (L3.Encode (L3.Decode_Parameters (V)) = V);
         pragma Assert (Encode (To_SF_Pointer (V)) = V);
         pragma Assert (Encode (To_CC_Pointer (V)) = V);
         pragma Assert (Clip.Encode (To_Cull (V)) = V);
         pragma Assert (Clip.Encode (To_Clip (V)) = V);
         pragma Assert (Clip.Encode (To_Clip_VP (V)) = V);
         pragma Assert (Raster.Encode (To_Raster (V)) = V);
         pragma Assert (Setup.Encode (To_Transform (V)) = V);
         pragma Assert (Setup.Encode (To_Deref (V)) = V);
         pragma Assert (Setup.Encode (To_Point (V)) = V);
         pragma Assert (WM.Encode (To_WM (V)) = V);
         pragma Assert (Sampling.Encode (To_Sampling (V)) = V);
         pragma Assert (Sampling.Encode (To_Coverage (V)) = V);
         pragma Assert (Blend.Encode (To_PS_Blend (V)) = V);
         pragma Assert (Blend.Encode (To_Blend_Common (V)) = V);
         pragma Assert (Blend.Encode (To_Blend_Color (V)) = V);
         pragma Assert (Blend.Encode (To_Blend_Clamp (V)) = V);
         pragma Assert (Blend.Encode (To_Blend_Pointer (V)) = V);
         pragma Assert (Depth.Encode (To_Depth_Stencil_Header (V)) = V);
         pragma Assert (Depth.Encode (To_Depth_Test_Control (V)) = V);
         pragma Assert (Depth.Encode (To_Depth_Masks (V)) = V);
         pragma Assert (Depth.Encode (To_Depth_References (V)) = V);
         pragma Assert (Depth.Encode (To_Depth_Bounds_Header (V)) = V);
         pragma Assert (Depth.Encode (To_Depth_Bounds_Control (V)) = V);
         pragma Assert (Null_Buffers.Encode (To_Null_Depth_Control (V)) = V);
         pragma Assert (Null_Buffers.Encode (To_Null_Stencil_Control (V)) = V);
         pragma Assert (Null_Buffers.Encode (To_Null_Dimensions (V)) = V);
         pragma Assert (Null_Buffers.Encode (To_Null_Array_Control (V)) = V);
         pragma Assert (Null_Buffers.Encode (To_Null_Tiling_Control (V)) = V);
         pragma Assert (Null_Buffers.Encode (To_Null_View_Control (V)) = V);
         pragma Assert (Null_Buffers.Encode (To_Null_HiZ_Control (V)) = V);
         pragma Assert (Null_Buffers.Encode (To_Null_HiZ_QPitch (V)) = V);
         pragma Assert (Null_Buffers.Encode (To_Null_Clear_Control (V)) = V);
         pragma Assert (Pass_Through.Encode (To_Pass_Stream_Control (V)) = V);
         pragma Assert (HS.Encode (To_HS_Resource (V)) = V);
         pragma Assert (DS.Encode (To_DS_Resource (V)) = V);
         pragma Assert (GS.Encode (To_GS_Resource (V)) = V);
         pragma Assert (Replication.Encode (To_Replication_Control (V)) = V);
         pragma Assert (Rectangle.Encode (To_Rectangle_Header (V)) = V);
         pragma Assert (State_Pointers.Encode (To_Pointer_Header (V)) = V);
         pragma Assert (Pool.Encode (To_Pool_Size (V)) = V);
         pragma Assert (State_Pointers.Encode (To_Binding_Pointer (V)) = V);
         pragma Assert (State_Pointers.Encode (To_Sampler_Pointer (V)) = V);
         pragma Assert (Rectangle.Encode (To_Rectangle_Coordinates (V)) = V);
         pragma Assert (Rectangle.Encode (To_Rectangle_Origin (V)) = V);
         pragma Assert (Replication.Encode (To_Replication_Offsets (V)) = V);
         pragma Assert (GS.Encode (To_GS_Payload (V)) = V);
         pragma Assert (GS.Encode (To_GS_Dispatch (V)) = V);
         pragma Assert (GS.Encode (To_GS_Thread (V)) = V);
         pragma Assert (DS.Encode (To_DS_Payload (V)) = V);
         pragma Assert (DS.Encode (To_DS_Dispatch (V)) = V);
         pragma Assert (DS.Encode (To_DS_Output (V)) = V);
         pragma Assert (HS.Encode (To_HS_Dispatch (V)) = V);
         pragma Assert (HS.Encode (To_HS_Payload (V)) = V);
         pragma Assert (Pass_Through.Encode (To_Pass_Stream_Reads (V)) = V);
         pragma Assert (Pass_Through.Encode (To_Pass_Pitch_Pair (V)) = V);
         pragma Assert (Pass_Through.Encode (To_Pass_Tessellation_Control (V)) = V);
         for I in SB'Range loop
            SB := [others => 0]; SB (I) := V;
            pragma Assert (SBE.Encode (To_SBE (SB)) = SB);
         end loop;
         for I in O'Range loop
            O := [others => 0]; O (I) := V;
            pragma Assert (Raster.Encode (To_Offsets (O)) = O);
         end loop;
         for I in S'Range loop
            S := [others => 0]; S (I) := V;
            pragma Assert (Encode (To_SF (S)) = S);
         end loop;
         for I in C'Range loop
            C := [others => 0]; C (I) := V;
            pragma Assert (Encode (To_CC (C)) = C);
         end loop;
      end;
   end loop;
   pragma Assert (SF = SF_Words'
     [16#42000000#,16#42000000#,16#3F800000#,16#42000000#,16#42000000#,0,0,0,
      16#BF800000#,16#3F800000#,16#BF800000#,16#3F800000#,0,16#427C0000#,0,16#427C0000#]);
   pragma Assert (CC = CC_Words'[0,16#3F800000#]);
   pragma Assert (Pointers = Pointer_Words'[16#78210000#,512,16#78230000#,576]);
   pragma Assert (Clip.Initial = Clip.Words'
     [16#78120002#,16#400#,16#90000000#,16#3FFE0#]);
   pragma Assert (Raster.Initial = Raster.Words'
     [16#78500003#,16#04210001#,0,0,0]);
   for Count in 0 .. 4096 loop
      Setup_Image := Setup.Build (Count);
      pragma Assert (Setup_Image.Valid = (Count in 64 .. 3576 and Count mod 8 = 0));
      if Setup_Image.Valid then
         pragma Assert (Setup_Image.Data = Setup.Words'
           [16#78130002#,16#00080402#,
            (if Count < 192 then 16#20000000# else 0),16#4808#]);
      else
         pragma Assert (Setup_Image.Data = Setup.Words'(others => 0));
      end if;
   end loop;
   pragma Assert (not Setup.Build (Natural'Last).Valid);
   pragma Assert (WM.Initial = WM.Words'[16#78140000#,16#80000000#]);
   pragma Assert (SBE.Initial = SBE.Words'
     [16#781F0004#,16#30000820#,0,0,16#FFFFFFFF#,16#FFFFFFFF#]);
   pragma Assert (Sampling.Initial = Sampling.Words'
     [16#780D0000#,0,16#78180000#,1]);
   pragma Assert (Surface_Image.Valid);
   pragma Assert (Unsigned_32
     (To_Surface_Samples (Surface_Image.Words (4)).Multisamples) =
     Unsigned_32 (Sampling.Single_Sample.Log2_Samples));
   pragma Assert (Blend.Initial = Blend.Words'
     [16#784D0000#,16#40000000#,16#78240000#,16#281#]);
   pragma Assert (Blend.State (0) = 0);
   for RT in 0 .. 7 loop
      pragma Assert (Blend.State (1 + RT * 2) = (if RT = 0 then 0 else 15));
      pragma Assert (Blend.State (2 + RT * 2) = 11);
   end loop;
   pragma Assert (Depth.Initial = Depth.Words'
     [16#784E0002#,0,0,0,16#78710002#,0,0,16#3F800000#]);
   for Policy in Unsigned_32 range 0 .. 1024 loop
      Null_Image := Null_Buffers.Build (Policy);
      pragma Assert (Null_Image.Valid = (Policy in 2 .. 126 and Policy mod 2 = 0));
      if Null_Image.Valid then
         -- Independent actual-ISL no-attachment fixture, including all zeros.
         Expected_Null :=
           [16#78050006#,16#E1000000#,0,0,0,Policy,0,0,
            16#78060006#,16#E0000000#,0,0,0,Policy,0,0,
            16#78070003#,Shift_Left (Policy,25),0,0,0,16#78040001#,0,0];
      else
         Expected_Null := [others => 0];
      end if;
      pragma Assert (Null_Image.Data = Expected_Null);
   end loop;
   pragma Assert (not Null_Buffers.Build (Unsigned_32'Last).Valid);
   pragma Assert (Pass_Through.Streamout_Disabled = Pass_Through.Words'
     [16#781E0003#,0,0,0,0]);
   pragma Assert (Pass_Through.Tessellation_Disabled = Pass_Through.Words'
     [16#781C0003#,0,0,0,0]);
   for Bit in 0 .. 63 loop
      declare
         V : constant Unsigned_64 := Shift_Left (Unsigned_64'(1), Bit);
      begin
         pragma Assert (HS.Encode (To_HS_Kernel (V)) = V);
         pragma Assert (Pool.Encode (To_Pool_Address (V)) = V);
         pragma Assert (HS.Encode (To_HS_Scratch (V)) = V);
      end;
   end loop;
   pragma Assert (HS.Disabled = HS.Words'[16#781B0007#,0,0,0,0,0,0,0,0]);
   pragma Assert (DS.Disabled = DS.Words'[16#781D0009#,0,0,0,0,0,0,0,0,0,0]);
   -- Check the group requirement with the actual assembled packet fields.
   pragma Assert (To_HS_Dispatch (HS.Disabled (2)).Enable_HS = 0);
   pragma Assert (To_Pass_Tessellation_Control
     (Pass_Through.Tessellation_Disabled (1)).Enable_TE = 0);
   pragma Assert (To_DS_Dispatch (DS.Disabled (7)).Enable_DS = 0);
   pragma Assert (GS.Disabled = GS.Words'[16#78110008#,0,0,0,0,0,0,0,0,0]);
   pragma Assert (To_GS_Dispatch (GS.Disabled (7)).Enable_GS = 0);
   pragma Assert (To_GS_Resource (GS.Disabled (3)).Accesses_UAV = 0);
   pragma Assert (Replication.Disabled = Replication.Words'
     [16#786C0004#,0,0,0,0,0]);
   -- Nonzero layout fixture only, not a valid rendering configuration.
   pragma Assert (Replication.Encode (Replication.Control'
     (Count => 15, Replica_Mask => 16#A55A#, others => <>)) = 16#A55A000F#);
   pragma Assert (Replication.Encode (Replication.Offset_Group'
     (0,1,2,3,4,5,6,7)) = 16#76543210#);
   pragma Assert (Replication.Encode (Replication.Offset_Group'
     (8,9,10,11,12,13,14,15)) = 16#FEDCBA98#);
   for Dimension in 0 .. 16_385 loop
      Rectangle_Image := Rectangle.Build (Dimension, 64);
      pragma Assert (Rectangle_Image.Valid = (Dimension in 1 .. 16_384));
      if Rectangle_Image.Valid then
         pragma Assert (Rectangle_Image.Data = Rectangle.Words'
           [16#79000002#,0,16#003F0000# or Unsigned_32 (Dimension - 1),0]);
      else
         pragma Assert (Rectangle_Image.Data = Rectangle.Words'(others => 0));
      end if;
      Rectangle_Image := Rectangle.Build (64, Dimension);
      pragma Assert (Rectangle_Image.Valid = (Dimension in 1 .. 16_384));
      if Rectangle_Image.Valid then
         pragma Assert (Rectangle_Image.Data (2) =
           (Shift_Left (Unsigned_32 (Dimension - 1),16) or 63));
      else
         pragma Assert (Rectangle_Image.Data = Rectangle.Words'(others => 0));
      end if;
   end loop;
   pragma Assert (not Rectangle.Build (Natural'Last, 64).Valid);
   pragma Assert (not Rectangle.Build (64, Natural'Last).Valid);
   pragma Assert (Rectangle.Build (64,64).Data = Rectangle.Words'
     [16#79000002#,0,16#003F003F#,0]);
   pragma Assert (Rectangle.Encode (Rectangle.Origin'(X => -16_384, Y => 16_383))
     = 16#3FFFC000#);
   for Stage in 0 .. 9 loop
      pragma Assert (State_Pointers.Initial (Stage * 2) =
        16#78260000# + Shift_Left (Unsigned_32 (Stage),16));
      pragma Assert (State_Pointers.Initial (Stage * 2 + 1) = 0);
   end loop;
   for Policy in Unsigned_32 range 0 .. 1024 loop
      Pool_Image := Pool.Disable (Policy);
      pragma Assert (Pool_Image.Valid = (Policy in 2 .. 126 and Policy mod 2 = 0));
      if Pool_Image.Valid then
         pragma Assert (Pool_Image.Data = Pool.Words'[16#79190002#,Policy,0,0]);
      else
         pragma Assert (Pool_Image.Data = Pool.Words'(others => 0));
      end if;
   end loop;
   pragma Assert (not Pool.Disable (Unsigned_32'Last).Valid);
   for Bit in 0 .. 31 loop
      pragma Assert (L3_Commands.Encode (To_L3_Load (Shift_Left (1, Bit))) = Shift_Left (1, Bit));
      pragma Assert (L3_Commands.Encode (To_L3_Store (Shift_Left (1, Bit))) = Shift_Left (1, Bit));
      pragma Assert (L3_Commands.Encode (To_L3_Register (Shift_Left (1, Bit))) = Shift_Left (1, Bit));
   end loop;
   for Bit in 0 .. 63 loop
      pragma Assert (L3_Commands.Encode (To_L3_Address (Shift_Left (1, Bit))) = Shift_Left (1, Bit));
   end loop;
   pragma Assert (L3_Commands.Initialize_And_Sample = L3_Commands.Words'
     [16#11000001#,16#B134#,16#B0000040#,16#12000002#,16#B134#,16#201010#,0,
      16#12000002#,16#B164#,16#201014#,0]);
   pragma Assert (L3.Render_Allocation_Matches (L3.Decode_Allocation (16#B0000040#)));
   pragma Assert (not L3.Render_Allocation_Matches (L3.Decode_Allocation (0)));
   pragma Assert (not L3.Render_Allocation_Matches (L3.Decode_Allocation (Unsigned_32'Last)));
   for Bit in 0 .. 31 loop
      declare
         Raw : constant Unsigned_32 := 16#B0000040# xor Shift_Left (1, Bit);
      begin
         pragma Assert (L3.Encode (L3.Decode_Allocation (Raw)) = Raw);
         pragma Assert (L3.Encode (L3.Decode_Fuse (Raw)) = Raw);
         pragma Assert (L3.Render_Allocation_Matches (L3.Decode_Allocation (Raw)) =
           (Bit in 8 .. 10));
      end;
   end loop;
   Put_Line ("raster pipeline PASS: 3456 bit checks, L3 commands/stencil/sample/CC/CPS, rectangle/URB/MOCS boundaries, Mesa fixtures");
end Viewport_Tests;
