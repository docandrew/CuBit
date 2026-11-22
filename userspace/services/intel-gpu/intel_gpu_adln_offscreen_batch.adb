with Intel_GPU_ADLN_State_Setup;
with Intel_GPU_ADLN_Coarse_Pixel;
with Intel_GPU_ADLN_Color_Calc;
with Intel_GPU_ADLN_Constants;
with Intel_GPU_ADLN_Binding_Pool;
with Intel_GPU_ADLN_State_Pointers;
with Intel_GPU_ADLN_URB;
with Intel_GPU_ADLN_Passthrough;
with Intel_GPU_ADLN_Hull_Shader;
with Intel_GPU_ADLN_Domain_Shader;
with Intel_GPU_ADLN_Geometry_Shader;
with Intel_GPU_ADLN_Replication;
with Intel_GPU_ADLN_Vertex_Shader;
with Intel_GPU_ADLN_Pixel_Shader;
with Intel_GPU_ADLN_Pixel_Extra;
with Intel_GPU_ADLN_Viewport;
with Intel_GPU_ADLN_Clip;
with Intel_GPU_ADLN_Raster;
with Intel_GPU_ADLN_SF;
with Intel_GPU_ADLN_Windower;
with Intel_GPU_ADLN_SBE;
with Intel_GPU_ADLN_Sampling;
with Intel_GPU_ADLN_Sample_Pattern;
with Intel_GPU_ADLN_Pixel_Blend;
with Intel_GPU_ADLN_Depth_Stencil;
with Intel_GPU_ADLN_Null_Buffers;
with Intel_GPU_ADLN_Stencil_Sync;
with Intel_GPU_ADLN_Drawing_Rectangle;
with Intel_GPU_ADLN_Vertex_Fetch;
with Intel_GPU_ADLN_Triangle;
package body Intel_GPU_ADLN_Offscreen_Batch with SPARK_Mode is
   function Build (MOCS : Unsigned_32; Usable_URB_KiB, VS_Threads,
                   PS_Threads : Natural) return Image is
      Result : Image;
      Base : constant Intel_GPU_ADLN_State_Setup.Image :=
        Intel_GPU_ADLN_State_Setup.Build (MOCS);
      Constants : constant Intel_GPU_ADLN_Constants.Image :=
        Intel_GPU_ADLN_Constants.Build (MOCS);
      Pool : constant Intel_GPU_ADLN_Binding_Pool.Image :=
        Intel_GPU_ADLN_Binding_Pool.Disable (MOCS);
      URB : constant Intel_GPU_ADLN_URB.Image :=
        Intel_GPU_ADLN_URB.Build (Usable_URB_KiB);
      SF : constant Intel_GPU_ADLN_SF.Image :=
        Intel_GPU_ADLN_SF.Build (URB.VS_Entries);
      VS : constant Intel_GPU_ADLN_Vertex_Shader.Image :=
        Intel_GPU_ADLN_Vertex_Shader.Build (VS_Threads);
      PS : constant Intel_GPU_ADLN_Pixel_Shader.Image :=
        Intel_GPU_ADLN_Pixel_Shader.Build (PS_Threads);
      Null_Buffers : constant Intel_GPU_ADLN_Null_Buffers.Image :=
        Intel_GPU_ADLN_Null_Buffers.Build (MOCS);
      Rectangle : constant Intel_GPU_ADLN_Drawing_Rectangle.Image :=
        Intel_GPU_ADLN_Drawing_Rectangle.Build (64, 64);
      Fetch : constant Intel_GPU_ADLN_Vertex_Fetch.Image :=
        Intel_GPU_ADLN_Vertex_Fetch.Build (MOCS);
   begin
      if not (Base.Valid and Constants.Valid and Pool.Valid and URB.Valid and
              SF.Valid and VS.Valid and PS.Valid and Null_Buffers.Valid and
              Rectangle.Valid and Fetch.Valid)
      then
         return Result;
      end if;
      declare
         -- SBA precedes all pointer reissues. Constants are cleared before
         -- binding pointers commit them. HS/TE/DS/GS all off before draw.
         Sequence : constant Words :=
           Words (Base.Data) &
           Words (Intel_GPU_ADLN_Coarse_Pixel.Pointer) &
           Words (Intel_GPU_ADLN_Color_Calc.Pointer) &
           Words (Constants.Data) &
           Words (Pool.Data) &
           Words (Intel_GPU_ADLN_State_Pointers.Initial) &
           Words (URB.Data) &
           Words (Intel_GPU_ADLN_Passthrough.Streamout_Disabled) &
           Words (Intel_GPU_ADLN_Hull_Shader.Disabled) &
           Words (Intel_GPU_ADLN_Passthrough.Tessellation_Disabled) &
           Words (Intel_GPU_ADLN_Domain_Shader.Disabled) &
           Words (Intel_GPU_ADLN_Geometry_Shader.Disabled) &
           Words (Intel_GPU_ADLN_Replication.Disabled) &
           Words (VS.Data) &
           Words (Intel_GPU_ADLN_Viewport.Pointers) &
           Words (Intel_GPU_ADLN_Clip.Initial) &
           Words (Intel_GPU_ADLN_Raster.Initial) &
           Words (SF.Data) &
           Words (Intel_GPU_ADLN_Windower.Initial) &
           Words (Intel_GPU_ADLN_SBE.Initial) &
           Words (Intel_GPU_ADLN_Sampling.Initial) &
           Words (Intel_GPU_ADLN_Sample_Pattern.Standard) &
           Words (Intel_GPU_ADLN_Pixel_Blend.Initial) &
           Words (Intel_GPU_ADLN_Depth_Stencil.Initial) &
           Words (Null_Buffers.Data (0 .. 15)) &
           Words (Intel_GPU_ADLN_Stencil_Sync.Packet) &
           Words (Null_Buffers.Data (16 .. 23)) &
           Words (Rectangle.Data) &
           Words (PS.Data) &
           Words (Intel_GPU_ADLN_Pixel_Extra.Enabled) &
           Words (Fetch.Data) &
           Words (Intel_GPU_ADLN_Triangle.Topology) &
           Words (Intel_GPU_ADLN_Triangle.Draw) &
           Words'(0 => 16#05000000#);
      begin
         pragma Assert (Sequence'First = 0 and Sequence'Length <= Page'Length);
         Result.Data (0 .. Sequence'Last) := Sequence;
         Result.Count := Sequence'Length;
      end;
      Result.Valid := True;
      return Result;
   end Build;
end Intel_GPU_ADLN_Offscreen_Batch;
