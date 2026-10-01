with Intel_GPU_ADLN_Probe_Shaders;
with Intel_GPU_ADLN_Vertex_Fetch;
with Intel_GPU_ADLN_Vertex_Shader;
package body Intel_GPU_ADLN_Pixel_Shader with SPARK_Mode is
   package Probe renames Intel_GPU_ADLN_Probe_Shaders;
   package VS renames Intel_GPU_ADLN_Vertex_Shader;
   pragma Compile_Time_Error
     (Probe.Fragment_GRF_Start /= 2 or not Probe.Fragment_Vector_Mask,
      "fixed fragment dispatch metadata changed");
   function Build (Thread_Limit : Natural) return Image is
      Result : Image;
      Kernel0 : constant Unsigned_64 := VS.Encode (VS.Kernel_Control'
        (Offset_64B => VS.B58 (Probe.Fragment_Offset / 64), others => <>));
      Kernel2 : constant Unsigned_64 := VS.Encode (VS.Kernel_Control'
        (Offset_64B => VS.B58 ((Probe.Fragment_Offset +
          Probe.Fragment_SIMD16_Offset) / 64), others => <>));
   begin
      if Thread_Limit not in 1 .. 64 then
         return Result;
      end if;
      Result.Data :=
        [Intel_GPU_ADLN_Vertex_Fetch.Encode
           (Intel_GPU_ADLN_Vertex_Fetch.Header'
              (Length => 10, Subopcode => 16#20#, others => <>)),
         Unsigned_32 (Kernel0 and 16#FFFF_FFFF#), Unsigned_32 (Shift_Right (Kernel0, 32)),
         Encode (Shader_Control'(Vector_Mask => 1, others => <>)),
         0, 0, -- Fixed shader does not access scratch.
         Encode (Dispatch_Control'(SIMD8 => 1, SIMD16 => 1,
           Threads_Minus_One => B9 (Thread_Limit - 1), others => <>)),
         Encode (Payload_Control'(GRF_Slot0 => 2, GRF_Slot2 => 2, others => <>)),
         -- Slot1 is unused; Mesa helper supplies the base kernel offset.
         Unsigned_32 (Kernel0 and 16#FFFF_FFFF#), Unsigned_32 (Shift_Right (Kernel0, 32)),
         Unsigned_32 (Kernel2 and 16#FFFF_FFFF#), Unsigned_32 (Shift_Right (Kernel2, 32))];
      Result.Valid := True;
      return Result;
   end Build;
end Intel_GPU_ADLN_Pixel_Shader;
