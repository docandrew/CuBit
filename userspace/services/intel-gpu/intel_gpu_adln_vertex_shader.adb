with Intel_GPU_ADLN_Probe_Shaders;
with Intel_GPU_ADLN_Vertex_Fetch;
package body Intel_GPU_ADLN_Vertex_Shader with SPARK_Mode is
   pragma Compile_Time_Error
     (Intel_GPU_ADLN_Probe_Shaders.Vertex_Dispatch_Mode /= 3 or
      Intel_GPU_ADLN_Probe_Shaders.Vertex_GRF_Start /= 2 or
      Intel_GPU_ADLN_Probe_Shaders.Vertex_URB_Read_Length /= 1,
      "vertex dispatch must match the fixed compiled SIMD8 payload");
   function Build (Thread_Limit : Natural) return Image is
      Result : Image;
      Kernel : constant Unsigned_64 := Encode (Kernel_Control'
        (Offset_64B => B58 (Intel_GPU_ADLN_Probe_Shaders.Vertex_Offset / 64), others => <>));
   begin
      if Thread_Limit not in 1 .. 546 then
         return Result;
      end if;
      Result.Data :=
        [Intel_GPU_ADLN_Vertex_Fetch.Encode
           (Intel_GPU_ADLN_Vertex_Fetch.Header'(Length => 7, Subopcode => 16#10#, others => <>)),
         Unsigned_32 (Kernel and 16#FFFF_FFFF#), Unsigned_32 (Shift_Right (Kernel, 32)),
         Encode (Shader_Control'(others => <>)),
         0, 0, -- No scratch accesses in the fixed compiled shader.
         Encode (Payload_Control'(others => <>)),
         Encode (Dispatch_Control'(Threads_Minus_One => B10 (Thread_Limit - 1), others => <>)),
         Encode (Output_Control'(others => <>))];
      Result.Valid := True;
      return Result;
   end Build;
end Intel_GPU_ADLN_Vertex_Shader;
