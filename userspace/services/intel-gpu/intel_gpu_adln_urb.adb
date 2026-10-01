with Intel_GPU_ADLN_Probe_Shaders;
package body Intel_GPU_ADLN_URB with SPARK_Mode is
   pragma Compile_Time_Error (Intel_GPU_ADLN_Probe_Shaders.Vertex_URB_Entry_Size /= 1,
                              "URB probe builder requires a one-row vertex shader");
   function Build (Usable_KiB : Natural) return Image is
      Result : Image;
      Count : Natural;
   begin
      if Usable_KiB not in 40 .. 512 then
         return Result;
      end if;
      -- Only whole8KiB chunks; each contains128 one-row64-byte entries.
      Count := Natural'Min (3576, (Usable_KiB / 8 - 4) * 128);
      Result.VS_Entries := Count;
      for Stage in Natural range 0 .. 3 loop
         Result.Data (Stage * 2) := Intel_GPU_ADLN_Vertex_Fetch.Encode
           (Header'(Subopcode => B8 (16#30# + Stage), others => <>));
         Result.Data (Stage * 2 + 1) := Encode (Stage_Control'
           (Entries => (if Stage = 0 then B16 (Count) else 0), others => <>));
      end loop;
      Result.Valid := True;
      return Result;
   end Build;
end Intel_GPU_ADLN_URB;
