with Intel_GPU_Submission_Backing;
with Intel_GPU_ADLN_Probe_Shaders;
package body Intel_GPU_ADLN_Vertex_Fetch with SPARK_Mode is
   function Build (MOCS : Unsigned_32) return Image is
      Address : constant Unsigned_64 :=
        Intel_GPU_Submission_Backing.Render_State_GPU_VA +
        Intel_GPU_ADLN_Probe_Shaders.Vertex_Data_Offset;
      Result : Image;
   begin
      if MOCS = 0 or else MOCS > 126 or else MOCS mod 2 /= 0 then
         return Result;
      end if;
      Result.Data :=
        [Encode (Header'(Length => 3, Subopcode => 8, others => <>)),
         Encode (Buffer_Control'(MOCS => B7 (MOCS), others => <>)),
         Unsigned_32 (Address and 16#FFFFFFFF#), Unsigned_32 (Shift_Right (Address, 32)),
         Intel_GPU_ADLN_Probe_Shaders.Vertices'Length * 4,
         Encode (Header'(Length => 1, Subopcode => 9, others => <>)),
         Encode (Element_Control'(others => <>)),
         Encode (Component_Control'(others => <>)),
         Encode (VF_Control'(others => <>)), 0,
         Encode (Header'(Length => 1, Subopcode => 16#49#, others => <>)),
         Encode (Instancing_Control'(others => <>)), 0,
         Encode (Header'(Length => 0, Subopcode => 16#4A#, others => <>)),
         Encode (SGV_Control'(others => <>)),
         Encode (Header'(Length => 1, Subopcode => 16#56#, others => <>)),
         Encode (Extended_SGV_Control'(others => <>)), Encode (XP2_Control'(others => <>))];
      Result.Valid := True;
      return Result;
   end Build;
end Intel_GPU_ADLN_Vertex_Fetch;
