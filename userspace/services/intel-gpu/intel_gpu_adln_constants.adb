package body Intel_GPU_ADLN_Constants with SPARK_Mode is
   function Build (MOCS : Unsigned_32) return Image is
      Result : Image;
   begin
      if MOCS = 0 or else MOCS > 126 or else MOCS mod 2 /= 0 then
         return Result;
      end if;
      for Stage in Natural range 0 .. 4 loop
         Result.Data (Stage * 2) := Intel_GPU_ADLN_Vertex_Fetch.Encode
           (Header'(Opcode => 1, Subopcode => B8 (16#12# + Stage), others => <>));
         Result.Data (Stage * 2 + 1) := Encode (Allocation_Control'(others => <>));
      end loop;
      -- No valid buffers and update-mode clear resets all pointers AND sizes.
      Result.Data (10) := Encode (Clear_Header'(others => <>));
      Result.Data (11) := Encode (Clear_Control'(MOCS => B7 (MOCS), others => <>));
      Result.Valid := True;
      return Result;
   end Build;
end Intel_GPU_ADLN_Constants;
