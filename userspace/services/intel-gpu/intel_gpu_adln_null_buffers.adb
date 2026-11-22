with Intel_GPU_ADLN_Vertex_Fetch;
package body Intel_GPU_ADLN_Null_Buffers with SPARK_Mode is
   function Build (MOCS : Unsigned_32) return Image is
      Result : Image;
      function Header (Subopcode, Length : Natural) return Unsigned_32 is
        (Intel_GPU_ADLN_Vertex_Fetch.Encode
           (Intel_GPU_ADLN_Vertex_Fetch.Header'
              (Subopcode => Intel_GPU_ADLN_Vertex_Fetch.B8 (Subopcode),
               Length => Intel_GPU_ADLN_Vertex_Fetch.B8 (Length),
               others => <>)))
        with Pre => Subopcode <= 255 and Length <= 255;
   begin
      if MOCS not in 2 .. 126 or else MOCS mod 2 /= 0 then
         return Result;
      end if;
      Result.Data :=
        [0 => Header (5, 6),
         1 => Encode (Depth_Control'
           (Surface_Type => 7, Surface_Format => 1, others => <>)),
         4 | 12 => Encode (Dimensions'(others => <>)),
         5 | 13 => Encode (Array_Control'(MOCS => B7 (MOCS), others => <>)),
         6 | 14 => Encode (Tiling_Control'(others => <>)),
         7 | 15 => Encode (View_Control'(others => <>)),
         8 => Header (6, 6),
         9 => Encode (Stencil_Control'(Surface_Type => 7, others => <>)),
         16 => Header (7, 3),
         17 => Encode (HiZ_Control'(MOCS => B7 (MOCS), others => <>)),
         20 => Encode (HiZ_QPitch'(others => <>)),
         21 => Header (4, 1),
         23 => Encode (Clear_Control'(others => <>)),
         others => 0];
      -- Remaining DWORDs are whole-field zero addresses and IEEE +0 clear.
      -- HiZ disabled in Depth_Control; no depth/stencil writes/compression.
      Result.Valid := True;
      return Result;
   end Build;
end Intel_GPU_ADLN_Null_Buffers;
