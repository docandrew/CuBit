with Intel_GPU_ADLN_Vertex_Fetch;
package body Intel_GPU_ADLN_SF with SPARK_Mode is
   function Build (VS_Entries : Natural) return Image is
      Result : Image;
   begin
      if VS_Entries not in 64 .. 3576 or else VS_Entries mod 8 /= 0 then
         return Result;
      end if;
      Result.Data :=
        [Intel_GPU_ADLN_Vertex_Fetch.Encode
           (Intel_GPU_ADLN_Vertex_Fetch.Header'
              (Length => 2, Subopcode => 16#13#, others => <>)),
         Encode (Transform_Control'(Viewport_Transform => 1, Statistics => 1,
           Line_Width_128ths => 128, others => <>)),
         Encode (Deref_Control'(Block_Size =>
           (if VS_Entries < 192 then 1 else 0), others => <>)),
         Encode (Point_Control'(Width_Eighths => 8, Width_From_State => 1,
           True_AA_Line_Distance => 1, others => <>))];
      Result.Valid := True;
      return Result;
   end Build;
end Intel_GPU_ADLN_SF;
