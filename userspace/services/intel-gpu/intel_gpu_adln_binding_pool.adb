with Intel_GPU_ADLN_Vertex_Fetch;
package body Intel_GPU_ADLN_Binding_Pool with SPARK_Mode is
   function Disable (MOCS : Unsigned_32) return Image is
      Result : Image;
      Address : Unsigned_64;
   begin
      if MOCS not in 2 .. 126 or else MOCS mod 2 /= 0 then
         return Result;
      end if;
      Address := Encode (Address_Control'
        (MOCS_Index => B6 (MOCS / 2), others => <>));
      Result.Data :=
        [Intel_GPU_ADLN_Vertex_Fetch.Encode
           (Intel_GPU_ADLN_Vertex_Fetch.Header'
              (Length => 2, Opcode => 1, Subopcode => 16#19#, others => <>)),
         Unsigned_32 (Address and 16#FFFF_FFFF#),
         Unsigned_32 (Shift_Right (Address,32)),
         Encode (Size_Control'(others => <>))];
      -- Zero size is legal only because Enable_Pool is clear. No address
      -- is published; binding pointers must be reissued after this packet.
      Result.Valid := True;
      return Result;
   end Disable;
end Intel_GPU_ADLN_Binding_Pool;
