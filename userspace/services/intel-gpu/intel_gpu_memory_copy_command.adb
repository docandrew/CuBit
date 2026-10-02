with Intel_GPU_VA_Encoding;
package body Intel_GPU_Memory_Copy_Command with SPARK_Mode is
   function Build (Source, Destination : Unsigned_64) return Command is
      S, D : Unsigned_64;
   begin
      if not Address_Valid (Source) or else not Address_Valid (Destination) then
         return (others => <>);
      end if;
      S := Intel_GPU_VA_Encoding.Canonical (Source);
      D := Intel_GPU_VA_Encoding.Canonical (Destination);
      return (Valid => True, Words =>
        [Encode ((others => <>)), Unsigned_32 (D and 16#FFFFFFFF#),
         Unsigned_32 (Shift_Right (D, 32)), Unsigned_32 (S and 16#FFFFFFFF#),
         Unsigned_32 (Shift_Right (S, 32))]);
   end Build;
end Intel_GPU_Memory_Copy_Command;
