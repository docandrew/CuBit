package body Intel_GPU_ADLN_PPGTT with SPARK_Mode is
   function Encode_Leaf
     (DMA_Address : Unsigned_64; Policy : Cache_Policy; Access_Mode : Page_Access)
      return Unsigned_64 is
   begin
      if not Valid_DMA_Page (DMA_Address) or else Access_Mode = Read_Only then
         return 0;
      end if;
      return DMA_Address + 3 + Cache_Bits (Policy);
   end Encode_Leaf;
   function Encode_Directory (DMA_Address : Unsigned_64) return Unsigned_64 is
   begin
      if not Valid_DMA_Page (DMA_Address) then return 0; end if;
      return DMA_Address + 3;
   end Encode_Directory;
   function Locate (GPU_Address : Unsigned_64) return Walk is
      Address, R1, R2, R3, R4 : Long_Long_Integer;
      Result : Walk;
   begin
      if GPU_Address >= 2 ** 48 then return (others => <>); end if;
      Address := Long_Long_Integer (GPU_Address);
      R1 := Address / 4096;
      R2 := R1 / 512;
      R3 := R2 / 512;
      R4 := R3 / 512;
      Result := (True, Table_Index (R4), Table_Index (R3 mod 512),
                 Table_Index (R2 mod 512), Table_Index (R1 mod 512),
                 Page_Offset (Address mod 4096));
      pragma Assert (R1 * 4096 + Long_Long_Integer (Result.Offset) = Address);
      pragma Assert (R2 * 512 + Long_Long_Integer (Result.PT) = R1);
      pragma Assert (R3 * 512 + Long_Long_Integer (Result.PD) = R2);
      pragma Assert (R4 * 512 + Long_Long_Integer (Result.PDP) = R3);
      return Result;
   end Locate;
end Intel_GPU_ADLN_PPGTT;
