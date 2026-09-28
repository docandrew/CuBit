package body Intel_GPU_Firmware with SPARK_Mode is
   function Fits_ADLN_WOPCM
     (Capacity, GuC_Base, GuC_Size, GuC_Upload, HuC_Upload : Unsigned_64)
      return Boolean
   is
      Context_Bytes : constant Unsigned_64 := 36 * 1024;
      Lower_Reserve : constant Unsigned_64 := 16 * 1024;
      GuC_Reserve : constant Unsigned_64 := 24 * 1024;
   begin
      --  Subtraction guards are intentional: no hostile input may wrap an
      --  addition and make an oversized region appear to fit.
      return Capacity <= 8 * 1024 * 1024 and then
        Capacity >= Context_Bytes and then
        GuC_Base mod (16 * 1024) = 0 and then
        GuC_Size mod 4096 = 0 and then
        GuC_Base >= Lower_Reserve and then
        HuC_Upload <= GuC_Base - Lower_Reserve and then
        GuC_Base <= Capacity - Context_Bytes and then
        GuC_Size <= Capacity - Context_Bytes - GuC_Base and then
        GuC_Size >= GuC_Reserve and then
        GuC_Upload > 128 and then GuC_Upload mod 4 = 0 and then
        (HuC_Upload = 0 or else
           (HuC_Upload > 128 and then HuC_Upload mod 4 = 0)) and then
        GuC_Upload <= GuC_Size - GuC_Reserve;
   end Fits_ADLN_WOPCM;
   function Word (Header : CSS_Header; Offset : Natural) return Unsigned_64
     with Pre => Offset <= 124,
          Post => Word'Result <= 16#FFFF_FFFF#
   is
   begin
      return Unsigned_64 (Header (Offset)) +
        Unsigned_64 (Header (Offset + 1)) * 256 +
        Unsigned_64 (Header (Offset + 2)) * 65536 +
        Unsigned_64 (Header (Offset + 3)) * 16777216;
   end Word;
   function Decode (Header : CSS_Header; Blob_Bytes : Unsigned_64) return Layout is
      Header_Words : constant Unsigned_64 := Word (Header, 4);
      Total_Words : constant Unsigned_64 := Word (Header, 24);
      Key_Words : constant Unsigned_64 := Word (Header, 28);
      Modulus_Words : constant Unsigned_64 := Word (Header, 32);
      Exponent_Words : constant Unsigned_64 := Word (Header, 36);
      Code_Size, Signature_Start, Signature_Size : Unsigned_64;
   begin
      if Blob_Bytes < 128 or else
        Header_Words /= 32 + Key_Words + Modulus_Words + Exponent_Words or else
        Total_Words <= Header_Words or else Key_Words = 0
      then
         return (others => <>);
      end if;
      Code_Size := (Total_Words - Header_Words) * 4;
      Signature_Start := 128 + Code_Size;
      Signature_Size := Key_Words * 4;
      if Signature_Start > Blob_Bytes or else
        Signature_Size > Blob_Bytes - Signature_Start
      then
         return (others => <>);
      end if;
      return (True, Code_Size, Signature_Start, Signature_Size);
   end Decode;
   function Matches_Selected_ADLN_GuC
     (Header : CSS_Header; Blob_Bytes : Unsigned_64) return Boolean
   is
      Parsed : constant Layout := Decode (Header, Blob_Bytes);
   begin
      return Parsed.Valid and then Blob_Bytes = 335360 and then
        Parsed.Code_Bytes = 334976 and then Parsed.Signature_Bytes = 256 and then
        Word (Header, 0) = 6 and then -- selected CSS module type
        Word (Header, 8) = 16#0001_0000# and then
        Word (Header, 12) = 0 and then
        Word (Header, 16) = 16#8086# and then
        Word (Header, 32) = 64 and then Word (Header, 36) = 1 and then
        Word (Header, 64) = 16#0046_3104# and then -- 70.49.4
        Word (Header, 120) = 16#0080_1000#;
   end Matches_Selected_ADLN_GuC;
end Intel_GPU_Firmware;
