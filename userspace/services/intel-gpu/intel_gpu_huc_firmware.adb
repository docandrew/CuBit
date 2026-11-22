with Intel_GPU_HuC_Registers;
package body Intel_GPU_HuC_Firmware with SPARK_Mode is
   -- CSS header dword offsets (legacy uc_css_header, intel_uc_fw_abi.h).
   Module_Type_Offset : constant := 0;
   Header_Version_Offset : constant := 8;
   Module_Id_Offset : constant := 12;
   Vendor_Offset : constant := 16;
   Key_Size_Offset : constant := 28;
   Modulus_Size_Offset : constant := 32;
   Exponent_Size_Offset : constant := 36;
   Version_Offset : constant := 64;
   Module_Type_CSS : constant Unsigned_64 := 6;
   Header_Version_1 : constant Unsigned_64 := 16#0001_0000#;
   Intel_Vendor : constant Unsigned_64 := 16#8086#;
   RSA_2048_Words : constant Unsigned_64 := 64;
   Exponent_Words : constant Unsigned_64 := 1;

   function Word (Header : Intel_GPU_Firmware.CSS_Header; Offset : Natural)
     return Unsigned_64
     with Pre => Offset <= 124, Post => Word'Result <= 16#FFFF_FFFF#
   is
   begin
      return Unsigned_64 (Header (Offset)) +
        Unsigned_64 (Header (Offset + 1)) * 256 +
        Unsigned_64 (Header (Offset + 2)) * 65536 +
        Unsigned_64 (Header (Offset + 3)) * 16777216;
   end Word;

   function Matches_Selected_ADLN_HuC
     (Header : Intel_GPU_Firmware.CSS_Header; Blob_Bytes : Unsigned_64)
     return Boolean
   is
      Parsed : constant Intel_GPU_Firmware.Layout :=
        Intel_GPU_Firmware.Decode (Header, Blob_Bytes);
   begin
      return Parsed.Valid and then Blob_Bytes = Selected_Blob_Bytes and then
        Parsed.Code_Bytes = Selected_Code_Bytes and then
        Parsed.Signature_Offset = CSS_Header_Bytes + Selected_Code_Bytes and then
        Parsed.Signature_Bytes = Selected_Signature_Bytes and then
        Word (Header, Module_Type_Offset) = Module_Type_CSS and then
        Word (Header, Header_Version_Offset) = Header_Version_1 and then
        Word (Header, Module_Id_Offset) = 0 and then
        Word (Header, Vendor_Offset) = Intel_Vendor and then
        Word (Header, Key_Size_Offset) = RSA_2048_Words and then
        Word (Header, Modulus_Size_Offset) = RSA_2048_Words and then
        Word (Header, Exponent_Size_Offset) = Exponent_Words and then
        Word (Header, Version_Offset) = Unsigned_64 (Selected_Version);
   end Matches_Selected_ADLN_HuC;

   function Select_Layout (HuC, GuC : Upload_Bytes) return WOPCM_Layout is
      Base : constant Unsigned_64 := GuC_Base_For (HuC);
      Size : Unsigned_64;
   begin
      if HuC = 0 or else Base > WOPCM_Capacity - HW_Context_Reserved then
         return (others => <>);
      end if;
      Size := WOPCM_Capacity - HW_Context_Reserved - Base;
      if not Intel_GPU_Firmware.Fits_ADLN_WOPCM
        (WOPCM_Capacity, Base, Size, GuC, HuC)
      then
         return (others => <>);
      end if;
      return (Valid => True, Base => Base, Size => Size);
   end Select_Layout;

   function WOPCM_Offset_Value (Layout : WOPCM_Layout) return Unsigned_32 is
     (Unsigned_32 (Layout.Base mod 2 ** 32) or
        Intel_GPU_HuC_Registers.HuC_Loading_Agent_GuC);

   function WOPCM_Admits_HuC (Raw : Unsigned_32; HuC : Upload_Bytes)
     return Boolean
   is
      use Intel_GPU_HuC_Registers;
   begin
      return Raw /= Unreadable and then HuC > 0 and then
        (Raw and WOPCM_Offset_Valid) /= 0 and then
        (Raw and HuC_Loading_Agent_GuC) /= 0 and then
        Unsigned_64 (Raw and WOPCM_Offset_Mask) >= GuC_Base_For (HuC);
   end WOPCM_Admits_HuC;
end Intel_GPU_HuC_Firmware;
