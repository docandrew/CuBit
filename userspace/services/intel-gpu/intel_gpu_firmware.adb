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
     with Pre => Offset <= Field_Offset'Last,
          Post => Word'Result <= 16#FFFF_FFFF#
   is
   begin
      return Unsigned_64 (Header (Offset)) +
        Unsigned_64 (Header (Offset + 1)) * 256 +
        Unsigned_64 (Header (Offset + 2)) * 65536 +
        Unsigned_64 (Header (Offset + 3)) * 16777216;
   end Word;
   function Word32 (Header : CSS_Header; Offset : Field_Offset) return Unsigned_32 is
     (Unsigned_32 (Word (Header, Offset)));
   Byte_Bits : constant := 8;
   function Version (Header : CSS_Header) return Firmware_Version is
      Packed : constant Unsigned_32 := Word32 (Header, SW_Version_Offset);
      Byte_Mask : constant Unsigned_32 := 16#FF#;
   begin
      return (Major => Unsigned_8 (Shift_Right (Packed, 2 * Byte_Bits) and Byte_Mask),
              Minor => Unsigned_8 (Shift_Right (Packed, Byte_Bits) and Byte_Mask),
              Patch => Unsigned_8 (Packed and Byte_Mask));
   end Version;
   function Version_Image (V : Firmware_Version) return String is
      Decimal : constant := 10;
      Max_Length : constant := 11; --  "255.255.255"
      Result : String (1 .. Max_Length) := [others => ' '];
      Last : Natural range 0 .. Max_Length := 0;
      procedure Append (B : Unsigned_8)
        with Pre => Last <= Max_Length - 3,
             Post => Last <= Last'Old + 3 and then Last > Last'Old
      is
         Hundreds : constant Unsigned_8 := B / (Decimal * Decimal);
         Tens : constant Unsigned_8 := B / Decimal mod Decimal;
         Ones : constant Unsigned_8 := B mod Decimal;
      begin
         if Hundreds > 0 then
            Last := Last + 1; Result (Last) := Character'Val (Character'Pos ('0') + Natural (Hundreds));
         end if;
         if Hundreds > 0 or else Tens > 0 then
            Last := Last + 1; Result (Last) := Character'Val (Character'Pos ('0') + Natural (Tens));
         end if;
         Last := Last + 1; Result (Last) := Character'Val (Character'Pos ('0') + Natural (Ones));
      end Append;
   begin
      Append (V.Major); Last := Last + 1; Result (Last) := '.';
      Append (V.Minor); Last := Last + 1; Result (Last) := '.';
      Append (V.Patch);
      return Result (1 .. Last);
   end Version_Image;
   function Fields (Header : CSS_Header) return CSS_Fields is
     ((Module_Type => Word32 (Header, Module_Type_Offset),
       Header_Words => Word32 (Header, Header_Size_Offset),
       Header_Version => Word32 (Header, Header_Version_Offset),
       Module_ID => Word32 (Header, Module_ID_Offset),
       Module_Vendor => Word32 (Header, Module_Vendor_Offset),
       Size_Words => Word32 (Header, Size_Offset),
       Key_Words => Word32 (Header, Key_Size_Offset),
       Modulus_Words => Word32 (Header, Modulus_Size_Offset),
       Exponent_Words => Word32 (Header, Exponent_Size_Offset),
       Private_Data_Bytes => Word32 (Header, Private_Data_Size_Offset)));
   function Decode (Header : CSS_Header; Blob_Bytes : Unsigned_64) return Layout is
      Header_Words : constant Unsigned_64 := Word (Header, Header_Size_Offset);
      Total_Words : constant Unsigned_64 := Word (Header, Size_Offset);
      Key_Words : constant Unsigned_64 := Word (Header, Key_Size_Offset);
      Modulus_Words : constant Unsigned_64 := Word (Header, Modulus_Size_Offset);
      Exponent_Words : constant Unsigned_64 := Word (Header, Exponent_Size_Offset);
      Code_Size, Signature_Start, Signature_Size : Unsigned_64;
   begin
      if Blob_Bytes < CSS_Header_Bytes or else
        Header_Words /= CSS_Words + Key_Words + Modulus_Words + Exponent_Words or else
        Total_Words <= Header_Words or else Key_Words = 0
      then
         return (others => <>);
      end if;
      Code_Size := (Total_Words - Header_Words) * 4;
      Signature_Start := CSS_Header_Bytes + Code_Size;
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
      --  The pinned header's values (tests/intel-gpu/firmware_tests.adb
      --  carries the real header): CSS module type 6, header version 1.0,
      --  module 0, vendor Intel, RSA-2048 (64-dword key and modulus, one
      --  exponent dword), 8 MiB + 4 KiB private data.
      Module_Type : constant := 6;
      Header_Version : constant := 16#0001_0000#;
      Module_ID : constant := 0;
      Intel_Vendor : constant := 16#8086#;
      RSA_Words : constant := 64;
      Exponent_Words : constant := 1;
      Signature_Bytes : constant := RSA_Words * 4;
      Private_Data_Bytes : constant := 16#0080_1000#;
      Version_Word_Limit : constant := 2 ** (3 * Byte_Bits); --  bits 31:24 zero
   begin
      return Parsed.Valid and then Blob_Bytes = Selected_GuC_Blob_Bytes and then
        Parsed.Code_Bytes = Selected_GuC_Code_Bytes and then
        Parsed.Signature_Bytes = Signature_Bytes and then
        Word (Header, Module_Type_Offset) = Module_Type and then
        Word (Header, Header_Version_Offset) = Header_Version and then
        Word (Header, Module_ID_Offset) = Module_ID and then
        Word (Header, Module_Vendor_Offset) = Intel_Vendor and then
        Word (Header, Modulus_Size_Offset) = RSA_Words and then
        Word (Header, Exponent_Size_Offset) = Exponent_Words and then
        Version (Header) = Selected_GuC_Version and then
        Word (Header, SW_Version_Offset) < Version_Word_Limit and then
        Word (Header, Private_Data_Size_Offset) = Private_Data_Bytes;
   end Matches_Selected_ADLN_GuC;
end Intel_GPU_Firmware;
