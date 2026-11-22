with Interfaces;
package Intel_GPU_Firmware with SPARK_Mode is
   use Interfaces;
   CSS_Header_Bytes : constant := 128;
   type CSS_Header is array (Natural range 0 .. CSS_Header_Bytes - 1) of Unsigned_8;
   --  struct uc_css_header byte offsets (Linux i915 v6.12,
   --  gt/uc/intel_uc_fw_abi.h); every field is a little-endian u32.
   subtype Field_Offset is Natural range 0 .. CSS_Header_Bytes - 4;
   Module_Type_Offset       : constant Field_Offset := 0;
   Header_Size_Offset       : constant Field_Offset := 4;   --  header_size_dw
   Header_Version_Offset    : constant Field_Offset := 8;
   Module_ID_Offset         : constant Field_Offset := 12;
   Module_Vendor_Offset     : constant Field_Offset := 16;
   Size_Offset              : constant Field_Offset := 24;  --  size_dw
   Key_Size_Offset          : constant Field_Offset := 28;  --  key_size_dw
   Modulus_Size_Offset      : constant Field_Offset := 32;
   Exponent_Size_Offset     : constant Field_Offset := 36;
   SW_Version_Offset        : constant Field_Offset := 64;
   Private_Data_Size_Offset : constant Field_Offset := 120; --  GuC only
   --  header_size_dw counts the CSS dwords plus key, modulus and exponent.
   CSS_Words : constant := CSS_Header_Bytes / 4;

   --  CSS_SW_VERSION_UC_MAJOR/MINOR/PATCH: bits 23:16, 15:8, 7:0.
   type Firmware_Version is record
      Major, Minor, Patch : Unsigned_8 := 0;
   end record;
   function Version (Header : CSS_Header) return Firmware_Version
     with Global => null;
   --  "70.49.4".
   function Version_Image (V : Firmware_Version) return String
     with Global => null, Post => Version_Image'Result'Length <= 11;

   --  The raw header fields the boot log reports (see Decode for checks).
   type CSS_Fields is record
      Module_Type, Header_Words, Header_Version, Module_ID, Module_Vendor,
      Size_Words, Key_Words, Modulus_Words, Exponent_Words,
      Private_Data_Bytes : Unsigned_32 := 0;
   end record;
   function Fields (Header : CSS_Header) return CSS_Fields
     with Global => null;
   type Layout is record
      Valid : Boolean := False;
      Code_Bytes, Signature_Offset, Signature_Bytes : Unsigned_64 := 0;
   end record;
   --  Alder Lake-N / Gen12 layout admission only. Capacity must come from
   --  trusted platform evidence, NOT from a possibly stale WOPCM register.
   --  Upload byte counts include CSS + code, exclude the RSA signature.
   --  A zero HuC size means no HuC upload. This does not authorize writes or
   --  establish reset, forcewake, register-lock state or authentication.
   function Fits_ADLN_WOPCM
     (Capacity, GuC_Base, GuC_Size, GuC_Upload, HuC_Upload : Unsigned_64)
      return Boolean
   with Global => null,
     Post => (if Fits_ADLN_WOPCM'Result then
       Capacity <= 8 * 1024 * 1024 and then
       GuC_Base <= Capacity and then GuC_Size <= Capacity and then
       GuC_Upload <= GuC_Size and then HuC_Upload <= GuC_Base and then
       GuC_Base + GuC_Size + 36 * 1024 <= Capacity and then
       GuC_Upload + 24 * 1024 <= GuC_Size and then
       HuC_Upload + 16 * 1024 <= GuC_Base);
   --  CSS layout only; not signature verification, device compatibility,
   --  version admission or WOPCM/DMA admission. Optional modulus/exponent
   --  bytes need not be present in the file. Header storage must be stable.
   function Decode (Header : CSS_Header; Blob_Bytes : Unsigned_64) return Layout
   with Global => null,
     Post =>
       (if Decode'Result.Valid then
          Decode'Result.Code_Bytes > 0 and then
          Decode'Result.Signature_Bytes > 0 and then
          Decode'Result.Signature_Offset >= 128 and then
          Decode'Result.Signature_Offset - 128 = Decode'Result.Code_Bytes and then
          Decode'Result.Signature_Offset <= Blob_Bytes and then
          Decode'Result.Signature_Bytes <= Blob_Bytes - Decode'Result.Signature_Offset
        else Decode'Result.Code_Bytes = 0 and then
          Decode'Result.Signature_Offset = 0 and then
          Decode'Result.Signature_Bytes = 0);
   --  The pinned tgl_guc_70.bin (linux-firmware 20250917, 70.49.4), which
   --  Linux i915 loads on every Alder Lake-N (intel_uc_fw.c, v6.12, lines
   --  293-300: ADL-N uses the ADL-S row). Intel_GPU_ADLN_Devices selects it.
   Selected_GuC_Blob_Bytes : constant := 335_360;
   Selected_GuC_Code_Bytes : constant := 334_976;
   Selected_GuC_Version : constant Firmware_Version :=
     (Major => 70, Minor => 49, Patch => 4);
   --  Bring-up allowlist for the hash-pinned tgl_guc_70.bin (70.49.4).
   --  Metadata can be forged: this is NOT authenticity or full ABI support.
   --  Updating the image pin requires deliberately updating this selection.
   function Matches_Selected_ADLN_GuC
     (Header : CSS_Header; Blob_Bytes : Unsigned_64) return Boolean
   with Global => null,
     Post => (if Matches_Selected_ADLN_GuC'Result then
       Decode (Header, Blob_Bytes).Valid);
end Intel_GPU_Firmware;
