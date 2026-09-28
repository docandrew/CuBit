with Interfaces;
package Intel_GPU_Firmware with SPARK_Mode is
   use Interfaces;
   type CSS_Header is array (Natural range 0 .. 127) of Unsigned_8;
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
   --  Bring-up allowlist for the hash-pinned tgl_guc_70.bin (70.49.4).
   --  Metadata can be forged: this is NOT authenticity or full ABI support.
   --  Updating the image pin requires deliberately updating this selection.
   function Matches_Selected_ADLN_GuC
     (Header : CSS_Header; Blob_Bytes : Unsigned_64) return Boolean
   with Global => null,
     Post => (if Matches_Selected_ADLN_GuC'Result then
       Decode (Header, Blob_Bytes).Valid);
end Intel_GPU_Firmware;
