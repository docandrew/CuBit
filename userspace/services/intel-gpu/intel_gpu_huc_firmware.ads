with Interfaces; use Interfaces;
with Intel_GPU_Firmware;
with Intel_GPU_Media_Engines;
-- HuC firmware selection, pinned-blob admission, WOPCM partitioning with a
-- HuC region, and RSA page placement for Gen12 ADL-P/ADL-N.
-- Hardware facts from Linux v6.16 (no code copied):
--   i915/gt/uc/intel_uc_fw.c:113-119   ADL-P HuC: huc_raw(tgl), tgl 7.9.3
--   i915/gt/uc/intel_uc_fw.c:669-672   RSA size = key_size_dw * 4
--   i915/gt/uc/intel_uc_fw.c:1185-1240 HuC RSA always read from memory, from a
--     separate GuC-accessible page (offset >= pin bias, below GUC_GGTT_TOP)
--   i915/gt/uc/intel_uc_fw.c:1111-1112 DMA size = CSS header + ucode
--   i915/gt/intel_wopcm.c:44-61,298-305 GuC base = align16K(HuC + 16K)
--   i915/gt/uc/intel_guc.h:400-425     GUC_GGTT_TOP 0xFEE00000
--   i915/gt/uc/intel_huc.c:294-297     no VCS, no HuC
-- Metadata admission is NOT authenticity: the GuC verifies the signature.
package Intel_GPU_HuC_Firmware with SPARK_Mode is
   type Firmware_Id is (No_HuC, TGL_HuC_7_9_3);
   -- Path inside the firmware read scope (same directory as the GuC blob).
   function Path (Item : Firmware_Id) return String is
     (case Item is
        when No_HuC => "",
        when TGL_HuC_7_9_3 => "firmware/intel/tgl_huc.bin");
   function Select_HuC (Media : Intel_GPU_Media_Engines.Engines)
     return Firmware_Id is
     (if Intel_GPU_Media_Engines.Has_Video (Media) then TGL_HuC_7_9_3
      else No_HuC);

   -- linux-firmware i915/tgl_huc.bin (= tgl_huc_7.9.3.bin), sha256
   -- dbb1316bac13a76427ffa7827e52b5b9affe8a4cadd23dc22e2a54e35c53bdad.
   Selected_Blob_Bytes : constant Unsigned_64 := 589_888;
   Selected_Code_Bytes : constant Unsigned_64 := 589_504;
   Selected_Signature_Bytes : constant Unsigned_64 := 256;
   Selected_Version : constant Unsigned_32 := 16#0007_0903#; -- 7.9.3
   CSS_Header_Bytes : constant Unsigned_64 := 128;

   function Matches_Selected_ADLN_HuC
     (Header : Intel_GPU_Firmware.CSS_Header; Blob_Bytes : Unsigned_64)
     return Boolean
   with Global => null,
     Post => (if Matches_Selected_ADLN_HuC'Result then
       Intel_GPU_Firmware.Decode (Header, Blob_Bytes).Valid and then
       Blob_Bytes = Selected_Blob_Bytes and then
       Intel_GPU_Firmware.Decode (Header, Blob_Bytes).Signature_Offset =
         CSS_Header_Bytes + Selected_Code_Bytes and then
       Intel_GPU_Firmware.Decode (Header, Blob_Bytes).Signature_Bytes =
         Selected_Signature_Bytes);

   -- DMA transfer size: CSS header + ucode, excluding the RSA signature.
   Max_Upload_Bytes : constant Unsigned_64 := 2 * 1024 * 1024;
   subtype Upload_Bytes is Unsigned_64 range 0 .. Max_Upload_Bytes;
   Selected_Upload_Bytes : constant Upload_Bytes :=
     CSS_Header_Bytes + Selected_Code_Bytes;

   -- WOPCM partition with HuC. Gen11+ capacity is 2 MiB.
   WOPCM_Capacity : constant Unsigned_64 := 2 * 1024 * 1024;
   WOPCM_Reserved : constant Unsigned_64 := 16 * 1024;   -- WOPCM_RESERVED_SIZE
   GuC_Offset_Alignment : constant Unsigned_64 := 16 * 1024;
   HW_Context_Reserved : constant Unsigned_64 := 36 * 1024; -- ICL+ reserve

   function GuC_Base_For (HuC : Upload_Bytes) return Unsigned_64 is
     ((HuC + WOPCM_Reserved + GuC_Offset_Alignment - 1) /
        GuC_Offset_Alignment * GuC_Offset_Alignment)
   with Post => GuC_Base_For'Result mod GuC_Offset_Alignment = 0 and then
     GuC_Base_For'Result >= HuC + WOPCM_Reserved and then
     GuC_Base_For'Result < HuC + WOPCM_Reserved + GuC_Offset_Alignment;

   type WOPCM_Layout is record
      Valid : Boolean := False;
      Base, Size : Unsigned_64 := 0;
   end record;
   -- Unlocked hardware only: a WOPCM already locked without a HuC region
   -- cannot be repartitioned before a full device reset.
   function Select_Layout (HuC, GuC : Upload_Bytes) return WOPCM_Layout
   with Global => null,
     Post => (if Select_Layout'Result.Valid then
       Select_Layout'Result.Base = GuC_Base_For (HuC) and then
       Select_Layout'Result.Size =
         WOPCM_Capacity - HW_Context_Reserved - Select_Layout'Result.Base and then
       Intel_GPU_Firmware.Fits_ADLN_WOPCM
         (WOPCM_Capacity, Select_Layout'Result.Base, Select_Layout'Result.Size,
          GuC, HuC)
       else Select_Layout'Result = (WOPCM_Layout'(others => <>)));

   -- Value to program into DMA_GUC_WOPCM_OFFSET: base | HuC agent = GuC.
   function WOPCM_Offset_Value (Layout : WOPCM_Layout) return Unsigned_32
   with Pre => Layout.Valid;

   -- Read-back evidence that the locked WOPCM admits this HuC upload.
   function WOPCM_Admits_HuC (Raw : Unsigned_32; HuC : Upload_Bytes)
     return Boolean;

   -- RSA page: 4 KiB aligned, at or above the GuC pin bias, wholly below
   -- GUC_GGTT_TOP. The page must hold exactly the blob's RSA bytes.
   GuC_GGTT_Top : constant Unsigned_64 := 16#FEE0_0000#;
   Page_Bytes : constant Unsigned_64 := 4096;
   function RSA_Page_Admissible (GGTT, Pin_Bias : Unsigned_64) return Boolean is
     (GGTT mod Page_Bytes = 0 and then Pin_Bias <= GGTT and then
      GGTT <= GuC_GGTT_Top - Page_Bytes);

   -- DMA source: the retained GGTT mapping of the whole blob, page aligned,
   -- below 4 GiB with the upload wholly below 4 GiB.
   function Source_Admissible (GGTT : Unsigned_64; Bytes : Upload_Bytes)
     return Boolean is
     (GGTT mod Page_Bytes = 0 and then GGTT < 2 ** 32 and then
      Bytes <= 2 ** 32 - GGTT);
end Intel_GPU_HuC_Firmware;
