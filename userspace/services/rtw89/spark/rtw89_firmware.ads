------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Realtek rtw89 firmware container: image selection and the download plan
--  (docs/wifi.md, "Firmware download").
--
--  @description
--  Parses only the documented container headers, as rtw89 fw.c does
--  (rtw89_mfw_recognize, rtw89_fw_hdr_parser). It never inspects firmware
--  code: the licence forbids reverse engineering, and the driver sends
--  sections unmodified.
--
--  Layout (little-endian):
--    MFW header, 16 bytes: signature 0xFF, entry count, rsvd, version[4], rsvd.
--    Entries, 16 bytes each: cut, type, mp, rsvd, offset u32, size u32, rsvd.
--    Image header v0: 8 dwords, then 16-byte section headers. w3[31:24] is
--    the header version (0), w3[23:16] the total header length with the
--    dynamic header, w6[15:8] the section count, w7 bit 16 "dynamic header".
--    Section: w0 download address (low 29 bits), w1 size [23:0], type
--    [27:24], checksum [28]; w2 the security-section signature count.
--    Section payloads follow the header back to back and end exactly at the
--    image's end.
--
--  The file is untrusted input: every function is total and proved free of
--  run-time errors; offsets are checked in 64-bit arithmetic before use.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package RTW89_Firmware with Pure, SPARK_Mode is

   Maximum_File_Bytes : constant := 4 * 1024 * 1024;
   subtype File_Index is Natural range 0 .. Maximum_File_Bytes - 1;
   subtype File_Length is Natural range 0 .. Maximum_File_Bytes;
   type Byte_Array is array (File_Index range <>) of Unsigned_8;

   --  rtw89 enum rtw89_fw_type values the driver asks for.
   type Image_Type is (Normal, Normal_CE);
   Normal_Code    : constant Unsigned_8 := 1;
   Normal_CE_Code : constant Unsigned_8 := 5;

   MFW_Signature    : constant Unsigned_8 := 16#FF#;
   MFW_Header_Bytes : constant := 16;
   MFW_Entry_Bytes  : constant := 16;

   --  An image inside the file: [Offset, Offset + Size).
   type Image is record
      Found  : Boolean := False;
      Offset : File_Index := 0;
      Size   : File_Length := 0;
   end record;

   --  The image rtw89 would pick for chip cut Cut: entries of Kind with
   --  cut <= Cut and mp = 0, the largest cut winning; a legacy (non-MFW)
   --  file is one Normal image.
   function Select_Image (File : Byte_Array; Kind : Image_Type; Cut : Unsigned_8)
     return Image
   with Pre  => File'First = 0,
        Post => (if Select_Image'Result.Found then
                   Select_Image'Result.Size > 0
                   and then Select_Image'Result.Offset + Select_Image'Result.Size
                            <= File'Length);

   ---------------------------------------------------------------------------
   --  Image header and download plan
   ---------------------------------------------------------------------------
   Header_Fixed_Bytes  : constant := 32;
   Section_Header_Bytes : constant := 16;
   Maximum_Sections    : constant := 10;   --  FWDL_SECTION_MAX_NUM
   Checksum_Bytes      : constant := 8;    --  FWDL_SECTION_CHKSUM_LEN
   Signature_Bytes     : constant := 512;  --  FWDL_SECURITY_SIGLEN
   Security_Section    : constant Unsigned_8 := 9;
   Formatted_MSSC      : constant Unsigned_32 := 16#FF#;
   Download_Address_Mask : constant Unsigned_32 := 16#1FFF_FFFF#;
   Section_Size_Mask   : constant Unsigned_32 := 16#00FF_FFFF#;
   --  FWDL_SECTION_PER_PKT_LEN: bytes of section payload per FWDL packet.
   Part_Size           : constant := 2020;
   --  Byte offset of w7[15:0] (PART_SIZE) in the header the driver sends.
   Part_Size_Offset    : constant := 28;

   subtype Section_Count is Natural range 0 .. Maximum_Sections;
   subtype Section_Index is Positive range 1 .. Maximum_Sections;

   type Section is record
      Data      : File_Index := 0;    --  absolute file offset of the payload
      Length    : File_Length := 0;   --  bytes to download (size, + checksum)
      Address   : Unsigned_32 := 0;   --  WCPU address (informational)
      Kind      : Unsigned_8 := 0;
   end record;
   type Section_Array is array (Section_Index) of Section;

   type Plan_Status is
     (Planned,
      Image_Too_Short,        --  shorter than the fixed header or its sections
      Unsupported_Version,    --  header version other than 0
      Bad_Section_Count,
      Section_Overruns_Image,
      Empty_Section,
      Formatted_Signatures,   --  MSS key pool: secure-boot parts only
      Size_Mismatch);         --  sections do not end exactly at the image end

   function Name (Status : Plan_Status) return String is
     (case Status is
        when Planned                => "planned",
        when Image_Too_Short        => "image-too-short",
        when Unsupported_Version    => "unsupported-header-version",
        when Bad_Section_Count      => "bad-section-count",
        when Section_Overruns_Image => "section-overruns-image",
        when Empty_Section          => "empty-section",
        when Formatted_Signatures   => "formatted-signatures",
        when Size_Mismatch          => "size-mismatch");

   type Plan is record
      Status       : Plan_Status := Image_Too_Short;
      Header       : File_Index := 0;     --  absolute offset of the header
      Header_Bytes : File_Length := 0;    --  bytes sent in the FWHDR_DL H2C
      Sections     : Section_Count := 0;
      Items        : Section_Array := [others => (others => <>)];
   end record;

   function Valid_Plan (P : Plan; File_Bytes : File_Length) return Boolean is
     (P.Status = Planned
      and then P.Sections >= 1
      and then P.Header_Bytes >= Header_Fixed_Bytes
      and then P.Header + P.Header_Bytes <= File_Bytes
      and then (for all I in 1 .. P.Sections =>
                  P.Items (I).Data + P.Items (I).Length <= File_Bytes));

   function Plan_Image (File : Byte_Array; Where : Image) return Plan
   with Pre  => File'First = 0 and then Where.Found
                and then Where.Offset + Where.Size <= File'Length,
        Post => (if Plan_Image'Result.Status = Planned
                 then Valid_Plan (Plan_Image'Result, File'Length));

   --  Number of FWDL packets a section needs.
   function Packets (Length : File_Length) return Natural is
     ((Length + Part_Size - 1) / Part_Size);

end RTW89_Firmware;
