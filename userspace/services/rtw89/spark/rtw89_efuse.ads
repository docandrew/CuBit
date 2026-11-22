------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  RTL8852B efuse: physical-to-logical map decoding and the fields the
--  bring-up driver reads (docs/wifi.md, "Efuse").
--
--  @description
--  The physical efuse is a run of records after a 4-byte security-control
--  prefix. Each record is a 2-byte header naming a logical block (8 bytes)
--  and a 4-bit word-enable mask (a CLEAR bit means the word is present),
--  followed by the present 16-bit words. 0xFF in either header byte ends
--  the run. Decoding fills a 2048-byte logical map that starts all 0xFF.
--  Layout facts from Linux rtw89 efuse.c (rtw89_dump_logical_efuse_map) and
--  rtw8852b_common.h (struct rtw8852bx_efuse), GPL-2.0 OR BSD-3-Clause.
--
--  The physical bytes come from the device and are untrusted: decoding is
--  total, proved free of run-time errors, and never reads or writes outside
--  either map.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package RTW89_Efuse with Pure, SPARK_Mode is

   --  rtw8852b_chip_info: physical_efuse_size, logical_efuse_size,
   --  sec_ctrl_efuse_size.
   Physical_Size         : constant := 1216;
   Logical_Size          : constant := 2048;
   Security_Control_Size : constant := 4;

   subtype Physical_Index is Natural range 0 .. Physical_Size - 1;
   subtype Logical_Index is Natural range 0 .. Logical_Size - 1;
   type Physical_Map is array (Physical_Index) of Unsigned_8;
   type Logical_Map is array (Logical_Index) of Unsigned_8;

   Unprogrammed : constant Unsigned_8 := 16#FF#;

   --  One record header: block index from the low nibble of the first byte
   --  (high bits of the index) and the high nibble of the second byte.
   Block_Bytes      : constant := 8;
   Word_Bytes       : constant := 2;
   Words_Per_Block  : constant := 4;
   Header_Bytes     : constant := 2;
   Nibble_Bits      : constant := 4;
   Low_Nibble       : constant Unsigned_8 := 16#0F#;
   High_Nibble      : constant Unsigned_8 := 16#F0#;

   type Decode_Status is
     (Decoded,              --  the run ended at a 0xFF header or at the end
      Word_Past_Physical,   --  a record's words run past the physical area
      Word_Past_Logical);   --  a record names words past the logical map

   function Name (Status : Decode_Status) return String is
     (case Status is
        when Decoded            => "decoded",
        when Word_Past_Physical => "word-past-physical",
        when Word_Past_Logical  => "word-past-logical");

   --  Records actually consumed (for the log line).
   subtype Record_Count is Natural range 0 .. Physical_Size / Header_Bytes;

   procedure Decode
     (Physical : Physical_Map;
      Logical  : out Logical_Map;
      Records  : out Record_Count;
      Status   : out Decode_Status);

   ---------------------------------------------------------------------------
   --  Logical-map fields of an RTL8852BE (PCIe), struct rtw8852bx_efuse.
   ---------------------------------------------------------------------------
   Crystal_Trim_Offset : constant Logical_Index := 16#2B9#;
   RFE_Type_Offset     : constant Logical_Index := 16#2CA#;
   Country_Offset      : constant Logical_Index := 16#2CB#;  --  2 ASCII bytes
   PCIe_MAC_Offset     : constant Logical_Index := 16#400#;  --  6 bytes

   MAC_Bytes : constant := 6;
   subtype MAC_Index is Natural range 0 .. MAC_Bytes - 1;
   type MAC_Address is array (MAC_Index) of Unsigned_8;

   function MAC (Logical : Logical_Map) return MAC_Address is
     ([for I in MAC_Index => Logical (PCIe_MAC_Offset + I)]);

   --  IEEE 802 individual/group bit of the first octet.
   Group_Bit : constant Unsigned_8 := 16#01#;

   --  A usable station address: unicast, not all-zero, not unprogrammed.
   function Usable (Address : MAC_Address) return Boolean is
     ((Address (0) and Group_Bit) = 0
      and then (for some I in MAC_Index => Address (I) /= 0)
      and then (for some I in MAC_Index => Address (I) /= Unprogrammed));

end RTW89_Efuse;
