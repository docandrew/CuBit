--  Descriptor encodings (RTW89_Descriptors) against the values rtw89
--  produces for the firmware-download path (docs/wifi.md).
pragma Ada_2022;
with Interfaces; use Interfaces;
with RTW89_Descriptors; use RTW89_Descriptors;
with Test_Support; use Test_Support;

procedure Descriptor_Tests is
   BD : BD_Words;
begin
   --  The FWHDR_DL packet: 24-byte WD + 8-byte H2C header + 80 bytes.
   BD := TX_BD (112, 16#1_2345_6000#);
   Check (BD.Low = (112 or Shift_Left (Unsigned_32 (16#4040#), 16)),
          "tx bd: length, last segment, DMA_HI bits 39..32 in [13:6]");
   Check (BD.High = 16#2345_6000#, "tx bd: low address");
   BD := TX_BD (2044, 16#8000_0000#);
   Check (BD.Low = (2044 or Shift_Left (Unsigned_32 (Option_Last_Segment), 16))
          and then BD.High = 16#8000_0000#, "tx bd: below 4 GiB has no DMA_HI");
   BD := RX_BD (4096, 16#F_0000_1000#);
   Check (BD.Low = (4096 or Shift_Left (Unsigned_32 (16#03C0#), 16))
          and then BD.High = 16#1000#, "rx bd: size and DMA_HI, no LS bit");

   Check (WD_Dword0 (False) = 16#000C_0000#, "wd0: H2C packet on channel 12");
   Check (WD_Dword0 (True) = 16#001C_0000#, "wd0: firmware chunk sets FW_DL");
   Check (WD_Dword2 (2020) = 2020, "wd2: payload size");

   Check (H2C_Header0 (H2C_Category_MAC, H2C_Class_FWDL, H2C_Function_Header_Download, 0)
          = 16#0000_000D#, "h2c hdr0: MAC / FWDL / FWHDR_DL, sequence 0");
   Check (H2C_Header0 (1, 9, 16#14#, 3) = 16#0300_1425#, "h2c hdr0: fields and sequence");
   Check (H2C_Header1 (80) = 88, "h2c hdr1: total length includes the header");

   Check (Hardware_Index (16#0023_0005#) = 16#23#, "index: hardware part [27:16]");
   Check (Outstanding (5, 250, 256) = 11, "outstanding: wraps around the ring");
   Check (Outstanding (7, 7, 256) = 0, "outstanding: empty");
end Descriptor_Tests;
