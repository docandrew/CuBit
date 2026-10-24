------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  RTL8852BE PCIe descriptor encodings used for firmware download:
--  TX/RX buffer descriptors, the WD body that heads every channel-12
--  packet, and the H2C command header (docs/wifi.md, "DMA rings").
--
--  @description
--  Layouts from rtw89 pci.h (rtw89_pci_tx_bd_32, rtw89_pci_rx_bd_32),
--  txrx.h (rtw89_txwd_body dword 0 and 2) and fw.h/fw.c (H2C header,
--  rtw89_h2c_pkt_set_hdr_fwdl), GPL-2.0 OR BSD-3-Clause. All little-endian.
--  Pure functions: the driver writes their results into DMA memory.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package RTW89_Descriptors with Pure, SPARK_Mode is

   --  The device reaches 40 address bits through an 8-bit "DMA_HI" field
   --  (bits 39..32); the driver further limits itself to what the link
   --  allows (32 bits, or 36 with the DAC enable).
   Address_Bits : constant := 40;
   subtype Bus_Address is Unsigned_64 range 0 .. 2 ** Address_Bits - 1;

   ---------------------------------------------------------------------------
   --  Buffer descriptors: 8 bytes, as two little-endian words:
   --    word 0: length [15:0], option [31:16]; word 1: address [31:0].
   ---------------------------------------------------------------------------
   type BD_Words is record
      Low  : Unsigned_32;   --  length | option << 16
      High : Unsigned_32;   --  low 32 address bits
   end record;
   BD_Bytes : constant := 8;

   Option_Last_Segment : constant Unsigned_16 := 2 ** 14;  --  RTW89_PCI_TXBD_OPT_LS
   Option_DMA_Hi_Shift : constant := 6;                    --  [13:6]
   Option_DMA_Hi_Mask  : constant Unsigned_16 := 16#3FC0#;

   function DMA_Hi (Address : Bus_Address) return Unsigned_16 is
     (Shift_Left (Unsigned_16 (Shift_Right (Address, 32) and 16#FF#), Option_DMA_Hi_Shift)
      and Option_DMA_Hi_Mask);

   subtype Buffer_Length is Unsigned_16 range 1 .. Unsigned_16'Last;

   function TX_BD (Length : Buffer_Length; Address : Bus_Address) return BD_Words is
     ((Low  => Unsigned_32 (Length)
               or Shift_Left (Unsigned_32 (Option_Last_Segment or DMA_Hi (Address)), 16),
       High => Unsigned_32 (Address and 16#FFFF_FFFF#)));

   function RX_BD (Size : Buffer_Length; Address : Bus_Address) return BD_Words is
     ((Low  => Unsigned_32 (Size) or Shift_Left (Unsigned_32 (DMA_Hi (Address)), 16),
       High => Unsigned_32 (Address and 16#FFFF_FFFF#)));

   ---------------------------------------------------------------------------
   --  WD body: 6 dwords (24 bytes) ahead of every channel-12 packet.
   ---------------------------------------------------------------------------
   WD_Body_Bytes : constant := 24;
   FWCMD_Channel : constant := 12;           --  RTW89_DMA_H2C
   WD0_Channel_Shift : constant := 16;       --  CHANNEL_DMA [19:16]
   WD0_FW_Download   : constant Unsigned_32 := 2 ** 20;
   WD2_Size_Mask     : constant Unsigned_32 := 16#3FFF#;   --  TXPKT_SIZE [13:0]

   subtype Packet_Size is Natural range 1 .. 16#3FFF#;

   --  Dword 0: header packets and other H2C use 0; section chunks set FW_DL.
   function WD_Dword0 (Firmware_Chunk : Boolean) return Unsigned_32 is
     (Shift_Left (Unsigned_32'(FWCMD_Channel), WD0_Channel_Shift)
      or (if Firmware_Chunk then WD0_FW_Download else 0));

   --  Dword 2: the payload size after the WD body.
   function WD_Dword2 (Size : Packet_Size) return Unsigned_32 is
     (Unsigned_32 (Size) and WD2_Size_Mask);

   ---------------------------------------------------------------------------
   --  H2C header: 2 dwords ahead of an H2C payload (not on section chunks).
   --    hdr0: category [1:0], class [7:2], function [15:8], delivery type
   --          [19:16], sequence [31:24]
   --    hdr1: total length incl. this header [13:0], receive-ack [14],
   --          done-ack [15]
   ---------------------------------------------------------------------------
   H2C_Header_Bytes : constant := 8;
   H2C_Category_MAC : constant := 1;
   H2C_Class_FWDL   : constant := 3;     --  H2C_CL_MAC_FWDL
   H2C_Function_Header_Download : constant := 0;  --  H2C_FUNC_MAC_FWHDR_DL
   H2C_Delivery_H2C : constant := 0;     --  FWCMD_TYPE_H2C
   H2C_Class_Shift    : constant := 2;
   H2C_Function_Shift : constant := 8;
   H2C_Delivery_Shift : constant := 16;
   H2C_Sequence_Shift : constant := 24;
   H2C_Length_Mask    : constant Unsigned_32 := 16#3FFF#;

   subtype H2C_Payload is Natural range 0 .. 16#3FFF# - H2C_Header_Bytes;

   function H2C_Header0
     (Category, Class, Func : Unsigned_8; Sequence : Unsigned_8) return Unsigned_32 is
     (Unsigned_32 (Category and 16#03#)
      or Shift_Left (Unsigned_32 (Class and 16#3F#), H2C_Class_Shift)
      or Shift_Left (Unsigned_32 (Func), H2C_Function_Shift)
      or Shift_Left (Unsigned_32'(H2C_Delivery_H2C), H2C_Delivery_Shift)
      or Shift_Left (Unsigned_32 (Sequence), H2C_Sequence_Shift));

   function H2C_Header1 (Payload : H2C_Payload) return Unsigned_32 is
     (Unsigned_32 (Payload + H2C_Header_Bytes) and H2C_Length_Mask);

   ---------------------------------------------------------------------------
   --  Ring index registers: host index [11:0], hardware index [27:16].
   ---------------------------------------------------------------------------
   Maximum_Ring_Entries : constant := 4096;
   subtype Ring_Size is Positive range 2 .. Maximum_Ring_Entries;

   function Hardware_Index (Register_Value : Unsigned_32) return Natural is
     (Natural (Shift_Right (Register_Value, 16) and 16#0FFF#));

   --  Packets the hardware still owns: from Hardware_Done up to Host_Next.
   function Outstanding (Host_Next, Hardware_Done : Natural; Size : Ring_Size)
     return Natural is
     ((Host_Next - Hardware_Done + Size) mod Size)
   with Pre => Host_Next < Size and then Hardware_Done < Size;

end RTW89_Descriptors;
