------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  How rtw89.drv carves its one DMA region (the manifest's "rings"
--  request, physically contiguous, from devmgr) for the bring-up stages.
--
--  @description
--  Every buffer the device reads or writes is aligned so it never crosses
--  a 4 KiB boundary (pci.h defines a TXBD 4K-boundary error indication).
--  TX rings have rtw89's 256 entries; RX rings are smaller than rtw89's
--  (64 x 4 KiB buffers instead of 256 x 11498 bytes) because bring-up only
--  expects firmware messages, and every RX descriptor must point at real
--  memory before RX DMA is enabled.
------------------------------------------------------------------------------
pragma Ada_2022;
with RTW89_Registers; use RTW89_Registers;
with RTW89_Descriptors;

package RTW89_Layout with Pure is

   Region_Bytes : constant := 1024 * 1024;   --  manifest: (dma "rings" 1048576)
   Page_Bytes   : constant := 4096;

   TX_Ring_Entries : constant := 256;        --  RTW89_PCI_TXBD_NUM_MAX
   TX_Ring_Bytes   : constant := TX_Ring_Entries * RTW89_Descriptors.BD_Bytes;
   TX_Rings_Offset : constant := 16#0_0000#;
   function TX_Ring_Offset (Channel : TX_Channel) return Natural is
     (TX_Rings_Offset + TX_Channel'Pos (Channel) * TX_Ring_Bytes);

   RX_Ring_Entries : constant := 64;
   RX_Ring_Bytes   : constant := RX_Ring_Entries * RTW89_Descriptors.BD_Bytes;
   RX_Rings_Offset : constant := 16#0_4000#;
   function RX_Ring_Offset (Channel : RX_Channel) return Natural is
     (RX_Rings_Offset + RX_Channel'Pos (Channel) * Page_Bytes);

   --  Channel-12 packet buffers: WD body + H2C header + up to 2020 bytes of
   --  section payload fit in 2 KiB.
   Command_Slot_Bytes : constant := 2048;
   Command_Slots      : constant := 16;
   Command_Offset     : constant := 16#0_8000#;
   --  rtw89 keeps the 8 most recently completed command buffers untouched
   --  (RTW89_PCI_MULTITAG); with 16 slots, a slot is reused only once at
   --  most Command_Slots - Kept_Completed packets are in flight.
   Kept_Completed     : constant := 8;
   Maximum_In_Flight  : constant := Command_Slots - Kept_Completed - 1;

   RX_Buffer_Bytes  : constant := Page_Bytes;
   RX_Buffers_Offset : constant := 16#1_0000#;
   function RX_Buffer_Offset (Channel : RX_Channel; Slot : Natural) return Natural is
     (RX_Buffers_Offset
      + (RX_Channel'Pos (Channel) * RX_Ring_Entries + Slot) * RX_Buffer_Bytes)
   with Pre => Slot < RX_Ring_Entries;

   Used_Bytes : constant := RX_Buffers_Offset
     + (RX_Channel'Pos (RX_Channel'Last) + 1) * RX_Ring_Entries * RX_Buffer_Bytes;
   pragma Assert (Used_Bytes <= Region_Bytes);
   pragma Assert (TX_Rings_Offset + 7 * TX_Ring_Bytes <= RX_Rings_Offset);
   pragma Assert (RX_Rings_Offset + 2 * Page_Bytes <= Command_Offset);
   pragma Assert (Command_Offset + Command_Slots * Command_Slot_Bytes <= RX_Buffers_Offset);

end RTW89_Layout;
