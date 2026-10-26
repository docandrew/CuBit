------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  RTL8852BE steps from MAC power-on to "firmware ready": DMA width,
--  DMAC pre-init in firmware-download quota mode, PCIe pre-init and ring
--  setup, and the firmware download over channel 12 (docs/wifi.md,
--  "Firmware download").
--
--  @description
--  Sequences from rtw89 (mac.c rtw89_mac_partial_init and
--  rtw89_mac_dmac_pre_init, pci.c rtw89_pci_ops_mac_pre_init_ax, fw.c
--  rtw89_fw_download), resolved for the 8852B over PCIe. Every wait is a
--  bounded register poll; no interrupt is used. Nothing enables RF.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with RTW89_Firmware;
with RTW89_Outcomes; use RTW89_Outcomes;
with RTW89_MMIO;
with RTW89_Registers;

package RTW89_Bringup is

   --  The DMA region as the device and as the driver see it.
   type Region is record
      Physical : Unsigned_64 := 0;
      Virtual  : Unsigned_64 := 0;
      Bytes    : Unsigned_64 := 0;
   end record;

   --  Mask every interrupt source in the device (before devmgr unmasks
   --  the MSI: bring-up polls).
   procedure Mask_Interrupts with Pre => RTW89_MMIO.Attached;

   --  Make the region reachable: below 4 GiB as is; below 64 GiB with the
   --  device's 36-bit DMA enable (configuration byte 0x719 bit 5, via DBI).
   procedure Configure_DMA_Width (DMA : Region; Wide : out Boolean;
                                  Result : out Outcome)
     with Pre => RTW89_MMIO.Attached;

   procedure DMAC_Pre_Init (Result : out Outcome) with Pre => RTW89_MMIO.Attached;

   procedure PCIe_Pre_Init (DMA : Region; Result : out Outcome)
     with Pre => RTW89_MMIO.Attached;

   --  Last firmware-download status read (for the log line).
   function Download_Status return RTW89_Registers.Download_Status
     with Pre => RTW89_MMIO.Attached;

   procedure Download
     (DMA     : Region;
      File    : RTW89_Firmware.Byte_Array;
      Plan    : RTW89_Firmware.Plan;
      Packets : out Natural;
      Result  : out Outcome)
     with Pre => RTW89_MMIO.Attached and then File'First = 0
                 and then RTW89_Firmware.Valid_Plan (Plan, File'Length);

   --  An H2C payload (the 8-byte header is added).
   Maximum_H2C_Payload : constant := 128;
   type H2C_Bytes is array (Positive range <>) of Unsigned_8
   with Predicate => H2C_Bytes'Length <= Maximum_H2C_Payload;

   --  Queue one H2C command on channel 12 (after Download). OK is False
   --  when the ring stayed full for its whole budget.
   procedure Send_H2C
     (DMA : Region; Category, Class, Func : Unsigned_8; Payload : H2C_Bytes;
      Done_Ack : Boolean; OK : out Boolean)
   with Pre => RTW89_MMIO.Attached;

   --  Whether the device consumed every queued command (bounded wait).
   function H2C_Drained return Boolean with Pre => RTW89_MMIO.Attached;

end RTW89_Bringup;
