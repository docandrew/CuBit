------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The RTL8852BE's normal-mode MAC init after the firmware runs, the
--  station port for a receiver that is not associated, the passive-scan
--  receive filter and PPDU status (NET-WIFI-001 W1, docs/wifi.md
--  "Scanning").
--
--  @description
--  Sequences from rtw89 mac.c (rtw89_mac_init: enable_bb_rf, sys_init_ax,
--  trx_init_ax, rtw89_mac_port_update, rtw89_mac_vif_init,
--  rtw89_mac_cfg_ppdu_status_ax, rtw89_mac_set_rx_fltr), pci.c
--  (rtw89_pci_ops_mac_post_init_ax) and fw.c (the H2C commands), resolved
--  for the 8852B over PCIe in SCC quota mode. Nothing here transmits over
--  the air: the H2Cs go to the device's own CPU.
------------------------------------------------------------------------------
pragma Ada_2022;
with RTW89_Outcomes; use RTW89_Outcomes;
with RTW89_MMIO;
with RTW89_Bringup;
with RTW89_Efuse;

package RTW89_MAC_Init is

   --  enable_bb_rf, sys_init, trx_init (DMAC: DLE, HFC, scheduler, MPDU,
   --  security; CMAC 0; error masks; release reports) and the PCIe
   --  post-init, then the offload-configuration H2C.
   procedure Initialize (DMA : RTW89_Bringup.Region; Result : out Outcome)
     with Pre => RTW89_MMIO.Attached;

   --  PPDU status reports to the host (per-frame signal), the RTS
   --  threshold, firmware logging configured off (rtw89_core_start's tail).
   procedure Start (DMA : RTW89_Bringup.Region; Result : out Outcome)
     with Pre => RTW89_MMIO.Attached;

   --  A station interface on port 0 that is not associated (macid 0):
   --  port registers, then role, join (disconnected), address CAM with
   --  our address, default CMAC table and macid unpause H2Cs.
   procedure Add_Station (DMA : RTW89_Bringup.Region; Address : RTW89_Efuse.MAC_Address;
                          Result : out Outcome)
     with Pre => RTW89_MMIO.Attached;

   --  The passive-scan receive filter: beacons and probe responses of
   --  every BSS reach the host (A1 match, broadcast and beacon checks
   --  off), the MPDU length limit kept.
   procedure Scan_Filter with Pre => RTW89_MMIO.Attached;

   --  The receive filter rtw89 restores after a scan.
   procedure Default_Filter with Pre => RTW89_MMIO.Attached;

end RTW89_MAC_Init;
