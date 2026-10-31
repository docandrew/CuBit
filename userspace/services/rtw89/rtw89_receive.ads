------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The RTL8852BE receive ring, polled (NET-WIFI-001 W1): every filled RXQ
--  buffer is copied out of DMA memory once, decoded by the proved
--  RTW89_RX and IEEE80211_Beacons, and each beacon or probe response is
--  folded into the scan's BSS list with the signal of its PPDU status.
--  RPQ (release reports) is drained too: each entry frees a transmit slot
--  (RTW89_Transmit). While the station is joined (RTW89_Station), every
--  802.11 frame goes to it instead of the scan.
--
--  @description
--  Ring protocol from rtw89 pci.c (rtw89_pci_rxbd_recalc,
--  rtw89_pci_rxbd_deliver_skbs): the hardware index [27:16] of the ring's
--  index register says how far the device filled; the driver consumes up
--  to it and returns the buffers in one 16-bit write of the host index.
--  No interrupt is used.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Wifi_Scan;
with RTW89_MMIO;
with RTW89_Bringup;
with Wifi_BSS_Table;

package RTW89_Receive is
   package WS renames CuBit.Wifi_Scan;

   type Statistics is record
      Buffers         : Natural := 0;   --  RXQ buffers consumed
      Release_Reports : Natural := 0;   --  RPQ buffers consumed
      Refused         : Natural := 0;   --  buffers the descriptor decoder refused
      Frames          : Natural := 0;   --  802.11 frames (RPKT_TYPE WIFI)
      CRC_Errors      : Natural := 0;
      Beacons         : Natural := 0;   --  beacons and probe responses accepted
      Malformed       : Natural := 0;   --  beacon/probe responses the decoder refused
      Other_Frames    : Natural := 0;   --  other management, control, data
      PPDU_Reports    : Natural := 0;
      Signals         : Natural := 0;   --  beacons given a PPDU status's signal
      C2H             : Natural := 0;
      Other_Packets   : Natural := 0;
   end record;

   --  Forget any half-matched frame (before a channel switch).
   procedure Restart;

   --  The rings were reprogrammed (PCIe pre-init): both start at slot 0.
   procedure Rings_Reset;

   --  Consume everything the device has filled. Heard_On is the channel
   --  the radio is tuned to (used when a beacon names none).
   procedure Poll (DMA : RTW89_Bringup.Region; Heard_Band : WS.Band;
                   Heard_On : WS.Channel_Number; Table : in out Wifi_BSS_Table.Table;
                   Counts : in out Statistics)
     with Pre => RTW89_MMIO.Attached and then Wifi_BSS_Table.Valid (Table);

   --  The first beacons accepted, as hex on serial only: fixtures for the
   --  hosted decoder tests (recorded on hardware).
   Frames_To_Dump : constant := 6;

end RTW89_Receive;
