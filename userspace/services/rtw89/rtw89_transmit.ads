------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Frames to the air (NET-WIFI-001 W3/W4): management frames on the
--  management channel (B0MG), data frames on best-effort data (ACH0),
--  each a TX BD pointing at a WD page (RTW89_TX.Fill) whose address info
--  points at the frame in the same slot (RTW89_Layout).
--
--  @description
--  rtw89 pci.c rtw89_pci_txbd_submit / rtw89_pci_txwd_submit: the BD's
--  length is the WD page's, the host index moves on by one. A slot is
--  busy from its submission until the release report (RPQ) names its WD
--  page sequence (RTW89_TX.Report_Sequence); Released takes those.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with RTW89_MMIO;
with RTW89_Bringup;
with RTW89_Layout;
with RTW89_TX;

package RTW89_Transmit is

   subtype Data_Channel is RTW89_Layout.Data_Channel;

   type Frame_Bytes is array (Natural range <>) of Unsigned_8;

   --  The rings were reprogrammed: every slot is free, both host indices 0.
   procedure Reset;

   function Has_Room (On : Data_Channel) return Boolean;

   --  Queue Frame. The WD page is filled from R (its Size, Frame_Bus and
   --  Page_Sequence are set here). Sent is False when no slot was free or
   --  the frame does not fit a slot.
   procedure Send (DMA : RTW89_Bringup.Region; On : Data_Channel; Frame : Frame_Bytes;
                   R : RTW89_TX.Request; Sent : out Boolean)
     with Pre => RTW89_MMIO.Attached and then Frame'First = 0;

   --  A release report entry (one 32-bit word from the RPQ).
   procedure Released (Word : Unsigned_32);

   --  Release report TX status (pci.h RTW89_PCI_RPP_TX_STATUS [15:13]; core.h
   --  RTW89_TX_DONE 0, RTW89_TX_RETRY_LIMIT 1, RTW89_TX_LIFE_TIME 2,
   --  RTW89_TX_MACID_DROP 3).
   subtype TX_Status is Natural range 0 .. 7;
   type Status_Counts is array (TX_Status) of Natural;
   type Statistics is record
      Sent, No_Room, Reports, Unknown_Reports : Natural := 0;
      By_Status : Status_Counts := [others => 0];
   end record;
   function Counts return Statistics;

end RTW89_Transmit;
