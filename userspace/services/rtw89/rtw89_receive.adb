------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages;
with CuBit.Net_Link;
with RTW89_Registers; use RTW89_Registers;
with RTW89_MAC_Registers;
with RTW89_MMIO; use RTW89_MMIO;
with RTW89_Layout; use RTW89_Layout;
with RTW89_RX;
with RTW89_Text; use RTW89_Text;
with IEEE80211_Beacons;
with RTW89_Station;
with RTW89_Transmit;

package body RTW89_Receive is
   use type RTW89_RX.Status;
   use type RTW89_RX.Packet_Type;
   use type RTW89_RX.PPDU_Result;
   use type IEEE80211_Beacons.Status;
   use type WS.Band;
   package NL renames CuBit.Net_Link;
   package MR renames RTW89_MAC_Registers;

   --  A release report entry (pci.h rtw89_pci_rpp_fmt): one word.
   Release_Entry_Bytes : constant := 4;

   --  The snapshot every buffer is copied into before it is decoded.
   Snapshot : RTW89_RX.Buffer := [others => 0];

   --  Host indexes: the next slot to consume, per ring.
   Next_RXQ, Next_RPQ : Natural := 0;

   --  A beacon waiting for its PPDU status (the status follows the frame).
   type Pending_Frame is record
      Present : Boolean := False;
      Desc    : RTW89_RX.Descriptor;
      Heard   : WS.BSS_Entry;
   end record;
   Pending : Pending_Frame;
   Dumped  : Natural := 0;

   procedure Restart is
   begin
      Pending := (others => <>);
   end Restart;

   procedure Rings_Reset is
   begin
      Next_RXQ := 0;
      Next_RPQ := 0;
      Pending := (others => <>);
   end Rings_Reset;

   function Buffer_Address (DMA : RTW89_Bringup.Region; Channel : RX_Channel; Slot : Natural)
     return System.Address is
     (To_Address (Integer_Address (DMA.Virtual + Unsigned_64 (RX_Buffer_Offset (Channel, Slot)))));

   --  Copy the bytes the device says it wrote (bounded by the buffer).
   procedure Copy_Out (DMA : RTW89_Bringup.Region; Slot : Natural; Channel : RX_Channel := RXQ) is
      Source : RTW89_RX.Buffer with Import, Volatile, Address => Buffer_Address (DMA, Channel, Slot);
      Info_Word : Unsigned_32;
      Size : Natural;
   begin
      for I in 0 .. RTW89_RX.Info_Bytes - 1 loop
         Snapshot (I) := Source (I);
      end loop;
      Info_Word := RTW89_RX.U32 (Snapshot, 0);
      Size := Natural'Min (Natural (Info_Word and RTW89_RX.Write_Size_Mask), RTW89_RX.Buffer_Bytes);
      for I in RTW89_RX.Info_Bytes .. Size - 1 loop
         Snapshot (I) := Source (I);
      end loop;
   end Copy_Out;

   procedure Commit (Table : in out Wifi_BSS_Table.Table; Signal : WS.Signal_dBm) is
      E : WS.BSS_Entry := Pending.Heard;
   begin
      E.Signal := Signal;
      Wifi_BSS_Table.Observe (Table, E);
      Pending.Present := False;
   end Commit;

   function Clamp_Signal (V : RTW89_RX.dBm) return WS.Signal_dBm is
     (WS.Signal_dBm (Integer'Min (Integer (V), 0)));

   procedure Dump (Frame : IEEE80211_Beacons.Frame_Bytes) is
      Line : String (1 .. 2 * Frame'Length);
   begin
      for I in Frame'Range loop
         Line (2 * I + 1 .. 2 * I + 2) := Hex2 (Frame (I));
      end loop;
      CuBit.Messages.debugPrint ("rtw89: frame " & Line & ASCII.LF);
   end Dump;

   procedure Take_Frame (D : RTW89_RX.Descriptor; Heard_Band : WS.Band; Heard_On : WS.Channel_Number;
                         Table : in out Wifi_BSS_Table.Table; Counts : in out Statistics) is
      Length : constant Natural := D.Length - RTW89_RX.FCS_Bytes;
      Frame : IEEE80211_Beacons.Frame_Bytes (0 .. Length - 1);
      Info : IEEE80211_Beacons.Decoded;
      Result : IEEE80211_Beacons.Status;
      E : WS.BSS_Entry;
      Channel : Unsigned_8;
   begin
      for I in Frame'Range loop
         Frame (I) := Snapshot (D.Payload_First + I);
      end loop;
      IEEE80211_Beacons.Decode (Frame, Info, Result);
      if Result = IEEE80211_Beacons.Not_Beacon or else Result = IEEE80211_Beacons.Not_Management then
         Counts.Other_Frames := Counts.Other_Frames + 1;
         return;
      elsif Result /= IEEE80211_Beacons.Accepted then
         Counts.Malformed := Counts.Malformed + 1;
         return;
      end if;
      Counts.Beacons := Counts.Beacons + 1;
      if Dumped < Frames_To_Dump then
         Dumped := Dumped + 1;
         Dump (Frame);
      end if;
      E.BSSID := [for I in NL.Octet_Index => Info.BSSID (I)];
      E.Length := Info.Length;
      for I in 1 .. Info.Length loop
         E.SSID (I) := Info.SSID (I);
      end loop;
      --  The BSS's own channel when it names one valid for some band.
      Channel := IEEE80211_Beacons.Channel_Of (Info, Unsigned_8 (Heard_On));
      if Channel in WS.Channel_2_4 then
         E.On_Band := WS.Band_2_4_GHz;
         E.Channel := Channel;
      elsif Channel in WS.Channel_5 then
         E.On_Band := WS.Band_5_GHz;
         E.Channel := Channel;
      else
         E.On_Band := Heard_Band;
         E.Channel := Heard_On;
      end if;
      E.Capability := Info.Capability;
      E.Interval := Info.Interval;
      E.RSN := Info.RSN;
      E.Signal := WS.Signal_dBm'First;
      if Pending.Present then
         Commit (Table, WS.Signal_dBm'First);   --  its status never came
      end if;
      Pending := (Present => True, Desc => D, Heard => E);
   end Take_Frame;

   procedure Take_Status (D : RTW89_RX.Descriptor; Table : in out Wifi_BSS_Table.Table;
                          Counts : in out Statistics) is
      Report : RTW89_RX.PPDU_Report;
      Result : RTW89_RX.PPDU_Result;
   begin
      Counts.PPDU_Reports := Counts.PPDU_Reports + 1;
      if not Pending.Present then
         return;
      end if;
      RTW89_RX.Decode_PPDU (Snapshot, D, Report, Result);
      if Result = RTW89_RX.Taken and then RTW89_RX.Matches (Pending.Desc, D) then
         Counts.Signals := Counts.Signals + 1;
         Commit (Table, Clamp_Signal (RTW89_RX.Signal_Of (Report)));
      else
         Commit (Table, WS.Signal_dBm'First);
      end if;
   end Take_Status;

   function Hardware_Index (Channel : RX_Channel) return Natural is
     (Natural (Shift_Right (Read32 (RX_Rings (Channel).Index) and RING_HW_INDEX_MASK,
                            RING_HW_INDEX_SHIFT)) mod RX_Ring_Entries);

   procedure Poll (DMA : RTW89_Bringup.Region; Heard_Band : WS.Band;
                   Heard_On : WS.Channel_Number; Table : in out Wifi_BSS_Table.Table;
                   Counts : in out Statistics) is
      Filled : Natural;
      D : RTW89_RX.Descriptor;
      Size : RTW89_RX.Payload_Length;
      Result : RTW89_RX.Status;
   begin
      --  Release reports: each 32-bit entry after the descriptor frees the
      --  transmit slot whose WD page it names (pci.c
      --  rtw89_pci_release_tx_skbs).
      Write32 (MR.PCIE_HISR00, MR.HISR00_RPQ_ACK);
      Filled := Hardware_Index (RPQ);
      if Filled /= Next_RPQ then
         while Next_RPQ /= Filled loop
            Copy_Out (DMA, Next_RPQ, RPQ);
            Counts.Release_Reports := Counts.Release_Reports + 1;
            RTW89_RX.Decode (Snapshot, D, Size, Result);
            if Result = RTW89_RX.Decoded and then D.Kind = RTW89_RX.TX_Release_Host then
               declare
                  Offset : Natural := D.Payload_First;
               begin
                  while Offset + Release_Entry_Bytes <= Size loop
                     RTW89_Transmit.Released (RTW89_RX.U32 (Snapshot, Offset));
                     Offset := Offset + Release_Entry_Bytes;
                  end loop;
               end;
            end if;
            Next_RPQ := (Next_RPQ + 1) mod RX_Ring_Entries;
         end loop;
         Write16 (RX_Rings (RPQ).Index, Unsigned_16 (Next_RPQ));
      end if;

      Write32 (MR.PCIE_HISR00, MR.HISR00_RXQ_ACK);
      Filled := Hardware_Index (RXQ);
      if Filled = Next_RXQ then
         return;
      end if;
      while Next_RXQ /= Filled loop
         Copy_Out (DMA, Next_RXQ);
         Counts.Buffers := Counts.Buffers + 1;
         RTW89_RX.Decode (Snapshot, D, Size, Result);
         if Result /= RTW89_RX.Decoded then
            Counts.Refused := Counts.Refused + 1;
         else
            case D.Kind is
               when RTW89_RX.WIFI =>
                  Counts.Frames := Counts.Frames + 1;
                  if D.CRC_Error then
                     Counts.CRC_Errors := Counts.CRC_Errors + 1;
                  elsif RTW89_Station.Joined then
                     --  Joined: every frame to the station (it filters by BSS).
                     if D.Length > RTW89_RX.FCS_Bytes then
                        declare
                           Length : constant Natural := D.Length - RTW89_RX.FCS_Bytes;
                           Frame : RTW89_Station.Frame_Bytes (0 .. Length - 1);
                        begin
                           for I in Frame'Range loop
                              Frame (I) := Snapshot (D.Payload_First + I);
                           end loop;
                           RTW89_Station.From_Air (Frame, D.HW_Decrypted, D.ICV_Error or else D.SW_Decrypt);
                        end;
                     end if;
                  elsif D.ICV_Error then
                     Counts.CRC_Errors := Counts.CRC_Errors + 1;
                  elsif D.Length >= IEEE80211_Beacons.Minimum_Frame + RTW89_RX.FCS_Bytes then
                     Take_Frame (D, Heard_Band, Heard_On, Table, Counts);
                  else
                     Counts.Other_Frames := Counts.Other_Frames + 1;
                  end if;
               when RTW89_RX.PPDU_Status =>
                  Take_Status (D, Table, Counts);
               when RTW89_RX.C2H =>
                  Counts.C2H := Counts.C2H + 1;
               when others =>
                  Counts.Other_Packets := Counts.Other_Packets + 1;
            end case;
         end if;
         Next_RXQ := (Next_RXQ + 1) mod RX_Ring_Entries;
      end loop;
      Write16 (RX_Rings (RXQ).Index, Unsigned_16 (Next_RXQ));
   end Poll;

end RTW89_Receive;
