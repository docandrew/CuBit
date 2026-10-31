------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  rtw89.drv's station (NET-WIFI-001 W3/W4): carries out wifi.svc's
--  commands (join, associated, keys, port, leave) on the RTL8852BE, and
--  moves frames between the air, wifi.svc and netstack.
--
--  @description
--  From the air, while joined: management frames from the BSS (and the
--  first beacon or probe response after Join) go to wifi.svc; data frames
--  (IEEE80211_Data) carrying EAPOL go to wifi.svc, others to netstack as
--  Ethernet frames, only while the 802.1X port is open. A frame the
--  hardware decrypted (CCMP) is accepted only with its packet number
--  above the last one accepted for its key and traffic class (replay
--  protection, 802.11-2020 12.5.3.4.4); a frame that should have been
--  decrypted and was not is dropped.
--
--  To the air: wifi.svc's management frames on the management channel
--  at the basic rate; EAPOL and netstack's frames as data on best effort,
--  encrypted by the hardware (its CCMP header written here, with the
--  next packet number) once the pairwise key is installed.
--
--  Keys: the pairwise key in security CAM entry 0 (address CAM slot 0),
--  a group key with index N in entry and slot N + 1 (rtw89's NORMAL mode:
--  slots 2 .. 4 group). The driver holds only these temporal keys.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Net_Link;
with CuBit.Wifi_Driver;
with CuBit.Wifi_Scan;
with Wifi_Station_Path;
with RTW89_MMIO;
with RTW89_Bringup;
with RTW89_PHY;

package RTW89_Station is
   package D renames CuBit.Wifi_Driver;
   package WS renames CuBit.Wifi_Scan;
   package NL renames CuBit.Net_Link;

   procedure Initialize (Own : NL.Hardware_Address);

   --  Tuned to a BSS: the scan passes stop.
   function Joined return Boolean;
   function Band return WS.Band;
   function Channel return WS.Channel_Number;

   --  A command from wifi.svc (its label and words), carried out.
   procedure Command (DMA : RTW89_Bringup.Region; Card : RTW89_PHY.Board; Label : Unsigned_32;
                      Words : CuBit.Messages.MessageWords; Result : out D.Outcome)
     with Pre => RTW89_MMIO.Attached;

   type Frame_Bytes is array (Natural range <>) of Unsigned_8;

   --  An 802.11 frame from the air (no FCS), and what the receive
   --  descriptor said of its decryption.
   procedure From_Air (Frame : Frame_Bytes; Decrypted, Decrypt_Failed : Boolean)
     with Pre => Frame'First = 0;

   --  Everything wifi.svc and netstack queued, to the air.
   procedure Send_Queued (DMA : RTW89_Bringup.Region)
     with Pre => RTW89_MMIO.Attached;

   --  The last scan (for the joined BSS's SSID in the link report).
   procedure Remember_Scan (Result : WS.Scan_Result);

   type Drop_Counts is array (Wifi_Station_Path.Drop_Reason) of Natural;
   type Statistics is record
      Data_Frames, To_Wifi, To_Netstack, EAPOL_In, EAPOL_Out, Sent_Data, Dropped_Data : Natural := 0;
      Dropped : Drop_Counts := [others => 0];
   end record;
   function Counts return Statistics;

   --  The last EAPOL PDU heard: its length and key information.
   function Last_EAPOL_Length return Natural;
   function Last_EAPOL_Information return Unsigned_16;

end RTW89_Station;
