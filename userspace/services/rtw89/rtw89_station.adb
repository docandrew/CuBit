------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with RTW89_Outcomes; use RTW89_Outcomes;
with RTW89_H2C;
with RTW89_MAC_Init;
with RTW89_TX;
with RTW89_Transmit;
with RTW89_Layout;
with RTW89_Link;
with RTW89_Wifi_Link;
with IEEE80211_Data;
with Wifi_Station_Path;
with Wifi_Replay;

package body RTW89_Station is
   package DT renames IEEE80211_Data;
   package SP renames Wifi_Station_Path;
   use type NL.Hardware_Address;
   use type D.Frame_Kind;
   use type D.Key_Kind;
   use type DT.Address;
   use type WS.Band;

   --  Frame Control octet 0 (802.11-2020 9.2.4.1): type and subtype.
   Type_Mask         : constant Unsigned_8 := 16#0C#;
   Type_Data         : constant Unsigned_8 := 16#08#;

   --  Fixed transmit rates until rate adaptation: management at the
   --  band's lowest basic rate, data at OFDM 24 Mb/s.
   Data_Rate : constant := RTW89_TX.Rate_OFDM_24;
   Data_Rate_Mbps : constant := 24;
   Own_MACID : constant := 0;

   Pairwise_CAM : constant RTW89_H2C.Security_CAM_Index := 0;

   Own       : NL.Hardware_Address := NL.No_Address;
   L : SP.Link;
   Is_Joined : Boolean renames L.Joined;
   BSSID     : NL.Hardware_Address := NL.No_Address;
   On_Band   : WS.Band := WS.Band_2_4_GHz;
   On_Channel : WS.Channel_Number := 1;
   Forward_Beacon : Boolean renames L.Forward_Beacon;
   Port_Open : Boolean renames L.Port_Open;
   Pairwise_Installed : Boolean renames L.Pairwise_Installed;
   Group_Installed renames L.Group_Installed;
   Pairwise_Replay : Wifi_Replay.Counters renames L.Pairwise_Replay;
   Group_Replay renames L.Group_Replay;
   --  The EAPOL key information octet offset (802.1X-2020 11.9; Wifi_EAPOL).
   EAPOL_Info_At : constant := 5;
   Last_Length : Natural := 0;
   Last_EAPOL_Info : Unsigned_16 := 0;
   Next_PN : DT.Packet_Number := 1;
   Next_Sequence : DT.Sequence_Number := 0;
   CAM : RTW89_H2C.Station;
   Last_Scan : WS.Scan_Result;
   Counted : Statistics;

   function To_H2C (A : NL.Hardware_Address) return RTW89_H2C.Address is
     [for I in RTW89_H2C.Address'Range => A (I - 1)];
   function To_DT (A : NL.Hardware_Address) return DT.Address is
     [for I in DT.Address'Range => A (I)];

   procedure Initialize (Own : NL.Hardware_Address) is
   begin
      RTW89_Station.Own := Own;
      L.Own := To_DT (Own);
      CAM := (Own => To_H2C (Own), others => <>);
   end Initialize;

   function Joined return Boolean is (Is_Joined);
   function Band return WS.Band is (On_Band);
   function Channel return WS.Channel_Number is (On_Channel);

   procedure Remember_Scan (Result : WS.Scan_Result) is
   begin
      Last_Scan := Result;
   end Remember_Scan;

   function Counts return Statistics is (Counted);
   function Last_EAPOL_Length return Natural is (Last_Length);
   function Last_EAPOL_Information return Unsigned_16 is (Last_EAPOL_Info);

   ---------------------------------------------------------------------------
   --  Commands
   ---------------------------------------------------------------------------
   procedure Erase_Keys (DMA : RTW89_Bringup.Region) is
      Zero : constant RTW89_H2C.Key_16 := [others => 0];
      R : Outcome;
   begin
      if Pairwise_Installed then
         RTW89_MAC_Init.Install_Key (DMA, Pairwise_CAM, Zero, R);
      end if;
      for I in D.Key_Index loop
         if Group_Installed (I) then
            RTW89_MAC_Init.Install_Key (DMA, RTW89_H2C.Security_CAM_Index (I + 1), Zero, R);
         end if;
      end loop;
      Pairwise_Installed := False;
      Group_Installed := [others => False];
      Pairwise_Replay := (others => <>);
      Group_Replay := [others => (others => <>)];
      CAM.Keys := RTW89_H2C.No_Keys;
   end Erase_Keys;

   function Up_Detail return NL.Up_Detail is
      Network : NL.Network_Identity;
   begin
      for I in 1 .. Last_Scan.Summary.Count loop
         if Last_Scan.Entries (I).BSSID = BSSID then
            Network.SSID := Last_Scan.Entries (I).SSID;
            Network.Length := Last_Scan.Entries (I).Length;
         end if;
      end loop;
      Network.Security := (if Pairwise_Installed then NL.WPA2_Personal else NL.Open);
      return (Kind => NL.Wireless, Speed => Data_Rate_Mbps, BSSID => BSSID, Network => Network);
   end Up_Detail;

   procedure Command (DMA : RTW89_Bringup.Region; Card : RTW89_PHY.Board; Label : Unsigned_32;
                      Words : CuBit.Messages.MessageWords; Result : out D.Outcome) is
      R : Outcome := Success;
   begin
      Result := D.Done;
      if not D.Is_Command (Label) then
         Result := D.Malformed;
         return;
      end if;
      case D.Command_Of (Label) is
         when D.Join =>
            declare
               J : constant D.Decoded_Join := D.Decode (D.Words2'[Words (0), Words (1)]);
               Locked : Boolean;
            begin
               if not J.Valid then
                  Result := D.Malformed;
                  return;
               end if;
               RTW89_PHY.Set_Channel (Card, J.Item.On_Band, J.Item.Channel, Locked, R);
               if not OK (R) then
                  Result := D.Radio_Fault;
                  return;
               end if;
               BSSID := J.Item.BSSID;
               L.BSSID := To_DT (BSSID);
               On_Band := J.Item.On_Band;
               On_Channel := J.Item.Channel;
               CAM := (Own => To_H2C (Own), Peer => To_H2C (BSSID), Kind => RTW89_H2C.No_Link,
                       AID => 0, Keys => RTW89_H2C.No_Keys);
               RTW89_MAC_Init.Update_Station (DMA, CAM, R);
               Is_Joined := True;
               Forward_Beacon := True;
               Next_Sequence := 0;
            end;
         when D.Associated =>
            declare
               AID : constant Unsigned_64 := Words (0) and D.Half_Field;
               Interval : constant Unsigned_16 := Unsigned_16 (Shift_Right (Words (0), D.Interval_Shift) and D.Half_Field);
            begin
               if not Is_Joined then
                  Result := D.Not_Joined;
                  return;
               elsif AID not in Unsigned_64 (D.Association_ID'First) .. Unsigned_64 (D.Association_ID'Last) then
                  Result := D.Malformed;
                  return;
               end if;
               CAM.Kind := RTW89_H2C.Infrastructure;
               CAM.AID := Natural (AID);
               RTW89_MAC_Init.Associate (DMA, CAM, Interval, R);
            end;
         when D.Install_Key =>
            declare
               K : D.Decoded_Key := D.Decode (D.Words4'[Words (0), Words (1), Words (2), Words (3)]);
               Key : RTW89_H2C.Key_16;
            begin
               if not Is_Joined then
                  Result := D.Not_Joined;
                  return;
               elsif not K.Valid or else (K.Item.Kind = D.Group and then K.Item.Index = 0) then
                  Result := D.Malformed;
                  return;
               end if;
               Key := [for I in RTW89_H2C.Key_16'Range => K.Item.Key (I - 1)];
               if K.Item.Kind = D.Pairwise then
                  RTW89_MAC_Init.Install_Key (DMA, Pairwise_CAM, Key, R);
                  CAM.Keys (RTW89_H2C.Pairwise_Slot) := (Valid => True, Index => Pairwise_CAM, Key => K.Item.Index);
                  Pairwise_Installed := True;
                  Pairwise_Replay := Wifi_Replay.Start (K.Item.RSC);
                  Next_PN := 1;
               else
                  RTW89_MAC_Init.Install_Key (DMA, RTW89_H2C.Security_CAM_Index (K.Item.Index + 1), Key, R);
                  CAM.Keys (RTW89_H2C.Security_Slot (K.Item.Index + 1)) :=
                    (Valid => True, Index => RTW89_H2C.Security_CAM_Index (K.Item.Index + 1), Key => K.Item.Index);
                  Group_Installed (K.Item.Index) := True;
                  Group_Replay (K.Item.Index) := Wifi_Replay.Start (K.Item.RSC);
               end if;
               Key := [others => 0];
               K := (Valid => False);
               RTW89_MAC_Init.Update_Station (DMA, CAM, R);
            end;
         when D.Port =>
            declare
               State : D.Port_State;
               Valid : Boolean;
               use type D.Port_State;
            begin
               D.Decode_Port (Words (0), State, Valid);
               if not Valid then
                  Result := D.Malformed;
                  return;
               elsif State = D.Open_Port and then not Is_Joined then
                  Result := D.Not_Joined;
                  return;
               end if;
               Port_Open := State = D.Open_Port;
               if Port_Open then
                  RTW89_Link.Report_Up (Up_Detail);
               else
                  RTW89_Link.Report_Down (NL.Not_Associated);
               end if;
            end;
         when D.Leave =>
            if Port_Open then
               RTW89_Link.Report_Down (NL.Not_Associated);
            end if;
            Port_Open := False;
            Erase_Keys (DMA);
            if Is_Joined then
               CAM := (Own => To_H2C (Own), others => <>);
               RTW89_MAC_Init.Disassociate (DMA, CAM, R);
            end if;
            Is_Joined := False;
            Forward_Beacon := False;
         when D.Transmit =>
            null;
      end case;
      if not OK (R) then
         Result := D.Radio_Fault;
      end if;
   end Command;

   ---------------------------------------------------------------------------
   --  From the air
   ---------------------------------------------------------------------------
   --  wifi.svc gets frames as fresh, 0-based arrays (Wifi_Station_Path).
   procedure Forward (Kind : D.Frame_Kind; F : SP.Octets) is
      Copy : constant RTW89_Wifi_Link.Frame_Bytes (0 .. F'Length - 1) :=
        [for I in 0 .. F'Length - 1 => F (F'First + I)];
   begin
      RTW89_Wifi_Link.Deliver (Kind, Copy);
      Counted.To_Wifi := Counted.To_Wifi + 1;
   end Forward;

   procedure From_Air (Frame : Frame_Bytes; Decrypted, Decrypt_Failed : Boolean) is
      F : constant SP.Octets (0 .. Frame'Length - 1) := [for I in 0 .. Frame'Length - 1 => Frame (I)];
      V : SP.Verdict;
   begin
      if not Is_Joined then
         return;
      end if;
      if (F'Length > 0 and then (F (0) and Type_Mask) = Type_Data) then
         Counted.Data_Frames := Counted.Data_Frames + 1;
      end if;
      SP.Classify (L, F, Decrypted, Decrypt_Failed, V);
      case V.Kind is
         when SP.Drop =>
            Counted.Dropped (V.Why) := Counted.Dropped (V.Why) + 1;
         when SP.To_Wifi_Management =>
            Forward (D.Management, F);
         when SP.To_Wifi_EAPOL =>
            declare
               PDU : constant SP.Octets := SP.EAPOL_PDU (F, V);
            begin
               Counted.EAPOL_In := Counted.EAPOL_In + 1;
               Last_Length := PDU'Length;
               Last_EAPOL_Info := (if PDU'Length >= EAPOL_Info_At + 2
                                   then Shift_Left (Unsigned_16 (PDU (EAPOL_Info_At)), 8)
                                        or Unsigned_16 (PDU (EAPOL_Info_At + 1)) else 0);
               Forward (D.EAPOL, PDU);
            end;
         when SP.To_Netstack =>
            if V.Data.Payload_Length <= DT.Maximum_Octets - SP.Ethernet_Header then
               declare
                  E : constant SP.Octets := SP.Ethernet (F, V);
                  Copy : constant RTW89_Link.Frame_Bytes (0 .. E'Length - 1) := [for I in E'Range => E (I)];
               begin
                  RTW89_Link.Deliver (Copy);
                  Counted.To_Netstack := Counted.To_Netstack + 1;
               end;
            end if;
      end case;
      RTW89_Link.Ring_Receive;
   end From_Air;

   ---------------------------------------------------------------------------
   --  To the air
   ---------------------------------------------------------------------------
   procedure Send_Data (DMA : RTW89_Bringup.Region; Destination : NL.Hardware_Address;
                        Ether_Type : Unsigned_16; Payload : Frame_Bytes) is
      P : constant DT.Protection :=
        (if Pairwise_Installed then (Encrypted => True, PN => Next_PN, Key => 0)
         else (Encrypted => False));
      Header : constant Natural := DT.Header_Bytes_For (P);
      F : DT.Octets (0 .. Header + Payload'Length - 1) := [others => 0];
      Sent : Boolean;
   begin
      if Header + Payload'Length > RTW89_Layout.Maximum_TX_Frame then
         Counted.Dropped_Data := Counted.Dropped_Data + 1;
         return;
      end if;
      DT.Build_Header (To_DT (BSSID), To_DT (Own), To_DT (Destination), Ether_Type, Next_Sequence, P,
                       F (0 .. DT.Maximum_Header - 1));
      for I in Payload'Range loop
         F (Header + I - Payload'First) := Payload (I);
      end loop;
      RTW89_Transmit.Send
        (DMA, RTW89_Layout.Data_Best_Effort,
         RTW89_Transmit.Frame_Bytes (F),
         (On => RTW89_TX.ACH0, Station => Own_MACID, SW_Sequence => Next_Sequence, Rate => Data_Rate,
          Header_Bytes => DT.Header_Bytes, Group => (Destination (0) and NL.Group_Bit) /= 0,
          Encrypt => Pairwise_Installed, Security_Entry => Pairwise_CAM, others => <>),
         Sent);
      if Sent then
         Counted.Sent_Data := Counted.Sent_Data + 1;
         Next_Sequence := (Next_Sequence + 1) mod (DT.Sequence_Number'Last + 1);
         if Pairwise_Installed then
            Next_PN := Next_PN + 1;
         end if;
      else
         Counted.Dropped_Data := Counted.Dropped_Data + 1;
      end if;
   end Send_Data;

   procedure Send_Queued (DMA : RTW89_Bringup.Region) is
      Kind : D.Frame_Kind;
      Frame : RTW89_Wifi_Link.Frame_Buffer;
      Length : Natural;
      Found : Boolean;
      Sent : Boolean;
   begin
      --  wifi.svc's frames first: they come before the command that
      --  follows them (a message 4 before Install_Key).
      loop
         exit when not RTW89_Transmit.Has_Room (RTW89_Layout.Management_Channel)
           or else not RTW89_Transmit.Has_Room (RTW89_Layout.Data_Best_Effort);
         RTW89_Wifi_Link.Next_Frame (Kind, Frame, Length, Found);
         exit when not Found;
         if Kind = D.Management then
            RTW89_Transmit.Send
              (DMA, RTW89_Layout.Management_Channel,
               RTW89_Transmit.Frame_Bytes (Frame (0 .. Length - 1)),
               (On => RTW89_TX.B0MG, Station => Own_MACID,
                Rate => (if On_Band = WS.Band_2_4_GHz then RTW89_TX.Rate_CCK_1 else RTW89_TX.Rate_OFDM_6),
                Group => (Frame (4) and NL.Group_Bit) /= 0, others => <>),
               Sent);
         elsif Is_Joined then
            Counted.EAPOL_Out := Counted.EAPOL_Out + 1;
            Send_Data (DMA, BSSID, DT.EtherType_EAPOL, Frame_Bytes (Frame (0 .. Length - 1)));
         end if;
      end loop;
      --  netstack's frames, while the port is open.
      while Port_Open and then RTW89_Transmit.Has_Room (RTW89_Layout.Data_Best_Effort) loop
         declare
            E : RTW89_Link.Ethernet_Buffer;
         begin
            RTW89_Link.Next_Frame (E, Length, Found);
            exit when not Found;
            if Length >= SP.Ethernet_Header then
               Send_Data (DMA, [for I in NL.Octet_Index => E (I)],
                          Shift_Left (Unsigned_16 (E (12)), 8) or Unsigned_16 (E (13)),
                          Frame_Bytes (E (SP.Ethernet_Header .. Length - 1)));
            end if;
         end;
      end loop;
   end Send_Queued;

end RTW89_Station;
