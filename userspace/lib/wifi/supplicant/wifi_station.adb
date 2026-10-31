------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with IEEE80211_Beacons;
with IEEE80211_Management;
with Wifi_RSN;

package body Wifi_Station with SPARK_Mode is
   package B renames IEEE80211_Beacons;
   package MG renames IEEE80211_Management;
   package R renames Wifi_RSN;

   use type M.Phase;
   use type M.Action_Kind;
   use type M.Security;
   use type MG.Answer_Kind;
   use type H.Result;
   use type H.Install_Kind;
   use type NL.Hardware_Address;

   CCMP_Key_Bytes : constant := 16;
   --  Key Information, big endian (Wifi_EAPOL): Secure is bit 9 (the high
   --  octet's bit 1), Pairwise bit 3 (the low octet's).
   Secure_High  : constant Unsigned_8 := 2 ** 1;
   Pairwise_Low : constant Unsigned_8 := 2 ** 3;
   Element_Header : constant := 2;

   function To_MG (A : NL.Hardware_Address) return MG.Address is
     [for I in MG.Address'Range => A (I)];
   function To_H (A : NL.Hardware_Address) return H.Address is
     [for I in H.Address'Range => A (I)];

   procedure Add (L : in out Outputs; O : Output) is
   begin
      if L.Count < Maximum_Outputs then
         L.Count := L.Count + 1;
         L.Items (L.Count) := O;
      end if;
   end Add;

   procedure Add_Command (L : in out Outputs; Kind : Output_Kind) is
      O : Output;
   begin
      O.Kind := Kind;
      Add (L, O);
   end Add_Command;

   procedure Add_Port (L : in out Outputs; State : D.Port_State) is
      O : Output;
   begin
      O.Kind := Do_Port;
      O.Port := State;
      Add (L, O);
   end Add_Port;

   procedure Send_Authentication (S : Station; L : in out Outputs) is
      O : Output;
      F : MG.Octets (0 .. MG.Authentication_Bytes - 1);
      Length : Natural;
   begin
      MG.Build_Authentication (To_MG (S.Target.BSSID), To_MG (S.Own), F, Length);
      O.Kind := Send_Management;
      for I in F'Range loop
         O.Frame (I) := F (I);
      end loop;
      O.Length := Length;
      Add (L, O);
   end Send_Authentication;

   procedure Send_Association (S : Station; L : in out Outputs) is
      O : Output;
      F : MG.Octets (0 .. MG.Maximum_Frame - 1);
      Length : Natural;
      Capability : constant Unsigned_16 :=
        MG.Capability_ESS
        or (S.Capability and (MG.Capability_Short_Preamble or MG.Capability_Short_Slot))
        or (if S.Kind = WPA2_PSK then MG.Capability_Privacy else 0);
      SSID : constant MG.Octets (0 .. S.Target.Length - 1) :=
        [for I in 0 .. S.Target.Length - 1 => S.Target.SSID (I + 1)];
      Rates : constant MG.Octets (0 .. Natural'Min (S.Rates_Length, MG.Maximum_Rates) - 1) :=
        [for I in 0 .. Natural'Min (S.Rates_Length, MG.Maximum_Rates) - 1 => S.Rates (I)];
      Extended : constant MG.Octets (0 .. S.Extended_Length - 1) :=
        [for I in 0 .. S.Extended_Length - 1 => S.Extended (I)];
      Own_RSN : constant MG.Octets (0 .. R.Own_Element_Bytes - 1) :=
        [for I in 0 .. R.Own_Element_Bytes - 1 => R.Own_Element (I)];
      None : constant MG.Octets (1 .. 0) := [others => 0];
   begin
      if S.Kind = WPA2_PSK then
         MG.Build_Association_Request (To_MG (S.Target.BSSID), To_MG (S.Own), Capability,
                                       SSID, Rates, Extended, Own_RSN, F, Length);
      else
         MG.Build_Association_Request (To_MG (S.Target.BSSID), To_MG (S.Own), Capability,
                                       SSID, Rates, Extended, None, F, Length);
      end if;
      O.Kind := Send_Management;
      for I in 0 .. Length - 1 loop
         O.Frame (I) := F (I);
      end loop;
      O.Length := Length;
      Add (L, O);
   end Send_Association;

   --  Back to idle: the port closed if it was open, the driver told.
   procedure Finish (S : in out Station; Why : M.Failure; L : in out Outputs)
   with Pre => Valid (S), Post => Valid (S)
   is
   begin
      if S.Port_Open then
         Add_Port (L, D.Closed);
         S.Port_Open := False;
      end if;
      Add_Command (L, Do_Leave);
      H.Clear (S.Session);
      S.PMK := [others => 0];
      S.Phase := None;
      S.Failure := Why;
   end Finish;

   --  Carry out what the MLME asked for.
   procedure Perform (S : in out Station; A : M.Actions; L : in out Outputs)
   with Pre => Valid (S), Post => Valid (S)
   is
   begin
      for I in 1 .. A.Count loop
         case A.Items (I) is
            when M.Send_Authentication => Send_Authentication (S, L);
            when M.Send_Association    => Send_Association (S, L);
            when M.Start_Handshake =>
               --  The beacon's RSN element (header included), for message 3.
               H.Start (S.Session, S.PMK, To_H (S.Target.BSSID), To_H (S.Own),
                        S.Beacon_RSN, S.Beacon_RSN_Length, S.S_Nonce);
            when M.Stop_Handshake => H.Clear (S.Session);
            when M.Open_Port =>
               Add_Port (L, D.Open_Port);
               S.Port_Open := True;
            when M.Close_Port =>
               Add_Port (L, D.Closed);
               S.Port_Open := False;
            when M.Report_Connected => null;
            when M.Report_Failure =>
               Add_Command (L, Do_Leave);
               H.Clear (S.Session);
               S.PMK := [others => 0];
               S.Phase := None;
               S.Failure := S.Machine.Reason;
         end case;
      end loop;
   end Perform;

   procedure Step (S : in out Station; Ev : M.Event; L : in out Outputs)
   with Pre => Valid (S), Post => Valid (S)
   is
      A : M.Actions;
   begin
      M.Step (S.Machine, Ev, A);
      Perform (S, A, L);
   end Step;

   procedure Reset (S : out Station; Own : NL.Hardware_Address; Bounds : M.Limits;
                    Beacon_Timeout : M.Milliseconds) is
   begin
      S := (Own => Own, Bounds => Bounds, Beacon_Timeout => Beacon_Timeout, others => <>);
   end Reset;

   procedure Join (S : in out Station; Target : WS.BSS_Entry; Kind : Security;
                   PMK : H.PMK_Value; S_Nonce : E.Nonce; Now : M.Milliseconds;
                   Out_List : out Outputs; Accepted : out Boolean) is
      O : Output;
   begin
      Out_List := (Count => 0, Items => [others => <>]);
      Accepted := S.Phase = None;
      if not Accepted then
         return;
      end if;
      S.Target := Target;
      S.Kind := Kind;
      S.PMK := PMK;
      S.S_Nonce := S_Nonce;
      S.Machine := M.Initial;
      S.Failure := M.None;
      S.Rates_Length := 0;
      S.Extended_Length := 0;
      S.Beacon_RSN_Length := 0;
      S.Phase := Waiting_Beacon;
      S.Beacon_Deadline := (if S.Beacon_Timeout > M.Milliseconds'Last - Now then M.Milliseconds'Last
                            else Now + S.Beacon_Timeout);
      O.Kind := Do_Join;
      O.Join := (BSSID => Target.BSSID, On_Band => Target.On_Band, Channel => Target.Channel);
      Add (Out_List, O);
   end Join;

   --  For WPA2: the beacon's RSN element (data at Place) must be
   --  joinable; it is kept, header included, for message 3's check.
   procedure Check_RSN (S : in out Station; F : B.Frame_Bytes; Present : Boolean;
                        Place : B.Element_Place; L : in out Outputs)
   with Pre => Valid (S) and then F'First = 0 and then B.Inside (Place, F'Length), Post => Valid (S)
   is
      Offer : R.Offer;
      Status : R.Status;
      Data : constant R.Octets (0 .. Place.Length - 1) :=
        [for I in 0 .. Place.Length - 1 => F (Place.First + I)];
      use type R.Status;
   begin
      if not Present or else Place.Length + Element_Header > H.Maximum_RSN_Element then
         Finish (S, M.Association_Refused, L);
         return;
      end if;
      R.Decode (Data, Offer, Status);
      if Status /= R.Decoded or else not R.Joinable (Offer) then
         Finish (S, M.Association_Refused, L);
         return;
      end if;
      S.Beacon_RSN := [others => 0];
      S.Beacon_RSN (0) := B.Element_RSN;
      S.Beacon_RSN (1) := Unsigned_8 (Place.Length);
      for I in Data'Range loop
         S.Beacon_RSN (Element_Header + I) := Data (I);
      end loop;
      S.Beacon_RSN_Length := Place.Length + Element_Header;
   end Check_RSN;

   --  The BSS's beacon or probe response, while waiting for it.
   procedure Take_Beacon (S : in out Station; Frame : Frame_Bytes; Now : M.Milliseconds;
                          L : in out Outputs; Taken : out Boolean)
   with Pre => Valid (S) and then Frame'First = 0 and then Frame'Length <= B.Maximum_Frame,
        Post => Valid (S)
   is
      F : constant B.Frame_Bytes (0 .. Frame'Length - 1) := [for I in 0 .. Frame'Length - 1 => Frame (I)];
      Info : B.Decoded;
      Result : B.Status;
      use type B.Status;
   begin
      Taken := False;
      B.Decode (F, Info, Result);
      if Result /= B.Accepted or else [for I in NL.Octet_Index => Info.BSSID (I)] /= S.Target.BSSID then
         return;
      end if;
      Taken := True;
      S.Capability := Info.Capability;
      S.Rates_Length := Info.Rates.Length;
      for I in 0 .. Info.Rates.Length - 1 loop
         S.Rates (I) := F (Info.Rates.First + I);
      end loop;
      S.Extended_Length := Info.Extended_Rates.Length;
      for I in 0 .. Info.Extended_Rates.Length - 1 loop
         S.Extended (I) := F (Info.Extended_Rates.First + I);
      end loop;
      if S.Kind = WPA2_PSK then
         Check_RSN (S, F, Info.RSN, Info.RSN_Element, L);
         if S.Phase = None then
            return;
         end if;
      end if;
      S.Phase := In_MLME;
      Step (S, (Kind => M.Join, Now => Now,
                Network => (if S.Kind = WPA2_PSK then M.WPA2_PSK else M.Open),
                Bounds => S.Bounds, Success => False), L);
   end Take_Beacon;

   procedure Management (S : in out Station; Frame : Frame_Bytes; Now : M.Milliseconds;
                         Out_List : out Outputs) is
      Taken : Boolean;
   begin
      Out_List := (Count => 0, Items => [others => <>]);
      case S.Phase is
         when None =>
            S.Count.Ignored_Frames := S.Count.Ignored_Frames + 1;
         when Waiting_Beacon =>
            Take_Beacon (S, Frame, Now, Out_List, Taken);
            if not Taken then
               S.Count.Ignored_Frames := S.Count.Ignored_Frames + 1;
            end if;
         when In_MLME =>
            declare
               F : constant MG.Octets (0 .. Frame'Length - 1) := [for I in 0 .. Frame'Length - 1 => Frame (I)];
               A : MG.Answer;
            begin
               MG.Decode (F, To_MG (S.Target.BSSID), To_MG (S.Own), A);
               case A.Kind is
                  when MG.Authentication_Answer =>
                     if A.Algorithm = MG.Algorithm_Open_System
                       and then A.Sequence = MG.Authentication_Response_Sequence
                     then
                        Step (S, (Kind => M.Authentication_Answer, Now => Now,
                                  Success => A.Status = MG.Status_Success, others => <>), Out_List);
                     else
                        S.Count.Ignored_Frames := S.Count.Ignored_Frames + 1;
                     end if;
                  when MG.Association_Answer =>
                     if S.Machine.Current = M.Associating and then A.Status = MG.Status_Success
                       and then Natural (A.AID) in D.Association_ID
                     then
                        S.AID := Natural (A.AID);
                        declare
                           O : Output;
                        begin
                           O.Kind := Do_Associated;
                           O.AID := S.AID;
                           O.Interval := S.Target.Interval;
                           Add (Out_List, O);
                        end;
                        Step (S, (Kind => M.Association_Answer, Now => Now, Success => True,
                                  others => <>), Out_List);
                     else
                        Step (S, (Kind => M.Association_Answer, Now => Now, Success => False,
                                  others => <>), Out_List);
                     end if;
                  when MG.Deauthenticated =>
                     Step (S, (Kind => M.Deauthentication, Now => Now, others => <>), Out_List);
                  when MG.Disassociated =>
                     Step (S, (Kind => M.Disassociation, Now => Now, others => <>), Out_List);
                  when MG.Other =>
                     S.Count.Ignored_Frames := S.Count.Ignored_Frames + 1;
               end case;
            end;
      end case;
   end Management;

   procedure Install (L : in out Outputs; Kind : D.Key_Kind; Index : D.Key_Index;
                      Key : D.Key_Octets; RSC : D.Packet_Number) is
      O : Output;
   begin
      O.Kind := Do_Install_Key;
      O.Key := (Kind => Kind, Index => Index, Suite => D.CCMP_128, Key => Key, RSC => RSC);
      Add (L, O);
   end Install;

   function RSC_Of (V : E.RSC_Value) return D.Packet_Number is
     (D.Packet_Number (V (0)) + D.Packet_Number (V (1)) * 2 ** 8 + D.Packet_Number (V (2)) * 2 ** 16
      + D.Packet_Number (V (3)) * 2 ** 24 + D.Packet_Number (V (4)) * 2 ** 32
      + D.Packet_Number (V (5)) * 2 ** 40);

   procedure EAPOL (S : in out Station; Frame : Frame_Bytes; Now : M.Milliseconds;
                    Out_List : out Outputs) is
      Reply : H.Reply_Frame;
      Length : Natural;
      Keys : H.Install_Request;
      Outcome : H.Result;
      F : constant E.Octets (0 .. Frame'Length - 1) := [for I in 0 .. Frame'Length - 1 => Frame (I)];
   begin
      Out_List := (Count => 0, Items => [others => <>]);
      S.Count.EAPOL_Frames := S.Count.EAPOL_Frames + 1;
      if S.Phase /= In_MLME or else S.Machine.Current not in M.Handshaking | M.Connected
        or else S.Kind /= WPA2_PSK
      then
         S.Count.Ignored_Frames := S.Count.Ignored_Frames + 1;
         S.Count.Last_EAPOL := Not_Expected;
         return;
      end if;
      H.Receive (S.Session, F, Reply, Length, Keys, Outcome);
      S.Count.Last_EAPOL :=
        (case Outcome is
           when H.Replied =>
             (if (Reply (E.Key_Information_At) and Secure_High) = 0 then Sent_Message_2
              elsif (Reply (E.Key_Information_At + 1) and Pairwise_Low) /= 0 then Sent_Message_4
              else Sent_Group_2),
           when H.Ignored => Ignored,
           when H.Replay_Refused => Replay_Refused,
           when H.MIC_Failed => MIC_Failed,
           when H.Nonce_Changed => Nonce_Changed,
           when H.RSN_Mismatch => RSN_Mismatch,
           when H.Key_Data_Refused => Key_Data_Refused,
           when H.Malformed => Malformed);
      case Outcome is
         when H.Replied =>
            --  The reply leaves first: message 4 goes out before the keys
            --  are installed, so it is not encrypted with them.
            declare
               O : Output;
            begin
               O.Kind := Send_EAPOL;
               for I in 0 .. Length - 1 loop
                  O.Frame (I) := Reply (I);
               end loop;
               O.Length := Length;
               Add (Out_List, O);
            end;
            if Keys.Kind /= H.Nothing and then Keys.GTK_Bytes /= CCMP_Key_Bytes then
               --  A group cipher other than CCMP-128: not CuBit's policy.
               S.Count.Last_EAPOL := Group_Cipher_Refused;
               Step (S, (Kind => M.Handshake_Refused, Now => Now, others => <>), Out_List);
               return;
            end if;
            if Keys.Kind = H.Pairwise_And_Group then
               Install (Out_List, D.Pairwise, 0, [for I in D.Key_Octets'Range => Keys.TK (I)], 0);
            end if;
            if Keys.Kind /= H.Nothing then
               Install (Out_List, D.Group, Keys.GTK_Index,
                        [for I in D.Key_Octets'Range => Keys.GTK (I + 1)], RSC_Of (Keys.GTK_RSC));
            end if;
            if Keys.Kind = H.Pairwise_And_Group then
               Step (S, (Kind => M.Handshake_Established, Now => Now, others => <>), Out_List);
            end if;
         when H.RSN_Mismatch | H.Nonce_Changed | H.Key_Data_Refused =>
            S.Count.EAPOL_Refused := S.Count.EAPOL_Refused + 1;
            Step (S, (Kind => M.Handshake_Refused, Now => Now, others => <>), Out_List);
         when H.Ignored | H.Replay_Refused | H.MIC_Failed | H.Malformed =>
            S.Count.EAPOL_Refused := S.Count.EAPOL_Refused + 1;
      end case;
   end EAPOL;

   procedure Tick (S : in out Station; Now : M.Milliseconds; Out_List : out Outputs) is
   begin
      Out_List := (Count => 0, Items => [others => <>]);
      case S.Phase is
         when None => null;
         when Waiting_Beacon =>
            if Now >= S.Beacon_Deadline then
               Finish (S, M.Authentication_Timeout, Out_List);
            end if;
         when In_MLME =>
            Step (S, (Kind => M.Tick, Now => Now, others => <>), Out_List);
      end case;
   end Tick;

   procedure Leave (S : in out Station; Now : M.Milliseconds; Out_List : out Outputs) is
   begin
      Out_List := (Count => 0, Items => [others => <>]);
      case S.Phase is
         when None => null;
         when Waiting_Beacon => Finish (S, M.Left, Out_List);
         when In_MLME => Step (S, (Kind => M.Leave, Now => Now, others => <>), Out_List);
      end case;
   end Leave;

   procedure Radio_Fault (S : in out Station; Now : M.Milliseconds; Out_List : out Outputs) is
      pragma Unreferenced (Now);
   begin
      Out_List := (Count => 0, Items => [others => <>]);
      if S.Phase /= None then
         Finish (S, M.Left, Out_List);
      end if;
   end Radio_Fault;

end Wifi_Station;
