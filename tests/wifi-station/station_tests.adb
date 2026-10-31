--  Hosted tests for the chip-independent Wi-Fi station units
--  (userspace/lib/wifi; NET-WIFI-001 W3/W4): the 4-way handshake replayed
--  against a recorded WPA2 exchange, the management frame builders and
--  decoder against the same capture, the join state machine, and the
--  wifi.svc <-> driver codecs (CuBit.Wifi_Driver).
--
--  The recorded frames are from the Wireshark sample capture
--  wpa-Induction.pcap (SSID "Coherer", passphrase "Induction"; AP
--  00:0c:41:82:b2:55, station 00:0d:93:82:36:3a; frames 78, 80, 82, 84 and
--  the EAPOL-Key frames 87, 89, 92, 94). The expected keys (PMK, KCK, TK,
--  GTK) were computed independently with Python hashlib/hmac and the
--  cryptography package's AES key unwrap, not with the code under test.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with SPARKNaCl; use SPARKNaCl;
with WiFi_Crypto.Legacy_WPA2;
with Wifi_EAPOL;
with Wifi_Handshake;
with Wifi_RSN;
with Wifi_MLME;
with IEEE80211_Management;
with IEEE80211_Data;
with CuBit.Wifi_Driver;
with CuBit.Wifi_Control;
with CuBit.Wifi_Scan;
with CuBit.Net_Link;
with Wifi_Station;
with Wifi_Replay;
with Wifi_Station_Path;
with WiFi_Crypto.AES_Key_Wrap;
with SPARKNaCl.AES;

procedure Station_Tests is
   package E renames Wifi_EAPOL;
   package H renames Wifi_Handshake;
   package M renames Wifi_MLME;
   package MG renames IEEE80211_Management;
   package D renames CuBit.Wifi_Driver;
   package W2 renames WiFi_Crypto.Legacy_WPA2;
   use type E.MIC_Value;
   use type D.Operation;
   use type D.Command;

   Passed, Failed : Natural := 0;

   procedure Check (Name : String; Condition : Boolean) is
   begin
      if Condition then
         Passed := Passed + 1;
      else
         Failed := Failed + 1;
         Put_Line ("FAIL " & Name);
      end if;
   end Check;

   function Nibble (C : Character) return Unsigned_8 is
     (case C is
        when '0' .. '9' => Character'Pos (C) - Character'Pos ('0'),
        when 'a' .. 'f' => Character'Pos (C) - Character'Pos ('a') + 10,
        when others     => raise Constraint_Error);

   function Hex (S : String) return E.Octets is
      R : E.Octets (0 .. S'Length / 2 - 1);
   begin
      for I in R'Range loop
         R (I) := Nibble (S (S'First + 2 * I)) * 16 + Nibble (S (S'First + 2 * I + 1));
      end loop;
      return R;
   end Hex;

   function To_MG (O : E.Octets) return MG.Octets is [for I in O'Range => O (I)];

   ---------------------------------------------------------------------------
   --  The recording.
   ---------------------------------------------------------------------------
   AP_Hex  : constant String := "000c4182b255";
   STA_Hex : constant String := "000d9382363a";
   Beacon_RSN_Hex : constant String := "30180100000fac020200000fac04000fac020100000fac020000";
   M1_Hex : constant String :=
     "0203007502008a001000000000000000003e8e967dacd960324cac5b6aa721235bf57b949771c867"
     & "989f49d04ed47c693300000000000000000000000000000000000000000000000000000000000000"
     & "00000000000000000000000000000000000016dd14000fac04592da88096c461da246c69001e877f"
     & "3d";
   M2_Hex : constant String :=
     "0203007502010a00100000000000000000cdf405ceb9d889ef3dec42609828fae546b7add7baecbb"
     & "1a394eac5214b1d38600000000000000000000000000000000000000000000000000000000000000"
     & "00a462a7029ad5ba30b6af0df391988e45001630140100000fac020100000fac040100000fac0200"
     & "00";
   M3_Hex : constant String :=
     "020300af0213ca001000000000000000013e8e967dacd960324cac5b6aa721235bf57b949771c867"
     & "989f49d04ed47c6933f57b949771c867989f49d04ed47c6934cf0200000000000000000000000000"
     & "007d0af6df51e99cde7a187453f0f935370050cfa72cde35b2c1e2319255806ab364179fd9673041"
     & "b9a5939fa1a2010d2ac794e25168055f794ddc1fdfae3521f4446bfd11da98345f543df6ce199df8"
     & "fe48f8cdd17adca87bf45711183c496d41aa0c";
   M4_Hex : constant String :=
     "0203005f02030a001000000000000000010000000000000000000000000000000000000000000000"
     & "00000000000000000000000000000000000000000000000000000000000000000000000000000000"
     & "0010bba3bdfbcfde2bc537509d71f2ecd10000";
   PMK_Hex : constant String := "a288fcf0caaacda9a9f58633ff35e8992a01d9c10ba5e02efdf8cb5d730ce7bc";
   KCK_Hex : constant String := "b1cd792716762903f723424cd7d16511";
   TK_Hex  : constant String := "15798d511beae0028313c8ab32f12c7e";
   GTK_Hex : constant String := "ee22041a83853263474c38811352282071c122359b7c35a7e7d034f3cd6ac565";
   GTK_Index : constant := 2;

   function Address_Of (S : String) return H.Address is
      O : constant E.Octets := Hex (S);
   begin
      return [for I in H.Address'Range => O (I)];
   end Address_Of;

   --  HMAC-SHA1-128 under KCK over Frame with its MIC field zero: the
   --  test's own MIC, through WiFi_Crypto directly (known-answer tested in
   --  tests/wifi-crypto), not through Wifi_Handshake.
   function MIC (KCK : E.Octets; Frame : E.Octets) return E.MIC_Value is
      K : Byte_Seq (0 .. KCK'Length - 1);
      B : Byte_Seq (0 .. Frame'Length - 1);
      Digest : W2.HMAC_SHA1.Digest;
   begin
      for I in K'Range loop
         K (I) := Byte (KCK (Natural (I)));
      end loop;
      for I in B'Range loop
         B (I) := (if Natural (I) in E.MIC_At .. E.MIC_At + E.MIC_Bytes - 1 then 0
                   else Byte (Frame (Natural (I))));
      end loop;
      W2.HMAC_SHA1.Compute (Digest, B, K);
      return [for I in E.MIC_Value'Range => Unsigned_8 (Digest (N32 (I)))];
   end MIC;

   function MIC_Field (Frame : E.Octets) return E.MIC_Value is
     [for I in E.MIC_Value'Range => Frame (E.MIC_At + I)];

   --  Frame with its replay counter set to Replay (and, if New_Nonce,
   --  its nonce's last octet changed), MIC recomputed under KCK: what an
   --  attacker holding the KCK, or a retransmitting AP, would send.
   function Resealed (Frame : E.Octets; Replay : Unsigned_8; New_Nonce : Boolean := False)
     return E.Octets
   is
      R : E.Octets := Frame;
   begin
      R (E.Replay_At + E.Replay_Bytes - 1) := Replay;
      if New_Nonce then
         R (E.Nonce_At + E.Nonce_Bytes - 1) := R (E.Nonce_At + E.Nonce_Bytes - 1) xor 1;
      end if;
      declare
         Value : constant E.MIC_Value := MIC (Hex (KCK_Hex), R);
      begin
         for I in Value'Range loop
            R (E.MIC_At + I) := Value (I);
         end loop;
      end;
      return R;
   end Resealed;

   procedure Start (S : out H.Session; Beacon_Hex : String := Beacon_RSN_Hex) is
      PMK : Byte_Seq (0 .. W2.PMK_Bytes - 1);
      Pass : constant String := "Induction";
      SSID : constant String := "Coherer";
      P : Byte_Seq (0 .. Pass'Length - 1);
      Q : Byte_Seq (0 .. SSID'Length - 1);
      B : constant E.Octets := Hex (Beacon_Hex);
      Beacon : H.RSN_Element := [others => 0];
      M2 : constant E.Octets := Hex (M2_Hex);
      Expected_PMK : constant E.Octets := Hex (PMK_Hex);
   begin
      for I in P'Range loop
         P (I) := Character'Pos (Pass (Pass'First + Natural (I)));
      end loop;
      for I in Q'Range loop
         Q (I) := Character'Pos (SSID (SSID'First + Natural (I)));
      end loop;
      W2.Passphrase_To_PMK (PMK, P, Q);
      Check ("PMK from passphrase", (for all I in PMK'Range => Unsigned_8 (PMK (I)) = Expected_PMK (Natural (I))));
      for I in B'Range loop
         Beacon (I) := B (I);
      end loop;
      H.Start (S, [for I in H.PMK_Value'Range => Unsigned_8 (PMK (N32 (I)))],
               AA => Address_Of (AP_Hex), SPA => Address_Of (STA_Hex),
               Beacon_RSN => Beacon, Beacon_RSN_Bytes => B'Length,
               S_Nonce => [for I in E.Nonce'Range => M2 (E.Nonce_At + I)]);
   end Start;

   procedure Handshake_Tests is
      S : H.Session;
      Reply : H.Reply_Frame;
      Length : Natural;
      Install : H.Install_Request;
      Outcome : H.Result;
      M1 : constant E.Octets := Hex (M1_Hex);
      M2 : constant E.Octets := Hex (M2_Hex);
      M3 : constant E.Octets := Hex (M3_Hex);
      M4 : constant E.Octets := Hex (M4_Hex);
      TK : constant E.Octets := Hex (TK_Hex);
      GTK : constant E.Octets := Hex (GTK_Hex);
      use type H.Result;
      use type H.Install_Kind;
      use type H.Phase;
   begin
      Start (S);
      --  Message 1 -> message 2: CuBit's RSN element instead of the
      --  recorded station's (which offered a TKIP group cipher), so the
      --  frame matches the recording up to the MIC, and its MIC verifies
      --  under the independently derived KCK.
      H.Receive (S, M1, Reply, Length, Install, Outcome);
      Check ("M1 answered", Outcome = H.Replied and then Length = E.Key_Data_At + Wifi_RSN.Own_Element_Bytes);
      Check ("M2 matches recording up to MIC", (for all I in 0 .. E.MIC_At - 1 => Reply (I) = M2 (I)));
      Check ("M2 key data is CuBit's RSN element",
             (for all I in 0 .. Wifi_RSN.Own_Element_Bytes - 1 =>
                Reply (E.Key_Data_At + I) = Wifi_RSN.Own_Element (I)));
      Check ("M2 MIC under KCK", MIC_Field (Reply) = MIC (Hex (KCK_Hex), Reply (0 .. Length - 1)));
      Check ("M1 installs nothing", Install.Kind = H.Nothing);

      --  Message 3 -> message 4, byte for byte the recorded one; TK and GTK out.
      H.Receive (S, M3, Reply, Length, Install, Outcome);
      Check ("M3 answered", Outcome = H.Replied and then H.Current (S) = H.Established);
      Check ("M4 equals recording", Length = M4'Length and then
             (for all I in M4'Range => Reply (I) = M4 (I)));
      Check ("M3 installs pairwise and group", Install.Kind = H.Pairwise_And_Group);
      Check ("TK", (for all I in TK'Range => Install.TK (I) = TK (I)));
      Check ("GTK", Install.GTK_Bytes = GTK'Length
             and then (for all I in GTK'Range => Install.GTK (I + 1) = GTK (I)));
      Check ("GTK index", Install.GTK_Index = GTK_Index);
      Check ("GTK RSC", (for all I in E.RSC_Value'Range => Install.GTK_RSC (I) = M3 (E.RSC_At + I)));

      --  The same message 3 again: replay counter not greater -> refused.
      H.Receive (S, M3, Reply, Length, Install, Outcome);
      Check ("M3 replay refused", Outcome = H.Replay_Refused and then Install.Kind = H.Nothing);
      --  A forged counter without the KCK: MIC fails.
      declare
         Forged : E.Octets := M3;
      begin
         Forged (E.Replay_At + E.Replay_Bytes - 1) := 2;
         H.Receive (S, Forged, Reply, Length, Install, Outcome);
         Check ("M3 forged counter: MIC fails", Outcome = H.MIC_Failed);
      end;
      --  A retransmitted message 3 (higher counter, valid MIC): answered
      --  with message 4, no key reinstalled (KRACK).
      H.Receive (S, Resealed (M3, 2), Reply, Length, Install, Outcome);
      Check ("M3 retransmission answered", Outcome = H.Replied and then Reply (E.Replay_At + 7) = 2);
      Check ("M3 retransmission installs nothing", Install.Kind = H.Nothing);
      Check ("M3 retransmission: no TK", (for all I in H.Key_16'Range => Install.TK (I) = 0));

      --  Message 3 whose ANonce differs from message 1's.
      Start (S);
      H.Receive (S, M1, Reply, Length, Install, Outcome);
      H.Receive (S, Resealed (M3, 1, New_Nonce => True), Reply, Length, Install, Outcome);
      Check ("M3 changed ANonce refused", Outcome = H.Nonce_Changed and then Install.Kind = H.Nothing);

      --  Downgrade: the beacon offered a different RSN element.
      Start (S, "30140100000fac040100000fac040100000fac020000");
      H.Receive (S, M1, Reply, Length, Install, Outcome);
      H.Receive (S, M3, Reply, Length, Install, Outcome);
      Check ("M3 RSN mismatch refused", Outcome = H.RSN_Mismatch and then Install.Kind = H.Nothing);

      --  Message 3 before message 1: ignored.
      Start (S);
      H.Receive (S, M3, Reply, Length, Install, Outcome);
      Check ("M3 before M1 ignored", Outcome = H.Ignored);
      --  Truncated frame.
      H.Receive (S, M1 (0 .. 50), Reply, Length, Install, Outcome);
      Check ("truncated malformed", Outcome = H.Malformed);
      H.Clear (S);
      Check ("cleared", H.Current (S) = H.Failed);
   end Handshake_Tests;

   procedure Management_Tests is
      AP  : constant MG.Address := [for I in MG.Address'Range => Hex (AP_Hex) (I)];
      STA : constant MG.Address := [for I in MG.Address'Range => Hex (STA_Hex) (I)];
      Frame : MG.Octets (0 .. MG.Maximum_Frame - 1);
      Length : Natural;
      Answer : MG.Answer;
      use type MG.Answer_Kind;
      --  Recorded frames 78 and 82 with duration and sequence control zero
      --  (the hardware fills them); 82's elements in 802.11-2020 Table
      --  9-35 order (the recorded station put Extended Rates after RSN).
      Auth_78 : constant E.Octets :=
        Hex ("b0000000000c4182b255000d9382363a000c4182b2550000000001000000");
      Assoc_82 : constant E.Octets :=
        Hex ("00000000000c4182b255000d9382363a000c4182b2550000"
             & "31040a00" & "0007436f6865726572" & "010882848b962430486c" & "32040c121860"
             & "30140100000fac020100000fac040100000fac020000");
      Auth_80 : constant MG.Octets := To_MG (Hex
        ("b0003a01000d9382363a000c4182b255000c4182b25590fc000002000000dd06001018020004"));
      Assoc_84 : constant MG.Octets := To_MG (Hex
        ("10003a01000d9382363a000c4182b255000c4182b255a0fc1104000001c0010882848b962430486c"
         & "32040c121860dd06001018020004"));
   begin
      MG.Build_Authentication (AP, STA, Frame, Length);
      Check ("authentication frame", Length = Auth_78'Length
             and then (for all I in Auth_78'Range => Frame (I) = Auth_78 (I)));
      MG.Build_Association_Request
        (AP, STA, 16#0431#, To_MG (Hex ("436f6865726572")), To_MG (Hex ("82848b962430486c")),
         To_MG (Hex ("0c121860")), To_MG (Hex ("30140100000fac020100000fac040100000fac020000")),
         Frame, Length);
      Check ("association request", Length = Assoc_82'Length
             and then (for all I in Assoc_82'Range => Frame (I) = Assoc_82 (I)));

      MG.Decode (Auth_80, AP, STA, Answer);
      Check ("authentication answer", Answer.Kind = MG.Authentication_Answer and then Answer.Status = 0
             and then Answer.Sequence = MG.Authentication_Response_Sequence
             and then Answer.Algorithm = MG.Algorithm_Open_System);
      MG.Decode (Assoc_84, AP, STA, Answer);
      Check ("association answer", Answer.Kind = MG.Association_Answer and then Answer.Status = 0
             and then Answer.AID = 1);
      --  Not from the BSS being joined, not to us, or protected: nothing.
      MG.Decode (Assoc_84, STA, STA, Answer);
      Check ("other BSS ignored", Answer.Kind = MG.Other);
      MG.Decode (Assoc_84, AP, AP, Answer);
      Check ("other station ignored", Answer.Kind = MG.Other);
      declare
         P : MG.Octets := Assoc_84;
      begin
         P (MG.Control_Flags_At) := MG.Flag_Protected;
         MG.Decode (P, AP, STA, Answer);
         Check ("protected ignored", Answer.Kind = MG.Other);
      end;
      MG.Decode (To_MG (Hex ("c0003a01ffffffffffff000c4182b255000c4182b25500000300")), AP, STA, Answer);
      Check ("broadcast deauthentication", Answer.Kind = MG.Deauthenticated and then Answer.Status = 3);
      MG.Decode (Auth_80 (0 .. 20), AP, STA, Answer);
      Check ("short frame ignored", Answer.Kind = MG.Other);
   end Management_Tests;

   procedure MLME_Tests is
      Bounds : constant M.Limits := (Frame_Timeout => 200, Frame_Attempts => 3, Handshake_Timeout => 1_000);
      S : M.State;
      A : M.Actions;
      use type M.Phase;
      use type M.Failure;
      use type M.Action_Kind;

      function Has (Kind : M.Action_Kind) return Boolean is
        (for some I in 1 .. A.Count => A.Items (I) = Kind);

      procedure Go (Kind : M.Event_Kind; Now : M.Milliseconds; Success : Boolean := True;
                    Network : M.Security := M.Open) is
      begin
         M.Step (S, (Kind => Kind, Now => Now, Network => Network, Bounds => Bounds, Success => Success), A);
         Check ("invariant after " & Kind'Image, M.Invariant (S));
      end Go;
   begin
      --  Open network.
      S := M.Initial;
      Go (M.Join, 0);
      Check ("join sends authentication", S.Current = M.Authenticating and then Has (M.Send_Authentication));
      Go (M.Authentication_Answer, 10);
      Check ("then association", S.Current = M.Associating and then Has (M.Send_Association));
      Go (M.Association_Answer, 20);
      Check ("open: connected, port open", S.Current = M.Connected and then S.Port_Open
             and then Has (M.Open_Port) and then Has (M.Report_Connected));
      Go (M.Deauthentication, 30);
      Check ("deauth: idle, port closed", S.Current = M.Idle and then not S.Port_Open
             and then Has (M.Close_Port) and then S.Reason = M.Deauthenticated);

      --  WPA2-PSK: connected only through the handshake.
      Go (M.Join, 100, Network => M.WPA2_PSK);
      Go (M.Authentication_Answer, 110);
      Go (M.Association_Answer, 120);
      Check ("rsn: handshaking, port closed", S.Current = M.Handshaking and then not S.Port_Open
             and then Has (M.Start_Handshake));
      Go (M.Tick, 500);
      Check ("handshake within deadline", S.Current = M.Handshaking);
      Go (M.Handshake_Established, 600);
      Check ("rsn: connected", S.Current = M.Connected and then Has (M.Open_Port));
      Go (M.Handshake_Refused, 700);
      Check ("rekey failure: idle, closed", S.Current = M.Idle and then Has (M.Close_Port)
             and then Has (M.Stop_Handshake) and then S.Reason = M.Handshake_Failed);

      --  Handshake timeout.
      Go (M.Join, 1_000, Network => M.WPA2_PSK);
      Go (M.Authentication_Answer, 1_010);
      Go (M.Association_Answer, 1_020);
      Go (M.Tick, 2_020);
      Check ("handshake timeout", S.Current = M.Idle and then S.Reason = M.Handshake_Timeout);

      --  Authentication retries, then timeout.
      Go (M.Join, 3_000);
      Go (M.Tick, 3_100);
      Check ("no retry before deadline", A.Count = 0);
      Go (M.Tick, 3_200);
      Check ("retry 2", Has (M.Send_Authentication) and then S.Tries = 2);
      Go (M.Tick, 3_400);
      Check ("retry 3", Has (M.Send_Authentication) and then S.Tries = 3);
      Go (M.Tick, 3_600);
      Check ("authentication timeout", S.Current = M.Idle and then S.Reason = M.Authentication_Timeout);

      --  Refusals and stray events.
      Go (M.Association_Answer, 4_000);
      Check ("stray answer ignored", S.Current = M.Idle and then A.Count = 0);
      Go (M.Join, 4_000);
      Go (M.Authentication_Answer, 4_010, Success => False);
      Check ("authentication refused", S.Current = M.Idle and then S.Reason = M.Authentication_Refused);
      Go (M.Join, 5_000);
      Go (M.Authentication_Answer, 5_010);
      Go (M.Association_Answer, 5_020, Success => False);
      Check ("association refused", S.Current = M.Idle and then S.Reason = M.Association_Refused);
      Go (M.Join, 6_000);
      Go (M.Handshake_Established, 6_010);
      Check ("handshake event outside handshake ignored", S.Current = M.Authenticating);
      Go (M.Leave, 6_020);
      Check ("leave", S.Current = M.Idle and then S.Reason = M.Left);
   end MLME_Tests;

   procedure Data_Tests is
      package DT renames IEEE80211_Data;
      AP  : constant DT.Address := [16#00#, 16#0c#, 16#41#, 16#82#, 16#b2#, 16#55#];
      STA : constant DT.Address := [16#00#, 16#0d#, 16#93#, 16#82#, 16#36#, 16#3a#];
      Peer : constant DT.Address := [16#02#, 16#11#, 16#22#, 16#33#, 16#44#, 16#55#];
      Frame : DT.Octets (0 .. 99) := [others => 16#5A#];
      Item : DT.Received;
      use type DT.Refusal;
      use type DT.Address;
      use type DT.Octets;

      function H (S : String) return DT.Octets is
         O : constant E.Octets := Hex (S);
      begin
         return [for I in O'Range => O (I)];
      end H;

      --  Turn a built ToDS frame into the FromDS frame the AP would send.
      procedure Reflect (F : in out DT.Octets) is
      begin
         F (1) := (F (1) and not DT.Flag_To_DS) or DT.Flag_From_DS;
         for I in DT.Address'Range loop
            F (DT.Address_1_At + I) := STA (I);
            F (DT.Address_2_At + I) := AP (I);
            F (DT.Address_3_At + I) := Peer (I);
         end loop;
      end Reflect;
   begin
      --  Open link: header, SNAP, payload.
      DT.Build_Header (AP, STA, Peer, 16#0800#, 5, (Encrypted => False), Frame);
      Check ("data header", Frame (0) = DT.FC_Data and then Frame (1) = DT.Flag_To_DS
             and then Frame (DT.Sequence_At) = 16#50# and then Frame (DT.Sequence_At + 1) = 0
             and then Frame (24 .. 31) = H ("aaaa030000000800"));
      Reflect (Frame);
      DT.Decode (Frame (0 .. 59), AP, STA, False, Item);
      Check ("data decode", Item.Why = DT.None and then Item.Ether_Type = 16#0800#
             and then Item.Destination = STA and then Item.Source = Peer
             and then Item.Payload_First = 32 and then Item.Payload_Length = 28 and then not Item.Group);
      DT.Decode (Frame (0 .. 59), AP, STA, True, Item);
      Check ("cleartext refused on protected link", Item.Why = DT.Unprotected_On_Protected_Link);
      DT.Decode (Frame (0 .. 59), STA, STA, False, Item);
      Check ("other BSS refused", Item.Why = DT.Not_From_BSS);
      Frame (DT.Address_1_At) := 16#01#;   --  group destination
      DT.Decode (Frame (0 .. 59), AP, STA, False, Item);
      Check ("group accepted", Item.Why = DT.None and then Item.Group);
      Frame (DT.Address_1_At) := 16#02#;
      DT.Decode (Frame (0 .. 59), AP, STA, False, Item);
      Check ("other station refused", Item.Why = DT.Not_For_Us);

      --  Encrypted: CCMP header with the PN, MIC excluded on receive.
      DT.Build_Header (AP, STA, Peer, DT.EtherType_EAPOL, 7,
                       (Encrypted => True, PN => 16#0605_0403_0201#, Key => 1), Frame);
      Check ("ccmp header", Frame (1) = (DT.Flag_To_DS or DT.Flag_Protected)
             and then Frame (24 .. 31) = H ("0102006003040506")
             and then Frame (32 .. 39) = H ("aaaa03000000888e"));
      Reflect (Frame);
      DT.Decode (Frame (0 .. 59), AP, STA, True, Item);
      Check ("protected decode", Item.Why = DT.None and then Item.Encrypted
             and then Item.PN = 16#0605_0403_0201#
             and then Item.Key = 1 and then Item.Payload_First = 40
             and then Item.Payload_Length = 60 - 40 - DT.CCMP_MIC_Bytes);
      Frame (27) := 0;   --  ExtIV clear
      DT.Decode (Frame (0 .. 59), AP, STA, True, Item);
      Check ("bad CCMP header", Item.Why = DT.Bad_CCMP_Header);

      --  QoS data, A-MSDU, QoS null, 4-address, short.
      DT.Build_Header (AP, STA, Peer, 16#0800#, 1, (Encrypted => False), Frame);
      Reflect (Frame);
      Frame (0) := DT.FC_QoS_Data;
      Frame (24 .. 33) := H ("0500aaaa030000000800");
      DT.Decode (Frame (0 .. 59), AP, STA, False, Item);
      Check ("qos decode", Item.Why = DT.None and then Item.QoS and then Item.Traffic = 5
             and then Item.Payload_First = 34);
      Frame (24) := DT.QoS_AMSDU_Bit;
      DT.Decode (Frame (0 .. 59), AP, STA, False, Item);
      Check ("a-msdu refused", Item.Why = DT.Aggregate);
      Frame (0) := 16#C8#;
      DT.Decode (Frame (0 .. 59), AP, STA, False, Item);
      Check ("qos null refused", Item.Why = DT.Null_Data);
      Frame (0) := DT.FC_Data;
      Frame (1) := DT.Flag_To_DS or DT.Flag_From_DS;
      DT.Decode (Frame (0 .. 59), AP, STA, False, Item);
      Check ("4-address refused", Item.Why = DT.Wrong_Direction);
      DT.Decode (Frame (0 .. 20), AP, STA, False, Item);
      Check ("short refused", Item.Why = DT.Too_Short);
   end Data_Tests;

   procedure Driver_Contract_Tests is
      J : constant D.Join_Request :=
        (BSSID => [16#00#, 16#0c#, 16#41#, 16#82#, 16#b2#, 16#55#],
         On_Band => CuBit.Wifi_Scan.Band_5_GHz, Channel => 36);
      W : D.Words2 := D.Encode (J);
      K : D.Key_Install :=
        (Kind => D.Group, Index => 2, Suite => D.CCMP_128,
         Key => [for I in D.Key_Octets'Range => Unsigned_8 (16#A0# + I)], RSC => 16#1234_5678_9ABC#);
      KW : D.Words4 := D.Encode (K);
      use type D.Join_Request;
      use type D.Key_Install;
      use type CuBit.Net_Link.Hardware_Address;
   begin
      declare
         R : constant D.Decoded_Join := D.Decode (W);
      begin
         Check ("join round trip", R.Valid and then R.Item = J);
      end;
      W (1) := W (1) or 16#7#;   --  no such band
      Check ("join bad band", not D.Decode (W).Valid);
      W := D.Encode ((J with delta Channel => 14));
      Check ("join channel not on band", not D.Decode (W).Valid);
      W := D.Encode (J);
      W (0) := 0;
      Check ("join no BSSID", not D.Decode (W).Valid);
      declare
         R : constant D.Decoded_Key := D.Decode (KW);
      begin
         Check ("key round trip", R.Valid and then R.Item = K);
      end;
      KW (0) := KW (0) xor Shift_Left (1, D.Cipher_Shift);
      Check ("key unknown cipher", not D.Decode (KW).Valid);
      KW := D.Encode (K);
      KW (3) := 2 ** 48;
      Check ("key RSC too wide", not D.Decode (KW).Valid);
      K.Kind := D.Pairwise;
      K.Index := 0;
      Check ("pairwise key round trip", D.Decode (D.Encode (K)).Valid);
      Check ("label decode", (for all A in D.Operation => D.Operation_Of (D.Label (A)) = A));
      Check ("not a label", not D.Is_Operation (16#0490#) and then not D.Is_Command (16#0480#));
      Check ("command decode", (for all C in D.Command => D.Command_Of (D.Label (C)) = C));
      declare
         P : D.Port_State;
         Valid : Boolean;
         use type D.Port_State;
      begin
         D.Decode_Port (2, P, Valid);
         Check ("port open", Valid and then P = D.Open_Port);
         D.Decode_Port (1, P, Valid);
         Check ("port closed", Valid and then P = D.Closed);
         D.Decode_Port (3, P, Valid);
         Check ("port invalid", not Valid);
      end;
   end Driver_Contract_Tests;

   --  The driver's receive classification (Wifi_Station_Path) on recorded
   --  frame 87 of wpa-Induction.pcap: message 1 as the AP sent it, a
   --  unicast data frame (FromDS) to the station with the 802.1X port
   --  closed. Regression (W3 hardware run 1): message 1 must reach wifi.svc
   --  as the EAPOL PDU itself, indexed from 0.
   procedure Path_Tests is
      package SP renames Wifi_Station_Path;
      package DTx renames IEEE80211_Data;
      Frame_87 : constant E.Octets := Hex
        ("08022c00000d9382363a000c4182b255000c4182b255b0fcaaaa03000000888e" & M1_Hex);
      F : constant DTx.Octets := [for I in Frame_87'Range => Frame_87 (I)];
      L : SP.Link;
      V : SP.Verdict;
      M1 : constant E.Octets := Hex (M1_Hex);
      use type SP.Verdict_Kind;
      use type SP.Drop_Reason;
   begin
      L.Joined := True;
      L.BSSID := [for I in 0 .. 5 => Hex (AP_Hex) (I)];
      L.Own := [for I in 0 .. 5 => Hex (STA_Hex) (I)];
      SP.Classify (L, F, False, False, V);
      Check ("path: message 1 to wifi.svc with the port closed", V.Kind = SP.To_Wifi_EAPOL);
      if V.Kind = SP.To_Wifi_EAPOL then
         declare
            PDU : constant DTx.Octets := SP.EAPOL_PDU (F, V);
         begin
            Check ("path: the EAPOL PDU, from 0", PDU'First = 0 and then PDU'Length = M1'Length
                   and then (for all I in M1'Range => PDU (I) = M1 (I)));
         end;
      end if;
      --  The same frame to another station, or before the join.
      L.Own (5) := L.Own (5) xor 1;
      SP.Classify (L, F, False, False, V);
      Check ("path: unicast to another station dropped", V.Kind = SP.Drop and then V.Why = SP.Data_Refused);
      L.Own (5) := L.Own (5) xor 1;
      L.Joined := False;
      SP.Classify (L, F, False, False, V);
      Check ("path: nothing before a join", V.Kind = SP.Drop and then V.Why = SP.Not_Joined);
      --  IPv4 data: netstack only while the port is open.
      L.Joined := True;
      declare
         IP : constant E.Octets := Hex ("08022c00000d9382363a000c4182b255000c4182b255b0fcaaaa030000000800"
                                        & "450000140000000040000000c0a80001c0a80002");
         G : constant DTx.Octets := [for I in IP'Range => IP (I)];
      begin
         SP.Classify (L, G, False, False, V);
         Check ("path: data dropped while the port is closed", V.Kind = SP.Drop and then V.Why = SP.Port_Closed);
         L.Port_Open := True;
         SP.Classify (L, G, False, False, V);
         Check ("path: data to netstack once open", V.Kind = SP.To_Netstack);
         if V.Kind = SP.To_Netstack then
            declare
               Eth : constant DTx.Octets := SP.Ethernet (G, V);
            begin
               Check ("path: Ethernet frame", Eth'First = 0 and then Eth'Length = 14 + 20
                      and then Eth (12) = 16#08# and then Eth (13) = 0 and then Eth (14) = 16#45#
                      and then Eth (0) = 16#00# and then Eth (5) = 16#3a# and then Eth (11) = 16#55#);
            end;
         end if;
         --  A protected frame the hardware did not decrypt.
         declare
            P : DTx.Octets := G;
         begin
            P (1) := P (1) or DTx.Flag_Protected;
            SP.Classify (L, P, False, False, V);
            Check ("path: protected frame not decrypted dropped", V.Kind = SP.Drop);
         end;
      end;
      --  Management: the first beacon once, authentication always.
      L.Forward_Beacon := True;
      declare
         B : constant E.Octets := Hex ("80000000ffffffffffff000c4182b255000c4182b2550000000000000000000064000104");
         BB : constant DTx.Octets := [for I in B'Range => B (I)];
      begin
         SP.Classify (L, BB, False, False, V);
         Check ("path: first beacon forwarded", V.Kind = SP.To_Wifi_Management);
         SP.Classify (L, BB, False, False, V);
         Check ("path: later beacons not", V.Kind = SP.Drop and then V.Why = SP.Beacon_Already_Forwarded);
      end;
   end Path_Tests;

   procedure Replay_Tests is
      package R renames Wifi_Replay;
      C : R.Counters;
      OK : Boolean;
   begin
      R.Accept_Frame (C, 0, 5, OK);
      Check ("replay: nothing before a key", not OK);
      C := R.Start (10);
      R.Accept_Frame (C, 0, 10, OK);
      Check ("replay: the RSC itself refused", not OK);
      R.Accept_Frame (C, 0, 11, OK);
      Check ("replay: above the RSC accepted", OK);
      R.Accept_Frame (C, 0, 11, OK);
      Check ("replay: the same number refused", not OK);
      R.Accept_Frame (C, 5, 11, OK);
      Check ("replay: per traffic class", OK);
      R.Accept_Frame (C, R.Non_QoS, 3, OK);
      Check ("replay: non-QoS below the RSC refused", not OK);
      R.Accept_Frame (C, 0, 2 ** 48 - 1, OK);
      Check ("replay: the largest number", OK);
   end Replay_Tests;

   procedure Control_Tests is
      package C renames CuBit.Wifi_Control;
      S : C.SSID := (Length => 7, Octets => [others => 0]);
      Name : constant String := "Coherer";
      W : C.Words4;
      use type C.SSID;
   begin
      for I in Name'Range loop
         S.Octets (I) := Character'Pos (Name (I));
      end loop;
      W := C.Encode (S);
      Check ("ssid words", W (0) = 16#0072_6572_6568_6F43# and then W (1) = 0);
      Check ("ssid round trip", C.Decode (W).Valid and then C.Decode (W).Item = S);
      W := [others => 0];
      Check ("ssid empty refused", not C.Decode (W).Valid);
      W := [others => 16#4141_4141_4141_4141#];
      Check ("ssid 32 octets", C.Decode (W).Valid and then C.Decode (W).Item.Length = 32);
      W := [0 => 16#41#, 1 => 16#4100#, others => 0];
      Check ("ssid inner NUL kept", C.Decode (W).Valid and then C.Decode (W).Item.Length = 10);
      Check ("lengths word", C.Lengths_Word (7, 9) = 16#0907#);
   end Control_Tests;

   ---------------------------------------------------------------------------
   --  The station end to end (Wifi_Station) against a simulated AP: the
   --  test plays the AP with WiFi_Crypto directly (its own PRF, MICs and
   --  AES key wrap), so the station's frames are checked independently.
   ---------------------------------------------------------------------------
   procedure Flow_Tests is
      package ST renames Wifi_Station;
      package WSc renames CuBit.Wifi_Scan;
      package KW renames WiFi_Crypto.AES_Key_Wrap.AES128;
      AP_A  : constant CuBit.Net_Link.Hardware_Address := [16#00#, 16#0c#, 16#41#, 16#82#, 16#b2#, 16#55#];
      STA_A : constant CuBit.Net_Link.Hardware_Address := [16#00#, 16#0d#, 16#93#, 16#82#, 16#36#, 16#3a#];
      Bounds : constant M.Limits := (Frame_Timeout => 200, Frame_Attempts => 3, Handshake_Timeout => 2_000);
      S : ST.Station;
      L : ST.Outputs;
      OK : Boolean;
      use type ST.Output_Kind;
      use type CuBit.Wifi_Control.State;
      use type D.Port_State;
      use type D.Key_Kind;
      use type E.Octets;

      function Frame (O : E.Octets) return ST.Frame_Bytes is [for I in O'Range => O (I)];

      function Has (Kind : ST.Output_Kind) return Boolean is
        (for some I in 1 .. L.Count => L.Items (I).Kind = Kind);

      function Find (Kind : ST.Output_Kind) return ST.Output is
      begin
         for I in 1 .. L.Count loop
            if L.Items (I).Kind = Kind then
               return L.Items (I);
            end if;
         end loop;
         raise Program_Error with "no such output";
      end Find;

      Coherer : constant String := "436f6865726572";
      RSN_CCMP : constant String := "30140100000fac040100000fac040100000fac020000";

      function Beacon (Privacy : Boolean; With_RSN : Boolean) return E.Octets is
        (Hex ("80000000ffffffffffff000c4182b255000c4182b2550000"
              & "00000000000000006400" & (if Privacy then "1104" else "0104")
              & "0007" & Coherer & "010882848b962430486c" & "030106"
              & (if With_RSN then RSN_CCMP else "")));

      function Target (Is_RSN : Boolean) return WSc.BSS_Entry is
         T : WSc.BSS_Entry;
         N : constant E.Octets := Hex (Coherer);
      begin
         T.BSSID := AP_A;
         T.Length := N'Length;
         for I in N'Range loop
            T.SSID (I + 1) := N (I);
         end loop;
         T.On_Band := WSc.Band_2_4_GHz;
         T.Channel := 6;
         T.RSN := Is_RSN;
         return T;
      end Target;

      Auth_80 : constant String :=
        "b0003a01000d9382363a000c4182b255000c4182b25590fc000002000000dd06001018020004";
      Assoc_84 : constant String :=
        "10003a01000d9382363a000c4182b255000c4182b255a0fc1104000001c0010882848b962430486c";

      PMK_Bytes : constant E.Octets := Hex (PMK_Hex);
      PMK : constant H.PMK_Value := [for I in H.PMK_Value'Range => PMK_Bytes (I)];
      A_Nonce : constant E.Octets := Hex ("1111111111111111111111111111111111111111111111111111111111111111");
      S_Nonce : constant E.Nonce := [others => 16#22#];
      GTK : constant E.Octets := Hex ("a0a1a2a3a4a5a6a7a8a9aaabacadaeaf");
      KCK, KEK, TK : E.Octets (0 .. 15);

      --  The AP's PTK: PRF-384 over min/max of addresses and nonces
      --  (AP < STA and ANonce < SNonce here).
      procedure Derive is
         K : Byte_Seq (0 .. 31);
         Label : constant String := "Pairwise key expansion";
         A : Byte_Seq (0 .. Label'Length - 1);
         Data : constant E.Octets := Hex (AP_Hex & STA_Hex) & A_Nonce & E.Octets'(0 .. 31 => 16#22#);
         B : Byte_Seq (0 .. Data'Length - 1);
         P : Byte_Seq (0 .. 47);
      begin
         for I in K'Range loop
            K (I) := Byte (PMK (Natural (I)));
         end loop;
         for I in A'Range loop
            A (I) := Character'Pos (Label (Label'First + Natural (I)));
         end loop;
         for I in B'Range loop
            B (I) := Byte (Data (Natural (I)));
         end loop;
         W2.PRF (P, K, A, B);
         for I in 0 .. 15 loop
            KCK (I) := Unsigned_8 (P (N32 (I)));
            KEK (I) := Unsigned_8 (P (N32 (16 + I)));
            TK (I) := Unsigned_8 (P (N32 (32 + I)));
         end loop;
      end Derive;

      function Sealed (F : E.Octets) return E.Octets is
         R : E.Octets := F;
         V : constant E.MIC_Value := MIC (KCK, F);
      begin
         for I in V'Range loop
            R (E.MIC_At + I) := V (I);
         end loop;
         return R;
      end Sealed;

      function Hex2 (V : Natural) return String is
         Digits_Text : constant String := "0123456789abcdef";
      begin
         return Digits_Text (V / 16 + 1) & Digits_Text (V mod 16 + 1);
      end Hex2;

      function M1 return E.Octets is
        (Hex ("0203005f02008a00100000000000000001") & A_Nonce & E.Octets'(0 .. 63 => 0));

      function M3 (Replay : Natural) return E.Octets is
         Plain : constant E.Octets := Hex (RSN_CCMP & "dd16000fac010100") & GTK & Hex ("dd00");
         P : Byte_Seq (0 .. Plain'Length - 1);
         W : Byte_Seq (0 .. Plain'Length + 7);
         KS : Bytes_16;
         Body_Length : constant Natural := 95 + W'Length;
         F : E.Octets (0 .. 4 + Body_Length - 1);
         Head : constant E.Octets :=
           Hex ("020300" & Hex2 (Body_Length) & "0213ca0010" & "00000000000000" & Hex2 (Replay)) & A_Nonce;
      begin
         for I in P'Range loop
            P (I) := Byte (Plain (Natural (I)));
         end loop;
         for I in KS'Range loop
            KS (I) := Byte (KEK (Natural (I)));
         end loop;
         KW.Wrap (W, SPARKNaCl.AES.Key_Expansion (SPARKNaCl.AES.Construct (KS)), P);
         F := [others => 0];
         F (0 .. Head'Length - 1) := Head;
         F (E.Key_Data_Length_At) := 0;
         F (E.Key_Data_Length_At + 1) := Unsigned_8 (W'Length);
         for I in W'Range loop
            F (E.Key_Data_At + Natural (I)) := Unsigned_8 (W (I));
         end loop;
         return Sealed (F);
      end M3;
   begin
      --  Open network: join, beacon, authentication, association, port.
      ST.Reset (S, STA_A, Bounds, Beacon_Timeout => 1_000);
      ST.Join (S, Target (False), ST.Open, [others => 0], [others => 0], 0, L, OK);
      Check ("flow open: join", OK and then L.Count = 1 and then L.Items (1).Kind = ST.Do_Join
             and then L.Items (1).Join.Channel = 6);
      ST.Management (S, Frame (Beacon (False, False)), 10, L);
      Check ("flow open: beacon -> authentication", Has (ST.Send_Management)
             and then Find (ST.Send_Management).Frame (0) = MG.FC_Authentication);
      ST.Management (S, Frame (Hex (Auth_80)), 20, L);
      Check ("flow open: auth ok -> association request", Has (ST.Send_Management)
             and then Find (ST.Send_Management).Frame (0) = MG.FC_Association_Request);
      declare
         F : constant ST.Output := Find (ST.Send_Management);
      begin
         --  Capability: ESS and the AP's short slot (no short preamble), no Privacy.
         Check ("flow open: association capability", F.Frame (24) = 16#01# and then F.Frame (25) = 16#04#);
      end;
      ST.Management (S, Frame (Hex (Assoc_84)), 30, L);
      Check ("flow open: associated, port open", Has (ST.Do_Associated) and then Find (ST.Do_Associated).AID = 1
             and then Has (ST.Do_Port) and then Find (ST.Do_Port).Port = D.Open_Port
             and then ST.State_Of (S) = CuBit.Wifi_Control.Connected);
      ST.Management (S, Frame (Hex ("c0003a01000d9382363a000c4182b255000c4182b25500000700")), 40, L);
      Check ("flow open: deauth closes and leaves", Has (ST.Do_Port) and then Find (ST.Do_Port).Port = D.Closed
             and then Has (ST.Do_Leave) and then ST.State_Of (S) = CuBit.Wifi_Control.Idle);

      --  WPA2-PSK against the simulated AP.
      Derive;
      ST.Reset (S, STA_A, Bounds, Beacon_Timeout => 1_000);
      ST.Join (S, Target (True), ST.WPA2_PSK, PMK, S_Nonce, 0, L, OK);
      Check ("flow wpa2: join", OK);
      ST.Management (S, Frame (Beacon (True, True)), 10, L);
      ST.Management (S, Frame (Hex (Auth_80)), 20, L);
      declare
         F : constant ST.Output := Find (ST.Send_Management);
         RSN : constant E.Octets := Hex (RSN_CCMP);
         Last : constant Natural := F.Length - 1;
      begin
         Check ("flow wpa2: association carries CuBit's RSN element and Privacy",
                F.Frame (24) = 16#11#
                and then (for all I in RSN'Range => F.Frame (Last - RSN'Length + 1 + I) = RSN (I)));
      end;
      ST.Management (S, Frame (Hex (Assoc_84)), 30, L);
      Check ("flow wpa2: associated, port still closed", Has (ST.Do_Associated) and then not Has (ST.Do_Port)
             and then ST.State_Of (S) = CuBit.Wifi_Control.Handshaking);
      ST.EAPOL (S, Frame (M1), 40, L);
      Check ("flow wpa2: message 1 answered", Has (ST.Send_EAPOL));
      if Has (ST.Send_EAPOL) then
         declare
            F : constant ST.Output := Find (ST.Send_EAPOL);
            Reply : constant E.Octets := [for I in 0 .. F.Length - 1 => F.Frame (I)];
         begin
            Check ("flow wpa2: message 2 MIC under the AP's KCK", MIC_Field (Reply) = MIC (KCK, Reply));
         end;
      end if;
      ST.EAPOL (S, Frame (M3 (2)), 50, L);
      Check ("flow wpa2: message 4, keys, then port", L.Count = 4
             and then L.Items (1).Kind = ST.Send_EAPOL and then L.Items (2).Kind = ST.Do_Install_Key
             and then L.Items (3).Kind = ST.Do_Install_Key and then L.Items (4).Kind = ST.Do_Port
             and then L.Items (4).Port = D.Open_Port
             and then ST.State_Of (S) = CuBit.Wifi_Control.Connected);
      Check ("flow wpa2: pairwise TK", L.Items (2).Key.Kind = D.Pairwise
             and then (for all I in 0 .. 15 => L.Items (2).Key.Key (I) = TK (I)));
      Check ("flow wpa2: group GTK, index 1", L.Items (3).Key.Kind = D.Group and then L.Items (3).Key.Index = 1
             and then (for all I in 0 .. 15 => L.Items (3).Key.Key (I) = GTK (I)));
      declare
         F : constant ST.Output := L.Items (1);
         Reply : constant E.Octets := [for I in 0 .. F.Length - 1 => F.Frame (I)];
      begin
         Check ("flow wpa2: message 4 MIC", MIC_Field (Reply) = MIC (KCK, Reply));
      end;
      --  The AP retransmits message 3: message 4 again, nothing installed.
      ST.EAPOL (S, Frame (M3 (3)), 60, L);
      Check ("flow wpa2: retransmitted message 3 installs nothing", L.Count = 1
             and then L.Items (1).Kind = ST.Send_EAPOL);
      ST.Leave (S, 70, L);
      Check ("flow wpa2: leave closes the port", Has (ST.Do_Port) and then Find (ST.Do_Port).Port = D.Closed
             and then Has (ST.Do_Leave));

      --  A beacon whose RSN element is not CuBit's policy (TKIP group).
      ST.Reset (S, STA_A, Bounds, Beacon_Timeout => 1_000);
      ST.Join (S, Target (True), ST.WPA2_PSK, PMK, S_Nonce, 0, L, OK);
      ST.Management (S, Frame (Hex ("80000000ffffffffffff000c4182b255000c4182b2550000"
                                    & "000000000000000064001104" & "0007" & Coherer
                                    & "30140100000fac020100000fac040100000fac020000")), 10, L);
      Check ("flow: TKIP group not joinable", Has (ST.Do_Leave) and then not Has (ST.Send_Management));
      --  No beacon at all.
      ST.Join (S, Target (False), ST.Open, PMK, S_Nonce, 100, L, OK);
      ST.Tick (S, 1_200, L);
      Check ("flow: no beacon times out", Has (ST.Do_Leave) and then ST.State_Of (S) = CuBit.Wifi_Control.Idle);
   end Flow_Tests;

begin
   Handshake_Tests;
   Data_Tests;
   Management_Tests;
   MLME_Tests;
   Driver_Contract_Tests;
   Control_Tests;
   Replay_Tests;
   Path_Tests;
   Flow_Tests;
   Put_Line ("wifi-station:" & Passed'Image & " passed," & Failed'Image & " failed");
   if Failed > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Station_Tests;
