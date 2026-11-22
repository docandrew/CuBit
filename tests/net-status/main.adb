------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  Hosted tests for the Deskbar's network and load items (UI-027).
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with CuBit.Net_Link;
with CuBit.Net_Management;
with CuBit.Wifi_Control;
with CuBit.Wifi_Scan;
with CuBit.Wifi_Report;
with CuBit.Net_Manager_Control;
with CuBit.Net_Manager_Report;
with CuBit.Canonical_Readers;
with CCL.Setting_Codecs.Net_Interface;
with Desktop_Net_Address;
with Desktop_Network_Model;
with Desktop_Address_Editor;
with Desktop_Load_Bars;
with Desktop_Network_Rows;
with Desktop_Status_Machines;
with Compositor_Requests;
with CuBit.Async_Requests;

procedure Main is
   package NL renames CuBit.Net_Link;
   package C renames CuBit.Wifi_Control;
   package WS renames CuBit.Wifi_Scan;
   package WR renames CuBit.Wifi_Report;
   package NMC renames CuBit.Net_Manager_Control;
   package NMR renames CuBit.Net_Manager_Report;
   package DNA renames Desktop_Net_Address;
   package M renames Desktop_Network_Model;
   package AE renames Desktop_Address_Editor;
   package LB renames Desktop_Load_Bars;
   package Codec renames CCL.Setting_Codecs.Net_Interface;
   use type C.State, C.Failure, WS.Signal_dBm, DNA.Problem, M.Phase, M.Request, M.Failure, M.Security;
   use type NMR.Policy, AE.Method, AE.Outcome, Codec.Net_Interface_Choice, Codec.Gateway_Choice;
   use type WR.Report, NMR.Report, M.Signal_Bars;
   use type Codec.Prefix_Length_Value, Codec.Route_Metric_Value;

   type Text is access constant String;
   B1 : aliased constant String := "10.0.2";
   B2 : aliased constant String := "10.0.2.256";
   B3 : aliased constant String := "010.0.0.1";
   B4 : aliased constant String := "1.2.3.4.5";
   B5 : aliased constant String := "";
   B6 : aliased constant String := " 1.2.3.4";
   B7 : aliased constant String := "1..2.3";
   B8 : aliased constant String := "1.2.3.";
   B9 : aliased constant String := ".1.2.3";
   B10 : aliased constant String := "1.2.3.4 ";
   B11 : aliased constant String := "a.b.c.d";
   B12 : aliased constant String := "1.2.3.-4";
   B13 : aliased constant String := "1234.1.1.1";
   Bad_Addresses : constant array (Positive range <>) of Text :=
     [B1'Access, B2'Access, B3'Access, B4'Access, B5'Access, B6'Access, B7'Access, B8'Access,
      B9'Access, B10'Access, B11'Access, B12'Access, B13'Access];
   P1 : aliased constant String := "10.0.2.15";
   P2 : aliased constant String := "10.0.2.15/";
   P3 : aliased constant String := "10.0.2.15/33";
   P4 : aliased constant String := "10.0.2.15/024";
   P5 : aliased constant String := "/24";
   P6 : aliased constant String := "10.0.2.15/2x";
   P7 : aliased constant String := "10.0.2.15/24/1";
   Bad_Prefixes : constant array (Positive range <>) of Text :=
     [P1'Access, P2'Access, P3'Access, P4'Access, P5'Access, P6'Access, P7'Access];

   Failures : Natural := 0;
   Checks : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   ---------------------------------------------------------------------------
   --  Addresses
   ---------------------------------------------------------------------------
   procedure Address_Tests is
      W, W2 : DNA.IPv4_Word;
      L : DNA.Prefix_Length;
      OK : Boolean;

      function Parses (T : String) return Boolean is
         V : DNA.IPv4_Word;
         R : Boolean;
      begin
         DNA.Parse_Address (T, V, R);
         return R;
      end Parses;
   begin
      DNA.Parse_Address ("10.0.2.15", W, OK);
      Check (OK and then W = DNA.Word_Of (10, 0, 2, 15)
             and then W = CuBit.Net_Management.Word_Of (CuBit.Net_Management.Address_Of (W)),
             "dotted quad is the netmgr word");
      Check (DNA.Image (W) = "10.0.2.15", "image round trip");
      Check (DNA.Image (DNA.Word_Of (255, 255, 255, 0)) = "255.255.255.0", "image of large octets");
      for T of Bad_Addresses loop
         Check (not Parses (T.all), "refuses """ & T.all & """");
      end loop;
      Check (Parses ("0.0.0.0") and then Parses ("255.255.255.255"), "extremes parse");

      DNA.Parse_Address_Prefix ("192.168.1.20/24", W, L, OK);
      Check (OK and then L = 24 and then W = DNA.Word_Of (192, 168, 1, 20), "address/prefix");
      for T of Bad_Prefixes loop
         DNA.Parse_Address_Prefix (T.all, W, L, OK);
         Check (not OK, "refuses prefix """ & T.all & """");
      end loop;

      DNA.Parse_DNS ("", W, W2, OK);
      Check (OK and then W = 0 and then W2 = 0, "empty DNS");
      DNA.Parse_DNS ("1.1.1.1, 8.8.8.8", W, W2, OK);
      Check (OK and then W = DNA.Word_Of (1, 1, 1, 1) and then W2 = DNA.Word_Of (8, 8, 8, 8), "two DNS");
      DNA.Parse_DNS ("9.9.9.9", W, W2, OK);
      Check (OK and then W = DNA.Word_Of (9, 9, 9, 9) and then W2 = 0, "one DNS");
      DNA.Parse_DNS (" 1.1.1.1  8.8.8.8 ", W, W2, OK);
      Check (OK and then W2 = DNA.Word_Of (8, 8, 8, 8), "blank-separated DNS");
      DNA.Parse_DNS ("1.1.1.1,8.8.8.8,9.9.9.9", W, W2, OK);
      Check (not OK and then W = 0 and then W2 = 0, "three DNS refused");
      DNA.Parse_DNS ("1.1.1.1,x", W, W2, OK);
      Check (not OK, "bad DNS refused");

      declare
         Net : constant DNA.IPv4_Word := DNA.Word_Of (10, 0, 2, 0);
         function Problem_Of (A, G : DNA.IPv4_Word; Len : DNA.Prefix_Length) return DNA.Problem is
           (DNA.Check ((Address => A, Length => Len, Gateway => G)));
      begin
         Check (Problem_Of (DNA.Word_Of (10, 0, 2, 15), DNA.Word_Of (10, 0, 2, 2), 24) = DNA.None, "good static");
         Check (Problem_Of (DNA.Word_Of (10, 0, 2, 15), 0, 24) = DNA.None, "no gateway is fine");
         Check (Problem_Of (Net, 0, 24) = DNA.Address_Is_Network, "network address");
         Check (Problem_Of (DNA.Word_Of (10, 0, 2, 255), 0, 24) = DNA.Address_Is_Broadcast, "broadcast");
         Check (Problem_Of (DNA.Word_Of (127, 0, 0, 1), 0, 8) = DNA.Address_Reserved, "loopback");
         Check (Problem_Of (DNA.Word_Of (224, 0, 0, 5), 0, 24) = DNA.Address_Reserved, "multicast");
         Check (Problem_Of (DNA.Word_Of (0, 1, 2, 3), 0, 24) = DNA.Address_Reserved, "this-network");
         Check (Problem_Of (DNA.Word_Of (10, 0, 2, 15), 0, 0) = DNA.Prefix_Too_Short, "/0");
         Check (Problem_Of (DNA.Word_Of (10, 0, 2, 15), DNA.Word_Of (10, 0, 3, 1), 24) = DNA.Gateway_Outside,
                "gateway outside");
         Check (Problem_Of (DNA.Word_Of (10, 0, 2, 15), DNA.Word_Of (10, 0, 3, 1), 16) = DNA.None,
                "gateway inside a /16");
         Check (Problem_Of (DNA.Word_Of (10, 0, 2, 15), DNA.Word_Of (10, 0, 2, 15), 24) = DNA.Gateway_Is_Address,
                "gateway is the address");
         Check (Problem_Of (DNA.Word_Of (10, 0, 2, 0), 0, 32) = DNA.None, "a /32 host route");
         Check (Problem_Of (DNA.Word_Of (10, 0, 2, 0), 0, 31) = DNA.None, "a /31 point-to-point");
         Check (DNA.Network_Of (DNA.Word_Of (10, 0, 2, 15), 24) = Net, "network of");
      end;

      --  The CCL Desktop writes is exactly what netmgr's codec reads.
      declare
         S : constant DNA.Static_Choice :=
           (Address => DNA.Word_Of (10, 0, 2, 15), Length => 24, Gateway => DNA.Word_Of (10, 0, 2, 2));
         Source : constant String := DNA.Static_Source (S);
         V : Codec.Net_Interface_Value;
         Decoded : Boolean;
      begin
         Check (Source = "(Net_Interface.Static (Static_Interface address => ""::ffff:10.0.2.15"" prefix => 120"
                         & " gateway => (Gateway.Via ""::ffff:10.0.2.2"") metric => 100))",
                "static source text: " & Source);
         Codec.Decode (Source, V, Decoded);
         Check (Decoded and then V.Choice = Codec.Static
                and then CuBit.Canonical_Readers.Image (V.Static_Payload.address) = "::ffff:10.0.2.15"
                and then V.Static_Payload.prefix = 120 and then V.Static_Payload.gateway.Choice = Codec.Via
                and then CuBit.Canonical_Readers.Image (V.Static_Payload.gateway.Via_Payload) = "::ffff:10.0.2.2"
                and then V.Static_Payload.metric = 100,
                "netmgr's codec reads the static source");
         Codec.Decode (DNA.Static_Source ((S.Address, 16, 0)), V, Decoded);
         Check (Decoded and then V.Static_Payload.gateway.Choice = Codec.None and then V.Static_Payload.prefix = 112,
                "no gateway reads as Gateway.None");
         Codec.Decode (DNA.Automatic_Source, V, Decoded);
         Check (Decoded and then V.Choice = Codec.Automatic, "automatic source");
         Codec.Decode (DNA.Disabled_Source, V, Decoded);
         Check (Decoded and then V.Choice = Codec.Disabled, "disabled source");
         Check (DNA.Setting_Line ("pci-00-10-0", DNA.Automatic_Source) =
                  "(setting ""net.interface.pci-00-10-0"" Net_Interface.Automatic)", "setting line");
      end;
   end Address_Tests;

   ---------------------------------------------------------------------------
   --  Reports on the wire
   ---------------------------------------------------------------------------
   function Entry_For (Name : String; Signal : WS.Signal_dBm; RSN : Boolean; Privacy : Boolean := False;
                       Last_Octet : Unsigned_8 := 1) return WS.BSS_Entry is
      E : WS.BSS_Entry;
   begin
      E.BSSID := [0 => 2, 1 => 0, 2 => 0, 3 => 0, 4 => 0, 5 => Last_Octet];
      E.Length := Name'Length;
      for I in 1 .. Name'Length loop
         E.SSID (I) := Character'Pos (Name (Name'First + I - 1));
      end loop;
      E.On_Band := WS.Band_2_4_GHz;
      E.Channel := 6;
      E.Signal := Signal;
      E.RSN := RSN;
      E.Capability := WS.ESS_Bit or (if Privacy or RSN then WS.Privacy_Bit else 0);
      E.Interval := 100;
      E.Heard := 3;
      return E;
   end Entry_For;


   function Sample_Report return WR.Report is
      R : WR.Report;
   begin
      R.Generation := 7;
      R.State := C.Idle;
      R.Scan.Summary := (Count => 5, Passes => 2, Total => 5, On_2_4 => 5, On_5 => 0, Channels => 3);
      R.Scan.Entries (1) := Entry_For ("Home", -70, True, Last_Octet => 1);
      R.Scan.Entries (2) := Entry_For ("Cafe", -55, False, Last_Octet => 2);
      R.Scan.Entries (3) := Entry_For ("Home", -48, True, Last_Octet => 3);
      R.Scan.Entries (4) := Entry_For ("", -40, False, Last_Octet => 4);
      R.Scan.Entries (5) := Entry_For ("Old", -60, False, Privacy => True, Last_Octet => 5);
      R.Remembered (3) := True;
      R.Scan_Generation := 12;
      return R;
   end Sample_Report;

   procedure Report_Tests is
      R : WR.Report := Sample_Report;
      W : WR.Words;
   begin
      Check (WR.Normal (R), "sample report is normal");
      W := WR.Encode (R);
      declare
         D : constant WR.Decoded_Report := WR.Decode (W);
      begin
         Check (D.Valid and then D.Item = R, "wifi report round trip");
      end;
      R.State := C.Connected;
      R.Target_Valid := True;
      R.Target := R.Scan.Entries (3);
      W := WR.Encode (R);
      declare
         D : constant WR.Decoded_Report := WR.Decode (W);
      begin
         Check (D.Valid and then D.Item = R, "wifi report with a target round trip");
      end;
      declare
         Bad : WR.Words := W;
      begin
         Bad (WR.State_Word) := Bad (WR.State_Word) or 2 ** 20;
         Check (not WR.Decode (Bad).Valid, "stray state bits refused");
         Bad := W;
         Bad (WR.State_Word) := (Bad (WR.State_Word) and not 16#FF#) or 9;
         Check (not WR.Decode (Bad).Valid, "unknown state refused");
         Bad := W;
         Bad (WR.Remembered_Word) := 2 ** 20;   --  past the scan's count
         Check (not WR.Decode (Bad).Valid, "remembered past the count refused");
         Bad := W;
         Bad (WR.State_Word) := Bad (WR.State_Word) and not WR.Target_Flag;
         Check (not WR.Decode (Bad).Valid, "target words without the flag refused");
         Check (not WR.Decode ([others => 0]).Valid or else WR.Decode ([others => 0]).Item.State = C.No_Radio,
                "an unwritten page is no radio at most");
      end;

      --  netmgr's report.
      declare
         N : NMR.Report;
         Name : NL.Interface_Name;
      begin
         Name.Text (1 .. 11) := "pci-00-10-0";
         Name.Length := 11;
         N.Generation := 3;
         N.Count := 2;
         N.Total := 2;
         N.DNS_1 := DNA.Word_Of (10, 0, 2, 3);
         N.DNS_From := NMR.Settings;
         N.Interfaces (1) := (Name => Name, Kind => NL.Ethernet, Condition => NL.Up, Setting => NMR.Static,
                              Settings => (Address => DNA.Word_Of (10, 0, 2, 15), Prefix => 24,
                                           Gateway => DNA.Word_Of (10, 0, 2, 2), Metric => 100),
                              MAC => [16#52#, 16#54#, 0, 16#12#, 16#34#, 16#56#], DHCP => NMR.Not_Used,
                              Applies => 1);
         Name.Text (1 .. 11) := "pci-02-00-0";
         N.Interfaces (2) := (Name => Name, Kind => NL.Wireless, Condition => NL.Down, Setting => NMR.Automatic,
                              Settings => (others => <>), MAC => [others => 1], DHCP => NMR.Selecting,
                              Applies => 0);
         Check (NMR.Normal (N), "net report is normal");
         declare
            W2 : constant NMR.Words := NMR.Encode (N);
            D : constant NMR.Decoded_Report := NMR.Decode (W2);
            Bad : NMR.Words := W2;
         begin
            Check (D.Valid and then D.Item = N, "net report round trip");
            Bad (1) := 3 or 2 ** 16;   --  three written, one in total
            Check (not NMR.Decode (Bad).Valid, "more records than interfaces refused");
            Bad := W2;
            Bad (NMR.Header_Words + 1) := Character'Pos (' ');   --  a blank in the name
            Check (not NMR.Decode (Bad).Valid, "blank name character refused");
            Bad := W2;
            Bad (NMR.Header_Words) := Bad (NMR.Header_Words) or 16#0900_0000#;   --  policy 9
            Check (not NMR.Decode (Bad).Valid, "unknown policy refused");
         end;
         Check (M.Wired_Present (True, N) and then not M.Wired_Present (False, N), "wired presence");
         Check (M.Interface_Of (N, NL.Wireless) = 2, "wireless interface found");
         N.Interfaces (1).Kind := NL.Wireless;
         Check (not M.Wired_Present (True, N), "no wired item with only Wi-Fi interfaces");
      end;

      --  Apply's lengths word.
      declare
         L : constant NMC.Decoded_Lengths := NMC.Decode_Lengths (NMC.Lengths_Word (11, 140));
      begin
         Check (L.Valid and then L.Name = 11 and then L.Source = 140, "lengths word round trip");
         Check (not NMC.Decode_Lengths (0).Valid, "empty lengths refused");
         Check (not NMC.Decode_Lengths (NMC.Lengths_Word (11, 140) or 2 ** 40).Valid, "stray bits refused");
         Check (not NMC.Decode_Lengths (17).Valid, "overlong name refused");
      end;
   end Report_Tests;

   ---------------------------------------------------------------------------
   --  The network list and joining
   ---------------------------------------------------------------------------
   procedure Model_Tests is
      R : WR.Report := Sample_Report;
      List : M.Network_List;
      Count : M.Network_Count;
      J : M.Join_State;
      Next : M.Request;
   begin
      Check (not M.Wifi_Present (True, (others => <>)), "no radio: no Wi-Fi item");
      Check (M.Wifi_Present (True, R) and then not M.Wifi_Present (False, R), "Wi-Fi item while bound");

      Check (M.Bars_Of (-40) = 4 and then M.Bars_Of (-50) = 4 and then M.Bars_Of (-51) = 3
             and then M.Bars_Of (-65) = 2 and then M.Bars_Of (-75) = 1 and then M.Bars_Of (-90) = 0,
             "signal bars");

      M.Build_List (R, List, Count);
      Check (Count = 3, "one row per named SSID, hidden left out");
      Check (M.Text_Of (List (1).Name) = "Home" and then List (1).Signal = -48 and then List (1).Remembered
             and then List (1).Kind = M.Secured, "strongest first, merged BSSes");
      Check (M.Text_Of (List (2).Name) = "Cafe" and then List (2).Kind = M.Open, "open network");
      Check (M.Text_Of (List (3).Name) = "Old" and then List (3).Kind = M.Unsupported, "WEP/WPA1 unsupported");

      R.State := C.Connected;
      R.Target_Valid := True;
      R.Target := R.Scan.Entries (2);
      M.Build_List (R, List, Count);
      Check (M.Text_Of (List (1).Name) = "Cafe" and then List (1).Current, "joined network first");

      --  Open: join at once, then wait for the address.
      M.Choose (J, List (1), Next);
      Check (Next = M.Nothing, "choosing the network already joined does nothing");
      J := (others => <>);
      R.State := C.Idle;
      R.Target_Valid := False;
      R.Target := WS.No_Entry;
      M.Build_List (R, List, Count);
      M.Choose (J, List (2), Next);   --  Cafe, open
      Check (Next = M.Join and then J.Current = M.Starting, "open network joins at once");
      M.Finished (J, M.Join, True, True, C.Malformed, Next);
      Check (J.Current = M.Joining and then M.Status_Text (J) = "Joining", "join under way");
      R.State := C.Connected;
      R.Target_Valid := True;
      R.Target := R.Scan.Entries (2);
      M.Observe (J, R, 0, 1_000, Next);
      Check (J.Current = M.Waiting_Address and then M.Status_Text (J) = "Getting an address", "associated");
      M.Tick (J, 1_000 + M.Address_Wait_Ms - 1);
      Check (J.Why = M.None, "still waiting for DHCP");
      M.Tick (J, 1_000 + M.Address_Wait_Ms);
      Check (M.Status_Text (J) = "No address (DHCP)", "no DHCP shown in the row");
      M.Observe (J, R, DNA.Word_Of (192, 168, 1, 7), 20_000, Next);
      Check (J.Current = M.Connected and then M.Status_Text (J) = "Connected", "a late lease connects");
      R.State := C.Idle;
      R.Failure := C.Deauthenticated;
      M.Observe (J, R, 0, 30_000, Next);
      Check (J.Current = M.Failed and then J.Why = M.Disconnected, "dropped later: disconnected");

      --  Secured, not remembered: passphrase, store, join; a wrong one is
      --  forgotten and asked for again.
      R := Sample_Report;
      R.Remembered := WR.Nothing_Remembered;
      M.Build_List (R, List, Count);
      J := (others => <>);
      M.Choose (J, List (1), Next);
      Check (Next = M.Nothing and then J.Current = M.Entering_Passphrase, "secured asks for its passphrase");
      M.Passphrase_Entered (J, 7, Next);
      Check (Next = M.Nothing and then J.Current = M.Entering_Passphrase, "seven characters are too few");
      M.Passphrase_Entered (J, 64, Next);
      Check (Next = M.Nothing, "64 characters are too many");
      M.Passphrase_Entered (J, 12, Next);
      Check (Next = M.Store_Passphrase and then J.Current = M.Storing and then J.Stored_Now, "stored first");
      M.Choose (J, List (2), Next);
      Check (Next = M.Nothing and then J.Current = M.Storing, "no second join while one is submitted");
      M.Finished (J, M.Store_Passphrase, True, True, C.Malformed, Next);
      Check (Next = M.Join and then J.Current = M.Starting, "then joined");
      M.Finished (J, M.Join, True, True, C.Malformed, Next);
      R.State := C.Handshaking;
      R.Target_Valid := True;
      R.Target := R.Scan.Entries (3);
      M.Observe (J, R, 0, 0, Next);
      Check (J.Current = M.Joining and then J.Seen_Handshake, "handshaking");
      R.State := C.Idle;
      R.Failure := C.Handshake_Timeout;
      R.Target_Valid := False;
      R.Target := WS.No_Entry;
      M.Observe (J, R, 0, 0, Next);
      Check (Next = M.Forget and then J.Current = M.Entering_Passphrase
             and then M.Status_Text (J) = "Wrong passphrase", "wrong passphrase forgotten, asked again");

      --  Remembered: joined without asking.
      R := Sample_Report;
      M.Build_List (R, List, Count);
      J := (others => <>);
      M.Choose (J, List (1), Next);
      Check (Next = M.Join, "remembered network joins at once");
      M.Finished (J, M.Join, True, False, C.Not_Found, Next);
      Check (J.Current = M.Failed and then M.Status_Text (J) = "Out of range", "not found: out of range");
      M.Cancel (J);
      Check (J.Current = M.Idle, "cancel clears a failure");
      M.Choose (J, List (1), Next);
      M.Finished (J, M.Join, True, False, C.No_Passphrase, Next);
      Check (J.Current = M.Entering_Passphrase, "no stored passphrase after all: ask");
      M.Choose (J, List (1), Next);
      M.Finished (J, M.Join, False, False, C.Malformed, Next);
      Check (J.Current = M.Failed and then J.Why = M.Unavailable, "wifi.svc gone");

      --  Unsupported networks are never joined.
      J := (others => <>);
      M.Choose (J, List (3), Next);
      Check (Next = M.Nothing and then J.Current = M.Failed and then J.Why = M.Not_Supported, "WEP refused");

      --  The adapter going away fails the join.
      M.Choose (J, List (2), Next);
      M.Finished (J, M.Join, True, True, C.Malformed, Next);
      R.State := C.No_Radio;
      M.Observe (J, R, 0, 0, Next);
      Check (J.Current = M.Failed and then J.Why = M.Unavailable, "radio gone");

      --  Failure mapping.
      Check (M.Failure_From (C.Association_Timeout, False) = M.Out_Of_Range
             and then M.Failure_From (C.Authentication_Refused, False) = M.Refused
             and then M.Failure_From (C.Disassociated, True) = M.Wrong_Passphrase
             and then M.Failure_From (C.Disassociated, False) = M.Disconnected, "failure mapping");
   end Model_Tests;

   ---------------------------------------------------------------------------
   --  Fields, editor, load
   ---------------------------------------------------------------------------

   procedure Editor_Tests is
      E : AE.Editor;
      S : NMR.Interface_Status;
      Submit : Boolean;
      Source : AE.Source_Text;
      D1, D2 : Unsigned_32;
      Changed : Boolean;
      P : DNA.Problem;
   begin
      S.Name.Text (1 .. 11) := "pci-00-10-0";
      S.Name.Length := 11;
      S.Setting := NMR.Automatic;
      S.DHCP := NMR.Bound;
      S.Settings := (Address => DNA.Word_Of (10, 0, 2, 15), Prefix => 24, Gateway => DNA.Word_Of (10, 0, 2, 2),
                     Metric => 200);
      AE.Open_For (E, S, DNA.Word_Of (10, 0, 2, 3), 0);
      Check (E.Open and then E.Chosen = AE.Automatic
             and then AE.Text (E.Fields (AE.Address_Field)) = "10.0.2.15/24"
             and then AE.Text (E.Fields (AE.Gateway_Field)) = "10.0.2.2"
             and then AE.Text (E.Fields (AE.DNS_Field)) = "10.0.2.3", "editor filled from what is in force");
      Check (AE.Shown_CCL (E) = "(setting ""net.interface.pci-00-10-0"" Net_Interface.Automatic)", "DHCP CCL");
      AE.Set_Field (E, AE.Address_Field, "10.0.2.159/24");
      Check (AE.Text (E.Fields (AE.Address_Field)) = "10.0.2.15/24", "no address typing while DHCP");
      AE.Choose (E, AE.Static);
      AE.Set_Field (E, AE.Address_Field, "10.0.2.15/2x");
      Check (AE.Text (E.Fields (AE.Address_Field)) = "10.0.2.15/24", "letters refused");
      AE.Set_Field (E, AE.Address_Field, "10.0.2.15/33");
      AE.Start_Apply (E, Submit);
      Check (not Submit and then E.Problem = DNA.Address_Malformed and then AE.Shown_CCL (E) = "",
             "/33 is refused before applying");
      AE.Set_Field (E, AE.Address_Field, "10.0.2.15/24");
      AE.Set_Field (E, AE.Gateway_Field, "10.0.2.1");
      AE.Set_Field (E, AE.DNS_Field, "10.0.2.3, 1.1.1.1");
      AE.Plan (E, Source, D1, D2, Changed, P);
      Check (P = DNA.None and then Changed and then D2 = DNA.Word_Of (1, 1, 1, 1)
             and then AE.Image (Source) = DNA.Static_Source ((DNA.Word_Of (10, 0, 2, 15), 24, DNA.Word_Of (10, 0, 2, 1))),
             "plan: static source and new DNS");
      AE.Start_Apply (E, Submit);
      Check (Submit and then E.Result = AE.Applying, "applying");
      AE.Set_Field (E, AE.DNS_Field, "10.0.2.3, 1.1.1.5");
      Check (AE.Text (E.Fields (AE.DNS_Field)) = "10.0.2.3, 1.1.1.1", "no typing while applying");
      AE.Finished (E, True);
      Check (E.Result = AE.Applied and then E.Can_Revert and then E.In_Force_Method = AE.Static, "applied");
      AE.Plan (E, Source, D1, D2, Changed, P);
      Check (not Changed, "DNS in force after applying");
      AE.Start_Revert (E, Submit);
      Check (Submit and then E.Chosen = AE.Automatic and then E.Result = AE.Reverting
             and then AE.Text (E.Fields (AE.DNS_Field)) = "10.0.2.3", "revert restores the fields");
      AE.Plan (E, Source, D1, D2, Changed, P);
      Check (Changed and then AE.Image (Source) = DNA.Automatic_Source, "revert submits DHCP and the old DNS");
      AE.Finished (E, True);
      Check (E.Result = AE.Reverted and then not E.Can_Revert, "reverted");
      AE.Start_Revert (E, Submit);
      Check (not Submit, "nothing more to revert");
      AE.Choose (E, AE.Static);
      AE.Start_Apply (E, Submit);
      AE.Finished (E, False);
      Check (E.Result = AE.Refused and then AE.Result_Text (E) = "netmgr refused it", "refusal shown");
   end Editor_Tests;

   --  The panel's Network rows: QEMU's single virtio-net shows one wired
   --  row and no Wi-Fi rows at all.
   procedure Row_Tests is
      package NR renames Desktop_Network_Rows;
      use type NR.Row_Kind, NR.Row;
      N : NMR.Report;
      L : NR.Layout;
   begin
      N.Count := 1;
      N.Total := 1;
      N.Interfaces (1).Name.Text (1 .. 11) := "pci-00-10-0";
      N.Interfaces (1).Name.Length := 11;
      N.Interfaces (1).Kind := NL.Ethernet;
      L := NR.Build (N, True, M.Wifi_Present (True, (others => <>)), 0, 0, 0);
      Check (L.Count = 1 and then L.Rows (1) = (NR.Wired_Row, 1), "QEMU: one wired row, no Wi-Fi rows");
      Check (NR.Build (N, False, False, 0, 0, 0).Count = 0, "no report: no rows");
      L := NR.Build (N, True, False, 0, 0, 1);
      Check (L.Count = 1 + NR.Editor_Rows and then L.Rows (2).Kind = NR.Method_Row
             and then L.Rows (L.Count).Kind = NR.Action_Row and then L.Rows (L.Count).Index = 1,
             "editor rows under their interface");
      Check (NR.Total_Height (L) = NR.Editor_Rows * NR.Row_Height + NR.CCL_Height, "row heights");
      N.Count := 2;
      N.Total := 2;
      N.Interfaces (2) := N.Interfaces (1);
      N.Interfaces (2).Kind := NL.Wireless;
      L := NR.Build (N, True, True, 9, 3, 2);
      Check (L.Rows (1) = (NR.Wired_Row, 1) and then L.Rows (2) = (NR.Wifi_Row, 2)
             and then L.Rows (3).Kind = NR.Method_Row and then L.Rows (3).Index = 2,
             "Wi-Fi row and its editor");
      Check (L.Count = 2 + NR.Editor_Rows + NR.Visible_Networks + 1, "networks capped, one passphrase row");
      Check (L.Rows (2 + NR.Editor_Rows + 4) = (NR.Passphrase_Row, 3), "passphrase under its network");
      L := NR.Build (N, True, True, 2, 0, 0);
      Check (L.Count = 4 and then L.Rows (3) = (NR.Network_Row, 1), "two networks listed");
   end Row_Tests;

   --  Status items recover from failed requests (regression: the
   --  Compositor_Requests tracker the clock and volume used stayed Faulted
   --  after any undelivered completion, so the clock stopped refreshing
   --  and a queued volume setting was never sent).
   procedure Machine_Tests is
      package SM renames Desktop_Status_Machines;
      package CR renames Compositor_Requests;
      package AR renames CuBit.Async_Requests;
      use type SM.Clock_State, SM.Clock_Action, SM.Audio_Action_Kind, SM.Audio_Phase, SM.Setting,
        SM.Loan_State, SM.Loan_Action, SM.Watch_Action, SM.Watch_Phase, SM.Switch_Phase, AR.Phase;
      Clock : SM.Clock_State := SM.Idle;
      CA : SM.Clock_Action;
      Audio : SM.Audio_State;
      AA : SM.Audio_Action;
      Loan : SM.Loan_State := SM.Free;
      LA : SM.Loan_Action;
      W : SM.Watch_State;
      WA : SM.Watch_Action;
      Old : CR.State;
      Sequence, Token : CR.ID := 0;
      Accepted : Boolean;
      T : AR.Tracker;
   begin
      --  The old tracker: an undelivered completion is a dead end.
      CR.Allocate (Sequence, Token);
      CR.Begin_Request (Old, Token, Accepted);
      CR.Complete (Old, Token, False);
      Check (CR.Faulted (Old) and then not CR.Available (Old), "old tracker: stuck after a failed completion");
      --  The tracker now used returns to Idle after any receipt.
      AR.Reserve (T, 5, Accepted);
      AR.Submitted (T, True);
      AR.Capture (T, 5, True, Accepted);
      AR.Release (T);
      Check (AR.State (T) = AR.Idle and then AR.Can_Reserve (T, 6), "async tracker: free again after a receipt");

      SM.Step (Clock, SM.Due, CA);
      Check (CA = SM.Submit_Read, "clock reads when due");
      SM.Step (Clock, SM.Due, CA);
      Check (CA = SM.None, "one clock read at a time");
      SM.Step (Clock, SM.Done, CA);   --  not delivered
      SM.Step (Clock, SM.Due, CA);
      Check (CA = SM.Submit_Read, "clock reads again after a failed read");

      SM.Step (Audio, (Kind => SM.Due), AA);
      Check (AA.Kind = SM.Submit_Get, "volume read");
      SM.Step (Audio, (Kind => SM.Set, Value => (40, False)), AA);
      Check (AA.Kind = SM.None and then Audio.Queued, "a setting waits for the read");
      SM.Step (Audio, (Kind => SM.Set, Value => (45, True)), AA);
      Check (Audio.Wanted = (45, True), "the newest setting wins");
      SM.Step (Audio, (Kind => SM.Done, Result => SM.Not_Delivered), AA);
      Check (AA.Kind = SM.None and then Audio.Queued and then Audio.Phase = SM.Idle,
             "a failed read keeps the queued setting");
      SM.Step (Audio, (Kind => SM.Due), AA);
      Check (AA.Kind = SM.Submit_Set and then AA.Value = (45, True), "queued setting sent when due");
      SM.Step (Audio, (Kind => SM.Done, Result => SM.Not_Delivered), AA);
      Check (Audio.Queued and then Audio.Wanted = (45, True), "an undelivered setting is queued again");
      SM.Step (Audio, (Kind => SM.Due), AA);
      Check (AA.Kind = SM.Submit_Set and then AA.Value = (45, True), "and sent again");
      SM.Step (Audio, (Kind => SM.Set, Value => (50, False)), AA);
      SM.Step (Audio, (Kind => SM.Done, Result => SM.Answered), AA);
      Check (AA.Kind = SM.Submit_Set and then AA.Value = (50, False), "the next setting follows a confirmed one");
      SM.Step (Audio, (Kind => SM.Done, Result => SM.Answered), AA);
      Check (AA.Kind = SM.None and then not SM.Pending (Audio), "settled");

      SM.Step (Loan, SM.Lend_Done, LA);
      SM.Step (Loan, SM.Request_Over, LA);
      Check (Loan = SM.Retiring and then LA = SM.Revoke, "a lent page is revoked after its request");
      SM.Step (Loan, SM.Lend_Done, LA);
      Check (Loan = SM.Retiring, "not lent again before it retired");
      SM.Step (Loan, SM.Retired, LA);
      Check (Loan = SM.Free, "free once retired");

      SM.Step (W, (Kind => SM.Tick, Now => 0), WA);
      Check (WA = SM.Lend_And_Submit and then W.Loan = SM.Lent, "watch lends and submits");
      SM.Step (W, (Kind => SM.Done, Now => 10, Result => SM.Answered), WA);
      Check (WA = SM.Submit and then W.Shown, "a report shows the item and re-submits");
      SM.Step (W, (Kind => SM.Done, Now => 20, Result => SM.Not_Delivered), WA);
      Check (WA = SM.Revoke and then not W.Shown and then W.Phase = SM.Waiting and then W.Loan = SM.Retiring,
             "service gone: item hidden, page revoked");
      SM.Step (W, (Kind => SM.Tick, Now => 20 + SM.Retry_Ms), WA);
      Check (WA = SM.None, "no new page before the old one retired");
      SM.Step (W, (Kind => SM.Retired, Now => 20 + SM.Retry_Ms), WA);
      SM.Step (W, (Kind => SM.Tick, Now => 20 + SM.Retry_Ms), WA);
      Check (WA = SM.Lend_And_Submit, "watched again on a fresh page");
      SM.Step (W, (Kind => SM.Lend_Failed, Now => 30), WA);
      Check (W.Loan = SM.Free and then W.Phase = SM.Waiting, "a failed lend waits");

      --  procmgr's "Allow app replacement" switch: off until confirmed,
      --  one request at a time, the newest flip wins, a refusal shows
      --  procmgr's state again.
      declare
         S : SM.Switch_State;
         A : SM.Switch_Action;
      begin
         Check (not SM.Shown (S) and then not S.Confirmed, "switch: off at boot");
         SM.Step (S, (Kind => SM.Flip, Value => False), A);
         Check (not A.Submit, "switch: flipping to what is confirmed sends nothing");
         SM.Step (S, (Kind => SM.Flip, Value => True), A);
         Check (A.Submit and then A.Value and then SM.Shown (S) and then not S.Confirmed,
                "switch: a flip is sent and shown, not yet confirmed");
         SM.Step (S, (Kind => SM.Flip, Value => False), A);
         Check (not A.Submit and then not SM.Shown (S), "switch: a flip while one is in flight waits");
         SM.Step (S, (Kind => SM.Done, Result => SM.Answered), A);
         Check (S.Confirmed and then A.Submit and then not A.Value,
                "switch: confirmed on, then the queued off is sent");
         SM.Step (S, (Kind => SM.Done, Result => SM.Refused), A);
         Check (S.Confirmed and then SM.Shown (S) and then not A.Submit and then S.Phase = SM.Idle,
                "switch: refused, it shows procmgr's state (on) again");
         SM.Step (S, (Kind => SM.Flip, Value => False), A);
         SM.Step (S, (Kind => SM.Done, Result => SM.Not_Delivered), A);
         Check (S.Confirmed and then SM.Shown (S), "switch: undelivered, it shows procmgr's state");
         SM.Step (S, (Kind => SM.Flip, Value => False), A);
         SM.Step (S, (Kind => SM.Done, Result => SM.Answered), A);
         Check (not S.Confirmed and then not SM.Shown (S) and then not A.Submit, "switch: off again");
         SM.Step (S, (Kind => SM.Done, Result => SM.Answered), A);
         Check (not S.Confirmed and then not A.Submit, "switch: a stray completion changes nothing");
      end;
   end Machine_Tests;

   procedure Load_Tests is
   begin
      Check (LB.Used_Percent (0, 0) = 0 and then LB.Used_Percent (100, 200) = 0, "nonsense totals");
      Check (LB.Used_Percent (8 * 2 ** 30, 2 * 2 ** 30) = 75, "75 percent used");
      Check (LB.Used_Percent (Unsigned_64'Last, 0) = 100, "huge total");
      Check (LB.Filled (20, 0) = 0 and then LB.Filled (20, 1) = 1 and then LB.Filled (20, 50) = 10
             and then LB.Filled (20, 100) = 20, "bar fill");
   end Load_Tests;
begin
   Address_Tests;
   Report_Tests;
   Model_Tests;
   Editor_Tests;
   Row_Tests;
   Machine_Tests;
   Load_Tests;
   Put_Line ("net-status:" & Natural'Image (Checks) & " checks," & Natural'Image (Failures) & " failed");
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;
