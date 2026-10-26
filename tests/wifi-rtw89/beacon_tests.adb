--  IEEE80211_Beacons: decoding constructed beacons and probe responses,
--  and refusing hostile ones. The differential test against an
--  independent Python decoder (reference.py) runs through Decode_Corpus.
pragma Ada_2022;
with Interfaces; use Interfaces;
with IEEE80211_Beacons; use IEEE80211_Beacons;
with Test_Support; use Test_Support;

procedure Beacon_Tests is

   Header : constant Frame_Bytes :=
     [16#80#, 16#00#, 0, 0,                                     --  beacon, duration
      16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#,           --  A1 broadcast
      16#02#, 16#11#, 16#22#, 16#33#, 16#44#, 16#55#,           --  A2 transmitter
      16#02#, 16#11#, 16#22#, 16#33#, 16#44#, 16#55#,           --  A3 BSSID
      16#10#, 16#00#,                                           --  sequence
      1, 2, 3, 4, 5, 6, 7, 8,                                   --  timestamp
      16#64#, 16#00#,                                           --  interval 100
      16#11#, 16#04#];                                          --  ESS | privacy | short slot

   function Join (A, B : Frame_Bytes) return Frame_Bytes is
      R : Frame_Bytes (0 .. A'Length + B'Length - 1);
   begin
      for I in 0 .. A'Length - 1 loop
         R (I) := A (A'First + I);
      end loop;
      for I in 0 .. B'Length - 1 loop
         R (A'Length + I) := B (B'First + I);
      end loop;
      return R;
   end Join;

   SSID_Home : constant Frame_Bytes := [0, 4, Character'Pos ('h'), Character'Pos ('o'),
                                        Character'Pos ('m'), Character'Pos ('e')];
   DS_6      : constant Frame_Bytes := [3, 1, 6];
   RSN_Elem  : constant Frame_Bytes := [48, 20, 1, 0, 0, 16#0F#, 16#AC#, 4, 1, 0, 0, 16#0F#, 16#AC#, 4,
                                        1, 0, 0, 16#0F#, 16#AC#, 2, 0, 0];
   Vendor    : constant Frame_Bytes := [221, 7, 0, 16#50#, 16#F2#, 2, 1, 1, 0];
   HT_Op_36  : constant Frame_Bytes (0 .. 23) := [61, 22, 36, others => 0];

   Info : Decoded;
   Result : Status;
begin
   --  A typical beacon.
   declare
      F : constant Frame_Bytes := Join (Join (Join (Join (Header, SSID_Home), DS_6), RSN_Elem), Vendor);
   begin
      Decode (F, Info, Result);
      Check (Result = Accepted, "beacon: accepted");
      Check (Info.Kind = Beacon and then Info.Length = 4 and then Info.SSID (1) = Character'Pos ('h')
             and then Info.SSID (5) = 0, "beacon: SSID, zero past it");
      Check (Info.BSSID = [16#02#, 16#11#, 16#22#, 16#33#, 16#44#, 16#55#], "beacon: BSSID from address 3");
      Check (Info.DS_Channel = 6 and then Channel_Of (Info, 1) = 6, "beacon: DS channel wins");
      Check (Info.Interval = 100 and then Info.Capability = 16#0411#, "beacon: fixed fields");
      Check (Info.RSN, "beacon: RSN present");
      Check (Info.Elements = 4 and then Info.Skipped = 1, "beacon: elements counted, vendor skipped");
   end;
   --  Probe response with HT operation only (5 GHz), hidden SSID.
   declare
      H : Frame_Bytes := Header;
   begin
      H (0) := Subtype_Probe_Response;
      Decode (Join (Join (H, [0, 0]), HT_Op_36), Info, Result);
      Check (Result = Accepted and then Info.Kind = Probe_Response and then Info.Has_SSID
             and then Info.Length = 0 and then Info.DS_Channel = No_Channel
             and then Channel_Of (Info, 40) = 36, "probe response: HT primary channel, hidden SSID");
   end;
   --  No elements at all.
   Decode (Header, Info, Result);
   Check (Result = Accepted and then not Info.Has_SSID and then Channel_Of (Info, 11) = 11,
          "beacon: no elements, channel as heard");
   --  Order bit: 4 more header octets.
   declare
      H : constant Frame_Bytes := Join (Header (0 .. 23), [1, 2, 3, 4]);
      F : Frame_Bytes := Join (Join (H, Header (24 .. 35)), DS_6);
   begin
      F (1) := Flag_Order;
      Decode (F, Info, Result);
      Check (Result = Accepted and then Info.DS_Channel = 6 and then Info.Interval = 100,
             "beacon: HT control shifts the body");
   end;
   --  Refusals.
   Decode (Header (0 .. 34), Info, Result);
   Check (Result = Too_Short, "refuse: one octet short of the fixed fields");
   Decode (Frame_Bytes'(0 .. -1 => 0), Info, Result);
   Check (Result = Too_Short, "refuse: empty");
   declare
      F : Frame_Bytes := Header;
   begin
      F (0) := 16#81#;
      Decode (F, Info, Result);
      Check (Result = Not_Version_0, "refuse: protocol version 1");
      F (0) := 16#88#;
      Decode (F, Info, Result);
      Check (Result = Not_Management, "refuse: data frame");
      F (0) := 16#40#;
      Decode (F, Info, Result);
      Check (Result = Not_Beacon, "refuse: probe request");
      F (0) := 16#80#;
      F (1) := Flag_Protected;
      Decode (F, Info, Result);
      Check (Result = Protected_Frame, "refuse: protected");
   end;
   Decode (Join (Header, [0, 5, 1, 2]), Info, Result);
   Check (Result = Element_Overrun, "refuse: element runs past the frame");
   Decode (Join (Header, [0]), Info, Result);
   Check (Result = Element_Overrun, "refuse: truncated element header");
   declare
      Long : constant Frame_Bytes (0 .. 34) := [0 => 0, 1 => 33, others => 16#41#];
   begin
      Decode (Join (Header, Long), Info, Result);
      Check (Result = SSID_Too_Long, "refuse: SSID of 33 octets");
   end;
   Decode (Join (Header, [3, 2, 6, 6]), Info, Result);
   Check (Result = DS_Params_Malformed, "refuse: DS parameter set of 2 octets");
   --  Repeated SSID: the first wins.
   Decode (Join (Join (Header, SSID_Home), [0, 1, Character'Pos ('x')]), Info, Result);
   Check (Result = Accepted and then Info.Length = 4 and then Info.Skipped = 1,
          "beacon: a second SSID element is skipped");
   --  A short RSN element is not RSN.
   Decode (Join (Header, [48, 1, 1]), Info, Result);
   Check (Result = Accepted and then not Info.RSN, "beacon: RSN shorter than its version is not RSN");
   --  The largest frame.
   declare
      Big : Frame_Bytes (0 .. Maximum_Frame - 1) := [others => 0];
   begin
      Big (0 .. 35) := Header;
      --  Elements of 255 octets of padding (ID 221) up to the end.
      declare
         P : Natural := 36;
      begin
         while P + 257 <= Maximum_Frame loop
            Big (P) := 221;
            Big (P + 1) := 255;
            P := P + 257;
         end loop;
         Big (P) := 221;
         Big (P + 1) := Unsigned_8 (Maximum_Frame - P - 2);
      end;
      Decode (Big, Info, Result);
      Check (Result = Accepted and then Info.Skipped = Info.Elements, "beacon: a full 4096-octet frame");
   end;
end Beacon_Tests;
