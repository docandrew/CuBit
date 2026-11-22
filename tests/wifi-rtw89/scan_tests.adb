--  CuBit.Wifi_Scan (the scan block's encoding) and Wifi_BSS_Table (the
--  scan's BSS list): round trips, refusals, merging, eviction, ordering.
pragma Ada_2022;
with Interfaces; use Interfaces;
with Ada.Numerics.Discrete_Random;
with CuBit.Net_Link;
with CuBit.Wifi_Scan;
with Wifi_BSS_Table;
with Wifi_Channel_Plan;
with Test_Support; use Test_Support;

procedure Scan_Tests is
   package WS renames CuBit.Wifi_Scan;
   package NL renames CuBit.Net_Link;
   use type WS.BSS_Entry;
   use type WS.Signal_dBm;
   use type WS.Band;
   use type WS.Scan_Result;
   use type WS.Entry_Words_Array;

   package Random_Words is new Ada.Numerics.Discrete_Random (Unsigned_32);
   Gen : Random_Words.Generator;

   function Entry_Of (Seed : Unsigned_32) return WS.BSS_Entry is
      E : WS.BSS_Entry;
      Length : constant Natural := Natural (Seed mod 33);
   begin
      E.BSSID := [for I in NL.Octet_Index => Unsigned_8 ((Seed / 7 + Unsigned_32 (I) * 31) mod 256)];
      E.Length := Length;
      for I in 1 .. Length loop
         E.SSID (I) := Unsigned_8 ((Seed + Unsigned_32 (I) * 13) mod 256);
      end loop;
      if Seed mod 2 = 0 then
         E.On_Band := WS.Band_2_4_GHz;
         E.Channel := WS.Channel_Number (1 + Seed mod 14);
      else
         E.On_Band := WS.Band_5_GHz;
         E.Channel := WS.Channel_Number (32 + Seed mod 146);
      end if;
      E.Signal := WS.Signal_dBm (-Integer (Seed mod 129));
      E.Capability := Unsigned_16 (Seed mod 65_536);
      E.Interval := Unsigned_16 ((Seed / 3) mod 65_536);
      E.RSN := Seed mod 3 = 0;
      E.Heard := Unsigned_16 ((Seed / 5) mod 65_536);
      return E;
   end Entry_Of;

   Round_Trips : Natural := 0;
   Exact : Natural := 0;
   Accepted : Natural := 0;
begin
   Random_Words.Reset (Gen, 2026);
   for I in 1 .. 100_000 loop
      declare
         E : constant WS.BSS_Entry := Entry_Of (Random_Words.Random (Gen));
         D : constant WS.Decoded_Entry := WS.Decode (WS.Encode (E));
      begin
         if D.Valid and then D.Item = E then
            Round_Trips := Round_Trips + 1;
         end if;
      end;
   end loop;
   Check (Round_Trips = 100_000, "entry: Decode (Encode (E)) = E for 100000 random entries");

   --  Random words: whatever decodes re-encodes exactly.
   for I in 1 .. 100_000 loop
      declare
         W : WS.Entry_Words_Array;
         D : WS.Decoded_Entry;
      begin
         for K in W'Range loop
            W (K) := Random_Words.Random (Gen);
         end loop;
         --  Bias half the samples towards plausible field values.
         if I mod 2 = 0 then
            W (WS.BSSID_High_Word) := (W (WS.BSSID_High_Word) and 16#0000_FFFF#) or 16#0106_0000#;
            W (WS.Signal_Word) := (W (WS.Signal_Word) and 16#0001_00FF#) or 16#0000_0300#;
            for K in WS.SSID_First_Word + 1 .. WS.Heard_Word - 1 loop
               W (K) := 0;
            end loop;
            W (WS.SSID_First_Word) := W (WS.SSID_First_Word) and 16#00FF_FFFF#;
            W (WS.Heard_Word) := W (WS.Heard_Word) and 16#FFFF#;
         end if;
         D := WS.Decode (W);
         if D.Valid then
            Accepted := Accepted + 1;
            if WS.Encode (D.Item) = W then
               Exact := Exact + 1;
            end if;
         end if;
      end;
   end loop;
   Check (Accepted = Exact, "entry: every accepted word set re-encodes exactly");
   Check (Accepted > 0, "entry: plausible random words are accepted");

   --  Refusals.
   declare
      W : WS.Entry_Words_Array := WS.Encode (Entry_Of (10));
   begin
      W (WS.BSSID_High_Word) := (W (WS.BSSID_High_Word) and 16#00FF_FFFF#) or 16#0300_0000#;
      Check (not WS.Decode (W).Valid, "entry: band 3 refused");
      W := WS.Encode (Entry_Of (10));
      W (WS.BSSID_High_Word) := (W (WS.BSSID_High_Word) and 16#FF00_FFFF#) or 16#0024_0000#;
      Check (not WS.Decode (W).Valid, "entry: channel 36 on 2.4 GHz refused");
      W := WS.Encode (Entry_Of (10));
      W (WS.Signal_Word) := (W (WS.Signal_Word) and 16#FFFF_FF00#) or 16#05#;
      Check (not WS.Decode (W).Valid, "entry: positive signal refused");
      W := WS.Encode (Entry_Of (10));
      W (WS.Signal_Word) := W (WS.Signal_Word) or 16#0002_0000#;
      Check (not WS.Decode (W).Valid, "entry: unknown flag refused");
      W := WS.Encode (Entry_Of (10));
      W (WS.Signal_Word) := (W (WS.Signal_Word) and 16#FFFF_00FF#) or 16#2100#;
      Check (not WS.Decode (W).Valid, "entry: SSID length 33 refused");
   end;
   declare
      E : WS.BSS_Entry := Entry_Of (4);   --  length 4
      W : WS.Entry_Words_Array;
   begin
      E.Signal := 0;
      Check (WS.Decode (WS.Encode (E)).Valid, "entry: 0 dBm round trips");
      E.Signal := -128;
      Check (WS.Decode (WS.Encode (E)).Valid and then WS.Decode (WS.Encode (E)).Item.Signal = -128,
             "entry: -128 dBm round trips");
      W := WS.Encode (E);
      W (WS.SSID_First_Word + 2) := 1;
      Check (not WS.Decode (W).Valid, "entry: a byte past the SSID length refused");
   end;

   --  The table.
   declare
      T : Wifi_BSS_Table.Table := Wifi_BSS_Table.Empty;
      A : WS.BSS_Entry := Entry_Of (6);
      B : WS.BSS_Entry := Entry_Of (8);
      R : WS.Scan_Result;
   begin
      A.Signal := -70;
      Wifi_BSS_Table.Observe (T, A);
      Check (T.Count = 1 and then T.Entries (1).Heard = 1, "table: first observation inserted");
      A.Signal := -40;
      A.Channel := 6;
      A.Length := 0;
      A.SSID := [others => 0];
      Wifi_BSS_Table.Observe (T, A);
      Check (T.Count = 1 and then T.Entries (1).Signal = -40 and then T.Entries (1).Channel = 6
             and then T.Entries (1).Length = Entry_Of (6).Length and then T.Entries (1).Heard = 2,
             "table: merge keeps strongest signal and its channel, keeps a known SSID");
      B.Signal := -90;
      Wifi_BSS_Table.Observe (T, B);
      Check (T.Count = 2 and then Wifi_BSS_Table.Valid (T), "table: second BSS");
      R := Wifi_BSS_Table.Publish (T, 7, 22);
      Check (R.Summary.Count = 2 and then R.Summary.Total = 2 and then R.Summary.Passes = 7
             and then R.Entries (1).Signal = -40 and then R.Entries (2).Signal = -90
             and then WS.Normal (R), "table: publish sorted, summary");
      Check (WS.Decode (WS.Encode (R, 4)).Valid
             and then WS.Decode (WS.Encode (R, 4)).Item = R, "block: Decode (Encode (R)) = R");
   end;
   declare
      T : Wifi_BSS_Table.Table := Wifi_BSS_Table.Empty;
      E : WS.BSS_Entry;
      R : WS.Scan_Result;
      Sorted : Boolean := True;
   begin
      for I in 1 .. Wifi_BSS_Table.Capacity + 20 loop
         E := Entry_Of (Unsigned_32 (I) * 1_000_003);
         E.BSSID := [0 => 16#02#, 1 => Unsigned_8 (I mod 256), 2 => Unsigned_8 (I / 256), others => 0];
         E.Signal := WS.Signal_dBm (-Integer (I mod 100));
         Wifi_BSS_Table.Observe (T, E);
      end loop;
      Check (T.Count = Wifi_BSS_Table.Capacity and then Wifi_BSS_Table.Valid (T)
             and then T.Evicted = 20, "table: full table stays distinct, counts evictions");
      R := Wifi_BSS_Table.Publish (T, 1, 22);
      for I in 1 .. R.Summary.Count - 1 loop
         Sorted := Sorted and then R.Entries (I).Signal >= R.Entries (I + 1).Signal;
      end loop;
      Check (R.Summary.Count = WS.Maximum_Entries and then Sorted
             and then R.Summary.On_2_4 + R.Summary.On_5 = R.Summary.Total,
             "table: publish keeps the strongest 32, descending");
      Check (WS.Decode (WS.Encode (R, 2)).Valid, "block: a full block decodes");
      declare
         Bad : WS.Block := WS.Encode (R, 2);
      begin
         Bad (WS.Summary_First_Word) := 33;
         Check (not WS.Decode (Bad).Valid, "block: count above 32 refused");
         Bad := WS.Encode (WS.Scan_Result'(Summary => (Count => 1, Total => 1, On_2_4 => 1, others => <>),
                                           Entries => [1 => R.Entries (1), others => WS.No_Entry]), 2);
         Bad (WS.Entry_Word (2, 0)) := 1;
         Check (not WS.Decode (Bad).Valid, "block: words past the count must be zero");
      end;
   end;

   Check (Wifi_Channel_Plan.Plan_Valid, "plan: every channel valid for its band");
   Check (Wifi_Channel_Plan.Frequency (Wifi_Channel_Plan.World_Passive (1)) = 2412
          and then Wifi_Channel_Plan.Frequency (Wifi_Channel_Plan.World_Passive (14)) = 5180
          and then Wifi_Channel_Plan.Frequency (Wifi_Channel_Plan.World_Passive (22)) = 5825,
          "plan: centre frequencies");
   Check ((for all P of Wifi_Channel_Plan.World_Passive =>
             P.On_Band = WS.Band_2_4_GHz or else P.Channel not in 52 .. 144),
          "plan: no DFS channel");
end Scan_Tests;
