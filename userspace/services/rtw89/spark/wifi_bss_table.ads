------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The passive scan's BSS list (NET-WIFI-001 W1): one entry per BSSID
--  heard during a pass over the channel plan, kept by signal strength, and
--  the published result (CuBit.Wifi_Scan) with its summary.
--
--  @description
--  Proved (tests/wifi-rtw89, level 2): no run-time errors; the table never
--  holds two entries with one BSSID; every entry is Normal; Count never
--  exceeds Capacity; when full, a new BSS replaces the weakest entry only
--  if it is stronger; the published result is Normal, holds the strongest
--  entries in descending signal order, and its per-band counts add up to
--  its total.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Wifi_Scan;
with CuBit.Net_Link;

package Wifi_BSS_Table with Pure, SPARK_Mode is
   package WS renames CuBit.Wifi_Scan;
   package NL renames CuBit.Net_Link;
   use type WS.Signal_dBm;
   use type WS.Band;
   use type NL.Hardware_Address;
   use type WS.BSS_Entry;

   --  How many distinct BSSes one pass remembers (the summary counts up to
   --  WS.Network_Count'Last).
   Capacity : constant := 96;
   pragma Compile_Time_Error (Capacity > WS.Network_Count'Last, "capacity exceeds the summary's count");
   subtype Count_Type is Natural range 0 .. Capacity;
   subtype Index is Positive range 1 .. Capacity;
   type Entry_Array is array (Index) of WS.BSS_Entry;

   type Table is record
      Count   : Count_Type := 0;
      Entries : Entry_Array := [others => WS.No_Entry];
      Evicted : Natural := 0;   --  weaker BSSes dropped when full (saturating)
   end record;

   function Distinct (T : Table) return Boolean is
     (for all I in 1 .. T.Count =>
        (for all J in 1 .. T.Count => (if I /= J then T.Entries (I).BSSID /= T.Entries (J).BSSID)));

   function Valid (T : Table) return Boolean is
     ((for all I in 1 .. T.Count => WS.Normal (T.Entries (I))) and then Distinct (T));

   Empty : constant Table := (others => <>);

   function Contains (T : Table; BSSID : NL.Hardware_Address) return Boolean is
     (for some I in 1 .. T.Count => T.Entries (I).BSSID = BSSID);

   --  One beacon or probe response heard: insert it, or merge it into the
   --  entry with its BSSID (the strongest signal and its channel win; a
   --  non-empty SSID replaces an empty one; Heard counts, saturating).
   procedure Observe (T : in out Table; Heard : WS.BSS_Entry)
   with Pre  => Valid (T) and then WS.Normal (Heard),
        Post => Valid (T) and then T.Count >= T.Count'Old
                and then (if Contains (T'Old, Heard.BSSID) then T.Count = T.Count'Old)
                and then (if T.Count'Old < Capacity then Contains (T, Heard.BSSID));

   --  The published result: the strongest WS.Maximum_Entries entries in
   --  descending signal order, the totals per band, and the pass count.
   function Publish (T : Table; Passes : WS.Pass_Count; Channels : WS.Channel_Count)
     return WS.Scan_Result
   with Pre  => Valid (T),
        Post => WS.Normal (Publish'Result)
                and then Publish'Result.Summary.Total = T.Count
                and then Publish'Result.Summary.Count = Natural'Min (T.Count, WS.Maximum_Entries)
                and then (for all I in 1 .. Publish'Result.Summary.Count - 1 =>
                            Publish'Result.Entries (I).Signal >= Publish'Result.Entries (I + 1).Signal);

end Wifi_BSS_Table;
