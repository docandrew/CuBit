------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A wireless driver's passive scan result as typed data (NET-WIFI-001 W1,
--  docs/wifi.md "Scan"; docs/net-link.md): the networks (BSSes) it heard,
--  and the block of the frame grant's header page that carries them from
--  the driver to netstack (CuBit.Frame_Rings.Scan_At), which netstack then
--  copies, unchanged, into a page an inspector lends it
--  (CuBit.Net_Management.Scan_Results).
--
--  The scan block is a level value like the link block: the driver alone
--  writes it under the same seqlock (CuBit.Net_Link.Begin_Write /
--  End_Write / Stable), and a reader takes it only between two equal, even
--  generation reads. Nothing is sent per change.
--
--  Every decoder here is total: words a writer did not form with Encode
--  decode as Valid => False. Proved (tests/wifi-rtw89, level 2): no
--  run-time errors; whatever a decoder accepts re-encodes to exactly the
--  words it was given. Tested (hosted): Decode (Encode (X)) = X for random
--  entries and summaries.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Frame_Rings;
with CuBit.Net_Link;

package CuBit.Wifi_Scan with Pure, SPARK_Mode is
   package NL renames CuBit.Net_Link;

   ---------------------------------------------------------------------------
   --  One network as heard: a beacon or probe response, decoded.
   ---------------------------------------------------------------------------
   type Band is (Band_2_4_GHz, Band_5_GHz);
   for Band use (Band_2_4_GHz => 1, Band_5_GHz => 2);

   --  IEEE 802.11-2020 Annex E channel numbers: 2.4 GHz 1 .. 14, 5 GHz
   --  32 .. 177 (the 5 GHz band's operating-class range).
   subtype Channel_Number is Unsigned_8 range 1 .. 177;
   subtype Channel_2_4 is Channel_Number range 1 .. 14;
   subtype Channel_5 is Channel_Number range 32 .. 177;

   function Valid_Channel (B : Band; C : Channel_Number) return Boolean is
     (case B is
        when Band_2_4_GHz => C in Channel_2_4,
        when Band_5_GHz   => C in Channel_5);

   --  Received signal strength in dBm (a receiver reports at most 0 dBm).
   type Signal_dBm is range -128 .. 0;

   --  The Capability Information field (IEEE 802.11-2020 9.4.1.4), as sent.
   subtype Capability_Information is Unsigned_16;
   ESS_Bit                 : constant Capability_Information := 2 ** 0;
   IBSS_Bit                : constant Capability_Information := 2 ** 1;
   Privacy_Bit             : constant Capability_Information := 2 ** 4;
   Short_Preamble_Bit      : constant Capability_Information := 2 ** 5;
   Spectrum_Management_Bit : constant Capability_Information := 2 ** 8;
   Short_Slot_Time_Bit     : constant Capability_Information := 2 ** 10;

   --  Time units (1024 us) between beacons (9.4.1.3).
   subtype Beacon_Interval_TU is Unsigned_16;

   --  How many beacons or probe responses of this BSS a scan pass heard.
   subtype Hearing_Count is Unsigned_16;

   type BSS_Entry is record
      BSSID      : NL.Hardware_Address := NL.No_Address;
      SSID       : NL.SSID_Bytes := [others => 0];   --  bytes past Length zero
      Length     : NL.SSID_Length := 0;             --  0: a hidden network
      On_Band    : Band := Band_2_4_GHz;
      Channel    : Channel_Number := 1;
      Signal     : Signal_dBm := Signal_dBm'First;  --  the strongest heard
      Capability : Capability_Information := 0;
      Interval   : Beacon_Interval_TU := 0;
      RSN        : Boolean := False;                --  an RSN element (48) was present
      Heard      : Hearing_Count := 0;
   end record;

   function Normal (E : BSS_Entry) return Boolean is
     (Valid_Channel (E.On_Band, E.Channel)
      and then (for all I in NL.SSID_Index => (if I > E.Length then E.SSID (I) = 0)));

   function Privacy (E : BSS_Entry) return Boolean is ((E.Capability and Privacy_Bit) /= 0);

   ---------------------------------------------------------------------------
   --  An entry's words (32-bit, little endian):
   --  0 = BSSID octets 0..3; 1 = octets 4..5 (0..15), channel (16..23),
   --  band (24..31); 2 = signal as a two's-complement octet (0..7), SSID
   --  length (8..15), flags (16..23: bit 0 RSN), zero (24..31);
   --  3 = capability (0..15), beacon interval (16..31); 4 .. 11 = SSID, four
   --  octets a word; 12 = beacons heard (0..15), zero (16..31).
   ---------------------------------------------------------------------------
   Entry_Words : constant := 13;
   subtype Entry_Word_Index is Natural range 0 .. Entry_Words - 1;
   type Entry_Words_Array is array (Entry_Word_Index) of Unsigned_32;

   BSSID_Low_Word   : constant Entry_Word_Index := 0;
   BSSID_High_Word  : constant Entry_Word_Index := 1;
   Signal_Word      : constant Entry_Word_Index := 2;
   Capability_Word  : constant Entry_Word_Index := 3;
   SSID_First_Word  : constant Entry_Word_Index := 4;
   Heard_Word       : constant Entry_Word_Index := 12;
   Octets_Per_Word  : constant := 4;
   pragma Compile_Time_Error
     (SSID_First_Word + NL.SSID_Octets / Octets_Per_Word /= Heard_Word,
      "the SSID words must lie between the capability and heard words");

   Channel_Shift  : constant := 16;
   Band_Shift     : constant := 24;
   Length_Shift   : constant := 8;
   Flags_Shift    : constant := 16;
   Interval_Shift : constant := 16;
   Byte_Mask      : constant Unsigned_32 := 16#FF#;
   Half_Mask      : constant Unsigned_32 := 16#FFFF#;
   RSN_Flag       : constant Unsigned_32 := 2 ** 0;
   Signal_Modulus : constant := 256;   --  a two's-complement octet

   function Encode (E : BSS_Entry) return Entry_Words_Array
   with Pre => Normal (E);

   type Decoded_Entry (Valid : Boolean := False) is record
      case Valid is
         when True  => Item : BSS_Entry;
         when False => null;
      end case;
   end record;

   function Decode (W : Entry_Words_Array) return Decoded_Entry
   with Post => (if Decode'Result.Valid then
                   Normal (Decode'Result.Item) and then
                   Encode (Decode'Result.Item) = W);

   ---------------------------------------------------------------------------
   --  The scan's summary: what one complete pass over the channel plan
   --  heard (Total may exceed the entries published: the strongest are
   --  kept), and how many passes completed.
   ---------------------------------------------------------------------------
   Maximum_Entries : constant := 32;
   subtype Entry_Count is Natural range 0 .. Maximum_Entries;
   subtype Entry_Index is Positive range 1 .. Maximum_Entries;

   subtype Network_Count is Natural range 0 .. 255;
   subtype Channel_Count is Natural range 0 .. 255;
   subtype Pass_Count is Unsigned_32;

   type Scan_Summary is record
      Count     : Entry_Count := 0;      --  entries published
      Passes    : Pass_Count := 0;       --  complete passes so far
      Total     : Network_Count := 0;    --  BSSes heard in the last pass
      On_2_4    : Network_Count := 0;
      On_5      : Network_Count := 0;
      Channels  : Channel_Count := 0;    --  channels listened to in the pass
   end record;

   function Normal (S : Scan_Summary) return Boolean is
     (S.On_2_4 + S.On_5 = S.Total and then S.Count <= S.Total);

   --  Summary words: 0 = count (0..7), zero (8..31); 1 = passes;
   --  2 = total (0..7), on 2.4 GHz (8..15), on 5 GHz (16..23), channels
   --  (24..31).
   Summary_Words : constant := 3;
   type Summary_Words_Array is array (0 .. Summary_Words - 1) of Unsigned_32;
   On_2_4_Shift   : constant := 8;
   On_5_Shift     : constant := 16;
   Channels_Shift : constant := 24;

   function Encode (S : Scan_Summary) return Summary_Words_Array
   with Pre => Normal (S);

   type Decoded_Summary (Valid : Boolean := False) is record
      case Valid is
         when True  => Item : Scan_Summary;
         when False => null;
      end case;
   end record;

   function Decode (W : Summary_Words_Array) return Decoded_Summary
   with Post => (if Decode'Result.Valid then
                   Normal (Decode'Result.Item) and then
                   Encode (Decode'Result.Item) = W);

   ---------------------------------------------------------------------------
   --  The block's layout, in 32-bit words from its start: 0 = generation
   --  (CuBit.Net_Link's seqlock), 1 .. 3 = the summary, then
   --  Maximum_Entries entries of Entry_Words words; entries past Count are
   --  all zero. The same layout fills an inspector's lent page from its
   --  start (CuBit.Net_Management.Scan_Results).
   ---------------------------------------------------------------------------
   Word_Bytes          : constant := 4;
   Generation_Word     : constant := 0;
   Summary_First_Word  : constant := 1;
   Entries_First_Word  : constant := Summary_First_Word + Summary_Words;
   Block_Words         : constant := Entries_First_Word + Maximum_Entries * Entry_Words;
   Block_Bytes         : constant := Block_Words * Word_Bytes;
   subtype Block_Word_Index is Natural range 0 .. Block_Words - 1;
   pragma Compile_Time_Error
     (Block_Bytes /= CuBit.Frame_Rings.Scan_Bytes,
      "the scan block's size must match its place in the frame grant");

   --  Word index of entry I's word K.
   function Entry_Word (I : Entry_Index; K : Entry_Word_Index) return Block_Word_Index is
     (Entries_First_Word + (I - 1) * Entry_Words + K);

   --  A whole block, as read under the seqlock (generation word included).
   type Block is array (Block_Word_Index) of Unsigned_32;

   type Entry_Array is array (Entry_Index) of BSS_Entry;
   No_Entry : constant BSS_Entry := (others => <>);

   --  A whole scan result: the summary and its first Count entries
   --  (the rest are No_Entry).
   type Scan_Result is record
      Summary : Scan_Summary;
      Entries : Entry_Array := [others => No_Entry];
   end record;

   function Normal (R : Scan_Result) return Boolean is
     (Normal (R.Summary)
      and then (for all I in Entry_Index =>
                  (if I <= R.Summary.Count then Normal (R.Entries (I))
                   else R.Entries (I) = No_Entry)));

   --  The block for R with the given generation word.
   function Encode (R : Scan_Result; Generation : NL.Generation) return Block
   with Pre => Normal (R),
        Post => Encode'Result (Generation_Word) = Generation;

   type Decoded_Result (Valid : Boolean := False) is record
      case Valid is
         when True  => Item : Scan_Result;
         when False => null;
      end case;
   end record;

   --  B formed by Encode (any generation); anything else is refused.
   function Decode (B : Block) return Decoded_Result
   with Post => (if Decode'Result.Valid then
                   Normal (Decode'Result.Item) and then
                   Encode (Decode'Result.Item, B (Generation_Word)) = B);

end CuBit.Wifi_Scan;
