------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The typed calls a holder of the "wifi" service endpoint (the console's
--  (wifi.join), (wifi.leave), (wifi.status), (wifi.passphrase)) makes to
--  wifi.svc (NET-WIFI-001 W3/W4, docs/wifi.md "UI").
--
--  @description
--  - Join (call: SSID words 0 .. 3) -> REPLY_OK once the join has started
--    (its progress is Status), or REPLY_ERR with a Refusal. The network is
--    the strongest BSS with that SSID in the last scan that CuBit's policy
--    can join (open, or WPA2-PSK with CCMP). A WPA2 network's passphrase
--    is the Secret setting wifi.secret.<SSID in hex> (docs/wifi.md D5).
--  - Leave (call) -> REPLY_OK.
--  - Status (call) -> REPLY_OK; word 0 state [7:0], last failure [15:8]
--    (Failure position), band [23:16], channel [31:24]; word 1
--    the BSSID (address word, 0: none).
--  - Set_Passphrase (call: grant slot, grant generation, lengths word) ->
--    REPLY_OK once stored, or REPLY_ERR with a Refusal. The caller's page
--    holds the SSID octets, then the passphrase (8 .. 63 printable ASCII
--    characters, 802.11 J.4.1); wifi.svc reads it once, stores the
--    passphrase as the Secret setting and never logs it. The caller wipes
--    its page.
--
--  An SSID travels in four words, 8 octets a word, little endian; its
--  length is up to its last nonzero octet (an SSID ending in NUL octets
--  cannot be named here). Decoders are total; what they accept re-encodes
--  exactly (proved).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Wifi_Control with Pure, SPARK_Mode is

   type Operation is (Join, Leave, Status, Set_Passphrase);
   for Operation use (Join => 16#04A0#, Leave => 16#04A1#, Status => 16#04A2#, Set_Passphrase => 16#04A3#);

   function Label (Op : Operation) return Unsigned_32 is (Operation'Enum_Rep (Op));
   function Is_Operation (Value : Unsigned_32) return Boolean is
     (for some Op in Operation => Value = Label (Op));
   function Operation_Of (Value : Unsigned_32) return Operation is
     (if Value = Label (Join) then Join elsif Value = Label (Leave) then Leave
      elsif Value = Label (Status) then Status else Set_Passphrase)
   with Pre => Is_Operation (Value), Post => Label (Operation_Of'Result) = Value;

   ---------------------------------------------------------------------------
   --  SSIDs (9.4.2.2): 1 .. 32 octets.
   ---------------------------------------------------------------------------
   Maximum_SSID : constant := 32;
   subtype SSID_Length is Natural range 0 .. Maximum_SSID;
   type SSID_Octets is array (1 .. Maximum_SSID) of Unsigned_8;
   type SSID is record
      Length : SSID_Length := 0;
      Octets : SSID_Octets := [others => 0];
   end record;

   --  Nameable here: nonempty, ending in a nonzero octet, zero after it.
   function Normal (S : SSID) return Boolean is
     (S.Length >= 1 and then S.Octets (S.Length) /= 0
      and then (for all I in S.Length + 1 .. Maximum_SSID => S.Octets (I) = 0));

   SSID_Words : constant := 4;
   type Words4 is array (0 .. SSID_Words - 1) of Unsigned_64;
   Octets_Per_Word : constant := 8;

   function Encode (S : SSID) return Words4
   with Pre => Normal (S);

   type Decoded_SSID (Valid : Boolean := False) is record
      case Valid is
         when True  => Item : SSID;
         when False => null;
      end case;
   end record;

   function Decode (W : Words4) return Decoded_SSID
   with Post => (if Decode'Result.Valid then
                   Normal (Decode'Result.Item) and then Encode (Decode'Result.Item) = W);

   ---------------------------------------------------------------------------
   --  Set_Passphrase's lengths word: SSID length [7:0], passphrase
   --  length [15:8]. The page: SSID, then passphrase.
   ---------------------------------------------------------------------------
   subtype Passphrase_Length is Natural range 8 .. 63;
   Passphrase_Shift : constant := 8;
   Page_Bytes : constant := 4_096;

   function Lengths_Word (SSID_Bytes : SSID_Length; Passphrase_Bytes : Passphrase_Length)
     return Unsigned_64 is
     (Unsigned_64 (SSID_Bytes) or Shift_Left (Unsigned_64 (Passphrase_Bytes), Passphrase_Shift));

   --  Printable ASCII (J.4.1: 32 .. 126).
   First_Printable : constant := 32;
   Last_Printable  : constant := 126;

   ---------------------------------------------------------------------------
   --  Refusals (REPLY_ERR word 0).
   ---------------------------------------------------------------------------
   type Refusal is
     (Malformed, No_Radio, Not_Found, Not_Joinable, No_Passphrase, Busy, Not_Stored);
   for Refusal use
     (Malformed => 1, No_Radio => 2, Not_Found => 3, Not_Joinable => 4, No_Passphrase => 5,
      Busy => 6, Not_Stored => 7);

   ---------------------------------------------------------------------------
   --  Status word 0.
   ---------------------------------------------------------------------------
   type State is (No_Radio, Idle, Joining, Handshaking, Connected);
   for State use (No_Radio => 0, Idle => 1, Joining => 2, Handshaking => 3, Connected => 4);
   --  Why the last join ended (Status word 0 [15:8], the position).
   type Failure is
     (None, Authentication_Refused, Authentication_Timeout, Association_Refused,
      Association_Timeout, Handshake_Failed, Handshake_Timeout, Deauthenticated,
      Disassociated, Left);
   Failure_Shift : constant := 8;
   Band_Shift    : constant := 16;
   Channel_Shift : constant := 24;

end CuBit.Wifi_Control;
