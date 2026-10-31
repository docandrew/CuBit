------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The typed contract between a Wi-Fi driver (rtw89.drv) and wifi.svc
--  (NET-WIFI-001 W3/W4, docs/wifi.md "Process structure"): the driver's
--  messages to wifi.svc, the commands wifi.svc answers them with, the
--  frame grant between them, and key installation.
--
--  @description
--  Split by authority: wifi.svc holds the passphrase, PMK, KCK and KEK;
--  the driver holds only the temporal keys installed into the hardware
--  (TK, GTK) and never anything that derives them.
--
--  The IPC graph stays a DAG exactly as between a network driver and
--  netstack (docs/net-link.md): the driver calls wifi.svc (Attach), rings
--  it one-way (Receive_Doorbell) and parks a Command_Wait with it;
--  wifi.svc never calls or submits to the driver. A command is the answer
--  to the parked wait (its label and words); the driver carries it out,
--  reports with Command_Done (one-way), and parks the next wait. Frames
--  wifi.svc sends wait in the grant's transmit ring and are drained
--  before the command that comes with them, so a message 4 queued before
--  Install_Key leaves unencrypted.
--
--  Driver -> wifi.svc (labels 0x0480..):
--  - Attach (call: address word) -> REPLY_OK, handle word, grant word.
--  - Receive_Doorbell (one-way: handle): frames or a scan report wait in
--    the grant.
--  - Command_Wait (submitted, parked: handle) -> a command.
--  - Command_Done (one-way: handle, command label, Outcome).
--  Commands (answer labels 0x0490..):
--  - Join (BSSID word, band/channel word): tune, filter for that BSS,
--    stop scanning; forward its next beacon or probe response.
--  - Associated (AID and beacon interval word): port 0 to
--    infrastructure, the AP's entries.
--  - Install_Key (kind/index/cipher word, two key words, RSC word).
--  - Port (Closed | Open_Port): the 802.1X controlled port; Open makes
--    the driver report link Up to netstack and pass data frames.
--  - Leave: keys erased, link Down, back to scanning.
--  - Transmit: frames wait in the transmit ring (no Command_Done).
--
--  Decoders are total; what they accept re-encodes exactly (proved).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Net_Link;
with CuBit.Slot_Rings;
with CuBit.Wifi_Scan;

package CuBit.Wifi_Driver with Pure, SPARK_Mode is
   package NL renames CuBit.Net_Link;
   package WS renames CuBit.Wifi_Scan;
   use type NL.Hardware_Address;

   type Operation is (Attach, Receive_Doorbell, Command_Wait, Command_Done);
   for Operation use
     (Attach => 16#0480#, Receive_Doorbell => 16#0481#, Command_Wait => 16#0482#,
      Command_Done => 16#0483#);

   function Label (Op : Operation) return Unsigned_32 is (Operation'Enum_Rep (Op));
   function Is_Operation (Value : Unsigned_32) return Boolean is
     (for some Op in Operation => Value = Label (Op));
   function Operation_Of (Value : Unsigned_32) return Operation
   with Pre => Is_Operation (Value), Post => Label (Operation_Of'Result) = Value;

   function Request_Words (Op : Operation) return Natural is
     (case Op is
        when Attach | Receive_Doorbell | Command_Wait => 1,
        when Command_Done => 3);

   type Command is (Join, Associated, Install_Key, Port, Leave, Transmit);
   for Command use
     (Join => 16#0490#, Associated => 16#0491#, Install_Key => 16#0492#, Port => 16#0493#,
      Leave => 16#0494#, Transmit => 16#0495#);

   function Label (C : Command) return Unsigned_32 is (Command'Enum_Rep (C));
   function Is_Command (Value : Unsigned_32) return Boolean is
     (for some C in Command => Value = Label (C));
   function Command_Of (Value : Unsigned_32) return Command
   with Pre => Is_Command (Value), Post => Label (Command_Of'Result) = Value;

   function Command_Words (C : Command) return Natural is
     (case C is
        when Join => 2,
        when Associated | Port => 1,
        when Install_Key => 4,
        when Leave | Transmit => 0);

   --  Command_Done word 2.
   type Outcome is (Done, Malformed, Not_Joined, Radio_Fault, No_Room);
   for Outcome use (Done => 0, Malformed => 1, Not_Joined => 2, Radio_Fault => 3, No_Room => 4);

   --  The interface handle: a nonzero 32-bit value wifi.svc chooses.
   subtype Handle is Unsigned_64 range 1 .. 2 ** 32 - 1;

   ---------------------------------------------------------------------------
   --  The frame grant (wifi.svc's memory, lent at Attach): a header page,
   --  then Slots receive slots (driver to wifi.svc) and Slots transmit
   --  slots (wifi.svc to driver) of Slot_Bytes. Each direction's header
   --  holds the producer's free-running count (Produced_At) and the
   --  consumer's (Consumed_At, its own cache line). A slot: length (4
   --  octets, little endian), kind (4 octets), the frame at Frame_At. The
   --  driver also keeps its scan block (CuBit.Wifi_Scan, under its own
   --  seqlock generation) at Scan_At, as in netstack's grant.
   ---------------------------------------------------------------------------
   Page_Bytes : constant := 4_096;
   Slot_Bytes : constant := 2_048;
   Slot_Bits  : constant := 4;
   Slots      : constant := 2 ** Slot_Bits;
   type Frame_Slot is array (0 .. Slot_Bytes - 1) of Unsigned_8;
   package Rings is new CuBit.Slot_Rings (Frame_Slot, Slot_Bits);

   Receive_Header_At  : constant := 0;
   Transmit_Header_At : constant := 2_048;
   Produced_At : constant := 0;
   Consumed_At : constant := 64;
   Scan_At : constant := 2_176;
   pragma Compile_Time_Error
     (Scan_At + WS.Block_Bytes > Page_Bytes or else Scan_At < Transmit_Header_At + Consumed_At + 4,
      "the scan block leaves the header page or overlaps the transmit header");
   Receive_Slots_At  : constant := Page_Bytes;
   Transmit_Slots_At : constant := Receive_Slots_At + Slots * Slot_Bytes;
   Grant_Bytes : constant := Transmit_Slots_At + Slots * Slot_Bytes;
   Grant_Pages : constant := Grant_Bytes / Page_Bytes;
   Length_At : constant := 0;
   Kind_At   : constant := 4;
   Frame_At  : constant := 16;
   Maximum_Frame : constant := Slot_Bytes - Frame_At;

   --  What a slot holds: an 802.11 management frame (MPDU, no FCS), or an
   --  EAPOL frame (the 802.1X PDU, from or to the BSS).
   type Frame_Kind is (Management, EAPOL);
   for Frame_Kind use (Management => 1, EAPOL => 2);

   function Fits (Length : Unsigned_32) return Boolean is (Length in 1 .. Maximum_Frame);

   ---------------------------------------------------------------------------
   --  Join: word 0 = BSSID (address word), word 1 = band (0..7), channel
   --  (8..15).
   ---------------------------------------------------------------------------
   type Join_Request is record
      BSSID   : NL.Hardware_Address := NL.No_Address;
      On_Band : WS.Band := WS.Band_2_4_GHz;
      Channel : WS.Channel_Number := 1;
   end record;

   function Valid (J : Join_Request) return Boolean is
     (WS.Valid_Channel (J.On_Band, J.Channel) and then J.BSSID /= NL.No_Address);

   type Words2 is array (0 .. 1) of Unsigned_64;
   Channel_Shift : constant := 8;
   Byte_Field : constant Unsigned_64 := 16#FF#;

   function Encode (J : Join_Request) return Words2 is
     [0 => NL.Address_Word (J.BSSID),
      1 => Unsigned_64 (WS.Band'Enum_Rep (J.On_Band))
           or Shift_Left (Unsigned_64 (J.Channel), Channel_Shift)];

   type Decoded_Join (Valid : Boolean := False) is record
      case Valid is
         when True  => Item : Join_Request;
         when False => null;
      end case;
   end record;

   function Decode (W : Words2) return Decoded_Join
   with Post => (if Decode'Result.Valid then
                   Valid (Decode'Result.Item) and then Encode (Decode'Result.Item) = W);

   ---------------------------------------------------------------------------
   --  Install_Key: word 0 = kind (0..7), key index (8..15), cipher (16..23);
   --  words 1 .. 2 = the 16 key octets (little endian, 8 a word); word 3 =
   --  the receive sequence counter (6 octets, PN for CCMP).
   ---------------------------------------------------------------------------
   type Key_Kind is (Pairwise, Group);
   for Key_Kind use (Pairwise => 1, Group => 2);
   type Cipher is (CCMP_128);
   for Cipher use (CCMP_128 => 4);
   subtype Key_Index is Natural range 0 .. 3;
   Key_Bytes : constant := 16;
   type Key_Octets is array (0 .. Key_Bytes - 1) of Unsigned_8;
   subtype Packet_Number is Unsigned_64 range 0 .. 2 ** 48 - 1;

   type Key_Install is record
      Kind    : Key_Kind := Pairwise;
      Index   : Key_Index := 0;
      Suite   : Cipher := CCMP_128;
      Key     : Key_Octets := [others => 0];
      RSC     : Packet_Number := 0;
   end record;

   type Words4 is array (0 .. 3) of Unsigned_64;
   Index_Shift  : constant := 8;
   Cipher_Shift : constant := 16;

   function Key_Word (K : Key_Octets; First : Natural) return Unsigned_64 is
     (Unsigned_64 (K (First)) or Shift_Left (Unsigned_64 (K (First + 1)), 8)
      or Shift_Left (Unsigned_64 (K (First + 2)), 16) or Shift_Left (Unsigned_64 (K (First + 3)), 24)
      or Shift_Left (Unsigned_64 (K (First + 4)), 32) or Shift_Left (Unsigned_64 (K (First + 5)), 40)
      or Shift_Left (Unsigned_64 (K (First + 6)), 48) or Shift_Left (Unsigned_64 (K (First + 7)), 56))
   with Pre => First = 0 or else First = 8;

   function Encode (K : Key_Install) return Words4 is
     [0 => Unsigned_64 (Key_Kind'Enum_Rep (K.Kind)) or Shift_Left (Unsigned_64 (K.Index), Index_Shift)
           or Shift_Left (Unsigned_64 (Cipher'Enum_Rep (K.Suite)), Cipher_Shift),
      1 => Key_Word (K.Key, 0),
      2 => Key_Word (K.Key, 8),
      3 => K.RSC];

   type Decoded_Key (Valid : Boolean := False) is record
      case Valid is
         when True  => Item : Key_Install;
         when False => null;
      end case;
   end record;

   function Decode (W : Words4) return Decoded_Key
   with Post => (if Decode'Result.Valid then Encode (Decode'Result.Item) = W);

   ---------------------------------------------------------------------------
   --  Port: word 0 = 1 (closed) or 2 (open).
   ---------------------------------------------------------------------------
   type Port_State is (Closed, Open_Port);
   for Port_State use (Closed => 1, Open_Port => 2);
   procedure Decode_Port (W : Unsigned_64; State : out Port_State; Valid : out Boolean)
   with Post => (if Valid then Unsigned_64 (Port_State'Enum_Rep (State)) = W);

   ---------------------------------------------------------------------------
   --  Associated: word 0 = AID [15:0], the BSS's beacon interval in TU
   --  [31:16] (0: unknown).
   ---------------------------------------------------------------------------
   subtype Association_ID is Natural range 1 .. 2007;   --  9.4.1.8
   Interval_Shift : constant := 16;
   Half_Field : constant Unsigned_64 := 16#FFFF#;
   function Associated_Word (AID : Association_ID; Interval : Unsigned_16) return Unsigned_64 is
     (Unsigned_64 (AID) or Shift_Left (Unsigned_64 (Interval), Interval_Shift));

end CuBit.Wifi_Driver;
