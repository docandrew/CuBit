------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The contract between netmgr (the network manager's authority) and
--  netstack (docs/net-link.md, "netmgr"): configuring interfaces, the
--  interface events, routes, and the DHCP client's datagrams.
--
--  The IPC graph stays a DAG: netmgr calls netstack; nothing flows back
--  except answers. Interface events are the answer to a Watch netmgr
--  submits and netstack parks; each names one interface whose state
--  changed after the sequence number netmgr says it has seen, so none is
--  lost however many coalesce (level state plus a sequence; Watch's
--  Seen word also acknowledges the detach records netstack keeps).
--
--  Addresses in words are IPv4 only so far (a | b << 8 | c << 16 | d << 24),
--  converted at this edge from and to the one 16-byte address type
--  (CuBit.Net_Address, IPv4 mapped).
--
--  Decoders are total; everything a decoder accepts re-encodes to the same
--  words (proved, tests/net-link-state).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Net_Address;
with CuBit.Net_Link;

package CuBit.Net_Management with Pure, SPARK_Mode is
   package NL renames CuBit.Net_Link;
   use type NL.Identity_Kind;

   type Operation is
     (Configure, Set_DNS, Route_Add, Route_Delete, Interface_Next,
      Interface_Detail, Route_List, Ping, Watch, DHCP_Open, DHCP_Send,
      DHCP_Await);
   for Operation use
     (Configure        => 16#0430#,
      Set_DNS          => 16#0432#,
      Route_Add        => 16#0433#,
      Route_Delete     => 16#0434#,
      Interface_Next   => 16#0435#,
      Interface_Detail => 16#0436#,
      Route_List       => 16#0437#,
      Ping             => 16#0438#,
      Watch            => 16#0439#,
      DHCP_Open        => 16#043A#,
      DHCP_Send        => 16#043B#,
      DHCP_Await       => 16#043C#);

   function Label (Op : Operation) return Unsigned_32 is (Operation'Enum_Rep (Op));

   function Is_Operation (Value : Unsigned_32) return Boolean is
     (for some Op in Operation => Value = Label (Op));

   function Operation_Of (Value : Unsigned_32) return Operation
   with Pre => Is_Operation (Value), Post => Label (Operation_Of'Result) = Value;

   --  Words each request carries (the tag's length).
   function Request_Words (Op : Operation) return Natural is
     (case Op is
         when Configure | Route_Add | Route_Delete => 4,
         when Set_DNS | DHCP_Open | DHCP_Send => 2,
         when Ping => 3,
         when Interface_Next | Interface_Detail | Route_List | Watch | DHCP_Await => 1);

   --  Who may send it: the manager's badge, or any holder of netstack's
   --  endpoint (inspection only).
   function Manager_Only (Op : Operation) return Boolean is
     (Op not in Interface_Next | Interface_Detail | Route_List);

   ---------------------------------------------------------------------------
   --  IPv4 words and the 16-byte address.
   ---------------------------------------------------------------------------
   subtype IPv4_Word is Unsigned_32;
   No_IPv4 : constant IPv4_Word := 0;

   --  a.b.c.d as the word a | b << 8 | c << 16 | d << 24.
   function Word_Of (A : CuBit.Net_Address.Address) return IPv4_Word is
     (Unsigned_32 (A (12)) or Shift_Left (Unsigned_32 (A (13)), 8) or
      Shift_Left (Unsigned_32 (A (14)), 16) or Shift_Left (Unsigned_32 (A (15)), 24))
   with Pre => CuBit.Net_Address.Is_Mapped (A);

   function Address_Of (W : IPv4_Word) return CuBit.Net_Address.Address is
     ([0 .. 9 => 0, 10 => 16#FF#, 11 => 16#FF#,
       12 => Unsigned_8 (W and 16#FF#),
       13 => Unsigned_8 (Shift_Right (W, 8) and 16#FF#),
       14 => Unsigned_8 (Shift_Right (W, 16) and 16#FF#),
       15 => Unsigned_8 (Shift_Right (W, 24) and 16#FF#)])
   with Post => CuBit.Net_Address.Is_Mapped (Address_Of'Result)
                and then Word_Of (Address_Of'Result) = W;

   subtype IPv4_Prefix is Natural range 0 .. 32;
   --  Lower is preferred (Linux's convention).
   subtype Route_Metric is Unsigned_32 range 0 .. 16#FFFF#;

   ---------------------------------------------------------------------------
   --  Configure: an interface's IPv4 address, prefix, gateway and the
   --  metric of the routes they give. Address No_IPv4 removes the
   --  configuration (and its routes). Words: 0 = handle, 1 = address,
   --  2 = prefix (bits 0..7), metric (16..31), 3 = gateway (No_IPv4: none).
   ---------------------------------------------------------------------------
   type Configuration is record
      Address : IPv4_Word := No_IPv4;
      Prefix  : IPv4_Prefix := 0;
      Gateway : IPv4_Word := No_IPv4;
      Metric  : Route_Metric := 0;
   end record;

   Prefix_Shift : constant := 0;
   Metric_Shift : constant := 16;
   Byte_Field   : constant Unsigned_64 := 16#FF#;
   Half_Field   : constant Unsigned_64 := 16#FFFF#;
   Word_Field   : constant Unsigned_64 := 16#FFFF_FFFF#;

   type Words4 is array (0 .. 3) of Unsigned_64;

   function Encode (H : NL.Interface_Handle; C : Configuration) return Words4 is
     [0 => NL.Handle_Word (H),
      1 => Unsigned_64 (C.Address),
      2 => Unsigned_64 (C.Prefix) or Shift_Left (Unsigned_64 (C.Metric), Metric_Shift),
      3 => Unsigned_64 (C.Gateway)];

   type Decoded_Configuration (Valid : Boolean := False) is record
      case Valid is
         when True  =>
            Handle : NL.Interface_Handle;
            Item   : Configuration;
         when False => null;
      end case;
   end record;

   function Decode_Configuration (W : Words4) return Decoded_Configuration
   with Post => (if Decode_Configuration'Result.Valid then
                   Encode (Decode_Configuration'Result.Handle,
                           Decode_Configuration'Result.Item) = W);

   ---------------------------------------------------------------------------
   --  Interface_Detail's answer: words
   --  0 = identity (CuBit.Net_Link's attach word 2),
   --  1 = hardware address (bits 0..47), kind (48..55), condition (56..63),
   --  2 = address (0..31), prefix (32..39), metric (48..63),
   --  3 = gateway (0..31), the interface's last event sequence (32..63).
   ---------------------------------------------------------------------------
   type Interface_Summary is record
      Identity  : NL.Stable_Identity := (Kind => NL.Factory_Address);
      Address   : NL.Hardware_Address := NL.No_Address;
      Kind      : NL.Link_Kind := NL.Ethernet;
      Condition : NL.Link_Condition := NL.Down;
      Settings  : Configuration;
      Sequence  : Unsigned_32 := 0;
   end record;

   Kind_Shift_W1      : constant := 48;
   Condition_Shift_W1 : constant := 56;
   Prefix_Shift_W2    : constant := 32;
   Metric_Shift_W2    : constant := 48;
   Sequence_Shift_W3  : constant := 32;

   function Identity_Word (I : NL.Stable_Identity) return Unsigned_64 is
     (Unsigned_64 (NL.Identity_Kind'Enum_Rep (I.Kind)) or
      (if I.Kind = NL.PCI_Function
       then Shift_Left (Unsigned_64 (I.Location), NL.Location_Shift) else 0));

   function Encode (S : Interface_Summary) return Words4 is
     [0 => Identity_Word (S.Identity),
      1 => NL.Address_Word (S.Address) or
           Shift_Left (Unsigned_64 (NL.Link_Kind'Enum_Rep (S.Kind)), Kind_Shift_W1) or
           Shift_Left (Unsigned_64 (NL.Link_Condition'Enum_Rep (S.Condition)), Condition_Shift_W1),
      2 => Unsigned_64 (S.Settings.Address) or
           Shift_Left (Unsigned_64 (S.Settings.Prefix), Prefix_Shift_W2) or
           Shift_Left (Unsigned_64 (S.Settings.Metric), Metric_Shift_W2),
      3 => Unsigned_64 (S.Settings.Gateway) or
           Shift_Left (Unsigned_64 (S.Sequence), Sequence_Shift_W3)];

   type Decoded_Summary (Valid : Boolean := False) is record
      case Valid is
         when True  => Item : Interface_Summary;
         when False => null;
      end case;
   end record;

   function Decode_Summary (W : Words4) return Decoded_Summary
   with Post => (if Decode_Summary'Result.Valid then
                   Encode (Decode_Summary'Result.Item) = W);

   ---------------------------------------------------------------------------
   --  Watch: word 0 = the last event sequence netmgr has handled. The
   --  answer (when netstack has one past it): 0 = the event's sequence,
   --  1 = the interface's handle, 2 = what happened (bits 0..7), the link
   --  condition now (8..15), how many times the link has come Up (16..31,
   --  modulo 2 ** 16) and, when Down, the driver's reason (32..39),
   --  3 = netstack's latest sequence (0..31) and the link report's
   --  generation (32..63), for netmgr's log line. Events
   --  coalesce to the interface's latest state; the Up count shows a
   --  bounce (Up, Down, Up) netmgr did not see as two events.
   ---------------------------------------------------------------------------
   type Interface_Event is (Attached, Detached, Changed);
   for Interface_Event use (Attached => 1, Detached => 2, Changed => 3);

   subtype Up_Count is Unsigned_32 range 0 .. 16#FFFF#;

   type Event_Report is record
      Sequence  : Unsigned_32 := 0;
      Handle    : NL.Interface_Handle;
      Event     : Interface_Event := Changed;
      Condition : NL.Link_Condition := NL.Down;
      Ups       : Up_Count := 0;
      Reason    : NL.Down_Reason := NL.Not_Reported;
      Latest    : Unsigned_32 := 0;
      Generation : NL.Generation := NL.Never_Reported;
   end record;

   Condition_Shift_Event : constant := 8;
   Ups_Shift_Event       : constant := 16;
   Reason_Shift_Event    : constant := 32;
   Generation_Shift_Event : constant := 32;

   function Encode (E : Event_Report) return Words4 is
     [0 => Unsigned_64 (E.Sequence),
      1 => NL.Handle_Word (E.Handle),
      2 => Unsigned_64 (Interface_Event'Enum_Rep (E.Event)) or
           Shift_Left (Unsigned_64 (NL.Link_Condition'Enum_Rep (E.Condition)), Condition_Shift_Event) or
           Shift_Left (Unsigned_64 (E.Ups), Ups_Shift_Event) or
           Shift_Left (Unsigned_64 (NL.Down_Reason'Enum_Rep (E.Reason)), Reason_Shift_Event),
      3 => Unsigned_64 (E.Latest) or
           Shift_Left (Unsigned_64 (E.Generation), Generation_Shift_Event)];

   type Decoded_Event (Valid : Boolean := False) is record
      case Valid is
         when True  => Item : Event_Report;
         when False => null;
      end case;
   end record;

   function Decode_Event (W : Words4) return Decoded_Event
   with Post => (if Decode_Event'Result.Valid then Encode (Decode_Event'Result.Item) = W);

   ---------------------------------------------------------------------------
   --  Route_List: word 0 = the first route index wanted. The answer holds
   --  up to two routes, two words each, the tag's length the routes in it,
   --  its reserved field the next index (0: no more). A route's words:
   --  0 = destination (0..31), prefix (32..39), usable (bit 40), metric
   --  (48..63); 1 = gateway (0..31), the interface's slot (32..63).
   ---------------------------------------------------------------------------
   type Route_Entry is record
      Destination : IPv4_Word := No_IPv4;
      Prefix      : IPv4_Prefix := 0;
      Usable      : Boolean := False;
      Metric      : Route_Metric := 0;
      Gateway     : IPv4_Word := No_IPv4;
      Slot        : NL.Interface_Slot := 0;
   end record;

   Usable_Bit : constant Unsigned_64 := 2 ** 40;
   Routes_Per_Answer : constant := 2;

   type Words2 is array (0 .. 1) of Unsigned_64;

   function Encode (R : Route_Entry) return Words2 is
     [0 => Unsigned_64 (R.Destination) or
           Shift_Left (Unsigned_64 (R.Prefix), Prefix_Shift_W2) or
           (if R.Usable then Usable_Bit else 0) or
           Shift_Left (Unsigned_64 (R.Metric), Metric_Shift_W2),
      1 => Unsigned_64 (R.Gateway) or Shift_Left (Unsigned_64 (R.Slot), Sequence_Shift_W3)];

   type Decoded_Route (Valid : Boolean := False) is record
      case Valid is
         when True  => Item : Route_Entry;
         when False => null;
      end case;
   end record;

   function Decode_Route (W : Words2) return Decoded_Route
   with Post => (if Decode_Route'Result.Valid then Encode (Decode_Route'Result.Item) = W);

   ---------------------------------------------------------------------------
   --  DHCP datagrams (RFC 2131). netmgr runs the client; netstack carries
   --  its messages on one interface: from 0.0.0.0:68 to the limited
   --  broadcast address, port 67, and back to port 68.
   --  - DHCP_Open: 0 = handle, 1 = the grant reference of one page netmgr
   --    lends for that interface (layout below).
   --  - DHCP_Send: 0 = handle, 1 = the message's length at Outgoing_At.
   --    Refused while the link is not Up.
   --  - DHCP_Await (submitted, parked): 0 = handle. Answered with word 0 =
   --    the length of a message netstack put at Incoming_At. netstack keeps
   --    the newest message that arrived with no Await parked and answers
   --    the next Await with it (DHCP retransmits; an older unanswered
   --    datagram is superseded, as RFC 2131 4.1 allows).
   ---------------------------------------------------------------------------
   DHCP_Page_Bytes  : constant := 4_096;
   Outgoing_At      : constant := 0;
   Incoming_At      : constant := 2_048;
   Maximum_Message  : constant := 2_048;
   --  The smallest message: the fixed BOOTP fields and the magic cookie.
   Minimum_Message  : constant := 240;
   subtype Message_Length is Natural range Minimum_Message .. Maximum_Message;
   DHCP_Client_Port : constant := 68;
   DHCP_Server_Port : constant := 67;

end CuBit.Net_Management;
