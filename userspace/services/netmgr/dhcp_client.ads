------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  One interface's DHCP client (RFC 2131), as a state machine over bytes:
--  netmgr hands it the clock, received messages and link transitions, and
--  carries out what each call returns (send these bytes, configure this
--  lease, remove the address). No IPC happens here; the messages travel
--  through netstack's DHCP_Send and DHCP_Await (CuBit.Net_Management).
--
--  States (RFC 2131 4.4, figure 5): Selecting (DISCOVER sent; the first
--  OFFER is taken; RFC 2131's INIT is the moment DISCOVER goes), Requesting (REQUEST for it), Bound, Renewing (from T1),
--  Rebinding (from T2), Rebooting (INIT-REBOOT: REQUEST for the address a
--  lease gave, after the link came back, 3.2 and 4.3.2), and Idle (link
--  not Up: nothing is sent; a lease is kept and re-validated on Up).
--
--  Every message goes out broadcast from 0.0.0.0 (netstack's DHCP channel
--  has no unicast path), with the BROADCAST flag set so the server answers
--  by broadcast too: a client without an address could not receive
--  otherwise. RENEWING therefore broadcasts its REQUEST as REBINDING does
--  (RFC 2131 4.4.5 allows servers to answer either).
--
--  Retransmission (4.1): 4 s, doubling to 64 s; Rebooting and Requesting
--  give up after Maximum_Attempts and fall back to DISCOVER. No random jitter:
--  the transaction identifier comes from the caller's randomness.
--
--  Proved (tests/netmgr-dhcp, level 2): no run-time errors on any input
--  bytes or clock; a built message is a well-formed BOOTP request of
--  Message_Bytes'Length bytes; a reply is accepted only if it carries our
--  transaction identifier and hardware address and a message type; a lease
--  handed to netmgr has a contiguous netmask.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Net_Link;
with CuBit.Net_Management;

package DHCP_Client with SPARK_Mode, Pure is
   package NL renames CuBit.Net_Link;
   package NM renames CuBit.Net_Management;

   --  Message bytes, both ways (the DHCP page's areas).
   subtype Byte_Index is Natural range 0 .. NM.Maximum_Message - 1;
   type Bytes is array (Byte_Index) of Unsigned_8;

   --  What we send is padded to BOOTP's minimum (RFC 1542 2.1).
   Sent_Bytes : constant := 300;

   --  Milliseconds of the monotonic clock.
   subtype Time is Unsigned_64;

   type Phase is (Idle, Selecting, Requesting, Bound, Renewing, Rebinding, Rebooting);

   type Lease is record
      Address : NM.IPv4_Word := NM.No_IPv4;
      Prefix  : NM.IPv4_Prefix := 0;
      Router  : NM.IPv4_Word := NM.No_IPv4;
      DNS_1   : NM.IPv4_Word := NM.No_IPv4;
      DNS_2   : NM.IPv4_Word := NM.No_IPv4;
      Server  : NM.IPv4_Word := NM.No_IPv4;
      Seconds : Unsigned_32 := 0;      --  lease time (option 51)
      T1      : Unsigned_32 := 0;      --  renewal time (58, or half the lease)
      T2      : Unsigned_32 := 0;      --  rebinding time (59, or 7/8)
   end record;

   Retry_First_MS   : constant Time := 4_000;
   Retry_Last_MS    : constant Time := 64_000;
   Maximum_Attempts : constant := 4;
   subtype Attempt_Count is Natural range 0 .. Maximum_Attempts;

   type Client is record
      Current    : Phase := Idle;
      Hardware   : NL.Hardware_Address := NL.No_Address;
      XID        : Unsigned_32 := 0;
      Offer      : Lease;                --  Selecting/Requesting: the one taken
      Held       : Lease;                --  the lease we hold (Has_Lease)
      Has_Lease  : Boolean := False;
      Leased_At  : Time := 0;            --  when Held was acknowledged
      Next_Send  : Time := Time'Last;    --  when to (re)transmit
      Retry      : Time := Retry_First_MS;
      Attempts   : Attempt_Count := 0;
   end record;

   --  What the caller does after a call, in this order: remove the
   --  address (Deconfigure), install Configured, send Out (0 .. Length - 1).
   type Action is record
      Deconfigure : Boolean := False;
      Configure   : Boolean := False;
      Configured  : Lease;
      Send        : Boolean := False;
      Length      : Natural := 0;
   end record;

   function Valid (C : Client) return Boolean is
     (C.Retry in Retry_First_MS .. Retry_Last_MS and then
      (if C.Has_Lease then C.Held.Address /= NM.No_IPv4));

   function Sent_Right (A : Action) return Boolean is
     (if A.Send then A.Length = Sent_Bytes else A.Length = 0);

   --  A client for this interface, Idle until its link is Up.
   function Create (Hardware : NL.Hardware_Address) return Client
   with Post => Valid (Create'Result) and then Create'Result.Current = Idle
                and then not Create'Result.Has_Lease;

   --  The link came Up: INIT-REBOOT with a lease, else DISCOVER. XID is a
   --  fresh random transaction identifier.
   procedure Link_Up (C : in out Client; Now : Time; XID : Unsigned_32;
                      Out_Bytes : in out Bytes; Result : out Action)
   with Pre  => Valid (C),
        Post => Valid (C) and then Sent_Right (Result) and then Result.Send
                and then C.Current = (if C'Old.Has_Lease then Rebooting else Selecting);

   --  The link left Up: stop sending; keep the lease.
   procedure Link_Down (C : in out Client)
   with Pre  => Valid (C),
        Post => Valid (C) and then C.Current = Idle and then C.Has_Lease = C'Old.Has_Lease
                and then C.Held = C'Old.Held;

   --  A message from the server, Length bytes of In_Bytes.
   procedure Receive (C : in out Client; Now : Time; In_Bytes : Bytes; Length : Natural;
                      Out_Bytes : in out Bytes; Result : out Action)
   with Pre  => Valid (C),
        Post => Valid (C) and then Sent_Right (Result) and then
                (if Result.Configure then
                   Result.Configured.Address /= NM.No_IPv4 and then C.Current = Bound
                   and then C.Has_Lease and then C.Held = Result.Configured);

   --  Timers: retransmission, T1, T2, the lease's end.
   procedure Tick (C : in out Client; Now : Time; XID : Unsigned_32;
                   Out_Bytes : in out Bytes; Result : out Action)
   with Pre  => Valid (C),
        Post => Valid (C) and then Sent_Right (Result);

   --  When Tick next has work (Time'Last: none).
   function Next_Deadline (C : Client) return Time;

   ---------------------------------------------------------------------------
   --  The wire (exposed for the tests).
   ---------------------------------------------------------------------------
   type Message_Type is (Discover, Offer, Request, Decline, Ack, Nak, Release);
   for Message_Type use
     (Discover => 1, Offer => 2, Request => 3, Decline => 4, Ack => 5, Nak => 6, Release => 7);

   --  What a server's message says (Parse).
   type Reply is record
      Kind  : Message_Type := Offer;
      Offer : Lease;                  --  yiaddr and options
   end record;

   --  Length bytes of B as a server reply to XID for Hardware.
   procedure Parse (B : Bytes; Length : Natural; XID : Unsigned_32;
                    Hardware : NL.Hardware_Address; Item : out Reply; OK : out Boolean)
   with Post => (if OK then Item.Kind in Offer | Ack | Nak and then
                   (if Item.Kind /= Nak then Item.Offer.Address /= NM.No_IPv4));

   --  Contiguous netmask to prefix length.
   procedure Prefix_Of (Mask : NM.IPv4_Word; Prefix : out NM.IPv4_Prefix; OK : out Boolean);

   --  A client request of Kind (Discover or Request) into B (Sent_Bytes).
   --  Requested: option 50 (No_IPv4: none); Server: option 54 (No_IPv4:
   --  none); Client_Address: ciaddr.
   procedure Build (B : in out Bytes; Kind : Message_Type; XID : Unsigned_32;
                    Hardware : NL.Hardware_Address;
                    Requested, Server, Client_Address : NM.IPv4_Word)
   with Pre => Kind in Discover | Request;

   --  BOOTP fields and options (RFC 2131 2, RFC 2132).
   Op_Request       : constant := 1;
   Op_Reply         : constant := 2;
   Hardware_Ethernet : constant := 1;
   Hardware_Length  : constant := 6;
   Op_At            : constant := 0;
   Hardware_Type_At : constant := 1;
   Hardware_Len_At  : constant := 2;
   XID_At           : constant := 4;
   Flags_At         : constant := 10;
   Client_Addr_At   : constant := 12;
   Your_Addr_At     : constant := 16;
   Hardware_At      : constant := 28;
   Cookie_At        : constant := 236;
   Options_At       : constant := 240;
   Broadcast_Flag   : constant := 16#80#;   --  high bit of the flags' first byte
   Cookie : constant array (0 .. 3) of Unsigned_8 := [99, 130, 83, 99];

   Option_Pad         : constant := 0;
   Option_Subnet_Mask : constant := 1;
   Option_Router      : constant := 3;
   Option_DNS         : constant := 6;
   Option_Requested   : constant := 50;
   Option_Lease_Time  : constant := 51;
   Option_Message_Type : constant := 53;
   Option_Server_ID   : constant := 54;
   Option_Parameters  : constant := 55;
   Option_Renewal     : constant := 58;
   Option_Rebinding   : constant := 59;
   Option_Client_ID   : constant := 61;
   Option_End         : constant := 255;

end DHCP_Client;
