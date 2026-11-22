------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The contract between a network driver and netstack (docs/net-link.md):
--  the typed messages a driver sends (Attach, Set_Address, the receive
--  doorbell, the parked transmit wait) and the link report it keeps in its
--  frame grant's header page (CuBit.Frame_Rings.Link_At).
--
--  Every decoder here is total: a word a driver or netstack did not form
--  correctly decodes as Valid => False, never as some other value.
--
--  Proved (tests/net-link-state, level 2): no run-time errors; whatever a
--  decoder accepts re-encodes to exactly the words it was given (the
--  attach request, the link report), so no two encodings mean one value
--  and no bits are ignored; handles and hardware addresses round-trip;
--  the seqlock accepts a report only between two equal, even generation
--  reads. Tested (hosted, not proved): the attach request and the link
--  report round-trip (Decode (Encode (X)) = X, 100,000 random values each).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Frame_Rings;

package CuBit.Net_Link with Pure, SPARK_Mode is

   ---------------------------------------------------------------------------
   --  Operations: a driver's messages to netstack (the endpoint devmgr or
   --  its launcher badged with the network driver's authority).
   --  - Attach (call): a new interface; answered with its handle and the
   --    frame grant.
   --  - Receive_Doorbell (one-way): frames, or a new link report, are in
   --    the grant (sent once per netstack wake epoch, as before).
   --  - Transmit_Wait (submitted, parked): answered when netstack has put
   --    frames in the transmit ring, or freed receive slots the driver
   --    asked for, after the driver published a new wake epoch. netstack
   --    never calls or submits to a driver.
   --  - Set_Address (call): a new hardware address, only while the link is
   --    not Up.
   ---------------------------------------------------------------------------
   type Operation is (Attach, Receive_Doorbell, Transmit_Wait, Set_Address);
   for Operation use
     (Attach           => 16#0400#,
      Receive_Doorbell => 16#0401#,
      Transmit_Wait    => 16#0402#,
      Set_Address      => 16#0403#);

   function Label (Op : Operation) return Unsigned_32 is
     (Operation'Enum_Rep (Op));

   function Is_Operation (Value : Unsigned_32) return Boolean is
     (for some Op in Operation => Value = Label (Op));

   function Operation_Of (Value : Unsigned_32) return Operation is
     (if Value = Label (Attach) then Attach
      elsif Value = Label (Receive_Doorbell) then Receive_Doorbell
      elsif Value = Label (Transmit_Wait) then Transmit_Wait
      else Set_Address)
   with Pre => Is_Operation (Value), Post => Label (Operation_Of'Result) = Value;

   --  Words each request carries (the tag's length).
   function Request_Words (Op : Operation) return Natural is
     (case Op is
         when Attach           => 3,
         when Receive_Doorbell => 1,
         when Transmit_Wait    => 1,
         when Set_Address      => 2);

   ---------------------------------------------------------------------------
   --  Hardware addresses (IEEE 802 MAC-48).
   ---------------------------------------------------------------------------
   Octets : constant := 6;
   subtype Octet_Index is Natural range 0 .. Octets - 1;
   type Hardware_Address is array (Octet_Index) of Unsigned_8;
   No_Address : constant Hardware_Address := [others => 0];

   --  The individual/group bit (IEEE 802-2014 8.2): set for multicast.
   Group_Bit : constant Unsigned_8 := 16#01#;

   --  An address an interface may own: individual and not all zeros.
   function Assignable (A : Hardware_Address) return Boolean is
     ((A (0) and Group_Bit) = 0 and then A /= No_Address);

   Octet_Bits    : constant := 8;
   Address_Bits  : constant := Octets * Octet_Bits;
   Address_Limit : constant Unsigned_64 := 2 ** Address_Bits;

   --  In a word: octet I at bits 8 * I .. 8 * I + 7, the rest zero.
   function Address_Word (A : Hardware_Address) return Unsigned_64 is
     (Unsigned_64 (A (0)) or Shift_Left (Unsigned_64 (A (1)), 8) or
      Shift_Left (Unsigned_64 (A (2)), 16) or Shift_Left (Unsigned_64 (A (3)), 24) or
      Shift_Left (Unsigned_64 (A (4)), 32) or Shift_Left (Unsigned_64 (A (5)), 40))
   with Post => Address_Word'Result < Address_Limit;

   function Valid_Address_Word (W : Unsigned_64) return Boolean is
     (W < Address_Limit);

   function Address_Of (W : Unsigned_64) return Hardware_Address is
     [for I in Octet_Index =>
        Unsigned_8 (Shift_Right (W, Octet_Bits * I) and 16#FF#)]
   with Pre => Valid_Address_Word (W);

   --  Round trips (proved).
   procedure Lemma_Address_Round_Trip (A : Hardware_Address) with
     Ghost, Global => null,
     Post => Address_Of (Address_Word (A)) = A;

   ---------------------------------------------------------------------------
   --  What a driver declares when it attaches.
   ---------------------------------------------------------------------------
   type Link_Kind is (Ethernet, Wireless);
   for Link_Kind use (Ethernet => 1, Wireless => 2);

   --  The smallest MTU any interface may have (an IPv6 link's, RFC 8200 5),
   --  and the largest a frame slot holds.
   Ethernet_Header : constant := 14;
   Minimum_MTU : constant := 1_280;
   Maximum_MTU : constant := CuBit.Frame_Rings.Maximum_Frame - Ethernet_Header;
   subtype MTU is Unsigned_32 range Minimum_MTU .. Maximum_MTU;

   --  Work the device does that netstack may then skip.
   type Offloads is record
      Receive_Checksum  : Boolean := False;   --  verified on receive
      Transmit_Checksum : Boolean := False;   --  filled on transmit
      Segmentation      : Boolean := False;   --  TCP segmentation on transmit
   end record;
   No_Offloads : constant Offloads := (others => False);

   --  Where the device is: what netmgr names the interface by, so a name
   --  never depends on attach order. A PCI function's location (bus << 8 |
   --  device << 3 | function), or, for a bus with no stable location (USB),
   --  the device's factory hardware address (the Attach address).
   type Identity_Kind is (PCI_Function, Factory_Address);
   for Identity_Kind use (PCI_Function => 1, Factory_Address => 2);
   subtype PCI_Location is Unsigned_32 range 0 .. 16#FFFF#;

   type Stable_Identity (Kind : Identity_Kind := PCI_Function) is record
      case Kind is
         when PCI_Function    => Location : PCI_Location := 0;
         when Factory_Address => null;
      end case;
   end record;

   --  The interface's name, from its identity (never from attach order):
   --  pci-<bus>-<device>-<function> in lower-case hexadecimal ("pci-00-03-0")
   --  or mac-<the factory address's 12 hexadecimal digits>. netstack logs
   --  it and netmgr keys its settings by it (net.interface.<name>).
   Name_Characters : constant := 16;
   subtype Name_Length is Natural range 0 .. Name_Characters;
   type Interface_Name is record
      Text   : String (1 .. Name_Characters) := [others => ' '];
      Length : Name_Length := 0;
   end record;

   function Name_Of (Identity : Stable_Identity; Factory : Hardware_Address)
     return Interface_Name
   with Post => Name_Of'Result.Length > 0;

   type Attach_Request is record
      Kind     : Link_Kind := Ethernet;
      Address  : Hardware_Address := No_Address;
      Size     : MTU := Minimum_MTU;
      Offload  : Offloads := No_Offloads;
      Identity : Stable_Identity := (Kind => Factory_Address);
   end record;

   function Valid (R : Attach_Request) return Boolean is (Assignable (R.Address));

   --  Word layout: 0 = kind (bits 0..7), MTU (16..31), offloads (32..34);
   --  1 = the address word; 2 = identity kind (0..7), PCI location (16..31).
   Kind_Shift     : constant := 0;
   MTU_Shift      : constant := 16;
   Offload_Shift  : constant := 32;
   Field_Mask_8   : constant Unsigned_64 := 16#FF#;
   Field_Mask_16  : constant Unsigned_64 := 16#FFFF#;
   Receive_Checksum_Bit  : constant Unsigned_64 := 2 ** 0;
   Transmit_Checksum_Bit : constant Unsigned_64 := 2 ** 1;
   Segmentation_Bit      : constant Unsigned_64 := 2 ** 2;
   Offload_Bits          : constant Unsigned_64 :=
     Receive_Checksum_Bit or Transmit_Checksum_Bit or Segmentation_Bit;
   Location_Shift : constant := 16;

   type Words3 is array (0 .. 2) of Unsigned_64;

   function Offload_Field (O : Offloads) return Unsigned_64 is
     ((if O.Receive_Checksum then Receive_Checksum_Bit else 0) or
      (if O.Transmit_Checksum then Transmit_Checksum_Bit else 0) or
      (if O.Segmentation then Segmentation_Bit else 0))
   with Post => (Offload_Field'Result and not Offload_Bits) = 0;

   function Encode (R : Attach_Request) return Words3 is
     [0 => Unsigned_64 (Link_Kind'Enum_Rep (R.Kind)) or
           Shift_Left (Unsigned_64 (R.Size), MTU_Shift) or
           Shift_Left (Offload_Field (R.Offload), Offload_Shift),
      1 => Address_Word (R.Address),
      2 => Unsigned_64 (Identity_Kind'Enum_Rep (R.Identity.Kind)) or
           (if R.Identity.Kind = PCI_Function
            then Shift_Left (Unsigned_64 (R.Identity.Location), Location_Shift) else 0)];

   type Decoded_Attach (Valid : Boolean := False) is record
      case Valid is
         when True  => Request : Attach_Request;
         when False => null;
      end case;
   end record;

   --  W formed by Encode from a Valid request; anything else is refused.
   function Decode (W : Words3) return Decoded_Attach
   with Post => (if Decode'Result.Valid then
                   Valid (Decode'Result.Request) and then
                   Encode (Decode'Result.Request) = W);

   --  Why an Attach was refused (the error reply's word 0).
   type Attach_Refusal is (Malformed, Out_Of_Memory, Grant_Refused, Not_A_Driver);
   for Attach_Refusal use
     (Malformed => 1, Out_Of_Memory => 2, Grant_Refused => 3, Not_A_Driver => 4);

   ---------------------------------------------------------------------------
   --  Interface handles: what Attach answers with and every later request
   --  names. A slot in netstack's interface table and the epoch of that
   --  slot's occupant: netstack never gives an epoch twice, so a handle to
   --  an interface that detached never names its slot's next occupant.
   --  Word: epoch in the high 32 bits (never zero), slot in the low 32.
   ---------------------------------------------------------------------------
   subtype Interface_Slot is Unsigned_32;
   subtype Interface_Epoch is Unsigned_32 range 1 .. Unsigned_32'Last;
   type Interface_Handle is record
      Slot  : Interface_Slot := 0;
      Epoch : Interface_Epoch := 1;
   end record;

   Handle_Field_Base : constant Unsigned_64 := 2 ** 32;

   function Handle_Word (H : Interface_Handle) return Unsigned_64 is
     (Unsigned_64 (H.Epoch) * Handle_Field_Base + Unsigned_64 (H.Slot))
   with Post => Handle_Word'Result / Handle_Field_Base = Unsigned_64 (H.Epoch)
                and then Handle_Word'Result mod Handle_Field_Base = Unsigned_64 (H.Slot);

   function Valid_Handle_Word (W : Unsigned_64) return Boolean is
     (W / Handle_Field_Base /= 0);

   function Handle_Of (W : Unsigned_64) return Interface_Handle is
     ((Slot  => Interface_Slot (W mod Handle_Field_Base),
       Epoch => Interface_Epoch (W / Handle_Field_Base)))
   with Pre => Valid_Handle_Word (W), Post => Handle_Word (Handle_Of'Result) = W;

   ---------------------------------------------------------------------------
   --  Set_Address: words 0 = handle, 1 = address word. The answer's word 0
   --  (a REPLY_OK for Address_Set, REPLY_ERR otherwise) is the outcome.
   ---------------------------------------------------------------------------
   type Address_Outcome is
     (Address_Set, Refused_Link_Up, Refused_Not_Assignable, Refused_Unknown_Interface);
   for Address_Outcome use
     (Address_Set => 0, Refused_Link_Up => 1, Refused_Not_Assignable => 2,
      Refused_Unknown_Interface => 3);

   function Is_Address_Outcome (W : Unsigned_64) return Boolean is
     (for some O in Address_Outcome => W = Unsigned_64 (Address_Outcome'Enum_Rep (O)));

   ---------------------------------------------------------------------------
   --  The link report (RFC 2863's operational states, as CuBit uses them):
   --  - Down, with why;
   --  - Dormant: the medium is there but not yet usable for traffic (Wi-Fi
   --    associated, its 802.1X controlled port not yet open);
   --  - Up: usable, with its speed (0: unknown), and on a Wireless link the
   --    BSSID and the network it belongs to (the ESS's identity).
   ---------------------------------------------------------------------------
   type Link_Condition is (Down, Dormant, Up);
   for Link_Condition use (Down => 1, Dormant => 2, Up => 3);

   type Down_Reason is
     (Not_Reported, No_Carrier, Administratively_Down, Not_Associated,
      Authentication_Failed, Hardware_Fault);
   for Down_Reason use
     (Not_Reported => 0, No_Carrier => 1, Administratively_Down => 2,
      Not_Associated => 3, Authentication_Failed => 4, Hardware_Fault => 5);

   subtype Speed_Mbps is Unsigned_32;

   SSID_Octets : constant := 32;      --  IEEE 802.11-2020 9.4.2.2
   subtype SSID_Length is Natural range 0 .. SSID_Octets;
   subtype SSID_Index is Positive range 1 .. SSID_Octets;
   type SSID_Bytes is array (SSID_Index) of Unsigned_8;

   type Wireless_Security is (Open, WPA2_Personal, WPA3_Personal, WPA3_Transition);
   for Wireless_Security use
     (Open => 1, WPA2_Personal => 2, WPA3_Personal => 3, WPA3_Transition => 4);

   --  The network (ESS): its SSID (bytes past Length zero) and security.
   type Network_Identity is record
      SSID     : SSID_Bytes := [others => 0];
      Length   : SSID_Length := 0;
      Security : Wireless_Security := Open;
   end record;

   function Normal (N : Network_Identity) return Boolean is
     (for all I in SSID_Index => (if I > N.Length then N.SSID (I) = 0));

   type Up_Detail (Kind : Link_Kind := Ethernet) is record
      Speed : Speed_Mbps := 0;
      case Kind is
         when Ethernet => null;
         when Wireless =>
            BSSID   : Hardware_Address := No_Address;
            Network : Network_Identity;
      end case;
   end record;

   type Link_State (Condition : Link_Condition := Down) is record
      case Condition is
         when Down    => Reason : Down_Reason := Not_Reported;
         when Dormant => null;
         when Up      => Detail : Up_Detail;
      end case;
   end record;

   Not_Reported_State : constant Link_State := (Condition => Down, Reason => Not_Reported);

   function Normal (S : Link_State) return Boolean is
     (if S.Condition = Up and then S.Detail.Kind = Wireless
      then Normal (S.Detail.Network) else True);

   --  The report's words (Frame_Rings.Link_Word_Index 1 .. Link_Words - 1):
   --  1 = condition (bits 0..7), reason (8..15), kind of an Up link (16..23);
   --  2 = speed; 3 = security (0..7), SSID length (8..15); 4 .. 5 = BSSID
   --  (octets 0..3, then 4..5); 6 .. 13 = SSID, four octets a word.
   subtype Report_Index is Natural range 1 .. CuBit.Frame_Rings.Link_Words - 1;
   type Report_Words is array (Report_Index) of Unsigned_32;

   Condition_Word : constant Report_Index := 1;
   Speed_Word     : constant Report_Index := 2;
   Network_Word   : constant Report_Index := 3;
   BSSID_Low_Word : constant Report_Index := 4;
   BSSID_High_Word : constant Report_Index := 5;
   SSID_First_Word : constant Report_Index := 6;
   Octets_Per_Word : constant := 4;
   pragma Compile_Time_Error
     (SSID_First_Word + SSID_Octets / Octets_Per_Word - 1 /= Report_Index'Last,
      "the SSID words must end the link block");

   Reason_Shift    : constant := 8;
   Up_Kind_Shift   : constant := 16;
   Length_Shift    : constant := 8;
   Byte_Mask_32    : constant Unsigned_32 := 16#FF#;

   function Encode (S : Link_State) return Report_Words
   with Pre => Normal (S);

   type Decoded_Report (Valid : Boolean := False) is record
      case Valid is
         when True  => State : Link_State;
         when False => null;
      end case;
   end record;

   --  Words formed by Encode from a Normal state; anything else is refused.
   function Decode (W : Report_Words) return Decoded_Report
   with Post => (if Decode'Result.Valid then
                   Normal (Decode'Result.State) and then
                   Encode (Decode'Result.State) = W);

   ---------------------------------------------------------------------------
   --  The report's generation (a seqlock, written only by the driver).
   --  Writer: Begin_Write (odd), the report words, End_Write (even), then
   --  the receive doorbell if netstack's wake epoch asks for one. Reader:
   --  the generation, then the words, then the generation again; the words
   --  are a report only if both reads are equal and even. Each published
   --  report adds 2, so a reader sees how many it missed (coalescing is
   --  allowed: the report is a level, the latest one is what counts).
   ---------------------------------------------------------------------------
   subtype Generation is Unsigned_32;
   Never_Reported : constant Generation := 0;

   function Writing (G : Generation) return Boolean is (G mod 2 = 1);

   function Begin_Write (G : Generation) return Generation is (G + 1)
   with Pre => not Writing (G), Post => Writing (Begin_Write'Result);

   function End_Write (G : Generation) return Generation is (G + 1)
   with Pre => Writing (G), Post => not Writing (End_Write'Result);

   --  Two reads around the report's words give a consistent report.
   function Stable (Before, After : Generation) return Boolean is
     (Before = After and then not Writing (Before));

   --  Reports published between generation Seen and Now (both stable).
   function Reports_Between (Seen, Now : Generation) return Generation is
     ((Now - Seen) / 2)
   with Pre => not Writing (Seen) and then not Writing (Now);

   --  How many times a reader tries before keeping what it had (a driver
   --  that stopped between Begin_Write and End_Write cannot stall it).
   Read_Attempts : constant := 4;

end CuBit.Net_Link;
