------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with IPv6_Header; use IPv6_Header;
with Internet_Checksum;
with ND_Message;
with Neighbor_Cache;
with RA_Message;
with SLAAC_Table;

package body IPv6_Link with
  SPARK_Mode,
  Refined_State => (State => (Our_MAC, Key, Link_Local, LL_State, LL_Until,
                              Addresses, Probed, Announced, Neighbors, Router,
                              Have_Router, Solicits_Sent, Next_Solicit, Test,
                              Test_Tries, Test_Until))
is

   use type ND_Message.Kind;
   use type ND_Message.MAC;
   use type SLAAC_Table.State;

   ICMPv6              : constant := 58;
   Echo_Request        : constant := 128;
   Echo_Reply          : constant := 129;
   Router_Solicitation : constant := 133;
   Router_Advertisement : constant := RA_Message.Advertisement_Type;
   Neighbor_Solicit    : constant := ND_Message.Solicitation_Type;
   Neighbor_Advert     : constant := ND_Message.Advertisement_Type;
   ND_Hop_Limit        : constant := 255;
   Default_Hop_Limit   : constant := 64;
   Ethernet_Header     : constant := IPv6_Frame.Ethernet_Header;
   Maximum_Frame       : constant := IPv6_Frame.Maximum_Frame;
   Maximum_IPv6        : constant := Maximum_Frame - Ethernet_Header;
   Maximum_Message     : constant := Maximum_IPv6 - IPv6_Header.Size;
   Pseudo_Header       : constant := 40;          --  RFC 8200 8.1
   Checksum_At         : constant := 2;
   Minimum_Message     : constant := 4;           --  type, code, checksum
   Echo_Minimum        : constant := 8;
   Milliseconds        : constant := 1_000;
   DAD_Milliseconds    : constant := SLAAC_Table.DAD_Seconds * Milliseconds;
   Retry_Milliseconds  : constant := 1_000;
   Solicit_Interval    : constant := 4 * Retry_Milliseconds;
   Maximum_Retries     : constant := 3;
   Echo_Identifier     : constant := 16#C0B1#;
   Flag_Solicited      : constant := 16#40#;      --  RFC 4861 4.4 S
   Flag_Override       : constant := 16#20#;      --  O
   MAC_Octets          : constant := 1;           --  option length, 8 bytes

   Link_Local_Prefix : constant Address := [0 => 16#FE#, 1 => 16#80#, others => 0];
   All_Nodes   : constant Address := [0 => 16#FF#, 1 => 16#02#, 15 => 1, others => 0];
   All_Routers : constant Address := [0 => 16#FF#, 1 => 16#02#, 15 => 2, others => 0];
   Unspecified : constant Address := [others => 0];

   type Link_Local_State is (Absent, Tentative, Usable, Duplicate);
   type Test_State is (Off, Waiting, Resolving, Sent, Done);
   subtype Retry_Count is Natural range 0 .. Maximum_Retries;
   type Flags is array (SLAAC_Table.Index) of Boolean;

   Our_MAC       : Net.MACAddress := Net.ZERO_MAC;
   Key           : SipHash.Key := (K0 => 0, K1 => 0);
   Link_Local    : Address := Unspecified;
   LL_State      : Link_Local_State := Absent;
   LL_Until      : Unsigned_64 := 0;
   Addresses     : SLAAC_Table.Table;
   Probed        : Flags := [others => False];
   Announced     : Flags := [others => False];
   Neighbors     : Neighbor_Cache.Table;
   Router        : Address := Unspecified;
   Have_Router   : Boolean := False;
   Solicits_Sent : Retry_Count := 0;
   Next_Solicit  : Unsigned_64 := Unsigned_64'Last;
   Test          : Test_State := Off;
   Test_Tries    : Retry_Count := 0;
   Test_Until    : Unsigned_64 := Unsigned_64'Last;

   subtype Message_Bytes is IPv6_Header.Bytes;

   function Valid return Boolean is
     (SLAAC_Table.Unique (Addresses) and then Neighbor_Cache.Unique (Neighbors))
   with Refined_Global => (Addresses, Neighbors);

   --  Now plus Delta, or the end of time rather than wrapping.
   function Later (Now : Unsigned_64; Delta_Ms : Unsigned_64) return Unsigned_64 is
     (if Now > Unsigned_64'Last - Delta_Ms then Unsigned_64'Last else Now + Delta_Ms);

   --  The address tables count seconds.
   function Seconds (Now : Unsigned_64) return SLAAC_Table.Time is
     (Now / Milliseconds);

   ---------------------------------------------------------------------------
   --  Text
   ---------------------------------------------------------------------------
   --  Fixed sizes: netstack has no secondary stack.
   subtype Hex_Text is String (1 .. 4);
   subtype Address_Text is String (1 .. 39);

   function Hex4 (V : Unsigned_16) return Hex_Text is
      Digits_Of : constant String (1 .. 16) := "0123456789abcdef";
      S : Hex_Text := [others => '0'];
   begin
      for K in 0 .. 3 loop
         S (4 - K) := Digits_Of (Natural (Shift_Right (V, 4 * K) and 16#F#) + 1);
      end loop;
      return S;
   end Hex4;

   function Image (A : Address) return Address_Text is
      S : Address_Text := [others => ':'];
   begin
      for G in 0 .. 7 loop
         S (G * 5 + 1 .. G * 5 + 4) :=
           Hex4 (Shift_Left (Unsigned_16 (A (2 * G)), 8) or Unsigned_16 (A (2 * G + 1)));
      end loop;
      return S;
   end Image;

   ---------------------------------------------------------------------------
   --  Addresses
   ---------------------------------------------------------------------------
   function Solicited_Node (Target : Address) return Address is
     [0 => 16#FF#, 1 => 16#02#, 11 => 1, 12 => 16#FF#,
      13 => Target (13), 14 => Target (14), 15 => Target (15), others => 0];

   function Multicast_MAC (A : Address) return Net.MACAddress is
     [16#33#, 16#33#, A (12), A (13), A (14), A (15)];

   function With_ID (Network : Address; ID : SLAAC_Table.Interface_ID) return Address is
      R : Address := Network;
   begin
      for K in 0 .. 7 loop
         R (8 + K) := ID (K);
      end loop;
      return R;
   end With_ID;

   --  One of our addresses, usable as a source.
   function Ours (A : Address) return Boolean is
     ((LL_State = Usable and then A = Link_Local) or else
      (for some I in SLAAC_Table.Index =>
         Addresses (I).St in SLAAC_Table.Preferred | SLAAC_Table.Deprecated and then
         Addresses (I).Addr = A));

   --  One of ours still under detection.
   function Tentative (A : Address) return Boolean is
     ((LL_State = Tentative and then A = Link_Local) or else
      (for some I in SLAAC_Table.Index =>
         Addresses (I).St = SLAAC_Table.Tentative and then Addresses (I).Addr = A));

   --  A destination this node receives.
   function For_Us (A : Address) return Boolean is
     (Ours (A) or else Tentative (A) or else A = All_Nodes or else
      (A (0 .. 12) = Solicited_Node (Unspecified) (0 .. 12)));

   ---------------------------------------------------------------------------
   --  Sending
   ---------------------------------------------------------------------------
   --  ICMPv6 checksum over the pseudo-header (RFC 8200 8.1) and Message.
   function Checksum (Source, Destination : Address; Message : Message_Bytes)
     return Unsigned_16
   with Pre => Message'Length <= Maximum_Message
   is
      Scratch : Message_Bytes (0 .. Pseudo_Header + Message'Length - 1) := [others => 0];
   begin
      Scratch (0 .. 15) := Message_Bytes (Source);
      Scratch (16 .. 31) := Message_Bytes (Destination);
      Scratch (34) := Unsigned_8 (Message'Length / 256);
      Scratch (35) := Unsigned_8 (Message'Length mod 256);
      Scratch (39) := ICMPv6;
      Scratch (Pseudo_Header .. Scratch'Last) := Message;
      return Internet_Checksum.Of_Bytes (Scratch);
   end Checksum;

   procedure Send_ICMP
     (To_MAC : Net.MACAddress; Source, Destination : Address;
      Hop_Limit : Unsigned_8; Message : Message_Bytes)
   with Pre => Message'First = 0 and then
               Message'Length in Minimum_Message .. Maximum_Message
   is
      Frame : Message_Bytes (0 .. Ethernet_Header + IPv6_Header.Size + Message'Length - 1) :=
        [others => 0];
      Packet : Message_Bytes (0 .. IPv6_Header.Size + Message'Length - 1) := [others => 0];
      Sum_At : constant Natural := IPv6_Header.Size + Checksum_At;
      Sum : Unsigned_16;
   begin
      if not Acceptable_Source (Source) or else not Acceptable_Destination (Destination) then
         return;
      end if;
      Packet (IPv6_Header.Size .. Packet'Last) := Message;
      Packet (Sum_At) := 0;
      Packet (Sum_At + 1) := 0;
      Sum := Checksum (Source, Destination, Packet (IPv6_Header.Size .. Packet'Last));
      Packet (Sum_At) := Unsigned_8 (Shift_Right (Sum, 8));
      Packet (Sum_At + 1) := Unsigned_8 (Sum and 16#FF#);
      IPv6_Header.Build
        ((Traffic_Class => 0, Flow_Label => 0, Payload_Length => Message'Length,
          Next_Header => ICMPv6, Hop_Limit => Hop_Limit,
          Source => Source, Destination => Destination),
         Packet);
      for K in 0 .. 5 loop
         Frame (K) := To_MAC (K);
         Frame (6 + K) := Our_MAC (K);
      end loop;
      Frame (12) := IPv6_Frame.Type_High;
      Frame (13) := IPv6_Frame.Type_Low;
      Frame (Ethernet_Header .. Frame'Last) := Packet;
      pragma Assert (Address_At (Frame, IPv6_Frame.Source_At) = Address_At (Packet, 8));
      pragma Assert (Address_At (Frame, IPv6_Frame.Destination_At) = Address_At (Packet, 24));
      Send (Frame);
   end Send_ICMP;

   --  A link-layer address option: fixed size (no secondary stack).
   subtype Option_Bytes is Message_Bytes (0 .. 7);
   function Link_Option (Kind : Unsigned_8) return Option_Bytes is
     [Kind, MAC_Octets, Our_MAC (0), Our_MAC (1), Our_MAC (2), Our_MAC (3),
      Our_MAC (4), Our_MAC (5)];

   --  Neighbor Solicitation for Target: from Source (with our link
   --  address), or from :: for duplicate address detection (without).
   procedure Solicit (Source, Target : Address) is
      Detecting : constant Boolean := Source = Unspecified;
      M : Message_Bytes (0 .. (if Detecting then 23 else 31)) := [others => 0];
   begin
      M (0) := Neighbor_Solicit;
      M (8 .. 23) := Message_Bytes (Target);
      if not Detecting then
         M (24 .. 31) := Link_Option (ND_Message.Source_Link_Option);
      end if;
      Send_ICMP (Multicast_MAC (Solicited_Node (Target)), Source, Solicited_Node (Target),
                 ND_Hop_Limit, M);
   end Solicit;

   procedure Advertise (To_MAC : Net.MACAddress; Destination, Target : Address;
                        Solicited : Boolean) is
      M : Message_Bytes (0 .. 31) := [others => 0];
   begin
      M (0) := Neighbor_Advert;
      M (4) := (if Solicited then Flag_Solicited + Flag_Override else Flag_Override);
      M (8 .. 23) := Message_Bytes (Target);
      M (24 .. 31) := Link_Option (ND_Message.Target_Link_Option);
      Send_ICMP (To_MAC, Target, Destination, ND_Hop_Limit, M);
   end Advertise;

   procedure Solicit_Routers is
      M : Message_Bytes (0 .. 15) := [others => 0];
   begin
      M (0) := Router_Solicitation;
      M (8 .. 15) := Link_Option (ND_Message.Source_Link_Option);
      Send_ICMP (Multicast_MAC (All_Routers), Link_Local, All_Routers, ND_Hop_Limit, M);
   end Solicit_Routers;

   --  The link address for an on-link neighbor, if resolved.
   procedure Neighbor_MAC (A : Address; MAC : out Net.MACAddress; Found : out Boolean) is
      Link : ND_Message.MAC;
   begin
      Neighbor_Cache.Lookup (Neighbors, A, Link, Found);
      MAC := [Link (0), Link (1), Link (2), Link (3), Link (4), Link (5)];
   end Neighbor_MAC;

   function Global_Source return Address is
   begin
      for I in SLAAC_Table.Index loop
         if Addresses (I).St = SLAAC_Table.Preferred then
            return Addresses (I).Addr;
         end if;
      end loop;
      return Unspecified;
   end Global_Source;

   procedure Echo (To_MAC : Net.MACAddress; Source, Destination : Address) is
      M : Message_Bytes (0 .. 15) := [others => 0];
   begin
      M (0) := Echo_Request;
      M (4 .. 5) := [Unsigned_8 (Echo_Identifier / 256), Unsigned_8 (Echo_Identifier mod 256)];
      M (7) := 1;
      M (8 .. 15) := [16#43#, 16#75#, 16#42#, 16#69#, 16#74#, 16#76#, 16#36#, 16#21#];  --  "CuBitv6!"
      Send_ICMP (To_MAC, Source, Destination, Default_Hop_Limit, M);
   end Echo;

   ---------------------------------------------------------------------------
   --  Start
   ---------------------------------------------------------------------------
   procedure Start
     (MAC : Net.MACAddress; Secret : SipHash.Key; Now : Unsigned_64;
      Self_Test : Boolean)
   is
      ID : SLAAC_Table.Interface_ID;
   begin
      Our_MAC := MAC;
      Key := Secret;
      ID := SLAAC_Table.Stable_ID (Key, Link_Local_Prefix, 0);
      if SLAAC_Table.Reserved (ID) then
         ID := SLAAC_Table.Stable_ID (Key, Link_Local_Prefix, 1);
      end if;
      Link_Local := With_ID (Link_Local_Prefix, ID);
      LL_State := Tentative;
      LL_Until := Later (Now, DAD_Milliseconds);
      Solicit (Unspecified, Link_Local);
      Test := (if Self_Test then Waiting else Off);
   end Start;

   ---------------------------------------------------------------------------
   --  Receiving
   ---------------------------------------------------------------------------
   --  Another node claims Target, which we are still detecting.
   procedure Claimed (Target : Address) with
     Pre  => SLAAC_Table.Unique (Addresses),
     Post => SLAAC_Table.Unique (Addresses)
   is
   begin
      if LL_State = Tentative and then Target = Link_Local then
         LL_State := Duplicate;
         Log ("netstack: IPv6 " & Image (Target) & " is in use by another node" & ASCII.LF);
      else
         SLAAC_Table.Conflict (Addresses, Target);
      end if;
   end Claimed;

   procedure Handle_Solicitation
     (H : Header; M : ND_Message.Message; From_MAC : Net.MACAddress; Now : Unsigned_64)
   with Pre  => Valid,
        Post => Valid
   is
      Target : constant Address := M.Target;
      Reply_MAC : Net.MACAddress;
      Found : Boolean;
   begin
      if Tentative (Target) then
         --  Another node detecting the same address: ours is a duplicate.
         if H.Source = Unspecified then
            Claimed (Target);
         end if;
         return;
      end if;
      if not Ours (Target) then
         return;
      end if;
      if H.Source = Unspecified then
         Advertise (Multicast_MAC (All_Nodes), All_Nodes, Target, Solicited => False);
         return;
      end if;
      Neighbor_Cache.Learn
        (Neighbors, (Of_Kind => ND_Message.Solicitation, Solicited => False, Ours => True,
                     Peer => H.Source, Has_Link => M.Has_Link, Link => M.Link), Now);
      Neighbor_MAC (H.Source, Reply_MAC, Found);
      Advertise ((if Found then Reply_MAC else From_MAC), H.Source, Target, Solicited => True);
   end Handle_Solicitation;

   procedure Handle_Advertisement (RA : RA_Message.Advertisement; Source : Address;
                                   Now : Unsigned_64)
   with Pre  => Valid and then
                (for all I in 1 .. RA.Count => RA_Message.Usable (RA.Prefixes (I))),
        Post => Valid
   is
      ID : SLAAC_Table.Interface_ID;
   begin
      if RA.Router_Lifetime > 0 then
         Router := Source;
         Have_Router := True;
      end if;
      for I in 1 .. RA.Count loop
         pragma Loop_Invariant (Valid);
         ID := SLAAC_Table.Stable_ID (Key, RA.Prefixes (I).Network, 0);
         if SLAAC_Table.Reserved (ID) then
            ID := SLAAC_Table.Stable_ID (Key, RA.Prefixes (I).Network, 1);
         end if;
         if not SLAAC_Table.Reserved (ID) then
            SLAAC_Table.Advertised (Addresses, RA.Prefixes (I), ID, Seconds (Now));
         end if;
      end loop;
   end Handle_Advertisement;

   --  An ICMPv6 message for us, checksum not yet checked.
   procedure Handle_ICMP
     (H : Header; M : Message_Bytes; From_MAC : Net.MACAddress; Now : Unsigned_64)
   with Pre  => Valid and then M'First = 0 and then
                M'Length in Minimum_Message .. Maximum_Message,
        Post => Valid
   is
      ND : ND_Message.Message;
      RA : RA_Message.Advertisement;
      OK : Boolean;
   begin
      if Checksum (H.Source, H.Destination, M) /= 0 then
         return;
      end if;
      case M (0) is
         when Neighbor_Solicit =>
            ND_Message.Parse (M, H.Hop_Limit, ND, OK);
            if OK then
               Handle_Solicitation (H, ND, From_MAC, Seconds (Now));
            end if;
         when Neighbor_Advert =>
            ND_Message.Parse (M, H.Hop_Limit, ND, OK);
            if not OK then
               return;
            end if;
            if Tentative (ND.Target) then
               Claimed (ND.Target);
               return;
            end if;
            Neighbor_Cache.Learn
              (Neighbors, (Of_Kind => ND_Message.Advertisement, Solicited => ND.Solicited,
                           Ours => False, Peer => ND.Target, Has_Link => ND.Has_Link,
                           Link => ND.Link), Seconds (Now));
         when Router_Advertisement =>
            RA_Message.Parse (M, H.Hop_Limit, H.Source, RA, OK);
            if not OK then
               return;
            end if;
            Handle_Advertisement (RA, H.Source, Now);
            Tick (Now);   --  probe new addresses at once
         when Echo_Request =>
            if Ours (H.Destination) and then H.Source /= Unspecified and then
              not Is_Multicast (H.Source)
            then
               declare
                  Reply : Message_Bytes := M;
                  Reply_MAC : Net.MACAddress;
                  Found : Boolean;
               begin
                  Reply (0) := Echo_Reply;
                  Neighbor_MAC (H.Source, Reply_MAC, Found);
                  Send_ICMP ((if Found then Reply_MAC else From_MAC), H.Destination,
                             H.Source, Default_Hop_Limit, Reply);
               end;
            end if;
         when Echo_Reply =>
            if Test = Sent and then H.Source = Router and then
              M'Length >= Echo_Minimum and then
              M (4) = Unsigned_8 (Echo_Identifier / 256) and then
              M (5) = Unsigned_8 (Echo_Identifier mod 256)
            then
               Test := Done;
               Log ("netstack: IPv6 echo reply from " & Image (H.Source) & ASCII.LF);
            end if;
         when others =>
            null;
      end case;
   end Handle_ICMP;

   procedure Receive (Frame : IPv6_Header.Bytes; Now : Unsigned_64)
   is
   begin
      if Frame'Length < Ethernet_Header + IPv6_Header.Size or else
        Frame'Length > Maximum_Frame or else LL_State = Absent
      then
         return;
      end if;
      declare
         Packet : constant Message_Bytes (0 .. Frame'Length - Ethernet_Header - 1) :=
           Frame (Frame'First + Ethernet_Header .. Frame'Last);
         From_MAC : constant Net.MACAddress :=
           [for K in 0 .. 5 => Frame (Frame'First + 6 + K)];
         H : Header;
         Buffer : Message_Bytes (0 .. Maximum_Message - 1) := [others => 0];
      begin
         if not Well_Formed (Packet) then
            return;
         end if;
         Parse (Packet, H);
         if H.Next_Header /= ICMPv6 or else not For_Us (H.Destination) or else
           H.Payload_Length < Minimum_Message
         then
            return;   --  no IPv6 transport or extension headers yet
         end if;
         --  Copied to a fixed buffer: its length is the header's.
         Buffer (0 .. H.Payload_Length - 1) :=
           Packet (IPv6_Header.Size .. IPv6_Header.Size + H.Payload_Length - 1);
         Handle_ICMP (H, Buffer (0 .. H.Payload_Length - 1), From_MAC, Now);
      end;
   end Receive;

   ---------------------------------------------------------------------------
   --  Timers
   ---------------------------------------------------------------------------
   procedure Tick (Now : Unsigned_64)
   is
      Router_MAC : Net.MACAddress;
      Found : Boolean;
   begin
      if LL_State = Tentative and then Now >= LL_Until then
         LL_State := Usable;
         Log ("netstack: IPv6 link-local " & Image (Link_Local) & ASCII.LF);
         Solicit_Routers;
         Solicits_Sent := 1;
         Next_Solicit := Later (Now, Solicit_Interval);
      end if;
      if LL_State = Usable and then not Have_Router and then Now >= Next_Solicit and then
        Solicits_Sent < Maximum_Retries
      then
         Solicit_Routers;
         Solicits_Sent := Solicits_Sent + 1;
         Next_Solicit := Later (Now, Solicit_Interval);
      end if;
      SLAAC_Table.Tick (Addresses, Seconds (Now));
      for I in SLAAC_Table.Index loop
         pragma Loop_Invariant (Valid);
         case Addresses (I).St is
            when SLAAC_Table.Free =>
               Probed (I) := False;
               Announced (I) := False;
            when SLAAC_Table.Tentative =>
               if not Probed (I) then
                  Solicit (Unspecified, Addresses (I).Addr);
                  Probed (I) := True;
               end if;
            when SLAAC_Table.Preferred =>
               if not Announced (I) then
                  Announced (I) := True;
                  Log ("netstack: IPv6 address " & Image (Addresses (I).Addr) & ASCII.LF);
               end if;
            when SLAAC_Table.Duplicate =>
               if not Announced (I) then
                  Announced (I) := True;
                  Log ("netstack: IPv6 " & Image (Addresses (I).Addr) &
                       " is in use by another node" & ASCII.LF);
               end if;
            when SLAAC_Table.Deprecated =>
               null;
         end case;
      end loop;
      --  The self-test: resolve the router, then ping it once.
      case Test is
         when Waiting =>
            if Have_Router and then Global_Source /= Unspecified and then LL_State = Usable then
               Neighbor_Cache.Solicit (Neighbors, Router, Seconds (Now));
               Solicit (Link_Local, Router);
               Test := Resolving;
               Test_Tries := 1;
               Test_Until := Later (Now, Retry_Milliseconds);
            end if;
         when Resolving =>
            Neighbor_MAC (Router, Router_MAC, Found);
            if Found then
               Echo (Router_MAC, Link_Local, Router);
               Test := Sent;
               Test_Until := Later (Now, Retry_Milliseconds);
            elsif Now >= Test_Until and then Test_Tries < Maximum_Retries then
               Solicit (Link_Local, Router);
               Test_Tries := Test_Tries + 1;
               Test_Until := Later (Now, Retry_Milliseconds);
            end if;
         when Sent =>
            if Now >= Test_Until and then Test_Tries < Maximum_Retries then
               Neighbor_MAC (Router, Router_MAC, Found);
               if Found then
                  Echo (Router_MAC, Link_Local, Router);
               end if;
               Test_Tries := Test_Tries + 1;
               Test_Until := Later (Now, Retry_Milliseconds);
            end if;
         when Off | Done =>
            null;
      end case;
   end Tick;

   function Next_Deadline return Unsigned_64 is
      D : Unsigned_64 := Unsigned_64'Last;
   begin
      if LL_State = Tentative then
         D := LL_Until;
      end if;
      if LL_State = Usable and then not Have_Router and then Solicits_Sent < Maximum_Retries then
         D := Unsigned_64'Min (D, Next_Solicit);
      end if;
      if Test in Resolving | Sent then
         D := Unsigned_64'Min (D, Test_Until);
      end if;
      for I in SLAAC_Table.Index loop
         if Addresses (I).St = SLAAC_Table.Tentative then
            D := Unsigned_64'Min
              (D, (if Addresses (I).DAD_Until > Unsigned_64'Last / Milliseconds
                   then Unsigned_64'Last
                   else Addresses (I).DAD_Until * Milliseconds));
         end if;
      end loop;
      return D;
   end Next_Deadline;

end IPv6_Link;
