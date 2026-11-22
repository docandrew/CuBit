------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  An ICMP error for IPv4 (RFC 792, RFC 1191, RFC 1122 4.2.3.9): what it
--  says, and which of our packets it quotes. The message's checksum is the
--  caller's; so is checking that the quoted packet was really ours and
--  belongs to a live connection (RFC 5927: a forged error must name the
--  addresses, ports and an in-window sequence number).
--
--  Accepted: Destination Unreachable (3) or Time Exceeded (11); the eight
--  ICMP header bytes, then a quoted IPv4 header (version 4, 20 to 60
--  bytes) and at least the first eight bytes of its transport header.
--
--  Kinds:
--  - Too_Big: fragmentation needed with DF set (3/4). Next_Hop_MTU is
--    the router's figure; 0 means an old router did not say.
--  - Hard: protocol or port unreachable (3/2, 3/3), or communication
--    administratively prohibited (3/9, 3/10, 3/13). The peer will not
--    answer; a connection attempt ends.
--  - Soft: anything else (network or host unreachable, TTL exceeded),
--    which may be transient. Retransmission goes on.
--
--  Proved (tests/net-tcp): no access outside the message for any bytes;
--  an accepted message has the stated type and a quoted header inside it,
--  and every returned field is its bytes on the wire.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with IPv4_Header;

package ICMPv4_Error with SPARK_Mode is

   use type IPv4_Header.Address;

   subtype Bytes is IPv4_Header.Bytes;
   subtype Address is IPv4_Header.Address;

   Destination_Unreachable : constant := 3;
   Time_Exceeded           : constant := 11;

   Protocol_Unreachable    : constant := 2;
   Port_Unreachable        : constant := 3;
   Fragmentation_Needed    : constant := 4;
   Network_Prohibited      : constant := 9;
   Host_Prohibited         : constant := 10;
   Administratively_Prohibited : constant := 13;

   ICMP_Header_Size : constant := 8;
   Quoted_Transport : constant := 8;   --  ports, and TCP's sequence number
   Quoted_At        : constant := ICMP_Header_Size;
   Maximum_Message  : constant := 2 ** 16;

   type Kind is (Soft, Hard, Too_Big);

   type Error is record
      Of_Kind          : Kind := Soft;
      Code             : Unsigned_8 := 0;
      Next_Hop_MTU     : Unsigned_16 := 0;
      Protocol         : Unsigned_8 := 0;
      Source           : Address := [others => 0];   --  the quoted packet's
      Destination      : Address := [others => 0];
      Source_Port      : Unsigned_16 := 0;
      Destination_Port : Unsigned_16 := 0;
      Sequence         : Unsigned_32 := 0;           --  TCP's; else bytes 4 .. 7
   end record;

   function U16 (B : Bytes; I : Natural) return Unsigned_16 is
     (Shift_Left (Unsigned_16 (B (I)), 8) or Unsigned_16 (B (I + 1)))
   with Pre => I >= B'First and then I < B'Last;

   function U32 (B : Bytes; I : Natural) return Unsigned_32 is
     (Shift_Left (Unsigned_32 (B (I)), 24) or Shift_Left (Unsigned_32 (B (I + 1)), 16) or
      Shift_Left (Unsigned_32 (B (I + 2)), 8) or Unsigned_32 (B (I + 3)))
   with Pre => I >= B'First and then B'Last >= 3 and then I <= B'Last - 3;

   function Classify (Message_Type, Code : Unsigned_8) return Kind is
     (if Message_Type = Destination_Unreachable and then Code = Fragmentation_Needed
      then Too_Big
      elsif Message_Type = Destination_Unreachable and then
        Code in Protocol_Unreachable | Port_Unreachable | Network_Prohibited |
                Host_Prohibited | Administratively_Prohibited
      then Hard
      else Soft);

   --  The quoted header's size (its IHL), from the byte after the ICMP
   --  header.
   function Quoted_Size (B : Bytes) return Natural is
     (Natural (B (Quoted_At) and 16#0F#) * 4)
   with Pre => B'First = 0 and then B'Length > Quoted_At;

   procedure Parse (Message : Bytes; E : out Error; OK : out Boolean) with
     Pre  => Message'First = 0 and then Message'Length <= Maximum_Message,
     Post => (if OK then
                Message (0) in Destination_Unreachable | Time_Exceeded and then
                Message'Length > Quoted_At and then
                Shift_Right (Message (Quoted_At), 4) = 4 and then
                Quoted_Size (Message) >= IPv4_Header.Minimum_Size and then
                Quoted_At + Quoted_Size (Message) + Quoted_Transport <= Message'Length and then
                E.Of_Kind = Classify (Message (0), Message (1)) and then
                --  Stated on their own: an unreachable port or protocol is
                --  final, a too-big names its MTU, time exceeded is transient.
                (if Message (0) = 3 and then Message (1) in 2 | 3 then E.Of_Kind = Hard) and then
                (if Message (0) = 3 and then Message (1) = 4 then E.Of_Kind = Too_Big) and then
                (if Message (0) = 11 then E.Of_Kind = Soft) and then
                E.Code = Message (1) and then
                E.Next_Hop_MTU = U16 (Message, 6) and then
                E.Protocol = Message (Quoted_At + 9) and then
                E.Source = [Message (Quoted_At + 12), Message (Quoted_At + 13),
                            Message (Quoted_At + 14), Message (Quoted_At + 15)] and then
                E.Destination = [Message (Quoted_At + 16), Message (Quoted_At + 17),
                                 Message (Quoted_At + 18), Message (Quoted_At + 19)] and then
                E.Source_Port = U16 (Message, Quoted_At + Quoted_Size (Message)) and then
                E.Destination_Port = U16 (Message, Quoted_At + Quoted_Size (Message) + 2) and then
                E.Sequence = U32 (Message, Quoted_At + Quoted_Size (Message) + 4));

end ICMPv4_Error;
