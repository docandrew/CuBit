------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The libc's network scopes and addresses (docs/c-removal.md): the single
--  16-byte address type (IPv4 as ::ffff:a.b.c.d), the scopes netstack
--  reports for each of the program's network endpoints (OP_NET_SCOPE), and
--  which scope allows a connection or listener. Nothing here adds
--  authority: netstack checks every OPEN against its scope again.
--
--  @description
--  Proved (tests/libc-ada): every index stays in range, a scope reported
--  with a prefix longer than an address is refused, and Allows holds only
--  for the scope's action, port range and network (or, for a host name,
--  only when the scope may name hosts).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Libc_Net_Addresses with Pure, SPARK_Mode is

   Address_Bytes : constant := 16;
   Address_Bits  : constant := 8 * Address_Bytes;
   subtype Address_Index is Natural range 0 .. Address_Bytes - 1;
   type Address is array (Address_Index) of Unsigned_8;
   subtype Prefix_Length is Natural range 0 .. Address_Bits;

   --  An IPv4 address as its four bytes in network order (sin_addr).
   type Octets is array (1 .. 4) of Unsigned_8;
   IPv4_Mapped_Prefix : constant := 96;
   IPv4_At : constant := 12;

   --  ::ffff:a.b.c.d
   function Mapped (IPv4 : Octets) return Address is
     ([0 .. 9 => 0, 10 | 11 => 16#FF#,
       12 => IPv4 (1), 13 => IPv4 (2), 14 => IPv4 (3), 15 => IPv4 (4)]);

   function IPv4_Of (A : Address) return Octets is
     ([A (12), A (13), A (14), A (15)]);

   --  A lies in Network/Prefix.
   function Matches (A, Network : Address; Prefix : Prefix_Length) return Boolean;

   --  Scope actions (CuBit.Network_Authority.Operation).
   Connect_TCP : constant := 1;
   Listen_TCP  : constant := 2;
   Connect_UDP : constant := 3;

   type Scope is record
      Slot    : Natural range 0 .. 63 := 0;   --  the endpoint's capability slot
      Action  : Unsigned_8 := 0;
      Prefix  : Prefix_Length := 0;
      Network : Address := [others => 0];
      First, Last : Unsigned_16 := 0;         --  ports
      Resolve : Boolean := False;             --  may name hosts
   end record;

   --  OP_NET_SCOPE's reply: words 0 and 1 the network (byte 0 of the
   --  address is the low byte of word 0), word 2 the descriptor (ports,
   --  prefix, action, DNS). Valid is False for an unusable prefix.
   procedure Decode
     (Slot : Natural; Word_0, Word_1, Descriptor : Unsigned_64;
      Result : out Scope; Valid : out Boolean)
   with Pre => Slot <= 63,
        Post => (if Valid then Result.Slot = Slot);

   --  Whether S allows Action on port Port to Target, or to a host name
   --  when Named.
   function Allows (S : Scope; Action : Unsigned_8; Named : Boolean;
                    Target : Address; Port : Unsigned_16) return Boolean is
     (S.Action = Action and then Port in S.First .. S.Last
      and then (if Named then S.Resolve else Matches (Target, S.Network, S.Prefix)));

end CuBit.Libc_Net_Addresses;
