------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The one network address type (docs/control-language.md, "Locators and
--  authority kinds"): 16 bytes of IPv6, network order. IPv4 is held in the
--  IPv4-mapped form ::ffff:a.b.c.d (RFC 4291 2.5.5.2); there is no address
--  family tag. The family appears only on the wire: a mapped address goes
--  out as IPv4, any other as IPv6, and an IPv6 packet must never carry a
--  mapped address.
--
--  Proved (tests/locators): prefix matching and containment are exact over
--  the 128 bits, and mapping IPv4 in and out round-trips.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Net_Address with Pure, SPARK_Mode is

   subtype Byte_Index is Natural range 0 .. 15;
   type Address is array (Byte_Index) of Unsigned_8;

   subtype Prefix_Length is Natural range 0 .. 128;
   --  An IPv4 prefix length N as a mapped prefix: 96 + N.
   Mapped_Prefix : constant := 96;

   Unspecified : constant Address := [others => 0];

   --  The ::ffff:0:0/96 prefix.
   function Is_Mapped (A : Address) return Boolean is
     ((for all I in 0 .. 9 => A (I) = 0) and then
      A (10) = 16#FF# and then A (11) = 16#FF#);

   function IPv4_Of (A : Address) return Unsigned_32 is
     (Shift_Left (Unsigned_32 (A (12)), 24) or
      Shift_Left (Unsigned_32 (A (13)), 16) or
      Shift_Left (Unsigned_32 (A (14)), 8) or Unsigned_32 (A (15)))
   with Pre => Is_Mapped (A);

   --  a.b.c.d as the integer 16#aabbccdd# (network order).
   function Mapped (IPv4 : Unsigned_32) return Address is
     [0 .. 9 => 0, 10 => 16#FF#, 11 => 16#FF#,
      12 => Unsigned_8 (Shift_Right (IPv4, 24) and 16#FF#),
      13 => Unsigned_8 (Shift_Right (IPv4, 16) and 16#FF#),
      14 => Unsigned_8 (Shift_Right (IPv4, 8) and 16#FF#),
      15 => Unsigned_8 (IPv4 and 16#FF#)]
   with Post => Is_Mapped (Mapped'Result) and then
                IPv4_Of (Mapped'Result) = IPv4;

   --  Byte I's bits that a prefix of Length covers, as a mask.
   function Byte_Mask
     (Length : Prefix_Length; I : Byte_Index) return Unsigned_8 is
     (if Length >= 8 * (I + 1) then 16#FF#
      elsif Length <= 8 * I then 0
      else Shift_Left (Unsigned_8'Last, 8 - (Length - 8 * I)));

   --  A lies in Network/Length.
   function Matches
     (A, Network : Address; Length : Prefix_Length) return Boolean is
     (for all I in Byte_Index =>
        (A (I) and Byte_Mask (Length, I)) =
        (Network (I) and Byte_Mask (Length, I)));

   --  Network has no bits set past its prefix (a canonical network).
   function Canonical
     (Network : Address; Length : Prefix_Length) return Boolean is
     (for all I in Byte_Index =>
        (Network (I) and not Byte_Mask (Length, I)) = 0);

   --  Inner/Inner_Length lies within Outer/Outer_Length.
   function Contains
     (Outer : Address; Outer_Length : Prefix_Length;
      Inner : Address; Inner_Length : Prefix_Length) return Boolean is
     (Inner_Length >= Outer_Length and then
      Matches (Inner, Outer, Outer_Length));

end CuBit.Net_Address;
