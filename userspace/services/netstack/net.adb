------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Network protocol helpers - body.
------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;

package body Net is

   ---------------------------------------------------------------------------
   --  putU8
   ---------------------------------------------------------------------------
   procedure putU8 (buf : System.Address; off : Natural;
                    val : Unsigned_8) is
      b : Unsigned_8 with Import, Address => buf + Storage_Offset (off);
   begin
      b := val;
   end putU8;

   ---------------------------------------------------------------------------
   --  putU16BE - write a 16-bit value in big-endian (network byte order)
   ---------------------------------------------------------------------------
   procedure putU16BE (buf : System.Address; off : Natural;
                       val : Unsigned_16) is
      hi : Unsigned_8 with
         Import, Address => buf + Storage_Offset (off);
      lo : Unsigned_8 with
         Import, Address => buf + Storage_Offset (off + 1);
   begin
      hi := Unsigned_8 (Shift_Right (val, 8) and 16#FF#);
      lo := Unsigned_8 (val and 16#FF#);
   end putU16BE;

   ---------------------------------------------------------------------------
   --  putU32BE - write a 32-bit value in big-endian
   ---------------------------------------------------------------------------
   procedure putU32BE (buf : System.Address; off : Natural;
                       val : Unsigned_32) is
   begin
      putU8 (buf, off,     Unsigned_8 (Shift_Right (val, 24) and 16#FF#));
      putU8 (buf, off + 1, Unsigned_8 (Shift_Right (val, 16) and 16#FF#));
      putU8 (buf, off + 2, Unsigned_8 (Shift_Right (val, 8) and 16#FF#));
      putU8 (buf, off + 3, Unsigned_8 (val and 16#FF#));
   end putU32BE;

   ---------------------------------------------------------------------------
   --  putMAC
   ---------------------------------------------------------------------------
   procedure putMAC (buf : System.Address; off : Natural;
                     m   : MACAddress) is
   begin
      for i in m'Range loop
         putU8 (buf, off + i, m (i));
      end loop;
   end putMAC;

   ---------------------------------------------------------------------------
   --  putIP
   ---------------------------------------------------------------------------
   procedure putIP (buf : System.Address; off : Natural;
                    ip  : IPv4Address) is
   begin
      for i in ip'Range loop
         putU8 (buf, off + i, ip (i));
      end loop;
   end putIP;

   ---------------------------------------------------------------------------
   --  getU8
   ---------------------------------------------------------------------------
   function getU8 (buf : System.Address; off : Natural)
                   return Unsigned_8 is
      b : Unsigned_8 with Import, Address => buf + Storage_Offset (off);
   begin
      return b;
   end getU8;

   ---------------------------------------------------------------------------
   --  getU16BE - read a 16-bit big-endian value
   ---------------------------------------------------------------------------
   function getU16BE (buf : System.Address; off : Natural)
                      return Unsigned_16 is
      hi : Unsigned_8 with
         Import, Address => buf + Storage_Offset (off);
      lo : Unsigned_8 with
         Import, Address => buf + Storage_Offset (off + 1);
   begin
      return Shift_Left (Unsigned_16 (hi), 8) or Unsigned_16 (lo);
   end getU16BE;

   ---------------------------------------------------------------------------
   --  getU32BE - read a 32-bit big-endian value
   ---------------------------------------------------------------------------
   function getU32BE (buf : System.Address; off : Natural)
                      return Unsigned_32 is
   begin
      return Shift_Left (Unsigned_32 (getU8 (buf, off)), 24) or
             Shift_Left (Unsigned_32 (getU8 (buf, off + 1)), 16) or
             Shift_Left (Unsigned_32 (getU8 (buf, off + 2)), 8) or
             Unsigned_32 (getU8 (buf, off + 3));
   end getU32BE;

   ---------------------------------------------------------------------------
   --  getMAC
   ---------------------------------------------------------------------------
   procedure getMAC (buf : System.Address; off : Natural;
                     m   : out MACAddress) is
   begin
      for i in m'Range loop
         m (i) := getU8 (buf, off + i);
      end loop;
   end getMAC;

   ---------------------------------------------------------------------------
   --  getIP
   ---------------------------------------------------------------------------
   procedure getIP (buf : System.Address; off : Natural;
                    ip  : out IPv4Address) is
   begin
      for i in ip'Range loop
         ip (i) := getU8 (buf, off + i);
      end loop;
   end getIP;

   ---------------------------------------------------------------------------
   --  internetChecksum - RFC 1071 one's complement checksum
   ---------------------------------------------------------------------------
   --  RFC 1071: the one's-complement sum is independent of byte order, so
   --  sum 32-bit little-endian words into 64 bits, fold, and swap the two
   --  bytes once at the end (2.2 and 2.3 there).
   function nativeSum (data : System.Address; len : Natural;
                       initial : Unsigned_64) return Unsigned_64 is
      --  Ones'-complement addition of 64-bit words (the end-around carry
      --  is added back), folded to 16 bits later: the same sum as 16-bit
      --  words (RFC 1071 2 (B)), with two accumulators to overlap work.
      type Quads is array (0 .. len / 8 - 1) of Unsigned_64;
      q    : Quads with Import, Address => data;
      rest : constant Natural := len mod 8;
      type Tail is array (0 .. 6) of Unsigned_8;
      t    : Tail with Import, Address => data + Storage_Offset (len - rest);
      acc0, acc1 : Unsigned_64 := 0;
      last : Unsigned_64 := 0;

      procedure add (acc : in out Unsigned_64; x : Unsigned_64) with Inline is
         s : constant Unsigned_64 := acc + x;
      begin
         acc := s + (if s < x then 1 else 0);
      end add;
   begin
      for k in 0 .. q'Length / 2 - 1 loop
         add (acc0, q (2 * k));
         add (acc1, q (2 * k + 1));
      end loop;
      if q'Length mod 2 = 1 then
         add (acc0, q (q'Last));
      end if;
      for k in 0 .. rest - 1 loop
         last := last or Shift_Left (Unsigned_64 (t (k)), 8 * k);
      end loop;
      add (acc0, last);
      add (acc0, acc1);
      add (acc0, initial);
      return acc0;
   end nativeSum;

   --  Fold a native sum to 16 bits, complement it, and put it in network order.
   function finish (sum : Unsigned_64) return Unsigned_16 is
      s : Unsigned_64 := sum;
      r : Unsigned_16;
   begin
      while s > 16#FFFF# loop
         s := (s and 16#FFFF#) + Shift_Right (s, 16);
      end loop;
      r := not Unsigned_16 (s);
      return Shift_Left (r, 8) or Shift_Right (r, 8);
   end finish;

   function internetChecksum (data : System.Address;
                              len  : Natural) return Unsigned_16 is
   begin
      return finish (nativeSum (data, len, 0));
   end internetChecksum;


   ---------------------------------------------------------------------------
   --  transportChecksum - RFC 1071 checksum over pseudo-header + segment
   ---------------------------------------------------------------------------
   function transportChecksum (srcIP   : IPv4Address;
                               dstIP   : IPv4Address;
                               proto   : Unsigned_8;
                               segment : System.Address;
                               segLen  : Natural) return Unsigned_16 is
      --  The 12-byte pseudo-header, in network order.
      pseudoHdr : constant array (0 .. 11) of Unsigned_8 :=
        [srcIP (0), srcIP (1), srcIP (2), srcIP (3),
         dstIP (0), dstIP (1), dstIP (2), dstIP (3),
         0, proto,
         Unsigned_8 (Shift_Right (Unsigned_16 (segLen), 8)),
         Unsigned_8 (Unsigned_16 (segLen) and 16#FF#)];
   begin
      return finish (nativeSum (segment, segLen, nativeSum (pseudoHdr'Address, 12, 0)));
   end transportChecksum;

   ---------------------------------------------------------------------------
   --  packIPv4
   ---------------------------------------------------------------------------
   function packIPv4 (addr : IPv4Address) return Unsigned_64 is
   begin
      return Unsigned_64 (addr (0)) or
             Shift_Left (Unsigned_64 (addr (1)), 8) or
             Shift_Left (Unsigned_64 (addr (2)), 16) or
             Shift_Left (Unsigned_64 (addr (3)), 24);
   end packIPv4;

   ---------------------------------------------------------------------------
   --  unpackIPv4
   ---------------------------------------------------------------------------
   function unpackIPv4 (packed : Unsigned_64) return IPv4Address is
   begin
      return (Unsigned_8 (packed and 16#FF#),
              Unsigned_8 (Shift_Right (packed, 8) and 16#FF#),
              Unsigned_8 (Shift_Right (packed, 16) and 16#FF#),
              Unsigned_8 (Shift_Right (packed, 24) and 16#FF#));
   end unpackIPv4;

   ---------------------------------------------------------------------------
   --  matchesPrefix
   ---------------------------------------------------------------------------
   function matchesPrefix (ip      : IPv4Address;
                           network : IPv4Address;
                           prefix  : Natural) return Boolean is
      ipPacked  : Unsigned_32;
      netPacked : Unsigned_32;
      mask      : Unsigned_32;
   begin
      if prefix = 0 then
         return True;
      end if;
      if prefix > 32 then
         return False;
      end if;

      ipPacked := Shift_Left (Unsigned_32 (ip (0)), 24) or
                  Shift_Left (Unsigned_32 (ip (1)), 16) or
                  Shift_Left (Unsigned_32 (ip (2)), 8) or
                  Unsigned_32 (ip (3));
      netPacked := Shift_Left (Unsigned_32 (network (0)), 24) or
                   Shift_Left (Unsigned_32 (network (1)), 16) or
                   Shift_Left (Unsigned_32 (network (2)), 8) or
                   Unsigned_32 (network (3));

      mask := Shift_Left (16#FFFF_FFFF#, 32 - prefix);
      return (ipPacked and mask) = (netPacked and mask);
   end matchesPrefix;

end Net;
