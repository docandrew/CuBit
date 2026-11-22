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
   function internetChecksum (data : System.Address;
                              len  : Natural) return Unsigned_16 is
      sum : Unsigned_32 := 0;
      i   : Natural := 0;
   begin
      --  Sum 16-bit words
      while i + 1 < len loop
         sum := sum + Unsigned_32 (getU16BE (data, i));
         i := i + 2;
      end loop;

      --  If odd length, add last byte shifted left
      if i < len then
         sum := sum + Shift_Left (Unsigned_32 (getU8 (data, i)), 8);
      end if;

      --  Fold 32-bit sum to 16 bits
      while (sum and 16#FFFF_0000#) /= 0 loop
         sum := (sum and 16#FFFF#) + Shift_Right (sum, 16);
      end loop;

      return not Unsigned_16 (sum and 16#FFFF#);
   end internetChecksum;

   ---------------------------------------------------------------------------
   --  arpUpdate - add or update an entry in the ARP cache
   ---------------------------------------------------------------------------
   procedure arpUpdate (cache : in out ARPTable;
                        ip    : IPv4Address;
                        mac   : MACAddress) is
      freeSlot : Integer := -1;
   begin
      for i in cache'Range loop
         if cache (i).valid and then cache (i).ip = ip then
            --  Update existing entry
            cache (i).mac := mac;
            return;
         end if;
         if not cache (i).valid and freeSlot < 0 then
            freeSlot := i;
         end if;
      end loop;

      --  Add new entry if room
      if freeSlot >= 0 then
         cache (freeSlot) := (ip => ip, mac => mac, valid => True);
      end if;
   end arpUpdate;

   ---------------------------------------------------------------------------
   --  arpLookup - look up a MAC address by IP in the cache
   ---------------------------------------------------------------------------
   function arpLookup (cache : ARPTable;
                       ip    : IPv4Address;
                       mac   : out MACAddress) return Boolean is
   begin
      for i in cache'Range loop
         if cache (i).valid and then cache (i).ip = ip then
            mac := cache (i).mac;
            return True;
         end if;
      end loop;
      mac := ZERO_MAC;
      return False;
   end arpLookup;

end Net;
