------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Network protocol types and helpers for the virtio-net driver.
--
--  Provides byte-level packet construction helpers, Internet checksum
--  (RFC 1071), and a simple static ARP cache.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;

package Net is

   type MACAddress  is array (0 .. 5) of Unsigned_8;
   type IPv4Address is array (0 .. 3) of Unsigned_8;

   BROADCAST_MAC : constant MACAddress :=
      (16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#);
   ZERO_MAC      : constant MACAddress := (others => 0);

   ETHERTYPE_ARP  : constant Unsigned_16 := 16#0806#;
   ETHERTYPE_IPV4 : constant Unsigned_16 := 16#0800#;

   PROTO_ICMP : constant Unsigned_8 := 1;
   PROTO_TCP  : constant Unsigned_8 := 6;
   PROTO_UDP  : constant Unsigned_8 := 17;

   ARP_REQUEST : constant Unsigned_16 := 1;
   ARP_REPLY   : constant Unsigned_16 := 2;

   ICMP_ECHO_REPLY   : constant Unsigned_8 := 0;
   ICMP_ECHO_REQUEST : constant Unsigned_8 := 8;

   ---------------------------------------------------------------------------
   --  Byte-level packet construction helpers.
   --  All offsets are in bytes from buf.
   ---------------------------------------------------------------------------
   procedure putU8 (buf : System.Address; off : Natural;
                    val : Unsigned_8);

   procedure putU16BE (buf : System.Address; off : Natural;
                       val : Unsigned_16);

   procedure putU32BE (buf : System.Address; off : Natural;
                       val : Unsigned_32);

   procedure putMAC (buf : System.Address; off : Natural;
                     m   : MACAddress);

   procedure putIP (buf : System.Address; off : Natural;
                    ip  : IPv4Address);

   function getU8 (buf : System.Address; off : Natural)
                   return Unsigned_8;

   function getU16BE (buf : System.Address; off : Natural)
                      return Unsigned_16;

   function getU32BE (buf : System.Address; off : Natural)
                      return Unsigned_32;

   procedure getMAC (buf : System.Address; off : Natural;
                     m   : out MACAddress);

   procedure getIP (buf : System.Address; off : Natural;
                    ip  : out IPv4Address);

   ---------------------------------------------------------------------------
   --  Internet checksum (RFC 1071) over len bytes starting at data.
   ---------------------------------------------------------------------------
   function internetChecksum (data : System.Address;
                              len  : Natural) return Unsigned_16;

   ---------------------------------------------------------------------------
   --  Transport-layer checksum (TCP/UDP pseudo-header + segment).
   ---------------------------------------------------------------------------
   function transportChecksum (srcIP   : IPv4Address;
                               dstIP   : IPv4Address;
                               proto   : Unsigned_8;
                               segment : System.Address;
                               segLen  : Natural) return Unsigned_16;

   ---------------------------------------------------------------------------
   --  Simple static ARP cache.
   ---------------------------------------------------------------------------
   MAX_ARP_ENTRIES : constant := 8;

   type ARPEntry is record
      ip    : IPv4Address;
      mac   : MACAddress;
      valid : Boolean := False;
   end record;

   type ARPTable is array (0 .. MAX_ARP_ENTRIES - 1) of ARPEntry;

   procedure arpUpdate (cache : in out ARPTable;
                        ip    : IPv4Address;
                        mac   : MACAddress);

   function arpLookup (cache : ARPTable;
                       ip    : IPv4Address;
                       mac   : out MACAddress) return Boolean;

end Net;
