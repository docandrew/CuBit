------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Protocol constants shared by the TCP units, each named after its RFC
--  meaning.
------------------------------------------------------------------------------
package TCP_Limits with SPARK_Mode, Pure is

   --  Headers without options (RFC 791 3.1, RFC 8200 3, RFC 9293 3.1).
   IPv4_Header_Size : constant := 20;
   IPv6_Header_Size : constant := 40;
   TCP_Header_Size  : constant := 20;

   --  Smallest MTUs a link may have for each IP version: every IPv4 host
   --  reassembles 576 octets (RFC 791 3.1); IPv6 links carry 1280 (RFC 8200 5).
   Minimum_IPv4_MTU : constant := 576;
   Minimum_IPv6_MTU : constant := 1280;
   Maximum_MTU      : constant := 65_535;

   --  MSS assumed when the peer sends none (RFC 9293 3.7.1).
   Default_IPv4_MSS : constant := Minimum_IPv4_MTU - IPv4_Header_Size - TCP_Header_Size;
   Default_IPv6_MSS : constant := Minimum_IPv6_MTU - IPv6_Header_Size - TCP_Header_Size;

   --  The 16-bit window field (RFC 9293 3.1) and its largest scaling
   --  shift (RFC 7323 2.3).
   Maximum_Window_Field  : constant := 2 ** 16 - 1;
   Maximum_Window_Shift  : constant := 14;
   --  Scaled windows stay below this (RFC 7323 2.3: less than 2**30).
   Maximum_Scaled_Window : constant := 2 ** 30;

   --  A SYN and a FIN each take one sequence number (RFC 9293 3.4).
   Control_Octets : constant := 2;

   --  Duplicate ACKs that signal a loss (RFC 5681 3.2, RFC 6675 2).
   Duplicate_Threshold : constant := 3;
end TCP_Limits;
