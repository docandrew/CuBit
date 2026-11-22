------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The libc's TCP sockets over netstack (docs/servo-port.md,
--  docs/netstack-redesign.md, docs/c-removal.md).
--
--  A socket is a netstack channel reached through one of the program's
--  network endpoints, which exist only for the scopes its manifest requested
--  and the launch approved. On first use the libc asks netstack what each
--  endpoint's scope allows (OP_NET_SCOPE) and routes each connection or
--  listener to one that permits it (CuBit.Libc_Net_Addresses); netstack
--  checks again. Nothing here adds authority.
--
--  Names are not resolved here: getaddrinfo gives a host name a placeholder
--  address (CuBit.Libc_Net_Names), and connecting to it opens
--  "@net:tcp:<name>:<port>", so netstack resolves the name in the scope.
--
--  Each socket's channel is a buffer of a channel arena lent to netstack
--  once: a header page, a send ring and a receive ring (Net_Channel_Layout),
--  moved through with the proved CuBit.Channel_Rings. OPEN and SHUT go
--  through a control queue per endpoint (the proved Submission_Queues
--  client over CuBit.Net_Control_Queues). IPC is needed only to wake an
--  idle side: a KICK when netstack asked for one, and one WAIT at a time,
--  which netstack completes when a socket a thread waits on is ready.
--  Listeners keep sockets offered for arriving connections (datagram
--  records, CuBit.Datagram_Rings).
--
--  No UDP yet.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Interfaces.C;
with System;

package CuBit.Libc_Net is

   subtype int is Interfaces.C.int;
   subtype long is Interfaces.C.long;
   subtype size_t is Interfaces.C.size_t;
   subtype unsigned is Interfaces.C.unsigned;
   subtype unsigned_long is Interfaces.C.unsigned_long;

   function Tcp_New return System.Address
   with Export, Convention => C, External_Name => "__cubit_tcp_new";
   procedure Tcp_Close (Socket : System.Address)
   with Export, Convention => C, External_Name => "__cubit_tcp_close";
   function Tcp_Connect (Socket, Address : System.Address; Length : unsigned;
                         Nonblocking : int) return long
   with Export, Convention => C, External_Name => "__cubit_tcp_connect";
   function Tcp_Read (Socket, Buffer : System.Address; Count : size_t;
                      Nonblocking : int) return long
   with Export, Convention => C, External_Name => "__cubit_tcp_read";
   function Tcp_Write (Socket, Vectors : System.Address; Count : int;
                       Nonblocking : int) return long
   with Export, Convention => C, External_Name => "__cubit_tcp_write";
   function Tcp_Poll (Socket : System.Address; Events : Integer_16) return Integer_16
   with Export, Convention => C, External_Name => "__cubit_tcp_poll";
   function Tcp_Mask (Socket : System.Address) return Unsigned_64
   with Export, Convention => C, External_Name => "__cubit_tcp_mask";
   function Tcp_Bind (Socket, Address : System.Address; Length : unsigned) return long
   with Export, Convention => C, External_Name => "__cubit_tcp_bind";
   function Tcp_Listen (Socket : System.Address; Backlog : int) return long
   with Export, Convention => C, External_Name => "__cubit_tcp_listen";
   function Tcp_Accept (Listener, Result, Address, Length : System.Address;
                        Nonblocking : int) return long
   with Export, Convention => C, External_Name => "__cubit_tcp_accept";
   function Tcp_Local (Socket, Address, Length : System.Address) return long
   with Export, Convention => C, External_Name => "__cubit_tcp_local";
   function Tcp_So_Error (Socket : System.Address) return long
   with Export, Convention => C, External_Name => "__cubit_tcp_so_error";
   function Tcp_Peer (Socket, Address, Length : System.Address) return long
   with Export, Convention => C, External_Name => "__cubit_tcp_peer";
   function Tcp_Shutdown (Socket : System.Address; How : int) return long
   with Export, Convention => C, External_Name => "__cubit_tcp_shutdown";

   --  Block until netstack reports a socket in Mask (or finishes an OPEN),
   --  the readiness futex moves on from Sequence, or the deadline (kernel
   --  milliseconds; all ones: none) passes.
   procedure Net_Wait (Sequence : int; Deadline : unsigned_long; Mask : Unsigned_64)
   with Export, Convention => C, External_Name => "__cubit_net_wait";
   --  A local event while a thread blocks for netstack: end its WAIT.
   procedure Net_Interrupt
   with Export, Convention => C, External_Name => "__cubit_net_interrupt";

   --  musl's getaddrinfo lookup (replaces src/network/lookup_name.c):
   --  numeric addresses and "localhost" are literal; any other name gets a
   --  placeholder address netstack resolves at connect time, in the scope.
   function Lookup_Name (Results, Canonical, Name : System.Address;
                         Family, Flags : int) return int
   with Export, Convention => C, External_Name => "__lookup_name";

end CuBit.Libc_Net;
