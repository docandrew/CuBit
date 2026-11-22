------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The endpoint's TCP side over netstack's channels: one listener on each
--  IPv4 address netstack's interfaces are configured with (Ethernet and
--  Wi-Fi alike), on the one port the launch parameters name, and one
--  connection served at a time.
--
--  @description
--  netstack listens only on an address one of its interfaces holds, so the
--  endpoint asks which those are (Interface_Next / Interface_Detail, the
--  inspection operations any holder of netstack's endpoint may use) and
--  follows changes (a DHCP lease, a Wi-Fi join) with Refresh. netstack's
--  interface events go to netmgr only, so Refresh is called on a deadline
--  (Main: Address_Refresh_Ms), not on an event.
--
--  Each listener has its own connection buffer, offered while no
--  connection uses it, so a connection on any address is accepted.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces;

package Remote_Transport is
   Maximum_Listeners : constant := 4;
   subtype Listener_Count is Natural range 0 .. Maximum_Listeners;
   subtype Listener_Index is Positive range 1 .. Maximum_Listeners;
   Maximum_Address_Text : constant := 15;   --  "255.255.255.255"

   procedure Initialize (Success : out Boolean);

   --  Listen on Port at every configured address; close listeners whose
   --  address went away. Changed: the set of addresses changed.
   procedure Refresh (Port : Interfaces.Unsigned_16; Changed : out Boolean);
   function Listening return Listener_Count;
   function Address (Index : Listener_Index) return String;

   --  Wait for a connection until Deadline (monotonic milliseconds).
   procedure Accept_Connection (Deadline : Interfaces.Unsigned_64; Success : out Boolean);
   --  The last accepted connection's peer: "a.b.c.d" ("" when not IPv4).
   function Peer return String;
   --  Exactly Data'Length bytes, by Deadline.
   procedure Read_Exact
     (Data : out String; Deadline : Interfaces.Unsigned_64; Success : out Boolean);
   procedure Write_All (Data : String; Deadline : Interfaces.Unsigned_64; Success : out Boolean);
   procedure Close_Connection;
   procedure Close;
end Remote_Transport;
