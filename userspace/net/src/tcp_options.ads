------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  TCP option negotiation: MSS (RFC 9293 3.7.1, RFC 6691), window scaling
--  and timestamps (RFC 7323), SACK-permitted (RFC 2018).
--
--  The RecordFlux TCP parser reads the options off the wire; this decides
--  what a connection uses from its SYN exchange, and converts windows
--  between the 16-bit field and bytes.
--
--  To be proved (tests/net-tcp): an option is in effect only if both SYNs
--  carried it (RFC 7323 2.2, 3.2; RFC 2018 2); a peer's shift above 14 is
--  used as 14 (RFC 7323 2.3); segments fit both the peer's MSS (536 or
--  1220 when it sent none) and our link, less the timestamp option, but
--  never below Minimum_MSS (a floor against tiny-MSS resource attacks, as
--  Linux's tcp_min_snd_mss); a scaled window stays below 2**30; the window
--  we advertise never promises more buffer than we have, and falls short
--  of it by less than one scale unit.
--
--  Options in segments other than SYNs (MSS, window scale,
--  SACK-permitted) are ignored by the connection engine, not here.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with TCP_Limits; use TCP_Limits;

package TCP_Options with SPARK_Mode, Pure is

   Maximum_Shift  : constant := Maximum_Window_Shift;
   --  Floor against tiny-MSS resource attacks (Linux's tcp_min_snd_mss
   --  is 48; ours leaves room for the timestamp option).
   Minimum_MSS    : constant := 64;
   Timestamp_Size : constant := 12;   --  the option, padded (RFC 7323 A)
   --  Our offered shift: 2**7 * 65535 bytes, about 8 MiB of window.
   Default_Shift  : constant := 7;

   subtype Shift is Natural range 0 .. Maximum_Shift;
   subtype Link_MTU is Unsigned_32 range Minimum_IPv4_MTU .. Maximum_MTU;
   subtype MSS_Value is Unsigned_32 range Minimum_MSS .. Maximum_MTU;
   subtype Window_Bytes is Unsigned_32 range 0 .. Maximum_Scaled_Window;

   --  RFC 9293 3.7.1: assumed when the peer sends no MSS option.
   function Default_MSS (IPv6 : Boolean) return MSS_Value is
     (if IPv6 then Default_IPv6_MSS else Default_IPv4_MSS);

   --  The MSS to offer for a link: MTU less fixed IP and TCP headers.
   function MSS_For (MTU : Link_MTU; IPv6 : Boolean) return MSS_Value is
     (MTU - (if IPv6 then IPv6_Header_Size else IPv4_Header_Size) - TCP_Header_Size)
   with Pre => (if IPv6 then MTU >= Minimum_IPv6_MTU);

   --  Options as parsed from one segment.
   type Received is record
      Has_MSS          : Boolean := False;
      MSS              : Unsigned_16 := 0;
      Has_Window_Scale : Boolean := False;
      Shift_Count      : Unsigned_8 := 0;
      SACK_Permitted   : Boolean := False;
      Has_Timestamps   : Boolean := False;
      TSval, TSecr     : Unsigned_32 := 0;
   end record;

   --  What we put in our SYN.
   type Offer is record
      MSS            : MSS_Value := Default_IPv4_MSS;
      Window_Scale   : Boolean := True;
      Shift_Count    : Shift := Default_Shift;
      SACK_Permitted : Boolean := True;
      Timestamps     : Boolean := True;
   end record;

   --  What the connection uses.
   type Agreement is record
      Send_MSS   : MSS_Value := Default_IPv4_MSS;   --  most data in one segment
      Scaling    : Boolean := False;
      Snd_Shift  : Shift := 0;         --  applies to the peer's windows
      Rcv_Shift  : Shift := 0;         --  applies to ours
      SACK       : Boolean := False;
      Timestamps : Boolean := False;
   end record;

   function Peer_MSS (Peer : Received; IPv6 : Boolean) return Unsigned_32 is
     (if Peer.Has_MSS then Unsigned_32 (Peer.MSS) else Default_MSS (IPv6));

   --  Ours: our SYN. Peer: the peer's SYN (or SYN-ACK). Link_MSS: what our
   --  path allows (MSS_For of the route's MTU).
   function Negotiate (Ours : Offer; Peer : Received; Link_MSS : MSS_Value;
                       IPv6 : Boolean) return Agreement
   with Post =>
     Negotiate'Result.Scaling = (Ours.Window_Scale and then Peer.Has_Window_Scale) and then
     (if Negotiate'Result.Scaling then
        Negotiate'Result.Rcv_Shift = Ours.Shift_Count and then
        Negotiate'Result.Snd_Shift =
          (if Peer.Shift_Count > Maximum_Shift then Maximum_Shift
           else Natural (Peer.Shift_Count))
      else Negotiate'Result.Rcv_Shift = 0 and then Negotiate'Result.Snd_Shift = 0) and then
     Negotiate'Result.SACK = (Ours.SACK_Permitted and then Peer.SACK_Permitted) and then
     Negotiate'Result.Timestamps = (Ours.Timestamps and then Peer.Has_Timestamps) and then
     Negotiate'Result.Send_MSS <= Link_MSS and then
     (Negotiate'Result.Send_MSS = Minimum_MSS or else
      Negotiate'Result.Send_MSS +
        (if Negotiate'Result.Timestamps then Timestamp_Size else 0) <=
          Unsigned_32'Min (Peer_MSS (Peer, IPv6), Link_MSS)) and then
     --  ... and as large as that allows.
     Negotiate'Result.Send_MSS + (if Negotiate'Result.Timestamps then Timestamp_Size else 0) >=
       Unsigned_32'Min (Unsigned_32'Min (Peer_MSS (Peer, IPv6), Link_MSS),
                        Minimum_MSS + (if Negotiate'Result.Timestamps then Timestamp_Size else 0));

   --  A window field in bytes.
   function Scaled (Raw : Unsigned_16; S : Shift) return Window_Bytes is
     (Shift_Left (Unsigned_32 (Raw), S));

   --  The peer's window: SYN segments are never scaled (RFC 7323 2.2).
   function Peer_Window (Raw : Unsigned_16; SYN : Boolean; A : Agreement)
     return Window_Bytes
   is (if SYN then Unsigned_32 (Raw) else Scaled (Raw, A.Snd_Shift));

   --  The window field advertising Space bytes, rounded down.
   function Advertise (Space : Window_Bytes; S : Shift) return Unsigned_16 is
     (Unsigned_16 (Unsigned_32'Min (Shift_Right (Space, S), Maximum_Window_Field)))
   with Post => Scaled (Advertise'Result, S) <= Space and then
                (Advertise'Result = Maximum_Window_Field or else
                 Space - Scaled (Advertise'Result, S) < Shift_Left (1, S));
end TCP_Options;
