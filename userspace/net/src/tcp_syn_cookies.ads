------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  SYN cookies (RFC 4987 3.6): when a listener's half-open queue is full,
--  answer a SYN without keeping state, encoding what the connection needs
--  in our initial sequence number, and rebuild it from the ACK.
--
--  Cookie layout (32 bits): a 5-bit counter (64-second periods, modulo
--  32), a 3-bit MSS index, and 24 bits of SipHash-2-4 over the 4-tuple,
--  the client's ISN and the counter.
--
--  To be proved (tests/net-tcp): a cookie we made is accepted for the
--  same 4-tuple and client ISN within one following period and returns the
--  MSS we encoded (the largest table entry not above the client's, or
--  536); a cookie older than that is refused. That a forged cookie is
--  refused except with probability 2**-24 per guess rests on SipHash
--  (assumed). Window scaling and SACK are not encoded (they need
--  timestamps, as Linux does), so cookie connections run without them.
------------------------------------------------------------------------------
with Interfaces;   use Interfaces;
with TCP_Sequence; use TCP_Sequence;
with SipHash;
with TCP_Isn;

package TCP_Syn_Cookies with SPARK_Mode is

   Period : constant := 64;   --  seconds per counter step

   --  Cookie layout, from the top bit down: counter, MSS index, tag.
   Counter_Bits   : constant := 5;
   Index_Bits     : constant := 3;
   Tag_Bits       : constant := 24;
   Index_Shift    : constant := Tag_Bits;
   Counter_Shift  : constant := Tag_Bits + Index_Bits;
   Counter_Steps  : constant := 2 ** Counter_Bits;
   Index_Mask     : constant := 2 ** Index_Bits - 1;
   Tag_Mask       : constant := 2 ** Tag_Bits - 1;
   --  A cookie is accepted in its own period and the next one.
   Periods_Valid  : constant := 1;

   subtype MSS_Index is Unsigned_32 range 0 .. Index_Mask;
   --  Common MSS values, ascending: RFC 9293's IPv4 default, the IPv6
   --  minimum link less headers, tunnels and VPNs, PPPoE, Ethernet
   --  (1500 - 40), and two jumbo frame sizes.
   MSS_Table : constant array (MSS_Index) of Unsigned_32 :=
     [536, 1220, 1300, 1400, 1440, 1460, 4312, 8960];

   --  The largest entry not above MSS (entry 0 if none is).
   function Index_For (MSS : Unsigned_32) return MSS_Index
   with Post => (Index_For'Result = 0 or else MSS_Table (Index_For'Result) <= MSS) and then
                (for all I in Index_For'Result + 1 .. MSS_Index'Last => MSS_Table (I) > MSS);

   function Counter (Now_Seconds : Unsigned_64) return Unsigned_32 is
     (Unsigned_32 ((Now_Seconds / Period) mod Counter_Steps));

   function Tag (K : SipHash.Key; E : TCP_Isn.Endpoints; Client_ISN : Seq;
                 T : Unsigned_32) return Unsigned_32
   with Post => Tag'Result <= Tag_Mask;

   --  The cookie as a 32-bit word: counter, MSS index, tag.
   function Cookie_Word (K : SipHash.Key; E : TCP_Isn.Endpoints; Client_ISN : Seq;
                         Now_Seconds : Unsigned_64; MSS : Unsigned_32) return Unsigned_32
   is (Shift_Left (Counter (Now_Seconds), Counter_Shift) or
       Shift_Left (Index_For (MSS), Index_Shift) or
       Tag (K, E, Client_ISN, Counter (Now_Seconds)));

   function Make (K : SipHash.Key; E : TCP_Isn.Endpoints; Client_ISN : Seq;
                  Now_Seconds : Unsigned_64; MSS : Unsigned_32) return Seq
   is (Seq (Cookie_Word (K, E, Client_ISN, Now_Seconds, MSS)));

   --  Cookie is the ACK's acknowledgement number less one; Client_ISN its
   --  sequence number less one.
   function Accepts (K : SipHash.Key; E : TCP_Isn.Endpoints; Client_ISN, Cookie : Seq;
                     Now_Seconds : Unsigned_64) return Boolean
   is ((Counter (Now_Seconds) - Shift_Right (Unsigned_32 (Cookie), Counter_Shift))
         mod Counter_Steps <= Periods_Valid
       and then (Unsigned_32 (Cookie) and Tag_Mask) =
                  Tag (K, E, Client_ISN, Shift_Right (Unsigned_32 (Cookie), Counter_Shift)));

   function Cookie_MSS (Cookie : Seq) return Unsigned_32 is
     (MSS_Table (Shift_Right (Unsigned_32 (Cookie), Index_Shift) and Index_Mask));

   procedure Lemma_Round_Trip (K : SipHash.Key; E : TCP_Isn.Endpoints; Client_ISN : Seq;
                               Sent, Now : Unsigned_64; MSS : Unsigned_32)
   with Ghost, Global => null,
        Pre  => Now >= Sent and then Now / Period - Sent / Period <= Periods_Valid,
        Post => Accepts (K, E, Client_ISN, Make (K, E, Client_ISN, Sent, MSS), Now) and then
                Cookie_MSS (Make (K, E, Client_ISN, Sent, MSS)) = MSS_Table (Index_For (MSS));

   --  Too old: past Periods_Valid, until the counter wraps (after
   --  Counter_Steps periods; the tag's counter input still separates
   --  those except by chance).
   procedure Lemma_Expired (K : SipHash.Key; E : TCP_Isn.Endpoints; Client_ISN : Seq;
                            Sent, Now : Unsigned_64; MSS : Unsigned_32)
   with Ghost, Global => null,
        Pre  => Now >= Sent and then
                Now / Period - Sent / Period in Periods_Valid + 1 .. Counter_Steps - 1,
        Post => not Accepts (K, E, Client_ISN, Make (K, E, Client_ISN, Sent, MSS), Now);
end TCP_Syn_Cookies;
