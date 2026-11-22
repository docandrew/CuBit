------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  TCP initial sequence numbers (RFC 6528): ISN = M + F (4-tuple, secret),
--  with M a clock ticking every 4 microseconds and F SipHash-2-4 keyed by
--  a secret chosen at boot from the entropy service.
--
--  Unpredictable to an off-path attacker who does not know the secret
--  (a property of SipHash, assumed); per-connection offsets keep old
--  segments of an earlier incarnation of the same 4-tuple out of the new
--  window as RFC 9293 3.4.1 intends. Proved: absence of runtime errors.
------------------------------------------------------------------------------
with Interfaces;   use Interfaces;
with TCP_Sequence; use TCP_Sequence;
with SipHash;

package TCP_Isn with SPARK_Mode is

   --  IPv6, or IPv4-mapped (::ffff:a.b.c.d), in network order.
   type Address is array (1 .. 16) of Unsigned_8;

   type Endpoints is record
      Local, Remote           : Address := [others => 0];
      Local_Port, Remote_Port : Unsigned_16 := 0;
   end record;

   subtype Tuple_Bytes is SipHash.Byte_Array (0 .. 35);

   --  Positional, from scalar fields only: equal fields give identical
   --  bytes, which proofs about hash tables rely on.
   function Serialize (E : Endpoints) return Tuple_Bytes is
     [E.Local (1),  E.Local (2),  E.Local (3),  E.Local (4),
      E.Local (5),  E.Local (6),  E.Local (7),  E.Local (8),
      E.Local (9),  E.Local (10), E.Local (11), E.Local (12),
      E.Local (13), E.Local (14), E.Local (15), E.Local (16),
      E.Remote (1),  E.Remote (2),  E.Remote (3),  E.Remote (4),
      E.Remote (5),  E.Remote (6),  E.Remote (7),  E.Remote (8),
      E.Remote (9),  E.Remote (10), E.Remote (11), E.Remote (12),
      E.Remote (13), E.Remote (14), E.Remote (15), E.Remote (16),
      Unsigned_8 (Shift_Right (E.Local_Port, 8)),  Unsigned_8 (E.Local_Port and 16#FF#),
      Unsigned_8 (Shift_Right (E.Remote_Port, 8)), Unsigned_8 (E.Remote_Port and 16#FF#)];

   function Initial_Sequence (K : SipHash.Key; E : Endpoints; Clock_4us : Unsigned_32)
     return Seq
   is (Seq (Clock_4us) + Seq (SipHash.Hash (K, Serialize (E)) and 16#FFFF_FFFF#));
end TCP_Isn;
