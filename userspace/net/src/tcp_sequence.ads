------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  TCP sequence-number arithmetic modulo 2**32 (RFC 9293 section 3.4).
--
--  A sequence number precedes another when the forward distance between
--  them is in 1 .. 2**31 - 1. This order is only meaningful for numbers
--  within half the space of each other; the lemmas state exactly which
--  properties hold (proved with gnatprove, tests/net-tcp).
------------------------------------------------------------------------------
package TCP_Sequence with SPARK_Mode, Pure is

   type Seq is mod 2 ** 32;

   Half : constant Seq := 2 ** 31;

   --  Forward distance from A to B.
   function Distance (From, To : Seq) return Seq is (To - From);

   --  A precedes B (RFC 9293: A < B).
   function Lt (A, B : Seq) return Boolean is (B - A in 1 .. Half - 1);
   function Le (A, B : Seq) return Boolean is (A = B or else Lt (A, B));
   function Gt (A, B : Seq) return Boolean is (Lt (B, A));
   function Ge (A, B : Seq) return Boolean is (Le (B, A));

   --  X lies in [Low, Low + Size): the window test used throughout TCP.
   function In_Window (X, Low : Seq; Size : Seq) return Boolean is
     (X - Low < Size);

   --  Lemmas (bodies are null; the postconditions are what is proved).

   procedure Lemma_Irreflexive (A : Seq) with
     Ghost, Global => null,
     Post => not Lt (A, A);

   procedure Lemma_Antisymmetric (A, B : Seq) with
     Ghost, Global => null,
     Post => not (Lt (A, B) and then Lt (B, A));

   procedure Lemma_Total (A, B : Seq) with
     Ghost, Global => null,
     Pre  => Distance (A, B) /= Half,
     Post => Lt (A, B) or else A = B or else Lt (B, A);

   --  Transitivity holds when the outer pair is within half the space.
   procedure Lemma_Transitive (A, B, C : Seq) with
     Ghost, Global => null,
     Pre  => Lt (A, B) and then Lt (B, C) and then Distance (A, C) < Half,
     Post => Lt (A, C);

   --  Adding a small amount moves forward.
   procedure Lemma_Advance (A : Seq; N : Seq) with
     Ghost, Global => null,
     Pre  => N in 1 .. Half - 1,
     Post => Lt (A, A + N);

   --  The window test agrees with the order for windows below half the space.
   procedure Lemma_Window (X, Low, Size : Seq) with
     Ghost, Global => null,
     Pre  => Size <= Half,
     Post => In_Window (X, Low, Size) =
               (Le (Low, X) and then Distance (Low, X) < Size);
end TCP_Sequence;
