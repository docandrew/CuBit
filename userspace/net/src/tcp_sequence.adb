------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body TCP_Sequence with SPARK_Mode is
   procedure Lemma_Irreflexive (A : Seq) is null;
   procedure Lemma_Antisymmetric (A, B : Seq) is null;
   procedure Lemma_Total (A, B : Seq) is null;
   procedure Lemma_Transitive (A, B, C : Seq) is null;
   procedure Lemma_Advance (A : Seq; N : Seq) is null;
   procedure Lemma_Window (X, Low, Size : Seq) is null;
end TCP_Sequence;
