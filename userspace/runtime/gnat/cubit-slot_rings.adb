------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Slot_Rings with SPARK_Mode is

   procedure Lemma_Distinct (I, J : Index) is
      D : constant Index := J - I;          --  0 < D < Slots, by the Pre
      A : constant Index := I and Mask;     --  I's slot, 0 .. Slots - 1
   begin
      --  The low bits of J = I + D are those of A + D, which is below
      --  2 * Slots: either A + D itself (no carry out of the low bits) or
      --  A + D - Slots. Neither equals A.
      pragma Assert (D in 1 .. Mask);
      pragma Assert ((J and Mask) = ((A + D) and Mask));
      if A + D <= Mask then
         pragma Assert (((A + D) and Mask) = A + D);
      else
         pragma Assert (((A + D) and Mask) = A + D - Index (Slots));
      end if;
      pragma Assert ((J and Mask) /= A);
   end Lemma_Distinct;

   procedure Accept_Consumed
     (P : in out Producer; Value : Index; OK : out Boolean) is
      Freed : constant Count := Released (P, Value);
   begin
      OK := Freed <= Count (P.Fill);
      if OK then
         P.Fill := P.Fill - Natural (Freed);
      end if;
   end Accept_Consumed;

   procedure Commit (P : in out Producer) is
   begin
      P.Produced := P.Produced + 1;
      P.Fill := P.Fill + 1;
   end Commit;

   procedure Push (P : in out Producer; R : in out Ring; E : Element) is
   begin
      R (Next_Slot (P)) := E;
      Commit (P);
   end Push;

   procedure Lemma_Free_Slot (P : Producer; K : Index) is
   begin
      --  K is B elements past Consumed, B < Fill < Slots, so it is
      --  Fill - B elements behind Produced: between 1 and Slots - 1.
      pragma Assert (P.Produced - K = Index (P.Fill) - (K - Consumed (P)));
      pragma Assert (P.Produced - K in 1 .. Mask);
      Lemma_Distinct (K, P.Produced);
   end Lemma_Free_Slot;

   procedure Accept_Produced
     (C : in out Consumer; Value : Index; OK : out Boolean) is
      Ahead : constant Count := Distance (C.Consumed, Value);
   begin
      OK := Ahead <= Count (Slots) and then Ahead >= Count (C.Available);
      if OK then
         C.Available := Natural (Ahead);
      end if;
   end Accept_Produced;

   procedure Release (C : in out Consumer) is
   begin
      C.Consumed := C.Consumed + 1;
      C.Available := C.Available - 1;
   end Release;

   procedure Take (C : in out Consumer; R : Ring; E : out Element) is
   begin
      E := R (Head_Slot (C));
      Release (C);
   end Take;

end CuBit.Slot_Rings;
