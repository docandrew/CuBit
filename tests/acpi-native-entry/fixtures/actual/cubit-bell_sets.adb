------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Bell_Sets with SPARK_Mode is

   procedure Register (S : in out Set; Ref : GR.Reference; Result : out Register_Result) is
      B : constant Bucket_Index := Bucket_Of (Ref);
      Free : Natural := 0;
   begin
      for I in Way_Index loop
         if S (B) (I).Used and then S (B) (I).Ref = Ref then
            Result := Already;
            return;
         end if;
         if Free = 0 and then not S (B) (I).Used then
            Free := I;
         end if;
         pragma Loop_Invariant
           (for all J in 1 .. I => not (S (B) (J).Used and then S (B) (J).Ref = Ref));
         pragma Loop_Invariant (Free <= I);
         pragma Loop_Invariant (if Free /= 0 then not S (B) (Free).Used);
         pragma Loop_Invariant
           (if Free = 0 then (for all J in 1 .. I => S (B) (J).Used));
      end loop;
      if Free = 0 then
         Result := Unrouted;
         return;
      end if;
      S (B) (Free) := (Used => True, Ref => Ref, Rung => False);
      Result := Registered;
   end Register;

   procedure Unregister (S : in out Set; Ref : GR.Reference) is
      B : constant Bucket_Index := Bucket_Of (Ref);
   begin
      for I in Way_Index loop
         if S (B) (I).Used and then S (B) (I).Ref = Ref then
            S (B) (I) := (Used => False, Ref => (slot => 0, generation => 1), Rung => False);
         end if;
         pragma Loop_Invariant
           (for all J in 1 .. I => not (S (B) (J).Used and then S (B) (J).Ref = Ref));
         pragma Loop_Invariant
           (for all C in Bucket_Index => (if C /= B then S (C) = S'Loop_Entry (C)));
         pragma Loop_Invariant
           (for all J in Way_Index =>
              (if not (S'Loop_Entry (B) (J).Used and then S'Loop_Entry (B) (J).Ref = Ref)
               then S (B) (J) = S'Loop_Entry (B) (J)));
         pragma Loop_Invariant
           (for all J in I + 1 .. Ways => S (B) (J) = S'Loop_Entry (B) (J));
         pragma Loop_Invariant (Valid (S));
      end loop;
   end Unregister;

   procedure Note (S : in out Set; Ref : GR.Reference; Known : out Boolean) is
      B : constant Bucket_Index := Bucket_Of (Ref);
   begin
      Known := False;
      for I in Way_Index loop
         if S (B) (I).Used and then S (B) (I).Ref = Ref then
            S (B) (I).Rung := True;
            Known := True;
         end if;
         pragma Loop_Invariant
           (Known = (for some J in 1 .. I =>
                       S'Loop_Entry (B) (J).Used and then S'Loop_Entry (B) (J).Ref = Ref));
         pragma Loop_Invariant
           (for all C in Bucket_Index => (if C /= B then S (C) = S'Loop_Entry (C)));
         pragma Loop_Invariant
           (for all J in Way_Index =>
              S (B) (J).Used = S'Loop_Entry (B) (J).Used
              and then S (B) (J).Ref = S'Loop_Entry (B) (J).Ref);
         pragma Loop_Invariant
           (for all J in 1 .. I =>
              S (B) (J).Rung = (S'Loop_Entry (B) (J).Rung or else
                                  (S'Loop_Entry (B) (J).Used and then S'Loop_Entry (B) (J).Ref = Ref)));
         pragma Loop_Invariant
           (for all J in I + 1 .. Ways => S (B) (J) = S'Loop_Entry (B) (J));
      end loop;
   end Note;

   procedure Take (S : in out Set; Ref : GR.Reference; Was_Rung : out Boolean) is
      B : constant Bucket_Index := Bucket_Of (Ref);
   begin
      Was_Rung := False;
      for I in Way_Index loop
         if S (B) (I).Used and then S (B) (I).Ref = Ref then
            Was_Rung := Was_Rung or else S (B) (I).Rung;
            S (B) (I).Rung := False;
         end if;
         pragma Loop_Invariant
           (Was_Rung = (for some J in 1 .. I =>
                          S'Loop_Entry (B) (J).Used and then S'Loop_Entry (B) (J).Ref = Ref
                          and then S'Loop_Entry (B) (J).Rung));
         pragma Loop_Invariant
           (for all C in Bucket_Index => (if C /= B then S (C) = S'Loop_Entry (C)));
         pragma Loop_Invariant
           (for all J in Way_Index =>
              S (B) (J).Used = S'Loop_Entry (B) (J).Used
              and then S (B) (J).Ref = S'Loop_Entry (B) (J).Ref);
         pragma Loop_Invariant
           (for all J in 1 .. I =>
              S (B) (J).Rung = (S'Loop_Entry (B) (J).Rung and then
                                  not (S'Loop_Entry (B) (J).Used and then S'Loop_Entry (B) (J).Ref = Ref)));
         pragma Loop_Invariant
           (for all J in I + 1 .. Ways => S (B) (J) = S'Loop_Entry (B) (J));
      end loop;
   end Take;

end CuBit.Bell_Sets;
