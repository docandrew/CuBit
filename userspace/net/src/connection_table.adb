------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body Connection_Table with SPARK_Mode is

   --  Flipping one slot moves one unit in or out of the free count.
   procedure Lemma_Free_Update (A, B : Flag_Array; S : Slot; K : Connection_Count) with
     Ghost, Global => null,
     Pre  => K <= Max_Connections and then
             (for all X in Slot => (if X /= S then A (X) = B (X))),
     Post => Free_Slots (B, K) =
               (if S > K or else A (S) = B (S) then Free_Slots (A, K)
                elsif B (S) then Free_Slots (A, K) - 1
                else Free_Slots (A, K) + 1),
     Subprogram_Variant => (Decreases => K);
   procedure Lemma_Free_Update (A, B : Flag_Array; S : Slot; K : Connection_Count) is
   begin
      if K > 0 then
         Lemma_Free_Update (A, B, S, K - 1);
      end if;
   end Lemma_Free_Update;

   procedure Lemma_Free_All (A : Flag_Array; K : Connection_Count) with
     Ghost, Global => null,
     Pre  => K <= Max_Connections and then (for all X in Slot => not A (X)),
     Post => Free_Slots (A, K) = K,
     Subprogram_Variant => (Decreases => K);
   procedure Lemma_Free_All (A : Flag_Array; K : Connection_Count) is
   begin
      if K > 0 then
         Lemma_Free_All (A, K - 1);
      end if;
   end Lemma_Free_All;

   --  An open slot keeps the free count below K.
   procedure Lemma_Free_Below (A : Flag_Array; S : Slot; K : Connection_Count) with
     Ghost, Global => null,
     Pre  => K <= Max_Connections and then S <= K and then A (S),
     Post => Free_Slots (A, K) <= K - 1,
     Subprogram_Variant => (Decreases => K);
   procedure Lemma_Free_Below (A : Flag_Array; S : Slot; K : Connection_Count) is
   begin
      if S < K then
         Lemma_Free_Below (A, S, K - 1);
      end if;
   end Lemma_Free_Below;

   procedure Initialize (T : out Table; Secret : SipHash.Key) is
   begin
      T := (Secret  => Secret,
            Keys    => [others => <>],
            Used    => [others => False],
            Gen     => [others => 0],
            Home    => [others => 0],
            Place   => [others => 0],
            Buckets => [others => [others => No_Slot]],
            Stack   => [for S in Slot => S],
            Spot    => [for S in Slot => S],
            Top     => Max_Connections,
            N       => 0);
      Lemma_Free_All (T.Used, Max_Connections);
   end Initialize;

   function Find (T : Table; E : Endpoints) return Maybe_Slot is
      B : constant Bucket_Id := Bucket_Of (T.Secret, E);
   begin
      for P in Position loop
         if T.Buckets (B) (P) /= No_Slot and then T.Keys (T.Buckets (B) (P)) = E then
            return T.Buckets (B) (P);
         end if;
         pragma Loop_Invariant
           (for all S in Slot =>
              (if T.Used (S) and then T.Home (S) = B and then T.Place (S) <= P
               then T.Keys (S) /= E));
      end loop;
      --  An open slot with this tuple would sit in bucket B, where the
      --  scan saw none.
      pragma Assert
        (for all S in Slot => (if T.Used (S) and then T.Keys (S) = E then T.Home (S) = B));
      return No_Slot;
   end Find;

   procedure Insert (T : in out Table; E : Endpoints; H : out Handle; Status : out Insert_Status)
   is
      Old : constant Table := T with Ghost;
      B   : Bucket_Id;
      P   : Maybe_Position := 0;
      S   : Slot;
   begin
      H := (Index => 1, Generation => 0);
      if Find (T, E) /= No_Slot then
         Status := Exists;
         return;
      end if;
      if T.Top = 0 then
         Status := Table_Full;
         return;
      end if;
      B := Bucket_Of (T.Secret, E);
      for Q in Position loop
         if T.Buckets (B) (Q) = No_Slot then
            P := Q;
            exit;
         end if;
      end loop;
      if P = 0 then
         Status := Bucket_Full;
         return;
      end if;

      S := T.Stack (T.Top);
      pragma Assert (not T.Used (S) and then T.Spot (S) = T.Top);
      T.Top := T.Top - 1;
      T.Spot (S) := 0;
      T.Used (S) := True;
      T.Keys (S) := E;
      T.Home (S) := B;
      T.Place (S) := P;
      T.Buckets (B) (P) := S;
      T.N := T.N + 1;
      Lemma_Free_Update (Old.Used, T.Used, S, Max_Connections);
      H := (Index => S, Generation => T.Gen (S));
      Status := Inserted;
   end Insert;

   procedure Remove (T : in out Table; H : Handle) is
      Old : constant Table := T with Ghost;
      S   : constant Slot := H.Index;
   begin
      Lemma_Free_Below (T.Used, S, Max_Connections);
      T.Buckets (T.Home (S)) (T.Place (S)) := No_Slot;
      T.Used (S) := False;
      T.Gen (S) := T.Gen (S) + 1;
      T.Top := T.Top + 1;
      T.Stack (T.Top) := S;
      T.Spot (S) := T.Top;
      T.N := T.N - 1;
      Lemma_Free_Update (Old.Used, T.Used, S, Max_Connections);
   end Remove;
end Connection_Table;
