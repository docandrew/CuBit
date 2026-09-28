------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body Timer_Heap with SPARK_Mode is

   pragma Compile_Time_Error (Max_Timers > 2 ** 29, "child indices must not overflow");

   --  Children of K that exist.
   function First_Child (K : Index) return Positive is (2 * K);
   function Last_Child (H : Heap; K : Index) return Timer_Count is
     (Natural'Min (2 * K + 1, H.Size));

   --  Ordered except on the edge above K; K's parent is no later than
   --  K's children (so K can move up).
   function Up_OK (H : Heap; K : Index) return Boolean is
     ((for all I in 2 .. H.Size => (if I /= K then Key (H, I / 2) <= Key (H, I))) and then
      (if K > 1 then
         (for all C in First_Child (K) .. Last_Child (H, K) => Key (H, K / 2) <= Key (H, C))))
   with Ghost, Pre => K <= H.Size;

   --  Ordered except on the edges below K; K's parent is no later than
   --  K's children (so K can move down).
   function Down_OK (H : Heap; K : Index) return Boolean is
     ((for all I in 2 .. H.Size => (if I / 2 /= K then Key (H, I / 2) <= Key (H, I))) and then
      (if K > 1 then
         (for all C in First_Child (K) .. Last_Child (H, K) => Key (H, K / 2) <= Key (H, C))))
   with Ghost, Pre => K <= H.Size;

   --  The same timers are armed.
   function Same_Armed (A, B : Heap) return Boolean is
     (for all T in Timer_Id => (A.Pos (T) /= Not_Armed) = (B.Pos (T) /= Not_Armed))
   with Ghost;

   procedure Lemma_Count_Same (A, B : Positions; N : Timer_Count) with
     Ghost, Global => null,
     Pre  => N <= Max_Timers and then (for all T in Timer_Id => (A (T) /= Not_Armed) = (B (T) /= Not_Armed)),
     Post => Armed_Count (A, N) = Armed_Count (B, N),
     Subprogram_Variant => (Decreases => N);
   procedure Lemma_Count_Same (A, B : Positions; N : Timer_Count) is
   begin
      if N > 0 then
         Lemma_Count_Same (A, B, N - 1);
      end if;
   end Lemma_Count_Same;

   --  Arming or disarming one timer moves the count by one.
   procedure Lemma_Count_Flip (A, B : Positions; T : Timer_Id; N : Timer_Count) with
     Ghost, Global => null,
     Pre  => N <= Max_Timers and then
             (for all U in Timer_Id => (if U /= T then (A (U) /= Not_Armed) = (B (U) /= Not_Armed))),
     Post => Armed_Count (B, N) =
               (if T > N or else (A (T) /= Not_Armed) = (B (T) /= Not_Armed) then Armed_Count (A, N)
                elsif B (T) /= Not_Armed then Armed_Count (A, N) + 1
                else Armed_Count (A, N) - 1),
     Subprogram_Variant => (Decreases => N);
   procedure Lemma_Count_Flip (A, B : Positions; T : Timer_Id; N : Timer_Count) is
   begin
      if N > 0 then
         Lemma_Count_Flip (A, B, T, N - 1);
      end if;
   end Lemma_Count_Flip;

   procedure Lemma_Count_Below (A : Positions; T : Timer_Id; N : Timer_Count) with
     Ghost, Global => null,
     Pre  => N <= Max_Timers and then T <= N and then A (T) = Not_Armed,
     Post => Armed_Count (A, N) <= N - 1,
     Subprogram_Variant => (Decreases => N);
   procedure Lemma_Count_Below (A : Positions; T : Timer_Id; N : Timer_Count) is
   begin
      if T < N then
         Lemma_Count_Below (A, T, N - 1);
      end if;
   end Lemma_Count_Below;

   procedure Lemma_Count_None (A : Positions; N : Timer_Count) with
     Ghost, Global => null,
     Pre  => N <= Max_Timers and then (for all T in Timer_Id => A (T) = Not_Armed),
     Post => Armed_Count (A, N) = 0,
     Subprogram_Variant => (Decreases => N);
   procedure Lemma_Count_None (A : Positions; N : Timer_Count) is
   begin
      if N > 0 then
         Lemma_Count_None (A, N - 1);
      end if;
   end Lemma_Count_None;

   procedure Swap (H : in out Heap; I, J : Index) with
     Pre  => Linked (H) and then I <= H.Size and then J <= H.Size and then I /= J,
     Post => Linked (H) and then H.Size = H.Size'Old and then H.Due = H.Due'Old and then
             H.Slot (I) = H.Slot'Old (J) and then H.Slot (J) = H.Slot'Old (I) and then
             (for all X in Index => (if X /= I and then X /= J then H.Slot (X) = H.Slot'Old (X))) and then
             Same_Armed (H, H'Old) and then
             --  The same, as keys.
             Key (H, I) = Key (H'Old, J) and then Key (H, J) = Key (H'Old, I) and then
             (for all X in 1 .. H.Size => (if X /= I and then X /= J then Key (H, X) = Key (H'Old, X)))
   is
      Old : constant Heap := H with Ghost;
      A   : constant Timer_Id := H.Slot (I);
      B   : constant Timer_Id := H.Slot (J);
   begin
      H.Slot (I) := B;
      H.Slot (J) := A;
      H.Pos (A) := J;
      H.Pos (B) := I;
      Lemma_Count_Same (H.Pos, Old.Pos, Max_Timers);
   end Swap;

   procedure Sift_Up (H : in out Heap; Start : Index) with
     Pre  => Linked (H) and then Start <= H.Size and then Up_OK (H, Start),
     Post => Valid (H) and then H.Size = H.Size'Old and then H.Due = H.Due'Old and then
             Same_Armed (H, H'Old)
   is
      K : Index := Start;
   begin
      loop
         pragma Loop_Invariant (Linked (H) and then K <= H.Size and then Up_OK (H, K));
         pragma Loop_Invariant
           (H.Size = H.Size'Loop_Entry and then H.Due = H.Due'Loop_Entry and then
            Same_Armed (H, H'Loop_Entry));
         pragma Loop_Variant (Decreases => K);
         exit when K = 1 or else Key (H, K / 2) <= Key (H, K);
         --  The parent's key is no earlier than its grandparent's.
         pragma Assert (if K / 2 > 1 then Key (H, K / 2 / 2) <= Key (H, K / 2));
         --  ... and no later than K's sibling (only the edge above K is out of order).
         pragma Assert
           (for all C in First_Child (K / 2) .. Last_Child (H, K / 2) =>
              (if C /= K then Key (H, K / 2) <= Key (H, C)));
         Swap (H, K, K / 2);
         --  The grandparent is no later than either of the parent's children.
         pragma Assert
           (if K / 2 > 1 then
              (for all C in First_Child (K / 2) .. Last_Child (H, K / 2) =>
                 Key (H, K / 2 / 2) <= Key (H, C)));
         pragma Assert (Up_OK (H, K / 2));
         K := K / 2;
      end loop;
   end Sift_Up;

   --  Swapping K with its earliest child M, which was earlier than K,
   --  leaves the heap ordered except below M.
   procedure Lemma_Down_After_Swap (Before, H : Heap; K, M : Index) with
     Ghost, Global => null,
     Pre  => K <= Before.Size and then M <= Before.Size and then H.Size = Before.Size and then
             M / 2 = K and then Down_OK (Before, K) and then
             Key (Before, M) < Key (Before, K) and then
             (for all C in First_Child (K) .. Last_Child (Before, K) =>
                Key (Before, M) <= Key (Before, C)) and then
             (if K > 1 then Key (Before, K / 2) <= Key (Before, M)) and then
             Key (H, K) = Key (Before, M) and then Key (H, M) = Key (Before, K) and then
             (for all X in 1 .. H.Size =>
                (if X /= K and then X /= M then Key (H, X) = Key (Before, X))),
     Post => Down_OK (H, M)
   is
   begin
      for I in 2 .. H.Size loop
         pragma Loop_Invariant
           (for all J in 2 .. I - 1 => (if J / 2 /= M then Key (H, J / 2) <= Key (H, J)));
         if I / 2 /= M then
            if I / 2 = K then
               --  A child of K: K now holds the earliest child's key.
               pragma Assert (Key (H, K) <= Key (H, I));
            elsif I = K then
               pragma Assert (Key (H, I / 2) <= Key (H, I));
            else
               pragma Assert (Key (H, I) = Key (Before, I) and then Key (H, I / 2) = Key (Before, I / 2));
               pragma Assert (Key (Before, I / 2) <= Key (Before, I));
            end if;
            pragma Assert (Key (H, I / 2) <= Key (H, I));
         end if;
      end loop;
      --  Below M: M now holds K's old key, and K holds M's, which was
      --  no later than M's children.
      pragma Assert
        (if M > 1 then
           (for all C in First_Child (M) .. Last_Child (H, M) => Key (H, M / 2) <= Key (H, C)));
   end Lemma_Down_After_Swap;

   procedure Sift_Down (H : in out Heap; Start : Index) with
     Pre  => Linked (H) and then Start <= H.Size and then Down_OK (H, Start),
     Post => Valid (H) and then H.Size = H.Size'Old and then H.Due = H.Due'Old and then
             Same_Armed (H, H'Old)
   is
      K : Index := Start;
      M : Index;
   begin
      loop
         pragma Loop_Invariant (Linked (H) and then K <= H.Size and then Down_OK (H, K));
         pragma Loop_Invariant
           (H.Size = H.Size'Loop_Entry and then H.Due = H.Due'Loop_Entry and then
            Same_Armed (H, H'Loop_Entry));
         pragma Loop_Variant (Increases => K);
         exit when First_Child (K) > H.Size;
         --  M: the earlier child.
         M := First_Child (K);
         if M + 1 <= H.Size and then Key (H, M + 1) < Key (H, M) then
            M := M + 1;
         end if;
         exit when Key (H, K) <= Key (H, M);
         --  M is the earliest child, earlier than K.
         pragma Assert (for all C in First_Child (K) .. Last_Child (H, K) => Key (H, M) <= Key (H, C));
         pragma Assert (if K > 1 then Key (H, K / 2) <= Key (H, M));
         declare
            Before : constant Heap := H with Ghost;
         begin
            Swap (H, K, M);
            Lemma_Down_After_Swap (Before, H, K, M);
         end;
         K := M;
      end loop;
   end Sift_Down;

   --  The root is due no later than any node.
   procedure Lemma_Root_Min (H : Heap) with
     Ghost, Global => null,
     Pre  => Valid (H) and then H.Size >= 1,
     Post => (for all I in 1 .. H.Size => Key (H, 1) <= Key (H, I))
   is
   begin
      for I in 2 .. H.Size loop
         pragma Assert (Key (H, 1) <= Key (H, I / 2));
         pragma Loop_Invariant (for all J in 1 .. I => Key (H, 1) <= Key (H, J));
      end loop;
   end Lemma_Root_Min;

   procedure Initialize (H : out Heap) is
   begin
      H := (Slot => [others => 1], Pos => [others => Not_Armed], Due => [others => 0], Size => 0);
      Lemma_Count_None (H.Pos, Max_Timers);
   end Initialize;

   procedure Arm (H : in out Heap; T : Timer_Id; At_Time : Time) is
      Old     : constant Heap := H with Ghost;
      K       : Index;
      Earlier : Boolean;
   begin
      if H.Pos (T) /= Not_Armed then
         K := H.Pos (T);
         Earlier := At_Time < H.Due (T);
         H.Due (T) := At_Time;
         if Earlier then
            pragma Assert (Up_OK (H, K));
            Sift_Up (H, K);
         else
            pragma Assert (Down_OK (H, K));
            Sift_Down (H, K);
         end if;
      else
         Lemma_Count_Below (H.Pos, T, Max_Timers);
         H.Size := H.Size + 1;
         K := H.Size;
         H.Slot (K) := T;
         H.Pos (T) := K;
         H.Due (T) := At_Time;
         Lemma_Count_Flip (Old.Pos, H.Pos, T, Max_Timers);
         pragma Assert (Linked (H));
         pragma Assert (Up_OK (H, K));
         Sift_Up (H, K);
      end if;
   end Arm;

   procedure Cancel (H : in out Heap; T : Timer_Id) is
      Old : constant Heap := H with Ghost;
      K   : Index;
      L   : Timer_Id;
   begin
      if H.Pos (T) = Not_Armed then
         return;
      end if;
      K := H.Pos (T);
      L := H.Slot (H.Size);
      H.Slot (K) := L;
      H.Pos (L) := K;
      H.Pos (T) := Not_Armed;
      H.Size := H.Size - 1;
      Lemma_Count_Flip (Old.Pos, H.Pos, T, Max_Timers);
      pragma Assert (Linked (H));
      if K <= H.Size then
         --  The last timer took T's place: move it whichever way it goes.
         if K > 1 and then Key (H, K) < Key (H, K / 2) then
            pragma Assert (Up_OK (H, K));
            Sift_Up (H, K);
         else
            pragma Assert (Down_OK (H, K));
            Sift_Down (H, K);
         end if;
      else
         pragma Assert (Ordered (H));
      end if;
   end Cancel;

   procedure Next_Due (H : Heap; Now : Time; T : out Timer_Id; Found : out Boolean) is
   begin
      if H.Size = 0 then
         T := 1;
         Found := False;
         return;
      end if;
      Lemma_Root_Min (H);
      T := H.Slot (1);
      Found := H.Due (T) <= Now;
   end Next_Due;
end Timer_Heap;
