------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body TCP_Scoreboard with SPARK_Mode is

   --  SACKed restricted to the ranges outside Lo .. Hi.
   function SACKed_Outside (B : Board; Lo, Hi : Positive; O : Offset) return Boolean is
     (for some I in 1 .. B.N => (I < Lo or else I > Hi) and then In_Span (B.R (I), O));

   --  Sorted order carries across ranges: every later range starts after
   --  an earlier one ends.
   procedure Lemma_Ordered (B : Board; I : Positive) with
     Ghost, Global => null,
     Pre  => Valid (B) and then I <= B.N,
     Post => (for all J in I + 1 .. B.N => B.R (I).Stop < B.R (J).First) and then
             (for all J in 1 .. I - 1 => B.R (J).Stop < B.R (I).First);
   procedure Lemma_Ordered (B : Board; I : Positive) is
   begin
      for J in I + 1 .. B.N loop
         pragma Assert (B.R (J - 1).Stop < B.R (J).First);
         if J > I + 1 then
            pragma Assert (B.R (J - 1).First < B.R (J - 1).Stop);
            pragma Assert (B.R (I).Stop < B.R (J - 1).First);
         end if;
         pragma Loop_Invariant (for all K in I + 1 .. J => B.R (I).Stop < B.R (K).First);
      end loop;
      for J in reverse 1 .. I - 1 loop
         pragma Assert (B.R (J).Stop < B.R (J + 1).First);
         if J < I - 1 then
            pragma Assert (B.R (J + 1).First < B.R (J + 1).Stop);
            pragma Assert (B.R (J + 1).Stop < B.R (I).First);
         end if;
         pragma Loop_Invariant (for all K in J .. I - 1 => B.R (K).Stop < B.R (I).First);
      end loop;
   end Lemma_Ordered;

   procedure Clear (B : out Board) is
   begin
      B := (R => [others => (First => 0, Stop => 0)], N => 0);
   end Clear;

   --  S goes in at position P, between its neighbours.
   procedure Insert_At (B : in out Board; P : Positive; S : Span) with
     Pre  => Valid (B) and then B.N < Max_Ranges and then P <= B.N + 1 and then
             S.First < S.Stop and then
             (P = 1 or else B.R (P - 1).Stop < S.First) and then
             (P = B.N + 1 or else S.Stop < B.R (P).First),
     Post => Valid (B) and then B.N = B.N'Old + 1 and then
             (for all O in Offset => SACKed (B, O) = (SACKed (B'Old, O) or else In_Span (S, O)));
   procedure Insert_At (B : in out Board; P : Positive; S : Span) is
      Old : constant Board := B with Ghost;
   begin
      for I in reverse P .. B.N loop
         B.R (I + 1) := B.R (I);
         pragma Loop_Invariant (for all K in 1 .. I => B.R (K) = Old.R (K));
         pragma Loop_Invariant (for all K in I + 1 .. Old.N + 1 => B.R (K) = Old.R (K - 1));
      end loop;
      B.R (P) := S;
      B.N := B.N + 1;
      pragma Assert (for all K in 1 .. P - 1 => B.R (K) = Old.R (K));
      pragma Assert (for all K in P + 1 .. B.N => B.R (K) = Old.R (K - 1));
      pragma Assert (for all K in 1 .. B.N => B.R (K).First < B.R (K).Stop);
      for K in 1 .. B.N - 1 loop
         if K < P - 1 then
            pragma Assert (B.R (K) = Old.R (K) and then B.R (K + 1) = Old.R (K + 1));
            pragma Assert (Old.R (K).Stop < Old.R (K + 1).First);
         elsif K = P - 1 then
            pragma Assert (B.R (K) = Old.R (K) and then B.R (K + 1) = S);
         elsif K = P then
            pragma Assert (B.R (K) = S and then B.R (K + 1) = Old.R (K));
         else
            pragma Assert (B.R (K) = Old.R (K - 1) and then B.R (K + 1) = Old.R (K));
            pragma Assert (Old.R (K - 1).Stop < Old.R (K).First);
         end if;
         pragma Loop_Invariant (for all J in 1 .. K => B.R (J).Stop < B.R (J + 1).First);
      end loop;
   end Insert_At;

   --  Ranges Lo .. Hi become S, which lies between their neighbours.
   procedure Replace_Run (B : in out Board; Lo, Hi : Positive; S : Span) with
     Pre  => Valid (B) and then Lo <= Hi and then Hi <= B.N and then
             S.First < S.Stop and then
             (Lo = 1 or else B.R (Lo - 1).Stop < S.First) and then
             (Hi = B.N or else S.Stop < B.R (Hi + 1).First),
     Post => Valid (B) and then B.N = B.N'Old - (Hi - Lo) and then
             (for all O in Offset =>
                SACKed (B, O) = (SACKed_Outside (B'Old, Lo, Hi, O) or else In_Span (S, O)));
   procedure Replace_Run (B : in out Board; Lo, Hi : Positive; S : Span) is
      Old : constant Board := B with Ghost;
      D   : constant Natural := Hi - Lo;
   begin
      B.R (Lo) := S;
      for I in Lo + 1 .. B.N - D loop
         B.R (I) := B.R (I + D);
         pragma Loop_Invariant (for all K in 1 .. Lo - 1 => B.R (K) = Old.R (K));
         pragma Loop_Invariant (B.R (Lo) = S);
         pragma Loop_Invariant (for all K in Lo + 1 .. I => B.R (K) = Old.R (K + D));
         pragma Loop_Invariant (for all K in I + 1 .. Old.N => B.R (K) = Old.R (K));
      end loop;
      B.N := B.N - D;
      pragma Assert (for all K in 1 .. Lo - 1 => B.R (K) = Old.R (K));
      pragma Assert (for all K in Lo + 1 .. B.N => B.R (K) = Old.R (K + D));
      pragma Assert (for all I in 1 .. Old.N =>
                       (if I < Lo then Old.R (I) = B.R (I)
                        elsif I > Hi then Old.R (I) = B.R (I - D)));
      pragma Assert (for all K in 1 .. B.N => B.R (K).First < B.R (K).Stop);
      for K in 1 .. B.N - 1 loop
         if K < Lo - 1 then
            pragma Assert (B.R (K) = Old.R (K) and then B.R (K + 1) = Old.R (K + 1));
            pragma Assert (Old.R (K).Stop < Old.R (K + 1).First);
         elsif K = Lo - 1 then
            pragma Assert (B.R (K) = Old.R (K) and then B.R (K + 1) = S);
         elsif K = Lo then
            pragma Assert (B.R (K) = S and then B.R (K + 1) = Old.R (Hi + 1));
         else
            pragma Assert (B.R (K) = Old.R (K + D) and then B.R (K + 1) = Old.R (K + D + 1));
            pragma Assert (Old.R (K + D).Stop < Old.R (K + D + 1).First);
         end if;
         pragma Loop_Invariant (for all J in 1 .. K => B.R (J).Stop < B.R (J + 1).First);
      end loop;
      pragma Assert
        (for all O in Offset =>
           (if SACKed (B, O) then SACKed_Outside (Old, Lo, Hi, O) or else In_Span (S, O)));
      pragma Assert
        (for all O in Offset =>
           (if SACKed_Outside (Old, Lo, Hi, O) or else In_Span (S, O) then SACKed (B, O)));
   end Replace_Run;

   procedure Add (B : in out Board; First, Stop : Offset) is
      Old  : constant Board := B with Ghost;
      L, H : Natural := 0;
      M    : Span;
   begin
      --  Ranges wholly before the block, not touching it: 1 .. L.
      while L < B.N and then B.R (L + 1).Stop < First loop
         L := L + 1;
         pragma Loop_Invariant (L <= B.N and then (for all K in 1 .. L => B.R (K).Stop < First));
         pragma Loop_Variant (Increases => L);
      end loop;
      pragma Assert (L = B.N or else B.R (L + 1).Stop >= First);
      --  Ranges touching it: L + 1 .. H, all within the hull of the first
      --  and the last.
      H := L;
      while H < B.N and then B.R (H + 1).First <= Stop loop
         H := H + 1;
         pragma Loop_Invariant (L < H and then H <= B.N and then B.R (H).First <= Stop);
         pragma Loop_Invariant
           (for all K in L + 1 .. H =>
              B.R (K).First >= B.R (L + 1).First and then B.R (K).Stop <= B.R (H).Stop);
         pragma Loop_Variant (Increases => H);
      end loop;

      if H = L then
         if B.N < Max_Ranges then
            Insert_At (B, L + 1, (First => First, Stop => Stop));
         end if;
      else
         M := (First => Offset'Min (First, B.R (L + 1).First),
               Stop  => Offset'Max (Stop, B.R (H).Stop));
         --  The hull adds nothing the block and the run did not cover.
         pragma Assert (B.R (L + 1).Stop >= First and then B.R (H).First <= Stop);
         pragma Assert
           (for all O in Offset =>
              (if In_Span (M, O) and then O < First then In_Span (B.R (L + 1), O)));
         pragma Assert
           (for all O in Offset =>
              (if In_Span (M, O) and then O >= Stop then In_Span (B.R (H), O)));
         pragma Assert
           (for all K in L + 1 .. H =>
              (for all O in Offset => (if In_Span (B.R (K), O) then In_Span (M, O))));
         pragma Assert
           (for all O in Offset =>
              (if In_Span (M, O) then O in First .. Stop - 1 or else SACKed (Old, O)));
         pragma Assert
           (for all O in Offset =>
              (if SACKed_Outside (Old, L + 1, H, O) then SACKed (Old, O)));
         pragma Assert
           (for all O in Offset =>
              (if SACKed (Old, O) then SACKed_Outside (Old, L + 1, H, O) or else In_Span (M, O)));
         Replace_Run (B, L + 1, H, M);
      end if;
   end Add;

   --  Ranges 1 .. L go; the rest move down.
   procedure Drop_Prefix (B : in out Board; L : Positive) with
     Pre  => Valid (B) and then L <= B.N,
     Post => Valid (B) and then B.N = B.N'Old - L and then
             (for all K in 1 .. B.N => B.R (K) = B'Old.R (K + L)) and then
             (for all O in Offset =>
                SACKed (B, O) = (for some I in L + 1 .. B'Old.N => In_Span (B'Old.R (I), O)));
   procedure Drop_Prefix (B : in out Board; L : Positive) is
      Old : constant Board := B with Ghost;
   begin
      for I in 1 .. B.N - L loop
         B.R (I) := B.R (I + L);
         pragma Loop_Invariant (for all K in 1 .. I => B.R (K) = Old.R (K + L));
         pragma Loop_Invariant (for all K in I + 1 .. Old.N => B.R (K) = Old.R (K));
      end loop;
      B.N := B.N - L;
      pragma Assert (for all I in L + 1 .. Old.N => Old.R (I) = B.R (I - L));
   end Drop_Prefix;

   --  A range above By, as offsets from the new SND.UNA.
   function Shifted (S : Span; By : Offset) return Span with
     Pre  => S.First < S.Stop and then S.Stop > By,
     Post => Shifted'Result.Stop = S.Stop - By and then
             (if S.First > By then Shifted'Result.First = S.First - By
              else Shifted'Result.First = 0) and then
             Shifted'Result.First < Shifted'Result.Stop and then
             (for all O in Offset =>
                (if O <= Max_Flight - By then In_Span (Shifted'Result, O) = In_Span (S, O + By)
                 else not In_Span (Shifted'Result, O)));
   function Shifted (S : Span; By : Offset) return Span is
   begin
      return (First => (if S.First > By then S.First - By else 0), Stop => S.Stop - By);
   end Shifted;

   --  Every range, all ending past By, moves down by By.
   procedure Shift_All (B : in out Board; By : Offset) with
     Pre  => Valid (B) and then (for all K in 1 .. B.N => B.R (K).Stop > By),
     Post => Valid (B) and then B.N = B.N'Old and then
             (for all O in Offset =>
                (if O <= Max_Flight - By then SACKed (B, O) = SACKed (B'Old, O + By)
                 else not SACKed (B, O)));
   procedure Shift_All (B : in out Board; By : Offset) is
      Old : constant Board := B with Ghost;
   begin
      for K in 1 .. B.N loop
         pragma Assert (B.R (K) = Old.R (K));
         B.R (K) := Shifted (B.R (K), By);
         pragma Assert (B.R (K) = Shifted (Old.R (K), By));
         pragma Loop_Invariant (for all L in 1 .. K => B.R (L) = Shifted (Old.R (L), By));
         pragma Loop_Invariant (for all L in K + 1 .. B.N => B.R (L) = Old.R (L));
      end loop;
      --  Neighbours stay apart: the later one starts past By, unclipped.
      for K in 1 .. B.N - 1 loop
         pragma Assert (Old.R (K).Stop < Old.R (K + 1).First and then Old.R (K).Stop > By);
         pragma Assert (B.R (K).Stop = Old.R (K).Stop - By);
         pragma Assert (B.R (K + 1).First = Old.R (K + 1).First - By);
         pragma Loop_Invariant (for all J in 1 .. K => B.R (J).Stop < B.R (J + 1).First);
      end loop;
   end Shift_All;

   procedure Advance (B : in out Board; By : Offset) is
      L : Natural := 0;
   begin
      --  Sorted: the acknowledged ranges are a prefix 1 .. L.
      while L < B.N and then B.R (L + 1).Stop <= By loop
         L := L + 1;
         pragma Loop_Invariant (L <= B.N and then (for all K in 1 .. L => B.R (K).Stop <= By));
         pragma Loop_Variant (Increases => L);
      end loop;
      --  Every range after them ends past By.
      if L < B.N then
         Lemma_Ordered (B, L + 1);
         pragma Assert (for all K in L + 1 .. B.N => B.R (K).Stop > By);
      end if;
      if L > 0 then
         Drop_Prefix (B, L);
      end if;
      Shift_All (B, By);
   end Advance;

   procedure Lemma_Above_Monotone (S : Span; Low, High : Offset) with
     Ghost, Global => null,
     Pre  => S.First <= S.Stop and then Low <= High,
     Post => Above (S, Low) >= Above (S, High) and then
             (if Above (S, High) > 0 then Above (S, Low) > 0);
   procedure Lemma_Above_Monotone (S : Span; Low, High : Offset) is
   begin
      if S.Stop <= High + 1 then
         pragma Assert (Above (S, High) = 0);
      elsif S.First > High then
         pragma Assert (S.First > Low and then Above (S, Low) = Above (S, High));
      elsif S.First > Low then
         pragma Assert (Above (S, Low) = S.Stop - S.First);
      else
         pragma Assert (Above (S, Low) - Above (S, High) = High - Low);
      end if;
   end Lemma_Above_Monotone;

   --  The sums over ranges are monotone in the offset.
   procedure Lemma_Sums_Monotone (B : Board; Low, High : Offset; N : Natural) with
     Ghost, Global => null,
     Pre  => Valid (B) and then N <= B.N and then Low <= High,
     Post => Bytes_Above (B, Low, N) >= Bytes_Above (B, High, N) and then
             Ranges_Above (B, Low, N) >= Ranges_Above (B, High, N),
     Subprogram_Variant => (Decreases => N);
   procedure Lemma_Sums_Monotone (B : Board; Low, High : Offset; N : Natural) is
   begin
      if N > 0 then
         Lemma_Sums_Monotone (B, Low, High, N - 1);
         Lemma_Above_Monotone (B.R (N), Low, High);
         pragma Assert (Bytes_Above (B, Low, N) =
                          Bytes_Above (B, Low, N - 1) + Long_Long_Integer (Above (B.R (N), Low)));
         pragma Assert (Bytes_Above (B, High, N) =
                          Bytes_Above (B, High, N - 1) + Long_Long_Integer (Above (B.R (N), High)));
         pragma Assert (Ranges_Above (B, Low, N) =
                          Ranges_Above (B, Low, N - 1) + (if Above (B.R (N), Low) > 0 then 1 else 0));
         pragma Assert (Ranges_Above (B, High, N) =
                          Ranges_Above (B, High, N - 1) + (if Above (B.R (N), High) > 0 then 1 else 0));
      end if;
   end Lemma_Sums_Monotone;

   --  Something counted above Off is a SACKed byte above it.
   procedure Lemma_Evidence (B : Board; Off : Offset; N : Natural) with
     Ghost, Global => null,
     Pre  => Valid (B) and then N <= B.N and then
             (Bytes_Above (B, Off, N) > 0 or else Ranges_Above (B, Off, N) > 0),
     Post => (for some O in Offset => O > Off and then SACKed (B, O)),
     Subprogram_Variant => (Decreases => N);
   procedure Lemma_Evidence (B : Board; Off : Offset; N : Natural) is
   begin
      if Above (B.R (N), Off) > 0 then
         pragma Assert (In_Span (B.R (N), B.R (N).Stop - 1));
         pragma Assert (SACKed (B, B.R (N).Stop - 1));
      else
         Lemma_Evidence (B, Off, N - 1);
      end if;
   end Lemma_Evidence;

   procedure Lemma_Lost_Evidence (B : Board; Off : Offset; SMSS : Positive) is
   begin
      Lemma_Evidence (B, Off, B.N);
   end Lemma_Lost_Evidence;

   procedure Lemma_Lost_Monotone (B : Board; Low, High : Offset; SMSS : Positive) is
   begin
      Lemma_Sums_Monotone (B, Low, High, B.N);
   end Lemma_Lost_Monotone;

   function Next_Unsacked (B : Board; From : Offset) return Offset is
   begin
      for I in 1 .. B.N loop
         if In_Span (B.R (I), From) then
            --  Sorted and apart: the byte after this range is in none.
            Lemma_Ordered (B, I);
            return B.R (I).Stop;
         end if;
         pragma Loop_Invariant (for all J in 1 .. I => not In_Span (B.R (J), From));
      end loop;
      return From;
   end Next_Unsacked;
end TCP_Scoreboard;
