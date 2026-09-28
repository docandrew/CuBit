------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body Chunked_Send_Queue with SPARK_Mode is

   --  An earlier position is in an earlier chunk, or earlier in the same.
   procedure Lemma_Before (X, A : Position) with
     Ghost, Global => null,
     Pre  => X < A,
     Post => X / Chunk_Bytes < A / Chunk_Bytes or else
             (X / Chunk_Bytes = A / Chunk_Bytes and then X mod Chunk_Bytes < A mod Chunk_Bytes);
   procedure Lemma_Before (X, A : Position) is null;

   --  Positions after A within its chunk stay in that chunk.
   procedure Lemma_Same_Chunk (A : Position; J : Chunk_Offset) with
     Ghost, Global => null,
     Pre  => A mod Chunk_Bytes + J < Chunk_Bytes,
     Post => (A + J) / Chunk_Bytes = A / Chunk_Bytes and then
             (A + J) mod Chunk_Bytes = A mod Chunk_Bytes + J;
   procedure Lemma_Same_Chunk (A : Position; J : Chunk_Offset) is null;

   --  A byte before Held chunks' end is in one of them.
   procedure Lemma_In_Held (A : Position; Held : Held_Count) with
     Ghost, Global => null,
     Pre  => A < Held * Chunk_Bytes,
     Post => A / Chunk_Bytes < Held;
   procedure Lemma_In_Held (A : Position; Held : Held_Count) is null;

   procedure Initialize (Q : out Queue; P : Chunks.Pool; Me : Chunks.Owner_Id; Start : Seq) is
   begin
      Q := (Me => Me, Ids => [others => 1], Held => 0, Head_Off => 0,
            Length => 0, In_Flight => 0, Start => Start);
   end Initialize;

   --  Held chunks have the same contents in A and B.
   function Same_Held (Q : Queue; A, B : Chunks.Pool) return Boolean is
     (for all K in 0 .. Q.Held - 1 =>
        Chunks.Contents (B, Q.Ids (K)) = Chunks.Contents (A, Q.Ids (K)))
   with Ghost;

   --  Allocating C (free in A) leaves every held chunk owned and unchanged.
   procedure Lemma_After_Allocate (Q : Queue; A, B : Chunks.Pool; C : Chunks.Chunk_Id) with
     Ghost, Global => null,
     Pre  => Owns_All (Q, A) and then Q.Me /= Chunks.Free and then
             Chunks.Owner (A, C) = Chunks.Free and then
             (for all D in Chunks.Chunk_Id =>
                (if D /= C then Chunks.Owner (B, D) = Chunks.Owner (A, D) and then
                                Chunks.Contents (B, D) = Chunks.Contents (A, D))),
     Post => Owns_All (Q, B) and then Same_Held (Q, A, B) and then
             (for all K in 0 .. Q.Held - 1 => Q.Ids (K) /= C)
   is
   begin
      for K in 0 .. Q.Held - 1 loop
         pragma Loop_Invariant (Owns_All (Q, A));
         pragma Loop_Invariant (for all J in 0 .. K - 1 => Q.Ids (J) /= C);
         pragma Loop_Invariant (for all J in 0 .. K - 1 => Chunks.Owner (B, Q.Ids (J)) = Q.Me);
         pragma Loop_Invariant
           (for all J in 0 .. K - 1 =>
              Chunks.Contents (B, Q.Ids (J)) = Chunks.Contents (A, Q.Ids (J)));
         pragma Assert (Chunks.Owner (A, Q.Ids (K)) = Q.Me);
         pragma Assert (Q.Ids (K) /= C);
         pragma Assert (Chunks.Owner (B, Q.Ids (K)) = Q.Me);
         pragma Assert (Chunks.Contents (B, Q.Ids (K)) =
                          Chunks.Contents (A, Q.Ids (K)));
      end loop;
   end Lemma_After_Allocate;

   --  Held chunks with unchanged contents hold unchanged bytes.
   procedure Lemma_Elements (Q : Queue; A, B : Chunks.Pool) with
     Ghost, Global => null,
     Pre  => Valid (Q, A) and then Valid (Q, B) and then Same_Held (Q, A, B),
     Post => (for all I in 0 .. Q.Length - 1 => Element (Q, B, I) = Element (Q, A, I))
   is
   begin
      for I in 0 .. Q.Length - 1 loop
         pragma Loop_Invariant (Valid (Q, A) and then Valid (Q, B) and then Same_Held (Q, A, B));
         pragma Loop_Invariant (for all X in 0 .. I - 1 => Element (Q, B, X) = Element (Q, A, X));
         Lemma_In_Held (Q.Head_Off + I, Q.Held);
         pragma Assert
           (Chunks.Contents (B, Q.Ids ((Q.Head_Off + I) / Chunk_Bytes)) =
              Chunks.Contents (A, Q.Ids ((Q.Head_Off + I) / Chunk_Bytes)));
         pragma Assert (Element (Q, B, I) = Element (Q, A, I));
      end loop;
   end Lemma_Elements;

   --  One more chunk at the end of the ring.
   procedure Grow (Q : in out Queue; P : in out Chunks.Pool; OK : out Boolean) with
     Pre  => Valid (Q, P) and then Q.Held < Max_Chunks,
     Post => Valid (Q, P) and then
             Q.Me = Q.Me'Old and then Q.Start = Q.Start'Old and then
             Q.In_Flight = Q.In_Flight'Old and then
             Q.Head_Off = Q.Head_Off'Old and then Q.Length = Q.Length'Old and then
             Q.Held = Q.Held'Old + (if OK then 1 else 0) and then
             (for all J in 0 .. Q.Held'Old - 1 => Q.Ids (J) = Q'Old.Ids (J)) and then
             Isolated (Q.Me, P'Old, P) and then
             (for all I in 0 .. Q.Length - 1 => Element (Q, P, I) = Element (Q'Old, P'Old, I))
   is
      Old_Q : constant Queue := Q with Ghost;
      Old_P : constant Chunks.Pool := P with Ghost;
      C     : Chunks.Chunk_Id;
   begin
      Chunks.Lemma_Equal (Old_P, P);
      Chunks.Allocate (P, Q.Me, C, OK);
      if not OK then
         Chunks.Lemma_Equal (Old_P, P);
         return;
      end if;
      Lemma_After_Allocate (Q, Old_P, P, C);
      Q.Ids (Q.Held) := C;
      Q.Held := Q.Held + 1;
      --  The old chunks keep their places; C is new and owned.
      pragma Assert (for all J in 0 .. Q.Held - 2 => Q.Ids (J) = Old_Q.Ids (J));
      pragma Assert (Chunks.Owner (P, C) = Q.Me);
      pragma Assert (Owns_All (Q, P));
      pragma Assert (Distinct (Q));
      Lemma_Elements (Old_Q, Old_P, P);
   end Grow;

   --  Bytes go at the end of the queue, all within its last chunk.
   procedure Append_Slice (Q : in out Queue; P : in out Chunks.Pool; Bytes : Byte_Array) with
     Pre  => Valid (Q, P) and then Bytes'Length > 0 and then
             Q.Length <= Capacity - Bytes'Length and then
             Q.Head_Off + Q.Length + Bytes'Length <= Q.Held * Chunk_Bytes and then
             (Q.Head_Off + Q.Length) mod Chunk_Bytes + Bytes'Length <= Chunk_Bytes,
     Post => Valid (Q, P) and then
             Q.Me = Q.Me'Old and then Q.Start = Q.Start'Old and then
             Q.In_Flight = Q.In_Flight'Old and then
             Q.Head_Off = Q.Head_Off'Old and then Q.Held = Q.Held'Old and then
             Q.Ids = Q.Ids'Old and then Q.Length = Q.Length'Old + Bytes'Length and then
             Isolated (Q.Me, P'Old, P) and then
             (for all I in 0 .. Q.Length'Old - 1 => Element (Q, P, I) = Element (Q'Old, P'Old, I)) and then
             (for all X in Q.Length'Old .. Q.Length - 1 =>
                Element (Q, P, X) = Bytes (Bytes'First + (X - Q.Length'Old)))
   is
      Old_Q : constant Queue := Q with Ghost;
      Old_P : constant Chunks.Pool := P with Ghost;
      A     : constant Position := Q.Head_Off + Q.Length;
      K     : constant Chunk_Slot := A / Chunk_Bytes;
      X     : Chunks.Chunk_Id;
   begin
      Chunks.Lemma_Equal (Old_P, P);
      Lemma_In_Held (A, Q.Held);
      X := Q.Ids (K);
      pragma Assert (Chunks.Owner (P, X) = Q.Me);
      Chunks.Write_Slice (P, Q.Me, X, A mod Chunk_Bytes + 1, Bytes);
      --  Owners are unchanged, so the queue still owns its chunks.
      for J in 0 .. Q.Held - 1 loop
         pragma Assert (Chunks.Owner (Old_P, Q.Ids (J)) = Q.Me);
         pragma Loop_Invariant
           (for all L in 0 .. J => Chunks.Owner (P, Q.Ids (L)) = Q.Me);
      end loop;
      Q.Length := Q.Length + Bytes'Length;
      --  Earlier bytes: another chunk (distinct, untouched), or earlier in this one.
      for I in 0 .. Q.Length - Bytes'Length - 1 loop
         Lemma_Before (Q.Head_Off + I, A);
         Lemma_In_Held (Q.Head_Off + I, Q.Held);
         pragma Assert
           (if (Q.Head_Off + I) / Chunk_Bytes /= K then
              Q.Ids ((Q.Head_Off + I) / Chunk_Bytes) /= X);
         pragma Loop_Invariant
           (for all Y in 0 .. I => Element (Q, P, Y) = Element (Old_Q, Old_P, Y));
      end loop;
      --  New bytes: this chunk, from A's index on.
      for J in 0 .. Bytes'Length - 1 loop
         pragma Loop_Invariant (Valid (Q, P) and then Q.Ids (K) = X);
         pragma Loop_Invariant
           (for all Z in A mod Chunk_Bytes + 1 .. A mod Chunk_Bytes + Bytes'Length =>
              Chunks.Data (P, X, Z) = Bytes (Bytes'First + (Z - (A mod Chunk_Bytes + 1))));
         pragma Loop_Invariant
           (for all Y in Old_Q.Length .. Old_Q.Length + J - 1 =>
              Element (Q, P, Y) = Bytes (Bytes'First + (Y - Old_Q.Length)));
         Lemma_Same_Chunk (A, J);
         pragma Assert (Chunks.Data (P, X, A mod Chunk_Bytes + 1 + J) = Bytes (Bytes'First + J));
         pragma Assert (Element (Q, P, Old_Q.Length + J) = Bytes (Bytes'First + J));
      end loop;
   end Append_Slice;

   procedure Push (Q : in out Queue; P : in out Chunks.Pool; Data : Byte_Array;
                   Accepted : out Natural)
   is
      Old_Q : constant Queue := Q with Ghost;
      Old_P : constant Chunks.Pool := P with Ghost;
      Want  : constant Byte_Count := Natural'Min (Data'Length, Capacity - Q.Length);
      Done  : Byte_Count := 0;
      A     : Position;
      N     : Natural range 0 .. Chunk_Bytes;
      OK    : Boolean;
   begin
      Chunks.Lemma_Equal (Old_P, P);
      loop
         pragma Loop_Invariant (Valid (Q, P));
         pragma Loop_Invariant
           (Q.Me = Old_Q.Me and then Q.Start = Old_Q.Start and then
            Q.In_Flight = Old_Q.In_Flight and then
            Q.Head_Off = Old_Q.Head_Off);
         pragma Loop_Invariant (Done <= Want and then Q.Length = Old_Q.Length + Done);
         pragma Loop_Invariant (Isolated (Q.Me, Old_P, P));
         pragma Loop_Invariant
           (for all I in 0 .. Old_Q.Length - 1 => Element (Q, P, I) = Element (Old_Q, Old_P, I));
         pragma Loop_Invariant
           (for all X in Old_Q.Length .. Old_Q.Length + Done - 1 =>
              Element (Q, P, X) = Data (Data'First + (X - Old_Q.Length)));
         exit when Done = Want;

         A := Q.Head_Off + Q.Length;
         if A = Q.Held * Chunk_Bytes then
            --  The next byte starts a new chunk.
            Grow (Q, P, OK);
            exit when not OK;
         end if;
         pragma Assert
           (for all I in 0 .. Old_Q.Length - 1 => Element (Q, P, I) = Element (Old_Q, Old_P, I));
         N := Natural'Min (Chunk_Bytes - A mod Chunk_Bytes, Want - Done);
         Append_Slice (Q, P, Data (Data'First + Done .. Data'First + Done + N - 1));
         --  What was written before this slice is unchanged.
         pragma Assert
           (for all I in 0 .. Old_Q.Length - 1 => Element (Q, P, I) = Element (Old_Q, Old_P, I));
         Done := Done + N;
      end loop;
      Accepted := Done;
   end Push;

   --  Into'Length bytes from offset From, all within one chunk.
   procedure Read_Slice_At (Q : Queue; P : Chunks.Pool; From : Byte_Count; Into : out Byte_Array) with
     Pre  => Valid (Q, P) and then Into'Length > 0 and then
             From <= Q.Length - Into'Length and then
             (Q.Head_Off + From) mod Chunk_Bytes + Into'Length <= Chunk_Bytes,
     Post => (for all Z in Into'Range => Into (Z) = Element (Q, P, From + (Z - Into'First)))
   is
      A : constant Position := Q.Head_Off + From;
   begin
      Lemma_In_Held (A, Q.Held);
      pragma Assert (Chunks.Owner (P, Q.Ids (A / Chunk_Bytes)) = Q.Me);
      Chunks.Read_Slice (P, Q.Me, Q.Ids (A / Chunk_Bytes), A mod Chunk_Bytes + 1, Into);
      for J in 0 .. Into'Length - 1 loop
         Lemma_Same_Chunk (A, J);
         pragma Loop_Invariant
           (for all Z in Into'First .. Into'First + J => Into (Z) = Element (Q, P, From + (Z - Into'First)));
      end loop;
   end Read_Slice_At;

   procedure Take (Q : in out Queue; P : Chunks.Pool; Limit : Natural; Data : out Byte_Array;
                   First : out Seq; Taken : out Natural)
   is
      Done : Byte_Count := 0;
      N    : Natural range 0 .. Chunk_Bytes;
   begin
      Taken := Natural'Min (Limit, Q.Length - Q.In_Flight);
      First := Q.Start + Seq (Q.In_Flight);
      Data := [others => 0];
      loop
         pragma Loop_Invariant (Done <= Taken);
         pragma Loop_Invariant
           (for all J in 1 .. Done => Data (J) = Element (Q, P, Q.In_Flight + J - 1));
         pragma Loop_Variant (Increases => Done);
         exit when Done = Taken;
         N := Natural'Min (Chunk_Bytes - (Q.Head_Off + Q.In_Flight + Done) mod Chunk_Bytes,
                           Taken - Done);
         Read_Slice_At (Q, P, Q.In_Flight + Done, Data (Done + 1 .. Done + N));
         Done := Done + N;
      end loop;
      Q.In_Flight := Q.In_Flight + Taken;
   end Take;

   --  Positions relative to a later start: dropping D whole chunks and
   --  keeping the offset within the next.
   procedure Lemma_Rebase (H : Chunk_Offset; F, X : Byte_Count) with
     Ghost, Global => null,
     Pre  => F + X <= Capacity,
     Post => ((H + F) mod Chunk_Bytes + X) / Chunk_Bytes + (H + F) / Chunk_Bytes =
               (H + (X + F)) / Chunk_Bytes and then
             ((H + F) mod Chunk_Bytes + X) mod Chunk_Bytes = (H + (X + F)) mod Chunk_Bytes;
   procedure Lemma_Rebase (H : Chunk_Offset; F, X : Byte_Count) is null;

   --  The first D chunks go back to the pool; the rest move down.
   procedure Release_Front (Q : in out Queue; P : in out Chunks.Pool; D : Held_Count) with
     Pre  => Valid (Q, P) and then D <= Q.Held and then
             Q.Head_Off + Q.Length <= Q.Held * Chunk_Bytes,
     Post => Chunks.Valid (P) and then
             Q.Me = Q.Me'Old and then Q.Held = Q.Held'Old - D and then
             Q.Start = Q.Start'Old and then Q.Length = Q.Length'Old and then
             Q.In_Flight = Q.In_Flight'Old and then Q.Head_Off = Q.Head_Off'Old and then
             (for all K in 0 .. Q.Held - 1 => Q.Ids (K) = Q.Ids'Old (K + D)) and then
             Owns_All (Q, P) and then Distinct (Q) and then
             Isolated (Q.Me, P'Old, P) and then
             (for all K in 0 .. Q.Held - 1 =>
                Chunks.Contents (P, Q.Ids (K)) = Chunks.Contents (P'Old, Q.Ids (K))) and then
             (for all K in 0 .. Q.Held - 1 =>
                Chunks.Contents (P, Q.Ids (K)) = Chunks.Contents (P'Old, Q'Old.Ids (K + D))) and then
             Chunks.Held (P, Q.Me) = Chunks.Held (P'Old, Q.Me) - D
   is
      Old_Q : constant Queue := Q with Ghost;
      Old_P : constant Chunks.Pool := P with Ghost;
   begin
      Chunks.Lemma_Equal (Old_P, P);
      declare
         J : Held_Count := 0;
      begin
      loop
         pragma Loop_Invariant (J <= D);
         pragma Loop_Invariant (Chunks.Valid (P));
         pragma Loop_Invariant
           (for all K in J .. Old_Q.Held - 1 => Chunks.Owner (P, Old_Q.Ids (K)) = Old_Q.Me);
         pragma Loop_Invariant
           (for all K in D .. Old_Q.Held - 1 =>
              Chunks.Contents (P, Old_Q.Ids (K)) = Chunks.Contents (Old_P, Old_Q.Ids (K)));
         pragma Loop_Invariant (Isolated (Old_Q.Me, Old_P, P));
         pragma Loop_Invariant (Chunks.Held (P, Old_Q.Me) = Chunks.Held (Old_P, Old_Q.Me) - J);
         exit when J = D;
         --  Chunk J is ours and distinct from every later one.
         pragma Assert (for all K in J + 1 .. Old_Q.Held - 1 => Old_Q.Ids (K) /= Old_Q.Ids (J));
         pragma Assert (Q.Ids (J) = Old_Q.Ids (J) and then Q.Me = Old_Q.Me);
         Chunks.Release (P, Q.Me, Q.Ids (J));
         J := J + 1;
      end loop;
      end;
      for K in 0 .. Q.Held - D - 1 loop
         pragma Loop_Invariant (for all L in 0 .. K - 1 => Q.Ids (L) = Old_Q.Ids (L + D));
         pragma Loop_Invariant (for all L in K .. Old_Q.Held - 1 => Q.Ids (L) = Old_Q.Ids (L));
         pragma Loop_Invariant (Q.Held = Old_Q.Held and then Q.Me = Old_Q.Me);
         Q.Ids (K) := Q.Ids (K + D);
      end loop;
      Q.Held := Q.Held - D;
      --  The kept chunks, in their new places, are ours and unchanged.
      for K in 0 .. Q.Held - 1 loop
         pragma Loop_Invariant
           (for all L in 0 .. K - 1 =>
              Chunks.Contents (P, Q.Ids (L)) = Chunks.Contents (Old_P, Q.Ids (L)) and then
              Chunks.Owner (P, Q.Ids (L)) = Q.Me);
         pragma Assert (Q.Ids (K) = Old_Q.Ids (K + D));
         pragma Assert (Chunks.Contents (P, Old_Q.Ids (K + D)) = Chunks.Contents (Old_P, Old_Q.Ids (K + D)));
         pragma Assert (Chunks.Owner (P, Old_Q.Ids (K + D)) = Q.Me);
      end loop;
   end Release_Front;

   --  After an ACK freed F bytes and D chunks: byte X of the new queue is
   --  byte X + F of the old.
   procedure Lemma_Ack_Byte (Old_Q, Q : Queue; Old_P, P : Chunks.Pool;
                             F : Byte_Count; D : Held_Count; X : Byte_Count)
   with
     Ghost, Global => null,
     Pre  => Valid (Q, P) and then X < Q.Length and then F + X <= Capacity and then
             Old_Q.Head_Off + (X + F) < Old_Q.Held * Chunk_Bytes and then
             D = (Old_Q.Head_Off + F) / Chunk_Bytes and then
             Q.Head_Off = (Old_Q.Head_Off + F) mod Chunk_Bytes and then
             Q.Held = Old_Q.Held - D and then
             (for all K in 0 .. Q.Held - 1 =>
                Chunks.Contents (P, Q.Ids (K)) = Chunks.Contents (Old_P, Old_Q.Ids (K + D))),
     Post => Element (Q, P, X) =
               Chunks.Data (Old_P, Old_Q.Ids ((Old_Q.Head_Off + (X + F)) / Chunk_Bytes),
                            (Old_Q.Head_Off + (X + F)) mod Chunk_Bytes + 1)
   is
      K_New : constant Chunk_Slot := (Q.Head_Off + X) / Chunk_Bytes;
      Idx   : constant Chunks.Chunk_Index := (Q.Head_Off + X) mod Chunk_Bytes + 1;
   begin
      Lemma_Rebase (Old_Q.Head_Off, F, X);
      Lemma_In_Held (Q.Head_Off + X, Q.Held);
      pragma Assert (K_New + D = (Old_Q.Head_Off + (X + F)) / Chunk_Bytes);
      pragma Assert (Idx = (Old_Q.Head_Off + (X + F)) mod Chunk_Bytes + 1);
      pragma Assert (Chunks.Contents (P, Q.Ids (K_New)) = Chunks.Contents (Old_P, Old_Q.Ids (K_New + D)));
      pragma Assert (Chunks.Contents (P, Q.Ids (K_New)) (Idx) = Chunks.Contents (Old_P, Old_Q.Ids (K_New + D)) (Idx));
      pragma Assert (Element (Q, P, X) = Chunks.Data (P, Q.Ids (K_New), Idx));
   end Lemma_Ack_Byte;

   procedure Acknowledge (Q : in out Queue; P : in out Chunks.Pool; Ack : Seq;
                          Result : out Ack_Result; Freed : out Natural)
   is
      Old_Q : constant Queue := Q with Ghost;
      Old_P : constant Chunks.Pool := P with Ghost;
      --  The forward distance as an integer, so the comparison is exact.
      Gap   : constant Long_Long_Integer := Long_Long_Integer (Distance (Q.Start, Ack));
      New_Off : Position;
      Drop    : Held_Count;
   begin
      Chunks.Lemma_Equal (Old_P, P);
      if Gap > Long_Long_Integer (Q.In_Flight) then
         Freed := 0;
         Result := (if Lt (Q.Start + Seq (Q.In_Flight), Ack) then Unsent_Data else Old);
         return;
      end if;
      Freed := Natural (Gap);
      Result := (if Freed = 0 then Duplicate else Advanced);
      New_Off := Q.Head_Off + Freed;
      Drop := New_Off / Chunk_Bytes;
      Release_Front (Q, P, Drop);
      Q.Head_Off := New_Off mod Chunk_Bytes;
      Q.Length := Q.Length - Freed;
      Q.In_Flight := Q.In_Flight - Freed;
      Q.Start := Ack;
      --  Every remaining byte sits where it did, counted from the new start.
      pragma Assert (Valid (Q, P));
      pragma Assert
        (for all K in 0 .. Q.Held - 1 =>
           Chunks.Contents (P, Q.Ids (K)) = Chunks.Contents (Old_P, Old_Q.Ids (K + Drop)));
      for X in 0 .. Q.Length - 1 loop
         pragma Loop_Invariant
           (for all Y in 0 .. X - 1 =>
              Element (Q, P, Y) =
                Chunks.Data (Old_P, Old_Q.Ids ((Old_Q.Head_Off + (Y + Freed)) / Chunk_Bytes),
                             (Old_Q.Head_Off + (Y + Freed)) mod Chunk_Bytes + 1));
         Lemma_Ack_Byte (Old_Q, Q, Old_P, P, Freed, Drop, X);
      end loop;
   end Acknowledge;

   procedure Release_All (Q : in out Queue; P : in out Chunks.Pool) is
   begin
      Release_Front (Q, P, Q.Held);
      Q.Head_Off := 0;
      Q.Length := 0;
      Q.In_Flight := 0;
   end Release_All;

   procedure Peek (Q : Queue; P : Chunks.Pool; Offset : Byte_Count; Limit : Natural;
                   Data : out Byte_Array; Taken : out Byte_Count)
   is
      Done : Byte_Count := 0;
      N    : Natural range 0 .. Chunk_Bytes;
   begin
      Taken := Natural'Min (Limit, Q.In_Flight - Offset);
      Data := [others => 0];
      loop
         pragma Loop_Invariant (Done <= Taken);
         pragma Loop_Invariant
           (for all J in 1 .. Done => Data (J) = Element (Q, P, Offset + J - 1));
         pragma Loop_Variant (Increases => Done);
         exit when Done = Taken;
         N := Natural'Min (Chunk_Bytes - (Q.Head_Off + Offset + Done) mod Chunk_Bytes,
                           Taken - Done);
         Read_Slice_At (Q, P, Offset + Done, Data (Done + 1 .. Done + N));
         Done := Done + N;
      end loop;
   end Peek;
end Chunked_Send_Queue;
