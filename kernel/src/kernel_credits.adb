-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
-------------------------------------------------------------------------------
package body Kernel_Credits with
    SPARK_Mode => On
is

    -- Setting or clearing bit B of X leaves every other bit as it was.
    procedure Lemma_Other_Bits (X : Unsigned_64; B : Bit_Index)
      with Ghost,
           Post => (for all C in Bit_Index =>
                      (if C /= B then
                         ((X or Mask (B)) and Mask (C)) = (X and Mask (C)) and then
                         ((X and not Mask (B)) and Mask (C)) = (X and Mask (C))));

    procedure Lemma_Other_Bits (X : Unsigned_64; B : Bit_Index) is null;

    -- Another sender in S's word is a different bit of that word.
    procedure Lemma_Same_Word (S, T : Sender)
      with Ghost,
           Pre  => T /= S and then Word_Of (T) = Word_Of (S),
           Post => Bit_Of (T) /= Bit_Of (S);

    procedure Lemma_Same_Word (S, T : Sender) is null;

    -- After word Word_Of (S) changed only in bit Bit_Of (S), every other
    -- sender (and the unused last bit) reads as before.
    procedure Lemma_Others_Unchanged (Old, W : Sender_Bits; S : Sender)
      with Ghost,
           Pre  => (for all I in Word_Index =>
                      (if I /= Word_Of (S) then W (I) = Old (I)))
                   and then
                   (for all C in Bit_Index =>
                      (if C /= Bit_Of (S) then
                         (W (Word_Of (S)) and Mask (C)) = (Old (Word_Of (S)) and Mask (C)))),
           Post => (for all T in Sender =>
                      (if T /= S then Is_Set (W, T) = Is_Set (Old, T))) and then
                   (W (Word_Index'Last) and Last_Word_Unused) =
                      (Old (Word_Index'Last) and Last_Word_Unused)
    is
    begin
        pragma Assert (Bit_Of (S) /= Bits_Per_Word - 1 or else Word_Of (S) /= Word_Index'Last);
        for T in Sender loop
            if T /= S and then Word_Of (T) = Word_Of (S) then
                Lemma_Same_Word (S, T);
            end if;
            pragma Loop_Invariant
              (for all U in Sender'First .. T =>
                 (if U /= S then Is_Set (W, U) = Is_Set (Old, U)));
        end loop;
    end Lemma_Others_Unchanged;

    procedure Set_Bit (W : in out Sender_Bits; S : Sender)
      with Post => Is_Set (W, S) and then
                   (for all T in Sender => (if T /= S then Is_Set (W, T) = Is_Set (W'Old, T))) and then
                   ((W (Word_Index'Last) and Last_Word_Unused) =
                      (W'Old (Word_Index'Last) and Last_Word_Unused))
    is
        Old : constant Sender_Bits := W with Ghost;
    begin
        Lemma_Other_Bits (W (Word_Of (S)), Bit_Of (S));
        W (Word_Of (S)) := W (Word_Of (S)) or Mask (Bit_Of (S));
        Lemma_Others_Unchanged (Old, W, S);
    end Set_Bit;

    procedure Clear_Bit (W : in out Sender_Bits; S : Sender)
      with Post => not Is_Set (W, S) and then
                   (for all T in Sender => (if T /= S then Is_Set (W, T) = Is_Set (W'Old, T))) and then
                   ((W (Word_Index'Last) and Last_Word_Unused) =
                      (W'Old (Word_Index'Last) and Last_Word_Unused))
    is
        Old : constant Sender_Bits := W with Ghost;
    begin
        Lemma_Other_Bits (W (Word_Of (S)), Bit_Of (S));
        W (Word_Of (S)) := W (Word_Of (S)) and not Mask (Bit_Of (S));
        Lemma_Others_Unchanged (Old, W, S);
    end Clear_Bit;

    procedure Initialize (R : in out Receiver_State; Each : Credit) is
    begin
        R.Each := Each;
        R.Cursor := Sender'First;
        R.Nonempty := (others => 0);
        for S in Sender loop
            R.Rings (S) := (Generation => 0, Head => 0, Count => 0);
            pragma Loop_Invariant (R.Each = Each and then R.Nonempty = (Word_Index => 0));
            pragma Loop_Invariant
              (for all T in Sender'First .. S =>
                 R.Rings (T).Count = 0 and then R.Rings (T).Head = 0);
        end loop;
    end Initialize;

    procedure Admit
      (R : in out Receiver_State; From : Sender; Generation : Unsigned_64;
       At_Position : out Position; Result : out Admit_Result)
    is
        Ring : Sender_Ring renames R.Rings (From);
    begin
        At_Position := 0;
        if Ring.Count = R.Each or else
           (Ring.Count > 0 and then Ring.Generation /= Generation)
        then
            Result := Busy;
            return;
        end if;
        At_Position := (Ring.Head + Ring.Count) mod R.Each;
        Ring.Count := Ring.Count + 1;
        Ring.Generation := Generation;
        Set_Bit (R.Nonempty, From);
        Result := Admitted;
    end Admit;

    procedure Take
      (R : in out Receiver_State; From : out Sender; At_Position : out Position;
       Found : out Boolean)
    is
        procedure Serve (S : Sender)
          with Pre  => Valid (R) and then Is_Set (R.Nonempty, S),
               Post => Valid (R) and then R.Each = R'Old.Each and then
                       R'Old.Rings (S).Count > 0 and then
                       At_Position = R'Old.Rings (S).Head and then From = S and then
                       R.Rings (S).Count = R'Old.Rings (S).Count - 1 and then
                       (for all T in Sender => (if T /= S then R.Rings (T) = R'Old.Rings (T)))
        is
        begin
            From := S;
            At_Position := R.Rings (S).Head;
            R.Rings (S).Head := (R.Rings (S).Head + 1) mod R.Each;
            R.Rings (S).Count := R.Rings (S).Count - 1;
            if R.Rings (S).Count = 0 then
                Clear_Bit (R.Nonempty, S);
            end if;
            R.Cursor := (if S = Sender'Last then Sender'First else S + 1);
        end Serve;

        -- The sender at bit B of word W, which is set (so not bit 63 of
        -- the last word, which no sender has).
        function Sender_At (W : Word_Index; B : Bit_Index) return Sender is
          (W * Bits_Per_Word + B + 1)
          with Pre  => Valid (R) and then (R.Nonempty (W) and Mask (B)) /= 0,
               Post => Is_Set (R.Nonempty, Sender_At'Result);

        Start_Word : constant Word_Index := Word_Of (R.Cursor);
        Start_Bit  : constant Bit_Index := Bit_Of (R.Cursor);
        -- Bits at or after the cursor's, in its word.
        At_Or_After : constant Unsigned_64 := not (Mask (Start_Bit) - 1);
        Bits : Unsigned_64;
    begin
        From := Sender'First;
        At_Position := 0;
        Found := False;
        -- The cursor's word from the cursor on, then the following words,
        -- then the cursor's word before the cursor: round-robin.
        Bits := R.Nonempty (Start_Word) and At_Or_After;
        if Bits /= 0 then
            Serve (Sender_At (Start_Word, Trailing_Zeros (Bits)));
            Found := True;
            return;
        end if;
        -- The words after the cursor's, then those before it (no mod, so
        -- each loop's invariant is a plain range).
        for X in Start_Word + 1 .. Word_Index'Last loop
            if R.Nonempty (X) /= 0 then
                Serve (Sender_At (X, Trailing_Zeros (R.Nonempty (X))));
                Found := True;
                return;
            end if;
            pragma Loop_Invariant (R = R'Loop_Entry);
            pragma Loop_Invariant
              (for all Y in Start_Word + 1 .. X => R.Nonempty (Y) = 0);
        end loop;
        pragma Assert (for all Y in Start_Word + 1 .. Word_Index'Last => R.Nonempty (Y) = 0);
        for X in Word_Index'First .. Start_Word - 1 loop
            if R.Nonempty (X) /= 0 then
                Serve (Sender_At (X, Trailing_Zeros (R.Nonempty (X))));
                Found := True;
                return;
            end if;
            pragma Loop_Invariant (R = R'Loop_Entry);
            pragma Loop_Invariant
              (for all Y in Start_Word + 1 .. Word_Index'Last => R.Nonempty (Y) = 0);
            pragma Loop_Invariant
              (for all Y in Word_Index'First .. X => R.Nonempty (Y) = 0);
        end loop;
        Bits := R.Nonempty (Start_Word) and not At_Or_After;
        if Bits /= 0 then
            Serve (Sender_At (Start_Word, Trailing_Zeros (Bits)));
            Found := True;
            return;
        end if;
        -- Nothing anywhere: the cursor's word is its two halves, and the
        -- loops saw every other word.
        pragma Assert
          (((R.Nonempty (Start_Word) and At_Or_After) or
            (R.Nonempty (Start_Word) and not At_Or_After)) = R.Nonempty (Start_Word));
        pragma Assert (R.Nonempty (Start_Word) = 0);
        pragma Assert (for all X in Word_Index => R.Nonempty (X) = 0);
        pragma Assert (Empty (R));
    end Take;

    procedure Forget (R : in out Receiver_State; From : Sender) is
    begin
        R.Rings (From) := (Generation => 0, Head => 0, Count => 0);
        Clear_Bit (R.Nonempty, From);
    end Forget;

end Kernel_Credits;
