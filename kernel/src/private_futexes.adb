-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-------------------------------------------------------------------------------
pragma Ada_2022;

package body Private_Futexes with SPARK_Mode => On is

    procedure Enqueue (T : in out Table; S : Slot; W : Unsigned_64; Thread : Thread_Ref) is
    begin
        T.Word (S) := W;
        T.Ticket (S) := T.Next_Ticket;
        T.Thread (S) := Thread;
        T.Used (S) := True;
        T.Next_Ticket := T.Next_Ticket + 1;
    end Enqueue;

    procedure Remove (T : in out Table; S : Slot; Thread : Thread_Ref; Removed : out Boolean) is
    begin
        Removed := T.Used (S) and then T.Thread (S) = Thread;
        if Removed then
            T.Used (S) := False;
        end if;
    end Remove;

    procedure Find_Oldest (T : Table; W : Unsigned_64; Found : out Boolean; S : out Slot) is
    begin
        Found := False;
        S := Slot'First;
        for U in Slot loop
            pragma Loop_Invariant
              (if Found then
                 T.Used (S) and then T.Word (S) = W
                 and then (for all V in Slot'First .. U - 1 =>
                             (if T.Used (V) and then T.Word (V) = W then T.Ticket (S) <= T.Ticket (V)))
               else (for all V in Slot'First .. U - 1 => not (T.Used (V) and then T.Word (V) = W)));
            if T.Used (U) and then T.Word (U) = W
              and then (not Found or else T.Ticket (U) < T.Ticket (S))
            then
                Found := True;
                S := U;
            end if;
        end loop;
    end Find_Oldest;

    procedure Take_Batch (T : in out Table; W : Unsigned_64; Max : Batch_Count;
                          All_Of_Them : Boolean; Taken : out Batch; Count : out Batch_Count)
    is
        Found : Boolean;
        S : Slot;
    begin
        Count := 0;
        Taken := [others => No_Thread];
        if Max = 0 then
            return;
        elsif All_Of_Them then
            for U in Slot loop
                pragma Loop_Invariant (Valid (T) and then Subset (T, T'Loop_Entry));
                pragma Loop_Invariant (Count < Max);
                pragma Loop_Invariant (T.Next_Ticket = T'Loop_Entry.Next_Ticket);
                pragma Loop_Invariant
                  (for all V in Slot =>
                     (if T'Loop_Entry.Used (V) and then not T.Used (V) then
                        V < U and then T'Loop_Entry.Word (V) = W));
                pragma Loop_Invariant
                  (for all V in Slot'First .. U - 1 => not (T.Used (V) and then T.Word (V) = W));
                pragma Loop_Invariant
                  (for all V in U .. Slot'Last => T.Used (V) = T'Loop_Entry.Used (V));
                pragma Loop_Invariant
                  (for all I in 1 .. Count =>
                     Taken (I) /= No_Thread
                     and then (for some V in Slot =>
                                 T'Loop_Entry.Used (V) and then not T.Used (V)
                                 and then T'Loop_Entry.Thread (V) = Taken (I)));
                pragma Loop_Invariant
                  (for all V in Slot =>
                     (if T'Loop_Entry.Used (V) and then not T.Used (V) then
                        (for some I in 1 .. Count => Taken (I) = T'Loop_Entry.Thread (V))));
                if T.Used (U) and then T.Word (U) = W then
                    Count := Count + 1;
                    Taken (Count) := T.Thread (U);
                    T.Used (U) := False;
                    exit when Count = Max;
                end if;
            end loop;
        else
            while Count < Max loop
                pragma Loop_Invariant (Valid (T) and then Subset (T, T'Loop_Entry));
                pragma Loop_Invariant (T.Next_Ticket = T'Loop_Entry.Next_Ticket);
                pragma Loop_Invariant
                  (for all V in Slot =>
                     (if T'Loop_Entry.Used (V) and then not T.Used (V) then
                        T'Loop_Entry.Word (V) = W));
                pragma Loop_Invariant
                  (for all U in Slot =>
                     (for all V in Slot =>
                        (if T.Used (U) and then T.Word (U) = W
                            and then T'Loop_Entry.Used (V) and then not T.Used (V)
                         then T'Loop_Entry.Ticket (V) < T.Ticket (U))));
                pragma Loop_Invariant
                  (for all I in 1 .. Count =>
                     Taken (I) /= No_Thread
                     and then (for some V in Slot =>
                                 T'Loop_Entry.Used (V) and then not T.Used (V)
                                 and then T'Loop_Entry.Thread (V) = Taken (I)));
                pragma Loop_Invariant
                  (for all V in Slot =>
                     (if T'Loop_Entry.Used (V) and then not T.Used (V) then
                        (for some I in 1 .. Count => Taken (I) = T'Loop_Entry.Thread (V))));
                Find_Oldest (T, W, Found, S);
                exit when not Found;
                Count := Count + 1;
                Taken (Count) := T.Thread (S);
                T.Used (S) := False;
            end loop;
        end if;
    end Take_Batch;

end Private_Futexes;
