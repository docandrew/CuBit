pragma Ada_2022;

package body Word_Protocol with SPARK_Mode => On is

    procedure Prove_Initial (V : Unsigned_32) is null;

    procedure Store (S : in out State; I : Thread; V : Unsigned_32) is
        pragma Unreferenced (I);  -- any running thread
    begin
        if V /= S.Value then
            S.Pending := True;
        end if;
        S.Value := V;
    end Store;

    procedure Wait_Load (S : in out State; I : Thread; E : Unsigned_32) is
    begin
        S.Holder := True;
        S.Owner := I;
        S.T (I) := (P => Loaded, Expected => E, Seen => S.Value);
    end Wait_Load;

    procedure Wait_Commit (S : in out State; I : Thread; Slept : out Boolean) is
    begin
        Slept := S.T (I).Seen = S.T (I).Expected;
        if Slept then
            Enqueue (S.Tab, Slot_Of (I), The_Word, Ref_Of (I));
            S.T (I).P := Sleeping;
        else
            S.T (I).P := Running;
        end if;
        S.Holder := False;
    end Wait_Commit;

    -- The threads whose entries a batch removed run again.
    procedure Ready_Taken (S : in out State; Before : Table)
    with Global => null,
         Pre  => Valid (S.Tab) and then Subset (S.Tab, Before)
                 and then (for all U in Slot => (if U > 2 then not Before.Used (U)))
                 and then (for all I in Thread => (S.T (I).P = Sleeping) = Before.Used (Slot_Of (I))),
         Post => (for all I in Thread => (S.T (I).P = Sleeping) = S.Tab.Used (Slot_Of (I)))
                 and then (for all I in Thread =>
                             (if S'Old.T (I).P /= Sleeping then S.T (I) = S'Old.T (I)))
                 and then (for all I in Thread =>
                             (if S.T (I).P = Sleeping then S.T (I) = S'Old.T (I)))
                 and then (for all I in Thread => S.T (I).P /= Loaded or else S.T (I) = S'Old.T (I))
                 and then S.Tab = S'Old.Tab and then S.Value = S'Old.Value
                 and then S.Holder = S'Old.Holder and then S.Owner = S'Old.Owner
                 and then S.Pending = S'Old.Pending
    is
    begin
        for J in Thread loop
            pragma Loop_Invariant
              (for all I in Thread'First .. J - 1 => (S.T (I).P = Sleeping) = S.Tab.Used (Slot_Of (I)));
            pragma Loop_Invariant
              (for all I in J .. Thread'Last => S.T (I) = S.T'Loop_Entry (I));
            pragma Loop_Invariant
              (for all I in Thread =>
                 (if S'Loop_Entry.T (I).P /= Sleeping then S.T (I) = S'Loop_Entry.T (I)));
            pragma Loop_Invariant
              (for all I in Thread => (if S.T (I).P = Sleeping then S.T (I) = S'Loop_Entry.T (I)));
            pragma Loop_Invariant
              (for all I in Thread => S.T (I).P /= Loaded or else S.T (I) = S'Loop_Entry.T (I));
            pragma Loop_Invariant
              (S.Tab = S'Loop_Entry.Tab and then S.Value = S'Loop_Entry.Value and then S.Holder = S'Loop_Entry.Holder
               and then S.Owner = S'Loop_Entry.Owner and then S.Pending = S'Loop_Entry.Pending);
            if S.T (J).P = Sleeping and then not S.Tab.Used (Slot_Of (J)) then
                S.T (J).P := Running;
            end if;
        end loop;
    end Ready_Taken;

    pragma Warnings (GNATprove, Off, "* is set by * but not used after the call");

    procedure Wake_All (S : in out State) is
        Taken : Batch;
        Count : Batch_Count;
        Before : Table;
    begin
        loop
            pragma Loop_Invariant (Inv (S) and then not S.Holder and then S.Value = S'Loop_Entry.Value);
            Before := S.Tab;
            Take_Batch (S.Tab, The_Word, Chunk, True, Taken, Count);
            Ready_Taken (S, Before);
            exit when Count < Chunk;
        end loop;
        S.Pending := False;
    end Wake_All;

    procedure Wake_One (S : in out State; Woke : out Boolean) is
        Taken : Batch;
        Count : Batch_Count;
        Before : constant Table := S.Tab;
    begin
        Take_Batch (S.Tab, The_Word, 1, False, Taken, Count);
        if Count = 1 then
            pragma Assert (for some U in Slot => Before.Used (U) and then not S.Tab.Used (U));
            pragma Assert (Before.Used (0) or else Before.Used (1) or else Before.Used (2));
            pragma Assert (S.T (1).P = Sleeping or else S.T (2).P = Sleeping
                           or else S.T (3).P = Sleeping);
        end if;
        Ready_Taken (S, Before);
        Woke := Count = 1;
    end Wake_One;

    procedure Prove_No_Stale_Sleeper (S : State) is null;

end Word_Protocol;
