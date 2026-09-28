package body Futex_Protocol with SPARK_Mode => On is

    procedure Prove_Initial (V : Unsigned_32) is null;

    procedure Store (S : in out State; I : Thread; V : Unsigned_32) is
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
        S.T (I).P := (if Slept then Sleeping else Running);
        S.Holder := False;
    end Wait_Commit;

    procedure Wake_All (S : in out State) is
    begin
        for I in Thread loop
            pragma Loop_Invariant (not S.Holder);
            pragma Loop_Invariant (S.Value = S'Loop_Entry.Value);
            pragma Loop_Invariant (Lock_Consistent (S));
            pragma Loop_Invariant
              (for all J in Thread'First .. I - 1 => S.T (J).P /= Sleeping);
            pragma Loop_Invariant
              (for all J in Thread =>
                 (if S.T (J).P = Sleeping then
                    S.T (J).Expected = S.Value or else S.Pending));
            if S.T (I).P = Sleeping then
                S.T (I).P := Running;
            end if;
        end loop;
        S.Pending := False;
    end Wake_All;

    procedure Wake_One (S : in out State; I : Thread) is
    begin
        S.T (I).P := Running;
    end Wake_One;

    procedure Prove_No_Stale_Sleeper (S : State) is null;

    procedure Prove_Racing_Store_Covered (V0, V1 : Unsigned_32) is
        S : State := Initial (V0);
        Slept : Boolean;
    begin
        Prove_Initial (V0);
        -- Thread 1 waits for V0 to change: it loads V0 under the lock.
        Wait_Load (S, 1, V0);
        -- Thread 2 changes the word before thread 1 commits.
        Store (S, 2, V1);
        pragma Assert (S.Pending);
        -- Thread 1 commits on the stale load and sleeps.
        Wait_Commit (S, 1, Slept);
        pragma Assert (Slept);
        -- Thread 2's wake needs the lock thread 1 has released; it wakes it.
        Wake_All (S);
        pragma Assert (S.T (1).P = Running);
    end Prove_Racing_Store_Covered;

end Futex_Protocol;
