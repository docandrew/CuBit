package body Wake_Word_Protocol with SPARK_Mode => On is

    procedure Wait_Load (S : in out State; E : Unsigned_32; Back : out Boolean) is
    begin
        Back := S.Activity or else S.Flag or else S.Value /= E;
        if Back then
            -- Taken: the wait returns with it.
            S.Flag := False;
        else
            S.P := Loaded;
            S.Expected := E;
            S.Holder := True;
        end if;
    end Wait_Load;

    procedure Wait_Commit (S : in out State) is
    begin
        S.P := Sleeping;
        S.Holder := False;
    end Wait_Commit;

    procedure Store (S : in out State; V : Unsigned_32) is
    begin
        if V /= S.Value then
            S.Owed := True;
        end if;
        S.Value := V;
    end Store;

    procedure Note_Wake (S : in out State) is
    begin
        S.Flag := True;
        S.Owed := False;
        if S.P = Sleeping then
            S.P := Running;
        end if;
    end Note_Wake;

    procedure Enqueue (S : in out State) is
    begin
        S.Activity := True;
        if S.P = Sleeping then
            S.P := Running;
        end if;
    end Enqueue;

    procedure Prove_No_Lost_Wake (S : State) is null;

    procedure Prove_Racing_Wake (V0, V1 : Unsigned_32) is
        S : State := Initial (V0);
        Back : Boolean;
    begin
        Wait_Load (S, V0, Back);       -- nothing pending: it will sleep
        pragma Assert (not Back);
        Store (S, V1);                 -- a peer changes the word meanwhile
        Wait_Commit (S);               -- it sleeps on the stale value
        pragma Assert (S.P = Sleeping and then S.Owed);
        Note_Wake (S);                 -- the owed wake, under the lock
        pragma Assert (S.P = Running and then S.Flag);
        Wait_Load (S, V0, Back);       -- the next look returns at once
        pragma Assert (Back);
    end Prove_Racing_Wake;

end Wake_Word_Protocol;
