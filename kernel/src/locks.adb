package body Locks with SPARK_Mode => On is
    procedure Acquire (S : in out State; CPU : CPU_ID;
                       Result : out Acquire_Result) is
    begin
        if S = Unowned then
            S := State (CPU);
            Result := Acquired;
        elsif Owned_By (S, CPU) then
            Result := Reentrant;
        else
            Result := Contended;
        end if;
    end Acquire;

    procedure Release (S : in out State; CPU : CPU_ID; Success : out Boolean) is
    begin
        Success := Owned_By (S, CPU);
        if Success then
            S := Unowned;
        end if;
    end Release;

    procedure Prove_Exclusive_Owner (Owner, Other : CPU_ID) is
        S : State := Unowned;
        Result : Acquire_Result;
        Success : Boolean;
    begin
        Acquire (S, Owner, Result);
        pragma Assert (Result = Acquired);
        Acquire (S, Other, Result);
        pragma Assert (Result = Contended and Owned_By (S, Owner));
        Release (S, Other, Success);
        pragma Assert (not Success and Owned_By (S, Owner));
        Acquire (S, Owner, Result);
        pragma Assert (Result = Reentrant);
        Release (S, Owner, Success);
        pragma Assert (Success and not Is_Locked (S));
    end Prove_Exclusive_Owner;
end Locks;
