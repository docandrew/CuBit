package body Spinlocks is
    procedure enterCriticalSection (S : in out Spinlock) is
        Before : constant Hook := Before_Next_Lock;
    begin
        Before_Next_Lock := null;
        if Before /= null then
            Before.all; -- deterministic competing operation before acquisition
        end if;
        pragma Assert (not S.Held, "recursive slab lock acquisition");
        S.Held := True;
    end enterCriticalSection;
    procedure exitCriticalSection (S : in out Spinlock) is
    begin
        pragma Assert (S.Held);
        S.Held := False;
    end exitCriticalSection;
end Spinlocks;
