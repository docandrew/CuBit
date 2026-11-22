package Spinlocks is
    type Spinlock is record
        Held : Boolean := False;
    end record;
    type Hook is access procedure;
    Before_Next_Lock : Hook := null;
    procedure enterCriticalSection (S : in out Spinlock);
    procedure exitCriticalSection (S : in out Spinlock);
end Spinlocks;
