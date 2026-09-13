package Spinlocks is
   type Spinlock is record
      Held : Boolean := False;
   end record;
   Locks_Held : Natural := 0;
   procedure enterCriticalSection (S : in out Spinlock);
   procedure exitCriticalSection (S : in out Spinlock);
end Spinlocks;
