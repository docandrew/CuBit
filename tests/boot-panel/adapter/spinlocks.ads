-- Hosted control-flow mock, NOT a test/proof of IRQ or SMP locking.
package Spinlocks is
   type Spinlock is record
      Held : Boolean := False;
   end record;
   Busy : Boolean := False;
   procedure enterCriticalSection (S : in out Spinlock);
   procedure tryEnterCriticalSection (S : in out Spinlock; Acquired : out Boolean);
   procedure exitCriticalSection (S : in out Spinlock);
end Spinlocks;
