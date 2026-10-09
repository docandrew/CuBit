package Spinlocks is
   protected type Spinlock is
      entry Enter;
      procedure Leave;
   private
      Locked : Boolean := False;
   end Spinlock;
   procedure enterCriticalSection (Object : in out Spinlock);
   procedure exitCriticalSection (Object : in out Spinlock);
   function Held_By_Caller return Boolean;
end Spinlocks;
