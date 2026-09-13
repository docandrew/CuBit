package body Spinlocks is
   procedure enterCriticalSection (S : in out Spinlock) is
   begin
      pragma Assert (not S.Held);
      S.Held := True;
      Locks_Held := Locks_Held + 1;
   end enterCriticalSection;
   procedure exitCriticalSection (S : in out Spinlock) is
   begin
      pragma Assert (S.Held and then Locks_Held > 0);
      S.Held := False;
      Locks_Held := Locks_Held - 1;
   end exitCriticalSection;
end Spinlocks;
