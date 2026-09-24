package body Spinlocks is
   procedure enterCriticalSection (S : in out Spinlock) is
   begin
      pragma Assert (not S.Held and not Busy);
      S.Held := True;
   end;
   procedure tryEnterCriticalSection (S : in out Spinlock; Acquired : out Boolean) is
   begin
      Acquired := not S.Held and not Busy;
      if Acquired then S.Held := True; end if;
   end;
   procedure exitCriticalSection (S : in out Spinlock) is
   begin
      pragma Assert (S.Held);
      S.Held := False;
   end;
end Spinlocks;
