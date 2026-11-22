package body Client_Frame_State with SPARK_Mode is
   procedure Allocate (S : in out State) is
   begin S := (Writable, 0, 0); end Allocate;
   procedure Seal (S : in out State; Protection_OK : Boolean) is
   begin S := (if Protection_OK then (Sealed, 0, 0) else (Uncertain, 0, 0)); end Seal;
   procedure Borrow (S : in out State; Epoch, Ticket : Identity) is
   begin S := (Held, Epoch, Ticket); end Borrow;
   procedure Retire (S : in out State; Epoch, Ticket : Identity; Confirmed : Boolean) is
   begin
      if S.Mode = Held and S.Epoch = Epoch and S.Ticket = Ticket and Confirmed then
         S := (Sealed, 0, 0);
      end if;
   end Retire;
   procedure Reopen (S : in out State; Protection_OK : Boolean) is
   begin S := (if Protection_OK then (Writable, 0, 0) else (Uncertain, 0, 0)); end Reopen;
   procedure Quarantine (S : in out State) is
   begin S.Mode := Uncertain; end Quarantine;
   procedure Release (S : in out State; Readers_Retired, Released : Boolean) is
   begin
      if Readers_Retired and Released then S := (Empty, 0, 0);
      else S.Mode := Uncertain; end if;
   end Release;
end Client_Frame_State;
