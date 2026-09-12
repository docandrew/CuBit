package body Deadline_Ownership with SPARK_Mode is
   procedure Arm (S : in out State; Identity : Owner; At_Tick : Tick;
                  Issued : out Ticket; Accepted : out Boolean) is
   begin
      Accepted := Identity /= No_Owner and S.Data.Serial < Max_Ticket;
      Issued := 0;
      if Accepted then
         S.Data := (True, Identity, At_Tick, S.Data.Serial + 1);
         Issued := S.Data.Serial;
      end if;
   end Arm;

   procedure Cancel (S : in out State; Identity : Owner; Issued : Ticket;
                     Cancelled : out Boolean) is
   begin
      Cancelled := S.Data.Active and S.Data.Identity = Identity and S.Data.Serial = Issued;
      if Cancelled then S.Data.Active := False; end if;
   end Cancel;

   procedure Poll (S : in out State; Now : Tick; Current : Owner;
                   Result : out Outcome; Lateness : out Tick) is
   begin
      Result := Nothing_Due;
      Lateness := 0;
      if S.Data.Active and then Now >= S.Data.Deadline then
         S.Data.Active := False;
         Lateness := Now - S.Data.Deadline;
         Result := (if Current = S.Data.Identity then Expired else Stale_Owner);
      end if;
   end Poll;

   function Next_Interrupt (S : State; Clock_Deadline : Tick) return Tick is
     (if S.Data.Active then Tick'Min (S.Data.Deadline, Clock_Deadline) else Clock_Deadline);
end Deadline_Ownership;
