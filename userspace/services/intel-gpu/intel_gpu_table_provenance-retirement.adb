package body Intel_GPU_Table_Provenance.Retirement is
   function Phase (Object : Ledger) return Retirement_Phase is (Object.Phase);
   function Pending_Ticket (Object : Ledger) return Unsigned_64 is
     (if Object.Phase = Awaiting_Ack then Object.Pending else 0);
   procedure Start (Object : in out Ledger; Session, Expected_Generation : Unsigned_64;
                    Accepted : out Boolean; Last_Ticket : Unsigned_64 := 0) is
   begin
      Accepted := False;
      if Expected_Generation /= Object.Epoch or else Object.Phase /= Open or else Session = 0 or else Session /= Object.Owner
        or else not Context_Released (Session) then return; end if;
      Object.Last_Ticket := Last_Ticket; Object.Include_Last := False;
      Object.Phase := Searching; Object.Cursor := 1; Accepted := True;
   end Start;
   procedure Reopen (Object : in out Ledger; Session, Expected_Generation : Unsigned_64;
                     Accepted : out Boolean) is
   begin
      Accepted := False;
      if Object.Phase /= Complete or else Session = 0 or else Session /= Object.Owner
        or else Expected_Generation /= Object.Epoch or else Object.Epoch = Unsigned_64'Last
        or else Object.Pending /= 0 or else not Context_Released (Session)
      then return; end if;
      -- Complete is reachable only after the bounded census found no retained
      -- tickets; each clearing sweep required the exact confirmed release.
      Object.Epoch := Object.Epoch + 1;
      Object.Owner := 0; Object.Used := 0; Object.Cursor := 1;
      Object.Last_Ticket := 0; Object.Include_Last := False;
      Object.Phase := Open; Accepted := True;
   end Reopen;
   procedure Recycle_Confirmed
     (Object : in out Ledger; Session, Expected_Generation, Ticket : Unsigned_64;
      Accepted : out Boolean)
   is
      OK : Boolean;
      Requested : Unsigned_64;
   begin
      Accepted := False;
      if Object.Phase /= Open or else Session = 0 or else Session /= Object.Owner
        or else Expected_Generation /= Object.Epoch or else Ticket = 0
        or else Object.Used not in 1 .. 64
      then return; end if;
      for I in 1 .. Object.Used loop
         if Records.Get (Object.Items, I).Ticket /= Ticket then return; end if;
      end loop;
      if not Release_Confirmed (Session, Ticket) then return; end if;
      Start (Object, Session, Expected_Generation, OK);
      if not OK then return; end if;
      Step (Object);
      Take_Request (Object, Session, Requested, OK);
      if not OK or else Requested /= Ticket then return; end if;
      Acknowledge (Object, Session, Ticket, OK);
      if not OK then return; end if;
      Step (Object); Step (Object);
      if Object.Phase /= Complete then return; end if;
      Reopen (Object, Session, Expected_Generation, Accepted);
   end Recycle_Confirmed;
   procedure Step (Object : in out Ledger) is
      Last : Natural;
      Item : Mapping;
   begin
      if Object.Phase not in Searching | Sweeping then return; end if;
      if not Context_Released (Object.Owner) then Object.Phase := Failed; return; end if;
      if Object.Cursor > Object.Used then Object.Phase := Complete; return; end if;
      Last := Object.Cursor + Natural'Min (63, Object.Used - Object.Cursor);
      for I in Object.Cursor .. Last loop
         Item := Records.Get (Object.Items, I);
         if Object.Phase = Searching and then Item.Ticket /= 0 and then
           (Object.Include_Last or else Item.Ticket /= Object.Last_Ticket)
         then
            if not May_Release (Object.Owner, Item.Ticket)
              or else not Context_Released (Object.Owner)
            then Object.Phase := Failed; return; end if;
            Object.Pending := Item.Ticket; Object.Phase := Request_Ready; return;
         elsif Object.Phase = Sweeping and Item.Ticket = Object.Pending then
            Records.Put (Object.Items, I, (others => 0));
         end if;
      end loop;
      if Last = Object.Used then
         if Object.Phase = Sweeping then
            Object.Pending := 0; Object.Cursor := 1; Object.Phase := Searching;
         elsif Object.Last_Ticket /= 0 and then not Object.Include_Last then
            -- A complete bounded census found no other retained tickets.
            -- The next pass may ask for the parent, still subject to its own
            -- consumer-exclusion and exact supervisor acknowledgment checks.
            Object.Include_Last := True; Object.Cursor := 1;
         else Object.Phase := Complete; end if;
      else Object.Cursor := Last + 1; end if;
   end Step;
   procedure Take_Request
     (Object : in out Ledger; Session : Unsigned_64;
      Ticket : out Unsigned_64; Accepted : out Boolean) is
   begin
      Ticket := 0; Accepted := False;
      if Object.Phase /= Request_Ready or else Session /= Object.Owner then return; end if;
      if not Context_Released (Session) or else not May_Release (Session, Object.Pending)
        or else not Context_Released (Session)
      then Object.Phase := Failed; return; end if;
      Object.Phase := Awaiting_Ack; Ticket := Object.Pending; Accepted := True;
   end Take_Request;
   procedure Acknowledge
     (Object : in out Ledger; Session, Ticket : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Object.Phase /= Awaiting_Ack or else Session /= Object.Owner
        or else Ticket = 0 or else Ticket /= Object.Pending then return; end if;
      if not Context_Released (Session) or else not Release_Confirmed (Session, Ticket)
        or else not Context_Released (Session)
      then Object.Phase := Failed; return; end if;
      Object.Cursor := 1; Object.Phase := Sweeping; Accepted := True;
   end Acknowledge;
end Intel_GPU_Table_Provenance.Retirement;
