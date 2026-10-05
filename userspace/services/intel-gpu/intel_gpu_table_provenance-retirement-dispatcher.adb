package body Intel_GPU_Table_Provenance.Retirement.Dispatcher is
   use type System.Address;
   function Status (Object : Controller) return State is (Object.Phase);
   procedure Reopen
     (Object : in out Controller; Tables : in out Ledger; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Object.Phase /= Done or else Tables'Address /= Object.Ledger_Address then return; end if;
      Retirement.Reopen (Tables, Object.Owner, Object.Epoch, Accepted);
      if not Accepted then return; end if;
      Object.Owner := 0; Object.Epoch := 0; Object.Ticket := 0;
      Object.Prepared := False; Object.Submitted := False;
      Object.Ledger_Address := System.Null_Address; Object.Phase := Unused;
   end Reopen;
   procedure Start
     (Object : in out Controller; Tables : in out Ledger;
      Session, Generation : Unsigned_64; Accepted : out Boolean;
      Last_Ticket : Unsigned_64 := 0) is
   begin
      Accepted := False;
      if Object.Phase /= Unused then return; end if;
      Object.Phase := Failed;
      Retirement.Start (Tables, Session, Generation, Accepted, Last_Ticket);
      if Accepted then
         Object.Owner := Session; Object.Epoch := Generation;
         Object.Ledger_Address := Tables'Address;
         Object.Phase := Running;
      end if;
   end Start;
   procedure Step (Object : in out Controller; Tables : in out Ledger) is
      OK, Complete, Broken : Boolean;
      Ticket : Unsigned_64;
      function Awaiting_Exact_Ticket return Boolean is
        (Tables.Owner = Object.Owner and then Tables.Epoch = Object.Epoch and then
         Retirement.Phase (Tables) = Awaiting_Ack and then Object.Ticket /= 0 and then
         Retirement.Pending_Ticket (Tables) = Object.Ticket);
      procedure Stop is
      begin
         Object.Phase := Failed;
         -- Retain identities; never reopen or dispatch after ambiguity.
         Tables.Phase := Intel_GPU_Table_Provenance.Failed;
      end Stop;
   begin
      if Object.Phase /= Running then return; end if;
      if Tables'Address /= Object.Ledger_Address then
         -- Do not poison an unrelated ledger passed by mistake. The original
         -- remains admission-closed; this controller can no longer dispatch.
         Object.Phase := Failed; return;
      end if;
      if Tables.Owner /= Object.Owner or else Tables.Epoch /= Object.Epoch or else
        not Context_Released (Object.Owner) then Stop; return; end if;
      case Retirement.Phase (Tables) is
         when Searching =>
            if Object.Ticket /= 0 then
               -- Sweep completed for this acknowledged allocation. Preserve
               -- its identity until retained metadata is finalized exactly once.
               if not Release_Confirmed (Object.Owner, Object.Ticket) or else
                 not Context_Released (Object.Owner) then Stop; return; end if;
               Finalize_Ticket (Object.Owner, Object.Ticket, OK);
               if not OK or else not Context_Released (Object.Owner) or else
                 Tables.Owner /= Object.Owner or else Tables.Epoch /= Object.Epoch or else
                 Retirement.Phase (Tables) /= Searching
               then Stop; return; end if;
               Object.Ticket := 0;
               Object.Prepared := False; Object.Submitted := False;
            else Retirement.Step (Tables); end if;
         when Sweeping =>
            Retirement.Step (Tables);
         when Request_Ready =>
            Retirement.Take_Request (Tables, Object.Owner, Ticket, OK);
            if not OK or else Ticket = 0 then Stop; return; end if;
            Object.Ticket := Ticket;
            Object.Prepared := False; Object.Submitted := False;
         when Awaiting_Ack =>
            if Object.Ticket = 0 or else
              Retirement.Pending_Ticket (Tables) /= Object.Ticket
            then Stop; return; end if;
            if not Object.Prepared then
               Prepare_Ticket (Object.Owner, Object.Ticket, Complete, Broken);
               if Broken or else not Context_Released (Object.Owner) or else
                 not Awaiting_Exact_Ticket
               then Stop; return; end if;
               Object.Prepared := Complete;
               return;
            elsif not Object.Submitted then
               Submit (Object.Owner, Object.Ticket, OK);
               if not OK or else not Context_Released (Object.Owner) or else
                 not Awaiting_Exact_Ticket then Stop; return; end if;
               Object.Submitted := True;
               return;
            end if;
            Poll (Object.Owner, Object.Ticket, Complete, Broken);
            if Broken or else not Context_Released (Object.Owner) or else
              not Awaiting_Exact_Ticket then Stop; return; end if;
            if Complete then
               Retirement.Acknowledge (Tables, Object.Owner, Object.Ticket, OK);
               if not OK then Stop; return; end if;
            end if;
         when Intel_GPU_Table_Provenance.Complete => Object.Phase := Done;
         when others => Stop;
      end case;
      if Retirement.Phase (Tables) = Intel_GPU_Table_Provenance.Failed then Stop; end if;
   end Step;
end Intel_GPU_Table_Provenance.Retirement.Dispatcher;
