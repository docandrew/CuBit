with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Table_Provenance;
with Intel_GPU_Table_Provenance.Retirement;
with Intel_GPU_Table_Provenance.Retirement.Dispatcher;
procedure Table_Dispatcher_Tests is
   package P renames Intel_GPU_Table_Provenance;
   use type P.Retirement_Phase;
begin
   for Stop_At in Unsigned_64 range 1 .. 3 loop
   for Fault in 0 .. 13 loop
      declare
         Tables : P.Ledger;
         Other : P.Ledger;
         type Bytes is array (1 .. 8192) of Unsigned_8;
         Metadata : Bytes := [others => 0] with Alignment => 4096;
         Held : Boolean := True;
         Submitted, Confirmed : Unsigned_64 := 0;
         Calls, Polls, Turns : Natural := 0;
         Finalizations : array (1 .. 3) of Natural := [others => 0];
         procedure Resolve (Session, Ticket, Offset : Unsigned_64;
                            CPU, DMA : out Unsigned_64; Accepted : out Boolean) is
         begin
            Accepted := Session = 42 and Ticket in 1 .. 3;
            CPU := 16#1000000# + Ticket * 16#100000# + Offset;
            DMA := Ticket * 16#100000# + Offset;
         end Resolve;
         package A is new P.Authority (Resolve);
         function Released (Session : Unsigned_64) return Boolean is (Held and Session = 42);
         function May_Release (Session, Ticket : Unsigned_64) return Boolean is
           (Released (Session) and Ticket in 1 .. 3);
         function Ack (Session, Ticket : Unsigned_64) return Boolean is
           (Released (Session) and Ticket = Confirmed);
         package R is new P.Retirement (Released, May_Release, Ack);
         procedure Advance_Unexpectedly (Session, Ticket : Unsigned_64) is
            Accepted : Boolean;
         begin
            -- Model a backend accidentally acknowledging the shared ledger
            -- itself. Only the dispatcher may advance this transaction.
            Confirmed := Ticket;
            R.Acknowledge (Tables, Session, Ticket, Accepted);
            pragma Assert (Accepted);
         end Advance_Unexpectedly;
         procedure Submit (Session, Ticket : Unsigned_64; Accepted : out Boolean) is
         begin
            pragma Assert (Session = 42 and Ticket = Submitted + 1);
            Submitted := Ticket; Calls := Calls + 1; Polls := 0;
            Accepted := Fault /= 1 or Ticket /= Stop_At;
            if Fault = 2 and Ticket = Stop_At then Held := False; end if;
            if Fault = 11 and Ticket = Stop_At then Advance_Unexpectedly (Session, Ticket); end if;
         end Submit;
         procedure Poll (Session, Ticket : Unsigned_64; Complete, Failed : out Boolean) is
         begin
            pragma Assert (Session = 42 and Ticket = Submitted);
            Calls := Calls + 1; Polls := Polls + 1;
            Complete := Polls = 3;
            Failed := Fault = 3 and Ticket = Stop_At;
            if Complete and (Fault /= 4 or Ticket /= Stop_At) then Confirmed := Ticket; end if;
            if Fault = 5 and Ticket = Stop_At then Held := False; end if;
            if Fault = 12 and Ticket = Stop_At then Advance_Unexpectedly (Session, Ticket); end if;
         end Poll;
         procedure Finalize (Session, Ticket : Unsigned_64; Accepted : out Boolean) is
            Found, Valid : Boolean;
            Next : Natural := 1;
         begin
            Calls := Calls + 1;
            pragma Assert (Ack (Session, Ticket));
            pragma Assert (R.Phase (Tables) = P.Searching);
            while Next /= 0 loop
               P.Scan_Ticket (Tables, Session, Ticket, Next, Found, Next, Valid);
               pragma Assert (Valid and not Found);
            end loop;
            Finalizations (Natural (Ticket)) := Finalizations (Natural (Ticket)) + 1;
            pragma Assert (Finalizations (Natural (Ticket)) = 1);
            Accepted := Fault /= 7 or Ticket /= Stop_At;
            if Fault = 8 and Ticket = Stop_At then Held := False; end if;
         end Finalize;
         Preparation_Ticket : Unsigned_64 := 0;
         Preparation_Steps : Natural := 0;
         procedure Prepare (Session, Ticket : Unsigned_64; Complete, Failed : out Boolean) is
         begin
            Calls := Calls + 1;
            pragma Assert (Session = 42 and Ticket = Submitted + 1);
            if Preparation_Ticket /= Ticket then
               Preparation_Ticket := Ticket; Preparation_Steps := 0;
            end if;
            Preparation_Steps := Preparation_Steps + 1;
            Complete := Preparation_Steps = 2;
            Failed := Fault = 9 and Ticket = Stop_At;
            if Failed then Complete := True; end if; -- failure wins
            if Fault = 10 and Ticket = Stop_At then Held := False; Complete := True; end if;
            if Fault = 13 and Ticket = Stop_At then Advance_Unexpectedly (Session, Ticket); end if;
         end Prepare;
         package D is new R.Dispatcher (Prepare, Submit, Poll, Finalize);
         use type D.State;
         Control : D.Controller;
         OK : Boolean;
         Before : Natural;
         Found, Any_Found : Boolean;
         Next : Natural;
      begin
         P.Extend (Tables, Unsigned_64 (To_Integer (Metadata'Address)), 8192, OK);
         pragma Assert (OK);
         for I in 1 .. 150 loop
            A.Install (Tables, 42, 1, I, Unsigned_64 ((I - 1) / 50 + 1),
                       Unsigned_64 ((I - 1) mod 50) * 4096, OK);
            pragma Assert (OK);
         end loop;
         D.Start (Control, Tables, 42, 1, OK); pragma Assert (OK);
         A.Install (Other, 42, 1, 1, 1, 0, OK); pragma Assert (OK);
         R.Start (Other, 42, 1, OK); pragma Assert (OK);
         while D.Status (Control) = D.Running loop
            Turns := Turns + 1; pragma Assert (Turns < 100);
            Before := Calls;
            if Fault = 6 and Submitted = Stop_At - 1 and R.Phase (Tables) = P.Searching then
               D.Step (Control, Other);
               pragma Assert (R.Phase (Other) = P.Searching and Calls = Before);
            else D.Step (Control, Tables); end if;
            pragma Assert (Calls <= Before + 1);
         end loop;
         if Fault = 0 then
            pragma Assert (D.Status (Control) = D.Done and Submitted = 3 and Calls = 21);
            D.Reopen (Control, Other, OK); pragma Assert (not OK);
            D.Reopen (Control, Tables, OK);
            pragma Assert (OK and P.Generation (Tables) = 2);
            pragma Assert (D.Status (Control) = D.Unused);
         else
            pragma Assert (D.Status (Control) = D.Failed);
            pragma Assert (Submitted = (if Fault in 6 | 9 | 10 | 13 then Stop_At - 1 else Stop_At));
            pragma Assert (R.Phase (Tables) = (if Fault = 6 then P.Searching else P.Failed));
            pragma Assert (P.Count (Tables) = 150);
            for Ticket in Unsigned_64 range 1 .. 3 loop
               Next := 1; Any_Found := False;
               while Next /= 0 loop
                  P.Scan_Ticket (Tables, 42, Ticket, Next, Found, Next, OK);
                  pragma Assert (OK);
                  Any_Found := Any_Found or Found;
               end loop;
               pragma Assert (Any_Found = (if Fault in 7 | 8 then Ticket > Stop_At else Ticket >= Stop_At));
            end loop;
            Held := True; Before := Calls;
            D.Start (Control, Tables, 42, 1, OK); pragma Assert (not OK);
            D.Step (Control, Tables); pragma Assert (Calls = Before);
            R.Reopen (Tables, 42, 1, OK); pragma Assert (not OK);
            D.Reopen (Control, Tables, OK); pragma Assert (not OK);
         end if;
      end;
   end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Table dispatcher PASS42: callback ledger mutation rejected before sweep, yielded preparation, delayed exact acknowledgments, once-only finalization, partial failures, wrong ledger, no replay");
end Table_Dispatcher_Tests;
