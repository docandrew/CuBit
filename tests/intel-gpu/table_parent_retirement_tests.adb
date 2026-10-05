with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Table_Provenance;
with Intel_GPU_Table_Provenance.Retirement;
with Intel_GPU_Table_Provenance.Retirement.Dispatcher;
procedure Table_Parent_Retirement_Tests is
   package P renames Intel_GPU_Table_Provenance;
begin
   for Fault in 0 .. 4 loop
      declare
         Ledger : P.Ledger;
         type Bytes is array (1 .. 8192) of Unsigned_8;
         Metadata : Bytes := [others => 0] with Alignment => 4096;
         Ack : array (1 .. 3) of Boolean := [others => False];
         Submitted, Calls, Turns : Natural := 0;
         Active : Unsigned_64 := 0;
         procedure Resolve (Session, Ticket, Offset : Unsigned_64;
                            CPU, DMA : out Unsigned_64; Accepted : out Boolean) is
         begin
            Accepted := Session = 42 and Ticket in 1 .. 3;
            CPU := 16#1000000# + Ticket * 16#100000# + Offset;
            DMA := Ticket * 16#100000# + Offset;
         end Resolve;
         package Authority is new P.Authority (Resolve);
         function Released (Session : Unsigned_64) return Boolean is (Session = 42);
         function May_Release (Session, Ticket : Unsigned_64) return Boolean is
           (Session = 42 and then Ticket in 1 .. 3 and then
            (Ticket /= 1 or else (Ack (2) and Ack (3) and Fault /= 1)));
         function Confirmed (Session, Ticket : Unsigned_64) return Boolean is
           (Session = 42 and then Ticket in 1 .. 3 and then Ack (Natural (Ticket)));
         package Retirement is new P.Retirement (Released, May_Release, Confirmed);
         procedure Submit (Session, Ticket : Unsigned_64; Accepted : out Boolean) is
            Order : constant array (1 .. 3) of Unsigned_64 := [2, 3, 1];
         begin
            Calls := Calls + 1; Submitted := Submitted + 1;
            pragma Assert (Submitted <= 3 and then Ticket = Order (Submitted));
            pragma Assert (May_Release (Session, Ticket));
            Active := Ticket; Accepted := True;
         end Submit;
         procedure Poll (Session, Ticket : Unsigned_64; Complete, Failed : out Boolean) is
         begin
            Calls := Calls + 1;
            pragma Assert (Session = 42 and Ticket = Active);
            Failed := (Fault = 2 and Ticket = 2) or (Fault = 3 and Ticket = 3) or
              (Fault = 4 and Ticket = 1);
            Complete := not Failed;
            if Complete then Ack (Natural (Ticket)) := True; end if;
         end Poll;
         procedure Finalize (Session, Ticket : Unsigned_64; Accepted : out Boolean) is
         begin
            Calls := Calls + 1; Accepted := Confirmed (Session, Ticket);
         end Finalize;
         procedure Prepare (Session, Ticket : Unsigned_64; Complete, Failed : out Boolean) is
         begin
            Calls := Calls + 1;
            Complete := May_Release (Session, Ticket); Failed := not Complete;
         end Prepare;
         package Dispatch is new Retirement.Dispatcher (Prepare, Submit, Poll, Finalize);
         use type Dispatch.State;
         Control : Dispatch.Controller;
         OK, Found, Any_Found : Boolean;
         Before : Natural;
         Next : Natural;
         Cursor : Positive;
      begin
         P.Extend (Ledger, Unsigned_64 (To_Integer (Metadata'Address)), 8192, OK);
         pragma Assert (OK);
         -- Interleaved parent/child references span multiple bounded sweeps.
         for I in 1 .. 150 loop
            Authority.Install (Ledger, 42, 1, I, Unsigned_64 ((I - 1) mod 3 + 1),
                               Unsigned_64 ((I - 1) / 3) * 4096, OK);
            pragma Assert (OK);
         end loop;
         Dispatch.Start (Control, Ledger, 42, 1, OK, Last_Ticket => 1);
         pragma Assert (OK);
         while Dispatch.Status (Control) = Dispatch.Running loop
            Turns := Turns + 1; pragma Assert (Turns < 100);
            Before := Calls;
            Dispatch.Step (Control, Ledger);
            pragma Assert (Calls - Before <= 1);
         end loop;
         pragma Assert ((Dispatch.Status (Control) = Dispatch.Done) = (Fault = 0));
         for Ticket in 1 .. 3 loop
            Cursor := 1; Any_Found := False;
            loop
               P.Scan_Ticket (Ledger, 42, Unsigned_64 (Ticket), Cursor, Found, Next, OK);
               pragma Assert (OK);
               Any_Found := Any_Found or Found;
               exit when Next = 0;
               Cursor := Next;
            end loop;
            pragma Assert (Any_Found = not Ack (Ticket));
         end loop;
         Dispatch.Reopen (Control, Ledger, OK);
         pragma Assert (OK = (Fault = 0));
         if Fault = 1 then pragma Assert (Submitted = 2 and not Ack (1)); end if;
         Before := Calls;
         Dispatch.Step (Control, Ledger);
         pragma Assert (Calls = Before);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("parent-last retirement PASS5: 150 interleaved records; exact parent gate; partial failures retain parent; bounded steps");
end Table_Parent_Retirement_Tests;
