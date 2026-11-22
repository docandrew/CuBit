with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Table_Provenance;
with Intel_GPU_Table_Provenance.Retirement;
procedure Table_Retirement_Tests is
begin
   for Fault in 0 .. 5 loop
      declare
         Ready : Boolean := False;
         Acks : Natural := 0;
         Release_Checks : Natural := 0;
         procedure Resolve (Session, Ticket, Offset : Unsigned_64;
                            CPU, DMA : out Unsigned_64; Accepted : out Boolean) is
         begin
            Accepted := Session = 42 and Ticket in 1 .. 2;
            CPU := 16#10000000# + Offset; DMA := 16#100000# + Offset;
         end Resolve;
         function Released (Session : Unsigned_64) return Boolean is (Ready and Session = 42);
         function May_Free (Session, Ticket : Unsigned_64) return Boolean is
         begin
            Release_Checks := Release_Checks + 1;
            -- Owner disappears inside the request-time allocation check.
            if Fault = 4 and Release_Checks = 2 then Ready := False; end if;
            return Fault /= 1 and Session = 42 and Ticket in 1 .. 2;
         end May_Free;
         function Confirmed (Session, Ticket : Unsigned_64) return Boolean is
         begin
            -- A valid receipt alone cannot preserve a lost exclusion hold.
            if Fault = 5 then Ready := False; end if;
            return Fault /= 2 and Session = 42 and Ticket in 1 .. 2;
         end Confirmed;
         package P renames Intel_GPU_Table_Provenance;
         package A is new P.Authority (Resolve);
         package R is new P.Retirement (Released, May_Free, Confirmed);
         use type P.Retirement_Phase;
         Object : P.Ledger;
         type Bytes is array (1 .. 4096) of Unsigned_8;
         Metadata : Bytes := [others => 0] with Alignment => 4096;
         OK, Found : Boolean;
         Next : Natural;
         Ticket, Duplicate : Unsigned_64;
      begin
         P.Extend (Object, Unsigned_64 (To_Integer (Metadata'Address)), 4096, OK);
         pragma Assert (OK);
         for I in 1 .. 80 loop
            A.Install (Object, 42, 1, I, (if I mod 2 = 0 then 2 else 1),
                       Unsigned_64 (I) * 4096, OK); pragma Assert (OK);
         end loop;
         R.Reopen (Object, 42, 1, OK); pragma Assert (not OK);
         R.Start (Object, 42, 1, OK); pragma Assert (not OK and R.Phase (Object) = P.Open);
         Ready := True; R.Start (Object, 42, 1, OK); pragma Assert (OK);
         pragma Assert (A.Lookup (Object, 42, 1, 1).Ticket = 0);
         A.Install (Object, 42, 1, 81, 1, 0, OK); pragma Assert (not OK);
         for Tick in 1 .. 20 loop
            R.Step (Object);
            if R.Phase (Object) = P.Request_Ready then
               R.Take_Request (Object, 43, Duplicate, OK);
               pragma Assert (not OK and Duplicate = 0);
               R.Take_Request (Object, 42, Ticket, OK);
               if Fault = 4 then
                  pragma Assert (not OK and Ticket = 0 and R.Phase (Object) = P.Failed);
                  exit;
               end if;
               pragma Assert (OK and Ticket /= 0);
               R.Take_Request (Object, 42, Duplicate, OK);
               pragma Assert (not OK and Duplicate = 0);
            end if;
            if R.Phase (Object) = P.Awaiting_Ack then
               Ticket := R.Pending_Ticket (Object);
               R.Acknowledge (Object, 43, Ticket, OK); pragma Assert (not OK);
               R.Acknowledge (Object, 42, Ticket + 100, OK); pragma Assert (not OK);
               pragma Assert (R.Phase (Object) = P.Awaiting_Ack);
               P.Scan_Ticket (Object, 42, Ticket, 1, Found, Next, OK);
               pragma Assert (OK and Found); -- no early reference clearing
               R.Acknowledge (Object, 42, Ticket, OK);
               pragma Assert (OK = (Fault not in 2 | 5));
               if OK then Acks := Acks + 1; end if;
               if Fault = 3 then Ready := False; end if;
            end if;
            exit when R.Phase (Object) in P.Complete | P.Failed;
         end loop;
         if Fault = 0 then
            pragma Assert (R.Phase (Object) = P.Complete and Acks = 2);
            for T in 1 .. 2 loop
               Next := 1;
               loop
                  P.Scan_Ticket (Object, 42, Unsigned_64 (T), Next, Found, Next, OK);
                  pragma Assert (OK and not Found); exit when Next = 0;
               end loop;
            end loop;
         else
            pragma Assert (R.Phase (Object) = P.Failed);
            P.Scan_Ticket (Object, 42, 2, 1, Found, Next, OK);
            pragma Assert (OK and Found); -- failed group retains unacked ticket
         end if;
         R.Start (Object, 42, 1, OK); pragma Assert (not OK);
         R.Reopen (Object, 43, 1, OK); pragma Assert (not OK);
         R.Reopen (Object, 42, 2, OK); pragma Assert (not OK);
         R.Reopen (Object, 42, 1, OK); pragma Assert (OK = (Fault = 0));
         if Fault = 0 then
            pragma Assert (P.Generation (Object) = 2 and P.Count (Object) = 0
                           and P.Capacity (Object) >= 80);
            -- Even identical session, index and physical addresses cannot
            -- turn an old-generation reference into authority over a new one.
            A.Install (Object, 42, 1, 1, 1, 4096, OK); pragma Assert (not OK);
            A.Install (Object, 42, 2, 1, 1, 4096, OK); pragma Assert (OK);
            pragma Assert (A.Lookup (Object, 42, 1, 1).Ticket = 0);
            pragma Assert (A.Lookup (Object, 42, 2, 1).Ticket = 1);
            R.Start (Object, 42, 1, OK); pragma Assert (not OK);
            R.Reopen (Object, 42, 1, OK); pragma Assert (not OK);
         else
            pragma Assert (P.Generation (Object) = 1);
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Grouped retirement PASS: 6 scenarios, 80 records/2 tickets; exact acknowledgements, callback ownership loss, bounded sweeping and fail-retention");
end Table_Retirement_Tests;
