with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Table_Provenance;
with Intel_GPU_Table_Provenance.Retirement;
procedure Table_Append_Tests is
   package P renames Intel_GPU_Table_Provenance;
   type Bytes is array (1 .. 8192) of Unsigned_8;
   Metadata : aliased Bytes := [others => 0] with Alignment => 4096;
   Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Metadata'Address));
begin
   for Fault in 0 .. 5 loop
      declare
         Ledger, Other : P.Ledger;
         Calls : Natural := 0;
         Revoked, Bad_Page : Boolean := False;
         procedure Resolve (Session, Ticket, Offset : Unsigned_64;
                            CPU, DMA : out Unsigned_64; Accepted : out Boolean) is
         begin
            Calls := Calls + 1;
            Accepted := not Revoked and Session = 42 and Ticket in 1 .. 3;
            CPU := 16#10000000# + Ticket * 16#100000# + Offset;
            DMA := Ticket * 16#100000# + Offset;
            if Bad_Page then DMA := 3; end if;
         end Resolve;
         package A is new P.Authority (Resolve);
         use type A.Append_Phase;
         use type P.Mapping;
         Operation, Invalid : A.Append_State;
         OK : Boolean;
         Saved : P.Mapping;
      begin
         P.Extend (Ledger, Base, 4096, OK); pragma Assert (OK);
         for I in 1 .. 64 loop
            A.Install (Ledger, 42, 1, I, 1, Unsigned_64 (I - 1) * 4096, OK);
            pragma Assert (OK);
         end loop;
         Saved := A.Lookup (Ledger, 42, 1, 1); Calls := 0;
         A.Begin_Append (Operation, Ledger, 42, 1, 2, 0, 3, OK);
         pragma Assert (OK and Calls = 0 and A.First_ID (Operation) = 0);
         A.Begin_Append (Operation, Ledger, 42, 1, 3, 0, 1, OK);
         pragma Assert (not OK and A.Status (Operation) = A.Appending);
         A.Step (Operation, Ledger);
         pragma Assert (Calls = 1 and P.Count (Ledger) = 65 and A.Installed (Operation) = 1);
         A.Rearm (Operation, Ledger, OK);
         pragma Assert (not OK and A.Status (Operation) = A.Appending);
         case Fault is
            when 1 => Revoked := True;
            when 2 => Calls := 0; A.Step (Operation, Other); pragma Assert (Calls = 0);
            when 3 => A.Install (Ledger, 42, 1, 66, 3, 0, OK); pragma Assert (OK);
            when 4 => P.Extend (Ledger, Base, 8192, OK); pragma Assert (OK);
            when 5 => Bad_Page := True;
            when others => null;
         end case;
         for Turn in 1 .. 4 loop
            Calls := 0; A.Step (Operation, Ledger); pragma Assert (Calls <= 1);
            exit when A.Status (Operation) /= A.Appending;
         end loop;
         if Fault in 0 | 4 then
            pragma Assert (A.Status (Operation) = A.Appended and A.First_ID (Operation) = 65);
            pragma Assert (A.Installed (Operation) = 3 and P.Count (Ledger) = 67);
            for I in 65 .. 67 loop
               pragma Assert (A.Lookup (Ledger, 42, 1, I).Ticket = 2);
               pragma Assert (A.Lookup (Ledger, 42, 1, I).Offset = Unsigned_64 (I - 65) * 4096);
            end loop;
         else
            pragma Assert (A.Status (Operation) = A.Rejected and A.First_ID (Operation) = 0);
            pragma Assert (A.Installed (Operation) = 1);
            pragma Assert (P.Count (Ledger) = (if Fault = 3 then 66 else 65));
         end if;
         Calls := 0; A.Step (Operation, Ledger); pragma Assert (Calls = 0);
         Revoked := False; Bad_Page := False;
         pragma Assert (A.Lookup (Ledger, 42, 1, 1) = Saved);
         pragma Assert (A.Lookup (Ledger, 42, 1, 65).Ticket = 2);
         pragma Assert (P.Generation (Ledger) = 1 and P.Count (Other) = 0);
         Calls := 0;
         A.Rearm (Operation, Other, OK); pragma Assert (not OK and Calls = 0);
         A.Rearm (Operation, Ledger, OK);
         pragma Assert (OK = (Fault in 0 | 4));
         pragma Assert (Calls = 0); -- reuse never resolves/frees backing
         if OK then
            pragma Assert (A.Status (Operation) = A.Unused and A.First_ID (Operation) = 0);
            for Cycle in 1 .. 10 loop
               A.Begin_Append (Operation, Ledger, 42, 1, 3,
                               Unsigned_64 (Cycle - 1) * 4096, 1, OK);
               pragma Assert (OK);
               A.Step (Operation, Ledger);
               pragma Assert (A.Status (Operation) = A.Appended and
                              A.First_ID (Operation) = 67 + Cycle);
               pragma Assert (P.Count (Ledger) = 67 + Cycle);
               pragma Assert (A.Lookup (Ledger, 42, 1, 65).Ticket = 2);
               pragma Assert (A.Lookup (Ledger, 42, 1, 1) = Saved);
               if Cycle < 10 then
                  A.Rearm (Operation, Ledger, OK); pragma Assert (OK);
               end if;
            end loop;
            -- A different append invalidates the captured completed count.
            A.Install (Ledger, 42, 1, 78, 3, 10 * 4096, OK); pragma Assert (OK);
            A.Rearm (Operation, Ledger, OK); pragma Assert (not OK);
         end if;
         Calls := 0;
         A.Begin_Append (Invalid, Ledger, 42, 1, 2, Unsigned_64'Last - 4095, 2, OK);
         pragma Assert (not OK and Calls = 0 and A.Status (Invalid) = A.Rejected);
         A.Begin_Append (Invalid, Ledger, 42, 1, 2, 0, 1, OK);
         pragma Assert (not OK); -- failed attempt is not replayable
      end;
   end loop;
   declare
      Ledger : P.Ledger;
      procedure Resolve (Session, Ticket, Offset : Unsigned_64;
                         CPU, DMA : out Unsigned_64; OK : out Boolean) is
      begin
         CPU := 16#10000000#; DMA := 16#200000#;
         OK := Session = 42 and Ticket = 1 and Offset = 0;
      end Resolve;
      package A is new P.Authority (Resolve);
      function Gone (Session : Unsigned_64) return Boolean is (Session = 42);
      function Released (Session, Ticket : Unsigned_64) return Boolean is
        (Session = 42 and Ticket = 1);
      package R is new P.Retirement (Gone, Released, Released);
      Operation : A.Append_State;
      OK : Boolean;
      Requested : Unsigned_64;
   begin
      A.Rearm (Operation, Ledger, OK); pragma Assert (not OK);
      A.Begin_Append (Operation, Ledger, 42, 1, 1, 0, 1, OK); pragma Assert (OK);
      A.Step (Operation, Ledger);
      R.Start (Ledger, 42, 1, OK); pragma Assert (OK);
      R.Step (Ledger);
      R.Take_Request (Ledger, 42, Requested, OK); pragma Assert (OK and Requested = 1);
      R.Acknowledge (Ledger, 42, 1, OK); pragma Assert (OK);
      R.Step (Ledger); R.Step (Ledger);
      A.Rearm (Operation, Ledger, OK); pragma Assert (not OK); -- closed generation
      R.Reopen (Ledger, 42, 1, OK); pragma Assert (OK and P.Generation (Ledger) = 2);
      A.Rearm (Operation, Ledger, OK); pragma Assert (not OK); -- stale operation
   end;
   Ada.Text_IO.Put_Line ("Table append rearm PASS: repeated append preserves IDs/backing; pending/failed/wrong-ledger/interleaved/retired/stale operations rejected");
   Ada.Text_IO.Put_Line ("Table append PASS6: IDs65..67, stable metadata growth, one resolution per step, partial retention, wrong ledger/interleaving/revocation, no replay");
end Table_Append_Tests;
