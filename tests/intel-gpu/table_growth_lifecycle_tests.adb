with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_Table_Provenance;
with Intel_GPU_Table_Provenance.IO;
with Intel_GPU_Table_Provenance.Retirement;
with Intel_GPU_Table_Provenance.Retirement.Dispatcher;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Growth;
with Intel_GPU_VM_Image.Growth.Backing;
with Intel_GPU_VM_Image.Growth.Backing.Writer;
with Intel_GPU_VM_Image.Insertion;
procedure Table_Growth_Lifecycle_Tests is
   package P renames Intel_GPU_Table_Provenance;
   package VM is new Intel_GPU_VM_Image (8);
   package G is new VM.Growth;
   type Words is array (Table_Index) of Unsigned_64;
   type Pages is array (1 .. 67) of Words;
   RAM : Pages := [others => [others => 0]] with Alignment => 4096, Volatile;
   type Bytes is array (1 .. 4096) of Unsigned_8;
   Metadata : aliased Bytes := [others => 0] with Alignment => 4096;
begin
   for Fault in 0 .. 10 loop
      declare
         Ledger : P.Ledger;
         Source : VM.Image;
         Live, Held : Boolean := True;
         Invalidated, Consumers_Gone : Boolean := False;
         Stores, Releases, Finalized : Natural := 0;
         Polls : Natural := 0;
         Growth_Succeeds : constant Boolean := Fault = 0 or Fault >= 5;
         Leaf_Succeeds : Boolean := False;
         Receipts : array (1 .. 2) of Boolean := [others => False];
         function Exclusive return Boolean is (Live and Held);
         procedure Resolve (Session, Ticket, Offset : Unsigned_64;
                            CPU, DMA : out Unsigned_64; OK : out Boolean) is
            Index : Natural;
         begin
            CPU := 0; DMA := 0; OK := False;
            if not Live or Session /= 42 or Ticket not in 1 .. 2 or Offset mod 4096 /= 0
              or Offset / 4096 >= (if Ticket = 1 then 64 else 3) then return; end if;
            Index := (if Ticket = 1 then 1 else 65) + Natural (Offset / 4096);
            CPU := Unsigned_64 (To_Integer (RAM (Index)'Address));
            DMA := Unsigned_64 (Index) * 4096; OK := True;
         end Resolve;
         package A is new P.Authority (Resolve);
         use type A.Append_Phase;
         Append : A.Append_State;
         function Flush_CPU (CPU : Unsigned_64) return Boolean is
           (Exclusive and CPU >= Unsigned_64 (To_Integer (RAM'Address)) and
            CPU < Unsigned_64 (To_Integer (RAM'Address)) + 67 * 4096);
         -- Real volatile reads/writes; flush completion is modeled, not CLFLUSH
         -- or Intel device visibility. No GPU is present in this fixture.
         package IO is new P.IO (A, Exclusive, Flush_CPU);
         function Owned (DMA : Unsigned_64) return Boolean is
         begin
            return DMA >= 4096 and then DMA <= 67 * 4096 and then DMA mod 4096 = 0 and then
              A.Lookup (Ledger, 42, 1, Positive (DMA / 4096)).DMA = DMA;
         end Owned;
         procedure Read_Word (DMA : Unsigned_64; Index : Table_Index;
                              Value : out Unsigned_64; OK : out Boolean) is
         begin
            IO.Read_Word (Ledger, 42, (if Fault = 4 then 2 else 1),
                          Positive (DMA / 4096), DMA, Index, Value, OK);
         end Read_Word;
         procedure Write_Word (DMA : Unsigned_64; Index : Table_Index;
                               Value : Unsigned_64; OK : out Boolean) is
         begin
            IO.Write_Word (Ledger, 42, (if Fault = 4 then 2 else 1),
                           Positive (DMA / 4096), DMA, Index, Value, OK);
            if OK then
               Stores := Stores + 1;
               if Fault = 2 then Live := False; end if;
            end if;
         end Write_Word;
         function Flush (DMA : Unsigned_64) return Boolean is
           (IO.Flush (Ledger, 42, 1, Positive (DMA / 4096), DMA));
         function Visible return Boolean is (Invalidated);
         package B is new G.Backing (Owned);
         package W is new B.Writer (Exclusive, Read_Word, Write_Word, Flush, Visible);
         Growth : W.State;
         procedure Write_Bound_Leaf
           (Table_DMA : Unsigned_64; Index : Table_Index;
            Expected, Replacement : Unsigned_64; Success : out Boolean) is
            Readback : Unsigned_64;
         begin
            pragma Assert (W.Committed (Growth) and Invalidated);
            pragma Assert (Table_DMA = 67 * 4096 and Index = 0);
            pragma Assert (VM.Lookup (Source, 2 ** 39) = 0);
            Read_Word (Table_DMA, Index, Readback, Success);
            if not Success or else Readback /= Expected then Success := False; return; end if;
            Write_Word (Table_DMA, Index, Replacement, Success);
            if not Success then return; end if;
            Success := Flush (Table_DMA) and Fault /= 9;
            if not Success then return; end if;
            Read_Word (Table_DMA, Index, Readback, Success);
            Success := Success and Readback = Replacement;
         end Write_Bound_Leaf;
         procedure Invalidate_Leaf (Success : out Boolean) is
         begin Success := Exclusive and Fault /= 10; end;
         package Insertion is new VM.Insertion (Exclusive, Write_Bound_Leaf, Invalidate_Leaf);
         Leaf : Insertion.Controller;
         function Context_Gone (Session : Unsigned_64) return Boolean is
           (Session = 42 and Consumers_Gone and Held);
         function May_Release (Session, Ticket : Unsigned_64) return Boolean is
           (Context_Gone (Session) and Ticket in 1 .. 2);
         function Confirmed (Session, Ticket : Unsigned_64) return Boolean is
           (May_Release (Session, Ticket) and then Receipts (Natural (Ticket)));
         package R is new P.Retirement (Context_Gone, May_Release, Confirmed);
         procedure Submit (Session, Ticket : Unsigned_64; OK : out Boolean) is
         begin
            pragma Assert (May_Release (Session, Ticket));
            Releases := Releases + 1;
            pragma Assert (Ticket = (if Releases = 1 then 2 else 1));
            OK := True;
         end Submit;
         procedure Poll (Session, Ticket : Unsigned_64; Complete, Failed : out Boolean) is
         begin
            Polls := Polls + 1;
            Failed := not May_Release (Session, Ticket) or else
              (Fault = 5 and Ticket = 2) or else (Fault = 6 and Ticket = 1);
            Complete := not Failed and then (Fault /= 8 or else Polls mod 3 = 0);
            Receipts (Natural (Ticket)) := Complete;
         end Poll;
         procedure Finalize (Session, Ticket : Unsigned_64; OK : out Boolean) is
            Found : Boolean;
            Cursor : Natural := 1;
         begin
            pragma Assert (Confirmed (Session, Ticket));
            loop
               P.Scan_Ticket (Ledger, Session, Ticket, Cursor, Found, Cursor, OK);
               pragma Assert (OK and not Found);
               exit when Cursor = 0;
            end loop;
            Finalized := Finalized + 1;
            if Fault = 7 then OK := False; end if;
         end Finalize;
         procedure Prepare (Session, Ticket : Unsigned_64; Complete, Failed : out Boolean) is
         begin
            Complete := May_Release (Session, Ticket); Failed := not Complete;
         end Prepare;
         package D is new R.Dispatcher (Prepare, Submit, Poll, Finalize);
         Retirement : D.Controller;
         use type D.State;
         Backing : VM.Backing_Pages := [1 => 4096, 2 => 8192, 3 => 12288, 4 => 16384, others => 0];
         OK : Boolean;
         Before : Unsigned_64;
         Value : Unsigned_64;
      begin
         RAM := [others => [others => 0]];
         P.Extend (Ledger, Unsigned_64 (To_Integer (Metadata'Address)), 4096, OK);
         pragma Assert (OK);
         for I in 1 .. 64 loop
            A.Install (Ledger, 42, 1, I, 1, Unsigned_64 (I - 1) * 4096, OK);
            pragma Assert (OK);
         end loop;
         VM.Initialize (Source, Backing, OK, Backing_Count => 4); pragma Assert (OK);
         VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK); pragma Assert (OK);
         VM.Seal (Source, OK); pragma Assert (OK);
         for I in 1 .. 4 loop
            for Word in Table_Index loop RAM (I) (Word) := VM.Entry_Value (Source, I, Word); end loop;
         end loop;
         RAM (64) := RAM (1); -- retained hardware root differs from logical image root
         Before := VM.Revision (Source);
         A.Begin_Append (Append, Ledger, 42, 1, 2, 0, 3, OK); pragma Assert (OK);
         for I in 1 .. 3 loop
            if Fault = 3 and I = 2 then Live := False; end if;
            A.Step (Append, Ledger);
         end loop;
         if Fault = 3 then
            pragma Assert (A.Status (Append) = A.Rejected and P.Count (Ledger) = 65);
            Live := True;
            W.Start (Growth, Source, 2 ** 39, 4096, 64 * 4096,
                     [65 * 4096, 66 * 4096, 67 * 4096], OK);
            pragma Assert (not OK and Stores = 0 and VM.Revision (Source) = Before);
            pragma Assert (A.First_ID (Append) = 0); -- incomplete group is not publishable
         else
            pragma Assert (A.Status (Append) = A.Appended and A.First_ID (Append) = 65);
            W.Start (Growth, Source, 2 ** 39, 4096, 64 * 4096,
                     [65 * 4096, 66 * 4096, 67 * 4096], OK); pragma Assert (OK);
            for Turn in 1 .. 4000 loop
               exit when not W.Pending (Growth);
               W.Step (Growth, Source);
            end loop;
            pragma Assert (not W.Pending (Growth));
            Invalidated := Growth_Succeeds;
            W.Commit (Growth, Source, OK);
            pragma Assert (OK = Growth_Succeeds);
            if Growth_Succeeds then
               pragma Assert (W.Committed (Growth) and VM.Used (Source) = 7);
               for I in 5 .. 7 loop
                  pragma Assert (VM.Page_DMA (Source, I) = A.Lookup (Ledger, 42, 1, 60 + I).DMA);
               end loop;
               pragma Assert (RAM (64) (1) = VM.Entry_Value (Source, 1, 1));
               pragma Assert (VM.Revision (Source) = Before + 1);
               Insertion.Start (Leaf, Source, VM.Revision (Source), 2 ** 39,
                 [1 => 16#110000#], Write_Back, Read_Write, OK);
               pragma Assert (OK and Insertion.Publishing (Leaf));
               Insertion.Step (Leaf, Source);
               pragma Assert (not Insertion.Publishing (Leaf));
               pragma Assert (RAM (67) (0) = Encode_Leaf (16#110000#, Write_Back, Read_Write));
               pragma Assert (VM.Lookup (Source, 2 ** 39) = 0);
               if Insertion.Published (Leaf) then
                  Invalidate_Leaf (OK);
                  Insertion.Commit (Leaf, Source, OK, Leaf_Succeeds);
               end if;
               pragma Assert (Leaf_Succeeds = (Fault not in 9 .. 10));
               pragma Assert (VM.Revision (Source) = Before + (if Leaf_Succeeds then 2 else 1));
               if Leaf_Succeeds then
                  pragma Assert (VM.Lookup (Source, 2 ** 39) = RAM (67) (0));
                  pragma Assert (VM.Lookup (Source, 4096) = Encode_Leaf (16#100000#, Write_Back, Read_Write));
               else
                  pragma Assert (Insertion.Failed (Leaf) and VM.Lookup (Source, 2 ** 39) = 0);
                  declare Written : constant Natural := Stores; begin
                     Insertion.Step (Leaf, Source);
                     Insertion.Commit (Leaf, Source, True, OK);
                     pragma Assert (not OK and Stores = Written and P.Count (Ledger) = 67);
                  end;
               end if;
            else
               pragma Assert (not W.Committed (Growth) and VM.Revision (Source) = Before);
               pragma Assert (VM.Used (Source) = 4 and P.Count (Ledger) = 67);
               pragma Assert (Stores = (if Fault = 1 then 1539 elsif Fault = 2 then 1 else 0));
            end if;
         end if;
         if Growth_Succeeds and Leaf_Succeeds then
            -- Explicit modeled proof that all hardware/CPU consumers retired.
            Consumers_Gone := True;
            D.Start (Retirement, Ledger, 42, 1, OK, Last_Ticket => 1); pragma Assert (OK);
            for Turn in 1 .. 100 loop
               D.Step (Retirement, Ledger);
               exit when D.Status (Retirement) /= D.Running;
            end loop;
            if Fault in 5 .. 7 then
               pragma Assert (D.Status (Retirement) = D.Failed);
               pragma Assert (Releases = (if Fault = 6 then 2 else 1));
               pragma Assert (Finalized = (if Fault = 5 then 0 else 1));
               pragma Assert (not Receipts (1)); -- never acknowledged parent
               declare
                  Found : Boolean;
                  Cursor : Natural;
               begin
                  P.Scan_Ticket (Ledger, 42, 1, 1, Found, Cursor, OK);
                  pragma Assert (OK and Found); -- retain parent allocation refs
                  P.Scan_Ticket (Ledger, 42, 2, 65, Found, Cursor, OK);
                  pragma Assert (OK and (Found = (Fault = 5)));
               end;
               -- Failure is terminal: no replay, no generation reuse, and no
               -- parent release after child finalization becomes uncertain.
               for Turn in 1 .. 10 loop D.Step (Retirement, Ledger); end loop;
               pragma Assert (Releases = (if Fault = 6 then 2 else 1));
               D.Reopen (Retirement, Ledger, OK);
               pragma Assert (not OK and P.Generation (Ledger) = 1);
            else
               pragma Assert (D.Status (Retirement) = D.Done and Releases = 2 and Finalized = 2);
               pragma Assert (Polls = (if Fault = 8 then 6 else 2));
               D.Reopen (Retirement, Ledger, OK); pragma Assert (OK and P.Generation (Ledger) = 2);
            end if;
            IO.Read_Word (Ledger, 42, 1, 65, 65 * 4096, 0, Value, OK);
            pragma Assert (not OK and Value = 0); -- stale generation cannot dereference RAM
         else
            Live := True;
            D.Start (Retirement, Ledger, 42, 1, OK, Last_Ticket => 1);
            pragma Assert (not OK and Releases = 0 and Finalized = 0);
            pragma Assert (A.Lookup (Ledger, 42, 1, 65).Ticket = 2);
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Table growth lifecycle PASS11: append + real provenance IO + directory/leaf publication and distinct visibility gates + parent-last retirement; partial publication/receipt failures retain allocations (modeled GPU/TLB/receipts)");
end Table_Growth_Lifecycle_Tests;
