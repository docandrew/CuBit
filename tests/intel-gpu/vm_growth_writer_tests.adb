with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Growth;
with Intel_GPU_VM_Image.Growth.Backing;
with Intel_GPU_VM_Image.Growth.Backing.Writer;
with Intel_GPU_VM_Image.Insertion;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_PPGTT_Scratch;
with Intel_GPU_Table_Provenance;
with Intel_GPU_Table_Provenance.IO;
with System.Storage_Elements; use System.Storage_Elements;
procedure VM_Growth_Writer_Tests is
   package VM is new Intel_GPU_VM_Image (8);
   package G is new VM.Growth;
   type Fault_List is array (Positive range <>) of Natural;
   Faults : constant Fault_List := [0, 1, 512, 513, 514, 1025, 1026,
                                    3075, 3076, 3077, 3078, 3079, 3080, 3087];
begin
   for Lose_Owner in Boolean loop
   for Fault of Faults loop
      declare
         Source : VM.Image;
         DMA : VM.Backing_Pages;
         Scratch : Intel_GPU_PPGTT_Scratch.Backing_Pages :=
           [16#20000#, 16#21000#, 16#22000#, 16#23000#];
         type Words is array (Table_Index) of Unsigned_64;
         RAM : array (1 .. 13) of Words := [others => [others => 16#DEAD#]]
           with Alignment => 4096;
         Calls : Natural := 0;
         Held : Boolean := True;
         Revoked : Boolean := False;
         Probe_Ownership : Boolean := False;
         Ownership_Calls, Lose_On_Call : Natural := 0;
         function Owner return Boolean is (Held);
         Invalidated : Boolean := True;
         function TLB_Ready return Boolean is (Invalidated);
         function Owned (Address : Unsigned_64) return Boolean is
         begin
            if Probe_Ownership then
               Ownership_Calls := Ownership_Calls + 1;
               if Ownership_Calls = Lose_On_Call then Held := False; end if;
            end if;
            return not Revoked and then Address in 4096 .. 13 * 4096
              and then Address mod 4096 = 0;
         end Owned;
         procedure Resolve_Page (Session, Ticket, Offset : Unsigned_64;
                                 CPU, DMA : out Unsigned_64; Accepted : out Boolean) is
         begin
            CPU := 0; DMA := 0;
            Accepted := not Revoked and Session = 42 and Ticket in 1 .. 13 and Offset = 0;
            if Accepted then
               CPU := Unsigned_64 (To_Integer (RAM (Natural (Ticket))'Address));
               DMA := Ticket * 4096;
            end if;
         end Resolve_Page;
         package Provenance renames Intel_GPU_Table_Provenance;
         package A is new Provenance.Authority (Resolve_Page);
         Ledger : Provenance.Ledger;
         function Visibility (CPU : Unsigned_64) return Boolean is
           (CPU mod 4096 = 0); -- host test, not real GPU cache flush
         package Accesses is new Provenance.IO (A, Owner, Visibility);
         procedure Result (OK : out Boolean) is
         begin
            Calls := Calls + 1;
            OK := Lose_Owner or else Calls /= Fault;
            if Lose_Owner and Calls = Fault then Held := False; end if;
         end Result;
         procedure Read_Word (Address : Unsigned_64; Index : Table_Index;
                              Value : out Unsigned_64; OK : out Boolean) is
         begin
            pragma Assert (Held and Owned (Address));
            Accesses.Read_Word (Ledger, 42, 1, Positive (Address / 4096), Address, Index, Value, OK);
            pragma Assert (OK); Result (OK);
         end Read_Word;
         procedure Write_Word (Address : Unsigned_64; Index : Table_Index;
                               Value : Unsigned_64; OK : out Boolean) is
         begin
            pragma Assert (Held and Owned (Address));
            if Address = 9 * 4096 then
               pragma Assert (Calls >= 3076); -- all three tables verified
            end if;
            Accesses.Write_Word (Ledger, 42, 1, Positive (Address / 4096), Address, Index, Value, OK);
            pragma Assert (OK);
            Result (OK); -- failure may occur AFTER the write
         end Write_Word;
         function Flush (Address : Unsigned_64) return Boolean is
            OK : Boolean;
         begin
            pragma Assert (Held and Owned (Address));
            pragma Assert (Accesses.Flush (Ledger, 42, 1, Positive (Address / 4096), Address));
            Result (OK); return OK;
         end Flush;
         package B is new G.Backing (Owned);
         package W is new B.Writer (Owner, Read_Word, Write_Word, Flush, TLB_Ready);
         procedure Publish (Object : in out W.State; Source : VM.Image;
           GPU, Bytes, Root : Unsigned_64; Pages : VM.Data_Pages; OK : out Boolean) is
            Before : Natural;
         begin
            Before := Calls;
            W.Start (Object, Source, GPU, Bytes, Root, Pages, OK);
            pragma Assert (Calls = Before);
            if not OK then return; end if;
            while W.Pending (Object) loop
               Before := Calls;
               W.Step (Object, Source);
               pragma Assert (Calls - Before <= 1);
            end loop;
            OK := W.Published (Object);
         end Publish;
         procedure Insert_Leaf
           (Table_DMA : Unsigned_64; Index : Table_Index;
            Expected, Replacement : Unsigned_64; Success : out Boolean)
         is
            Value : Unsigned_64;
         begin
            Accesses.Read_Word (Ledger, 42, 1, Positive (Table_DMA / 4096),
              Table_DMA, Index, Value, Success);
            Success := Success and then Value = Expected;
            if Success then
               Accesses.Write_Word (Ledger, 42, 1, Positive (Table_DMA / 4096),
                 Table_DMA, Index, Replacement, Success);
            end if;
         end Insert_Leaf;
         procedure Invalidate (Success : out Boolean) is
         begin Success := Held and Invalidated; end Invalidate;
         package Insertions is new VM.Insertion (Owner, Insert_Leaf, Invalidate);
         Insertion_State : Insertions.Controller;
         State : W.State;
         New_Pages : VM.Data_Pages := [10 * 4096, 11 * 4096, 12 * 4096];
         OK : Boolean;
         Before : Natural;
         Rejected_Value : Unsigned_64;
      begin
         for I in 1 .. 13 loop
            A.Install (Ledger, 42, 1, I, Unsigned_64 (I), 0, OK);
            pragma Assert (OK);
         end loop;
         Accesses.Write_Word (Ledger, 43, 1, 10, 10 * 4096, 0, 999, OK);
         pragma Assert (not OK);
         Accesses.Write_Word (Ledger, 42, 1, 10, 11 * 4096, 0, 999, OK);
         pragma Assert (not OK);
         Revoked := True;
         Accesses.Write_Word (Ledger, 42, 1, 10, 10 * 4096, 0, 999, OK);
         pragma Assert (not OK);
         Accesses.Read_Word (Ledger, 42, 1, 10, 10 * 4096, 0, Rejected_Value, OK);
         pragma Assert (not OK and Rejected_Value = 0);
         pragma Assert (not Accesses.Flush (Ledger, 42, 1, 10, 10 * 4096));
         Revoked := False; Held := False;
         Accesses.Write_Word (Ledger, 42, 1, 10, 10 * 4096, 0, 999, OK);
         pragma Assert (not OK and RAM (10) (0) = 16#DEAD#);
         Held := True;
         for P in DMA'Range loop DMA (P) := Unsigned_64 (P) * 4096; end loop;
         VM.Initialize (Source, DMA, OK, Scratch); pragma Assert (OK);
         VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK); pragma Assert (OK);
         VM.Seal (Source, OK); pragma Assert (OK);
         for P in 1 .. VM.Used (Source) loop
            for I in Table_Index loop RAM (P) (I) := VM.Entry_Value (Source, P, I); end loop;
         end loop;
         RAM (9) := RAM (1);
         Publish (State, Source, 2 ** 39, 4096, 9 * 4096, New_Pages, OK);
         pragma Assert (OK = (Fault = 0) and W.Published (State) = OK);
         pragma Assert (W.Attempted (State) and Calls = (if Fault = 0 then 3087 else Fault));
         pragma Assert (RAM (1) (1) = VM.Entry_Value (Source, 1, 1)); -- historical root untouched
         pragma Assert (VM.Used (Source) = 4 and VM.Lookup (Source, 2 ** 39) = 0);
         if OK then
            pragma Assert (RAM (9) (1) = Encode_Directory (10 * 4096));
            pragma Assert (RAM (10) (0) = Encode_Directory (11 * 4096));
            pragma Assert (RAM (11) (0) = Encode_Directory (12 * 4096));
            pragma Assert (for all I in Table_Index => RAM (12) (I) =
              Intel_GPU_PPGTT_Scratch.Fallback (Scratch, 0));
         end if;
         Before := Calls; Held := True;
         Publish (State, Source, 2 ** 39, 4096, 9 * 4096, New_Pages, OK);
         pragma Assert (not OK and Calls = Before);
         -- Both successful and failed publication are consumed once. In one
         -- successful case deliberately deny TLB evidence: no metadata change.
         Invalidated := not Lose_Owner;
         W.Commit (State, Source, OK);
         pragma Assert (OK = (Fault = 0 and not Lose_Owner));
         pragma Assert (W.Committed (State) = OK and Calls = Before);
         if OK then
            pragma Assert (VM.Used (Source) = 7 and VM.Lookup (Source, 4096) =
              Encode_Leaf (16#100000#, Write_Back, Read_Write));
            pragma Assert (VM.Lookup (Source, 2 ** 39) = 0);
            for P in 5 .. 7 loop
               for I in Table_Index loop
                  pragma Assert (VM.Entry_Value (Source, P, I) = RAM (P + 5) (I));
               end loop;
            end loop;
            pragma Assert (VM.Entry_Value (Source, 1, 1) = RAM (9) (1));
         else pragma Assert (VM.Used (Source) = 4); end if;
         Invalidated := True;
         W.Commit (State, Source, OK); pragma Assert (not OK);
         Before := Calls;
         if W.Committed (State) then
            W.Rearm (State, Source, 4096, OK);
            pragma Assert (not OK and W.Committed (State));
            Held := False;
            W.Rearm (State, Source, 9 * 4096, OK);
            pragma Assert (not OK and W.Committed (State));
            Held := True; Revoked := True;
            W.Rearm (State, Source, 9 * 4096, OK);
            pragma Assert (not OK and W.Committed (State));
            Revoked := False;
            -- A later, committed leaf update must not strand the growth
            -- receipt merely because the source epoch has advanced again.
            Insertions.Execute (Insertion_State, Source, VM.Revision (Source),
              2 ** 39, [16#200000#], Write_Back, Read_Write, OK);
            pragma Assert (OK and not Insertions.Failed (Insertion_State));
            pragma Assert (VM.Lookup (Source, 2 ** 39) =
              Encode_Leaf (16#200000#, Write_Back, Read_Write));
            declare
               Replacement : VM.Image;
               Other_DMA : VM.Backing_Pages;
            begin
               for P in Other_DMA'Range loop
                  Other_DMA (P) := Unsigned_64 (P + 13) * 4096;
               end loop;
               VM.Prepare_Update (Replacement, Source, Other_DMA, OK);
               pragma Assert (OK);
               VM.Seal_Update (Replacement, OK); pragma Assert (OK);
               W.Rearm (State, Replacement, 9 * 4096, OK);
               pragma Assert (not OK and W.Committed (State));
            end;
            -- Ownership may disappear inside a provenance callback. Every
            -- such loss must preserve the consumed receipt, never rearm it.
            Probe_Ownership := True;
            for N in 1 .. 4 loop
               Ownership_Calls := 0; Lose_On_Call := N; Held := True;
               W.Rearm (State, Source, 9 * 4096, OK);
               pragma Assert (not OK and W.Committed (State));
            end loop;
            Probe_Ownership := False; Held := True;
         end if;
         Rejected_Value := VM.Revision (Source);
         W.Rearm (State, Source, 9 * 4096, OK);
         pragma Assert (OK = (Fault = 0 and not Lose_Owner));
         pragma Assert (Calls = Before and VM.Revision (Source) = Rejected_Value);
         if OK then
            pragma Assert (not W.Attempted (State) and not W.Published (State)
              and not W.Committed (State));
            -- A second distinct growth uses the SAME receipt; prior pages
            -- and mappings remain owned, rather than being released/retried.
            Publish (State, Source, 2 ** 39 + 2 ** 21, 4096,
              9 * 4096, [13 * 4096], OK);
            pragma Assert (OK);
            W.Commit (State, Source, OK);
            pragma Assert (OK and W.Committed (State));
            pragma Assert (VM.Used (Source) = 8 and
              VM.Revision (Source) = Rejected_Value + 1);
            pragma Assert (VM.Lookup (Source, 4096) =
              Encode_Leaf (16#100000#, Write_Back, Read_Write));
            pragma Assert (VM.Lookup (Source, 2 ** 39) =
              Encode_Leaf (16#200000#, Write_Back, Read_Write));
         else
            pragma Assert (W.Attempted (State));
         end if;
      end;
   end loop;
   end loop;
   -- A callback may interrupt the transaction with an early Commit. Even
   -- though no adoption is possible, the outer step must not revive it.
   for Interrupt_At of Fault_List'[1, 3087] loop
      declare
         Source : VM.Image;
         State : VM.Growth_Receipt;
         type Words is array (Table_Index) of Unsigned_64;
         RAM : array (1 .. 8) of Words := [others => [others => 0]];
         Calls : Natural := 0;
         OK : Boolean;
         function Owner return Boolean is (True);
         function Owned (DMA : Unsigned_64) return Boolean is
           (DMA in 4096 .. 8 * 4096 and then DMA mod 4096 = 0);
         procedure Interrupt;
         procedure Read_Word (DMA : Unsigned_64; Index : Table_Index;
                              Value : out Unsigned_64; Accepted : out Boolean) is
         begin
            Value := RAM (Natural (DMA / 4096)) (Index);
            Calls := Calls + 1; Interrupt; Accepted := True;
         end;
         procedure Write_Word (DMA : Unsigned_64; Index : Table_Index;
                               Value : Unsigned_64; Accepted : out Boolean) is
         begin
            RAM (Natural (DMA / 4096)) (Index) := Value;
            Calls := Calls + 1; Interrupt; Accepted := True;
         end;
         function Flush (DMA : Unsigned_64) return Boolean is
         begin Calls := Calls + 1; Interrupt; return Owned (DMA); end;
         package B is new G.Backing (Owned);
         package W is new B.Writer (Owner, Read_Word, Write_Word, Flush, Owner);
         procedure Interrupt is
            Accepted : Boolean;
         begin
            if Calls = Interrupt_At then
               W.Commit (State, Source, Accepted);
               pragma Assert (not Accepted);
            end if;
         end;
         Before : Unsigned_64;
      begin
         VM.Initialize (Source, [4096, 8192, 12288, 16384, others => 0], OK,
                        Backing_Count => 4);
         pragma Assert (OK);
         VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK);
         pragma Assert (OK);
         VM.Seal (Source, OK); pragma Assert (OK);
         Before := VM.Revision (Source);
         for P in 1 .. 4 loop
            for I in Table_Index loop RAM (P) (I) := VM.Entry_Value (Source, P, I); end loop;
         end loop;
         W.Start (State, Source, 2 ** 39, 4096, 4096,
                  [5 * 4096, 6 * 4096, 7 * 4096], OK);
         pragma Assert (OK);
         while W.Pending (State) loop W.Step (State, Source); end loop;
         pragma Assert (Calls = Interrupt_At and not W.Published (State));
         W.Step (State, Source);
         W.Commit (State, Source, OK);
         pragma Assert (not OK and not W.Committed (State) and Calls = Interrupt_At);
         pragma Assert (VM.Revision (Source) = Before and VM.Used (Source) = 4);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Growth callback interruption PASS2: premature commit cannot revive publication");
   Ada.Text_IO.Put_Line ("Growth writer PASS28: host RAM publication and gated mirror commit; failure/owner-loss/TLB rejection and no replay (no GPU validation)");
end VM_Growth_Writer_Tests;
