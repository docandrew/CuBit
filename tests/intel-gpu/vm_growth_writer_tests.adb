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
   use type G.Inspection_Status;
   use type G.Plan_Status;
   type Fault_List is array (Positive range <>) of Natural;
   Faults : constant Fault_List := [0, 1, 512, 513, 514, 1025, 1026,
                                    3075, 3076, 3077, 3078, 3079, 3080, 3087];
begin
   for Lose_Owner in Boolean loop
   for Fault of Faults loop
      declare
         Source : VM.Image;
         Active_Query, Finished_Query : G.Inspection;
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
            Work : W.Preparation;
            Input : VM.Data_Pages := Pages;
            Reads, Turns : Natural := 0;
            function Read_Page (Ordinal : Positive) return Unsigned_64 is
            begin
               Reads := Reads + 1;
               return Input (Input'First + Ordinal - 1);
            end Read_Page;
            procedure Prepare is new W.Prepare_Step (Read_Page);
         begin
            Before := Calls;
            W.Begin_Preparation (Work, Object, Source, GPU, Bytes, Root, Pages'Length, OK);
            pragma Assert (Calls = Before);
            if not OK then return; end if;
            while W.Preparing (Work) loop
               declare Previous : constant Natural := Reads; begin
                  pragma Assert (not W.Pending (Object) and not W.Published (Object));
                  Prepare (Work, Object, Source); Turns := Turns + 1;
                  pragma Assert (Reads - Previous <= 1 and Calls = Before and Turns < 1000);
                  -- Once captured, later planning must not read borrowed input.
                  if Reads = Input'Length then Input := [others => 123]; end if;
               end;
            end loop;
            OK := W.Prepared (Work, Object, Source);
            if not OK then return; end if;
            pragma Assert (Reads = Pages'Length and Turns > Reads);
            while W.Pending (Object) loop
               Before := Calls;
               W.Step (Object, Source);
               pragma Assert (Calls - Before <= 1);
            end loop;
            OK := W.Published (Object);
         end Publish;
         procedure Adopt (Object : in out W.State; Source : in out VM.Image; OK : out Boolean) is
            Work : W.Adoption;
            Hidden_Turns, Turns : Natural := 0;
            Before : constant Natural := Calls;
            Epoch : constant Unsigned_64 := VM.Revision (Source);
         begin
            W.Begin_Commit (Work, Object, Source, OK);
            if not OK then return; end if;
            while W.Committing (Work) loop
               if VM.Root_DMA (Source) = 0 then
                  Hidden_Turns := Hidden_Turns + 1;
                  pragma Assert (VM.Lookup (Source, 4096) = 0);
                  pragma Assert (VM.Entry_Value (Source, 1, 0) = 0);
                  pragma Assert (VM.Revision (Source) = Epoch);
               end if;
               W.Commit_Step (Work, Object, Source); Turns := Turns + 1;
               pragma Assert (Calls = Before and Turns < 1000);
            end loop;
            OK := W.Committed (Object);
            if OK then
               pragma Assert (Hidden_Turns >= 49 and VM.Revision (Source) = Epoch + 1);
            end if;
         end Adopt;
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
         Adopt (State, Source, OK);
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
            G.Start_Inspection (Active_Query, Source, 8192, 510 * 4096, OK);
            pragma Assert (OK);
            G.Step_Inspection (Active_Query, Source);
            pragma Assert (G.Inspection_State (Active_Query) = G.Scanning);
            G.Start_Inspection (Finished_Query, Source, 2 ** 40, 4096, OK);
            pragma Assert (OK);
            G.Step_Inspection (Finished_Query, Source);
            pragma Assert (G.Inspection_State (Finished_Query) = G.Complete);
            pragma Assert (G.Inspection_Result (Finished_Query, Source).Status = G.Ready);
            Insertions.Execute (Insertion_State, Source, VM.Revision (Source),
              2 ** 39, [16#200000#], Write_Back, Read_Write, OK);
            pragma Assert (OK and not Insertions.Failed (Insertion_State));
            -- The root is retained, but a committed leaf changes the live
            -- revision. Neither in-flight nor completed planning may survive.
            G.Step_Inspection (Active_Query, Source);
            pragma Assert (G.Inspection_State (Active_Query) = G.Stale);
            pragma Assert (G.Inspection_Result (Finished_Query, Source).Status = G.Invalid_Range);
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
            for Interruption in 0 .. 1 loop
               declare
                  Work : W.Rearming;
               begin
                  W.Begin_Rearm (Work, State, Source, 9 * 4096, OK);
                  pragma Assert (OK);
                  Probe_Ownership := True; Ownership_Calls := 0; Lose_On_Call := 0;
                  W.Rearm_Step (Work, State, Source);
                  pragma Assert (Ownership_Calls = 2 and W.Rearm_Pending (Work));
                  Probe_Ownership := False;
                  if Interruption = 0 then W.Cancel_Rearm (Work);
                  else Held := False; end if;
                  W.Rearm_Step (Work, State, Source);
                  pragma Assert (not W.Rearm_Pending (Work) and not W.Rearmed (Work));
                  Held := True;
                  W.Rearm_Step (Work, State, Source);
                  pragma Assert (not W.Rearmed (Work) and W.Committed (State) and Calls = Before);
               end;
            end loop;
         end if;
         Rejected_Value := VM.Revision (Source);
         declare
            Work : W.Rearming;
            Turns : Natural := 0;
         begin
            W.Begin_Rearm (Work, State, Source, 9 * 4096, OK);
            while W.Rearm_Pending (Work) loop
               pragma Assert (W.Committed (State));
               W.Rearm_Step (Work, State, Source);
               Turns := Turns + 1;
               pragma Assert (Turns <= 3 and Calls = Before);
            end loop;
            OK := W.Rearmed (Work);
            if OK then pragma Assert (Turns = 3); end if;
         end;
         pragma Assert (OK = (Fault = 0 and not Lose_Owner));
         pragma Assert (Calls = Before and VM.Revision (Source) = Rejected_Value);
         if OK then
            pragma Assert (not W.Attempted (State) and not W.Published (State)
              and not W.Committed (State));
            -- Retained plan storage is logically invalid, not erased. An idle
            -- publication step must not replay its previous hardware writes.
            W.Step (State, Source);
            pragma Assert (not W.Pending (State) and Calls = Before);
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
   for Fault in 0 .. 4 loop
      declare
         Source : VM.Image;
         Object : VM.Growth_Receipt;
         Held : Boolean := True;
         Reads, IO : Natural := 0;
         OK : Boolean;
         Epoch : Unsigned_64;
         function Owner return Boolean is (Held);
         function Owned (DMA : Unsigned_64) return Boolean is (DMA /= 0 and DMA mod 4096 = 0);
         procedure Read_Word (DMA : Unsigned_64; Index : Table_Index;
                              Value : out Unsigned_64; Accepted : out Boolean) is
            pragma Unreferenced (DMA, Index);
         begin IO := IO + 1; Value := 0; Accepted := True; end;
         procedure Write_Word (DMA : Unsigned_64; Index : Table_Index;
                               Value : Unsigned_64; Accepted : out Boolean) is
            pragma Unreferenced (DMA, Index, Value);
         begin IO := IO + 1; Accepted := True; end;
         function Flush (DMA : Unsigned_64) return Boolean is
            pragma Unreferenced (DMA);
         begin IO := IO + 1; return True; end;
         package B is new G.Backing (Owned);
         package W is new B.Writer (Owner, Read_Word, Write_Word, Flush, Owner);
         Work : W.Preparation;
         function Read_Page (Ordinal : Positive) return Unsigned_64 is
            Accepted : Boolean;
         begin
            Reads := Reads + 1;
            if Fault = 0 then Held := False;
            elsif Fault = 1 then W.Cancel_Preparation (Work, Object);
            elsif Fault = 2 then
               W.Commit (Object, Source, Accepted); pragma Assert (not Accepted);
            end if;
            return Unsigned_64 (Ordinal + 4) * 4096;
         end;
         procedure Prepare is new W.Prepare_Step (Read_Page);
      begin
         VM.Initialize (Source, [4096, 8192, 12288, 16384, others => 0], OK, Backing_Count => 4);
         pragma Assert (OK);
         VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK); pragma Assert (OK);
         VM.Seal (Source, OK); pragma Assert (OK);
         Epoch := VM.Revision (Source);
         W.Begin_Preparation (Work, Object, Source, 2 ** 39, 4096, 4096, 3, OK);
         pragma Assert (OK);
         if Fault = 4 then W.Cancel_Preparation (Work, Object); end if;
         while W.Preparing (Work) loop
            Prepare (Work, Object, Source);
            if Fault = 3 and Reads = 3 then Held := False; end if;
         end loop;
         pragma Assert (Reads = (if Fault = 4 then 0 elsif Fault = 3 then 3 else 1));
         pragma Assert (not W.Prepared (Work, Object, Source) and not W.Pending (Object));
         Held := True; Prepare (Work, Object, Source); W.Step (Object, Source);
         pragma Assert (IO = 0 and W.Attempted (Object) and not W.Published (Object));
         pragma Assert (VM.Revision (Source) = Epoch and VM.Used (Source) = 4);
         W.Begin_Preparation (Work, Object, Source, 2 ** 39, 4096, 4096, 3, OK);
         pragma Assert (not OK);
      end;
   end loop;
   for Fault in 0 .. 4 loop
      declare
         Source : VM.Image;
         Object : VM.Growth_Receipt;
         type Words is array (Table_Index) of Unsigned_64;
         RAM : array (1 .. 8) of Words := [others => [others => 0]];
         Held, Invalidated : Boolean := True;
         Revoke_On_TLB : Boolean := False;
         Calls, Before, Turns : Natural := 0;
         OK : Boolean;
         Epoch : Unsigned_64;
         function Owner return Boolean is (Held);
         function TLB return Boolean is
         begin
            if Revoke_On_TLB then Held := False; end if;
            return Invalidated;
         end TLB;
         function Owned (DMA : Unsigned_64) return Boolean is
           (DMA in 4096 .. 8 * 4096 and then DMA mod 4096 = 0);
         procedure Read_Word (DMA : Unsigned_64; Index : Table_Index;
                              Value : out Unsigned_64; Accepted : out Boolean) is
         begin
            Calls := Calls + 1; Value := RAM (Natural (DMA / 4096)) (Index); Accepted := True;
         end;
         procedure Write_Word (DMA : Unsigned_64; Index : Table_Index;
                               Value : Unsigned_64; Accepted : out Boolean) is
         begin
            Calls := Calls + 1; RAM (Natural (DMA / 4096)) (Index) := Value; Accepted := True;
         end;
         function Flush (DMA : Unsigned_64) return Boolean is
         begin Calls := Calls + 1; return Owned (DMA); end;
         package B is new G.Backing (Owned);
         package W is new B.Writer (Owner, Read_Word, Write_Word, Flush, TLB);
         Work : W.Adoption;
      begin
         VM.Initialize (Source, [4096, 8192, 12288, 16384, others => 0], OK, Backing_Count => 4);
         pragma Assert (OK);
         VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK); pragma Assert (OK);
         VM.Seal (Source, OK); pragma Assert (OK);
         for P in 1 .. 4 loop
            for I in Table_Index loop RAM (P) (I) := VM.Entry_Value (Source, P, I); end loop;
         end loop;
         Epoch := VM.Revision (Source);
         W.Start (Object, Source, 2 ** 39, 4096, 4096, [5 * 4096, 6 * 4096, 7 * 4096], OK);
         pragma Assert (OK);
         while W.Pending (Object) loop W.Step (Object, Source); end loop;
         pragma Assert (W.Published (Object));
         Before := Calls;
         W.Begin_Commit (Work, Object, Source, OK); pragma Assert (OK);
         while VM.Root_DMA (Source) /= 0 loop
            W.Commit_Step (Work, Object, Source); Turns := Turns + 1;
            pragma Assert (W.Committing (Work) and Turns < 1000);
         end loop;
         if Fault = 4 then Revoke_On_TLB := True;
         elsif Fault = 3 then
            -- Lose exclusion after some mirror words have actually changed.
            for I in 1 .. 4 loop W.Commit_Step (Work, Object, Source); end loop;
            Held := False;
         elsif Fault = 2 then Invalidated := False;
         elsif Fault = 1 then Held := False;
         else W.Cancel_Commit (Work); end if;
         W.Commit_Step (Work, Object, Source);
         pragma Assert (not W.Committing (Work) and not W.Committed (Object));
         pragma Assert (VM.Root_DMA (Source) = 0 and VM.Lookup (Source, 4096) = 0);
         pragma Assert (VM.Entry_Value (Source, 1, 0) = 0 and VM.Revision (Source) = Epoch);
         Held := True; Invalidated := True;
         W.Commit_Step (Work, Object, Source); W.Commit (Object, Source, OK);
         pragma Assert (not OK and not W.Committed (Object) and VM.Root_DMA (Source) = 0);
         pragma Assert (Calls = Before);
      end;
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
   Ada.Text_IO.Put_Line ("Growth writer PASS28 with stepped capture/adoption; preparation faults PASS5 and hidden-adoption faults PASS5 no IO/replay; host RAM publication and gated mirror commit (no GPU validation)");
end VM_Growth_Writer_Tests;
