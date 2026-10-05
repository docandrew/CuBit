with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Growth;
with Intel_GPU_VM_Image.Growth.Backing;
with Intel_GPU_VM_Image.Growth.Backing.Writer;
with Intel_GPU_VM_Image.Insertion;
with Intel_GPU_VM_Update;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_PPGTT_Scratch;
procedure VM_Growth_Bind_Tests is
   package VM is new Intel_GPU_VM_Image (8);
   package G is new VM.Growth;
begin
   -- Hosted composition test: CPU RAM and explicit fake invalidation results,
   -- not Intel hardware/cache validation. Faults fail at either invalidation,
   -- ownership between phases, or after a leaf write.
   for Sparse in Boolean loop
   for Async in Boolean loop
   for Fault in 0 .. 4 loop
      declare
         Source : VM.Image;
         DMA : VM.Backing_Pages;
         Scratch : Intel_GPU_PPGTT_Scratch.Backing_Pages :=
           [16#20000#, 16#21000#, 16#22000#, 16#23000#];
         type Words is array (Table_Index) of Unsigned_64;
         RAM : array (1 .. 13) of Words := [others => [others => 0]];
         Held : Boolean := True;
         Directory_TLB, Leaf_TLB : Boolean := False;
         Invalidations, Leaf_Writes : Natural := 0;
         function Owner return Boolean is (Held);
         function Owned (Address : Unsigned_64) return Boolean is
           (Address in 4096 .. 13 * 4096 and then Address mod 4096 = 0);
         procedure Check_Closed;
         procedure Read_Word (Address : Unsigned_64; Index : Table_Index;
                              Value : out Unsigned_64; OK : out Boolean) is
         begin
            Check_Closed;
            OK := Held and Owned (Address); Value := 0;
            if OK then Value := RAM (Natural (Address / 4096)) (Index); end if;
         end Read_Word;
         procedure Write_Word (Address : Unsigned_64; Index : Table_Index;
                               Value : Unsigned_64; OK : out Boolean) is
         begin
            Check_Closed;
            OK := Held and Owned (Address);
            if OK then RAM (Natural (Address / 4096)) (Index) := Value; end if;
         end Write_Word;
         function Flush (Address : Unsigned_64) return Boolean is
         begin Check_Closed; return Held and Owned (Address); end Flush;
         function Directories_Visible return Boolean is (Directory_TLB);
         package B is new G.Backing (Owned);
         package W is new B.Writer (Owner, Read_Word, Write_Word, Flush, Directories_Visible);
         Growth : VM.Growth_Receipt;
         procedure Write_Leaf (Address : Unsigned_64; Index : Table_Index;
                               Expected, Replacement : Unsigned_64; OK : out Boolean) is
         begin
            Check_Closed;
            pragma Assert (Directory_TLB and W.Committed (Growth));
            OK := Held and then Owned (Address) and then
              RAM (Natural (Address / 4096)) (Index) = Expected;
            if not OK then return; end if;
            RAM (Natural (Address / 4096)) (Index) := Replacement;
            Leaf_Writes := Leaf_Writes + 1;
            OK := Fault /= 4; -- ambiguous failure AFTER the actual store
         end Write_Leaf;
         procedure Invalidate_Leaf (OK : out Boolean) is
         begin
            Check_Closed;
            pragma Assert (Directory_TLB and Leaf_Writes = 1);
            Invalidations := Invalidations + 1;
            Leaf_TLB := Held and Fault /= 2; OK := Leaf_TLB;
         end Invalidate_Leaf;
         package I is new VM.Insertion (Owner, Write_Leaf, Invalidate_Leaf);
         Insertion : I.Controller;
         Data : VM.Data_Pages := [16#110000#];
         Target : Unsigned_64 := 2 ** 39;
         New_Tables : VM.Data_Pages (1 .. 3) := [10 * 4096, 11 * 4096, 12 * 4096];
         New_Count : Positive := 3;
         procedure Drain (OK : out Boolean) is
         begin Check_Closed; OK := Held; end Drain;
         Publish_Phase : Natural := 0;
         procedure Advance_Publication (Finished, OK : out Boolean) is
         begin
            Check_Closed;
            Finished := False; OK := True;
            case Publish_Phase is
               when 0 =>
                  W.Start (Growth, Source, Target, 4096, 9 * 4096,
                           New_Tables (1 .. New_Count), OK);
                  Publish_Phase := 1;
               when 1 =>
                  W.Step (Growth, Source);
                  if not W.Pending (Growth) then
                     OK := W.Published (Growth); Publish_Phase := 2;
                  end if;
               when 2 =>
                  Invalidations := Invalidations + 1;
                  Directory_TLB := Fault /= 1;
                  OK := Directory_TLB; Publish_Phase := 3;
               when 3 =>
                  W.Commit (Growth, Source, OK);
                  if Fault = 3 then Held := False; OK := False; end if;
                  Publish_Phase := 4;
               when 4 =>
                  I.Start (Insertion, Source, VM.Revision (Source), Target,
                           Data, Write_Back, Read_Write, OK);
                  Publish_Phase := 5;
               when 5 =>
                  I.Step (Insertion, Source);
                  Finished := not I.Publishing (Insertion);
                  OK := not Finished or else I.Published (Insertion);
               when others => OK := False;
            end case;
            if not OK then Finished := True; end if;
         end Advance_Publication;
         procedure Publish (OK : out Boolean) is
            Finished : Boolean;
         begin
            loop
               Advance_Publication (Finished, OK);
               exit when Finished;
            end loop;
         end Publish;
         procedure Resume (OK : out Boolean) is
         begin
            Check_Closed;
            I.Commit (Insertion, Source, Leaf_TLB, OK);
         end Resume;
         package C is new Intel_GPU_VM_Update
           (Owner, Drain, Publish, Invalidate_Leaf, Resume);
         State : C.State;
         procedure Advance is new C.Advance (Advance_Publication);
         use type C.Result;
         use type C.Phase;
         procedure Check_Closed is
         begin pragma Assert (not C.Can_Submit (State)); end Check_Closed;
         OK : Boolean;
         Before : Unsigned_64;
         Result : C.Result;
      begin
         for P in DMA'Range loop
            DMA (P) := (if Sparse and P > 4 then 0 else Unsigned_64 (P) * 4096);
         end loop;
         VM.Initialize (Source, DMA, OK, Scratch,
                        Backing_Count => (if Sparse then 4 else 8));
         pragma Assert (OK);
         VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK); pragma Assert (OK);
         if Sparse then
            -- Metadata capacity remains eight, but only four pages exist.
            -- Offline mapping must not publish a zero-address directory.
            VM.Map_Page (Source, 2 ** 39, 16#110000#, Write_Back, Read_Write, OK);
            pragma Assert (not OK and VM.Used (Source) = 4);
            pragma Assert (VM.Lookup (Source, 2 ** 39) = 0);
            pragma Assert (VM.Lookup (Source, 4096) /= 0);
         end if;
         VM.Seal (Source, OK); pragma Assert (OK);
         Before := VM.Revision (Source);
         for P in 1 .. VM.Used (Source) loop
            for Index in Table_Index loop RAM (P) (Index) := VM.Entry_Value (Source, P, Index); end loop;
         end loop;
         RAM (9) := RAM (1); -- retained hardware root differs from historical root
         pragma Assert (not I.Range_Reusable (Source, 2 ** 39, 4096));
         if Async then
            declare
               Finished : Boolean;
               Turns : Natural := 0;
            begin
               C.Begin_Update (State, 0, OK, Result); pragma Assert (OK);
               loop
                  Check_Closed;
                  pragma Assert (C.Generation (State) = 0);
                  Turns := Turns + 1; pragma Assert (Turns < 20000);
                  Advance (State, Finished, Result);
                  exit when Finished;
               end loop;
            end;
         else C.Execute (State, 0, Result); end if;
         pragma Assert ((Result = C.Complete) = (Fault = 0));
         pragma Assert (RAM (1) (1) = Intel_GPU_PPGTT_Scratch.Fallback (Scratch, 3));
         pragma Assert (VM.Lookup (Source, 4096) = Encode_Leaf (16#100000#, Write_Back, Read_Write));
         if Fault = 0 then
            pragma Assert (C.Generation (State) = 1 and C.Can_Submit (State));
            pragma Assert (VM.Revision (Source) = Before + 2);
            pragma Assert (VM.Lookup (Source, 2 ** 39) = Encode_Leaf (Data (1), Write_Back, Read_Write));
            pragma Assert (RAM (12) (0) = VM.Lookup (Source, 2 ** 39));
            pragma Assert (RAM (9) (1) = VM.Entry_Value (Source, 1, 1));
            pragma Assert (Invalidations = 2 and Leaf_Writes = 1);
            -- Reuse the same controllers after BOTH directory and leaf epochs
            -- advanced. Allocate only one new PT in the already-added subtree.
            W.Rearm (Growth, Source, 9 * 4096, OK); pragma Assert (OK);
            Target := 2 ** 39 + 2 ** 21;
            Data := [16#120000#]; New_Tables (1) := 13 * 4096; New_Count := 1;
            Publish_Phase := 0; Directory_TLB := False; Leaf_TLB := False;
            Invalidations := 0; Leaf_Writes := 0;
            declare
               Needed : constant G.Requirements := G.Inspect (Source, Target, 4096);
               use type G.Plan_Status;
               Finished : Boolean;
               Turns : Natural := 0;
            begin
               pragma Assert (Needed.Status = G.Ready and Needed.Additional_Tables = 1);
               if Async then
                  C.Begin_Update (State, 1, OK, Result); pragma Assert (OK);
                  loop
                     pragma Assert (not C.Can_Submit (State) and C.Generation (State) = 1);
                     Turns := Turns + 1; pragma Assert (Turns < 20000);
                     Advance (State, Finished, Result);
                     exit when Finished;
                  end loop;
               else C.Execute (State, 1, Result); end if;
            end;
            pragma Assert (Result = C.Complete and C.Generation (State) = 2 and C.Can_Submit (State));
            pragma Assert (VM.Revision (Source) = Before + 4 and VM.Used (Source) = 8);
            pragma Assert (VM.Lookup (Source, 2 ** 39) = Encode_Leaf (16#110000#, Write_Back, Read_Write));
            pragma Assert (VM.Lookup (Source, Target) = Encode_Leaf (16#120000#, Write_Back, Read_Write));
            pragma Assert (RAM (13) (0) = VM.Lookup (Source, Target));
            pragma Assert (RAM (9) (1) = VM.Entry_Value (Source, 1, 1));
            pragma Assert (RAM (1) (1) = Intel_GPU_PPGTT_Scratch.Fallback (Scratch, 3));
            pragma Assert (Invalidations = 2 and Leaf_Writes = 1);
         else
            pragma Assert (C.Current_Phase (State) = C.Quarantined and not C.Can_Submit (State));
            pragma Assert (C.Generation (State) = 0 and VM.Lookup (Source, 2 ** 39) = 0);
            pragma Assert (VM.Revision (Source) = Before + (if Fault = 1 then 0 else 1));
            pragma Assert (Leaf_Writes = (if Fault in 1 | 3 then 0 else 1));
            -- Poisoned/uncertain hardware state is retained, never rolled back.
            Held := True;
            C.Execute (State, 0, Result); pragma Assert (Result = C.Rejected);
         end if;
      end;
   end loop;
   end loop;
   end loop;
   -- Partial backing must be explicit, contiguous in the supplied descriptor,
   -- and free of hidden suffix allocations. The source stays intact on a
   -- replacement rejected for insufficient backing.
   declare
      Source, Target, Replacement, Hidden, Hole, Legacy : VM.Image;
      Pages : VM.Backing_Pages := [1 => 4096, 2 => 8192, 3 => 12288,
                                   4 => 16384, others => 0];
      OK : Boolean;
   begin
      VM.Initialize (Legacy, Pages, OK); pragma Assert (not OK);
      VM.Initialize (Source, Pages, OK, Backing_Count => 4); pragma Assert (OK);
      VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK);
      pragma Assert (OK);
      VM.Seal (Source, OK); pragma Assert (OK);
      Pages (2) := 0;
      VM.Initialize (Hole, Pages, OK, Backing_Count => 4); pragma Assert (not OK);
      Pages (2) := 8192; Pages (8) := 32768;
      VM.Initialize (Hidden, Pages, OK, Backing_Count => 4); pragma Assert (not OK);
      Pages := [1 => 16#30000#, 2 => 16#31000#, 3 => 16#32000#, others => 0];
      VM.Prepare_Update (Target, Source, Pages, OK, Backing_Count => 3);
      pragma Assert (not OK and VM.Used (Source) = 4 and VM.Lookup (Source, 4096) /= 0);
      Pages (4) := 16#33000#;
      VM.Prepare_Update (Replacement, Source, Pages, OK, Backing_Count => 4);
      pragma Assert (OK and VM.Used (Replacement) = 4);
      pragma Assert (VM.Lookup (Replacement, 4096) = VM.Lookup (Source, 4096));
      VM.Map_Page (Replacement, 8192, 16#101000#, Write_Back, Read_Write, OK);
      pragma Assert (OK); -- existing leaf table requires no additional backing
      VM.Map_Page (Replacement, 2 ** 39, 16#102000#, Write_Back, Read_Write, OK);
      pragma Assert (not OK and VM.Used (Replacement) = 4);
      VM.Seal_Update (Replacement, OK); pragma Assert (OK);
      pragma Assert (VM.Direct_Successor (Source, Replacement));
   end;
   Ada.Text_IO.Put_Line ("Growth+bind PASS20: full/partial initial backing, synchronous/stepped writer, retained root, two visibility gates, one public generation, quarantine on partial failures (host model)");
   Ada.Text_IO.Put_Line ("Repeated growth+bind PASS4: same context/root/controllers, rearm after leaf commit, three-page then one-page growth, retained previous mappings, two public generations (host model)");
end VM_Growth_Bind_Tests;
