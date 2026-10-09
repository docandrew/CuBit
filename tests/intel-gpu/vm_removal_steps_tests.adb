with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Removal;
with Intel_GPU_PPGTT_Scratch;
procedure VM_Removal_Steps_Tests is
   package VM is new Intel_GPU_VM_Image (8);
begin
   for With_Scratch in Boolean loop
   for Fault in 0 .. 11 loop
      declare
         Source, Other : VM.Image;
         DMA : VM.Backing_Pages;
         Data : VM.Data_Pages (5 .. 7) := [16#200000#, 16#201000#, 16#202000#];
         Scratch : constant Intel_GPU_PPGTT_Scratch.Backing_Pages :=
           (if With_Scratch then [16#90000#, 16#91000#, 16#92000#, 16#93000#]
            else [others => 0]);
         Held : Boolean := True;
         Writes, Before : Natural := 0;
         Epoch : Unsigned_64;
         OK : Boolean;
         procedure Reenter;
         function Exclusive return Boolean is (Held);
         procedure Write_Leaf
           (Table_DMA : Unsigned_64; Index : Table_Index;
            Expected, Replacement : Unsigned_64; Success : out Boolean) is
         begin
            Writes := Writes + 1;
            pragma Assert (Table_DMA = 16#4000# and Index = Table_Index (Writes + 1));
            pragma Assert (Expected = Encode_Leaf
              (16#200000# + Unsigned_64 (Writes - 1) * 4096, Write_Back, Read_Write));
            pragma Assert (Replacement = Intel_GPU_PPGTT_Scratch.Fallback (Scratch, 0));
            pragma Assert (VM.Lookup (Source, 8192) /= 0 and VM.Revision (Source) = Epoch);
            if Writes = 1 and Fault in 5 | 6 | 10 | 11 then Reenter; end if;
            if Writes = 3 and Fault = 8 then Held := False; end if;
            Success := not (Fault = 7 and Writes = 2);
         end Write_Leaf;
         procedure Invalidate (Success : out Boolean) is
         begin Success := True; end;
         package Remove is new VM.Removal (Exclusive, Write_Leaf, Invalidate);
         State : Remove.Controller;
         function Expected_Page (Ordinal : Positive) return Unsigned_64 is
           (16#200000# + Unsigned_64 (Ordinal - 1) * 4096);
         procedure Capture is new Remove.Capture_Step (Expected_Page);
         procedure Reenter is
            Accepted : Boolean;
         begin
            if Fault = 5 then Remove.Step (State, Source);
            elsif Fault = 10 then
               Remove.Begin_Prepare (State, Source, Epoch, 8192, 3, Accepted);
               pragma Assert (not Accepted);
            elsif Fault = 11 then
               Capture (State, Source, Accepted);
               pragma Assert (not Accepted);
            else Remove.Commit (State, Source, True, Accepted); pragma Assert (not Accepted);
            end if;
         end Reenter;
      begin
         for P in DMA'Range loop DMA (P) := Unsigned_64 (P) * 4096; end loop;
         VM.Initialize (Source, DMA, OK, Scratch); pragma Assert (OK);
         VM.Map_Pages (Source, 8192, Data, Write_Back, Read_Write, OK); pragma Assert (OK);
         VM.Seal (Source, OK); pragma Assert (OK);
         for P in DMA'Range loop DMA (P) := Unsigned_64 (P + 16) * 4096; end loop;
         VM.Initialize (Other, DMA, OK, Scratch); pragma Assert (OK);
         VM.Map_Pages (Other, 8192, Data, Write_Back, Read_Write, OK); pragma Assert (OK);
         VM.Seal (Other, OK); pragma Assert (OK);
         Epoch := VM.Revision (Source);
         Remove.Start (State, Source, Epoch, 8192, [16#200000#, 16#BAD000#], OK);
         pragma Assert (not OK and Writes = 0 and not Remove.Failed (State));
         Remove.Start (State, Source, Epoch, 8192, Data, OK);
         pragma Assert (OK and Writes = 0 and Remove.Publishing (State) and not Remove.Published (State));
         Data := [others => 16#BAD000#]; -- no borrowed client array after Start
         Remove.Start (State, Source, Epoch, 8192, Data, OK);
         pragma Assert (not OK and Writes = 0 and Remove.Publishing (State));
         if Fault = 1 then Held := False; end if;
         if Fault = 4 then Remove.Commit (State, Source, True, OK); pragma Assert (not OK); end if;
         while Remove.Publishing (State) loop
            Before := Writes;
            if Fault = 2 and Writes = 1 then Held := False; end if;
            if Fault = 3 and Writes = 1 then Remove.Step (State, Other);
            else Remove.Step (State, Source); end if;
            pragma Assert (Writes <= Before + 1 and Writes <= 3);
         end loop;
         pragma Assert (VM.Lookup (Source, 8192) /= 0 and VM.Revision (Source) = Epoch);
         pragma Assert (Remove.Published (State) = (Fault in 0 | 9));
         Remove.Commit (State, Source, Fault /= 9, OK);
         pragma Assert (OK = (Fault = 0));
         if OK then
            pragma Assert (Writes = 3 and VM.Revision (Source) = Epoch + 1);
            pragma Assert (VM.Lookup (Source, 8192) = 0 and VM.Lookup (Source, 16384) = 0);
            pragma Assert (VM.Used (Source) = 4 and VM.Root_DMA (Source) = 4096);
         else
            pragma Assert (Remove.Failed (State) and VM.Revision (Source) = Epoch);
            pragma Assert (Writes = (case Fault is
              when 1 | 4 => 0, when 2 | 3 | 5 | 6 | 10 | 11 => 1, when 7 => 2, when others => 3));
            Held := True; Before := Writes;
            Remove.Step (State, Source); pragma Assert (Writes = Before);
            Remove.Start (State, Source, Epoch, 8192, Data, OK);
            pragma Assert (not OK and Writes = Before);
            Remove.Commit (State, Source, True, OK); pragma Assert (not OK);
         end if;
      end;
   end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Stepped removal PASS24: one write/turn, fault/scratch fallbacks, retained source, no early metadata commit, owner/source loss, reentry and no replay (mock GPU)");
end VM_Removal_Steps_Tests;
