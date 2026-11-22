with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Removal;
procedure VM_Removal_Commit_Tests is
   package VM is new Intel_GPU_VM_Image (8);
begin
   for Fault in 0 .. 6 loop
      declare
         Source, Other : VM.Image;
         Backing : VM.Backing_Pages;
         Held : Boolean := True;
         Writes, Turns : Natural := 0;
         Epoch : Unsigned_64;
         OK, Complete : Boolean;
         Conflict_Calls : Natural := 0;
         function No_Conflict (Page : Unsigned_64) return Boolean is
            pragma Unreferenced (Page);
         begin Conflict_Calls := Conflict_Calls + 1; return False; end No_Conflict;
         function Backing_Disjoint is new VM.Backing_Disjoint (No_Conflict);
         procedure Check_Hidden is
         begin
            Conflict_Calls := 0;
            pragma Assert (VM.Root_DMA (Source) = 0);
            -- A hidden mirror is unknown, never proof of absence of aliases.
            pragma Assert (not Backing_Disjoint (Source) and Conflict_Calls = 0);
            pragma Assert (not VM.DMA_Disjoint (Source, 16#400000#, 4096));
         end Check_Hidden;
         function Exclusive return Boolean is (Held);
         procedure Write_Leaf
           (Table_DMA : Unsigned_64; Index : Table_Index;
            Expected, Replacement : Unsigned_64; Success : out Boolean) is
         begin
            Writes := Writes + 1;
            pragma Assert (Table_DMA = 16384 and Index = Table_Index (Writes));
            pragma Assert (Expected = Encode_Leaf
              (16#200000# + Unsigned_64 (Writes - 1) * 4096, Write_Back, Read_Write));
            pragma Assert (Replacement = 0);
            Success := True;
         end Write_Leaf;
         procedure Invalidate (Success : out Boolean) is
         begin Success := True; end Invalidate;
         package Removal is new VM.Removal (Exclusive, Write_Leaf, Invalidate);
         State : Removal.Controller;
      begin
         for P in Backing'Range loop Backing (P) := Unsigned_64 (P) * 4096; end loop;
         VM.Initialize (Source, Backing, OK); pragma Assert (OK);
         VM.Map_Pages (Source, 4096, [16#200000#, 16#201000#, 16#202000#, 16#203000#],
           Write_Back, Read_Write, OK); pragma Assert (OK);
         VM.Seal (Source, OK); pragma Assert (OK);
         for P in Backing'Range loop Backing (P) := Unsigned_64 (P + 16) * 4096; end loop;
         VM.Initialize (Other, Backing, OK); pragma Assert (OK);
         VM.Map_Pages (Other, 4096, [16#300000#], Write_Back, Read_Write, OK);
         pragma Assert (OK);
         VM.Seal (Other, OK); pragma Assert (OK);
         Epoch := VM.Revision (Source);
         Removal.Publish (State, Source, Epoch, 4096,
           [16#200000#, 16#201000#, 16#202000#], OK);
         pragma Assert (OK and Writes = 3 and VM.Lookup (Source, 4096) /= 0);
         Removal.Begin_Commit (State, Source, Fault /= 1, OK);
         pragma Assert (OK = (Fault /= 1));
         if OK then
            Check_Hidden;
            pragma Assert (VM.Root_DMA (Source) = 0 and VM.Lookup (Source, 16384) = 0);
            Removal.Begin_Commit (State, Source, True, OK);
            pragma Assert (not OK and Removal.Committing (State));
            if Fault = 5 then Removal.Cancel_Commit (State);
            elsif Fault = 6 then Held := False; end if;
            while Removal.Committing (State) loop
               Turns := Turns + 1; pragma Assert (Turns <= 3);
               pragma Assert (VM.Revision (Source) = Epoch and VM.Root_DMA (Source) = 0);
               Check_Hidden;
               if Fault = 3 then Removal.Commit_Step (State, Other, Complete);
               else Removal.Commit_Step (State, Source, Complete); end if;
               if Fault = 2 then Held := False;
               elsif Fault = 4 then Removal.Cancel_Commit (State); end if;
            end loop;
         end if;
         pragma Assert (VM.Lookup (Other, 4096) = 16#300003#);
         if Fault = 0 then
            pragma Assert (Complete and Turns = 3 and not Removal.Failed (State));
            pragma Assert (VM.Revision (Source) = Epoch + 1 and VM.Root_DMA (Source) = 4096);
            pragma Assert (VM.Lookup (Source, 4096) = 0 and VM.Lookup (Source, 12288) = 0);
            pragma Assert (VM.Lookup (Source, 16384) = 16#203003#);
            pragma Assert (VM.DMA_Disjoint (Source, 16#200000#, 3 * 4096));
            pragma Assert (not VM.DMA_Disjoint (Source, 16#203000#, 4096));
         else
            pragma Assert (Removal.Failed (State) and VM.Revision (Source) = Epoch);
            pragma Assert ((VM.Root_DMA (Source) /= 0) = (Fault = 1));
            if Fault /= 1 then Check_Hidden; end if;
            Held := True;
            Removal.Commit_Step (State, Source, Complete); pragma Assert (not Complete);
            Removal.Commit (State, Source, True, OK); pragma Assert (not OK);
            pragma Assert (VM.Revision (Source) = Epoch and Writes = 3);
            if Fault /= 1 then Check_Hidden; end if;
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Removal commit PASS7: hidden partial mirror denies retirement, final alias checks/epoch, invalidation/owner/source failure, cancel and no replay (mock GPU)");
end VM_Removal_Commit_Tests;
