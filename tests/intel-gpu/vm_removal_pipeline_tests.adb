with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Removal;
with Intel_GPU_VM_Update;
procedure VM_Removal_Pipeline_Tests is
   package Image is new Intel_GPU_VM_Image (8);
begin
   for Fault in 0 .. 3 loop
      declare
         Source : Image.Image;
         Backing : Image.Backing_Pages;
         Alive, Invalidated : Boolean := True;
         Epoch : Unsigned_64;
         Writes, Resumes : Natural := 0;
         OK, Done : Boolean;
         function Owner return Boolean is (Alive);
         procedure Write_Leaf
           (Table_DMA : Unsigned_64; Index : Table_Index;
            Expected, Replacement : Unsigned_64; Success : out Boolean) is
         begin
            Writes := Writes + 1;
            pragma Assert (Table_DMA = 16384 and Index = Table_Index (Writes));
            pragma Assert (Expected = Encode_Leaf
              (16#200000# + Unsigned_64 (Writes - 1) * 4096, Write_Back, Read_Write));
            pragma Assert (Replacement = 0); Success := True;
         end Write_Leaf;
         procedure Invalidate (Success : out Boolean) is
         begin
            pragma Assert (Writes = 3 and Image.Root_DMA (Source) /= 0);
            Invalidated := Fault /= 1; Success := Invalidated;
         end Invalidate;
         package Removal is new Image.Removal (Owner, Write_Leaf, Invalidate);
         Receipt : Removal.Controller;
         procedure Drain (Success : out Boolean) is
         begin Success := True; end Drain;
         procedure Publish (Success : out Boolean) is
         begin
            Removal.Publish (Receipt, Source, Epoch, 4096,
              [16#200000#, 16#201000#, 16#202000#], Success);
         end Publish;
         procedure No_Resume (Success : out Boolean) is
         begin pragma Assert (False); Success := False; end No_Resume;
         package Update is new Intel_GPU_VM_Update (Owner, Drain, Publish, Invalidate, No_Resume);
         use type Update.Result;
         State : Update.State;
         procedure Publish_Once (Finished, Success : out Boolean) is
         begin Publish (Success); Finished := True; end Publish_Once;
         procedure Resume (Finished, Success : out Boolean) is
            Complete : Boolean;
         begin
            Resumes := Resumes + 1;
            pragma Assert (Invalidated and not Update.Can_Submit (State));
            pragma Assert (Update.Generation (State) = 0);
            if not Removal.Committing (Receipt) then
               Removal.Begin_Commit (Receipt, Source, Invalidated, Success);
               Finished := not Success;
            else
               Removal.Commit_Step (Receipt, Source, Complete);
               Finished := not Removal.Committing (Receipt);
               Success := not Finished or else Complete;
            end if;
         end Resume;
         procedure Advance is new Update.Advance (Publish_Once, Resume);
         Status : Update.Result;
         Turns : Natural := 0;
      begin
         Invalidated := False;
         for P in Backing'Range loop Backing (P) := Unsigned_64 (P) * 4096; end loop;
         Image.Initialize (Source, Backing, OK); pragma Assert (OK);
         Image.Map_Pages (Source, 4096, [16#200000#, 16#201000#, 16#202000#, 16#203000#],
           Write_Back, Read_Write, OK); pragma Assert (OK);
         Image.Seal (Source, OK); pragma Assert (OK);
         Epoch := Image.Revision (Source);
         Update.Begin_Update (State, 0, OK, Status); pragma Assert (OK);
         loop
            Turns := Turns + 1; pragma Assert (Turns <= 7);
            pragma Assert (not Update.Can_Submit (State));
            Advance (State, Done, Status);
            exit when Done;
            if Resumes > 0 then
               pragma Assert (Image.Root_DMA (Source) = 0 and Image.Revision (Source) = Epoch);
            end if;
            if (Fault = 2 and Resumes = 1) or else (Fault = 3 and Resumes = 2) then
               Alive := False;
            end if;
         end loop;
         if Fault = 0 then
            pragma Assert (Status = Update.Complete and Resumes = 4);
            pragma Assert (Update.Can_Submit (State) and Update.Generation (State) = 1);
            pragma Assert (Image.Revision (Source) = Epoch + 1 and Image.Root_DMA (Source) = 4096);
            pragma Assert (Image.Lookup (Source, 4096) = 0 and Image.Lookup (Source, 16384) = 16#203003#);
         else
            -- Mirrors native Fail_Update: cancel the retained partial commit.
            Removal.Cancel_Commit (Receipt);
            pragma Assert (not Update.Can_Submit (State) and Update.Generation (State) = 0);
            pragma Assert (Image.Revision (Source) = Epoch);
            pragma Assert (Resumes = (case Fault is when 1 => 0, when 2 => 1, when others => 2));
            Alive := True;
            Advance (State, Done, Status); pragma Assert (Done and Status = Update.Rejected);
            pragma Assert (not Update.Can_Submit (State));
            pragma Assert ((Image.Root_DMA (Source) /= 0) = (Fault = 1));
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Removal pipeline PASS4: real coordinator/commit composition, hidden yields, single epochs, invalidation/owner failure and no replay (mock hardware)");
end VM_Removal_Pipeline_Tests;
