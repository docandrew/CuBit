with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Insertion;
with Intel_GPU_VM_Image.Test_Mutation;
procedure VM_Commit_Step_Tests is
   package VM is new Intel_GPU_VM_Image (8);
begin
   for Fault in 0 .. 5 loop
      declare
         Source, Other : VM.Image;
         package Mutation is new VM.Test_Mutation;
         Held : Boolean := True;
         OK, Done : Boolean;
         Epoch : Unsigned_64;
         Writes : Natural := 0;
         function Exclusive return Boolean is (Held);
         procedure Write_Leaf
           (Table_DMA : Unsigned_64; Index : Table_Index;
            Expected, Replacement : Unsigned_64; Success : out Boolean) is
         begin
            pragma Assert (Table_DMA = 16384 and Expected = 0);
            pragma Assert (Index in 2 .. 4 and Replacement /= 0);
            Writes := Writes + 1;
            Success := True;
         end Write_Leaf;
         procedure Invalidate (Success : out Boolean) is
         begin Success := True; end Invalidate;
         package Insert is new VM.Insertion (Exclusive, Write_Leaf, Invalidate);
         State : Insert.Controller;
      begin
         VM.Initialize (Source, [4096, 8192, 12288, 16384, others => 0], OK,
           Backing_Count => 4);
         pragma Assert (OK);
         VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK);
         pragma Assert (OK);
         VM.Seal (Source, OK); pragma Assert (OK);
         VM.Initialize (Other, [16#11000#, 16#12000#, 16#13000#, 16#14000#, others => 0], OK,
           Backing_Count => 4); pragma Assert (OK);
         VM.Map_Page (Other, 4096, 16#100000#, Write_Back, Read_Write, OK);
         pragma Assert (OK);
         VM.Seal (Other, OK); pragma Assert (OK);
         Epoch := VM.Revision (Source);
         Insert.Publish (State, Source, Epoch, 8192,
           [16#200000#, 16#201000#, 16#202000#], Write_Back, Read_Write, OK);
         pragma Assert (OK and Writes = 3);
         Insert.Begin_Commit (State, Source, Fault /= 1, OK);
         pragma Assert (OK = (Fault /= 1));
         pragma Assert (VM.Lookup (Source, 8192) = 0);
         if OK then
            pragma Assert (VM.Root_DMA (Source) = 0); -- hide the incomplete mirror
            Insert.Commit_Step (State, Source, Done);
            pragma Assert (not Done and Insert.Committing (State));
            pragma Assert (VM.Revision (Source) = Epoch);
            pragma Assert (VM.Lookup (Source, 8192) = 0); -- no partial public lookup
            pragma Assert (VM.Lookup (Source, 12288) = 0);
            if Fault = 2 then Held := False; end if;
            if Fault = 5 then Mutation.Change_Revision (Source); end if;
            if Fault = 3 then
               Insert.Begin_Commit (State, Source, True, OK);
               pragma Assert (not OK); -- cannot restart the cursor
            end if;
            if Fault = 4 then Insert.Commit_Step (State, Other, Done);
            else Insert.Commit_Step (State, Source, Done); end if;
            pragma Assert (not Done);
            if Fault not in 2 | 4 | 5 then
               pragma Assert (VM.Lookup (Source, 12288) = 0);
               pragma Assert (VM.Lookup (Source, 16384) = 0);
               Insert.Commit_Step (State, Source, Done);
               pragma Assert (Done and VM.Revision (Source) = Epoch + 1);
               pragma Assert (VM.Root_DMA (Source) = 4096);
               pragma Assert (VM.Lookup (Source, 12288) = Encode_Leaf
                 (16#201000#, Write_Back, Read_Write));
               pragma Assert (not Insert.Failed (State));
            else
               pragma Assert (Insert.Failed (State) and not Insert.Committing (State));
               pragma Assert (VM.Root_DMA (Source) = 0);
               pragma Assert (VM.Root_DMA (Other) = 16#11000#);
               Held := True;
               Insert.Begin_Commit (State, Source, True, OK);
               pragma Assert (not OK);
            end if;
         else
            pragma Assert (Insert.Failed (State));
         end if;
         Insert.Commit_Step (State, Source, Done);
         pragma Assert (not Done and Writes = 3);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Commit steps PASS6: hidden partial state, final epoch, invalidation refusal, exclusion/revision/image loss, no restart (mock GPU)");
end VM_Commit_Step_Tests;
