with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Insertion;
with Intel_GPU_VM_Image.Test_Mutation;
procedure VM_Capture_Step_Tests is
   package VM is new Intel_GPU_VM_Image (8);
   package Mutation is new VM.Test_Mutation;
begin
   for Fault in 0 .. 7 loop
      declare
         Source : VM.Image;
         Held, OK : Boolean := True;
         Calls, Writes, Validation_Turns : Natural := 0;
         Epoch : Unsigned_64;
         function Exclusive return Boolean is (Held);
         procedure Write_Leaf
           (Table_DMA : Unsigned_64; Index : Table_Index;
            Expected, Replacement : Unsigned_64; Success : out Boolean) is
         begin
            pragma Assert (Calls = 65 and Table_DMA = 16384 and Expected = 0);
            Writes := Writes + 1;
            pragma Assert (Index = Table_Index (Writes + 1));
            pragma Assert (Replacement = Encode_Leaf
              (16#200000# + Unsigned_64 (Writes - 1) * 4096, Write_Back, Read_Write));
            Success := True;
         end Write_Leaf;
         procedure Invalidate (Success : out Boolean) is
         begin Success := True; end Invalidate;
         package Insert is new VM.Insertion (Exclusive, Write_Leaf, Invalidate);
         State : Insert.Controller;
         function Data_Page (Ordinal : Positive) return Unsigned_64 is
            Nested : Boolean;
         begin
            Calls := Calls + 1;
            pragma Assert (Ordinal = Calls and Writes = 0);
            if Calls = 33 then
               case Fault is
                  when 1 => return 16#200001#;
                  when 2 => Held := False;
                  when 3 => Mutation.Change_Revision (Source);
                  when 4 =>
                     Insert.Finish_Prepare (State, Source, Nested);
                     pragma Assert (not Nested);
                  when 5 => return 4096;
                  when others => null;
               end case;
            end if;
            return 16#200000# + Unsigned_64 (Ordinal - 1) * 4096;
         end Data_Page;
         procedure Capture is new Insert.Capture_Step (Data_Page);
      begin
         VM.Initialize (Source, [4096, 8192, 12288, 16384, others => 0], OK,
           Backing_Count => 4); pragma Assert (OK);
         VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK);
         pragma Assert (OK);
         VM.Seal (Source, OK); pragma Assert (OK);
         Epoch := VM.Revision (Source);
         Insert.Begin_Prepare (State, Source, Epoch, 8192, 65,
           Write_Back, Read_Write, OK);
         pragma Assert (OK and Insert.Preparing (State) and Calls = 0);
         Insert.Finish_Prepare (State, Source, OK);
         pragma Assert (not OK and Insert.Preparing (State));
         Capture (State, Source, OK);
         pragma Assert (OK and Calls = 32 and not Insert.Captured (State));
         -- Duplicate begin must not reset the captured prefix.
         Insert.Begin_Prepare (State, Source, Epoch, 8192, 1,
           Write_Back, Read_Write, OK);
         pragma Assert (not OK);
         Capture (State, Source, OK);
         if Fault in 1 .. 4 then
            pragma Assert (not OK and Calls = 33 and not Insert.Preparing (State));
         else
            pragma Assert (OK and Calls = 64 and not Insert.Captured (State));
            Capture (State, Source, OK);
            pragma Assert (OK and Calls = 65 and Insert.Captured (State));
            Capture (State, Source, OK);
            pragma Assert (OK and Calls = 65); -- idempotent completed capture
            while Insert.Preparing (State) loop
               Validation_Turns := Validation_Turns + 1;
               pragma Assert (Validation_Turns <= 200);
               if Validation_Turns = 2 then
                  if Fault = 6 then Held := False; end if;
                  if Fault = 7 then Mutation.Change_Revision (Source); end if;
               end if;
               Insert.Finish_Prepare (State, Source, OK);
               pragma Assert (Writes = 0 and Calls = 65);
               if Insert.Preparing (State) then
                  pragma Assert (not Insert.Publishing (State));
               end if;
               exit when not OK;
            end loop;
            pragma Assert (OK = (Fault = 0));
            if Fault = 0 then pragma Assert (Validation_Turns > 64); end if;
            if Fault in 6 .. 7 then pragma Assert (Validation_Turns = 2); end if;
         end if;
         pragma Assert (Writes = 0 and VM.Lookup (Source, 8192) = 0);
         if Fault = 0 then
            while Insert.Publishing (State) loop Insert.Step (State, Source); end loop;
            pragma Assert (Writes = 65 and Calls = 65 and Insert.Published (State));
            Insert.Commit (State, Source, True, OK);
            pragma Assert (OK and VM.Revision (Source) = Epoch + 1);
         else
            pragma Assert (not Insert.Publishing (State) and not Insert.Published (State));
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Capture/validation steps PASS8: 32/32/1 pages, multi-turn validation, no early writes, invalid DMA/owner/revision/reentry/table alias and inter-turn loss rejected (mock GPU)");
end VM_Capture_Step_Tests;
