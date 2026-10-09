with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Insertion;
with Intel_GPU_VM_Update;
with Intel_GPU_Table_Preflight;
procedure VM_Commit_Integration_Tests is
   package Image is new Intel_GPU_VM_Image (8);
begin
   for Fault in 0 .. 4 loop
      declare
         Source : Image.Image;
         Held, Invalidated, Started : Boolean := False;
         Live : Boolean := True;
         OK, Done : Boolean;
         Epoch : Unsigned_64;
         Writes, Turns, Resume_Turns, Prepare_Turns : Natural := 0;
         Table_Calls : Natural := 0;
         function Owner return Boolean is (Live);
         function Exclusive return Boolean is (Held and Live);
         function Valid_Table (Ordinal : Positive) return Boolean is
         begin
            Table_Calls := Table_Calls + 1;
            pragma Assert (Ordinal = Table_Calls and Held and Live and Writes = 0);
            return not (Fault = 4 and Ordinal = 33);
         end Valid_Table;
         package Preflight is new Intel_GPU_Table_Preflight (Exclusive, Valid_Table);
         use type Preflight.Phase;
         Tables : Preflight.Controller;
         procedure Write_Leaf
           (Table_DMA : Unsigned_64; Index : Table_Index;
            Expected, Replacement : Unsigned_64; Success : out Boolean) is
         begin
            pragma Assert (Table_DMA = 16384 and Index in 2 .. 4);
            pragma Assert (Expected = 0 and Replacement /= 0 and not Invalidated);
            pragma Assert (Table_Calls = 65 and Preflight.Status (Tables) = Preflight.Complete);
            Writes := Writes + 1; Success := True;
         end Write_Leaf;
         procedure Invalidate (Success : out Boolean) is
         begin
            pragma Assert (Writes = 3);
            Success := Fault /= 1; Invalidated := Success;
         end Invalidate;
         package Insert is new Image.Insertion (Exclusive, Write_Leaf, Invalidate);
         Receipt : Insert.Controller;
         function Data_Page (Ordinal : Positive) return Unsigned_64 is
           (16#200000# + Unsigned_64 (Ordinal - 1) * 4096);
         procedure Capture is new Insert.Capture_Step (Data_Page);
         procedure Drain (Success : out Boolean) is
         begin Held := True; Success := True; end Drain;
         procedure Unused (Success : out Boolean) is
         begin pragma Assert (False); Success := False; end Unused;
         package Update is new Intel_GPU_VM_Update (Owner, Drain, Unused, Invalidate, Unused);
         use type Update.Result, Update.Phase;
         State : Update.State;
         Result : Update.Result;
         procedure Publication (Finished, Success : out Boolean) is
         begin
            if not Started then
               if Preflight.Status (Tables) = Preflight.Idle then
                  Preflight.Start (Tables, 65, Success);
                  pragma Assert (Success);
               end if;
               Preflight.Step (Tables);
               if Preflight.Status (Tables) = Preflight.Running then
                  Finished := False; Success := True; return;
               elsif Preflight.Status (Tables) /= Preflight.Complete then
                  Finished := True; Success := False; return;
               end if;
               Started := True;
               Insert.Begin_Prepare (Receipt, Source, Epoch, 8192,
                 3, Write_Back, Read_Write, Success);
               Finished := not Success;
            elsif Insert.Preparing (Receipt) then
               Prepare_Turns := Prepare_Turns + 1;
               pragma Assert (Writes = 0 and not Invalidated and not Update.Can_Submit (State));
               if Insert.Captured (Receipt) then
                  Insert.Finish_Prepare (Receipt, Source, Success);
               else
                  Capture (Receipt, Source, Success);
               end if;
               Finished := not Success;
            else
               Insert.Step (Receipt, Source);
               Finished := not Insert.Publishing (Receipt);
               Success := not Finished or else Insert.Published (Receipt);
            end if;
         end Publication;
         procedure Resume (Finished, Success : out Boolean) is
            Committed : Boolean;
         begin
            Resume_Turns := Resume_Turns + 1;
            pragma Assert (Held and Invalidated and not Update.Can_Submit (State));
            if not Insert.Committing (Receipt) then
               Insert.Begin_Commit (Receipt, Source, Invalidated, Success);
               Finished := not Success;
            else
               Insert.Commit_Step (Receipt, Source, Committed);
               Finished := not Insert.Committing (Receipt);
               Success := not Finished or else Committed;
            end if;
         end Resume;
         procedure Advance is new Update.Advance (Publication, Resume);
      begin
         Image.Initialize (Source, [4096, 8192, 12288, 16384, others => 0], OK,
           Backing_Count => 4); pragma Assert (OK);
         Image.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK);
         pragma Assert (OK);
         Image.Seal (Source, OK); pragma Assert (OK);
         Epoch := Image.Revision (Source);
         Update.Begin_Update (State, 0, OK, Result); pragma Assert (OK);
         loop
            pragma Assert (not Update.Can_Submit (State));
            Turns := Turns + 1; pragma Assert (Turns <= 100);
            Advance (State, Done, Result);
            exit when Done;
            pragma Assert (Update.Generation (State) = 0);
            if Resume_Turns /= 0 then
               pragma Assert (Image.Root_DMA (Source) = 0);
               pragma Assert (Image.Lookup (Source, 8192) = 0);
            end if;
            if Fault = 2 and Resume_Turns = 2 then Live := False; end if;
            if Fault = 3 and Prepare_Turns = 2 then Live := False; end if;
         end loop;
         if Fault = 0 then
            pragma Assert (Prepare_Turns > 64);
            pragma Assert (Result = Update.Complete and Resume_Turns = 4);
            pragma Assert (Update.Can_Submit (State) and Update.Generation (State) = 1);
            pragma Assert (Image.Revision (Source) = Epoch + 1);
            pragma Assert (Image.Lookup (Source, 16384) = Encode_Leaf
              (16#202000#, Write_Back, Read_Write));
         else
            pragma Assert (Update.Current_Phase (State) = Update.Quarantined);
            pragma Assert (not Update.Can_Submit (State) and Update.Generation (State) = 0);
            if Fault = 2 then pragma Assert (Image.Root_DMA (Source) = 0); end if;
            if Fault = 3 then pragma Assert (Writes = 0 and not Invalidated); end if;
            if Fault = 4 then pragma Assert (Writes = 0 and not Invalidated and Table_Calls = 33); end if;
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("combined preflight/commit/update PASS5: bounded table sweep, multi-turn validation, invalidation before commit, hidden partial image, closed admission, failed table/preflight/resume quarantine (mock GPU)");
end VM_Commit_Integration_Tests;
