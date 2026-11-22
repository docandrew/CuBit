with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Removal;
procedure VM_Removal_Stream_Tests is
   package VM is new Intel_GPU_VM_Image (8);
begin
   for Stepped in Boolean loop
   for Fault in 0 .. (if Stepped then 8 else 5) loop
      declare
         Source : VM.Image;
         Held : Boolean := True;
         Calls, Writes : Natural := 0;
         Epoch : Unsigned_64;
         OK : Boolean;
         function Exclusive return Boolean is (Held);
         procedure Write_Leaf
           (Table_DMA : Unsigned_64; Index : Table_Index;
            Expected, Replacement : Unsigned_64; Success : out Boolean) is
         begin
            pragma Assert (Calls = 3 and Table_DMA = 16384);
            Writes := Writes + 1;
            pragma Assert (Index = Table_Index (Writes));
            pragma Assert (Expected = Encode_Leaf
              (16#200000# + Unsigned_64 (Writes - 1) * 4096, Write_Back, Read_Write));
            pragma Assert (Replacement = 0);
            Success := True;
         end Write_Leaf;
         procedure Invalidate (Success : out Boolean) is
         begin Success := True; end Invalidate;
         package Remove is new VM.Removal (Exclusive, Write_Leaf, Invalidate);
         State : Remove.Controller;
         function Expected_Page (Ordinal : Positive) return Unsigned_64 is
            Nested : Boolean;
         begin
            Calls := Calls + 1;
            pragma Assert (Writes = 0 and Ordinal = Calls);
            if Ordinal = 3 then
               case Fault is
                  when 1 => return 0;
                  when 2 => Held := False;
                  when 3 => return 16#BAD000#;
                  when 4 =>
                     Remove.Start (State, Source, Epoch, 4096, [16#200000#], Nested);
                     pragma Assert (not Nested);
                  when 5 =>
                     Remove.Commit (State, Source, True, Nested);
                     pragma Assert (not Nested);
                  when others => null;
               end case;
            end if;
            return 16#200000# + Unsigned_64 (Ordinal - 1) * 4096;
         end Expected_Page;
         procedure Start is new Remove.Start_From_Pages (Expected_Page);
         procedure Capture is new Remove.Capture_Step (Expected_Page);
      begin
         VM.Initialize (Source, [4096, 8192, 12288, 16384, others => 0], OK,
           Backing_Count => 4); pragma Assert (OK);
         VM.Map_Pages (Source, 4096, [16#200000#, 16#201000#, 16#202000#],
           Write_Back, Read_Write, OK); pragma Assert (OK);
         VM.Seal (Source, OK); pragma Assert (OK);
         Epoch := VM.Revision (Source);
         declare
            High_State : Remove.Controller;
            High_Data : constant VM.Data_Pages (Positive'Last .. Positive'Last) :=
              [others => 16#200000#];
         begin
            Remove.Start (High_State, Source, Epoch, 4096, High_Data, OK);
            pragma Assert (OK and Writes = 0);
            Remove.Commit (High_State, Source, False, OK);
            pragma Assert (not OK and Writes = 0);
         end;
         Start (State, Source, Epoch, 4096, 0, OK);
         pragma Assert (not OK and Calls = 0 and Writes = 0);
         if Stepped then
            Remove.Begin_Prepare (State, Source, Epoch, 4096, 3, OK);
            pragma Assert (OK and Calls = 0 and Writes = 0 and Remove.Preparing (State));
            declare
               Duplicate : Boolean;
               Before : Natural;
            begin
               Remove.Begin_Prepare (State, Source, Epoch, 4096, 1, Duplicate);
               pragma Assert (not Duplicate and Remove.Preparing (State));
               while Remove.Preparing (State) loop
                  Before := Calls;
                  if Calls = 1 then
                     case Fault is
                        when 6 => Held := False;
                        when 7 => Remove.Cancel_Prepare (State);
                        when 8 =>
                           Remove.Commit (State, Source, True, Duplicate);
                           pragma Assert (not Duplicate);
                        when others => null;
                     end case;
                  end if;
                  Capture (State, Source, OK);
                  pragma Assert (Calls <= Before + 1 and Writes = 0);
                  pragma Assert (VM.Revision (Source) = Epoch);
                  if Calls < 3 then pragma Assert (not Remove.Publishing (State)); end if;
                  exit when not OK;
               end loop;
            end;
         else
            Start (State, Source, Epoch, 4096, 3, OK);
         end if;
         pragma Assert (OK = (Fault = 0));
         pragma Assert (Calls = (if Fault >= 6 then 1 else 3));
         pragma Assert (Writes = 0 and VM.Revision (Source) = Epoch);
         if OK then
            while Remove.Publishing (State) loop Remove.Step (State, Source); end loop;
            pragma Assert (Writes = 3 and Remove.Published (State));
            Remove.Commit (State, Source, True, OK);
            pragma Assert (OK and VM.Revision (Source) = Epoch + 1);
            pragma Assert (VM.Lookup (Source, 4096) = 0 and VM.Lookup (Source, 12288) = 0);
         else
            pragma Assert (not Remove.Publishing (State) and not Remove.Published (State));
            pragma Assert (VM.Lookup (Source, 12288) /= 0);
            pragma Assert (Remove.Failed (State) = (Fault in 4 .. 5 | 7 .. 8));
            declare
               Before : constant Natural := Calls;
            begin
               Capture (State, Source, OK);
               pragma Assert (not OK and Calls = Before and Writes = 0);
            end;
         end if;
      end;
   end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Removal streaming PASS15: synchronous/one-page capture, no early writes, callback and between-turn owner loss, cancellation/premature commit, mismatch/reentry (mock GPU)");
end VM_Removal_Stream_Tests;
