with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Insertion;
procedure VM_Insertion_Stream_Tests is
   package VM is new Intel_GPU_VM_Image (8);
begin
   for Fault in 0 .. 7 loop
      declare
         Source : VM.Image;
         Held, Mutated : Boolean := True;
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
            pragma Assert (Index = Table_Index (Writes + 1));
            pragma Assert (Expected = 0 and Replacement = Encode_Leaf
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
            pragma Assert (Writes = 0 and Ordinal = Calls);
            if Mutated then return 16#BAD000#; end if;
            if Ordinal = 3 then
               case Fault is
                  when 1 => return 0;
                  when 2 => Held := False;
                  when 3 => return 16#200001#;
                  when 4 =>
                     Insert.Start (State, Source, Epoch, 8192, [16#200000#],
                       Write_Back, Read_Write, Nested);
                     pragma Assert (not Nested);
                  when 5 =>
                     Insert.Commit (State, Source, True, Nested);
                     pragma Assert (not Nested);
                  when 6 => return 4096; -- retained table alias
                  when 7 => return 16#300000#; -- conflicting cache alias
                  when others => null;
               end case;
            end if;
            return 16#200000# + Unsigned_64 (Ordinal - 1) * 4096;
         end Data_Page;
         procedure Start is new Insert.Start_From_Pages (Data_Page);
      begin
         VM.Initialize (Source, [4096, 8192, 12288, 16384, others => 0], OK,
           Backing_Count => 4); pragma Assert (OK);
         VM.Map_Page (Source, 4096, 16#300000#, Write_Through, Read_Write, OK);
         pragma Assert (OK);
         VM.Seal (Source, OK); pragma Assert (OK);
         Epoch := VM.Revision (Source);
         declare
            High_State : Insert.Controller;
            High_Data : constant VM.Data_Pages (Positive'Last .. Positive'Last) :=
              [others => 16#200000#];
         begin
            Insert.Start (High_State, Source, Epoch, 8192, High_Data,
              Write_Back, Read_Write, OK);
            pragma Assert (OK and Writes = 0);
            Insert.Commit (High_State, Source, False, OK);
            pragma Assert (not OK);
         end;
         Mutated := False;
         Start (State, Source, Epoch, 8192, 0, Write_Back, Read_Write, OK);
         pragma Assert (not OK and Calls = 0 and Writes = 0);
         Start (State, Source, Epoch, 8192, 3, Write_Back, Read_Write, OK);
         pragma Assert (OK = (Fault = 0));
         pragma Assert (Calls = 3 and Writes = 0 and VM.Revision (Source) = Epoch);
         Mutated := True; -- no callback or borrowed input after admission
         if OK then
            while Insert.Publishing (State) loop Insert.Step (State, Source); end loop;
            pragma Assert (Writes = 3 and Calls = 3 and Insert.Published (State));
            Insert.Commit (State, Source, True, OK);
            pragma Assert (OK and VM.Revision (Source) = Epoch + 1);
            pragma Assert (VM.Lookup (Source, 8192) = Encode_Leaf (16#200000#, Write_Back, Read_Write));
         else
            pragma Assert (not Insert.Publishing (State) and not Insert.Published (State));
            pragma Assert (VM.Lookup (Source, 8192) = 0);
            pragma Assert (Insert.Failed (State) = (Fault in 4 .. 5));
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Insertion streaming PASS8: exact retained words, one input read/page, owner/reentry/encoding/table/cache rejection before GPU writes (mock GPU)");
end VM_Insertion_Stream_Tests;
