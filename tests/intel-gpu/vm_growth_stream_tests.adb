with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Growth;
with Intel_GPU_VM_Image.Growth.Backing;
with Intel_GPU_VM_Image.Growth.Backing.Writer;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
procedure VM_Growth_Stream_Tests is
   package VM is new Intel_GPU_VM_Image (8);
   package G is new VM.Growth;
begin
   for Fault in 0 .. 3 loop
      declare
         Source : VM.Image;
         Held, Mutated : Boolean := False;
         Reads, IO_Count : Natural := 0;
         OK : Boolean;
         Epoch : Unsigned_64;
         type Words is array (Table_Index) of Unsigned_64;
         RAM : array (1 .. 7) of Words := [others => [others => 0]];
         function Exclusive return Boolean is (Held);
         function Owned (DMA : Unsigned_64) return Boolean is
           (DMA in 4096 .. 7 * 4096 and then DMA mod 4096 = 0);
         procedure Read_Word (DMA : Unsigned_64; Index : Table_Index;
           Value : out Unsigned_64; Accepted : out Boolean) is
         begin
            IO_Count := IO_Count + 1;
            Value := RAM (Positive (DMA / 4096)) (Index); Accepted := Held;
         end;
         procedure Write_Word (DMA : Unsigned_64; Index : Table_Index;
           Value : Unsigned_64; Accepted : out Boolean) is
         begin
            IO_Count := IO_Count + 1;
            RAM (Positive (DMA / 4096)) (Index) := Value; Accepted := Held;
         end;
         function Flush (DMA : Unsigned_64) return Boolean is (Held and Owned (DMA));
         function Invalidated return Boolean is (True);
         package B is new G.Backing (Owned);
         package W is new B.Writer (Exclusive, Read_Word, Write_Word, Flush, Invalidated);
         State : W.State;
         function Page (Ordinal : Positive) return Unsigned_64 is
            Accepted : Boolean;
         begin
            Reads := Reads + 1;
            pragma Assert (Ordinal = Reads and not Mutated and IO_Count = 0);
            if Ordinal = 3 then
               case Fault is
                  when 1 => Held := False;
                  when 2 => return 5 * 4096;
                  when 3 => W.Commit (State, Source, Accepted); pragma Assert (not Accepted);
                  when others => null;
               end case;
            end if;
            return Unsigned_64 (Ordinal + 4) * 4096;
         end Page;
         procedure Start is new W.Start_From_Pages (Page);
      begin
         VM.Initialize (Source, [4096, 8192, 12288, 16384, others => 0], OK,
           Backing_Count => 4); pragma Assert (OK);
         VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK);
         pragma Assert (OK);
         VM.Seal (Source, OK); pragma Assert (OK);
         Epoch := VM.Revision (Source); Held := True;
         Start (State, Source, 2 ** 39, 4096, 4096, 3, OK);
         pragma Assert (Reads = 3 and IO_Count = 0 and OK = (Fault = 0));
         Mutated := True;
         if OK then
            while W.Pending (State) loop W.Step (State, Source); end loop;
            pragma Assert (W.Published (State) and Reads = 3);
            W.Commit (State, Source, OK);
            pragma Assert (OK and VM.Used (Source) = 7 and VM.Revision (Source) = Epoch + 1);
            pragma Assert (VM.Lookup (Source, 4096) = Encode_Leaf (16#100000#, Write_Back, Read_Write));
         else
            pragma Assert (not W.Pending (State) and not W.Published (State));
            pragma Assert (VM.Revision (Source) = Epoch and VM.Used (Source) = 4);
            Held := True;
            Start (State, Source, 2 ** 39, 4096, 4096, 3, OK);
            pragma Assert (not OK and Reads = 3 and IO_Count = 0);
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Directory stream PASS4: read-once input, retained commit, callback owner/reentry/duplicate rejection before IO (mock GPU)");
end VM_Growth_Stream_Tests;
