with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Growth;
with Intel_GPU_VM_Image.Growth.Backing;
with Intel_GPU_VM_Image.Growth.Backing.Writer;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
procedure VM_Growth_Capacity_Tests is
   package VM is new Intel_GPU_VM_Image
     (8, Bootstrap_Tables => 4, Bootstrap_Descriptors => 4);
   package G is new VM.Growth;
begin
   for Mode in 0 .. 3 loop
      declare
         Source : VM.Image;
         Mirrors : array (1 .. 16384) of Unsigned_8 := [others => 0] with Alignment => 4096;
         Descriptors : array (1 .. 4096) of Unsigned_8 := [others => 0] with Alignment => 4096;
         type Words is array (Table_Index) of Unsigned_64;
         RAM : array (1 .. 7) of Words := [others => [others => 0]];
         Calls : Natural := 0;
         OK : Boolean;
         Epoch : Unsigned_64;
         function Exclusive return Boolean is (True);
         function Owned (DMA : Unsigned_64) return Boolean is
           (DMA in 4096 .. 7 * 4096 and then DMA mod 4096 = 0);
         procedure Read_Word (DMA : Unsigned_64; Index : Table_Index;
           Value : out Unsigned_64; Accepted : out Boolean) is
         begin
            Calls := Calls + 1; Value := RAM (Positive (DMA / 4096)) (Index); Accepted := True;
         end;
         procedure Write_Word (DMA : Unsigned_64; Index : Table_Index;
           Value : Unsigned_64; Accepted : out Boolean) is
         begin
            Calls := Calls + 1; RAM (Positive (DMA / 4096)) (Index) := Value; Accepted := True;
         end;
         function Flush (DMA : Unsigned_64) return Boolean is (Owned (DMA));
         package B is new G.Backing (Owned);
         package W is new B.Writer (Exclusive, Read_Word, Write_Word, Flush, Exclusive);
         State : W.State;
      begin
         VM.Initialize (Source, [4096, 8192, 12288, 16384, others => 0], OK,
           Backing_Count => 4); pragma Assert (OK);
         VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK); pragma Assert (OK);
         VM.Seal (Source, OK); pragma Assert (OK);
         if Mode in 1 | 3 then
            VM.Extend_Metadata (Source, Unsigned_64 (To_Integer (Mirrors'Address)), Mirrors'Length, OK);
            pragma Assert (OK);
         end if;
         if Mode in 2 | 3 then
            VM.Extend_Descriptors (Source, Unsigned_64 (To_Integer (Descriptors'Address)), Descriptors'Length, OK);
            pragma Assert (OK);
         end if;
         pragma Assert (VM.Mirror_Capacity (Source) = (if Mode in 1 | 3 then 8 else 4));
         pragma Assert (VM.Descriptor_Capacity (Source) = (if Mode in 2 | 3 then 8 else 4));
         pragma Assert (VM.Metadata_Capacity (Source) = (if Mode = 3 then 8 else 4));
         pragma Assert (G.Inspect (Source, 2 ** 39, 4096).Additional_Tables = 3);
         pragma Assert (G.Inspect (Source, 2 ** 39, 4096).Required_Tables = 7);
         pragma Assert (G.Inspect (Source, 2 ** 39, 4096).Fits_Quota);
         pragma Assert (G.Inspect (Source, 2 ** 39, 4096).Fits_Reserved = (Mode = 3));
         -- Valid topology but beyond the configured image quota is different
         -- from metadata that could grow. Neither may publish prematurely.
         pragma Assert (G.Inspect (Source, 2 ** 39, 8 * 2 ** 21).Required_Tables = 14);
         pragma Assert (not G.Inspect (Source, 2 ** 39, 8 * 2 ** 21).Fits_Quota);
         pragma Assert (not G.Inspect (Source, 2 ** 39, 8 * 2 ** 21).Fits_Reserved);
         Epoch := VM.Revision (Source);
         W.Start (State, Source, 2 ** 39, 4096, 4096, [20480, 24576, 28672], OK);
         pragma Assert (Calls = 0 and OK = (Mode = 3));
         if OK then
            while W.Pending (State) loop W.Step (State, Source); end loop;
            pragma Assert (W.Published (State));
            W.Commit (State, Source, OK);
            pragma Assert (OK and VM.Used (Source) = 7 and VM.Revision (Source) = Epoch + 1);
         else
            pragma Assert (not W.Pending (State) and VM.Used (Source) = 4);
            pragma Assert (VM.Revision (Source) = Epoch and VM.Table_Backing_DMA (Source, 5) = 0);
            pragma Assert (not W.Attempted (State));
            -- Capacity admission is retryable: grow only the missing stores,
            -- retaining the exact same image and transaction identity.
            if Mode in 0 | 2 then
               VM.Extend_Metadata (Source, Unsigned_64 (To_Integer (Mirrors'Address)), Mirrors'Length, OK);
               pragma Assert (OK);
            end if;
            if Mode in 0 | 1 then
               VM.Extend_Descriptors (Source, Unsigned_64 (To_Integer (Descriptors'Address)), Descriptors'Length, OK);
               pragma Assert (OK);
            end if;
            pragma Assert (Calls = 0 and VM.Revision (Source) = Epoch);
            pragma Assert (VM.Mirror_Capacity (Source) = 8 and VM.Descriptor_Capacity (Source) = 8);
            W.Start (State, Source, 2 ** 39, 4096, 4096, [20480, 24576, 28672], OK);
            pragma Assert (OK and W.Attempted (State) and Calls = 0);
            while W.Pending (State) loop W.Step (State, Source); end loop;
            pragma Assert (W.Published (State));
            W.Commit (State, Source, OK);
            pragma Assert (OK and W.Committed (State));
            pragma Assert (VM.Used (Source) = 7 and VM.Revision (Source) = Epoch + 1);
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Directory capacity PASS4 plus 3 same-transaction growth retries: source mirrors AND descriptors required before publication");
end VM_Growth_Capacity_Tests;
