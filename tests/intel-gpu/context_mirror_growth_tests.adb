with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_VM_Image;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Metadata_Initialize;
with Intel_GPU_Record_Growth;
procedure Context_Mirror_Growth_Tests is
   package VM is new Intel_GPU_VM_Image (64, 4);
   Bytes : constant Unsigned_64 := (64 - 4) * 4096;
   type RAM is array (1 .. Natural (Bytes)) of Unsigned_8;
begin
   for Fault in 0 .. 4 loop
      declare
         Source : VM.Image;
         DMA : VM.Backing_Pages;
         Memory : RAM := [others => 16#AB#] with Alignment => 4096;
         Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Memory'Address));
         Commits, Turns : Natural := 0;
         Last_Bytes : Unsigned_64 := 0;
         function Reserve (Requested : Unsigned_64) return Unsigned_64 is
         begin pragma Assert (Requested = Bytes); return Base; end Reserve;
         function Commit (Address, Offset, Count : Unsigned_64) return Boolean is
         begin
            pragma Assert (Address = Base and Count <= 65536 and Offset + Count <= Bytes);
            Commits := Commits + 1; Last_Bytes := Count;
            return Commits /= Fault;
         end Commit;
         package Storage is new Intel_GPU_Metadata_Arena
           (Reserve, Commit, Intel_GPU_Metadata_Initialize.Clear);
         function Capacity return Positive is (VM.Metadata_Capacity (Source));
         procedure Publish (Address, Count : Unsigned_64; OK : out Boolean) is
         begin VM.Extend_Metadata (Source, Address, Count, OK); end Publish;
         package Growth is new Intel_GPU_Record_Growth (Storage, Capacity, Publish);
         use type Growth.Phase;
         Controller : Growth.Controller;
         OK : Boolean;
         Previous : Natural;
         Epoch : Unsigned_64;
      begin
         for P in DMA'Range loop DMA (P) := Unsigned_64 (P) * 4096; end loop;
         VM.Initialize (Source, DMA, OK); pragma Assert (OK);
         VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK); pragma Assert (OK);
         Epoch := VM.Revision (Source);
         Growth.Configure (Controller, Bytes, 64, OK); pragma Assert (OK);
         Growth.Request (Controller, 64, OK); pragma Assert (OK);
         while Growth.Snapshot (Controller).State not in Growth.Idle | Growth.Failed loop
            Turns := Turns + 1; pragma Assert (Turns <= 20);
            Previous := Commits;
            Growth.Step (Controller);
            pragma Assert (Commits <= Previous + 1 and VM.Revision (Source) = Epoch);
            pragma Assert (VM.Lookup (Source, 4096) = Encode_Leaf (16#100000#, Write_Back, Read_Write));
         end loop;
         if Fault = 0 then
            pragma Assert (Capacity = 64 and Commits = 4 and Last_Bytes = 12 * 4096);
         else
            pragma Assert (Growth.Snapshot (Controller).State = Growth.Failed and Capacity < 64);
            Previous := Commits;
            Growth.Request (Controller, 64, OK); pragma Assert (not OK);
            Growth.Step (Controller); pragma Assert (Commits = Previous);
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Context mirror growth PASS5: exact non-multiple64KiB quota, four commits, unchanged live mirror words/epoch, each commit failure retains prefix (host RAM)");
end Context_Mirror_Growth_Tests;
