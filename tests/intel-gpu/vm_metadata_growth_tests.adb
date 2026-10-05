with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Snapshots;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
procedure VM_Metadata_Growth_Tests is
   package VM is new Intel_GPU_VM_Image (132, 4);
   package Snapshots is new VM.Snapshots;
   Source, Candidate : VM.Image;
   DMA, Other_DMA : VM.Backing_Pages;
   type Words is array (1 .. 128 * 512) of Unsigned_64;
   RAM, Other_RAM : Words := [others => 16#DEAD#] with Alignment => 4096;
   Base : constant Unsigned_64 := Unsigned_64 (To_Integer (RAM'Address));
   Other : constant Unsigned_64 := Unsigned_64 (To_Integer (Other_RAM'Address));
   OK : Boolean;
   Epoch : Unsigned_64;
   procedure Grow (Object : in out VM.Image; Address : Unsigned_64) is
      Before : constant Unsigned_64 := VM.Revision (Object);
   begin
      VM.Extend_Metadata (Object, Address,
        Unsigned_64 (VM.Metadata_Capacity (Object) - 4 + 16) * 4096, OK);
      pragma Assert (OK and VM.Revision (Object) = Before);
   end Grow;
begin
   pragma Assert (VM.Metadata_Capacity (Source) = 4);
   -- Metadata quota no longer embeds 132 complete pages in every Image.
   pragma Assert (VM.Image'Object_Size < 32 * 4096 * 8);
   for P in DMA'Range loop
      DMA (P) := Unsigned_64 (P) * 4096;
      Other_DMA (P) := 16#400000# + Unsigned_64 (P) * 4096;
   end loop;
   VM.Initialize (Source, DMA, OK); pragma Assert (OK);
   for N in 0 .. 95 loop
      declare
         GPU : constant Unsigned_64 := 4096 + Unsigned_64 (N) * 2 ** 21;
         Data : constant Unsigned_64 := 16#100000# + Unsigned_64 (N) * 4096;
         Before : constant Natural := VM.Used (Source);
      begin
         VM.Map_Page (Source, GPU, Data, Write_Back, Read_Write, OK);
         if not OK then
            pragma Assert (VM.Used (Source) = Before and VM.Lookup (Source, GPU) = 0);
            Grow (Source, Base);
            VM.Map_Page (Source, GPU, Data, Write_Back, Read_Write, OK);
         end if;
         pragma Assert (OK);
         for Old in 0 .. N loop
            pragma Assert (VM.Lookup (Source, 4096 + Unsigned_64 (Old) * 2 ** 21) =
              Encode_Leaf (16#100000# + Unsigned_64 (Old) * 4096, Write_Back, Read_Write));
         end loop;
      end;
   end loop;
   pragma Assert (VM.Used (Source) = 99 and VM.Metadata_Capacity (Source) = 100);
   VM.Seal (Source, OK); pragma Assert (OK);
   Epoch := VM.Revision (Source);
   VM.Prepare_Update (Candidate, Source, Other_DMA, OK);
   pragma Assert (not OK and VM.Revision (Candidate) = 0);
   while VM.Metadata_Capacity (Candidate) < VM.Used (Source) loop Grow (Candidate, Other); end loop;
   VM.Prepare_Update (Candidate, Source, Other_DMA, OK); pragma Assert (OK);
   VM.Unmap_Pages (Candidate, 4096, [16#100000#], OK); pragma Assert (OK);
   VM.Seal_Update (Candidate, OK); pragma Assert (OK);
   pragma Assert (VM.Lookup (Source, 4096) /= 0 and VM.Lookup (Candidate, 4096) = 0);
   Snapshots.Adopt_Committed (Source, Candidate, OK);
   pragma Assert (OK and VM.Revision (Source) = Epoch + 1 and VM.Lookup (Source, 4096) = 0);
   Snapshots.Forget_Retired (Source, VM.Revision (Source), VM.Root_DMA (Source), True, OK);
   pragma Assert (OK and VM.Metadata_Capacity (Source) = 100);
   -- Clearing/reusing one mirror never clears its independent candidate.
   pragma Assert (VM.Lookup (Candidate, 4096 + 2 ** 21) /= 0);
   VM.Initialize (Source, DMA, OK); pragma Assert (OK);
   VM.Map_Page (Source, 4096, 16#180000#, Write_Back, Read_Write, OK);
   pragma Assert (OK and VM.Lookup (Source, 4096 + 2 ** 21) = 0);
   Ada.Text_IO.Put_Line ("VM metadata growth PASS: 4->100 committed mirrors, 99 used tables/96 sparse mappings; copy/adopt/retire independent (host RAM, not native GPU)");
end VM_Metadata_Growth_Tests;
