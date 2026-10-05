with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_VM_Image;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
procedure VM_Sparse_Scale_Tests is
   package VM is new Intel_GPU_VM_Image
     (4096, Bootstrap_Tables => 4, Bootstrap_Descriptors => 4,
      Bootstrap_Insertion_Words => 512, Bootstrap_Growth_Links => 2);
   Source : VM.Image;
   type Bytes is array (Positive range <>) of Unsigned_8;
   Descriptors : aliased Bytes (1 .. 69632) := [others => 16#AC#]
     with Alignment => 4096;
   Mirrors : aliased Bytes (1 .. 125 * 4096) := [others => 16#BD#]
     with Alignment => 4096;
   DMA : VM.Backing_Pages := [1 => 4096, 2 => 8192, 3 => 12288,
                              4 => 16384, others => 0];
   High : constant Unsigned_64 := 2 ** 47 + 4096;
   OK : Boolean;
   First : Natural;
   Epoch : Unsigned_64;
   Published : Unsigned_64 := 16384;
   Extra : VM.Data_Pages (1 .. 120);
begin
   -- Metadata object size must depend on the bootstrap, not the table quota.
   pragma Assert (Source'Size / 8 < 65536);
   pragma Assert (VM.Metadata_Capacity (Source) = 4);
   VM.Initialize (Source, DMA, OK, Backing_Count => 4);
   pragma Assert (OK and VM.Backed_Tables (Source) = 4);
   VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK);
   pragma Assert (OK and VM.Used (Source) = 4);
   Epoch := VM.Revision (Source);
   VM.Map_Page (Source, High, 16#200000#, Write_Back, Read_Write, OK);
   pragma Assert (not OK and VM.Used (Source) = 4 and VM.Revision (Source) = Epoch);
   pragma Assert (VM.Lookup (Source, High) = 0);
   -- Descriptor growth alone neither commits table mirrors nor grants DMA.
   pragma Assert (VM.Descriptor_Metadata_Bytes <= 65536);
   VM.Extend_Descriptors (Source, Unsigned_64 (To_Integer (Descriptors'Address)),
                         VM.Descriptor_Metadata_Bytes, OK);
   pragma Assert (OK and VM.Descriptor_Capacity (Source) = 4096);
   pragma Assert (VM.Metadata_Capacity (Source) = 4 and VM.Backed_Tables (Source) = 4);
   VM.Append_Offline_Backing (Source, [20480, 24576, 28672, 32768], First, OK);
   pragma Assert (not OK and First = 0 and VM.Revision (Source) = Epoch);
   VM.Extend_Metadata (Source, Unsigned_64 (To_Integer (Mirrors'Address)), 16384, OK);
   pragma Assert (OK and VM.Metadata_Capacity (Source) = 8);
   pragma Assert (VM.Backed_Tables (Source) = 4 and VM.Revision (Source) = Epoch);
   -- Still no physical backing for the new branch, despite both CPU stores.
   VM.Map_Page (Source, High, 16#200000#, Write_Back, Read_Write, OK);
   pragma Assert (not OK and VM.Used (Source) = 4 and VM.Revision (Source) = Epoch);
   VM.Append_Offline_Backing (Source, [20480, 24576, 28672, 32768], First, OK);
   pragma Assert (OK and First = 5 and VM.Backed_Tables (Source) = 8);
   VM.Map_Page (Source, High, 16#200000#, Write_Back, Read_Write, OK);
   pragma Assert (OK and VM.Used (Source) = 7 and VM.Backed_Tables (Source) = 8);
   pragma Assert (VM.Lookup (Source, 4096) = Encode_Leaf (16#100000#, Write_Back, Read_Write));
   pragma Assert (VM.Lookup (Source, High) = Encode_Leaf (16#200000#, Write_Back, Read_Write));
   pragma Assert (VM.Lookup (Source, High + 4096) = 0);
   pragma Assert (not VM.DMA_Disjoint (Source, 32768, 4096));
   pragma Assert (for all I in Natural (VM.Descriptor_Metadata_Bytes) + 1 .. Descriptors'Last => Descriptors (I) = 16#AC#);
   pragma Assert (for all I in 16385 .. Mirrors'Last => Mirrors (I) = 16#BD#);
   -- Cross the native 64-table policy without changing this image's quota,
   -- moving its root, or committing the remaining quota-sized mirror tail.
   while Published < 124 * 4096 loop
      Published := Unsigned_64'Min (Published + 65536, 124 * 4096);
      VM.Extend_Metadata (Source, Unsigned_64 (To_Integer (Mirrors'Address)), Published, OK);
      pragma Assert (OK and VM.Backed_Tables (Source) = 8);
   end loop;
   pragma Assert (VM.Metadata_Capacity (Source) = 128);
   for I in Extra'Range loop Extra (I) := Unsigned_64 (I + 8) * 4096; end loop;
   VM.Append_Offline_Backing (Source, Extra, First, OK);
   pragma Assert (OK and First = 9 and VM.Backed_Tables (Source) = 128);
   for I in 1 .. 40 loop
      VM.Map_Page (Source, Unsigned_64 (I) * 2 ** 30,
                   16#300000# + Unsigned_64 (I) * 4096, Write_Back, Read_Write, OK);
      pragma Assert (OK);
   end loop;
   pragma Assert (VM.Used (Source) = 87 and VM.Root_DMA (Source) = 4096);
   for I in 1 .. 40 loop
      pragma Assert (VM.Lookup (Source, Unsigned_64 (I) * 2 ** 30) =
        Encode_Leaf (16#300000# + Unsigned_64 (I) * 4096, Write_Back, Read_Write));
   end loop;
   pragma Assert (VM.Lookup (Source, 4096) = Encode_Leaf (16#100000#, Write_Back, Read_Write));
   pragma Assert (VM.Lookup (Source, High) = Encode_Leaf (16#200000#, Write_Back, Read_Write));
   pragma Assert (not VM.DMA_Disjoint (Source, 128 * 4096, 4096));
   pragma Assert (for all I in 124 * 4096 + 1 .. Mirrors'Last => Mirrors (I) = 16#BD#);
   VM.Seal (Source, OK); pragma Assert (OK);
   Ada.Text_IO.Put_Line ("Sparse VM scale PASS: quota4096, bootstrap4, eight then128 backed/87 used tables, low/high48 and40 GiB-spaced mappings, stable root, bounded metadata growth and suffix guards");
end VM_Sparse_Scale_Tests;
