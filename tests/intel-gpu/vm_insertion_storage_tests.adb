with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Insertion;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
procedure VM_Insertion_Storage_Tests is
   package VM is new Intel_GPU_VM_Image (8, 8, 2);
   Source : VM.Image;
   State : VM.Insertion_Receipt;
   type Bytes is array (1 .. 8192) of Unsigned_8;
   Storage : Bytes := [others => 16#A5#] with Alignment => 4096;
   Writes : Natural := 0;
   function Owner return Boolean is (True);
   procedure Write_Leaf (Table_DMA : Unsigned_64; Index : Table_Index;
     Expected, Replacement : Unsigned_64; OK : out Boolean) is
   begin
      Writes := Writes + 1;
      pragma Assert (Index = Table_Index (Writes + 1));
      pragma Assert (Expected = 0 and Replacement =
        Encode_Leaf (16#200000# + Unsigned_64 (Writes - 1) * 4096, Write_Back, Read_Write));
      OK := True;
   end Write_Leaf;
   procedure Invalidate (OK : out Boolean) is begin OK := True; end;
   package Insert is new VM.Insertion (Owner, Write_Leaf, Invalidate);
   OK : Boolean;
   Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Storage'Address));
   Data : VM.Data_Pages := [16#200000#, 16#201000#, 16#202000#];
begin
   VM.Initialize (Source, [for P in VM.Page_Number => Unsigned_64 (P) * 4096], OK);
   pragma Assert (OK);
   VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK); pragma Assert (OK);
   VM.Seal (Source, OK); pragma Assert (OK);
   pragma Assert (VM.Insertion_Capacity (State) = 2);
   Insert.Start (State, Source, VM.Revision (Source), 8192, Data, Write_Back, Read_Write, OK);
   pragma Assert (not OK and not Insert.Failed (State) and Writes = 0);
   VM.Extend_Insertion_Metadata (State, Base + 1, 4096, OK); pragma Assert (not OK);
   VM.Extend_Insertion_Metadata (State, Base, 4096, OK); pragma Assert (OK);
   pragma Assert (VM.Insertion_Capacity (State) > 2);
   Insert.Start (State, Source, VM.Revision (Source), 8192, Data, Write_Back, Read_Write, OK);
   pragma Assert (OK and Writes = 0);
   -- Captured words must not borrow the caller's mutable data.
   Data := [others => 16#DEAD000#];
   VM.Extend_Insertion_Metadata (State, Base, 8192, OK); pragma Assert (not OK);
   while Insert.Publishing (State) loop Insert.Step (State, Source); end loop;
   pragma Assert (Insert.Published (State) and Writes = 3);
   Insert.Commit (State, Source, True, OK); pragma Assert (OK);
   for P in 0 .. 2 loop
      pragma Assert (VM.Lookup (Source, 8192 + Unsigned_64 (P) * 4096) =
        Encode_Leaf (16#200000# + Unsigned_64 (P) * 4096, Write_Back, Read_Write));
   end loop;
   for P in 4097 .. 8192 loop pragma Assert (Storage (P) = 16#A5#); end loop;
   VM.Extend_Insertion_Metadata (State, Base, 8192, OK); pragma Assert (OK);
   -- Successful completion reopens idle storage, not a failed transaction.
   Insert.Start (State, Source, VM.Revision (Source), 5 * 4096,
     [16#300000#], Write_Back, Read_Write, OK); pragma Assert (OK);
   Insert.Commit (State, Source, False, OK); pragma Assert (not OK);
   VM.Extend_Insertion_Metadata (State, Base, 12288, OK); pragma Assert (not OK);
   Ada.Text_IO.Put_Line ("Insertion storage PASS: dynamic growth, retained words, active/failed exclusion, guard suffix");
end VM_Insertion_Storage_Tests;
