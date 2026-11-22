with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_VM_Image;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_PPGTT_Scratch;
procedure VM_Offline_Backing_Tests is
   package VM is new Intel_GPU_VM_Image (8);
   package Small is new Intel_GPU_VM_Image (8, 4);
   Backing : constant VM.Backing_Pages := [1 => 4096, 2 => 8192,
     3 => 12288, 4 => 16384, others => 0];
   Scratch : constant Intel_GPU_PPGTT_Scratch.Backing_Pages :=
     [0 => 16#200000#, 1 => 16#201000#, 2 => 16#202000#, 3 => 16#203000#];
begin
   for Fault in 0 .. 9 loop
      declare
         Source : VM.Image;
         Pages : VM.Data_Pages (3 .. 5) := [16#300000#, 16#301000#, 16#302000#];
         First : Natural;
         OK : Boolean;
         Revision, Prior : Unsigned_64;
      begin
         if Fault /= 9 then
            VM.Initialize (Source, Backing, OK, Scratch, Backing_Count => 4); pragma Assert (OK);
            VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK); pragma Assert (OK);
            -- Reproduces why four physical pages alone cannot support a
            -- second directory before the first context publication.
            VM.Map_Page (Source, 2 ** 39, 16#101000#, Write_Back, Read_Write, OK);
            pragma Assert (not OK and VM.Used (Source) = 4 and VM.Lookup (Source, 2 ** 39) = 0);
         end if;
         case Fault is
            when 1 => Pages (4) := Pages (3);
            when 2 => Pages (4) := 4096;
            when 3 => Pages (4) := 16#100000#;
            when 4 => Pages (4) := Scratch (2);
            when 5 => Pages (4) := 0;
            when 6 => Pages (4) := 16#300001#;
            when 7 => Pages (4) := 2 ** 32;
            when 8 => VM.Seal (Source, OK); pragma Assert (OK);
            when others => null;
         end case;
         Revision := VM.Revision (Source); Prior := VM.Lookup (Source, 4096);
         VM.Append_Offline_Backing (Source, Pages, First, OK);
         pragma Assert (OK = (Fault = 0));
         pragma Assert (VM.Lookup (Source, 4096) = Prior);
         pragma Assert (VM.Used (Source) = (if Fault = 9 then 0 else 4));
         if Fault = 0 then
            pragma Assert (First = 5 and VM.Revision (Source) = Revision + 1);
            pragma Assert (VM.Page_DMA (Source, 5) = 0); -- reserved, not used
            VM.Map_Page (Source, 2 ** 39, 16#101000#, Write_Back, Read_Write, OK);
            pragma Assert (OK and VM.Used (Source) = 7);
            for I in 5 .. 7 loop pragma Assert (VM.Page_DMA (Source, I) = Pages (I - 2)); end loop;
            Revision := VM.Revision (Source);
            VM.Append_Offline_Backing (Source, [1 => 16#400000#, 2 => 16#401000#], First, OK);
            pragma Assert (not OK and First = 0 and VM.Revision (Source) = Revision);
            VM.Append_Offline_Backing (Source, [1 => 16#400000#], First, OK);
            pragma Assert (OK and First = 8);
            VM.Map_Page (Source, 2 ** 39 + 2 ** 21, 16#102000#, Write_Back, Read_Write, OK);
            pragma Assert (OK and VM.Used (Source) = 8 and VM.Page_DMA (Source, 8) = 16#400000#);
            VM.Seal (Source, OK); pragma Assert (OK);
         else
            pragma Assert (First = 0 and VM.Revision (Source) = Revision);
            if Fault in 1 .. 7 then
               -- Rejected groups installed no prefix; the same valid first
               -- page can still be appended with the corrected remainder.
               VM.Append_Offline_Backing (Source,
                 [1 => 16#300000#, 2 => 16#301000#, 3 => 16#302000#], First, OK);
               pragma Assert (OK and First = 5);
            end if;
         end if;
      end;
   end loop;
   declare
      Source : Small.Image;
      First : Natural;
      OK : Boolean;
      Epoch : Unsigned_64;
   begin
      Small.Initialize (Source, [1 => 4096, 2 => 8192, 3 => 12288, 4 => 16384, others => 0],
        OK, Backing_Count => 4); pragma Assert (OK);
      Epoch := Small.Revision (Source);
      Small.Append_Offline_Backing (Source, [1 => 16#300000#], First, OK);
      pragma Assert (not OK and First = 0 and Small.Revision (Source) = Epoch);
      Small.Append_Offline_Backing (Source, [1 .. 0 => 0], First, OK);
      pragma Assert (not OK and First = 0 and Small.Revision (Source) = Epoch);
   end;
   declare
      Source : Small.Image;
      First : Natural;
      OK : Boolean;
      Pages : Small.Data_Pages (Positive'Last .. Positive'Last) := [others => 8192];
   begin
      Small.Initialize (Source, [1 => 4096, others => 0], OK, Backing_Count => 1);
      pragma Assert (OK);
      Small.Append_Offline_Backing (Source, Pages, First, OK);
      pragma Assert (OK and First = 2 and Small.Used (Source) = 1);
      Small.Append_Offline_Backing (Source, Pages, First, OK);
      pragma Assert (not OK and First = 0 and Small.Used (Source) = 1);
   end;
   Ada.Text_IO.Put_Line ("Offline backing PASS14: four-page bootstrap, sparse repeated expansion, stable mappings, atomic alias/geometry/frozen/quota rejection, high array bound, reserved alias");
end VM_Offline_Backing_Tests;
