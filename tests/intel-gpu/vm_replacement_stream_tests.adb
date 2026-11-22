with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_VM_Image;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
procedure VM_Replacement_Stream_Tests is
   package VM is new Intel_GPU_VM_Image (8);
begin
   for Case_ID in 0 .. 7 loop
      declare
         Source, Candidate : VM.Image;
         Reads : Natural := 0;
         function Read_Page (Page : VM.Page_Number) return Unsigned_64 is
         begin
            Reads := Reads + 1;
            pragma Assert (Page = Reads and Page <= 6);
            return (if Case_ID = 1 and Page = 3 then 40960
                    elsif Case_ID = 2 and Page = 2 then 20480
                    elsif Case_ID = 3 and Page = 2 then 16#100000#
                    elsif Case_ID = 4 and Page = 2 then 0
                    elsif Case_ID = 7 and Page = 2 then 45057
                    else 40960 + Unsigned_64 (Page - 1) * 4096);
         end;
         procedure Prepare is new VM.Prepare_Update_From_Pages (Read_Page);
         OK : Boolean;
         Epoch : Unsigned_64;
         Before : Natural;
      begin
         VM.Initialize (Source, [4096, 8192, 12288, 16384, 20480, others => 0],
                        OK, Backing_Count => 5); pragma Assert (OK);
         VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK);
         pragma Assert (OK);
         if Case_ID /= 6 then VM.Seal (Source, OK); pragma Assert (OK); end if;
         Epoch := VM.Revision (Source);
         Prepare (Candidate, Source, (if Case_ID = 5 then 3 else 6), OK);
         pragma Assert (OK = (Case_ID = 0));
         pragma Assert (Reads = (case Case_ID is when 0 => 6, when 1 => 3,
           when 5 | 6 => 0, when others => 2));
         pragma Assert (VM.Root_DMA (Source) = 4096 and VM.Revision (Source) = Epoch);
         pragma Assert (VM.Backed_Tables (Source) = 5 and VM.Used (Source) = 4);
         pragma Assert (VM.Lookup (Source, 4096) = Encode_Leaf (16#100000#, Write_Back, Read_Write));
         if OK then
            pragma Assert (VM.Root_DMA (Candidate) = 40960 and VM.Backed_Tables (Candidate) = 6);
            pragma Assert (VM.Used (Candidate) = 4);
            pragma Assert (VM.Lookup (Candidate, 4096) = VM.Lookup (Source, 4096));
            VM.Seal_Update (Candidate, OK); pragma Assert (OK);
            pragma Assert (VM.Direct_Successor (Source, Candidate));
         else
            pragma Assert (VM.Root_DMA (Candidate) = 0 and VM.Backed_Tables (Candidate) = 0);
            pragma Assert (VM.Used (Candidate) = 0);
         end if;
         Before := Reads;
         Prepare (Candidate, Source, 6, OK);
         pragma Assert (not OK and Reads = Before);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Replacement stream PASS8: single-read prefix, duplicate/reserved-table/data/zero/alignment rejection, source preserved, successor rebase and no replay");
end VM_Replacement_Stream_Tests;
