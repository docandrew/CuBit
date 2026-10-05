with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_VM_Image;
with Intel_GPU_PPGTT_Scratch;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
procedure VM_Initial_Stream_Tests is
   package VM is new Intel_GPU_VM_Image
     (4096, Bootstrap_Tables => 4, Bootstrap_Descriptors => 4,
      Bootstrap_Insertion_Words => 512, Bootstrap_Growth_Links => 2);
begin
   for Case_ID in 0 .. 5 loop
      declare
         Source : VM.Image;
         Reads : Natural := 0;
         function Read_Page (Page : VM.Page_Number) return Unsigned_64 is
         begin
            Reads := Reads + 1;
            pragma Assert (Page = Reads and Page <= 4);
            return (if Case_ID = 1 and Page = 3 then 4096
                    elsif Case_ID = 2 and Page = 2 then 0
                    elsif Case_ID = 3 and Page = 4 then 16385
                    else Unsigned_64 (Page) * 4096);
         end;
         procedure Initialize is new VM.Initialize_From_Pages (Read_Page);
         Scratch : Intel_GPU_PPGTT_Scratch.Backing_Pages := [others => 0];
         OK : Boolean;
         Before : Natural;
      begin
         if Case_ID = 4 then
            Initialize (Source, 5, OK);
            pragma Assert (not OK and Reads = 0 and VM.Revision (Source) = 0);
         elsif Case_ID = 5 then
            Scratch (Scratch'First) := 123;
         end if;
         Initialize (Source, 4, OK, Scratch);
         pragma Assert (OK = (Case_ID in 0 | 4));
         pragma Assert (Reads = (case Case_ID is
           when 0 | 3 | 4 => 4, when 1 => 3, when 2 => 2, when others => 0));
         if OK then
            pragma Assert (VM.Root_DMA (Source) = 4096 and VM.Backed_Tables (Source) = 4);
            VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK);
            pragma Assert (OK and VM.Used (Source) = 4);
         else
            pragma Assert (VM.Root_DMA (Source) = 0 and VM.Backed_Tables (Source) = 0);
         end if;
         Before := Reads;
         Initialize (Source, 4, OK);
         pragma Assert (not OK and Reads = Before);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Initial stream PASS6: quota4096 with four reads, duplicate/zero/unaligned/scratch rejection, metadata retry only before consumption, no callback replay");
end VM_Initial_Stream_Tests;
