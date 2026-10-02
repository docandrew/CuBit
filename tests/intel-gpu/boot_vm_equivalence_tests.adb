with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_Submission_Backing;
with Intel_GPU_Submission_Image;
with Intel_GPU_VM_Image;
procedure Boot_VM_Equivalence_Tests is
   package Layout renames Intel_GPU_Submission_Backing;
   package Probe renames Intel_GPU_Submission_Image;
   package VM is new Intel_GPU_VM_Image (4);
   type Starts is array (Positive range <>) of Unsigned_64;
   Table_Regions : constant array (VM.Page_Number) of Layout.Region :=
     [Layout.PML4, Layout.PDPT, Layout.PD, Layout.PT];
   Data_Regions : constant array (Positive range 1 .. 8) of Layout.Region :=
     [Layout.Batch_Buffer, Layout.Completion_Page,
      Layout.Offscreen_Buffer, Layout.Offscreen_Buffer,
      Layout.Offscreen_Buffer, Layout.Offscreen_Buffer,
      Layout.Render_State_Page, Layout.Shader_Page];
begin
   for Base of Starts'[4096, 16#12345000#, 2 ** 32 - Probe.Byte_Count] loop
      declare
         Boot : constant Probe.Image := Probe.Build (Base, 16#100000#);
         Original, Candidate : VM.Image;
         Tables, Fresh : VM.Backing_Pages;
         Data : VM.Data_Pages (1 .. 8);
         OK : Boolean;
         Word : Natural;
         Expected : Unsigned_64;
      begin
         pragma Assert (Boot.Valid);
         for P in VM.Page_Number loop
            Tables (P) := Base + Layout.Offsets (Table_Regions (P)) - Layout.First;
            Fresh (P) := 16#80000000# + Unsigned_64 (P) * 4096;
         end loop;
         for P in Data'Range loop
            Data (P) := Base + Layout.Offsets (Data_Regions (P)) - Layout.First;
            if P in 4 .. 6 then
               Data (P) := Data (P) + Unsigned_64 (P - 3) * 4096;
            end if;
         end loop;
         VM.Initialize (Original, Tables, OK); pragma Assert (OK);
         VM.Map_Pages (Original, Probe.Batch_VA, Data, Write_Back, Read_Write, OK);
         pragma Assert (OK);
         VM.Seal (Original, OK); pragma Assert (OK);
         pragma Assert (VM.Used (Original) = 4);
         pragma Assert (VM.Root_DMA (Original) = Tables (1));
         -- Compare the actual packed boot image, not another invocation of
         -- Initial_VM. Include every unused directory/leaf entry as well.
         for P in VM.Page_Number loop
            for I in Table_Index loop
               Word := Natural ((Tables (P) - Base) / 4) + 2 * I;
               Expected := Unsigned_64 (Boot.Words (Word)) or
                 Shift_Left (Unsigned_64 (Boot.Words (Word + 1)), 32);
               pragma Assert (VM.Entry_Value (Original, P, I) = Expected,
                 "base" & Base'Image & " page" & P'Image & " index" & I'Image &
                 " got" & VM.Entry_Value (Original, P, I)'Image &
                 " expected" & Expected'Image);
            end loop;
         end loop;
         VM.Prepare_Update (Candidate, Original, Fresh, OK);
         pragma Assert (OK);
         -- Stage a ninth page without replacing the live root or modifying
         -- the sealed source. This is offline preparation, NOT publication.
         VM.Map_Page (Candidate, Probe.Batch_VA + 8 * 4096,
                      16#90000000#, Write_Back, Read_Write, OK);
         pragma Assert (OK);
         VM.Seal (Candidate, OK); pragma Assert (OK);
         for P in Data'Range loop
            Expected := Encode_Leaf (Data (P), Write_Back, Read_Write);
            pragma Assert (VM.Lookup (Original, Probe.Batch_VA +
              Unsigned_64 (P - 1) * 4096) = Expected);
            pragma Assert (VM.Lookup (Candidate, Probe.Batch_VA +
              Unsigned_64 (P - 1) * 4096) = Expected);
         end loop;
         pragma Assert (VM.Lookup (Original, Probe.Batch_VA + 8 * 4096) = 0);
         pragma Assert (VM.Lookup (Candidate, Probe.Batch_VA + 8 * 4096) =
           Encode_Leaf (16#90000000#, Write_Back, Read_Write));
         pragma Assert (VM.Root_DMA (Original) = Tables (1));
         pragma Assert (VM.Root_DMA (Candidate) = Fresh (1));
      end;
   end loop;
   Ada.Text_IO.Put_Line ("boot VM equivalence and retained-source update PASS");
end Boot_VM_Equivalence_Tests;
