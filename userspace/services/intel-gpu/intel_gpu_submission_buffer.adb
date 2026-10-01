with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Submission_Backing;
with Intel_GPU_Submission_Image;
with Intel_GPU_Submission_Materialize;
with Intel_GPU_DMA_Cache;
with Intel_GPU_VM_Buffer;
with Intel_GPU_VM_Materialize;
with Intel_GPU_ADLN_PPGTT;
package body Intel_GPU_Submission_Buffer is
   package Backing renames Intel_GPU_Submission_Backing;
   package Images renames Intel_GPU_Submission_Image;
   package Writer renames Intel_GPU_Submission_Materialize;
   function Initialized_GPU_Start (Object : Buffer_State) return Unsigned_64 is
     (Object.GPU_Address);
   function Completed_Pixel_View (Object : Buffer_State)
      return Intel_GPU_Buffer_Reply.Backing is
      View : Intel_GPU_Buffer_Reply.Backing;
   begin
      if Object.GPU_Address = 0 or else Object.Boot_Root.CPU = 0 or else
        not VM.Sealed (Object.Boot_VM) or else Object.Update_Failed or else
        not Owner_Ready or else not Completed_Read_Owner
      then return (Ready => False); end if;
      View := Intel_GPU_Buffer_Reply.Slice (Object.Allocation,
        Backing.Offsets (Backing.Offscreen_Buffer) - Backing.First,
        Backing.Sizes (Backing.Offscreen_Buffer));
      if not Owner_Ready or else not Completed_Read_Owner then
         return (Ready => False);
      end if;
      return View;
   end Completed_Pixel_View;
   function Retained_Boot_Root (Object : Buffer_State) return Root_Mapping is
     (Object.Boot_Root);

   procedure Prepare_Boot_Update
     (Object : Buffer_State; Candidate : in out VM.Image;
      Backing : VM.Backing_Pages; Success : out Boolean) is
   begin
      Success := False;
      if Object.Update_Attempted or else Object.GPU_Address = 0 or else Object.Boot_Root.CPU = 0 or else
        not VM.Sealed (Object.Boot_VM) or else not Owner_Ready
      then return; end if;
      VM.Prepare_Update (Candidate, Object.Boot_VM, Backing, Success);
      Success := Success and then Owner_Ready;
   end Prepare_Boot_Update;

   procedure Initialize_Common
     (Object : in out Buffer_State;
      Allocation : Intel_GPU_Buffer_Reply.Backing;
      GGTT_Start, Bytes : Unsigned_64;
      External_VM : Boolean; Root_DMA : Unsigned_64;
      Success : out Boolean) is
      package VM_Buffers is new Intel_GPU_VM_Buffer (VM);
      CPU_Start : constant Unsigned_64 := (if Allocation.Ready then Allocation.CPU_Address else 0);
      Capacity : constant Unsigned_64 := (if Allocation.Ready then Allocation.Bytes else 0);
      function Owns_Backing return Boolean is (Owner_Ready);
      function Flush_Page (CPU : Unsigned_64) return Boolean is
        (Intel_GPU_DMA_Cache.Flush_Range (CPU, 4096));
      package VM_Writer is new Intel_GPU_VM_Materialize (VM, Owns_Backing, Flush_Page);
      Page_Image : VM.Image renames Object.Boot_VM;
      Page_State : VM_Writer.State;
      Tables : VM.Backing_Pages;
      Mappings : VM_Writer.Mappings;
      Root : Unsigned_64;
      Ready : Boolean;
      Image_Pages : Images.Backing_Pages;
   begin
      Success := False;
      if Object.Attempted then return; end if;
      Object.Attempted := True;
      if not Intel_GPU_Buffer_Reply.Valid (Allocation) or else
        Capacity < Writer.Byte_Count or else
        not Backing.Valid_Layout or else
        Backing.After_Last - Backing.First /= Writer.Byte_Count or else
        Bytes /= Images.GGTT_Bytes or else
        (External_VM and then Intel_GPU_Buffer_Reply.Overlaps_DMA (Allocation, Root_DMA, 4096)) or else
        not Owns_Backing
      then return; end if;
      for P in Image_Pages'Range loop
         Image_Pages (P) := Intel_GPU_Buffer_Reply.Page_Address
           (Allocation, Unsigned_64 (P) * 4096);
      end loop;
      declare
         Image : constant Images.Image :=
           (if External_VM then Images.Build_For_VM (Image_Pages, GGTT_Start, Root_DMA)
            else Images.Build (Image_Pages, GGTT_Start));
         Buffer : Writer.Bytes (0 .. Writer.Byte_Count - 1)
           with Import, Address => To_Address (Integer_Address (CPU_Start));
         -- Volatile reads must reach the backing rather than be optimized
         -- into reads of the source image. No device owns this tail yet.
         Readback : Writer.Bytes (0 .. Writer.Byte_Count - 1)
           with Import, Volatile,
             Address => To_Address (Integer_Address (CPU_Start));
      begin
         if not Image.Valid then return; end if;
         if External_VM then
            if not Owns_Backing then return; end if;
            Writer.Write (Image, Buffer, Success);
            if not Success then return; end if;
            Success := False;
         else
            for P in VM.Page_Number loop
               declare
                  Offset : constant Unsigned_64 :=
                    Backing.Offsets (Backing.PML4) - Backing.First + Unsigned_64 (P - 1) * 4096;
               begin
                  Tables (P) := Intel_GPU_Buffer_Reply.Page_Address (Allocation, Offset);
                  Mappings (P) := (CPU_Start + Offset, Tables (P));
               end;
            end loop;
            VM.Initialize (Page_Image, Tables, Ready);
            if not Ready then return; end if;
            VM_Buffers.Bind_Range (Page_Image, Allocation, Images.Batch_VA,
              Backing.Offsets (Backing.Batch_Buffer) - Backing.First, 2 * 4096,
              Intel_GPU_ADLN_PPGTT.Write_Back, Intel_GPU_ADLN_PPGTT.Read_Write, Ready);
            if not Ready then return; end if;
            -- The target/state/shader tail is contiguous in retained backing
            -- and private VA. Deliberately skip Engine_Status_Page: it is a
            -- separate GGTT object, never exposed through this PPGTT.
            VM_Buffers.Bind_Range (Page_Image, Allocation, Backing.Offscreen_GPU_VA,
              Backing.Offsets (Backing.Offscreen_Buffer) - Backing.First,
              Backing.Sizes (Backing.Offscreen_Buffer) +
                Backing.Sizes (Backing.Render_State_Page) + Backing.Sizes (Backing.Shader_Page),
              Intel_GPU_ADLN_PPGTT.Write_Back, Intel_GPU_ADLN_PPGTT.Read_Write, Ready);
            if not Ready then return; end if;
            VM.Seal (Page_Image, Ready);
            if not Ready or else not Owns_Backing then return; end if;
            Writer.Write_Non_VM (Image, Buffer, Success);
            if not Success then return; end if;
            Success := False;
            VM_Writer.Prepare (Page_State, Page_Image, Mappings, Root, Ready);
            if not Ready or else Root /= Tables (1) then return; end if;
         end if;
         for I in Image.Words'Range loop
            for B in Natural range 0 .. 3 loop
               if Readback (I * 4 + B) /= Unsigned_8
                 (Shift_Right (Image.Words (I), B * 8) and 16#FF#)
               then return; end if;
            end loop;
         end loop;
         if not Owns_Backing then return; end if;
         Success := Intel_GPU_DMA_Cache.Flush_Range
           (CPU_Start, Writer.Byte_Count) and then Owns_Backing;
         if Success then
            Object.GPU_Address := GGTT_Start;
            Object.Allocation := Allocation;
            if not External_VM then
               Object.Boot_Root := (Mappings (1).CPU, Tables (1));
            end if;
         end if;
      end;
   end Initialize_Common;

   procedure Initialize
     (Object : in out Buffer_State; Allocation : Intel_GPU_Buffer_Reply.Backing;
      GGTT_Start, Bytes : Unsigned_64; Success : out Boolean) is
   begin
      Initialize_Common (Object, Allocation, GGTT_Start, Bytes, False, 0, Success);
   end Initialize;

   procedure Initialize_For_VM
     (Object : in out Buffer_State; Allocation : Intel_GPU_Buffer_Reply.Backing;
      GGTT_Start, Bytes, Root_DMA : Unsigned_64; Success : out Boolean) is
   begin
      Initialize_Common (Object, Allocation, GGTT_Start, Bytes, True, Root_DMA, Success);
   end Initialize_For_VM;
end Intel_GPU_Submission_Buffer;
