with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Firmware_Buffer;
with Intel_GPU_Submission_Backing;
with Intel_GPU_Submission_Image;
with Intel_GPU_Submission_Materialize;
with Intel_GPU_DMA_Cache;
package body Intel_GPU_Submission_Buffer is
   package Backing renames Intel_GPU_Submission_Backing;
   package Images renames Intel_GPU_Submission_Image;
   package Writer renames Intel_GPU_Submission_Materialize;
   Attempted : Boolean := False;
   GPU_Address : Unsigned_64 := 0;
   function Initialized_GPU_Start return Unsigned_64 is (GPU_Address);

   procedure Initialize
     (GGTT_Start, Bytes : Unsigned_64; Success : out Boolean)
   is
      View : constant Intel_GPU_Firmware_Buffer.Prepared_Buffer :=
        Intel_GPU_Firmware_Buffer.Prepared;
   begin
      Success := False;
      if Attempted then return; end if;
      Attempted := True;
      if not View.Ready or else View.CPU_Address /= 16#61000000# or else
        View.Allocation_Bytes /= 1024 * 1024 or else
        not Backing.Valid_Layout or else
        Backing.After_Last - Backing.First /= Writer.Byte_Count or else
        Bytes /= Images.GGTT_Bytes
      then return; end if;
      declare
         Image : constant Images.Image := Images.Build
           (View.DMA_Address, GGTT_Start);
         CPU_Start : constant Unsigned_64 := View.CPU_Address + Backing.First;
         Buffer : Writer.Bytes (0 .. Writer.Byte_Count - 1)
           with Import, Address => To_Address (Integer_Address (CPU_Start));
         -- Volatile reads must reach the backing rather than be optimized
         -- into reads of the source image. No device owns this tail yet.
         Readback : Writer.Bytes (0 .. Writer.Byte_Count - 1)
           with Import, Volatile,
             Address => To_Address (Integer_Address (CPU_Start));
      begin
         Writer.Write (Image, Buffer, Success);
         if not Success then return; end if;
         Success := False;
         for I in Image.Words'Range loop
            for B in Natural range 0 .. 3 loop
               if Readback (I * 4 + B) /= Unsigned_8
                 (Shift_Right (Image.Words (I), B * 8) and 16#FF#)
               then return; end if;
            end loop;
         end loop;
         Success := Intel_GPU_DMA_Cache.Flush_Range
           (CPU_Start, Writer.Byte_Count);
         if Success then GPU_Address := GGTT_Start; end if;
      end;
   end Initialize;
end Intel_GPU_Submission_Buffer;
