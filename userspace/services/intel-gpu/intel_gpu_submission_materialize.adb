with Intel_GPU_Submission_Backing;
package body Intel_GPU_Submission_Materialize with SPARK_Mode is
   procedure Write
     (Image : Intel_GPU_Submission_Image.Image;
      Buffer : in out Bytes; Success : out Boolean)
   is
   begin
      Success := False;
      if not Image.Valid or else Buffer'Length /= Byte_Count then
         return;
      end if;
      for I in Image.Words'Range loop
         for B in Natural range 0 .. 3 loop
            Buffer (Buffer'First + I * 4 + B) := Unsigned_8
              (Shift_Right (Image.Words (I), B * 8) and 16#FF#);
         end loop;
      end loop;
      Success := True;
   end Write;
   procedure Write_Non_VM
     (Image : Intel_GPU_Submission_Image.Image;
      Buffer : in out Bytes; Success : out Boolean) is
      use Intel_GPU_Submission_Backing;
      VM_First : constant Natural := Natural (Offsets (PML4) - First);
      VM_Limit : constant Natural := Natural (Offsets (PT) + Sizes (PT) - First);
   begin
      Success := False;
      if not Image.Valid or else Buffer'Length /= Byte_Count then return; end if;
      for I in Image.Words'Range loop
         if I * 4 < VM_First or else I * 4 >= VM_Limit then
            for B in Natural range 0 .. 3 loop
               Buffer (Buffer'First + I * 4 + B) := Unsigned_8
                 (Shift_Right (Image.Words (I), B * 8) and 16#FF#);
            end loop;
         end if;
      end loop;
      Success := True;
   end Write_Non_VM;
end Intel_GPU_Submission_Materialize;
