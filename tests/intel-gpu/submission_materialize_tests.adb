with Interfaces; use Interfaces;
with Intel_GPU_Submission_Image;
with Intel_GPU_Submission_Materialize;
with Intel_GPU_Submission_Backing;
procedure Submission_Materialize_Tests is
   package Images renames Intel_GPU_Submission_Image;
   package Writer renames Intel_GPU_Submission_Materialize;
   use type Writer.Bytes;
   Value : Images.Image := Images.Build (16#108C000#, 16#2000000#);
   -- Nonzero lower bound and surrounding sentinels detect offset mistakes.
   Buffer : Writer.Bytes (17 .. 17 + Writer.Byte_Count + 1) :=
     [others => 16#A5#];
   Success : Boolean;
begin
   pragma Assert (Value.Valid);
   Writer.Write (Value, Buffer, Success);
   pragma Assert (not Success and Buffer = [Buffer'Range => 16#A5#]);
   Writer.Write (Value, Buffer (18 .. Buffer'Last - 2), Success);
   pragma Assert (not Success and Buffer = [Buffer'Range => 16#A5#]);
   Value.Valid := False;
   Writer.Write (Value, Buffer (18 .. Buffer'Last - 1), Success);
   pragma Assert (not Success and Buffer = [Buffer'Range => 16#A5#]);
   Value.Valid := True;
   Writer.Write (Value, Buffer (18 .. Buffer'Last - 1), Success);
   pragma Assert (Success and Buffer (17) = 16#A5# and
                  Buffer (Buffer'Last) = 16#A5#);
   for I in Value.Words'Range loop
      declare
         At_Byte : constant Natural := 18 + I * 4;
         Decoded : constant Unsigned_32 :=
           Unsigned_32 (Buffer (At_Byte)) or
           Shift_Left (Unsigned_32 (Buffer (At_Byte + 1)), 8) or
           Shift_Left (Unsigned_32 (Buffer (At_Byte + 2)), 16) or
           Shift_Left (Unsigned_32 (Buffer (At_Byte + 3)), 24);
      begin
         pragma Assert (Decoded = Value.Words (I));
      end;
   end loop;
   Writer.Write (Value, Buffer (18 .. 17), Success);
   pragma Assert (not Success);
   Buffer := [others => 16#A5#];
   Writer.Write_Non_VM (Value, Buffer, Success);
   pragma Assert (not Success and Buffer = [Buffer'Range => 16#A5#]);
   Writer.Write_Non_VM (Value, Buffer (18 .. Buffer'Last - 1), Success);
   pragma Assert (Success and Buffer (17) = 16#A5# and Buffer (Buffer'Last) = 16#A5#);
   declare
      use Intel_GPU_Submission_Backing;
      VM_First : constant Natural := Natural (Offsets (PML4) - First);
      VM_Limit : constant Natural := Natural (Offsets (PT) + Sizes (PT) - First);
   begin
      for I in Value.Words'Range loop
         for B in Natural range 0 .. 3 loop
            pragma Assert (Buffer (18 + I * 4 + B) =
              (if I * 4 >= VM_First and I * 4 < VM_Limit then 16#A5# else
               Unsigned_8 (Shift_Right (Value.Words (I), B * 8) and 16#FF#)));
         end loop;
      end loop;
   end;
end Submission_Materialize_Tests;
