with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Batch_Start;
with Intel_GPU_Submission_Image;
with Intel_GPU_VA_Encoding;
procedure ADLN_Batch_Start_Tests is
   package Start renames Intel_GPU_ADLN_Batch_Start;
   use type Start.Command_Words;
   Words : constant Start.Command_Words := Start.Build;
   Address : constant Unsigned_64 := Unsigned_64 (Words (2)) or
     Shift_Left (Unsigned_64 (Words (3)), 32);
begin
   pragma Assert (Words =
     [16#04000001#, 16#18800101#, 16#200000#, 0, 16#04000000#, 0]);
   pragma Assert (Words (1) = (Shift_Left (Unsigned_32 (16#31#), 23)
     or Shift_Left (Unsigned_32 (1), 8) or 1));
   pragma Assert (Address = Intel_GPU_Submission_Image.Batch_VA);
   pragma Assert (Address mod 4096 = 0);
   pragma Assert (Words'Length * 4 mod 8 = 0);
   declare
      type Addresses is array (Positive range <>) of Unsigned_64;
   begin
      for GPU of Addresses'(0, 1, 4, 7, 8, 4095, 4096, 4097, 16#200400#, 2 ** 47 - 4096,
          2 ** 47, 2 ** 48 - 4096, 2 ** 48, 16#FFFF_8000_0000_0000#,
          Unsigned_64'Last) loop
         declare
            Batch : constant Start.Encoded_Batch := Start.Build_At (GPU);
         begin
            pragma Assert (Batch.Valid = (GPU /= 0 and GPU < 2 ** 48 and GPU mod 8 = 0));
            if Batch.Valid then
               -- Dynamic addresses must never change the privilege selector.
               pragma Assert ((Batch.Words (1) and 16#100#) /= 0);
               pragma Assert ((Unsigned_64 (Batch.Words (2)) or
                 Shift_Left (Unsigned_64 (Batch.Words (3)), 32)) =
                 Intel_GPU_VA_Encoding.Canonical (GPU));
               for I in Start.Command_Words'Range loop
                  if I not in 2 .. 3 then pragma Assert (Batch.Words (I) = Words (I)); end if;
               end loop;
            else
               pragma Assert (for all Word of Batch.Words => Word = 0);
            end if;
         end;
      end loop;
   end;
end ADLN_Batch_Start_Tests;
