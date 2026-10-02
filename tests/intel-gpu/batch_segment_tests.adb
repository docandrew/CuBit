with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Context_Init;
with Intel_GPU_VA_Encoding;
procedure Batch_Segment_Tests is
   package Init renames Intel_GPU_ADLN_Context_Init;
   type Addresses is array (Positive range <>) of Unsigned_64;
   type Sequences is array (Positive range <>) of Unsigned_32;
begin
   for Readable in Boolean loop
      for WM of Sequences'(0, 16#12345678#, Unsigned_32'Last) loop
         for Sequence of Sequences'(0, 1, 5, Unsigned_32'Last) loop
            for GPU of Addresses'(0, 1, 7, 8, 16#200400#, 2 ** 47,
              2 ** 48 - 8, 2 ** 48, Unsigned_64'Last) loop
               declare
                  Baseline : constant Init.Segment := Init.Build (Readable, WM, Sequence);
                  Result : constant Init.Segment := Init.Build_Batch (Readable, WM, Sequence, GPU);
               begin
                  pragma Assert (Result.Valid = (Baseline.Valid and GPU /= 0 and
                    GPU < 2 ** 48 and GPU mod 8 = 0));
                  if Result.Valid then
                     -- Every barrier/settings/arbitration/completion word
                     -- must remain unchanged. Only the branch address varies.
                     for I in Init.Command_Words'Range loop
                        if I not in 60 .. 61 then
                           pragma Assert (Result.Words (I) = Baseline.Words (I));
                        end if;
                     end loop;
                     pragma Assert ((Unsigned_64 (Result.Words (60)) or
                       Shift_Left (Unsigned_64 (Result.Words (61)), 32)) =
                       Intel_GPU_VA_Encoding.Canonical (GPU));
                     pragma Assert (Result.Words (90) = Sequence);
                  else
                     pragma Assert (for all Word of Result.Words => Word = 0);
                  end if;
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Batch segment PASS: 216 input combinations, exact barriers/settings/completion retained (encoding only)");
end Batch_Segment_Tests;
