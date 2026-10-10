--  Hosted model test for Console_Ring: random writers and drains against
--  a reference FIFO, including wrap-around, full and empty rings.
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Console_Ring;
procedure Ring_Tests is
   package R renames Console_Ring;
   use type R.Byte_Count;
   subtype Op is Natural range 0 .. 99;
   package Random_Op is new Ada.Numerics.Discrete_Random (Op);
   Gen : Random_Op.Generator;
   Ring : R.Ring;
   Model : array (0 .. 9_999_999) of Character;
   Head, Tail : Natural := 0;   --  Model (Head .. Tail - 1) is queued
   Next_Char : Natural := 0;
   B : R.Batch;
   N : R.Batch_Count;
   Taken, Full_Seen, Writer_Paid : Natural := 0;
begin
   Random_Op.Reset (Gen, 1);
   for Step in 1 .. 2_000_000 loop
      if Random_Op.Random (Gen) < (if Step <= 1_000_000 then 60 else 99) then
         declare C : constant Character := Character'Val (32 + Next_Char mod 95);
         begin
            Next_Char := Next_Char + 1;
            if R.Is_Full (Ring) then
               --  The writer pays for one batch, as TextIO does.
               Full_Seen := Full_Seen + 1;
               R.Take (Ring, B, N);
               pragma Assert (N = R.Batch_Capacity);
               for I in 1 .. Natural (N) loop
                  pragma Assert (B (I) = Model (Head)); Head := Head + 1;
               end loop;
               Writer_Paid := Writer_Paid + 1;
            end if;
            R.Put (Ring, C);
            Model (Tail) := C; Tail := Tail + 1;
         end;
      else
         R.Take (Ring, B, N);
         pragma Assert (Natural (N) = Natural'Min (Tail - Head, R.Batch_Capacity));
         for I in 1 .. Natural (N) loop
            pragma Assert (B (I) = Model (Head)); Head := Head + 1;
         end loop;
         Taken := Taken + Natural (N);
      end if;
      pragma Assert (Natural (R.Length (Ring)) = Tail - Head);
   end loop;
   Ada.Text_IO.Put_Line ("CONSOLE-RING: PASS bytes=" & Tail'Image & " drained=" & Taken'Image &
     " full_writer_batches=" & Writer_Paid'Image);
end Ring_Tests;
