with Intel_GPU_ADLN_L3_Commands;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Context_Init;
with Intel_GPU_ADLN_Context_Settings;
with Intel_GPU_ADLN_Batch_Start;
with Intel_GPU_ADLN_Barrier;
procedure Context_Init_Tests is
   package Init renames Intel_GPU_ADLN_Context_Init;
   function Bit (N : Natural) return Unsigned_32 is (Shift_Left (1, N));
   -- Independently assemble the upstream named bit groups instead of copying
   -- the production hexadecimal masks.
   Flush : constant Unsigned_32 := Bit (27) or Bit (28) or Bit (12) or Bit (0) or
     Bit (13) or Bit (5) or Bit (7) or Bit (21) or Bit (14) or Bit (20);
   Invalidate : constant Unsigned_32 := Bit (29) or Bit (18) or Bit (11) or
     Bit (10) or Bit (4) or Bit (3) or Bit (2) or Bit (21) or Bit (14) or Bit (20);
   type Samples is array (Positive range <>) of Unsigned_32;
begin
   for Sequence of Samples'(0, 1, 2, 16#80000000#, Unsigned_32'Last) loop
      declare
         B : constant Intel_GPU_ADLN_Barrier.Segment := Intel_GPU_ADLN_Barrier.Build (Sequence);
         R : constant Init.Segment := Init.Build (True, 0, Sequence);
      begin
         pragma Assert (B.Valid = (Sequence /= 0));
         if B.Valid then
            for I in 0 .. 21 loop pragma Assert (B.Words (I) = R.Words (I)); end loop;
            for I in 0 .. 5 loop pragma Assert (B.Words (22 + I) = R.Words (86 + I)); end loop;
            pragma Assert (B.Words (28) = Shift_Left (5, 23) and B.Words (29) = 0);
         else
            pragma Assert (for all Word of B.Words => Word = 0);
         end if;
      end;
   end loop;
   for Readable in Boolean loop
      for WM of Samples'(0, 1, 16#20#, 16#12345678#, Unsigned_32'Last) loop
         declare
            R : constant Init.Segment := Init.Build (Readable, WM);
            Setup : constant Init.Segment := Init.Build_Setup (Readable, WM);
            S : constant Intel_GPU_ADLN_Context_Settings.Segment :=
              Intel_GPU_ADLN_Context_Settings.Build (Readable, WM);
         begin
            pragma Assert (R.Valid = S.Valid);
            pragma Assert (Setup.Valid = R.Valid);
            for I in R.Words'Range loop
               pragma Assert (Setup.Words (I) =
                 (if I in 58 .. 63 or I = 92 then 0 else R.Words (I)));
            end loop;
            if R.Valid then
               for Start of Samples'(0, 36, 64) loop
                  declare B : constant Natural := Natural (Start); begin
                     pragma Assert (R.Words (B) = (16#7A000004# or Bit (9)));
                     pragma Assert (R.Words (B + 1) = Flush and R.Words (B + 2) = 16#D0#);
                     pragma Assert (R.Words (B + 6) = (16#02800000# or Bit (8) or 1));
                     pragma Assert (R.Words (B + 7) = 16#7A000004#);
                     pragma Assert (R.Words (B + 8) = Invalidate and R.Words (B + 9) = 16#D0#);
                     pragma Assert (R.Words (B + 13) = (16#11000001# or Bit (17)));
                     pragma Assert (R.Words (B + 14) = 16#4208# and R.Words (B + 15) = 1);
                     pragma Assert (R.Words (B + 16) = (Shift_Left (16#1C#, 23) or
                       3 or Bit (16) or Bit (15) or Shift_Left (4, 12)));
                     pragma Assert (R.Words (B + 18) = 16#4208#);
                     pragma Assert (R.Words (B + 21) = (16#02800000# or Bit (8)));
                  end;
               end loop;
               for I in S.Words'Range loop pragma Assert (R.Words (22 + I) = S.Words (I)); end loop;
               pragma Assert (R.Words (26) = (WM or 16#20#));
               declare
                  Batch : constant Intel_GPU_ADLN_Batch_Start.Command_Words :=
                    Intel_GPU_ADLN_Batch_Start.Build;
               begin
                  for I in Batch'Range loop
                     pragma Assert (R.Words (58 + I) = Batch (I));
                  end loop;
               end;
               -- Independent encoding oracle retained from the main checkout.
               pragma Assert (R.Words (58) = 16#04000001# and
                 R.Words (59) = 16#18800101# and
                 R.Words (60) = 16#200000# and R.Words (61) = 0 and
                 R.Words (62) = 16#04000000# and R.Words (63) = 0);
               pragma Assert (R.Words (86) = 16#7A000004#);
               pragma Assert (R.Words (87) =
                 (Bit (20) or Bit (21) or Bit (14) or Bit (7)));
               pragma Assert (R.Words (88) = 16#D0# and R.Words (89) = 0);
               pragma Assert (R.Words (90) = 1 and R.Words (91) = 0);
               pragma Assert (R.Words (92) = Shift_Left (8, 23) + 1);
               pragma Assert (R.Words (93) = 0 and R.Words (95) = 0);
               pragma Assert (R.Words (94) = Shift_Left (5, 23));
            else
               pragma Assert (for all Word of R.Words => Word = 0);
            end if;
         end;
      end loop;
   end loop;
   for Sequence of Samples'(0, 1, 2, 16#80000000#, Unsigned_32'Last) loop
      declare
         R : constant Init.Segment := Init.Build (True, 0, Sequence);
         First : constant Init.Segment := Init.Build (True, 0);
      begin
         pragma Assert (R.Valid = (Sequence /= 0));
         if R.Valid then
            for I in R.Words'Range loop
               pragma Assert (R.Words (I) =
                 (if I = 90 then Sequence else First.Words (I)));
            end loop;
         else
            pragma Assert (for all Word of R.Words => Word = 0);
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("ADL-N context initialization barriers PASS (encoding only, NOT executed)");
   for Sequence of Samples'(0, 1, 3, 16#80000000#, Unsigned_32'Last) loop
      declare
         R : constant Init.Segment := Init.Build_L3 (Sequence);
         Original : constant Init.Segment := Init.Build (True, 0, Sequence);
      begin
         pragma Assert (R.Valid = (Sequence /= 0));
         if R.Valid then
            for I in R.Words'Range loop
               if I in 22 .. 32 then
                  pragma Assert (R.Words (I) =
                    Intel_GPU_ADLN_L3_Commands.Initialize_And_Sample (I - 22));
               elsif I in 33 .. 35 | 58 .. 63 | 92 then
                  pragma Assert (R.Words (I) = 0);
               else
                  pragma Assert (R.Words (I) = Original.Words (I));
               end if;
            end loop;
         else
            pragma Assert (for all Word of R.Words => Word = 0);
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("ADL-N context/L3 barriers PASS (encoding only, NOT executed)");
   for Readable in Boolean loop
      for WM of Samples'(0, 16#12345678#, Unsigned_32'Last) loop
         for Sequence of Samples'(0, 4, Unsigned_32'Last) loop
            declare
               R : constant Init.Segment := Init.Build_Draw (Readable, WM, Sequence);
               M : constant Init.Segment := Init.Build (Readable, WM, Sequence);
            begin
               pragma Assert (R.Valid = M.Valid);
               for I in R.Words'Range loop
                  pragma Assert (R.Words (I) =
                    (if R.Valid and I = 60 then 16#200400# else M.Words (I)));
               end loop;
            end;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Drawing branch PASS: only fixed private VA differs; guards/barriers retained");
end Context_Init_Tests;
