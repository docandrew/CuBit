with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_GGTT_Reservations; use Intel_GPU_GGTT_Reservations;
procedure GGTT_Reservations_Tests is
   Object : Ledger;
   OK : Boolean;
   Status : Result;
   Candidate : Unsigned_64;
begin
   Allocate (Object, 4096, 4096, Candidate, Status);
   pragma Assert (Status = Rejected and Candidate = 0 and Count (Object) = 0);
   Find_Free (Object, 4096, 4096, Candidate, OK);
   pragma Assert (not OK and Candidate = 0);
   Reserve (Object, 4096, 4096, Status);
   pragma Assert (Status = Rejected and Count (Object) = 0);
   Admit (Object, 4096, 4096, 4097, OK);
   pragma Assert (not OK);
   Admit (Object, 4096, 4096, 511 * 4096, OK);
   pragma Assert (OK);
   Admit (Object, 4096, 0, 4096, OK);
   pragma Assert (not OK); -- cannot replace the admitted aperture
   Reserve (Object, 0, 4096, Status);
   pragma Assert (Status = Rejected);
   Reserve (Object, 511 * 4096, 8192, Status);
   pragma Assert (Status = Rejected);
   Reserve (Object, 4096, Unsigned_64'Last, Status);
   pragma Assert (Status = Rejected);
   -- Compare all pairs of bounded intervals against independent page-set
   -- intersection, including adjacency, enclosure and partial overlap.
   for A in 1 .. 8 loop
      for B in A .. 8 loop
         for C in 1 .. 8 loop
            for D in C .. 8 loop
               declare
                  Pair : Ledger;
                  Intersects : Boolean := False;
               begin
                  Admit (Pair, 4096, 4096, 8 * 4096, OK);
                  pragma Assert (OK);
                  Reserve (Pair, Unsigned_64 (A) * 4096,
                           Unsigned_64 (B - A + 1) * 4096, Status);
                  pragma Assert (Status = Reserved);
                  for Page in 1 .. 8 loop
                     Intersects := Intersects or
                       (Page in A .. B and Page in C .. D);
                  end loop;
                  Reserve (Pair, Unsigned_64 (C) * 4096,
                           Unsigned_64 (D - C + 1) * 4096, Status);
                  pragma Assert (Status = (if Intersects then Overlap else Reserved));
                  pragma Assert (Count (Pair) = (if Intersects then 1 else 2));
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   for I in 1 .. 64 loop
      Reserve (Object, Unsigned_64 (I) * 4096, 4096, Status);
      pragma Assert (Status = Reserved and Count (Object) = I);
   end loop;
   Reserve (Object, 65 * 4096, 4096, Status);
   pragma Assert (Status = Exhausted and Count (Object) = 64);
   Reserve (Object, 4096, 4096, Status);
   pragma Assert (Status = Overlap and Count (Object) = 64);
   declare
      Edge : Ledger;
   begin
      Admit (Edge, 8 * 1024 * 1024, Unsigned_64'Last - 4095, 4096, OK);
      pragma Assert (not OK);
      Admit (Edge, 8 * 1024 * 1024, 2 ** 32 - 4096, 4096, OK);
      pragma Assert (OK);
      Reserve (Edge, 2 ** 32 - 4096, 4096, Status);
      pragma Assert (Status = Reserved);
      Reserve (Edge, 2 ** 32, 4096, Status);
      pragma Assert (Status = Rejected and Count (Edge) = 1);
   end;
   -- Independent occupancy oracle, reverse insertion order, every eight-page
   -- occupancy pattern, four alignments and sizes including a too-large one.
   for Mask in Unsigned_32 range 0 .. 255 loop
      declare
         Search : Ledger;
         Expected : Unsigned_64;
         Available : Boolean;
         Saved_Count : Natural;
      begin
         Admit (Search, 4096, 0, 8 * 4096, OK);
         pragma Assert (OK);
         for Page in reverse 0 .. 7 loop
            if (Mask and Shift_Left (1, Page)) /= 0 then
               Reserve (Search, Unsigned_64 (Page) * 4096, 4096, Status);
               pragma Assert (Status = Reserved);
            end if;
         end loop;
         Saved_Count := Count (Search);
         for Pages in 1 .. 9 loop
            for Power in 0 .. 3 loop
               Expected := Unsigned_64'Last;
               for Start in 0 .. 7 loop
                  Available := Start mod (2 ** Power) = 0 and Start + Pages <= 8;
                  for Offset in 0 .. Pages - 1 loop
                     if Start + Offset > 7 or else
                       (Mask and Shift_Left (1, Start + Offset)) /= 0
                     then Available := False; end if;
                  end loop;
                  if Available then Expected := Unsigned_64 (Start) * 4096; exit; end if;
               end loop;
               Find_Free (Search, Unsigned_64 (Pages) * 4096,
                          4096 * 2 ** Power, Candidate, OK);
               pragma Assert (OK = (Expected /= Unsigned_64'Last));
               pragma Assert ((if OK then Candidate = Expected else Candidate = 0));
               pragma Assert (Count (Search) = Saved_Count);
            end loop;
         end loop;
         Find_Free (Search, 0, 4096, Candidate, OK);
         pragma Assert (not OK);
         Find_Free (Search, 4097, 4096, Candidate, OK);
         pragma Assert (not OK);
         Find_Free (Search, 4096, 12288, Candidate, OK);
         pragma Assert (not OK);
         Find_Free (Search, 4096, 0, Candidate, OK);
         pragma Assert (not OK);
      end;
   end loop;
   declare
      Pool : Ledger;
   begin
      Admit (Pool, 4096, 4096, 511 * 4096, OK);
      pragma Assert (OK);
      for I in 1 .. 64 loop
         Allocate (Pool, 4096, 8192, Candidate, Status);
         pragma Assert (Status = Reserved and Candidate = Unsigned_64 (I) * 8192);
         pragma Assert (Count (Pool) = I);
      end loop;
      Allocate (Pool, 4096, 8192, Candidate, Status);
      pragma Assert (Status = Exhausted and Candidate = 0 and Count (Pool) = 64);
      Allocate (Pool, 4097, 4096, Candidate, Status);
      pragma Assert (Status = Rejected and Candidate = 0 and Count (Pool) = 64);
   end;
   declare
      Pool : Ledger;
   begin
      Admit (Pool, 4096, 0, 4096, OK);
      pragma Assert (OK);
      Allocate (Pool, 4096, 4096, Candidate, Status);
      pragma Assert (Status = Reserved and Candidate = 0 and Count (Pool) = 1);
      Allocate (Pool, 4096, 4096, Candidate, Status);
      pragma Assert (Status = Rejected and Candidate = 0 and Count (Pool) = 1);
   end;
   Ada.Text_IO.Put_Line ("GGTT reservations PASS: 1296 interval pairs, admission and capacity; 9216 free-space queries; retained allocation");
end GGTT_Reservations_Tests;
