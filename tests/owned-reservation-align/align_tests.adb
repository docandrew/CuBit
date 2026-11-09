--  RESERVE_OWNED_MEMORY's alignment argument (kernel
--  Process.Owned_Memory.Reserve, Owned_Reservation_Policy): which
--  alignments are accepted, and Align_Up, the step Find_Base takes past
--  each existing reservation. A placement walk over a fixed set of
--  reservations checks the result is aligned, clear of them and the
--  lowest such base. The live syscall is checked natively by
--  tests/userspace-allocator/cubit (cualloc-stress).
with Interfaces; use Interfaces;
with Owned_Reservation_Policy; use Owned_Reservation_Policy;
with Ada.Text_IO;
procedure Align_Tests is
   Count : Natural := 0;
   procedure Check (Good : Boolean; What : String) is
   begin
      Count := Count + 1;
      if not Good then
         Ada.Text_IO.Put_Line ("FAIL " & What);
         raise Program_Error;
      end if;
   end Check;

   type Range_Record is record
      Base, Bytes : Unsigned_64;
   end record;
   type Ranges is array (Positive range <>) of Range_Record;
   FIRST : constant := 16#5800_0000_0000#;
   LIMIT : constant := 16#5900_0000_0000#;
   MIB : constant := 1_048_576;

   --  Find_Base's walk (kernel/src/process-owned_memory.adb), over Taken
   --  sorted by base.
   function Place (Taken : Ranges; Bytes, Alignment : Unsigned_64) return Unsigned_64 is
      Candidate : Unsigned_64 := Align_Up (FIRST, Alignment);
   begin
      for Item of Taken loop
         if Candidate = 0 or else Candidate > LIMIT - Bytes then return 0; end if;
         if Candidate <= Item.Base and then Bytes <= Item.Base - Candidate then
            return Candidate;
         end if;
         Candidate := Align_Up (Unsigned_64'Max (Candidate, Item.Base + Item.Bytes), Alignment);
      end loop;
      return (if Candidate /= 0 and then Candidate <= LIMIT - Bytes then Candidate else 0);
   end Place;

   Taken : constant Ranges :=
     [(FIRST, 4096), (FIRST + 8192, 3 * MIB), (FIRST + 40 * MIB, 32 * MIB),
      (FIRST + 100 * MIB, 4096)];
   Sizes : constant array (1 .. 4) of Unsigned_64 := [4096, 64 * 1024, 32 * MIB, 64 * MIB];
begin
   --  Accepted alignments: 0, and powers of two up to 1 GiB.
   Check (Valid_Alignment (0), "0 (no alignment asked) is accepted");
   for Shift in 0 .. 30 loop
      Check (Valid_Alignment (2 ** Shift), "2 **" & Shift'Image & " is accepted");
      Check (Effective_Alignment (2 ** Shift) = Unsigned_64'Max (2 ** Shift, 4096), "effective");
   end loop;
   Check (not Valid_Alignment (2 ** 31), "2 GiB is refused");
   Check (not Valid_Alignment (3) and then not Valid_Alignment (12_288)
          and then not Valid_Alignment (Unsigned_64'Last), "non-powers are refused");
   Check (Effective_Alignment (0) = 4096, "0 means a page");

   --  Align_Up at its edges.
   for Shift in 12 .. 30 loop
      declare
         A : constant Unsigned_64 := 2 ** Shift;
      begin
         Check (Align_Up (0, A) = 0 and then Align_Up (A, A) = A and then Align_Up (A + 1, A) = 2 * A
                and then Align_Up (A - 1, A) = A, "Align_Up small values");
         Check (Align_Up (Unsigned_64'Last, A) = 0
                and then Align_Up (Unsigned_64'Last - A + 2, A) = 0, "Align_Up never wraps");
         Check (Align_Up (Unsigned_64'Last - (A - 1) - (A - 1), A) mod A = 0, "Align_Up near the top");
      end;
   end loop;

   --  Placement: aligned, clear of every reservation, the lowest.
   for Shift in 12 .. 30 loop
      for Bytes of Sizes loop
         declare
            A : constant Unsigned_64 := 2 ** Shift;
            Base : constant Unsigned_64 := Place (Taken, Bytes, A);
            Clear : Boolean := Base /= 0 and then Base mod A = 0;
            Lower : Unsigned_64 := FIRST;
            Collides : Boolean;
         begin
            for T of Taken loop
               Clear := Clear and then (Base + Bytes <= T.Base or else T.Base + T.Bytes <= Base);
            end loop;
            Check (Clear, "placement is aligned and clear, alignment" & A'Image & " bytes" & Bytes'Image);
            --  Lowest: every aligned base below it collides.
            while Lower < Base loop
               Collides := False;
               for T of Taken loop
                  Collides := Collides or else not (Lower + Bytes <= T.Base or else T.Base + T.Bytes <= Lower);
               end loop;
               Check (Collides, "no lower aligned base fits");
               Lower := Lower + A;
            end loop;
         end;
      end loop;
   end loop;
   --  32 MiB at 32 MiB alignment skips the small reservations at FIRST.
   Check (Place (Taken, 32 * MIB, 32 * MIB) = FIRST + 96 * MIB + 32 * MIB, "a 32 MiB arena lands on its boundary");
   Check (Place (Taken, 4096, 4096) = FIRST + 4096, "a page fills the first hole");
   Ada.Text_IO.Put_Line ("owned reservation alignment: PASS," & Count'Image & " checks");
end Align_Tests;
