--  Hosted concurrency test for Object_Table. Reader tasks (real OS threads)
--  look up random IDs lock-free and check each record's tag, passing a
--  quiescent point between lookups; a writer task allocates, tags, releases
--  and reclaims. Freed pages are poisoned, so a reader touching freed memory
--  sees a bad tag and the test fails.
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Quiescent_Reclamation; use Quiescent_Reclamation;
with Test_Table; use Test_Table;

procedure Main is
   Readers : constant := 4;
   Writer_CPU : constant CPU_Index := Readers;
   Online : constant CPU_Set :=
     [for C in CPU_Index => C <= Writer_CPU];

   Stop : Boolean := False with Atomic;
   Bad_Reads : Natural := 0 with Atomic;
   Good_Reads : Natural := 0 with Atomic;

   function Rand (Seed : in out Unsigned_64; N : Positive) return Natural is
   begin
      Seed := Seed * 6364136223846793005 + 1442695040888963407;
      return Natural (Shift_Right (Seed, 33) mod Unsigned_64 (N));
   end Rand;

   task type Reader (Me : CPU_Index);
   task body Reader is
      Seed : Unsigned_64 := Unsigned_64 (Me) + 17;
      Local_Good : Natural := 0;
   begin
      while not Stop loop
         declare
            I : constant Table.Id := Rand (Seed, 256) + 1;
         begin
            if Table.Present (I) then
               declare
                  R : constant Table.Element_Ref := Table.Lookup (I);
                  T1, T2 : Unsigned_64;
               begin
                  T1 := R.Tag;
                  for Spin in 1 .. 50 loop
                     null;
                  end loop;
                  T2 := R.Tag;
                  if (T1 /= 0 and then T1 /= Unsigned_64 (I) * Tag_Multiplier)
                    or else (T2 /= 0 and then T2 /= Unsigned_64 (I) * Tag_Multiplier)
                  then
                     Bad_Reads := Bad_Reads + 1;
                  else
                     Local_Good := Local_Good + 1;
                  end if;
               end;
            end if;
         end;
         --  Holding no record reference here.
         Table.Quiescent (Me);
      end loop;
      Good_Reads := Good_Reads + Local_Good;
   end Reader;

   Allocations, Releases : Natural := 0;
begin
   Table.Initialize;
   declare
      R0 : Reader (0);
      R1 : Reader (1);
      R2 : Reader (2);
      R3 : Reader (3);
      Seed : Unsigned_64 := 99;
      Live : array (1 .. 256) of Boolean := [others => False];
   begin
      for Step in 1 .. 2_000_000 loop
         case Rand (Seed, 3) is
            when 0 =>
               declare
                  I : Table.Id;
               begin
                  Table.Allocate (I);
                  if I /= 0 then
                     Table.Lookup (I).Tag := Unsigned_64 (I) * Tag_Multiplier;
                     Live (I) := True;
                     Allocations := Allocations + 1;
                  end if;
               end;
            when 1 =>
               declare
                  I : constant Positive := Rand (Seed, 256) + 1;
               begin
                  if Live (I) then
                     Table.Release (I);
                     Live (I) := False;
                     Releases := Releases + 1;
                  end if;
               end;
            when others =>
               Table.Quiescent (Writer_CPU);
               Table.Reclaim (Online);
         end case;
      end loop;
      Stop := True;
   end;   --  waits for the readers

   pragma Assert (Bad_Reads = 0, "a reader saw freed (poisoned) memory");
   pragma Assert (Table.Absent_Is_Pristine, "a write went to the Absent record");
   pragma Assert (Pages_Freed > 0, "no page was ever reclaimed");
   Ada.Text_IO.Put_Line
     ("PASS: object table," & Allocations'Image & " allocations," &
      Releases'Image & " releases," & Pages_Allocated'Image & " pages mapped," &
      Pages_Freed'Image & " freed after grace," & Good_Reads'Image &
      " concurrent lock-free reads, none of freed memory");
end Main;
