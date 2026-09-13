with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Numerics.Discrete_Random;
with Free_Block_Set; use Free_Block_Set;
procedure Benchmark is
   N : constant Capacity := 2 ** 21; -- order-zero capacity for 8 GiB
   type Storage_Access is access Storage;
   Bits : constant Storage_Access := new Storage (1 .. 2 * N - 1);
   type Oracle_Array is array (Index range <>) of Boolean;
   type Oracle_Access is access Oracle_Array;
   Oracle : constant Oracle_Access := new Oracle_Array'(0 .. N - 1 => False);
   package Random_Indices is new Ada.Numerics.Discrete_Random (Index);
   Generator : Random_Indices.Generator;
   Count, Expected, Item : Index := 0;
   Hint : Index := 0;
   Changed, Found, Present : Boolean;
   Start : Time;
   Elapsed : Duration;
   Batch : array (1 .. 128) of Index;
   type Link is record
      Prev, Next : Index := 0;
   end record;
   type Links is array (Index range <>) of Link;
   type Links_Access is access Links;
   Nodes : constant Links_Access := new Links (1 .. N);
   Head : Index := 0;
   procedure Check (Condition : Boolean) is
   begin
      -- Deliberately not pragma Assert: this release harness has Ghost and
      -- contracts disabled, but its independent oracle checks stay executable.
      if not Condition then
         raise Program_Error with "free-set oracle mismatch";
      end if;
   end Check;
begin
   Initialize (Bits.all);
   Random_Indices.Reset (Generator, 20260913);
   for Trial in 1 .. 200_000 loop
      Item := Random_Indices.Random (Generator) mod N;
      Present := not Oracle (Item);
      Update (Bits.all, Count, Item, Present, Changed);
      Check (Changed);
      Oracle (Item) := Present;
      Expected := (if Present then Expected + 1 else Expected - 1);
      Check (Count = Expected and Contains (Bits.all, Item) = Present);
      Find (Bits.all, Item, Found);
      Check (Found = (Expected /= 0));
      if Found then
         Check (Oracle (Item));
      end if;
   end loop;
   for I in Oracle'Range loop
      Check (Contains (Bits.all, I) = Oracle (I));
   end loop;
   Put_Line ("PASS release oracle: 200000 mutations, 2097152 final membership checks");

   Initialize (Bits.all);
   Count := 0;
   for I in 0 .. N / 2 - 1 loop
      Update (Bits.all, Count, I * 2, True, Changed);
   end loop;
   Start := Clock;
   for Trial in 1 .. 1_000 loop
      for I in Batch'Range loop
         Find (Bits.all, Batch (I), Found);
         Update (Bits.all, Count, Batch (I), False, Changed);
      end loop;
      for I in reverse Batch'Range loop
         Update (Bits.all, Count, Batch (I), True, Changed);
      end loop;
   end loop;
   Elapsed := To_Duration (Clock - Start);
   Check (Count = N / 2);
   Put_Line ("tree ns/operation:" & Duration'Image (Elapsed * 1_000_000_000 / 256_000));

   Start := Clock;
   for Trial in 1 .. 1_000 loop
      for I in Batch'Range loop
         Find_Near (Bits.all, Hint, Batch (I), Found);
         Hint := Batch (I);
         Update (Bits.all, Count, Batch (I), False, Changed);
      end loop;
      for I in reverse Batch'Range loop
         Update (Bits.all, Count, Batch (I), True, Changed);
      end loop;
   end loop;
   Elapsed := To_Duration (Clock - Start);
   Check (Count = N / 2);
   Put_Line ("hinted tree ns/operation:" & Duration'Image (Elapsed * 1_000_000_000 / 256_000));

   for I in reverse 1 .. N / 2 loop
      Item := I * 2;
      Nodes (Item) := (Prev => 0, Next => Head);
      if Head /= 0 then
         Nodes (Head).Prev := Item;
      end if;
      Head := Item;
   end loop;
   Start := Clock;
   for Trial in 1 .. 1_000 loop
      for I in Batch'Range loop
         Batch (I) := Head;
         Head := Nodes (Head).Next;
         Nodes (Head).Prev := 0;
      end loop;
      for I in reverse Batch'Range loop
         Item := Batch (I);
         Nodes (Item) := (Prev => 0, Next => Head);
         Nodes (Head).Prev := Item;
         Head := Item;
      end loop;
   end loop;
   Elapsed := To_Duration (Clock - Start);
   Put_Line ("ideal hot indexed-list ns/operation:" &
     Duration'Image (Elapsed * 1_000_000_000 / 256_000));
   Put_Line ("Relative microbenchmark only; not full allocator, contention or p99 latency.");
end Benchmark;
