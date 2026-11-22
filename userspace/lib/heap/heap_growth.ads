pragma Ada_2022;
with Heap_Arrays;

--  Growing a Heap_Arrays block by doubling, from Initial elements up to a
--  named Ceiling, keeping its first Used elements: the one way the growable
--  stores (metrics.svc's series store, MET-002) get room. Grown False when
--  Wanted is past the ceiling or the heap refuses; B is then unchanged.
generic
   with package Blocks is new Heap_Arrays (<>);
procedure Heap_Growth
  (B : in out Blocks.Block; Used : Natural; Initial, Ceiling : Positive; Wanted : Natural;
   Grown : out Boolean)
with SPARK_Mode => Off,
     Pre => Used <= Blocks.Capacity (B) and then Initial <= Ceiling;
