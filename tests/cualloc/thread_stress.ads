--  CuAlloc under threads (hosted, GNAT tasks over pthreads): generations of
--  Workers, each allocating blocks of mixed sizes, filling them with its
--  own pattern and passing them through a shared queue, where other
--  workers check the pattern and free them (remote frees, batched to the
--  owner's inbox). Each generation's tasks end (their heaps are drained and
--  abandoned) and the next generation adopts them.
with Interfaces; use Interfaces;
package Thread_Stress is
   type Outcome is record
      Corrupt : Boolean := False;     --  a block did not hold its pattern
      Unaligned : Boolean := False;
      Refused : Boolean := False;     --  an allocation failed
      Blocks : Natural := 0;          --  blocks passed between threads
      Before_Arenas : Unsigned_64 := 0;  --  arenas before the first generation
      First_Arenas : Unsigned_64 := 0;  --  arenas after the first generation
      Last_Arenas : Unsigned_64 := 0;   --  and after the last
   end record;
   procedure Run (Workers, Generations, Rounds : Positive; Result : out Outcome);
end Thread_Stress;
