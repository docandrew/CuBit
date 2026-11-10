------------------------------------------------------------------------------
--  CuAlloc: the process heap (docs/userspace-allocator.md, "CuAlloc: one
--  allocator for everything"). Every CuBit process allocates through it,
--  whatever its language: Ada System.Memory, the libc malloc family, Rust
--  GlobalAlloc. It grows until the provider (the kernel's owned-memory
--  quota) says no; it has no size limit of its own.
--
--  Three paths, by size:
--    small, up to 4 KiB   arenas of 64 KiB slabs, each with its own proved
--                         Heap_Slabs metadata;
--    medium, up to 1 MiB  arenas of 4 KiB page runs, each with its own
--                         proved Heap_Extents metadata;
--    huge, above 1 MiB    one reservation per block, given back on Free;
--                         it starts on a 32 MiB boundary, the block one
--                         alignment unit above it (from 32 MiB alignment
--                         up, at it), so the page below is a zero header.
--  A small or medium arena is one naturally aligned 32 MiB reservation
--  (its metadata first, then 16 MiB of payload, backed as blocks reach
--  further into it), so the arena holding an address is the address with
--  its low 25 bits cleared; a two-level radix map from that slot names the
--  arena's heap and its place in that heap's arena table. Huge blocks are
--  found in a sorted directory by binary search.
--
--  Fast paths. Each small class caches one slab (arena and slab number):
--  an allocation takes a slot from that slab's free-slot cache or its
--  never-used slots, with no search and no call; a free of the caller's
--  own small block finds its arena from its address and the map entry
--  alone, releases it and makes its slab the class's cache (LIFO reuse).
--  Availability sets change only when a slab stops being full or becomes
--  empty.
--
--  Threads. Each thread holds its own heap (its arenas, its per-class
--  availability sets, its outgoing batches): allocation and frees of its
--  own blocks touch nothing shared and take no lock. A free of another
--  heap's block goes into a batch for that heap (a node block of 62
--  addresses, taken from the freeing thread's own heap); a full batch, or
--  the thread's periodic tending, publishes it to the owner's inbox with
--  one compare-and-swap; the owner takes its whole inbox with one atomic
--  exchange when it misses or tends, releases the blocks, and returns the
--  node (as its own batch node back to the producer: no allocation). At
--  thread exit (Thread_Exit) a heap is drained, its batches published and
--  it is marked abandoned; the next thread that needs a heap adopts it,
--  arenas, inbox and all. A thread without a thread pointer (FS base 0)
--  uses one shared fallback heap behind its own lock. Shared state is
--  only: the huge directory, the radix map and the heap registry, all
--  changed under one directory lock (arena creation, huge blocks, heap
--  attach/abandon: rare, and those paths already make a provider call),
--  and the inboxes (atomics).
--
--  SPARK, level 2 (tests/cualloc/run.sh). Proved: no run-time error
--  (overflow, range, index) in any size, offset, commit, map or batch
--  computation; the huge directory stays sorted and disjoint and routes
--  exactly; the provider is only asked to commit the exact next prefix of
--  a reservation, within it and within Maximum_Commit; every change to
--  the directory, the map and the registry is made with the directory lock
--  held (ghost Lock_Held); every change to a heap's private state and its
--  arenas' metadata is made by a call whose precondition names the
--  calling thread as the heap's recorded holder (checked at run time at
--  each entry) and, for the fallback heap, holds the fallback lock; each
--  heap's invariant is kept. Model-checked, not proved
--  (tests/cualloc/model/inbox_model.py): the inbox push/take-all protocol
--  and abandon/adopt, for no lost or duplicated batch. Assumed, listed in
--  tests/cualloc/check-proof.py: at each overlay, the memory is as its
--  last writer left it (a heap's private part as its holder left it; an
--  arena's core metadata likewise; the huge directory as the last locked
--  operation left it), which the lock and holder discipline give; the
--  atomics' semantics; the token is unique per live thread (its FS base).
--  Not proved: payload accesses (zeroing, the realloc copy) and node
--  contents, the provider, the lock word itself.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

generic
   --  Reserve Bytes (a page multiple) of address space with no backing,
   --  its base a multiple of Alignment (a power of two, at least a page):
   --  the base, or 0.
   with function Reserve (Bytes, Alignment : Unsigned_64) return Unsigned_64;
   --  Back [Offset, Offset + Bytes) of the reservation at Base, where Offset
   --  is exactly what is backed already and Bytes at most Maximum_Commit.
   --  Fresh backing reads as zero.
   with function Commit (Base, Offset, Bytes : Unsigned_64) return Boolean;
   --  Give back the whole reservation at Base, backed or not.
   with function Release (Base, Bytes : Unsigned_64) return Boolean;
   Maximum_Commit : Unsigned_64;
   --  Called in a thread right after it takes a heap (hosted builds arm a
   --  thread-exit destructor that calls Thread_Exit; CuBit's thread exits
   --  call it directly).
   with procedure Attached;
package CuAlloc with
  SPARK_Mode,
  Abstract_State => Heap_State,
  Initializes => Heap_State,
  Initial_Condition => Invariant and then Unlocked
is
   No_Address : constant Unsigned_64 := 0;
   Page_Bytes : constant := 4_096;
   --  The largest alignment a request may ask for.
   Maximum_Alignment : constant := 16#4000_0000#;
   --  The largest request; anything larger is refused before any rounding.
   Maximum_Request : constant := 2 ** 62;

   --  The heap's own invariant (its variables; overlaid memory is assumed,
   --  see above). It holds initially and every operation keeps it.
   function Invariant return Boolean with Ghost, Global => (Input => Heap_State);
   --  This caller holds neither the directory lock nor the fallback lock
   --  (ghost): every operation starts and ends so, so none can deadlock on
   --  its own lock or reenter it.
   function Unlocked return Boolean with Ghost, Global => (Input => Heap_State);

   --  A block of at least Bytes (1 when 0), aligned to Alignment (a power of
   --  two; at least 16), or No_Address when the provider refuses.
   function Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64 with
     Side_Effects, Pre => Invariant and then Unlocked, Post => Invariant and then Unlocked,
     Global => (In_Out => Heap_State);
   --  As Allocate, every byte zero.
   function Allocate_Zeroed (Bytes, Alignment : Unsigned_64) return Unsigned_64 with
     Side_Effects, Pre => Invariant and then Unlocked, Post => Invariant and then Unlocked,
     Global => (In_Out => Heap_State);
   --  No_Address is ignored; anything not from Allocate is ignored too.
   --  Any thread may free any block.
   procedure Free (Item : Unsigned_64) with
     Pre => Invariant and then Unlocked, Post => Invariant and then Unlocked,
     Global => (In_Out => Heap_State);
   --  Free, given the block's size and alignment as allocated (or as last
   --  reallocated): Rust's GlobalAlloc dealloc. A small or medium block is
   --  found from its arena's identity word instead of the radix map. The
   --  size is a hint, not trusted: the word is read only where a small or
   --  medium block can start, every huge block is placed so that the same
   --  probe reads its own zero header page, and anything that is not this
   --  heap's small arena takes the radix map. A wrong size costs a lookup,
   --  never a corruption (tests/cualloc). Item must still be a block.
   procedure Free_Sized (Item, Bytes, Alignment : Unsigned_64) with
     Pre => Invariant and then Unlocked, Post => Invariant and then Unlocked,
     Global => (In_Out => Heap_State);
   --  The bytes the block at Item holds (0 when Item is not a live block).
   function Usable_Size (Item : Unsigned_64) return Unsigned_64 with
     Side_Effects, Pre => Invariant and then Unlocked, Post => Invariant and then Unlocked,
     Global => (In_Out => Heap_State);
   --  Item's contents in a block of at least Bytes (Item itself when it
   --  already holds them); No_Address on failure, Item then unchanged.
   function Reallocate (Item, Bytes : Unsigned_64) return Unsigned_64 with
     Side_Effects, Pre => Invariant and then Unlocked, Post => Invariant and then Unlocked,
     Global => (In_Out => Heap_State);
   --  The calling thread ends: its heap is drained, its batches published
   --  and the heap left for the next thread to adopt. Harmless when the
   --  thread holds no heap; a later call from the same thread takes one
   --  again.
   procedure Thread_Exit with
     Pre => Invariant and then Unlocked, Post => Invariant and then Unlocked,
     Global => (In_Out => Heap_State);

   --  What the heap holds from the provider, for diagnostics and tests:
   --  small/medium arenas plus huge blocks, reserved and backed bytes
   --  (other threads' heaps are read without stopping them: approximate
   --  while they run).
   procedure Statistics (Arenas, Reserved, Committed : out Unsigned_64) with
     Pre => Invariant and then Unlocked, Post => Invariant and then Unlocked,
     Global => (In_Out => Heap_State);
end CuAlloc;
