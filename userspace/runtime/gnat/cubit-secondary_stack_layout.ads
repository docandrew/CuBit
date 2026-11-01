------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Where a thread's secondary stack lives and how it grows (the user
--  runtime's System.Secondary_Stack, CuBit.Thread_Areas).
--
--  @description
--  Every thread's secondary stack is its own owned-memory reservation
--  (kernel Process.Owned_Memory: Reserve, Commit_Prefix,
--  Release_Reservation), never part of a primary stack. A region is laid
--  out from its base as
--
--     [prefix][stack header][stack data ...........][guard]
--
--  The prefix is the thread's static TLS block and thread control block
--  when the runtime made the thread pointer itself (CuBit.Thread_Areas);
--  it is empty for threads whose TLS another runtime owns (musl, Rust).
--  The region is committed from its base: the first commit backs the
--  prefix, the header and Initial_Data_Bytes of data; later commits extend
--  it as the stack's top crosses the committed mark, Minimum_Growth_Bytes
--  or the size already committed (doubling), whichever is larger, and at
--  most Maximum_Commit_Bytes per system call. Committed addresses never
--  move. The last Guard_Bytes are never committed, so the stack's data is
--  never contiguous with whatever owned region follows it.
--
--  Reservation_Bytes is address space only: reserving costs no RAM. The
--  RAM a stack really uses is bounded by the process's frame quota, which
--  refuses a commit; that limit is policy, not this constant. The
--  reservation bounds one stack's reach so an allocation past it is
--  refused by a bounds check before any memory is touched.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Secondary_Stack_Layout with Pure, SPARK_Mode is

   --  CuBit.Kernel_ABI.Page_Bytes.
   Page_Bytes : constant := 4096;

   --  Address space each thread's secondary stack reserves: 256 MiB, the
   --  same reach as the largest primary stack a program may declare
   --  (kernel Process.MAX_USER_STACK_SIZE). The owned-memory aperture is
   --  1 TiB, so this admits 4096 threads' stacks at once.
   Reservation_Bytes : constant := 256 * 1024 * 1024;

   --  The never-committed tail of every region.
   Guard_Bytes : constant := Page_Bytes;

   --  The most a region may ever commit.
   Usable_Bytes : constant := Reservation_Bytes - Guard_Bytes;

   --  Data the first commit backs: twice the fixed 32 KiB stack this
   --  replaces, which nearly every program stays within.
   Initial_Data_Bytes : constant := 64 * 1024;

   --  The smallest commit after the first.
   Minimum_Growth_Bytes : constant := 1024 * 1024;

   --  The most one commit may add (kernel Owned_Reservation_Policy
   --  .Maximum_Commit_Bytes, CuBit.Kernel_ABI.Maximum_Owned_Commit_Bytes).
   Maximum_Commit_Bytes : constant := 16 * 1024 * 1024;

   --  The largest prefix (static TLS and thread control block) a region
   --  holds. Programs' static TLS is a few words; a larger one is refused.
   Maximum_Prefix_Bytes : constant := 1024 * 1024;

   --  Room for the stack header (System.Secondary_Stack.SS_Stack's fields
   --  before its data), which the runtime checks its record against.
   Header_Bytes : constant := 64;

   --  Offsets and sizes within one region.
   subtype Region_Bytes is Unsigned_64 range 0 .. Usable_Bytes;
   subtype Prefix_Bytes is Unsigned_64 range 0 .. Maximum_Prefix_Bytes;
   subtype Commit_Bytes is Unsigned_64 range 0 .. Maximum_Commit_Bytes;

   function Page_Aligned (Bytes : Region_Bytes) return Boolean is
     (Bytes mod Page_Bytes = 0);

   --  Bytes rounded up to whole pages.
   function Page_Round (Bytes : Region_Bytes) return Region_Bytes is
     ((Bytes + Page_Bytes - 1) / Page_Bytes * Page_Bytes)
   with Post => Page_Aligned (Page_Round'Result)
                  and then Page_Round'Result >= Bytes
                  and then Page_Round'Result - Bytes < Page_Bytes;

   --  The prefix rounded up so the header that follows it is aligned to
   --  Header_Bytes (at least the target's maximum alignment).
   function Header_Offset (Prefix : Prefix_Bytes) return Region_Bytes is
     ((Prefix + Header_Bytes - 1) / Header_Bytes * Header_Bytes)
   with Post => Header_Offset'Result >= Prefix
                  and then Header_Offset'Result mod Header_Bytes = 0
                  and then Header_Offset'Result - Prefix < Header_Bytes;

   --  Where the stack's data starts.
   function Data_Offset (Prefix : Prefix_Bytes) return Region_Bytes is
     (Header_Offset (Prefix) + Header_Bytes);

   --  The data a region can ever hold.
   function Data_Capacity (Prefix : Prefix_Bytes) return Region_Bytes is
     (Usable_Bytes - Data_Offset (Prefix))
   with Post => Data_Offset (Prefix) + Data_Capacity'Result = Usable_Bytes;

   --  The first commit: the prefix, the header and Initial_Data_Bytes.
   function First_Commit (Prefix : Prefix_Bytes) return Commit_Bytes is
     (Page_Round (Data_Offset (Prefix) + Initial_Data_Bytes))
   with Post => First_Commit'Result > 0
                  and then Page_Aligned (First_Commit'Result)
                  and then First_Commit'Result >= Data_Offset (Prefix) + Initial_Data_Bytes;

   --  The next commit for a region that has Committed bytes backed and
   --  needs Needed: at least Minimum_Growth_Bytes and at least Committed
   --  again, enough to reach Needed when one call may, and at most
   --  Maximum_Commit_Bytes and what the region has left. A caller repeats
   --  it until Committed reaches Needed.
   function Next_Commit (Committed, Needed : Region_Bytes) return Commit_Bytes
   with Pre => Page_Aligned (Committed) and then Committed < Needed,
        Post => Next_Commit'Result > 0
                  and then Page_Aligned (Next_Commit'Result)
                  and then Next_Commit'Result <= Usable_Bytes - Committed
                  and then (Committed + Next_Commit'Result >= Needed
                            or else Next_Commit'Result = Maximum_Commit_Bytes);

   --  The prefix of a thread area (CuBit.Thread_Areas): the static TLS
   --  block, then the thread control block at the thread pointer.

   --  The x86-64 ELF thread control block the runtime makes: its own
   --  address, the thread's secondary stack, then zeros (the
   --  stack-protector word, %fs:0x28, among them).
   Control_Block_Bytes : constant := 64;

   --  The static TLS segment (PT_TLS) the runtime lays out: at most half
   --  the largest prefix, aligned to a power of two dividing the control
   --  block's size, so the thread pointer is aligned for every
   --  thread-local variable.
   Maximum_TLS_Bytes : constant := Maximum_Prefix_Bytes / 2;
   Maximum_TLS_Alignment : constant := Control_Block_Bytes;
   subtype TLS_Bytes is Unsigned_64 range 0 .. Maximum_TLS_Bytes;
   subtype TLS_Alignment is Unsigned_64 range 1 .. Maximum_TLS_Alignment;

   function Valid_TLS_Alignment (Alignment : Unsigned_64) return Boolean is
     (Alignment in TLS_Alignment and then (Alignment and (Alignment - 1)) = 0);

   --  The static TLS block as the linker counts it below the thread
   --  pointer: the segment's size rounded up to its alignment.
   function TLS_Block_Bytes (Bytes : TLS_Bytes; Alignment : TLS_Alignment) return Region_Bytes
   with Post => TLS_Block_Bytes'Result >= Bytes
                  and then TLS_Block_Bytes'Result - Bytes < Alignment;

   --  Where the thread pointer is in the region: past the TLS block,
   --  aligned to the control block's size.
   function Thread_Pointer_Offset (Bytes : TLS_Bytes; Alignment : TLS_Alignment) return Region_Bytes is
     ((TLS_Block_Bytes (Bytes, Alignment) + Control_Block_Bytes - 1)
        / Control_Block_Bytes * Control_Block_Bytes)
   with Post => Thread_Pointer_Offset'Result >= TLS_Block_Bytes (Bytes, Alignment)
                  and then Thread_Pointer_Offset'Result mod Control_Block_Bytes = 0;

   --  The whole prefix: the TLS block and the control block.
   function Area_Prefix (Bytes : TLS_Bytes; Alignment : TLS_Alignment) return Prefix_Bytes is
     (Thread_Pointer_Offset (Bytes, Alignment) + Control_Block_Bytes)
   with Post => Area_Prefix'Result = Thread_Pointer_Offset (Bytes, Alignment) + Control_Block_Bytes;

end CuBit.Secondary_Stack_Layout;
