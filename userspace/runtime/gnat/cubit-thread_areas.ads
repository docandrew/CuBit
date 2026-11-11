------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Each thread's secondary stack, found through its thread pointer.
--
--  @description
--  Every thread that runs Ada code using the secondary stack has its own
--  stack (System.Secondary_Stack, upstream GNAT's: a static chunk, then
--  dynamic chunks on CuAlloc up to a named per-thread ceiling;
--  CuBit.Secondary_Stack_Layout). The thread's pointer to it
--  is a thread-local variable: the program's static TLS block, below the
--  thread pointer (the FS base) as the x86-64 ELF TLS ABI lays it out
--  (variant II; the linker places it, so it never collides with another
--  runtime's thread control block above the thread pointer or with other
--  thread-local variables). __gnat_get_secondary_stack is one load from
--  it (%fs:offset) and a test; no system call. A thread's stack is made on
--  its first use (a thread area's is made with the area), and given back,
--  chunks and all, when the thread ends.
--
--  Every such thread therefore needs an ELF thread pointer: FS pointing at
--  a thread control block whose first word is its own address, with the
--  program's static TLS block below it.
--    - musl (programs linked with the libc): its own, for the main thread
--      and every pthread. The libc's thread exit calls
--      __cubit_secondary_stack_release (weak).
--    - Rust std (moto-rt-cubit): its thread-local blocks carry the static
--      TLS block below them and call the release hook at thread exit.
--    - Ada and freestanding programs (userspace/c/crt0.S): Start_Main
--      below, from the start code before any constructor or Ada code, for
--      the main thread; Create and Exit_Thread for threads the program
--      makes itself with THREAD_CREATE.
--  The thread areas made here are one owned-memory region per thread: the
--  static TLS block, the thread control block, then the thread's stack
--  object with its static chunk. Musl's and Rust's threads have no area:
--  their stack object is a CuAlloc block.
--
--  A thread whose FS base is not such a thread pointer (zero, or a value a
--  program wrote for itself) must not use the secondary stack: the load
--  faults at a fixed, recognizable address instead of corrupting memory.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

pragma Warnings (Off, "internal GNAT unit");
with System.Secondary_Stack;
pragma Warnings (On, "internal GNAT unit");

package CuBit.Thread_Areas with Preelaborate is

   --  A thread pointer (FS base) for THREAD_CREATE: a new thread area (its
   --  static TLS block, thread control block and secondary stack), or
   --  No_Area when it is refused. Call from a thread that has a thread
   --  pointer itself.
   No_Area : constant Unsigned_64 := 0;
   function Create return Unsigned_64;

   --  Give back an area whose thread never ran (THREAD_CREATE refused it).
   procedure Discard (Thread_Pointer : Unsigned_64);

   --  End the calling thread, giving back its secondary stack, its CuAlloc
   --  heap (for adoption) and, for a thread Create made, its area first.
   procedure Exit_Thread with No_Return;

   --  The calling thread's secondary stack, made on its first use. When
   --  CuAlloc refuses its object, Storage_Error (HEAP_REFUSED).
   function Current_Stack return System.Secondary_Stack.SS_Stack_Ptr
   with Export, Convention => C, External_Name => "__gnat_get_secondary_stack";

   --  Give back the calling thread's secondary stack, at thread exit
   --  (the libc's and Rust std's thread exit call it, weakly linked): its
   --  chunks, then the thread's CuAlloc heap for adoption (again, if the
   --  caller already did: the chunks' frees may have taken it back), then
   --  its area, if the runtime made it. It writes nothing thread-local
   --  after the release.
   procedure Release_Current
   with Export, Convention => C, External_Name => "__cubit_secondary_stack_release";

   --  The main thread's thread area: its thread pointer and its secondary
   --  stack, installed by the start code (userspace/c/crt0.S) before any
   --  constructor or Ada code runs. When it is refused the program prints
   --  a fixed message and exits with Start_Refused_Status.
   Start_Refused_Status : constant Unsigned_64 := 125;
   procedure Start_Main
   with Export, Convention => C, External_Name => "__cubit_thread_start";

end CuBit.Thread_Areas;
