------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  pthread_getattr_np for CuBit (replaces the C overlay; docs/c-removal.md),
--  and two attribute setters GNAT's tasking needs (below):
--  a thread's attributes from musl's struct pthread; for the main thread,
--  whose stack musl did not allocate, the stack the loader gave it
--  (CuBit.Libc_Start).
--
--  @description
--  musl's struct pthread and pthread_attr_t are private to musl; the
--  offsets here are its x86-64 layout, and tests/libc-ada checks them
--  against musl's src/internal/pthread_impl.h.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces.C;
with System;

package CuBit.Libc_Threads is

   function Get_Attributes (Thread, Attributes : System.Address) return Interfaces.C.int
   with Export, Convention => C, External_Name => "pthread_getattr_np";

   --  Two attribute setters GNAT's run-time library for Linux (libgnarl,
   --  System.Task_Primitives.Operations) calls, which musl lacks. Neither
   --  is reached under the default locking policy; both answer as musl's
   --  neighbours do. Priority ceilings (Locking_Policy Ceiling_Locking):
   --  ENOTSUP, as musl's pthread_mutexattr_setprotocol answers for
   --  PTHREAD_PRIO_PROTECT. A read/write lock's preference
   --  (Concurrent_Readers_Locking): a hint, accepted without effect, as
   --  musl's read/write locks have no preference to set.
   function Set_Priority_Ceiling (Attributes : System.Address; Ceiling : Interfaces.C.int)
     return Interfaces.C.int
   with Export, Convention => C, External_Name => "pthread_mutexattr_setprioceiling";
   function Set_Read_Write_Kind (Attributes : System.Address; Kind : Interfaces.C.int)
     return Interfaces.C.int
   with Export, Convention => C, External_Name => "pthread_rwlockattr_setkind_np";

end CuBit.Libc_Threads;
