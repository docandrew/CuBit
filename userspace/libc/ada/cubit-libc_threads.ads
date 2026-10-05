------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  pthread_getattr_np for CuBit (replaces the C overlay; docs/c-removal.md):
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

end CuBit.Libc_Threads;
