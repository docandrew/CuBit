------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What libgcc's DWARF unwinder (the one GNAT's zero-cost exceptions and
--  System.Traceback use, EXC-001) needs from the platform, for programs on
--  the user runtime.
--
--  @description
--  The unwinder is the pinned toolchain's own (libgcc_eh.a's unwind-dw2.o
--  and unwind-dw2-fde-dip.o, unchanged code), copied into libgnat-user.a by
--  the runtime build (kernel/Makefile, user_runtime) with its library calls
--  renamed to the __cubit_unwind_* names below (objcopy --redefine-syms,
--  userspace/runtime/unwinder-symbols.txt). Renamed because the musl
--  versions (pthread_once, pthread_mutex_lock, dl_iterate_phdr) assume
--  musl's thread control block and auxiliary vector, which a program
--  started by userspace/c/crt0.S does not have, and so that a program
--  linking both the user runtime and the CuBit libc keeps the libc's
--  versions for its own C code.
--
--  Each is the C function it replaces, for the unwinder's uses only:
--    - Iterate_Program_Headers: dl_iterate_phdr for the one static,
--      non-PIE object, from its own ELF and program headers (__ehdr_start;
--      the linker scripts load them): the unwinder finds PT_GNU_EH_FRAME
--      (the .eh_frame_hdr search table) there. It reports the size without
--      dlpi_adds/dlpi_subs, so libgcc keeps no cache between calls (that
--      cache relies on the dynamic loader's lock).
--    - Once, Lock, Unlock, Wait, Broadcast: pthread_once, mutexes and
--      condition variables over futexes (CuBit.Futex_Locks), on the
--      unwinder's statically zeroed objects;
--    - Allocate, Free: CuAlloc (frame registration only);
--    - String_Length; Abort: report on the debug console, end the process
--      as an unhandled exception would (CuBit.Program_Endings).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces.C;
with System;

package CuBit.Unwinder_Support with Preelaborate is

   subtype int is Interfaces.C.int;
   subtype size_t is Interfaces.C.size_t;

   --  dl_iterate_phdr's callback: (info, size, data) -> int.
   type Header_Callback is access function
     (Info : System.Address; Size : size_t; Data : System.Address) return int
   with Convention => C;

   function Iterate_Program_Headers
     (Callback : Header_Callback; Data : System.Address) return int
   with Export, Convention => C, External_Name => "__cubit_unwind_dl_iterate_phdr";

   type Init_Routine is access procedure with Convention => C;

   function Once (Control : System.Address; Init : Init_Routine) return int
   with Export, Convention => C, External_Name => "__cubit_unwind_once";

   function Lock (Mutex : System.Address) return int
   with Export, Convention => C, External_Name => "__cubit_unwind_mutex_lock";
   function Unlock (Mutex : System.Address) return int
   with Export, Convention => C, External_Name => "__cubit_unwind_mutex_unlock";

   function Wait (Condition, Mutex : System.Address) return int
   with Export, Convention => C, External_Name => "__cubit_unwind_cond_wait";
   function Broadcast (Condition : System.Address) return int
   with Export, Convention => C, External_Name => "__cubit_unwind_cond_broadcast";

   function Allocate (Bytes : size_t) return System.Address
   with Export, Convention => C, External_Name => "__cubit_unwind_malloc";
   procedure Free (Item : System.Address)
   with Export, Convention => C, External_Name => "__cubit_unwind_free";

   function String_Length (Item : System.Address) return size_t
   with Export, Convention => C, External_Name => "__cubit_unwind_strlen";

   procedure Abort_Unwinding
   with Export, Convention => C, External_Name => "__cubit_unwind_abort", No_Return;

end CuBit.Unwinder_Support;
