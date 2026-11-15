------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Fixed addresses in every user address space that the kernel and the
--  user runtimes agree on.
--
--  @description
--  The kernel (Process) checks its own constants against these when it is
--  compiled, so the runtime needs no system call to learn them.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use type Interfaces.Unsigned_64;

package Process_Layout with Pure, SPARK_Mode is

   --  Top of the main thread's stack, exclusive.
   Stack_Top : constant := 16#0000_8000_0000_0000#;
   --  The main thread's initial stack (the bootstrap process's; ELF
   --  programs declare theirs in PT_GNU_STACK). No secondary stack lives
   --  in any primary stack: each thread's is its own owned-memory
   --  reservation (CuBit.Thread_Areas).
   Initial_Stack_Bytes : constant := 16 * 1024 * 1024;

   --  The device-mapping arena: the user virtual range a program places its
   --  own MAP_DEVICE mappings in (driver BARs, firmware windows, the
   --  platform-register pool's regions for AML). The kernel puts nothing
   --  of its own here: received grants, owned memory, the launch argument
   --  block, the boot image and the clock page all lie below
   --  Device_Mapping_First (the kernel checks this when it is compiled),
   --  and the main stack lies above Device_Mapping_Limit (checked below).
   --  A program partitions the arena among its own users; the kernel
   --  still refuses a mapping over a page already mapped.
   Device_Mapping_First : constant := 16#0000_6000_0000_0000#;
   Device_Mapping_Limit : constant := 16#0000_7000_0000_0000#;

   --  Whether [Base, Base + Bytes) lies inside the arena. Empty, wrapping
   --  or out-of-arena requests are False (fail closed: no permissive default).
   function In_Device_Mapping_Arena (Base, Bytes : Interfaces.Unsigned_64) return Boolean is
     (Bytes > 0
      and then Base >= Device_Mapping_First
      and then Base < Device_Mapping_Limit
      and then Bytes <= Device_Mapping_Limit - Base);

   pragma Compile_Time_Error
     (Stack_Top - Initial_Stack_Bytes < Device_Mapping_Limit,
      "the main thread's stack must lie above the device-mapping arena");

end Process_Layout;
