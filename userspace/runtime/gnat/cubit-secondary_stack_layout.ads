------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Where a thread's secondary stack lives and how large it may grow (the
--  user runtime's System.Secondary_Stack, CuBit.Thread_Areas).
--
--  @description
--  A thread's secondary stack is upstream GNAT's: a stack object holding a
--  static chunk, then dynamic chunks on CuAlloc as its top needs them, each
--  max (request, Chunk_Bytes) bytes, kept for reuse after a release and
--  given back when the thread ends. All of one thread's chunks together
--  never pass Ceiling_Bytes; a request that would is refused before any
--  memory is touched, with Storage_Error and a CAPACITY message.
--
--  A thread area (CuBit.Thread_Areas) is one owned-memory region, laid out
--  from its base as
--
--     [prefix][stack object: header and static chunk]
--
--  The prefix is the thread's static TLS block and thread control block
--  when the runtime made the thread pointer itself; threads whose TLS
--  another runtime owns (musl, Rust) have no area, and their stack object
--  is a CuAlloc block. The whole area is committed when it is made: it
--  never grows (the stack grows on CuAlloc).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Secondary_Stack_Layout with Pure, SPARK_Mode is

   --  CuBit.Kernel_ABI.Page_Bytes.
   Page_Bytes : constant := 4096;

   --  Every thread's static chunk, and the smallest dynamic chunk: twice
   --  the fixed 32 KiB stack the runtime once had, which nearly every
   --  program stays within, so most threads never touch the heap for it.
   Chunk_Bytes : constant := 64 * 1024;

   --  The named ceiling: the most one thread's secondary-stack chunks may
   --  hold together, 256 MiB, the same reach as the largest primary stack
   --  a program may declare (kernel Process.MAX_USER_STACK_SIZE). The RAM
   --  really used is also bounded by the process's frame quota, which
   --  makes CuAlloc refuse (HEAP_REFUSED) whatever this constant says.
   Ceiling_Bytes : constant := 256 * 1024 * 1024;

   --  The largest prefix (static TLS and thread control block) an area
   --  holds. Programs' static TLS is a few words; a larger one is refused.
   Maximum_Prefix_Bytes : constant := 1024 * 1024;

   --  Where the stack object starts after the prefix: a multiple of this
   --  (at least the target's maximum alignment).
   Object_Alignment : constant := 64;

   --  The largest stack object an area holds: the static chunk and room for
   --  the header (System.Secondary_Stack.SS_Object_Bytes, which the runtime
   --  checks against it).
   Maximum_Header_Bytes : constant := 256;
   Maximum_Object_Bytes : constant := Chunk_Bytes + Maximum_Header_Bytes;

   --  The largest area.
   Maximum_Area_Bytes : constant :=
     Maximum_Prefix_Bytes + Object_Alignment + Maximum_Object_Bytes + Page_Bytes;

   --  Offsets and sizes within one area.
   subtype Region_Bytes is Unsigned_64 range 0 .. Maximum_Area_Bytes;
   subtype Prefix_Bytes is Unsigned_64 range 0 .. Maximum_Prefix_Bytes;
   subtype Object_Bytes is Unsigned_64 range 1 .. Maximum_Object_Bytes;

   function Page_Aligned (Bytes : Region_Bytes) return Boolean is
     (Bytes mod Page_Bytes = 0);

   --  Bytes rounded up to whole pages.
   function Page_Round (Bytes : Region_Bytes) return Region_Bytes is
     ((Bytes + Page_Bytes - 1) / Page_Bytes * Page_Bytes)
   with Pre  => Bytes <= Maximum_Area_Bytes - Page_Bytes + 1,
        Post => Page_Aligned (Page_Round'Result)
                  and then Page_Round'Result >= Bytes
                  and then Page_Round'Result - Bytes < Page_Bytes;

   --  Where the stack object starts: the prefix rounded up to
   --  Object_Alignment.
   function Object_Offset (Prefix : Prefix_Bytes) return Region_Bytes is
     ((Prefix + Object_Alignment - 1) / Object_Alignment * Object_Alignment)
   with Post => Object_Offset'Result >= Prefix
                  and then Object_Offset'Result mod Object_Alignment = 0
                  and then Object_Offset'Result - Prefix < Object_Alignment;

   --  The whole area: the prefix and the stack object, in whole pages.
   function Area_Bytes (Prefix : Prefix_Bytes; Object : Object_Bytes) return Region_Bytes is
     (Page_Round (Object_Offset (Prefix) + Object))
   with Post => Area_Bytes'Result > 0
                  and then Page_Aligned (Area_Bytes'Result)
                  and then Area_Bytes'Result >= Object_Offset (Prefix) + Object
                  and then Area_Bytes'Result <= Maximum_Area_Bytes;

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
