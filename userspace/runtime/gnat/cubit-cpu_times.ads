------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The reader of the kernel's per-CPU times publication
--  (CPU_Times_Publication, KERN-002): a snapshot of every CPU without a
--  system call, for CuBit.CPU_Load.
--
--  @description
--  The page is mapped only into a process whose manifest requests it,
--  (request-publication cpu-times), and whose launcher granted it: the
--  startup table's Publication entry "cpu-times" then holds the page's
--  address. Without that grant nothing here touches the page (it is not
--  mapped; a load would fault): Available is False and Read gives an empty
--  snapshot.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CPU_Times_Publication;
with CuBit.CPU_Load;

package CuBit.CPU_Times is
   package CTP renames CPU_Times_Publication;

   --  The startup table label of the grant (the manifest's request name).
   Label : constant String := "cpu-times";

   --  Granted to this process and published by the kernel.
   function Available return Boolean;

   --  Every published CPU's times at about the same moment; Count 0 when
   --  not Available.
   procedure Read (Into : out CuBit.CPU_Load.Snapshot);

   --  Counter ticks per second (0: unknown or not Available).
   function Tick_Frequency return Unsigned_64;

   --  Free physical memory and the RAM the kernel manages, bytes (0 when
   --  not Available). Free is refreshed by the kernel at most every
   --  CTP.Memory_Refresh_Microseconds while any CPU schedules.
   function Free_Memory_Bytes return Unsigned_64;
   function Memory_Bytes return Unsigned_64;
end CuBit.CPU_Times;
