pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Log_Records;

--  The kernel log ring (Kernel_Log_Publication, LOG-004) drained into the
--  store: the kernel's console lines under the "kernel" source
--  (Kernel_Log_Publication.Kernel_Source), and the summary of each
--  process the kernel stopped (Fault_Reports) under the process's own
--  source, as an Error reading "com.cubit.hello: stopped by a page fault
--  (write); details restricted" with its kind, access and mode as fields,
--  so (logs.recent "kernel") and (logs.recent "<program>") find them. A
--  fault's detail (where, anchored addresses) is for diagnostics readers
--  only, and the store has none: it stays in the kernel ring. Every
--  record keeps the kernel's time and the kernel's order, including those
--  written before logstore started (the ring starts with the boot).
--
--  Logstore calls Poll every pass of its loop; it reads the pages the kernel
--  mapped into it at registration (read-only), never waits and never calls
--  the kernel. A pass takes at most Budget records (More: call again soon).
--  Records the kernel overwrote before they were read become one Warning
--  per run, "kernel log: N records dropped (sequence a..b)", placed before
--  the record that follows the run.
--
--  Nothing is echoed to the debug console: every kernel line is already on
--  the serial console, and a process's output was printed there too.
generic
   --  A record for the store.
   with procedure Publish
     (Source : Unsigned_64; Time_Ms : Unsigned_64;
      Level : CuBit.Log_Records.Severity; Text : String);
   --  A record with typed fields for the store.
   with procedure Publish_Record
     (Source : Unsigned_64; Time_Ms : Unsigned_64; Value : CuBit.Log_Records.Log_Record);
   --  Source is a process the kernel named Name: make sure readers can
   --  find its records by that name (a process logstore never heard of).
   with procedure Learn (Source : Unsigned_64; Name : String);
   --  How a reader knows Source (its manifest identity), else Fallback.
   with function Who (Source : Unsigned_64; Fallback : String) return String;
package Kernel_Log_Reader is
   Budget : constant := 256;
   procedure Poll (Now_Ms : Unsigned_64; More : out Boolean);
   --  The first time Poll has taken every record there was, it says on the
   --  debug console how many it took, how many were dropped, and the
   --  earliest record's time against logstore's own start (Started_Ms).
   procedure Set_Started (Started_Ms : Unsigned_64);
end Kernel_Log_Reader;
