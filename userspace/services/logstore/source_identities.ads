pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Log_Sources;
with Log_Fanout;

--  Who each log source is (LOG-003): what the launchers' registrar notices
--  said of each process as it started (CuBit.Log_Sources), kept past its
--  exit so its records can still be found by name.
--
--  Sized by the input and bounded by retention: an entry is kept while its
--  process runs, and after it exits while the broker still retains any of
--  its records (Log_Fanout's per-source histories, LOG-002); then it goes.
--  The table grows on the process heap (CuAlloc) as launches need it; a
--  refused growth gives the notice up and counts it (Lost).
package Source_Identities is
   type Table is limited private;

   --  A registrar said Who started (a repeated notice replaces the earlier
   --  entry).
   procedure Started (Item : in out Table; Who : CuBit.Log_Sources.Source_Identity; Now_Ms : Unsigned_64)
   with Pre => CuBit.Log_Sources.Valid (Who) and then CuBit.Log_Sources.Named (Who);
   --  A registrar said Source exited.
   procedure Exited (Item : in out Table; Source : Unsigned_64; Now_Ms : Unsigned_64);

   --  How long an exited source's entry outlives its last retained record
   --  at least: its final records may still be in its publisher channel
   --  when its exit is reported.
   Exit_Grace_Ms : constant := 5_000;
   --  Forget exited sources whose records Store no longer retains, once
   --  Exit_Grace_Ms has passed since their exit.
   procedure Prune (Item : in out Table; Store : Log_Fanout.Broker; Now_Ms : Unsigned_64);

   --  Sources known now, and each by position, newest first (the latest
   --  start at 1).
   function Count (Item : Table) return Natural;
   function Newest (Item : Table; Position : Positive) return CuBit.Log_Sources.Source_Entry
   with Pre => Position <= Count (Item);

   --  Whether an entry for Source is kept.
   function Knows (Item : Table; Source : Unsigned_64) return Boolean;
   --  Source's identity (com.cubit.hello), else its name, else "".
   function Identity_Of (Item : Table; Source : Unsigned_64) return String;

   --  Notices given up for want of memory.
   function Lost (Item : Table) return Unsigned_64;
private
   type Known is record
      Listed    : CuBit.Log_Sources.Source_Entry;
      Exited_Ms : Unsigned_64 := 0;
   end record;
   type Known_Array is array (Positive range <>) of Known;
   No_Block : constant Unsigned_64 := 0;
   --  Block: Capacity entries from the process heap (No_Block: none yet);
   --  the first Used of them, oldest first.
   type Table is limited record
      Block    : Unsigned_64 := No_Block;
      Capacity : Natural := 0;
      Used     : Natural := 0;
      Given_Up : Unsigned_64 := 0;
   end record;
end Source_Identities;
