------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The process-list object: the kernel's process table, one 32-byte entry
--  per process (CuBit.Process_List decodes them), as a Call on a
--  CAP_PROCESS_LIST handle. Replaces SYSCALL_PROCLIST (71), whose gate
--  accepted any process handle with Read, so every process (holding one
--  for itself) could list them all.
--
--  @description
--  A kernel object reached through the ordinary Call: the kernel resolves
--  the handle (type, generation, rights: Read) and copies the entries into
--  the caller's own memory, with no process switch. The kernel mints one
--  for devmgr, which passes it to procmgr, its only holder; other programs
--  ask procmgr's process-observer role.
--
--  Request: MessageTag.label = Operation'Enum_Rep, length = Request_Words,
--     words (Destination_Word) the caller's buffer, words (Capacity_Word)
--     its size in bytes (whole entries are written; Entry_Bytes each).
--  Reply: label = the request's, length = Reply_Words, words (Outcome_Word)
--     an Outcome, words (Count_Word) the entries written when Listed.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Process_List_ABI with Pure, SPARK_Mode is

   type Operation is (List);
   for Operation use (List => 1);

   Entry_Bytes      : constant := 32;
   Destination_Word : constant := 0;
   Capacity_Word    : constant := 1;
   Request_Words    : constant := 2;
   Outcome_Word     : constant := 0;
   Count_Word       : constant := 1;
   Reply_Words      : constant := 2;

   --  Codes start at 1 and are fixed.
   type Outcome is
     (Listed,                  --  the entries were written (Count_Word)
      Malformed,               --  wrong length
      Unknown_Operation,       --  the label is no Operation
      Not_Permitted,           --  the handle lacks Read
      Destination_Unwritable); --  the buffer is not the caller's writable memory
   for Outcome use
     (Listed => 1, Malformed => 2, Unknown_Operation => 3, Not_Permitted => 4,
      Destination_Unwritable => 5);

   function Outcome_Word_Of (What : Outcome) return Unsigned_64 is
     (Unsigned_64 (Outcome'Enum_Rep (What)));

   function Is_Outcome_Word (Word : Unsigned_64) return Boolean is
     (Word in Outcome_Word_Of (Outcome'First) .. Outcome_Word_Of (Outcome'Last));

   --  What a request asks, or why not. Checked in this order: rights,
   --  operation, length.
   function Refusal
     (Readable : Boolean; Label : Unsigned_64; Length : Unsigned_64) return Outcome is
     (if not Readable then Not_Permitted
      elsif Label /= Unsigned_64 (Operation'Enum_Rep (List)) then Unknown_Operation
      elsif Length /= Request_Words then Malformed
      else Listed);

   --  Entries that fit Capacity bytes.
   function Entries_In (Capacity : Unsigned_64) return Unsigned_64 is
     (Capacity / Entry_Bytes);

end Process_List_ABI;
