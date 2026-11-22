------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The test console object: one line of text onto the kernel's serial
--  console, as a Call on a CAP_TEST_CONSOLE handle. Test builds only, held
--  only by logstore: it mirrors the debug text programs publish into their
--  log rings, so headless tests keep reading their serial markers once
--  programs no longer write to the console themselves (SYSCALL_WRITE is
--  retired, ABI-002). Shared by the kernel (Test_Console_Object) and the
--  runtime (CuBit.Handles.Test_Consoles).
--
--  @description
--  A kernel object reached through the ordinary Call: the kernel resolves
--  the handle (type, generation, rights: Write), copies the caller's bytes
--  out of its own memory and prints them, in ring 0 with no process switch,
--  then writes the reply into the caller's message. The text is a
--  process's, never a kernel line: it stays out of the kernel log ring
--  (LOG-004), so logstore never reads back what it mirrored.
--
--  No shipped manifest declares it and no shipped startup plan approves it
--  (tests/test-console checks both, and the shipped images): only the test
--  variant of logstore's manifest (tests/headless/logstore-test) declares
--  (Request.Kernel_Object (Kernel_Object_Request Kernel_Object.Test_Console
--  "test-console")), and only test plans say (test-console
--  approve-declared).
--
--  Request: MessageTag.label = Operation'Enum_Rep, length = Request_Words,
--  words (Address_Word) = the text's address in the caller, words
--  (Count_Word) = its length in bytes (1 .. Maximum_Bytes). Reply: label =
--  the request's, length = Reply_Words, words (Outcome_Word) =
--  Outcome'Enum_Rep.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Test_Console_ABI with Pure, SPARK_Mode is

   --  Write: print the bytes as they are (the caller ends its lines).
   type Operation is (Write);
   for Operation use (Write => 1);

   Address_Word  : constant := 0;
   Count_Word    : constant := 1;
   Request_Words : constant := 2;
   Outcome_Word  : constant := 0;
   Reply_Words   : constant := 1;

   --  The most one Write prints: a record's text (CuBit.Log_Records, 512
   --  bytes) behind its source's name, with room to spare.
   Maximum_Bytes : constant := 1024;
   subtype Byte_Count is Unsigned_64 range 1 .. Maximum_Bytes;

   --  Codes start at 1 and are fixed.
   type Outcome is
     (Done,               --  the bytes were printed
      Malformed,          --  not Request_Words words
      Unknown_Operation,  --  the label is no Operation
      Not_Permitted,      --  the handle lacks Write
      Too_Long,           --  zero bytes, or more than Maximum_Bytes
      Unreadable);        --  the bytes are not the caller's readable memory
   for Outcome use
     (Done              => 1,
      Malformed         => 2,
      Unknown_Operation => 3,
      Not_Permitted     => 4,
      Too_Long          => 5,
      Unreadable        => 6);

   function Outcome_Word_Of (What : Outcome) return Unsigned_64 is
     (Unsigned_64 (Outcome'Enum_Rep (What)));

   function Is_Outcome_Word (Word : Unsigned_64) return Boolean is
     (Word in Outcome_Word_Of (Outcome'First) .. Outcome_Word_Of (Outcome'Last));

   function Is_Operation_Label (Label : Unsigned_64) return Boolean is
     (Label = Unsigned_64 (Operation'Enum_Rep (Write)));

   --  What a request asks: Valid with how many bytes, or the refusal.
   --  Whether the bytes can be read is the kernel's to find out after.
   type Decision (Valid : Boolean := False) is record
      case Valid is
         when True  => Count : Byte_Count;
         when False => Why   : Outcome;
      end case;
   end record;

   --  Writable: the handle carries Write. Checked in this order: rights,
   --  operation, words, then the byte count.
   function Decide
     (Writable : Boolean; Label : Unsigned_64; Length : Unsigned_64; Bytes : Unsigned_64)
      return Decision
   is (if not Writable then (Valid => False, Why => Not_Permitted)
       elsif not Is_Operation_Label (Label) then (Valid => False, Why => Unknown_Operation)
       elsif Length /= Request_Words then (Valid => False, Why => Malformed)
       elsif Bytes not in Byte_Count then (Valid => False, Why => Too_Long)
       else (Valid => True, Count => Bytes))
   with Post => (if Decide'Result.Valid
                 then Writable and then Length = Request_Words
                      and then Is_Operation_Label (Label)
                      and then Decide'Result.Count = Bytes
                      and then Bytes in 1 .. Maximum_Bytes
                 else Decide'Result.Why /= Done);

end Test_Console_ABI;
