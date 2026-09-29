------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The filesystem service's request queue (docs/filesystem-data-plane.md):
--  a queue pair a client lends once (OP_FS_QUEUE), with a transfer arena
--  its requests name data in, instead of a message and a grant per
--  request. The byte layout is the one C clients use (cubit_fs_queue.h).
--
--  OP_FS_QUEUE (call): words 0 = the queue grant, 1 = the arena grant
--  (CuBit.Grant_References wire form), 2 = the arena's bytes. Replies
--  REPLY_OK. One queue per client process.
--  OP_FS_KICK (one-way): the client produced requests while the service's
--  wake word was armed.
--  OP_FS_WAIT (submit): completes once answers wait in the client's queue.
--
--  Queue grant: a header page (submission header at Submissions_At,
--  completion header at Completions_At; words as CuBit.Frame_Rings), then
--  Slots requests of Request_Bytes at Requests_At and Slots answers of
--  Answer_Bytes at Answers_At.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Submission_Queues;

package CuBit.Filesystem_Queues with Pure, SPARK_Mode is

   OP_FS_QUEUE : constant := 16#0020#;
   OP_FS_KICK  : constant := 16#0021#;
   OP_FS_WAIT  : constant := 16#0022#;

   Queue_Bytes    : constant := 12_288;
   Slot_Bits      : constant := 6;
   Slots          : constant := 64;
   Request_Bytes  : constant := 64;
   Answer_Bytes   : constant := 32;
   Submissions_At : constant := 0;
   Completions_At : constant := 2_048;
   Requests_At    : constant := 4_096;
   Answers_At     : constant := 8_192;
   --  Header words (as CuBit.Frame_Rings).
   Produced_At : constant := 0;
   Consumed_At : constant := 64;
   Wake_At     : constant := 68;

   --  Read delegations (docs/filesystem-data-plane.md), written by the
   --  service in the header page, one entry per file handle slot (a
   --  handle's low 32 bits are its slot + 1). While an entry's valid word
   --  is nonzero, no other handle can write the file and the client may
   --  answer reads from its cache: it checks the word before and after
   --  copying from the cache. The service clears it before any write,
   --  resize or writable open by another handle takes effect. The version
   --  changes with every change to the file, so a reopen may keep cached
   --  pages of an unchanged version.
   Delegations_At        : constant := 256;
   Delegation_Bytes      : constant := 32;
   Maximum_Delegations   : constant := 32;
   Delegation_Valid_At   : constant := 0;
   Delegation_Inode_At   : constant := 8;    --  volume << 32 | inode
   Delegation_Version_At : constant := 16;
   Delegation_Size_At    : constant := 24;

   --  Operations.
   Queue_Open     : constant := 1;   --  Options, path at Arena_Offset, Length
   Queue_Close    : constant := 2;   --  Handle
   Queue_Read_At  : constant := 3;   --  Handle, Position, Length, Arena_Offset
   Queue_Write_At : constant := 4;   --  Handle, Position, Length, Arena_Offset
   Queue_Flush    : constant := 5;   --  Handle

   --  Byte offsets within a request and an answer (the token first).
   Token_At        : constant := 0;
   Operation_At    : constant := 8;
   Options_At      : constant := 12;
   Handle_At       : constant := 16;
   Position_At     : constant := 24;
   Length_At       : constant := 32;
   Arena_Offset_At : constant := 40;
   Status_At       : constant := 8;    --  the reply label (REPLY_OK, ...)
   Value_At        : constant := 16;   --  the reply's word 0

   Word_Bits : constant := 32;
   Long_Bits : constant := 64;

   --  A request after its token.
   type Request is record
      Operation    : Unsigned_32 := 0;
      Options      : Unsigned_32 := 0;
      Handle       : Unsigned_64 := 0;
      Position     : Unsigned_64 := 0;
      Length       : Unsigned_64 := 0;
      Arena_Offset : Unsigned_64 := 0;
      Spare_1      : Unsigned_64 := 0;
      Spare_2      : Unsigned_64 := 0;
   end record;
   for Request use record
      Operation    at Operation_At - Operation_At range 0 .. Word_Bits - 1;
      Options      at Options_At - Operation_At range 0 .. Word_Bits - 1;
      Handle       at Handle_At - Operation_At range 0 .. Long_Bits - 1;
      Position     at Position_At - Operation_At range 0 .. Long_Bits - 1;
      Length       at Length_At - Operation_At range 0 .. Long_Bits - 1;
      Arena_Offset at Arena_Offset_At - Operation_At range 0 .. Long_Bits - 1;
      Spare_1      at Arena_Offset_At - Operation_At + 8
        range 0 .. Long_Bits - 1;
      Spare_2      at Arena_Offset_At - Operation_At + 16
        range 0 .. Long_Bits - 1;
   end record;
   for Request'Size use (Request_Bytes - 8) * 8;
   for Request'Alignment use 8;

   --  An answer after its token.
   type Answer is record
      Status   : Unsigned_32 := 0;
      Reserved : Unsigned_32 := 0;
      Value    : Unsigned_64 := 0;
      Spare    : Unsigned_64 := 0;
   end record;
   for Answer use record
      Status   at Status_At - Status_At range 0 .. Word_Bits - 1;
      Reserved at Status_At - Status_At + 4 range 0 .. Word_Bits - 1;
      Value    at Value_At - Status_At range 0 .. Long_Bits - 1;
      Spare    at Value_At - Status_At + 8 range 0 .. Long_Bits - 1;
   end record;
   for Answer'Size use (Answer_Bytes - 8) * 8;
   for Answer'Alignment use 8;

   package Queues is new CuBit.Submission_Queues
     (Request, Answer, Slot_Bits, Slot_Bits);

   pragma Compile_Time_Error
     (Queues.Submission'Size /= Request_Bytes * 8 or else
      Queues.Completion'Size /= Answer_Bytes * 8,
      "filesystem queue entries must be Request_Bytes and Answer_Bytes");
   pragma Compile_Time_Error
     (Queues.Submissions.Slots /= Slots or else
      Requests_At + Slots * Request_Bytes > Answers_At or else
      Answers_At + Slots * Answer_Bytes > Queue_Bytes or else
      Delegations_At < Wake_At + 4 or else
      Delegations_At + Maximum_Delegations * Delegation_Bytes >
        Completions_At,
      "filesystem queue layout");

   --  A request's arena range lies inside an arena of Arena_Bytes.
   function In_Arena
     (Offset, Length, Arena_Bytes : Unsigned_64) return Boolean is
     (Length <= Arena_Bytes and then Offset <= Arena_Bytes - Length);

end CuBit.Filesystem_Queues;
