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
--  (CuBit.Grant_References wire form), 2 = the arena's bytes, 3 = the
--  dirty arena grant (Dirty_Arena_Bytes), or 0 for none. Replies
--  REPLY_OK. One queue per client process.
--  OP_FS_KICK (one-way): the client produced requests while the service's
--  wake word was armed.
--  OP_FS_WAIT (submit): completes once answers wait in the client's queue.
--
--  Queue grant: a header page (submission header at Submissions_At,
--  completion header at Completions_At; words as CuBit.Frame_Rings), then
--  Slots requests of Request_Bytes at Requests_At and Slots answers of
--  Answer_Bytes at Answers_At, then the delegation pages (one entry per
--  service handle slot) at Delegations_At.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Submission_Queues;

package CuBit.Filesystem_Queues with Pure, SPARK_Mode is

   OP_FS_QUEUE : constant := 16#0020#;
   OP_FS_KICK  : constant := 16#0021#;
   OP_FS_WAIT  : constant := 16#0022#;

   Queue_Bytes    : constant := 77_824;       --  3 pages + delegations
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
   --  The namespace generation (32 bits, in the submission header page):
   --  the service moves it on before any change that could alter what a
   --  name resolves to or whether this client may open it (unlink,
   --  rename, rmdir, mkdir, a change to the client's access policy). A
   --  client reuses a closed ("parked") handle for a name only while it
   --  is unchanged since the handle was parked.
   Namespace_Generation_At : constant := 72;

   --  Read delegations (docs/filesystem-data-plane.md), written by the
   --  service in the header page, one entry per file handle slot (a
   --  handle's low 32 bits are its slot + 1). While an entry's valid word
   --  is nonzero, no other handle can write the file and the client may
   --  answer reads from its cache: it checks the word before and after
   --  copying from the cache. The service clears it before any write,
   --  resize or writable open by another handle takes effect. The version
   --  changes with every change to the file, so a reopen may keep cached
   --  pages of an unchanged version.
   Delegations_At        : constant := 12_288;
   Delegation_Bytes      : constant := 32;
   Maximum_Delegations   : constant := 2_048;
   Delegation_Valid_At   : constant := 0;
   Delegation_Mode_At    : constant := 4;    --  Read_/Write_Delegation
   Delegation_Inode_At   : constant := 8;    --  volume << 32 | inode
   Delegation_Version_At : constant := 16;
   Delegation_Size_At    : constant := 24;
   Read_Delegation  : constant := 1;
   --  The handle is the file's only one: the client may also keep written
   --  pages in its dirty arena, which the service harvests on close, on
   --  flush, on write-back, and before any other handle opens the file.
   Write_Delegation : constant := 2;

   --  The dirty arena (docs/filesystem-data-plane.md, "Write delegations"):
   --  Dirty_Entries entries of Dirty_Entry_Bytes, then as many pages. The
   --  client makes an entry's sequence word odd while it writes the page
   --  and even (nonzero) when done; the service takes an entry whose even
   --  word stays unchanged across its copy and frees it by setting the word
   --  to zero (compare-and-swap on both sides). The service writes only the
   --  entry's bytes Start .. Stop - 1 of its page.
   Dirty_Entries       : constant := 2_048;
   Dirty_Entry_Bytes   : constant := 16;
   Dirty_Pages_At      : constant := 32_768;       --  after the entries
   Dirty_Page_Bytes    : constant := 4_096;
   Dirty_Arena_Bytes   : constant := 8_421_376;    --  entries and pages
   Dirty_Sequence_At   : constant := 0;    --  32 bits
   --  32 bits: the handle's tag, its slot in the low Tag_Slot_Bits and the
   --  low Tag_Generation_Bits of its generation above: an entry of a
   --  released handle never matches the slot's next handle.
   Dirty_Slot_At       : constant := 4;
   Tag_Slot_Bits       : constant := 11;
   Tag_Generation_Bits : constant := 21;
   Dirty_Page_At       : constant := 8;    --  32 bits: page within the file
   Dirty_Start_At      : constant := 12;   --  16 bits
   Dirty_Stop_At       : constant := 14;   --  16 bits

   --  Operations.
   Queue_Open     : constant := 1;   --  Options, path at Arena_Offset, Length
   Queue_Close    : constant := 2;   --  Handle
   Queue_Read_At  : constant := 3;   --  Handle, Position, Length, Arena_Offset
   Queue_Write_At : constant := 4;   --  Handle, Position, Length, Arena_Offset
   Queue_Flush    : constant := 5;   --  Handle (after its dirty pages)
   Queue_Writeback : constant := 6;  --  Handle: harvest its dirty pages
   --  path at Arena_Offset, Length; Handle: 0, or the client's parked
   --  handle for the name, closed with the unlink (its buffered pages
   --  dropped if it alone held the file that dies). The service checks
   --  the handle against its own table (owner, file, sole holder) and
   --  finds its buffered entries itself.
   Queue_Unlink   : constant := 7;
   Queue_Mkdir    : constant := 8;   --  path at Arena_Offset, Length
   Queue_Rmdir    : constant := 9;   --  path at Arena_Offset, Length
   --  Handle (a directory), Length = bytes of Directory.Page.V1 pages
   --  wanted at Arena_Offset; the answer's value is the pages filled (the
   --  last one ends the directory if its END flag is set).
   Queue_Read_Directory : constant := 10;
   Queue_Open_Directory  : constant := 11;   --  path at Arena_Offset, Length
   Queue_Close_Directory : constant := 12;   --  Handle
   --  Handle (a file that may read): keep it open for a later read-only
   --  open of its name ("parked"). Its rights drop to reading; it keeps
   --  its delegation (a write delegation's pages are harvested later, as
   --  on write-back), or is delegated as a reader's would be. A handle
   --  that cannot read is closed instead (answer REPLY_ERR).
   Queue_Park : constant := 13;

   --  Byte offsets within a request and an answer (the token first).
   Token_At        : constant := 0;
   Operation_At    : constant := 8;
   Options_At      : constant := 12;
   Handle_At       : constant := 16;
   Position_At     : constant := 24;
   Length_At       : constant := 32;
   Arena_Offset_At : constant := 40;
   Status_At       : constant := 8;    --  the reply label (REPLY_OK, ...)
   --  An open's answer: Rights_Write if the handle may write; Rights_Read
   --  if the client's policy lets it read the file, even when the handle
   --  was opened write-only (which still cannot read): only such a handle
   --  may be parked (Queue_Park), which leaves it read rights alone.
   Rights_At       : constant := 12;
   Rights_Read     : constant := 1;
   Rights_Write    : constant := 2;
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
      Reserved at Rights_At - Status_At range 0 .. Word_Bits - 1;
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
      Namespace_Generation_At < Wake_At + 4 or else
      Namespace_Generation_At + 4 > Completions_At or else
      Delegations_At < Answers_At + Slots * Answer_Bytes or else
      Delegations_At mod 4_096 /= 0 or else
      Delegations_At + Maximum_Delegations * Delegation_Bytes /=
        Queue_Bytes or else
      Dirty_Pages_At /= Dirty_Entries * Dirty_Entry_Bytes or else
      Dirty_Arena_Bytes /= Dirty_Pages_At + Dirty_Entries * Dirty_Page_Bytes,
      "filesystem queue layout");

   --  A request's arena range lies inside an arena of Arena_Bytes.
   function In_Arena
     (Offset, Length, Arena_Bytes : Unsigned_64) return Boolean is
     (Length <= Arena_Bytes and then Offset <= Arena_Bytes - Length);

end CuBit.Filesystem_Queues;
