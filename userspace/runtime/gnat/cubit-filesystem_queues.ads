------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The filesystem service's request queue (docs/filesystem-data-plane.md):
--  a queue pair a client opens once, with a transfer arena its requests
--  name data in, instead of a message and a grant per request. The byte
--  layout is the one C clients use (cubit_fs_queue.h).
--
--  The client opens three channels on the filesystem endpoint
--  (CuBit.Channels, docs/data-plane.md), lending what it owns:
--    Transfer_Connector: the transfer arena (TRANSFER_CONTRACT), which both
--      sides write: request data, read results.
--    Dirty_Connector (optional): the dirty arena (DIRTY_CONTRACT); without
--      it, no write delegations.
--    Event_Connector (optional, after the queue pair): the event ring
--      (EVENT_CONTRACT), which the client opens consuming: the service
--      produces CuBit.Filesystem_Events records for its watches
--      (Queue_Watch) into memory it owns, granted read-only.
--    Queue_Connector: the queue pair (QUEUE_CONTRACT, duplex). The
--      client's region holds the requests and the indices it writes; the
--      service's region (granted back, read-only) the answers, the indices
--      it writes, its wake word, the namespace generation and the
--      delegations. Each side writes only its own region.
--  One queue per client process. Kicks are the channel protocol's OP_KICK
--  on the queue's channel number.
--  OP_FS_WAKE, a wake request (docs/filesystem-protocol-v2.md step 1),
--  is answered (REPLY_OK) once answers wait in the client's queue or
--  records in its event ring, at once if some already do. Submitted asynchronously (capSubmit), its completion
--  wakes the client's own event loop (Wait_For_Activity_Until), so a UI
--  thread never blocks in the service; called, it is the blocking wait.
--  The service holds one per queue: a newer one supersedes it (the held one
--  is answered first), and the queue's end answers it REPLY_ERR. It has no
--  timeout of its own: the deadline is the waiting client's.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Channel_Contracts;
with CuBit.Protocols;
with CuBit.Submission_Queues;

package CuBit.Filesystem_Queues with Pure, SPARK_Mode is

   OP_FS_WAKE  : constant := 16#0022#;

   Queue_Connector    : constant := 1;
   Transfer_Connector : constant := 2;
   Dirty_Connector    : constant := 3;
   Event_Connector    : constant := 4;

   Slot_Bits      : constant := 6;
   Slots          : constant := 64;
   Request_Bytes  : constant := 64;
   Answer_Bytes   : constant := 32;
   Page_Bytes     : constant := 4_096;

   --  The client's region: what the client writes (indices as
   --  CuBit.Slot_Rings, free-running entry counts, 32 bits).
   Client_Pages       : constant := 2;
   Client_Submitted_At : constant := 0;      --  requests produced
   Client_Reaped_At    : constant := 64;     --  answers consumed
   Client_Requests_At  : constant := 4_096;

   --  The service's region: what the service writes.
   Server_Answered_At  : constant := 0;      --  answers produced
   Server_Taken_At     : constant := 64;     --  requests consumed
   --  Nonzero while the service sleeps and wants a kick for new requests;
   --  it changes each time it arms, so one kick answers one arming.
   Server_Wake_At      : constant := 128;
   --  The namespace generation (32 bits): the service moves it on before
   --  any change that could alter what a name resolves to or whether this
   --  client may open it (unlink, rename, rmdir, mkdir, a change to the
   --  client's access policy). A client reuses a closed ("parked") handle
   --  for a name only while it is unchanged since the handle was parked.
   Server_Namespace_At : constant := 192;
   --  Server-side copies' progress (Queue_Copy): Maximum_Copies entries
   --  of Copy_Entry_Bytes, each the copy's request token and the bytes it
   --  has copied (monotonic). The service writes the count, then the
   --  token, when a copy starts; the token 0 when it ends. A reader takes
   --  the token, the count and the token again, and keeps the count only
   --  if both tokens are its copy's.
   Server_Copies_At    : constant := 256;
   Copy_Entry_Bytes    : constant := 16;
   Copy_Token_At       : constant := 0;
   Copy_Done_At        : constant := 8;
   Maximum_Copies      : constant := 4;   --  per client; past it, REPLY_BUSY
   Server_Answers_At   : constant := 4_096;

   --  Read delegations (docs/filesystem-data-plane.md), written by the
   --  service in the header page, one entry per file handle slot (a
   --  handle's low 32 bits are its slot + 1). While an entry's valid word
   --  is nonzero, no other handle can write the file and the client may
   --  answer reads from its cache: it checks the word before and after
   --  copying from the cache. The service clears it before any write,
   --  resize or writable open by another handle takes effect. The version
   --  changes with every change to the file, so a reopen may keep cached
   --  pages of an unchanged version.
   Delegations_At        : constant := 8_192;    --  in the service's region
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
   --  Handle (a directory), Length = bytes of Directory.Page.V2 pages
   --  (CuBit.Directory_Pages) wanted at Arena_Offset, Options =
   --  Directory_Metadata to fill each entry's metadata (else only its kind
   --  and object); the answer's value is the pages filled (the last one
   --  ends the directory if its Page_End flag is set). Each page's resume
   --  token continues the listing after it (Queue_Seek_Directory).
   Queue_Read_Directory : constant := 10;
   Directory_Metadata : constant := 1;
   Queue_Open_Directory  : constant := 11;   --  path at Arena_Offset, Length
   Queue_Close_Directory : constant := 12;   --  Handle
   --  Handle (a file that may read): keep it open for a later read-only
   --  open of its name ("parked"). Its rights drop to reading; it keeps
   --  its delegation (a write delegation's pages are harvested later, as
   --  on write-back), or is delegated as a reader's would be. A handle
   --  that cannot read is closed instead (answer REPLY_ERR).
   Queue_Park : constant := 13;
   --  Handle (an open file or directory): one Directory.Inspection.V1
   --  record of its object at Arena_Offset (Length at least
   --  CuBit.Filesystems.DIRECTORY_INSPECTION_BYTES), as fstat needs. A
   --  write-delegated handle's buffered pages are taken in first, so the
   --  size and times include them.
   Queue_Describe : constant := 15;
   --  Handle (a file that may write), Length = its new size (truncate,
   --  ftruncate). Queued, it follows the client's earlier writes.
   Queue_Resize : constant := 16;
   --  The old path and then the new path at Arena_Offset; Length = both,
   --  Position = the old path's length (1 .. Length - 1). As OP_RENAME
   --  (CuBit.Filesystems.Rename_Request): POSIX rename or move within one
   --  volume, REPLY_CROSS_VOLUME across volumes (the client copies).
   Queue_Rename : constant := 17;
   --  Handle (a directory), Position = a resume token from one of its pages
   --  (0: the start): the next read continues after that page. A token of
   --  another directory is refused (REPLY_OUT_OF_RANGE); one whose entry
   --  has since gone resumes at the next entry still there.
   Queue_Seek_Directory : constant := 18;
   --  Handle (a directory the client may read), Options = Watch_Subtree to
   --  watch everything below it too: changes to its entries arrive as
   --  CuBit.Filesystem_Events records in the client's event ring (opened
   --  first; REPLY_ERR without it). The answer's value is the watch number.
   --  REPLY_NO_SPACE: the client's watches are all in use, or its ring has
   --  no room for one more watch's reserve. The watch names the folder by
   --  its path; it outlives the handle.
   Queue_Watch : constant := 19;
   Watch_Subtree : constant := 1;
   --  Handle = a watch number: it ends; its last record is a Watch_Ended
   --  (REPLY_ERR: not a live watch). The number is reused only after the
   --  client has read that record.
   Queue_Unwatch : constant := 20;
   --  The caller's own access profile (docs/filesystem-protocol-v2.md step
   --  6) into the arena range as CuBit.File_Access wire entries
   --  (Wire_Entry_Bytes each: rights, prefix length, prefix), as Decode
   --  takes them. The answer's value is the entry count: REPLY_OK, written;
   --  REPLY_NO_SPACE, the range holds fewer and nothing was written. A
   --  bootstrap wildcard profile is one entry with an empty prefix.
   Queue_List_Scopes : constant := 21;
   --  Free space (step 7): a scoped path the caller may read, Position =
   --  its length, at the start of the arena range; the range (Length, at
   --  least Volume_Description_Bytes) then receives one
   --  Volume.Description.V1 record (CuBit.Volume_Descriptions) of the
   --  path's volume. The answer's value is the record's length.
   Queue_Describe_Volume : constant := 22;
   Volume_Description_Bytes : constant := 104;
   --  Server-side copy (step 5): Handle = the source (a file handle that
   --  may read: ext2, ISO 9660 or the boot archive), Spare_1 = the target
   --  (one that may write, ext2, any volume), Position = the source offset,
   --  Arena_Offset = the target offset (no arena is used), Length = bytes
   --  or Copy_To_End (to the source's end), Spare_2 = an absolute
   --  monotonic deadline in ms (CuBit.Messages.Wait_Forever spelled out
   --  for none). Answered once, when it ends: REPLY_OK, REPLY_CANCELLED or
   --  REPLY_DEADLINE, or an error, with the bytes copied (always a prefix
   --  of the range; a partly copied target is the client's to remove).
   --  Progress meanwhile: Server_Copies_At. REPLY_BUSY: Maximum_Copies are
   --  running. One file into itself only between ranges that cannot
   --  overlap (REPLY_ERR otherwise).
   Queue_Copy : constant := 23;
   Copy_To_End : constant Unsigned_64 := Unsigned_64'Last;
   --  Handle = a running copy's request token: it ends REPLY_CANCELLED at
   --  its next slice (this request: REPLY_OK; REPLY_NOT_FOUND if none).
   Queue_Cancel : constant := 24;

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
   --  Whether the client's policy would let it write the file, or create
   --  in the directory, whatever the handle was opened for (access(W_OK)).
   --  Directory opens answer this bit alone.
   Rights_Policy_Write : constant := 4;
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
   Server_Pages : constant := (Delegations_At + Maximum_Delegations * Delegation_Bytes) / Page_Bytes;
   Dirty_Arena_Pages : constant := Dirty_Arena_Bytes / Page_Bytes;
   Transfer_Pages : constant := 256;
   Transfer_Bytes : constant := Transfer_Pages * Page_Bytes;

   pragma Compile_Time_Error
     (Queues.Submissions.Slots /= Slots or else
      Client_Requests_At + Slots * Request_Bytes > Client_Pages * Page_Bytes or else
      Server_Answers_At + Slots * Answer_Bytes > Delegations_At or else
      Server_Copies_At < Server_Namespace_At + 8 or else
      Server_Copies_At + Maximum_Copies * Copy_Entry_Bytes > Server_Answers_At or else
      Delegations_At mod Page_Bytes /= 0 or else
      (Delegations_At + Maximum_Delegations * Delegation_Bytes) mod Page_Bytes /= 0 or else
      Dirty_Pages_At /= Dirty_Entries * Dirty_Entry_Bytes or else
      Dirty_Arena_Bytes /= Dirty_Pages_At + Dirty_Entries * Dirty_Page_Bytes or else
      Dirty_Arena_Bytes mod Page_Bytes /= 0,
      "filesystem queue layout");

   --  The channels (schema identities name these layouts).
   QUEUE_SCHEMA    : constant CuBit.Protocols.Schema_Id := 16#4653_5155_4555_0001#;
   TRANSFER_SCHEMA : constant CuBit.Protocols.Schema_Id := 16#4653_5452_414E_0001#;
   DIRTY_SCHEMA    : constant CuBit.Protocols.Schema_Id := 16#4653_4449_5254_0001#;
   EVENT_SCHEMA    : constant CuBit.Protocols.Schema_Id := 16#4653_4556_4E54_0001#;

   QUEUE_CONTRACT : constant CuBit.Channel_Contracts.Contract :=
     (Element => (Identity => QUEUE_SCHEMA, Version => 1,
                  Sizing => CuBit.Protocols.Fixed_Size, Wire_Size => Request_Bytes),
      Kind    => CuBit.Channel_Contracts.Duplex,
      Policy  => CuBit.Channel_Contracts.Lossless,
      Pages   => Client_Pages,
      Buffers => Server_Pages,
      Rule    => CuBit.Channel_Contracts.Copy_Then_Validate);
   --  One-page buffers; requests name byte ranges within them.
   TRANSFER_CONTRACT : constant CuBit.Channel_Contracts.Contract :=
     (Element => (Identity => TRANSFER_SCHEMA, Version => 1,
                  Sizing => CuBit.Protocols.Bounded_Size, Wire_Size => Page_Bytes),
      Kind    => CuBit.Channel_Contracts.Arena,
      Policy  => CuBit.Channel_Contracts.Lossless,
      Pages   => 1,
      Buffers => Transfer_Pages,
      Rule    => CuBit.Channel_Contracts.Copy_Then_Validate);
   DIRTY_CONTRACT : constant CuBit.Channel_Contracts.Contract :=
     (Element => (Identity => DIRTY_SCHEMA, Version => 1,
                  Sizing => CuBit.Protocols.Fixed_Size, Wire_Size => Dirty_Page_Bytes),
      Kind    => CuBit.Channel_Contracts.Arena,
      Policy  => CuBit.Channel_Contracts.Lossless,
      Pages   => 1,
      Buffers => Dirty_Arena_Pages,
      Rule    => CuBit.Channel_Contracts.Copy_Then_Validate);

   --  The event ring: bounded records (CuBit.Filesystem_Events, at most
   --  Event_Record_Bytes), lossless; the service never waits on it: what
   --  would not fit becomes a Rescan_Needed record.
   Event_Record_Bytes : constant := 4_128;
   Event_Ring_Pages : constant := 16;
   EVENT_CONTRACT : constant CuBit.Channel_Contracts.Contract :=
     (Element => (Identity => EVENT_SCHEMA, Version => 1,
                  Sizing => CuBit.Protocols.Bounded_Size, Wire_Size => Event_Record_Bytes),
      Kind    => CuBit.Channel_Contracts.Queue,
      Policy  => CuBit.Channel_Contracts.Lossless,
      Pages   => Event_Ring_Pages,
      Buffers => 1,
      Rule    => CuBit.Channel_Contracts.Copy_Then_Validate);

   --  A request's arena range lies inside an arena of Arena_Bytes.
   function In_Arena
     (Offset, Length, Arena_Bytes : Unsigned_64) return Boolean is
     (Length <= Arena_Bytes and then Offset <= Arena_Bytes - Length);

end CuBit.Filesystem_Queues;
