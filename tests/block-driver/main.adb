--  Regression tests for CuBit.Block_Driver (DEV-002): the shared async
--  block driver contract, driven through a fake four-slot device.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants; use CuBit.Memory_Grants;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with CuBit.Block_Requests; use CuBit.Block_Requests;
with CuBit.Block_Work; use CuBit.Block_Work;
with CuBit.Device_Identities;
with Block_Script; use Block_Script;
with Test_Device; use Test_Device;

procedure Main is
   Checks : Natural := 0;
   Progressed : Boolean;

   procedure Check (Condition : Boolean; What : String) is
   begin
      if not Condition then
         Put_Line ("FAIL: " & What);
         raise Program_Error;
      end if;
      Checks := Checks + 1;
   end Check;

   function Transfer
     (Op : Unsigned_32; LBA, Count : Unsigned_64; Grant : Unsigned_64;
      Writer : Boolean := True) return Message is
     ((tag => (label => Op, length => 4, protocol => 0, ipc => <>, reserved => 0),
       authorityTag => (if Writer then Write_Authority_Tag else 0),
       words => [LBA, Grant, Count, 7]));

   function Simple (Op : Unsigned_32; Writer : Boolean := True) return Message is
     ((tag => (label => Op, length => 0, protocol => 0, ipc => <>, reserved => 0),
       authorityTag => (if Writer then Write_Authority_Tag else 0),
       words => [others => 0]));

   procedure Poll is
   begin
      Driver.Poll (Progressed);
   end Poll;

   function OK (Item : Answer; Moved : Unsigned_64) return Boolean is
     (Item.Response.tag.label = REPLY_OK and then Item.Response.words (0) = Moved);
   function Error (Item : Answer) return Boolean is
     (Item.Response.tag.label = REPLY_ERROR and then Item.Response.words (0) = 0);

   Description : Device_Description;
   Before : Natural;
begin
   --  1. Unstarted: the device is faulted and every transfer is refused.
   Push (Transfer (OP_READ_BLOCKS, 0, 1, 1));
   Poll;
   Check (Answered = 1 and then Error (Last) and then not Last.Saved,
          "unstarted transfer refused at once");
   Check (Posts = 0 and then Active = 0, "nothing posted or held");

   Driver.Start
     ((Presence => Available, Blocks => Disk_Blocks,
       Logical_Size => Block_Bytes, Physical_Size => Block_Bytes,
       Max_Transfer => 8, Features => FEATURE_FLUSH or FEATURE_VOLATILE_CACHE,
       Media => Fixed_Media),
      (Model => CuBit.Device_Identities.To_Text ("Fake Disk"),
       Serial => CuBit.Device_Identities.To_Text ("SN-1")));

   --  2. Describe: the writer's view, and read only for everyone else.
   Push (Simple (OP_DESCRIBE_DEVICE));
   Push (Simple (OP_DESCRIBE_DEVICE, Writer => False));
   Poll;
   Check (Answered = 3, "two describes answered in one poll");
   Check (Decode_Description
            (Answers (2).Response.words (0), Answers (2).Response.words (1),
             Answers (2).Response.words (2), Answers (2).Response.words (3),
             Description)
          and then Description.blockCount = Disk_Blocks
          and then not Is_Read_Only (Description), "writer sees writable device");
   Check (Decode_Description
            (Answers (3).Response.words (0), Answers (3).Response.words (1),
             Answers (3).Response.words (2), Answers (3).Response.words (3),
             Description)
          and then Is_Read_Only (Description), "reader sees read-only device");

   --  3. Writes without the write grant are refused and counted (first,
   --  then doublings).
   for I in 1 .. 4 loop
      Push (Transfer (OP_WRITE_BLOCKS, 0, 1, 1, Writer => False));
   end loop;
   Poll;
   Check (Answered = 7 and then Error (Last), "unauthorised writes refused");
   Check (Driver.Refused_Writes = 4 and then Notes (Write_Refused) = 3
          and then Last_Count (Write_Refused) = 4, "refusals noted 1, 2, 4");
   Check (Posts = 0 and then Active = 0, "refused writes post nothing");

   --  4. An asynchronous write then read: no reply until the slot finishes.
   Buffers (1) := [others => 16#A5#];
   Push (Transfer (OP_WRITE_BLOCKS, 3, 2, 1));
   Poll;
   Check (Answered = 7 and then Posts = 1 and then Driver.Outstanding,
          "write posted, not answered");
   Check (Held (1), "write grant held while in flight");
   Done (Slot'First) := True;
   Poll;
   Check (Answered = 8 and then Last.Saved and then OK (Last, 1024),
          "write answered through kept reply with bytes moved");
   Check (not Held (1) and then not Driver.Outstanding, "grant returned");
   Check (Disk (3 * Block_Bytes + 1) = 16#A5# and then Disk (5 * Block_Bytes) = 16#A5#
          and then Disk (5 * Block_Bytes + 1) = 0, "write reached exactly its blocks");

   Buffers (2) := [others => 0];
   Push (Transfer (OP_READ_BLOCKS, 3, 2, 2));
   Poll;
   Done (Slot'First) := True;
   Poll;
   Check (OK (Last, 1024) and then Buffers (2) (1) = 16#A5#
          and then Buffers (2) (1024) = 16#A5# and then Buffers (2) (1025) = 0,
          "read returns written data, nothing past it");

   --  5. Four requests fill the four slots; a fifth waits for a free slot.
   Before := Answered;
   for G in 1 .. 5 loop
      Push (Transfer (OP_READ_BLOCKS, Unsigned_64 (G), 1, Unsigned_64 (G)));
   end loop;
   Poll;
   Check (Answered = Before and then Waiting = 1 and then Active = 4,
          "four in flight, fifth still queued");
   Done (2) := True;
   Poll;
   Check (Answered = Before + 1 and then Last.Reply_Word = Unsigned_64 (Taken - 2)
          and then Waiting = 0 and then Active = 4,
          "slot 2 answered its own request, fifth took the slot");
   Done := [others => True];
   Poll;
   Check (Answered = Before + 5 and then Active = 0 and then not Driver.Outstanding,
          "all five answered, all grants returned");

   --  6. A short or failed transfer reports nothing moved.
   Push (Transfer (OP_READ_BLOCKS, 0, 2, 1));
   Poll;
   Short (Slot'First) := True;
   Done (Slot'First) := True;
   Poll;
   Check (Error (Last), "short read reports zero bytes");
   Push (Transfer (OP_WRITE_BLOCKS, 0, 2, 1));
   Poll;
   Done (Slot'First) := True;
   Poll;
   Check (Last.Response.tag.label = REPLY_ERROR
          and then Last.Response.words (0) = Block_Bytes and then Active = 0,
          "short write is an error reporting its completed prefix");
   Short (Slot'First) := False;
   Push (Transfer (OP_WRITE_BLOCKS, 0, 1, 1));
   Poll;
   Fail (Slot'First) := True;
   Done (Slot'First) := True;
   Poll;
   Check (Error (Last) and then Active = 0,
          "failed write claiming every byte reports zero bytes");
   Fail (Slot'First) := False;

   --  7. A command past its deadline is cancelled and answered as failed.
   Done := [others => False];
   Push (Transfer (OP_READ_BLOCKS, 0, 1, 1));
   Poll;
   Before := Answered;
   Clock := Clock + Deadline - 1;
   Poll;
   Check (Answered = Before and then not Cancelled (Slot'First), "not yet expired");
   Clock := Clock + 1;
   Poll;
   Check (Answered = Before + 1 and then Error (Last) and then Cancelled (Slot'First)
          and then Active = 0, "expired command cancelled and refused");
   Check (Driver.Expired_Commands = 1 and then Notes (Command_Expired) = 1,
          "expiry noted");

   --  8. If the reply cannot be kept, the posted command is cancelled, the
   --  request fails at once and its grant goes back.
   Refuse_Keep := True;
   Before := Posts;
   Push (Transfer (OP_READ_BLOCKS, 0, 1, 1));
   Poll;
   Check (Error (Last) and then not Last.Saved and then Posts = Before + 1
          and then Cancelled (Slot'First)
          and then Active = 0 and then not Driver.Outstanding,
          "unkept reply: command cancelled, request refused, grant returned");
   Refuse_Keep := False;
   Before := Posts;

   --  9. A grant the sender cannot lend is refused before any slot is used.
   Allow_Write := False;
   Push (Transfer (OP_READ_BLOCKS, 0, 1, 1));
   Poll;
   Check (Error (Last) and then Posts = Before and then Active = 0,
          "unadmitted grant refused");
   Allow_Write := True;
   Push (Transfer (OP_READ_BLOCKS, 0, 1, 1, Writer => True));
   Push (Transfer (OP_READ_BLOCKS, 0, 1, 1, Writer => True), Sender => 43);
   Poll;
   Check (Error (Last) and then Active = 1, "foreign sender's grant refused");
   Done (Slot'First) := True;
   Poll;
   Check (OK (Last, 512) and then Active = 0, "owner's read completes");

   --  10. Flush: posted as a cache flush, its outcome is the reply.
   Push (Simple (OP_FLUSH_DEVICE));
   Poll;
   Check (Work_Of (Slot'First).Kind = Cache_Flush and then Driver.Outstanding,
          "flush posted");
   Done (Slot'First) := True;
   Poll;
   Check (Last.Saved and then Last.Response.tag.label = REPLY_OK, "flush passed");
   Push (Simple (OP_FLUSH_DEVICE));
   Poll;
   Fail (Slot'First) := True;
   Done (Slot'First) := True;
   Poll;
   Check (Last.Response.tag.label = REPLY_ERROR, "failed flush reported");
   Fail (Slot'First) := False;

   --  11. Identify: the identity record, the rest of the block zero, at once.
   Buffers (3) := [others => 16#FF#];
   Before := Posts;
   Push ((tag => (label => OP_IDENTIFY_DEVICE, length => 4, protocol => 0,
                  ipc => <>, reserved => 0),
          authorityTag => 0, words => [0, 3, 1, 7]));
   Poll;
   declare
      Image : CuBit.Device_Identities.Record_Image;
      Item : CuBit.Device_Identities.Identity;
      Valid : Boolean;
   begin
      for I in Image'Range loop
         Image (I) := Buffers (3) (I);
      end loop;
      CuBit.Device_Identities.Decode (Image, Item, Valid);
      Check (OK (Last, Block_Bytes) and then Posts = Before and then Active = 0,
             "identify answered at once");
      Check (Valid and then CuBit.Device_Identities.Same
               (Item.Model, CuBit.Device_Identities.To_Text ("Fake Disk")),
             "identity record decodes");
      Check (Buffers (3) (Block_Bytes) = 0 and then Buffers (3) (Block_Bytes + 1) = 16#FF#,
             "rest of block zeroed, nothing past it");
   end;

   --  12. A synchronous device: answered at once, no reply kept, and
   --  Wait_And_Serve takes a request without polling.
   Instant := True;
   Before := Kept_Replies;
   Buffers (4) := [others => 0];
   Push (Transfer (OP_READ_BLOCKS, 3, 1, 4));
   Driver.Wait_And_Serve;
   Check (OK (Last, Block_Bytes) and then not Last.Saved
          and then Kept_Replies = Before and then Active = 0
          and then not Driver.Outstanding and then Buffers (4) (1) = 16#A5#,
          "synchronous read answered directly by Wait_And_Serve");
   Push (Simple (OP_FLUSH_DEVICE));
   Push (Transfer (OP_WRITE_BLOCKS, 9, 1, 4));
   Poll;
   Check (not Answers (Answered - 1).Saved and then not Last.Saved
          and then Last.Response.tag.label = REPLY_OK
          and then Kept_Replies = Before and then not Driver.Outstanding,
          "synchronous flush and write answered in one poll");
   --  13. Any_Client_Writes: a write without the badge is served, and the
   --  device is described writable to everyone.
   Open_Driver.Start
     ((Presence => Available, Blocks => Disk_Blocks,
       Logical_Size => Block_Bytes, Physical_Size => Block_Bytes,
       Max_Transfer => 8, Features => FEATURE_VOLATILE, Media => Memory_Media),
      (Model => CuBit.Device_Identities.To_Text ("RAM"),
       Serial => CuBit.Device_Identities.To_Text ("")));
   Buffers (5) := [others => 16#5A#];
   Push (Transfer (OP_WRITE_BLOCKS, 20, 1, 5, Writer => False));
   Push (Simple (OP_DESCRIBE_DEVICE, Writer => False));
   Open_Driver.Poll (Progressed);
   Check (OK (Answers (Answered - 1), Block_Bytes) and then Disk (20 * Block_Bytes + 1) = 16#5A#
          and then Open_Driver.Refused_Writes = 0, "open device takes badge-less write");
   Check (Decode_Description
            (Last.Response.words (0), Last.Response.words (1),
             Last.Response.words (2), Last.Response.words (3), Description)
          and then not Is_Read_Only (Description), "open device described writable");
   Push (Simple (OP_FLUSH_DEVICE, Writer => False));
   Open_Driver.Poll (Progressed);
   Check (Error (Last), "flush refused without FEATURE_FLUSH");
   Instant := False;

   Put_Line ("block-driver: PASS" & Natural'Image (Checks) & " checks");
end Main;
