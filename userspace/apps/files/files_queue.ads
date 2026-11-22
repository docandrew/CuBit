with Interfaces; use Interfaces;
with CuBit.Filesystem_Queues;
with CuBit.Filesystem_Events;
with CuBit.Messages;
with Files_Listing;

--  Files' side of the filesystem's request queue (docs/files-app.md, "I/O
--  model"): requests are submitted and answers reaped without ever
--  blocking. On CuBit the body is CuBit.Filesystem_Sessions
--  (native/files_queue.adb); hosted, the client rings over the mock's
--  memory (tests/files-app/host/files_queue.adb). Submissions are published,
--  and the service kicked (only when its wake word asks for it), once per
--  batch by Flush. Arena bytes are copied in and out here; callers validate
--  the copies (the service is not trusted).
package Files_Queue is
   package FQ renames CuBit.Filesystem_Queues;
   subtype Token is FQ.Queues.Token;
   use type Token;
   NO_TOKEN : constant Token := 0;

   type Answer is record
      Tag : Token := NO_TOKEN;
      Status : Unsigned_32 := 0;
      Value : Unsigned_64 := 0;
   end record;

   procedure Open (Arena_Pages : Positive; Success : out Boolean);
   function Ready return Boolean;
   function Arena_Bytes return Unsigned_64;
   --  Room for one more request, and requests whose answer is not reaped.
   function Can_Submit return Boolean;
   function Outstanding return Natural;
   procedure Submit (Request : FQ.Request; Tag : out Token)
     with Pre => Ready and then Can_Submit;
   procedure Flush;
   procedure Reap (Item : out Answer; Got : out Boolean);
   --  With requests out, ask the service to wake the event loop when
   --  their answers wait (OP_FS_WAKE); the loop then sleeps until input,
   --  that wake or its own deadline, instead of polling.
   procedure Arm_Wake;
   --  A completion-queue entry from the platform's event loop: Consumed when
   --  it answered the armed wake (Woken: answers wait). Hosted, the mock
   --  wakes the loop itself and nothing is consumed.
   procedure Complete (Receipt : CuBit.Messages.CompletionEntry; Consumed, Woken : out Boolean);
   --  Change watches (Queue_Watch): the event ring, opened once after Open,
   --  and its records, copied and then checked; Malformed is a record the
   --  service got wrong, never skipped silently. The wake (Arm_Wake) also
   --  fires when records wait.
   package FE renames CuBit.Filesystem_Events;
   procedure Open_Events (Success : out Boolean);
   function Events_Open return Boolean;
   type Event_Result is (Taken, Empty, Malformed);
   procedure Take_Event
     (Item : out FE.Event; Name : out FE.Name_Bytes; Length : out FE.Name_Length; Result : out Event_Result);
   --  A server-side copy's progress (Queue_Copy, its request's Tag): the
   --  bytes copied so far, or 0 when the service shows no copy of that tag.
   function Copy_Progress (Tag : Token) return Unsigned_64;
   --  Copies between the arena and private memory; ranges outside the
   --  arena copy nothing (Into is then zeroes).
   procedure Read_Arena (Offset : Unsigned_64; Into : out Files_Listing.Name_Bytes);
   procedure Write_Arena (Offset : Unsigned_64; From : Files_Listing.Name_Bytes);
   procedure Close;
end Files_Queue;
