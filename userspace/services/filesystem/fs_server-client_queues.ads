------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Client request queues of the filesystem service: channels, wakes, dispatch of queue entries.
------------------------------------------------------------------------------
with CuBit.Control_Events;
private package Fs_Server.Client_Queues is

   --  A client opens its queue's channels (FQ, docs/data-plane.md): the
   --  transfer arena and the optional dirty arena first, then the queue
   --  pair, which takes them in; the event ring last. One queue per process.
   procedure handleChannelOpen (sender : Process_ID; msg : Message);

   --  Return a client queue's grants and free its entry.
   procedure releaseClientQueue (owner : Process_ID);

   --  A grant event: a client that let go of any of its channels (or died)
   --  is done with all of them, as on its OP_CLOSE, which a full mailbox
   --  may have refused. The kernel's notice always arrives
   --  (docs/ipc-delivery.md).
   procedure clientChannelEnded (event : CuBit.Control_Events.Event);

   --  A wake request (FQ.OP_FS_WAKE, a call or an asynchronous submit):
   --  answered now if answers wait, else held until one is posted
   --  (Queue_Wakes). A held one it supersedes is answered first, which
   --  frees its slot for this one.
   procedure handleWake (sender : Process_ID);

   --  Take every queue's requests and handle each, then keep polling for a
   --  short window (CuBit.Busy_Poll) while requests keep coming.
   procedure serviceClientQueues;

   --  About to block: arm each queue's wake word so its client kicks, then
   --  look once more. True if requests wait: do not block.
   function armClientQueues return Boolean;

end Fs_Server.Client_Queues;
