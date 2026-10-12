------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Answers of the filesystem service: messages, queue entries, wakes; client memory.
------------------------------------------------------------------------------
private package Fs_Server.Replies is

   --  The queue of a client, if it opened one.
   function queueOf (owner : Process_ID) return Client_Queue_Count;

   --  Publish the namespace generation in queue q's page.
   procedure publishNamespace (q : Client_Queue_Index);

   --  Move the namespace generation on, before the change it announces.
   --  owner: only that client's access changes (its policy), so only its
   --  queue needs the new value; No_Process: every client's.
   procedure bumpNamespace (owner : Process_ID := No_Process);

   --  Answer queue q's held wake request (its saved reply capability is
   --  consumed whether or not the client still waits for it).
   procedure answerWake (q : Client_Queue_Index; label : Unsigned_32);

   --  Answer the entry being handled on its queue, and answer the client's
   --  held wake request, if any.
   procedure answerEntry
     (label : Unsigned_32; word0, word1 : Unsigned_64; rights : Unsigned_32 := 0);

   --  Decode an untrusted wire reference only after checking that both fields
   --  fit the strongly typed userspace representation.  The kernel then
   --  authenticates the owner, current grantee, generation, access, and full
   --  byte range before returning an address. A queue entry's data is the
   --  arena range already checked.
   procedure acquireClientMemory
     (sender        : Process_ID;
      rawSlot       : Unsigned_64;
      rawGeneration : Unsigned_64;
      byteLength    : Unsigned_64;
      requiredAccess : CuBit.Memory_Grants.Required_Access;
      address       : out System.Address;
      success       : out Boolean);

   procedure returnClientMemory
     (rawSlot       : Unsigned_64;
      rawGeneration : Unsigned_64;
      success       : out Boolean);

   --  Send a reply with the given label and word0 value (on the entry's
   --  queue while one is handled).
   procedure sendReply
     (dest   : Process_ID;
      label  : Unsigned_32;
      word0  : Unsigned_64);

end Fs_Server.Replies;
