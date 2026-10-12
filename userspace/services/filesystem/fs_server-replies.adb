with System.Machine_Code;
with CuBit.Filesystems; use CuBit.Filesystems;

package body Fs_Server.Replies is

   function queueOf (owner : Process_ID) return Client_Queue_Count is
   begin
      for q in clientQueues'Range loop
         if clientQueues (q).owner = owner then
            return q;
         end if;
      end loop;
      return No_Client_Queue;
   end queueOf;

   procedure publishNamespace (q : Client_Queue_Index) is
      word : Unsigned_32 with Volatile, Import,
        Address => serverWord (q, FQ.Server_Namespace_At);
   begin
      word := namespaceGeneration;
   end publishNamespace;

   procedure bumpNamespace (owner : Process_ID := No_Process) is
   begin
      namespaceGeneration :=
        (if namespaceGeneration = Unsigned_32'Last then 1 else namespaceGeneration + 1);
      for q in clientQueues'Range loop
         if clientQueues (q).serverBase /= System.Null_Address and then
           (owner = No_Process or else clientQueues (q).owner = owner)
         then
            publishNamespace (q);
         end if;
      end loop;
      --  Published before the change it announces takes effect.
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
   end bumpNamespace;

   --  Answer queue q's held wake request (its saved reply capability is
   --  consumed whether or not the client still waits for it).
   procedure answerWake (q : Client_Queue_Index; label : Unsigned_32) is
      ignore : Unsigned_64;
   begin
      ignore := replyCap
        (CapabilitySlot (WAKE_REPLY_SLOT_BASE + q),
         (tag => (label => label, length => 0, flags => 0, reserved => 0),
          authorityTag => 0, words => [others => 0]));
   end answerWake;

   --  Answer the entry being handled on its queue, and answer the client's
   --  held wake request, if any.
   procedure answerEntry
     (label : Unsigned_32; word0, word1 : Unsigned_64; rights : Unsigned_32 := 0)
   is
      q : constant Client_Queue_Count := curRoute.queue;
   begin
      if q = No_Client_Queue or else clientQueues (q).server.Owed = 0 then
         return;
      end if;
      declare
         ring : FQueues.Completions.Ring with Import,
           Address => serverWord (q, FQ.Server_Answers_At);
         produced : Unsigned_32 with Volatile, Import,
           Address => serverWord (q, FQ.Server_Answered_At);
         wakeHeld : Boolean;
      begin
         --  Owed <= Space (FQueues.Valid): the answer has its slot.
         FQueues.Complete
           (clientQueues (q).server, ring, curRoute.token,
            (Status => label, Reserved => rights, Value => word0, Spare => word1));
         --  The answer is written before the count that hands it over.
         System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
         produced := Unsigned_32 (clientQueues (q).server.Answers.Produced);
         Queue_Wakes.Posted (clientQueues (q).wake, wakeHeld);
         if wakeHeld then
            answerWake (q, REPLY_OK);
         end if;
      end;
   end answerEntry;

   procedure acquireClientMemory
     (sender        : Process_ID;
      rawSlot       : Unsigned_64;
      rawGeneration : Unsigned_64;
      byteLength    : Unsigned_64;
      requiredAccess : CuBit.Memory_Grants.Required_Access;
      address       : out System.Address;
      success       : out Boolean)
   is
   begin
      --  A queue entry's data is in the arena range already checked.
      if curRoute.queue /= No_Client_Queue then
         address := curArena;
         success := curArena /= System.Null_Address and then
                    byteLength <= curArenaBytes;
         return;
      end if;
      address := System.Null_Address;
      success := False;
      if rawSlot > CuBit.Memory_Grants.MAXIMUM_GLOBAL_SLOT or else
         rawGeneration = 0 or else
         rawGeneration > CuBit.Memory_Grants.MAXIMUM_GENERATION or else
         byteLength = 0
      then
         return;
      end if;

      CuBit.Memory_Grants.Acquire
        (reference      =>
           (slot => CuBit.Memory_Grants.Global_Grant_Slot (rawSlot),
            generation =>
              CuBit.Memory_Grants.Grant_Generation (rawGeneration)),
         expectedOwner => sender,
         byteOffset     => 0,
         byteLength     => byteLength,
         requiredAccess => requiredAccess,
         mappedAddress  => address,
         success        => success);
   end acquireClientMemory;

   procedure returnClientMemory
     (rawSlot       : Unsigned_64;
      rawGeneration : Unsigned_64;
      success       : out Boolean)
   is
   begin
      if curRoute.queue /= No_Client_Queue then
         success := True;   --  the arena stays mapped
         return;
      end if;
      CuBit.Memory_Grants.Return_Acquisition
        ((slot => CuBit.Memory_Grants.Global_Grant_Slot (rawSlot),
          generation =>
            CuBit.Memory_Grants.Grant_Generation (rawGeneration)),
         success);
   end returnClientMemory;

   --  Send a reply with the given label and word0 value
   procedure sendReply
     (dest   : Process_ID;
      label  : Unsigned_32;
      word0  : Unsigned_64)
   is
      replyMsg : Message;
      ignore   : Unsigned_64;
   begin
      if curRoute.queue /= No_Client_Queue then
         answerEntry (label, word0, 0);
         return;
      end if;
      replyMsg.tag := (label  => label,
                       length => 1,
                       flags  => 0,
                       reserved  => 0);
      replyMsg.words := [0 => word0, others => 0];
      ignore := reply (dest, replyMsg);
   end sendReply;

end Fs_Server.Replies;
