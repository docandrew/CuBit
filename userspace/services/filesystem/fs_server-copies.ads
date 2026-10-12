------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Server-side copies of the filesystem service (Queue_Copy), one bounded slice at a time.
------------------------------------------------------------------------------
private package Fs_Server.Copies is

   --  Queue_Copy: Handle = source (may read), Spare_1 = target (may write,
   --  ext2), Position / Arena_Offset = source / target offsets, Length (or
   --  FQ.Copy_To_End), Spare_2 = deadline (absolute ms, or Wait_Forever).
   --  Answered once, when the copy ends.
   procedure handleCopy (q : Client_Queue_Index; r : FQ.Request);

   --  Queue_Cancel: Handle = the copy's token. The copy is answered
   --  REPLY_CANCELLED (with its prefix) at its next slice.
   procedure handleCancel (q : Client_Queue_Index; token : Unsigned_64);

   --  A handle is going: a copy that reads or writes it ends (REPLY_ERR,
   --  with the prefix it copied).
   procedure copyHandleGone (slot : Natural);

   --  Whether any copy is in progress (the service then does not block).
   function copiesActive return Boolean;

   --  Each pass: one slice of every copy.
   procedure runCopies;

   --  Queue q is released: its copies end unanswered.
   procedure forgetCopies (q : Client_Queue_Index);

end Fs_Server.Copies;
