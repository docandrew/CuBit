------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Advisory file locks of the filesystem service (FQ.Queue_Lock,
--  Queue_Unlock): the proved table (File_Locks) over the handle table,
--  and the answers of waits granted, expired or cancelled.
------------------------------------------------------------------------------
private package Fs_Server.Locks is

   --  Queue_Lock: Handle (an ext2 file), Options = FQ.Lock_Shared or
   --  FQ.Lock_Exclusive, plus FQ.Lock_Wait to wait until Spare_2 (absolute
   --  ms, Wait_Forever spelled out). A wait is answered when it ends.
   procedure handleLock (q : Client_Queue_Index; r : FQ.Request);

   --  Queue_Unlock: Handle. Its lock ends; its wait is answered CANCELLED.
   procedure handleUnlock (q : Client_Queue_Index; handleWord : Unsigned_64);

   --  The handle in slot is closing or being parked: its lock ends (others
   --  waiting for the file may be granted) and its wait is answered
   --  CANCELLED. O(1) when it holds and waits for nothing.
   procedure lockHandleGone (slot : Natural);

   --  Queue q is released: its waits end unanswered (their handles keep
   --  any lock they hold until they close).
   procedure forgetLockWaits (q : Client_Queue_Index);

   --  Each pass: waits whose deadline passed are answered DEADLINE.
   procedure expireLockWaits;

   --  The earliest deadline of a wait (Wait_Forever: none), so the loop
   --  does not sleep past it.
   function nextLockDeadline return Unsigned_64;

end Fs_Server.Locks;
