------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Read and write delegations of the filesystem service; harvesting dirty pages.
------------------------------------------------------------------------------
private package Fs_Server.Delegations is

   --  Kept write-back failures of released handles (errseq-like): kept for
   --  the owner's next flush or close of the same file, never dropped.
   procedure keepWriteError (slot : Natural);

   --  Take (and clear) a kept failure of owner for key.
   procedure takeWriteError (owner : Process_ID; key : Inode_Identity; found : out Boolean);

   --  An exited owner's kept failures have no one left to report to.
   procedure forgetWriteErrors (owner : Process_ID);

   --  Grant (publish the file's identity, version and size, then the valid
   --  word) or revoke (clear the valid word) slot's delegation in its
   --  owner's queue page.
   procedure delegate (slot : Handle_Slot; grant : Boolean; write : Boolean := False);

   --  Harvest slot's dirty pages from its client's dirty arena.
   procedure harvest (slot : Handle_Slot);

   --  Every write-delegated handle of queue q's owner, in one scan.
   procedure harvestOwner (q : Client_Queue_Index);

   --  Take back slot's write delegation: clear its valid word, then harvest.
   procedure recallWrite (slot : Handle_Slot);

   --  The file key is about to change through slot except: every other
   --  handle's delegation goes first, and the version moves on.
   procedure fileChanging (key : Inode_Identity; except : Integer);

   --  Take back slot's delegation (harvesting a write delegation's pages).
   procedure dropDelegation (slot : Natural);

   --  After slot's own change: its delegation, if held, carries the new
   --  version and size.
   procedure refreshDelegation (slot : Handle_Slot);

   --  A handle was opened: a writable open revokes the others; the handle
   --  is delegated if no other handle can write the file.
   procedure handleOpened (slot : Handle_Slot);

end Fs_Server.Delegations;
