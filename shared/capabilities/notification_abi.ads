------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Wait and Wake (IPC revision 2, S1): waiting on one object, a notification
--  or a private futex word, and signalling it. Shared by the kernel
--  (Process.Notifications, Process.Private_Futex, Syscall.Waits) and the
--  runtime (CuBit.Notifications).
--
--  @description
--  Wait (Kind, Object, Expected, Deadline), registers arg0 .. arg3:
--    Notification: Object is a handle with the Wait right. Takes every
--      pending bit and clears them; with none pending, blocks until a Wake
--      or the deadline. Result: the bits (never Status_First or above; 0
--      when the deadline came with none), or Busy (another thread waits,
--      or the object is a leaf bound to a root: its root is told), Ended
--      (the object ended while waiting), Revoked (the handle's generation
--      is no longer the object's), Refused (not such a handle).
--    Private_Word: Object is an aligned 32-bit word of the caller's own
--      memory; sleeps while it holds Expected (at most 2**32 - 1), until a
--      Wake of it or the deadline. Result: Woken, Retry (it did not hold
--      Expected), Timed_Out, Bad_Argument (unaligned, not user memory, or
--      not mapped).
--    Deadline: an absolute monotonic microsecond (the clock of
--    CuBit.Monotonic), Now (0: look once, never block) or Forever. There
--    is no default.
--
--  Wake (Kind, Object, Count), registers arg0 .. arg2:
--    Notification: Object is a handle with the Wake right. ORs that
--      handle's badge into the object (the caller passes no bits); a leaf
--      also signals its root with the root badge. Result 1, or Ended (the
--      object has ended), Revoked, Refused.
--    Private_Word: wake up to Count of the caller's threads waiting on the
--      word, oldest first. Result: how many, or Bad_Argument.
--
--  The object operations are Calls (labels below), with the reply's word
--  0 an Outcome:
--    Create       on the caller's own process handle (Write): a new
--                 notification, charged to the caller; (1) = its handle,
--                 with the Wait right and the Wake right, unbadged, never
--                 copied (it has no Grant right).
--    Mint_Wake    on a Wait handle: (0) = badge (1 .. Badge_Last), (1) =
--                 Grant_Too (0 or 1); (1) = a Wake handle carrying that
--                 badge for good, which may be copied with Grant.
--    Bind_Leaf    on a leaf's Wait handle: (0) = a Wake handle of the
--                 root. Signals of the leaf then reach the root with that
--                 handle's badge. Two levels only (Not_Allowed otherwise):
--                 a root never becomes a leaf, a leaf has no leaves and no
--                 thread.
--    Unbind_Leaf  on the leaf's Wait handle.
--    Bind_Thread  on a root's Wait handle: binds it to the calling thread
--                 (its bound notification); until the endpoint receive of
--                 IPC revision 2 (S2), a signal with no waiter rings the
--                 thread's process like a channel doorbell, so its receive
--                 returns (one Channel_Wake_ABI notice).
--    Unbind_Thread on that handle, from the bound thread.
--    Revoke       on a handle with Right_Revoke (the Wait handle): the
--                 object's generation advances (generations never go back,
--                 not even when its slot is reused), so every other handle
--                 of it is stale: a Wake through one answers Revoked, and a
--                 thread waiting with the old Wait right wakes Revoked. (1)
--                 = the revoker's handle, re-stamped (a new handle word; the
--                 old one is closed).
--  Closing the Wait handle (or the holder's exit) ends the object: its
--  waiter returns Ended, and every Wake handle of it answers Ended.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Notification_ABI with Pure, SPARK_Mode is

   Wait_System_Call : constant := 150;
   Wake_System_Call : constant := 151;

   --  Wait's and Wake's Kind (arg0).
   Notification : constant Unsigned_64 := 1;
   Private_Word : constant Unsigned_64 := 2;

   --  Deadlines.
   Now     : constant Unsigned_64 := 0;
   Forever : constant Unsigned_64 := Unsigned_64'Last;

   --  Badges: 1 .. Badge_Last (bit 63 is never a pending bit).
   Badge_Last : constant Unsigned_64 := 2 ** 63 - 1;

   --  A private word's expected value.
   Word_Value_Last : constant Unsigned_64 := 2 ** 32 - 1;

   --  Results.
   Status_First : constant Unsigned_64 := 2 ** 63;
   Busy         : constant Unsigned_64 := Status_First + 1;
   Ended        : constant Unsigned_64 := Status_First + 2;
   Refused      : constant Unsigned_64 := Status_First + 3;
   Bad_Argument : constant Unsigned_64 := Status_First + 4;
   --  The capability's generation is not the object's (it was revoked).
   Revoked      : constant Unsigned_64 := Status_First + 5;
   --  Private_Word Wait.
   Woken     : constant Unsigned_64 := 0;
   Retry     : constant Unsigned_64 := 1;
   Timed_Out : constant Unsigned_64 := 2;
   --  Notification Wake.
   Signalled : constant Unsigned_64 := 1;

   function Is_Status (Result : Unsigned_64) return Boolean is (Result >= Status_First);

   --  Labels (one block, like VM_ABI's).
   Create_Label        : constant := 16#0F01#;
   Mint_Wake_Label     : constant := 16#0F02#;
   Bind_Leaf_Label     : constant := 16#0F03#;
   Unbind_Leaf_Label   : constant := 16#0F04#;
   Bind_Thread_Label   : constant := 16#0F05#;
   Unbind_Thread_Label : constant := 16#0F06#;
   Revoke_Label        : constant := 16#0F07#;

   function Is_Label (Label : Unsigned_64) return Boolean is
     (Label in Create_Label .. Revoke_Label);

   type Outcome is
     (Ok, Refused_Call, Not_Allowed, Already, Busy_Object, Limit_Reached, No_Memory,
      Ended_Object, Revoked_Object);
   for Outcome use
     (Ok => 0, Refused_Call => 1, Not_Allowed => 2, Already => 3, Busy_Object => 4,
      Limit_Reached => 5, No_Memory => 6, Ended_Object => 7, Revoked_Object => 8);

   --  Notifications one process may hold the Wait right of.
   Per_Process_Limit : constant := 128;

end Notification_ABI;
