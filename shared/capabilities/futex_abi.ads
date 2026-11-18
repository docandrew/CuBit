------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Shared futexes (IPC-010): naming a futex word in a grant. Shared by the
--  kernel (Process.Futex) and the runtime (CuBit.Waits).
--
--  @description
--  A private futex is a word of the caller's own address space, named by
--  its address. A shared futex is a word in a memory grant, named by the
--  grant's slot and the word's byte offset in the grant, packed in one
--  register word (Shared_Word). Both parties of the grant, its owner and
--  its grantee, name the same word the same way whatever their mappings,
--  and only they may wait on it or wake it. When the grant's life ends,
--  its waiters return Wait_ABI.Futex_Revoked.
--
--  Where a shared word is named:
--    Wait, futex form: Sources Futex_Sources + Wait_ABI.Shared_Futex, A the
--      shared word, B the expected value.
--    Wait's wake operand: Wait_ABI.Wake_First + Wait_ABI.Wake_Shared, arg5
--      the shared word.
--    FUTEX_WAKE: arg0 the shared word, arg1 the count, arg2 Shared_Space
--      (Private_Space, 0, is a private address as before).
--
--  Waking a channel peer (IPC-010 option B, Channel_Wake_ABI): FUTEX_WAKE
--  with arg2 Peer_Space, arg0 a grant's reference word (generation << 32 |
--  slot) of a live grant made with the notify flag (a channel's) that the
--  caller is a party of, arg1 ignored, arg3 Sync_Hint when the caller is
--  about to block (the woken thread is placed on its CPU). It wakes the
--  other party's wake word: the Ready_Word wait or the receive it is in,
--  coalesced. Result 1 (woken, or already pending), or Wake_Refused.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Futex_ABI with Pure, SPARK_Mode is

   Slot_Bits   : constant := 20;
   Offset_Bits : constant := 44;
   Slot_Limit   : constant := 2 ** Slot_Bits;
   Offset_Limit : constant := 2 ** Offset_Bits;

   subtype Grant_Slot_Number is Unsigned_64 range 1 .. Slot_Limit - 1;
   subtype Grant_Offset is Unsigned_64 range 0 .. Offset_Limit - 1;

   --  FUTEX_WAKE's arg2.
   Private_Space : constant Unsigned_64 := 0;
   Shared_Space  : constant Unsigned_64 := 1;
   Peer_Space    : constant Unsigned_64 := 2;
   --  FUTEX_WAKE's arg3 (Peer_Space): the waker is about to block.
   Sync_Hint     : constant Unsigned_64 := 1;

   --  FUTEX_WAKE of a shared word by a process that is neither party of a
   --  live grant (or a malformed word): nothing woken.
   Wake_Refused : constant Unsigned_64 := Unsigned_64'Last;

   function Shared_Word (Slot : Grant_Slot_Number; Offset : Grant_Offset)
     return Unsigned_64 is
     (Slot + Offset * Slot_Limit)
   with Post => Shared_Word'Result mod Slot_Limit = Slot and then
                Shared_Word'Result / Slot_Limit = Offset;

   function Slot_Of (Word : Unsigned_64) return Unsigned_64 is
     (Word mod Slot_Limit)
   with Post => Slot_Of'Result < Slot_Limit;

   function Offset_Of (Word : Unsigned_64) return Unsigned_64 is
     (Word / Slot_Limit)
   with Post => Offset_Of'Result < Offset_Limit;

   --  A word names a grant slot (slot 0 names none) and a 4-aligned offset.
   function Well_Formed (Word : Unsigned_64) return Boolean is
     (Slot_Of (Word) /= 0 and then Offset_Of (Word) mod 4 = 0);

   --  Packing and unpacking agree (proved).
   function Round_Trips (Slot : Grant_Slot_Number; Offset : Grant_Offset) return Boolean is
     (Slot_Of (Shared_Word (Slot, Offset)) = Slot and then
      Offset_Of (Shared_Word (Slot, Offset)) = Offset and then
      (if Offset mod 4 = 0 then Well_Formed (Shared_Word (Slot, Offset))))
   with Ghost, Post => Round_Trips'Result;

end Futex_ABI;
