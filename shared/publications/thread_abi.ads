------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  THREAD_CREATE's result: the new thread's ID and its thread-count slot
--  (Readiness_Publication), or Refused.
--
--  @description
--  The ID is the low 32 bits, so a 32-bit store of the result (musl's
--  CLONE_PARENT_SETTID) keeps it; the slot is above it. The new thread
--  also starts with its slot in RSI, its second argument, so it knows its
--  own line without waiting for its creator.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Readiness_Publication;

package Thread_ABI with Pure, SPARK_Mode is

   Refused : constant Unsigned_64 := Unsigned_64'Last;

   ID_Bits    : constant := 32;
   ID_Mask    : constant Unsigned_64 := 2 ** ID_Bits - 1;

   function Created (Thread : Unsigned_64; Slot : Readiness_Publication.Thread_Slot)
     return Unsigned_64 is
     (Thread or Shift_Left (Unsigned_64 (Slot), ID_Bits))
   with Pre => Thread <= ID_Mask;

   function Thread_Of (Result : Unsigned_64) return Unsigned_64 is
     (Result and ID_Mask);

   function Is_Created (Result : Unsigned_64) return Boolean is
     (Shift_Right (Result, ID_Bits) <=
        Unsigned_64 (Readiness_Publication.Thread_Slot'Last));

   function Slot_Of (Result : Unsigned_64) return Readiness_Publication.Thread_Slot is
     (Readiness_Publication.Thread_Slot (Shift_Right (Result, ID_Bits)))
   with Pre => Is_Created (Result);

end Thread_ABI;
