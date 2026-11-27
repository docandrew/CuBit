-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary Futex keys and buckets (docs/threads.md, IPC-010)
--
-- A shared futex (IPC-010) is (grant slot, grant life, byte offset in the
-- grant): both parties of a grant reach the same word through their own
-- mappings, and both resolve it to the same key, so a wake by one finds
-- the other's waiter. (Private futexes are each process's own table,
-- Private_Futexes, IPC revision 2.)
--
-- Buckets: a key hashes its grant slot alone, so every waiter of one grant
-- is in one bucket (and the overflow set), and ending a grant sweeps one
-- bucket.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Futex_Keys with SPARK_Mode => On is

    Bucket_Count : constant := 64;
    Max_Waiter   : constant := 1023;

    type Bucket_Index is range 0 .. Bucket_Count - 1;

    -- A waiting thread. Zero means none.
    type Waiter_Id is range 0 .. Max_Waiter;
    No_Waiter : constant Waiter_Id := 0;

    -- A shared word: a grant slot, its life (a later life of the same slot
    -- is another key; a live life is never zero) and the word's offset.
    -- Private words are in their process's own table (Private_Futexes).
    type Key is record
        Owner      : Unsigned_32 := 0;   -- grant slot
        Generation : Unsigned_32 := 0;
        Address    : Unsigned_64 := 0;   -- offset; 4-aligned
    end record;

    No_Key : constant Key := (Owner => 0, Generation => 0, Address => 0);

    function Shared_Key (Slot, Generation : Unsigned_32; Offset : Unsigned_64) return Key is
      ((Owner => Slot, Generation => Generation, Address => Offset))
    with Post => Shared_Key'Result.Owner = Slot and then
                 Shared_Key'Result.Generation = Generation and then
                 Shared_Key'Result.Address = Offset;

    -- Whether K is a shared key of grant slot Slot (any life, any offset).
    function Of_Grant (K : Key; Slot : Unsigned_32) return Boolean is
      (K.Owner = Slot);

    -- The bucket of a grant's shared keys.
    function Grant_Bucket (Slot : Unsigned_32) return Bucket_Index;

    function Bucket_Of (K : Key) return Bucket_Index
    with Post => Bucket_Of'Result = Grant_Bucket (K.Owner);

    -- Proved lemmas.

    -- Every shared key of one grant is in that grant's bucket: a sweep of
    -- one bucket finds every waiter of the grant.
    procedure Prove_Grant_One_Bucket (S, G1, G2 : Unsigned_32; O1, O2 : Unsigned_64)
    with Ghost, Global => null,
         Post => Bucket_Of (Shared_Key (S, G1, O1)) = Bucket_Of (Shared_Key (S, G2, O2))
                 and then Bucket_Of (Shared_Key (S, G1, O1)) = Grant_Bucket (S);

    -- Two lives of one slot are different keys, so ending one life never
    -- touches a waiter of the next.
    procedure Prove_Lives_Distinct (S, G1, G2 : Unsigned_32; O : Unsigned_64)
    with Ghost, Global => null,
         Pre  => G1 /= G2,
         Post => Shared_Key (S, G1, O) /= Shared_Key (S, G2, O);

end Futex_Keys;
