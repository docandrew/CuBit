-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary Futex keys and buckets (docs/threads.md, IPC-010)
--
-- A private futex is (process, user address): its word is in one address
-- space. A shared futex (IPC-010) is (grant slot, grant life, byte offset
-- in the grant): both parties of a grant reach the same word through their
-- own mappings, and both resolve it to the same key, so a wake by one
-- finds the other's waiter. The two spaces never meet: a private key and a
-- shared key are never equal, whatever their numbers.
--
-- Buckets: a private key hashes its address and process; a shared key
-- hashes its grant slot alone, so every waiter of one grant is in one
-- bucket (and the overflow set), and ending a grant sweeps one bucket.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Futex_Keys with SPARK_Mode => On is

    Bucket_Count : constant := 64;
    Max_Waiter   : constant := 1023;

    type Bucket_Index is range 0 .. Bucket_Count - 1;

    -- A waiting thread. Zero means none.
    type Waiter_Id is range 0 .. Max_Waiter;
    No_Waiter : constant Waiter_Id := 0;

    type Key_Space is (Private_Space, Shared_Space);

    type Key is record
        Space      : Key_Space   := Private_Space;
        Owner      : Unsigned_32 := 0;   -- process, or grant slot
        -- The grant's life (shared keys): a later life of the same slot
        -- is another key. Zero for private keys.
        Generation : Unsigned_32 := 0;
        Address    : Unsigned_64 := 0;   -- user address, or offset; 4-aligned
    end record;

    No_Key : constant Key :=
      (Space => Private_Space, Owner => 0, Generation => 0, Address => 0);

    function Private_Key (Process : Unsigned_32; Address : Unsigned_64) return Key is
      ((Space => Private_Space, Owner => Process, Generation => 0, Address => Address))
    with Post => Private_Key'Result.Space = Private_Space and then
                 Private_Key'Result.Owner = Process and then
                 Private_Key'Result.Address = Address;

    function Shared_Key (Slot, Generation : Unsigned_32; Offset : Unsigned_64) return Key is
      ((Space => Shared_Space, Owner => Slot, Generation => Generation, Address => Offset))
    with Post => Shared_Key'Result.Space = Shared_Space and then
                 Shared_Key'Result.Owner = Slot and then
                 Shared_Key'Result.Generation = Generation and then
                 Shared_Key'Result.Address = Offset;

    -- Whether K is a shared key of grant slot Slot (any life, any offset).
    function Of_Grant (K : Key; Slot : Unsigned_32) return Boolean is
      (K.Space = Shared_Space and then K.Owner = Slot);

    -- The bucket of a grant's shared keys.
    function Grant_Bucket (Slot : Unsigned_32) return Bucket_Index;

    function Bucket_Of (K : Key) return Bucket_Index
    with Post => (if K.Space = Shared_Space then
                    Bucket_Of'Result = Grant_Bucket (K.Owner));

    -- Proved lemmas.

    -- No private key equals a shared key.
    procedure Prove_Spaces_Disjoint (P : Unsigned_32; A : Unsigned_64;
                                     S, G : Unsigned_32; O : Unsigned_64)
    with Ghost, Global => null,
         Post => Private_Key (P, A) /= Shared_Key (S, G, O);

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
