-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary Futex keys and buckets (docs/threads.md)
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Futex_Keys with SPARK_Mode => On is

    Bucket_Count : constant := 64;
    Max_Waiter   : constant := 1023;

    type Bucket_Index is range 0 .. Bucket_Count - 1;

    -- A waiting thread. Zero means none.
    type Waiter_Id is range 0 .. Max_Waiter;
    No_Waiter : constant Waiter_Id := 0;

    type Key is record
        Owner   : Unsigned_32 := 0;   -- process
        Address : Unsigned_64 := 0;   -- user virtual address, 4-aligned
    end record;

    function Bucket_Of (K : Key) return Bucket_Index;

end Futex_Keys;
