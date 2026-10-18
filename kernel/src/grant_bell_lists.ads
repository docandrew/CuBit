-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary A process's bell list (CAP-002 step 3): the grants it receives
-- whose doorbell is pending, oldest first, linked through the grant records
-- (as the owned and received lists already are).
--
-- Generic over the link fields so the same code runs in the kernel (links
-- in the grant records, under grantLock) and in the hosted test, which
-- checks it against a model over random sequences (tests/grant-doorbells).
-- The caller keeps a grant on the list exactly while its bell is pending
-- (Grant_Doorbells says when): Append only a grant not on the list, Remove
-- only one that is.
-------------------------------------------------------------------------------
generic
    type Slot is range <>;
    No_Slot : Slot;
    with function Previous (S : Slot) return Slot;
    with function Next (S : Slot) return Slot;
    with procedure Set_Previous (S : Slot; Value : Slot);
    with procedure Set_Next (S : Slot; Value : Slot);
package Grant_Bell_Lists is

    type List is record
        Head, Tail : Slot := No_Slot;
        Count      : Natural := 0;
    end record;

    Empty : constant List := (Head => No_Slot, Tail => No_Slot, Count => 0);

    -- Link S at the tail. S is not on the list.
    procedure Append (L : in out List; S : Slot)
      with Pre => S /= No_Slot;

    -- Unlink S. S is on the list.
    procedure Remove (L : in out List; S : Slot)
      with Pre => S /= No_Slot and then L.Count > 0;

    -- Unlink the head into S (No_Slot when empty).
    procedure Pop (L : in out List; S : out Slot);

end Grant_Bell_Lists;
