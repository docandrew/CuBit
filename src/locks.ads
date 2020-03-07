-------------------------------------------------------------------------------
-- Fortress OS
-- Copyright (C) 2019 Jon Andrew
--
-- Generic lock primitives to be used in other lock types.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package locks is
    -- Define a LockBool as a boolean type that we can perform atomic operations
    -- on in memory.
    subtype LockBool is Unsigned_32 range 0..1;
    
    UNLOCKED : constant LockBool := 0;
    LOCKED : constant LockBool := 1;
end locks;