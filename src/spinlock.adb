-------------------------------------------------------------------------------
-- CubitOS
-- Copyright (C) 2019 Jon Andrew
--
-- CubitOS Locking & Synchronization Primitives
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package body Spinlock
    with SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- Getter function for lock state.
    ---------------------------------------------------------------------------
    function isLocked(s : in Spinlock) return Boolean is (s.state = LOCKED);

    ---------------------------------------------------------------------------
    -- disable interrupts and loop until we acquire the lock
    ---------------------------------------------------------------------------
    procedure enterCriticalSection(s : in out Spinlock) with
        SPARK_Mode => On
    is
        oldval : LockBool := LOCKED;
    begin
        s.priorFlags := x86.getFlags;       -- save state of interrupts
        x86.cli;                            -- disable interrupts, avoid deadlock

        -- spin here until the lock is released and we can acquire it.
        -- we keep setting the state to LOCKED, if somebody else
        -- sets it to UNLOCKED, we get that update and exit the loop.
        while oldval = LOCKED loop
            x86.lock_xchg(s.state, LOCKED, oldval);
        end loop;

    end enterCriticalSection;

    ---------------------------------------------------------------------------
    -- Release the lock and set interrupts to their state prior to entering
    -- the lock.
    ---------------------------------------------------------------------------
    procedure exitCriticalSection(s : in out Spinlock) with
        SPARK_Mode => On
    is
        ignore : LockBool;
    begin
        x86.lock_xchg(s.state, UNLOCKED, ignore);

        -- if interrupts were enabled previously, then re-enable them.
        if s.priorFlags.interrupt then
            x86.sti;
        end if;

    end exitCriticalSection;

end Spinlock;