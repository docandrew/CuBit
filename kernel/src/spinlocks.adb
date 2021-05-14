-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- CuBitOS Locking & Synchronization Primitives
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;

with PerCPUData;
with TextIO; use TextIO;

package body Spinlocks
    with SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- Getter function for lock state.
    ---------------------------------------------------------------------------
    function isLocked (s : in Spinlock) return Boolean is (s.state = LOCKED);

    ---------------------------------------------------------------------------
    -- disable interrupts on this CPU and loop until we acquire the lock
    ---------------------------------------------------------------------------
    procedure enterCriticalSection (s : in out Spinlock) with
        SPARK_Mode => On
    is
        use ASCII;

        oldval : LockBool := LOCKED;

    begin
        -- disable interrupts
        PerCPUData.pushCLI;

        -- Can't reacquire the same lock from the same CPU.
        if s.state = LOCKED and s.cpu = PerCPUData.getCPUNumber then
            if s.name /= null then
                print (" Lock: "); println (s.name.all);
            else
                print (" Lock: (no name)");
            end if;
            raise SpinLockException with "Attempted reacquisition of lock from same CPU";
        end if;

        -- spin here until the lock is released and we can acquire it.
        -- we keep setting the state to LOCKED, if somebody else
        -- sets it to UNLOCKED, we get that update and exit the loop.
        while oldval = LOCKED loop
            x86.lock_xchg (s.state, LOCKED, oldval);
        end loop;

        -- save info for debugging and ensuring per-CPU mutex
        s.cpu := PerCPUData.getCPUNumber;
        
    end enterCriticalSection;

    ---------------------------------------------------------------------------
    -- Release the lock and set interrupts to their state prior to entering
    -- the lock.
    ---------------------------------------------------------------------------
    procedure exitCriticalSection (s : in out Spinlock) with
        SPARK_Mode => On
    is
        ignore : LockBool;
    begin
        if s.state /= LOCKED then
            if s.name /= null then
                print (" Lock: "); println (s.name.all);
            else
                print (" Lock: (no name)");
            end if;
            raise SpinLockException with "Attempted release of un-held lock.";
        end if;

        s.cpu := -1;

        x86.lock_xchg (s.state, UNLOCKED, ignore);

        -- if interrupts were enabled previously, then re-enable them.
        PerCPUData.popCLI;

    end exitCriticalSection;

end Spinlocks;