-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- CuBitOS Locking & Synchronization Primitives
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;

with PerCPUData;

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
            -- println ("SpinLock: Fatal error.");
            -- println (" Call Stack:");
            -- println (s.callStack(1));
            -- println (s.callStack(2));
            -- println (s.callStack(3));
            -- println (s.callStack(4));
            -- println (s.callStack(5));
            -- println (s.callStack(6));
            -- println (s.callStack(7));
            -- println (s.callStack(8));

            raise SpinLockException with "Attempted reacquisition of lock from same CPU";
        end if;

        -- spin here until the lock is released and we can acquire it.
        -- we keep setting the state to LOCKED, if somebody else
        -- sets it to UNLOCKED, we get that update and exit the loop.
        while oldval = LOCKED loop
            x86.lock_xchg (s.state, LOCKED, oldval);
        end loop;

        -- save info for debugging.
        -- @TODO I believe the rbp stack frame chain is only saved in debug mode.
        --  we probably need to skip this for release builds.
        s.cpu := PerCPUData.getCPUNumber;
        
        -- getCallStack : declare
        --     rbp : Unsigned_64 := x86.getRBP;
        --     i : Integer := 1;
        -- begin
        --     loop
        --         exit when i > 10 or 
        --                   rbp = 0 or 
        --                   rbp < Unsigned_64(Virtmem.KERNEL_BASE) or
        --                   rbp = Unsigned_64'Last;

        --         declare
        --             rip : Unsigned_64 with
        --                 Import, Address => Util.addrToNum (rbp + 8);
        --             nextrbp : Unsigned_64 with
        --                 Import, Address => Util.addrToNum (rbp);
        --         begin
        --             callStack(i) := rip;
        --             rbp := nextrbp;
        --         end;

        --         i := i + 1;
        --     end loop;

        --     if i < 10 then
        --         for j in i..10 loop
        --             callStack(i) := 0;
        --         end loop;
        --     end if;
        -- end getCallStack;
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
            raise SpinLockException with "Attempted release of un-held lock.";
        end if;

        s.cpu := -1;

        x86.lock_xchg (s.state, UNLOCKED, ignore);

        -- if interrupts were enabled previously, then re-enable them.
        PerCPUData.popCLI;

    end exitCriticalSection;

end Spinlocks;