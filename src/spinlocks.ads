-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- @summary
-- CuBitOS Spinlocks
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with Locks; use Locks;
with x86;

package Spinlocks with
    SPARK_Mode => On
is
    -- type SpinLock is private;

    SpinLockException : exception;
    
    ---------------------------------------------------------------------------
    -- Structure for basic spinlocks.
    -- @field state - whether this spinlock is currently locked or not.
    --  This is used for formal verification.
    -- @field priorFlags - store the state of the RFLAGS register prior to
    --  entering a critical section using this lock. This is so that if
    --  interrupts were disabled prior to entering the critical section, we
    --  don't blindly re-enable them.
    --
    -- For debugging:
    -- @field cpu - the CPU number who is holding this lock.
    -- @field name - Name of this lock.
    ---------------------------------------------------------------------------
    type Spinlock is
    record
        state      : LockBool := UNLOCKED;
        priorFlags : x86.RFlags;
        cpu        : Integer := -1;
        -- callStack  : CallStackT := (others => 0);
        name       : access String;
    end record;

    ---------------------------------------------------------------------------
    -- Getter function for lock state. Necessary for SPARK verification,
    --  since contracts can't access the member variable .state of an abstract
    --  state.
    ---------------------------------------------------------------------------
    function isLocked(s : in Spinlock) return Boolean;

    ---------------------------------------------------------------------------
    -- enterCriticalSection tries to acquire a spinlock before returning. 
    -- Disables interrupts, so these need to be set up before attempting to
    -- use a Spinlocks.
    ---------------------------------------------------------------------------
    procedure enterCriticalSection(s : in out Spinlock) with
        Global => (In_Out => x86.interruptsEnabled),
        Post => isLocked(s);

    ---------------------------------------------------------------------------
    -- exitCriticalSection releases a Spinlocks. 
    --
    -- Re-enables interrupts if they were enabled prior to a call to
    --  enterCriticalSection. If they weren't, it won't.
    --
    -- It is an error to exitCriticalSection unless we've previously
    --  entered it via enterCriticalSection.
    --
    --  (We don't have an explicit check
    --  for PICInitialized, since we already check for locked, and we can't
    --  lock it without calling enterCriticalSection, which checks for PIC
    --  initialization.)
    ---------------------------------------------------------------------------
    procedure exitCriticalSection(s : in out Spinlock) with
        Global => (In_Out => x86.interruptsEnabled),
        Pre => isLocked(s),
        Post => not isLocked(s);

end Spinlocks;