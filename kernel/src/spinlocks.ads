with Locks;
with Interfaces;

-- Raw IRQ-masking locks, not sleeping mutexes. See docs/kernel-locking.md for
-- acquisition order, scheduler handoff, and the remaining audit obligations.
-- NMI and TLB acknowledgement service must never acquire these locks.
package Spinlocks with SPARK_Mode => On is
    SpinLockException : exception;
    type Lock_Name is access constant String;
    type Spinlock is private;

    -- Initialization only: never replace/copy a published live lock.
    procedure Initialize (S : out Spinlock; Name : Lock_Name := null);

    -- Snapshots, not stable claims about a concurrently changing lock.
    function isLocked (S : Spinlock) return Boolean;
    function ownedBy (S : Spinlock; CPU : Locks.CPU_ID) return Boolean;

    -- Trusted hardware adapter. No sequential postcondition on the released
    -- word: another CPU may acquire it before exitCriticalSection returns.
    procedure enterCriticalSection (S : in out Spinlock);
    procedure exitCriticalSection (S : in out Spinlock);

private
    type Spinlock is record
        Owner : Locks.State := Locks.Unowned with Atomic;
        Name : Lock_Name := null;
        Hold_Started : Interfaces.Unsigned_64 := 0;
        Measure_Hold : Boolean := False;
    end record;
end Spinlocks;
