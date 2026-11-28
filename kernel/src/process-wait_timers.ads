-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary The deadlines of one-object Waits (IPC revision 2, S1): one
-- Wait_Deadlines heap per CPU, keyed by TSC.
--
-- @description
-- A thread that waits with a deadline joins the heap of the CPU it waits on
-- before it looks at its object (Arm), and leaves it itself once its wait
-- is over (Disarm). A waker never touches a heap. The CPU's timer takes due
-- threads (Expire, at most Expire_Chunk per tick, the rest at the next
-- alarm) and times each out under its object's lock (Notifications or
-- Private_Futex: Time_Out), so a timeout racing a wake resolves under that
-- lock: the first to see the wait Pending ends it. A thread whose deadline
-- came before it enqueued finds Wait_Timed_Out under the object's lock and
-- does not block.
--
-- Lock order: a heap lock, then an object lock (a notification's or a
-- process's private futex lock), then Process.lock. Nothing that holds an
-- object lock or Process.lock takes a heap lock.
-------------------------------------------------------------------------------
package Process.Wait_Timers is

    -- Due waits one tick times out under one hold of its heap's lock.
    Expire_Chunk : constant := 16;

    -- How late another CPU's due deadline may be before this CPU's timer
    -- times it out (that CPU's timer is not running its clock work).
    Steal_After_Us : constant := 2_000;

    -- The TSC value of an absolute deadline on the user monotonic clock
    -- (microseconds); Passed when it is not in the future.
    procedure Deadline_Of (deadlineUs : Unsigned_64; tsc : out Unsigned_64; passed : out Boolean);

    -- The same for an absolute deadline on the kernel's millisecond clock
    -- (the old WAIT's futex form).
    procedure Deadline_Of_Ms (deadlineMs : Unsigned_64; tsc : out Unsigned_64; passed : out Boolean);

    -- The calling thread, its oneWait fields set (status Wait_Pending),
    -- joins this CPU's heap. No object lock held.
    procedure Arm (me : ThreadID; tsc : Unsigned_64);

    -- The calling thread, its wait over, leaves the heap it joined, if its
    -- deadline has not taken it out already. No object lock held.
    procedure Disarm (me : ThreadID);

    -- This CPU's timer: time out the waits that are due by now (TSC), and
    -- another CPU's that are Steal_After_Us overdue.
    procedure Expire (cpu : Natural; now : Unsigned_64);

    -- Caller holds the lock of the object t waits on (or waited on): its
    -- wait ended (how, with bits); t runs again if it blocked, on this CPU
    -- when sync (readySync). The object's lock is held until t is readied,
    -- so the reaper (Cancel takes it) never frees a thread being finished.
    procedure Finish (t : ThreadID; how : One_Wait_Status; bits : Unsigned_64 := 0;
                      sync : Boolean := False);

    -- The calling thread sleeps (SLEEPING) until TSC tsc, or until
    -- Wake_Sleeper. No lock held.
    procedure Sleep (tsc : Unsigned_64);

    -- t, asleep in Sleep, runs again now (woken); else nothing. Caller
    -- does not hold Process.lock.
    procedure Wake_Sleeper (t : ThreadID; woken : out Boolean);

    -- The reaper: a thread that will not run again leaves its heap, its
    -- object and its bound notification. Not called with Process.lock held.
    procedure Cancel (t : ThreadID);

end Process.Wait_Timers;
