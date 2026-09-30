with Interfaces;
with Spinlocks;
with Process_Lifetime;
package Process is
    subtype ProcessID is Natural range 0 .. 255;
    NO_PROCESS : constant ProcessID := 0;
    -- The production queues are woven through the thread table; here one
    -- table stands for both.
    subtype ThreadID is ProcessID;
    NO_THREAD : constant ThreadID := NO_PROCESS;
    type ProcessState is (INVALID, SLEEPING, READY);
    type Readiness_Origin is (Rescheduled, Awakened);
    type PCB is record
        state : ProcessState := INVALID;
        prev, next : ProcessID := NO_PROCESS;
        queueKey : Integer := 0;
        readiness : Readiness_Origin := Rescheduled;
        name : String (1 .. 4) := "test";
        cpu : Natural := 0;
        pinned : Boolean := False;
        lifetime : Process_Lifetime.State;
        queuedTSC : Interfaces.Unsigned_64 := 0;
        priority : Integer := 0;
        runKey : Interfaces.Unsigned_64 := 0;
    end record;
    type Table is array (1 .. 255) of PCB;
    proctab : Table;
    threadtab : Table renames proctab;
    function processOf (T : ThreadID) return ProcessID is (T);
    type ProcQueue is record
        lock : Spinlocks.Spinlock;
        head, tail : ProcessID := NO_PROCESS;
    end record;
    lock : Spinlocks.Spinlock;
    sleepList, readyList : ProcQueue;
    Ready_Count : Natural := 0;
    procedure ready (PID : ProcessID);
end Process;
