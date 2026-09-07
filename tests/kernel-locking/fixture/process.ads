with Spinlocks;
package Process is
    subtype ProcessID is Natural range 0 .. 255;
    NO_PROCESS : constant ProcessID := 0;
    type ProcessState is (INVALID, SLEEPING, READY);
    type PCB is record
        state : ProcessState := INVALID;
        prev, next : ProcessID := NO_PROCESS;
        queueKey : Integer := 0;
        name : String (1 .. 4) := "test";
    end record;
    proctab : array (1 .. 255) of PCB;
    type ProcQueue is record
        lock : Spinlocks.Spinlock;
        head, tail : ProcessID := NO_PROCESS;
    end record;
    lock : Spinlocks.Spinlock;
    sleepList, readyList : ProcQueue;
    Ready_Count : Natural := 0;
    procedure ready (PID : ProcessID);
end Process;
