with Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Locks; use Locks;
with Spinlocks;
with PerCPUData;
with TLB_Shootdown;
with Process;
with Process.Queues;

procedure Main is
    Shared, Nested : Spinlocks.Spinlock;
    Count, Mirror : Natural := 0;
    Failed : Boolean := False with Atomic;
    Iterations : constant := 100_000;

    procedure Check_Policy is
        S, Before : State;
        Result : Acquire_Result;
        Success : Boolean;
    begin
        for Owner in CPU_ID loop
            for Other in CPU_ID loop
                S := Unowned;
                Acquire (S, Owner, Result);
                pragma Assert (Result = Acquired and Owned_By (S, Owner));
                Before := S;
                Acquire (S, Other, Result);
                pragma Assert (S = Before);
                pragma Assert (Result =
                  (if Owner = Other then Reentrant else Contended));
                Release (S, Other, Success);
                pragma Assert (Success = (Owner = Other));
                pragma Assert (if Success then S = Unowned else S = Before);
            end loop;
        end loop;
    end Check_Policy;

    procedure Check_Queues is
        use type Process.ProcessState;
        Ignored, Removed : Process.ProcessID;
        Woken : Boolean;
    begin
        -- Model the caller's locked sleep publication. Exercise the actual
        -- kernel queue implementation; the fixture only supplies PCB storage
        -- and a ready() adapter that checks the lock protocol.
        Spinlocks.enterCriticalSection (Process.lock);
        Spinlocks.enterCriticalSection (Process.sleepList.lock);
        for PID in 1 .. 3 loop
            Process.proctab(PID).state := Process.SLEEPING;
            Process.Queues.insertDeltaNoLock
              (Process.sleepList, PID, (if PID = 1 then 1 else 2), Ignored);
        end loop;
        Spinlocks.exitCriticalSection (Process.sleepList.lock);
        Spinlocks.exitCriticalSection (Process.lock);
        Process.Queues.clockTick;
        pragma Assert (Process.Ready_Count = 1);
        pragma Assert (Process.proctab(1).state = Process.READY);
        pragma Assert (Process.sleepList.head = 2);
        Process.Queues.wakeFromSleep (2, Woken);
        pragma Assert (Woken and Process.Ready_Count = 2);
        Process.Queues.wakeFromSleep (2, Woken);
        pragma Assert (not Woken and Process.Ready_Count = 2);
        Process.Queues.clockTick;
        pragma Assert (Process.Ready_Count = 3);
        pragma Assert (Process.Queues.isEmpty (Process.sleepList));
        Process.Queues.clockTick;
        Process.Queues.popFront (Process.readyList, Removed);
        pragma Assert (Removed = 1);
        Process.Queues.popBack (Process.readyList, Removed);
        pragma Assert (Removed = 3);
        Process.Queues.popFront (Process.readyList, Removed);
        pragma Assert (Removed = 2);
        Process.Queues.popBack (Process.readyList, Removed);
        pragma Assert (Removed = Process.NO_PROCESS);
        Process.Queues.popFront (Process.readyList, Removed);
        pragma Assert (Removed = Process.NO_PROCESS);
        pragma Assert (PerCPUData.Depth = 0);
        Ada.Text_IO.Put_Line
          ("SLEEP-QUEUE-CHECK: PASS (timer/IPC wake serialization, delta preservation, queue endpoints)");
    end Check_Queues;
begin
    Check_Policy;

    -- A real lock held by CPU 4 makes all workers exercise the contention path
    -- before their concurrent increment test. Only CLI/GS/TLB/trace are mocked.
    PerCPUData.Set_CPU (4);
    Spinlocks.enterCriticalSection (Shared);
    declare
        task type Worker (CPU : CPU_ID);
        task body Worker is
        begin
            PerCPUData.Set_CPU (CPU);
            for I in 1 .. Iterations loop
                Spinlocks.enterCriticalSection (Shared);
                pragma Assert (Spinlocks.ownedBy (Shared, CPU));
                pragma Assert (PerCPUData.Depth = 1);
                pragma Assert (Count = Mirror);
                Count := Count + 1;
                Spinlocks.enterCriticalSection (Nested);
                pragma Assert (PerCPUData.Depth = 2);
                Mirror := Mirror + 1;
                Spinlocks.exitCriticalSection (Nested);
                pragma Assert (PerCPUData.Depth = 1);
                Spinlocks.exitCriticalSection (Shared);
                pragma Assert (PerCPUData.Depth = 0);
            end loop;
        exception
            when others => Failed := True;
        end Worker;
        W0 : Worker (0);
        W1 : Worker (1);
        W2 : Worker (2);
        W3 : Worker (3);
        Deadline : constant Time := Clock + Seconds (5);
    begin
        loop
            exit when (for all CPU in 0 .. 3 => TLB_Shootdown.Calls (CPU) > 0);
            if Clock >= Deadline then
                Failed := True;
                exit;
            end if;
            delay 0.001;
        end loop;
        Spinlocks.exitCriticalSection (Shared);
        -- Ada task scope joins every worker before inspecting the final count.
    end;
    pragma Assert (not Failed and Count = 4 * Iterations and Count = Mirror);
    pragma Assert (not Spinlocks.isLocked (Shared));
    pragma Assert (PerCPUData.Depth = 0);

    -- A foreign release must not clear the live atomic word.
    PerCPUData.Set_CPU (0);
    Spinlocks.enterCriticalSection (Shared);
    PerCPUData.Set_CPU (1);
    declare
        Rejected : Boolean := False;
    begin
        begin
            Spinlocks.exitCriticalSection (Shared);
        exception
            when Spinlocks.SpinLockException => Rejected := True;
        end;
        pragma Assert (Rejected and Spinlocks.ownedBy (Shared, 0));
    end;
    PerCPUData.Set_CPU (0);
    Spinlocks.exitCriticalSection (Shared);
    Check_Queues;
    Ada.Text_IO.Put_Line
      ("LOCKING-CHECK: PASS (400000 increments, nested exclusion, foreign release, contention service)");
end Main;
