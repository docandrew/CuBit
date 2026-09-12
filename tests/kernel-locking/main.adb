with Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Locks; use Locks;
with Spinlocks;
with PerCPUData;
with TLB_Shootdown;
with Process;
with Process.Queues;
with Process_Lifetime;

procedure Main is
    Shared, Nested : Spinlocks.Spinlock;
    Count, Mirror : Natural := 0;
    Failed : Boolean := False with Atomic;
    Iterations : constant := 100_000;

    procedure Check_Ready_Fairness is
        use Process;
        Q : ProcQueue;
        Got, Ignored : ProcessID;
        -- Independent stable-array oracle, not another linked-list insertion.
        type Entry_Info is record
            PID : ProcessID;
            Priority : Integer;
        end record;
        Expected : array (1 .. 16) of Entry_Info;
        Length : Natural := 0;
        Seed : Natural := 17;
        Present : array (ProcessID range 1 .. 16) of Boolean := [others => False];

        procedure Add (PID : ProcessID; Priority : Integer) is
            Position : Positive := Length + 1;
        begin
            Queues.insert (Q, PID, Priority, Ignored);
            pragma Assert (Ignored = PID);
            for I in 1 .. Length loop
                if Expected (I).Priority < Priority then
                    Position := I;
                    exit;
                end if;
            end loop;
            for I in reverse Position .. Length loop
                Expected (I + 1) := Expected (I);
            end loop;
            Expected (Position) := (PID, Priority);
            Length := Length + 1;
            Present (PID) := True;
        end Add;

        procedure Verify is
            Cursor : ProcessID := Q.head;
            Previous : ProcessID := NO_PROCESS;
        begin
            pragma Assert (not Queues.hasReadyPeer (Q, 100));
            if Length = 0 then
                pragma Assert (not Queues.hasReadyPeer (Q, -100));
            else
                pragma Assert (Queues.hasReadyPeer (Q, Expected (1).Priority));
                pragma Assert (Queues.hasReadyPeer (Q, Expected (1).Priority - 1));
                pragma Assert (not Queues.hasReadyPeer (Q, Expected (1).Priority + 1));
            end if;
            for I in 1 .. Length loop
                pragma Assert (Cursor = Expected (I).PID);
                pragma Assert (proctab (Cursor).prev = Previous);
                pragma Assert (proctab (Cursor).queueKey = Expected (I).Priority);
                Previous := Cursor;
                Cursor := proctab (Cursor).next;
            end loop;
            pragma Assert (Cursor = NO_PROCESS and Q.tail = Previous);
        end Verify;

        procedure Remove_First is
        begin
            Queues.dequeue (Q, Got);
            pragma Assert (Got = Expected (1).PID);
            Present (Got) := False;
            Length := Length - 1;
            for I in 1 .. Length loop
                Expected (I) := Expected (I + 1);
            end loop;
            pragma Assert (proctab (Got).prev = NO_PROCESS);
            pragma Assert (proctab (Got).next = NO_PROCESS);
        end Remove_First;
    begin
        -- Simulate quantum expiration using the production dequeue/reinsert
        -- path. The old >= insertion fails the very first FIFO check.
        for PID in 1 .. 3 loop Add (PID, 4); end loop;
        Verify;
        for Quantum in 1 .. 300 loop
            Remove_First;
            pragma Assert (Got = (Quantum - 1) mod 3 + 1);
            Add (Got, 4);
            Verify;
        end loop;
        while Length > 0 loop Remove_First; end loop;
        -- Exercise arrival, blocking, re-entry, ties and distinct priorities.
        for Step in 1 .. 10_000 loop
            Seed := (Seed * 251 + 17) mod 65521;
            declare
                PID : constant ProcessID := Seed mod 16 + 1;
            begin
                if not Present (PID) then
                    Add (PID, (Seed / 16) mod 5);
                elsif Length > 0 then
                    Remove_First;
                end if;
            end;
            Verify;
        end loop;
        while Length > 0 loop Remove_First; Verify; end loop;
        Queues.dequeue (Q, Got);
        pragma Assert (Got = NO_PROCESS and PerCPUData.Depth = 0);
        pragma Assert (not Queues.hasReadyPeer (Q, -100));
        pragma Assert (not Queues.hasAwakenedPeer (Q, -100));
        Add (1, 4);
        Add (2, 4);
        Add (3, 3);
        pragma Assert (not Queues.hasAwakenedPeer (Q, 4));
        proctab (3).readiness := Awakened;
        pragma Assert (not Queues.hasAwakenedPeer (Q, 4));
        pragma Assert (Queues.hasAwakenedPeer (Q, 3));
        proctab (2).readiness := Awakened;
        pragma Assert (Queues.hasAwakenedPeer (Q, 4));
        pragma Assert (not Queues.hasAwakenedPeer (Q, 5));
        Verify;
        -- The wake flag changes when to rotate, not the FIFO selection.
        Remove_First;
        pragma Assert (Got = 1);
        Remove_First;
        pragma Assert (Got = 2);
        proctab (2).readiness := Rescheduled;
        Add (2, 4);
        pragma Assert (not Queues.hasAwakenedPeer (Q, 4));
        while Length > 0 loop Remove_First; end loop;
        proctab (3).readiness := Rescheduled;
        -- Interrupted turn resumes before equal peers, never above higher
        -- priority work. Exhausted/voluntarily relinquished turns use FIFO.
        Queues.insert (Q, 1, 4, Ignored);
        Queues.insert (Q, 2, 5, Ignored);
        Queues.insert (Q, 3, 4, Ignored, Queues.Resume_Turn);
        pragma Assert (Queues.hasReadyPeer (Q, 4, Queues.Strictly_Higher));
        pragma Assert (not Queues.hasReadyPeer (Q, 5, Queues.Strictly_Higher));
        pragma Assert (not Queues.hasReadyPeer (Q, Integer'Last, Queues.Strictly_Higher));
        pragma Assert (Queues.hasReadyPeer (Q, Integer'First, Queues.Strictly_Higher));
        Queues.dequeue (Q, Got);
        pragma Assert (Got = 2);
        Queues.dequeue (Q, Got);
        pragma Assert (Got = 3);
        Queues.insert (Q, 3, 4, Ignored);
        Queues.dequeue (Q, Got);
        pragma Assert (Got = 1);
        Queues.dequeue (Q, Got);
        pragma Assert (Got = 3 and Queues.isEmpty (Q));
        Ada.Text_IO.Put_Line
          ("READY-FAIRNESS-CHECK: PASS (300 quanta, 10000 stable-priority oracle steps)");
    end Check_Ready_Fairness;

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
        Spinlocks.enterCriticalSection (Process.lock);
        Process.Queues.insertDelta (Process.sleepList, 1, 3, Ignored);
        Process.Queues.insertDelta (Process.sleepList, 2, 7, Ignored);
        Process.Queues.insertDelta (Process.sleepList, 3, 11, Ignored);
        Process.Queues.detach (Process.sleepList, 2, Process.Queues.Delta_Queue);
        pragma Assert (Process.proctab(3).queueKey = 8);
        Process.Queues.detach (Process.sleepList, 1, Process.Queues.Delta_Queue);
        pragma Assert (Process.proctab(3).queueKey = 11);
        Process.Queues.detach (Process.sleepList, 2, Process.Queues.Delta_Queue);
        Process.Queues.detach (Process.sleepList, 3, Process.Queues.Delta_Queue);
        pragma Assert (Process.Queues.isEmpty (Process.sleepList));
        Spinlocks.exitCriticalSection (Process.lock);
        Process.Queues.popFront (Process.readyList, Removed);
        pragma Assert (Removed = Process.NO_PROCESS);
        pragma Assert (PerCPUData.Depth = 0);
        -- A coalesced timer advances every elapsed deadline without a loop
        -- per missed millisecond, preserving the first future delta.
        Spinlocks.enterCriticalSection (Process.lock);
        for PID in 1 .. 3 loop
            Process.proctab(PID).state := Process.SLEEPING;
            Process.Queues.insertDelta (Process.sleepList, PID, PID * 3, Ignored);
        end loop;
        Spinlocks.exitCriticalSection (Process.lock);
        Process.Queues.clockTick (7);
        pragma Assert (Process.proctab(1).state = Process.READY);
        pragma Assert (Process.proctab(2).state = Process.READY);
        pragma Assert (Process.sleepList.head = 3 and Process.proctab(3).queueKey = 2);
        Process.Queues.clockTick (1);
        pragma Assert (Process.proctab(3).queueKey = 1);
        Process.Queues.clockTick (1_000_000);
        pragma Assert (Process.Queues.isEmpty (Process.sleepList));
        for PID in 1 .. 3 loop
            Process.Queues.popFront (Process.readyList, Removed);
            pragma Assert (Removed = PID);
        end loop;
        Ada.Text_IO.Put_Line
          ("SLEEP-QUEUE-CHECK: PASS (timer/IPC wake serialization, delta preservation, queue endpoints)");
    end Check_Queues;

    procedure Check_Lifetime is
        use Process_Lifetime;
        Life : Process_Lifetime.State := Initial_State;
        CPU_Present : Boolean := False;
        Reaped : Natural := 0;
        Rounds : constant := 10_000;
        Deadline : constant Time := Clock + Seconds (10);
    begin
        declare
            task type Participant (Role : Natural);
            task body Participant is
                OK, Done : Boolean;
            begin
                PerCPUData.Set_CPU (Role);
                loop
                    Spinlocks.enterCriticalSection (Shared);
                    Done := Reaped = Rounds or Failed;
                    if not Done then
                        case Role is
                            when 0 =>
                                if Can_Run (Life) then
                                    Enter_CPU (Life, OK);
                                    pragma Assert (OK and not CPU_Present);
                                    CPU_Present := True;
                                elsif Closing (Life) and Executing (Life) then
                                    -- Model the adapter's acknowledgement only
                                    -- after leaving the old stack/address space.
                                    CPU_Present := False;
                                    Leave_CPU (Life, OK);
                                    pragma Assert (OK);
                                end if;
                            when 1 =>
                                if Executing (Life) then
                                    Request_Stop (Life);
                                    pragma Assert (not Can_Reap (Life));
                                    Claim_Reap (Life, OK);
                                    pragma Assert (not OK);
                                end if;
                            when others =>
                                if Can_Reap (Life) then
                                    pragma Assert (not CPU_Present);
                                    Claim_Reap (Life, OK);
                                    pragma Assert (OK);
                                    Claim_Reap (Life, OK);
                                    pragma Assert (not OK);
                                    Finish_Reap (Life, OK);
                                    pragma Assert (OK and Retired (Life));
                                    Enter_CPU (Life, OK);
                                    pragma Assert (not OK);
                                    Reaped := Reaped + 1;
                                    Life := Initial_State; -- Fresh PID incarnation
                                end if;
                        end case;
                    end if;
                    if Clock >= Deadline then Failed := True; end if;
                    Spinlocks.exitCriticalSection (Shared);
                    exit when Done or Failed;
                    delay 0.0;
                end loop;
            exception
                when others =>
                    Failed := True;
                    if Spinlocks.ownedBy (Shared, Role) then
                        Spinlocks.exitCriticalSection (Shared);
                    end if;
            end Participant;
            CPU : Participant (0);
            Stopper : Participant (1);
            Reaper : Participant (2);
        begin
            null;
        end;
        pragma Assert (not Failed and Reaped = Rounds and not CPU_Present);
        Ada.Text_IO.Put_Line
          ("PROCESS-LIFETIME-CHECK: PASS (10000 concurrent stop/acknowledge/reap/reuse rounds)");
    end Check_Lifetime;
begin
    Check_Ready_Fairness;
    Check_Policy;
    Check_Lifetime;

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
