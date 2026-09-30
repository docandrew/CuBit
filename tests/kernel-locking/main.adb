with Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Locks; use Locks;
with Spinlocks;
with PerCPUData;
with TLB_Shootdown;
with Process;
with Process.Queues;
with Process_Lifetime;
with Virtual_Deadlines;
with Interfaces;

procedure Main is
    Shared, Nested : Spinlocks.Spinlock;
    Count, Mirror : Natural := 0;
    Failed : Boolean := False with Atomic;
    Iterations : constant := 100_000;

    procedure Check_Ready_Fairness is
        use Process;
        use type Interfaces.Unsigned_64;
        Q : ProcQueue;
        Got, Ignored : ProcessID;
        -- Independent stable-array oracle, not another linked-list insertion.
        type Entry_Info is record
            PID : ProcessID;
            Key : Interfaces.Unsigned_64;
        end record;
        Expected : array (1 .. 16) of Entry_Info;
        Length : Natural := 0;
        Seed : Natural := 17;
        Present : array (ProcessID range 1 .. 16) of Boolean := [others => False];

        procedure Add (PID : ProcessID; Key : Interfaces.Unsigned_64) is
            Position : Positive := Length + 1;
        begin
            Queues.insertByKey (Q, PID, Key, Ignored);
            pragma Assert (Ignored = PID);
            for I in 1 .. Length loop
                if Expected (I).Key > Key then
                    Position := I;
                    exit;
                end if;
            end loop;
            for I in reverse Position .. Length loop
                Expected (I + 1) := Expected (I);
            end loop;
            Expected (Position) := (PID, Key);
            Length := Length + 1;
            Present (PID) := True;
        end Add;

        procedure Verify is
            Cursor : ProcessID := Q.head;
            Previous : ProcessID := NO_PROCESS;
        begin
            pragma Assert
              (Queues.headKey (Q) =
                 (if Length = 0 then Virtual_Deadlines.Idle_Key else Expected (1).Key));
            for I in 1 .. Length loop
                pragma Assert (Cursor = Expected (I).PID);
                pragma Assert (proctab (Cursor).prev = Previous);
                pragma Assert (proctab (Cursor).runKey = Expected (I).Key);
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
        -- Equal deadlines rotate FIFO through the production dequeue and
        -- reinsert path.
        for PID in 1 .. 3 loop Add (PID, 4); end loop;
        Verify;
        for Rotation in 1 .. 300 loop
            Remove_First;
            pragma Assert (Got = (Rotation - 1) mod 3 + 1);
            Add (Got, 4);
            Verify;
        end loop;
        while Length > 0 loop Remove_First; end loop;
        -- Arrival, blocking, re-entry, ties and distinct deadlines.
        for Step in 1 .. 10_000 loop
            Seed := (Seed * 251 + 17) mod 65521;
            declare
                PID : constant ProcessID := Seed mod 16 + 1;
            begin
                if not Present (PID) then
                    Add (PID, Interfaces.Unsigned_64 ((Seed / 16) mod 5));
                elsif Length > 0 then
                    Remove_First;
                end if;
            end;
            Verify;
        end loop;
        while Length > 0 loop Remove_First; Verify; end loop;
        Queues.dequeue (Q, Got);
        pragma Assert (Got = NO_PROCESS and PerCPUData.Depth = 0);
        pragma Assert (Queues.headKey (Q) = Virtual_Deadlines.Idle_Key);

        -- Another CPU takes the earliest entry that is ordinary, unpinned
        -- work; the idle thread and pinned threads stay.
        proctab (1).priority := -1;
        proctab (2).pinned := True;
        Queues.insertByKey (Q, 1, Virtual_Deadlines.Idle_Key, Ignored);
        Queues.insertByKey (Q, 2, 10, Ignored);
        Queues.insertByKey (Q, 3, 20, Ignored);
        Queues.insertByKey (Q, 4, 15, Ignored);
        pragma Assert (Queues.headKey (Q) = 10);
        pragma Assert (Queues.takeableKey (Q) = 15);
        Queues.takeFirst (Q, Got);
        pragma Assert (Got = 4);
        pragma Assert (Queues.takeableKey (Q) = 20);
        Queues.takeFirst (Q, Got);
        pragma Assert (Got = 3);
        pragma Assert (Queues.takeableKey (Q) = Virtual_Deadlines.Idle_Key);
        Queues.takeFirst (Q, Got);
        pragma Assert (Got = NO_PROCESS);
        Queues.dequeue (Q, Got);
        pragma Assert (Got = 2);
        Queues.dequeue (Q, Got);
        pragma Assert (Got = 1 and Queues.isEmpty (Q));
        proctab (1).priority := 0;
        proctab (2).pinned := False;
        Ada.Text_IO.Put_Line ("READY-LIST-CHECK: PASS (deadline order, FIFO ties, taking skips idle and pinned)");
    end Check_Ready_Fairness;

    -- IPC hands work to a receiver on the sender's CPU when one is waiting,
    -- else to the longest-waiting receiver; the queue stays consistent.
    procedure Check_Receiver_Preference is
        use Process;
        Q : ProcQueue renames Process.sleepList;
        Got, Ignored : ProcessID;
    begin
        for T in 11 .. 15 loop
            proctab (T).cpu := (if T = 13 then 2 elsif T = 15 then 2 else 1);
            Queues.enqueue (Q, T, Ignored);
        end loop;
        Queues.dequeuePreferring (Q, 2, Got);
        pragma Assert (Got = 13);                       -- first on CPU 2
        Queues.dequeuePreferring (Q, 2, Got);
        pragma Assert (Got = 15);
        Queues.dequeuePreferring (Q, 2, Got);
        pragma Assert (Got = 11);                       -- none left: FIFO head
        Queues.dequeuePreferring (Q, 3, Got);
        pragma Assert (Got = 12);
        Queues.dequeue (Q, Got);
        pragma Assert (Got = 14 and then Queues.isEmpty (Q));
        Queues.dequeuePreferring (Q, 1, Got);
        pragma Assert (Got = NO_PROCESS);
        for T in 11 .. 15 loop
            pragma Assert (proctab (T).prev = NO_PROCESS and then proctab (T).next = NO_PROCESS);
            proctab (T).cpu := 0;
        end loop;
        Ada.Text_IO.Put_Line ("RECEIVER-PREFERENCE-CHECK: PASS (same-CPU first, FIFO fallback, links cleared)");
    end Check_Receiver_Preference;

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
        use type Interfaces.Unsigned_64;
    begin
        -- Model the caller's locked sleep publication. Exercise the actual
        -- kernel queue implementation; the fixture only supplies PCB storage
        -- and a ready() adapter that checks the lock protocol.
        Spinlocks.enterCriticalSection (Process.lock);
        Spinlocks.enterCriticalSection (Process.sleepList.lock);
        for PID in 1 .. 3 loop
            Process.proctab(PID).state := Process.SLEEPING;
            Process.Queues.insertByKeyNoLock
              (Process.sleepList, PID, (if PID = 1 then 1 else 2), Ignored);
        end loop;
        Spinlocks.exitCriticalSection (Process.sleepList.lock);
        Spinlocks.exitCriticalSection (Process.lock);
        pragma Assert (Process.Queues.nextWake = 1);
        Process.Queues.expireSleepers (1);
        pragma Assert (Process.Ready_Count = 1);
        pragma Assert (Process.proctab(1).state = Process.READY);
        pragma Assert (Process.sleepList.head = 2);
        Process.Queues.wakeFromSleep (2, Woken);
        pragma Assert (Woken and Process.Ready_Count = 2);
        Process.Queues.wakeFromSleep (2, Woken);
        pragma Assert (not Woken and Process.Ready_Count = 2);
        Process.Queues.expireSleepers (2);
        pragma Assert (Process.Ready_Count = 3);
        pragma Assert (Process.Queues.isEmpty (Process.sleepList));
        pragma Assert (Process.Queues.nextWake = Virtual_Deadlines.Idle_Key);
        Process.Queues.expireSleepers (Interfaces.Unsigned_64'Last);
        Process.Queues.popFront (Process.readyList, Removed);
        pragma Assert (Removed = 1);
        Process.Queues.popBack (Process.readyList, Removed);
        pragma Assert (Removed = 3);
        Process.Queues.popFront (Process.readyList, Removed);
        pragma Assert (Removed = 2);
        Process.Queues.popBack (Process.readyList, Removed);
        pragma Assert (Removed = Process.NO_PROCESS);
        Spinlocks.enterCriticalSection (Process.lock);
        -- Absolute deadlines: removing one sleeper changes no other's.
        Process.Queues.insertByKey (Process.sleepList, 1, 3, Ignored);
        Process.Queues.insertByKey (Process.sleepList, 2, 7, Ignored);
        Process.Queues.insertByKey (Process.sleepList, 3, 11, Ignored);
        Process.Queues.detach (Process.sleepList, 2);
        pragma Assert (Process.proctab(3).runKey = 11);
        Process.Queues.detach (Process.sleepList, 1);
        pragma Assert (Process.Queues.nextWake = 11);
        Process.Queues.detach (Process.sleepList, 2);
        Process.Queues.detach (Process.sleepList, 3);
        pragma Assert (Process.Queues.isEmpty (Process.sleepList));
        Spinlocks.exitCriticalSection (Process.lock);
        Process.Queues.popFront (Process.readyList, Removed);
        pragma Assert (Removed = Process.NO_PROCESS);
        pragma Assert (PerCPUData.Depth = 0);
        -- A late timer wakes every passed deadline in one expiry, and keeps
        -- the first future one.
        Spinlocks.enterCriticalSection (Process.lock);
        for PID in 1 .. 3 loop
            Process.proctab(PID).state := Process.SLEEPING;
            Process.Queues.insertByKey
              (Process.sleepList, PID, Interfaces.Unsigned_64 (PID * 3), Ignored);
        end loop;
        Spinlocks.exitCriticalSection (Process.lock);
        Process.Queues.expireSleepers (7);
        pragma Assert (Process.proctab(1).state = Process.READY);
        pragma Assert (Process.proctab(2).state = Process.READY);
        pragma Assert (Process.sleepList.head = 3 and Process.Queues.nextWake = 9);
        Process.Queues.expireSleepers (8);
        pragma Assert (Process.sleepList.head = 3);
        Process.Queues.expireSleepers (1_000_000);
        pragma Assert (Process.Queues.isEmpty (Process.sleepList));
        for PID in 1 .. 3 loop
            Process.Queues.popFront (Process.readyList, Removed);
            pragma Assert (Removed = PID);
        end loop;
        Ada.Text_IO.Put_Line
          ("SLEEP-QUEUE-CHECK: PASS (timer/IPC wake serialization, absolute deadlines, queue endpoints)");
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
    Check_Receiver_Preference;
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
