pragma Ada_2022;
with System;
with Capability_ABI;
with Notification_ABI;
with Notification_Objects;
with Object_Table;
with PerCPUData;
with Process_Capabilities;
with Quiescent_Reclamation;
with Process.IPC;
with Process.Wait_Timers;

package body Process.Notifications is

    package NO renames Notification_Objects;
    package NA renames Notification_ABI;
    package Caps renames Capabilities;
    use type Caps.CapabilityType;
    use type NO.Object_Id;
    use type NO.Thread_Ref;
    use type NO.Action;
    use type NO.Bind_Outcome;
    use type NA.Outcome;

    pragma Compile_Time_Error
      (Natural (NO.Thread_Ref'Last) /= Natural (ThreadID'Last),
       "Notification_Objects.Thread_Ref must cover ThreadID");
    pragma Compile_Time_Error
      (Natural (NO.Owner_Ref'Last) /= Natural (ProcessID'Last),
       "Notification_Objects.Owner_Ref must cover ProcessID");
    -- Every process may hold its whole limit at once.
    pragma Compile_Time_Error
      (NA.Per_Process_Limit * (Natural (ProcessID'Last) + 1) > NO.Max_Objects,
       "the table must hold every process's limit");

    -- One object: its lock, the generation its handles carry while it lives
    -- (0 once it ended), and its state. The lock and the generation are
    -- written only under the lock; the lock is set up once, when its page
    -- is mapped, and never reset (a stale waker may hold it while the slot
    -- is reused).
    type Kernel_Notification is record
        Lock  : Spinlocks.Spinlock;
        Gen   : Unsigned_32 := 0;
        State : NO.Notification := NO.Empty;
    end record;

    Entries_Per_Page : constant := 40;
    type Page_Records is array (0 .. Entries_Per_Page - 1) of aliased Kernel_Notification;

    objectLockName : aliased String := "notification";
    tableLockName  : aliased String := "notifications";
    chargeLockName : aliased String := "notify-charge";
    tableLock  : Spinlocks.Spinlock;
    -- Serializes the per-process counts (notificationsHeld). A leaf lock.
    chargeLock : Spinlocks.Spinlock;

    procedure resetRecord (E : in out Kernel_Notification) is null;

    procedure allocPage (Page_Bytes : Natural; Addr : out System.Address) is
    begin
        allocTablePage (Page_Bytes, Addr);
        if System."/=" (Addr, System.Null_Address) then
            declare
                Page : Page_Records with Import, Address => Addr;
            begin
                for E of Page loop
                    Spinlocks.Initialize (E.Lock, objectLockName'Access);
                    E.Gen := 0;
                    E.State := NO.Empty;
                end loop;
            end;
        end if;
    end allocPage;

    procedure lockTable is
    begin
        Spinlocks.enterCriticalSection (tableLock);
    end lockTable;

    procedure unlockTable is
    begin
        Spinlocks.exitCriticalSection (tableLock);
    end unlockTable;

    package Table is new Object_Table
      (Element          => Kernel_Notification,
       Max_Id           => NO.Max_Objects,
       Entries_Per_Page => Entries_Per_Page,
       Reserved_Last    => 0,
       -- A handle's generation is the ledger's plus one, never 0.
       Generation_Limit => Unsigned_32'Last - 1,
       Reset            => resetRecord,
       Alloc_Page       => allocPage,
       Free_Page        => freeTablePage,
       Lock             => lockTable,
       Unlock           => unlockTable);

    procedure Setup is
    begin
        Spinlocks.Initialize (tableLock, tableLockName'Access);
        Spinlocks.Initialize (chargeLock, chargeLockName'Access);
        Table.Initialize;
    end Setup;

    procedure Quiescent (cpu : Natural) is
    begin
        Table.Quiescent (Quiescent_Reclamation.CPU_Index (cpu));
    end Quiescent;

    procedure Reclaim is
    begin
        Table.Reclaim (cpuOnline);
    end Reclaim;

    ---------------------------------------------------------------------------
    -- Helpers
    ---------------------------------------------------------------------------
    function present (id : Unsigned_64) return Boolean is
      (id in 1 .. NO.Max_Objects and then Table.Present (Table.Id (id)));

    function current (E : Kernel_Notification; gen : Unsigned_32) return Boolean is
      (E.Gen = gen and then E.State.Live);

    -- What a stale handle answers: Revoked while the object lives on (its
    -- generation advanced), else Ended.
    function stale (E : Kernel_Notification) return Unsigned_64 is
      (if E.State.Live then NA.Revoked else NA.Ended);
    function staleOutcome (E : Kernel_Notification) return NA.Outcome is
      (if E.State.Live then NA.Revoked_Object else NA.Ended_Object);

    -- A leaf's root has ended (racy read of its generation: a link names a
    -- generation that changes only when that root ends).
    function rootGone (L : NO.Link) return Boolean is
      (not present (Unsigned_64 (L.Id))
       or else Table.Lookup (Table.Id (L.Id)).Gen /= L.Gen);

    type Resolved is record
        OK    : Boolean := False;
        Id    : NO.Object_Id := NO.No_Object;
        Gen   : Unsigned_32 := 0;
        Badge : Unsigned_64 := 0;
    end record;

    -- The notification handle names in pid's table, if it holds Right.
    function resolve (pid : ProcessID; handle : Unsigned_64; right : Caps.CapabilityRight)
      return Resolved
    is
        need : Caps.CapabilityRights := Caps.NO_RIGHTS;
        found : Process_Capabilities.Found;
    begin
        need (right) := True;
        Spinlocks.enterCriticalSection (mailtab (pid).lock);
        found := Process_Capabilities.Find (proctab (pid).caps, handle, Caps.CAP_NOTIFICATION_OBJECT, need);
        Spinlocks.exitCriticalSection (mailtab (pid).lock);
        if not found.OK or else found.Cap.object.ref not in 1 .. NO.Max_Objects then
            return (others => <>);
        end if;
        return (OK => True, Id => NO.Object_Id (found.Cap.object.ref), Gen => found.Cap.gen,
                Badge => found.Cap.authorityTag);
    end resolve;

    -- Caller holds the object's lock: carry out a signal's action. A bound
    -- thread to ring is returned, rung after the lock is released.
    procedure act (r : NO.Signal_Result; ring : in out ThreadID) is
    begin
        case r.What is
            when NO.Wake_Waiter => Wait_Timers.Finish (ThreadID (r.Thread), Wait_Woken, r.Taken);
            when NO.Ring_Bound  => ring := ThreadID (r.Thread);
            when others         => null;
        end case;
    end act;

    -- The bridge to the old receive (deleted with it, S3c): a signal of a
    -- bound notification with no waiter rings the bound thread's process.
    procedure ringBound (t : ThreadID) is
        pid : ProcessID;
        ignore : Boolean;
    begin
        if t /= NO_THREAD then
            pid := processOf (t);
            if pid /= NO_PROCESS then
                ignore := IPC.ringDoorbell (pid);
            end if;
        end if;
    end ringBound;

    -- Signal object id (generation gen) with badge: the object, then its
    -- root when it is a leaf, each under its own lock, one after the other.
    function signal (id : NO.Object_Id; gen : Unsigned_32; badge : Unsigned_64) return Unsigned_64 is
        r1, r2 : NO.Signal_Result;
        ring1, ring2 : ThreadID := NO_THREAD;
    begin
        if not present (Unsigned_64 (id)) then
            return NA.Ended;
        end if;
        declare
            E : Kernel_Notification renames Table.Lookup (Table.Id (id)).E.all;
        begin
            Spinlocks.enterCriticalSection (E.Lock);
            if not current (E, gen) then
                Spinlocks.exitCriticalSection (E.Lock);
                return stale (E);
            end if;
            NO.Signal (E.State, badge, NO.First, r1);
            act (r1, ring1);
            Spinlocks.exitCriticalSection (E.Lock);
        end;
        if r1.What = NO.Forward and then present (Unsigned_64 (r1.To.Id)) then
            declare
                R : Kernel_Notification renames Table.Lookup (Table.Id (r1.To.Id)).E.all;
            begin
                Spinlocks.enterCriticalSection (R.Lock);
                -- A root that ended: the bits wait in the leaf.
                if current (R, r1.To.Gen) then
                    NO.Signal (R.State, r1.To_Badge, NO.Second, r2);
                    act (r2, ring2);
                end if;
                Spinlocks.exitCriticalSection (R.Lock);
            end;
        end if;
        ringBound (ring1);
        ringBound (ring2);
        return NA.Signalled;
    end signal;

    -- End object id (generation gen): its waiter returns Ended, its bound
    -- thread is unbound, its slot and its owner's charge are given back.
    procedure endObject (id : NO.Object_Id; gen : Unsigned_32) is
        waiter, bound : NO.Thread_Ref;
        owner : ProcessID;
    begin
        if not present (Unsigned_64 (id)) then
            return;
        end if;
        declare
            E : Kernel_Notification renames Table.Lookup (Table.Id (id)).E.all;
        begin
            Spinlocks.enterCriticalSection (E.Lock);
            if not current (E, gen) then
                Spinlocks.exitCriticalSection (E.Lock);
                return;
            end if;
            owner := ProcessID (E.State.Owner);
            NO.Finish (E.State, waiter, bound);
            E.Gen := 0;
            if waiter /= NO.No_Thread then
                Wait_Timers.Finish (ThreadID (waiter), Wait_Ended);
            end if;
            if bound /= NO.No_Thread
              and then threadtab (ThreadID (bound)).boundNotification = Unsigned_32 (id)
              and then threadtab (ThreadID (bound)).boundGen = gen
            then
                threadtab (ThreadID (bound)).boundNotification := 0;
                threadtab (ThreadID (bound)).boundGen := 0;
            end if;
            Spinlocks.exitCriticalSection (E.Lock);
        end;
        Table.Release (Table.Id (id));
        Spinlocks.enterCriticalSection (chargeLock);
        if proctab (owner).notificationsHeld > 0 then
            proctab (owner).notificationsHeld := proctab (owner).notificationsHeld - 1;
        end if;
        Spinlocks.exitCriticalSection (chargeLock);
    end endObject;

    ---------------------------------------------------------------------------
    -- Wait and Wake
    ---------------------------------------------------------------------------
    function wait (handle, deadlineUs : Unsigned_64) return Unsigned_64 is
        me  : constant ThreadID := PerCPUData.getCurrentThread;
        pid : constant ProcessID := processOf (me);
        h : Resolved;
        block : Boolean := deadlineUs /= NA.Now;
        timed : Boolean := block and then deadlineUs /= NA.Forever;
        passed : Boolean := False;
        tsc : Unsigned_64 := 0;
        outcome : NO.Wait_Outcome;
        taken : Unsigned_64;
        result : Unsigned_64 := 0;
        slept : Boolean := False;
    begin
        if pid = NO_PROCESS then
            return NA.Refused;
        end if;
        h := resolve (pid, handle, Caps.RIGHT_READ);
        if not h.OK then
            return NA.Refused;
        elsif not present (Unsigned_64 (h.Id)) then
            return NA.Ended;
        end if;
        if timed then
            Wait_Timers.Deadline_Of (deadlineUs, tsc, passed);
            if passed then
                block := False;
                timed := False;
            end if;
        end if;
        threadtab (me).oneWaitKind := Notification_Wait;
        threadtab (me).oneWaitObject := Unsigned_32 (h.Id);
        threadtab (me).oneWaitGen := h.Gen;
        threadtab (me).oneWaitStatus := Wait_Pending;
        threadtab (me).oneWaitBits := 0;
        if timed then
            Wait_Timers.Arm (me, tsc);
        end if;
        declare
            E : Kernel_Notification renames Table.Lookup (Table.Id (h.Id)).E.all;
        begin
            Spinlocks.enterCriticalSection (E.Lock);
            if not current (E, h.Gen) then
                result := stale (E);
            elsif threadtab (me).oneWaitStatus = Wait_Timed_Out then
                result := 0;
            else
                -- A leaf whose root ended is a leaf no more.
                if NO.Is_Leaf (E.State) and then rootGone (E.State.Root) then
                    NO.Unbind_Leaf (E.State);
                end if;
                NO.Start_Wait (E.State, NO.Thread_Ref (me), block, outcome, taken);
                case outcome is
                    when NO.Got       => result := taken;
                    when NO.Empty_Now => result := 0;
                    when NO.Busy      => result := NA.Busy;
                    when NO.Blocked   =>
                        -- Published before the lock is released: a waker,
                        -- which needs it, always finds this state.
                        threadtab (me).state := FUTEXWAITING;
                        slept := True;
                end case;
            end if;
            Spinlocks.exitCriticalSection (E.Lock);
        end;
        if slept then
            yield;
            result := (case threadtab (me).oneWaitStatus is
                         when Wait_Woken     => threadtab (me).oneWaitBits,
                         when Wait_Ended     => NA.Ended,
                         when Wait_Revoked   => NA.Revoked,
                         when others         => 0);
        end if;
        if timed then
            Wait_Timers.Disarm (me);
        end if;
        threadtab (me).oneWaitKind := No_One_Wait;
        return result;
    end wait;

    function wake (handle : Unsigned_64) return Unsigned_64 is
        pid : constant ProcessID := PerCPUData.getCurrentPID;
        h : Resolved;
    begin
        if pid = NO_PROCESS then
            return NA.Refused;
        end if;
        h := resolve (pid, handle, Caps.RIGHT_WRITE);
        if not h.OK or else h.Badge = 0 or else h.Badge > NA.Badge_Last then
            return NA.Refused;
        end if;
        return signal (h.Id, h.Gen, h.Badge);
    end wake;

    ---------------------------------------------------------------------------
    -- Calls
    ---------------------------------------------------------------------------
    procedure answer (reply : out Message; o : NA.Outcome; w1 : Unsigned_64 := 0) is
    begin
        reply := NULL_MESSAGE;
        reply.tag.label := 0;
        reply.tag.length := 4;
        reply.words := [Unsigned_64 (NA.Outcome'Enum_Rep (o)), w1, 0, 0];
    end answer;

    function outcomeOf (b : NO.Bind_Outcome) return NA.Outcome is
      (case b is
         when NO.Bound_Now   => NA.Ok,
         when NO.Not_Allowed => NA.Not_Allowed,
         when NO.Already     => NA.Already,
         when NO.Busy        => NA.Busy_Object);

    -- Place a handle for object id in caller's table: the handle word, or a
    -- Capability_ABI refusal word.
    function insertHandle (caller : ProcessID; id : NO.Object_Id; gen : Unsigned_32;
                           rights : Caps.CapabilityRights; badge : Unsigned_64) return Unsigned_64
    is
        word : Unsigned_64;
    begin
        Spinlocks.enterCriticalSection (mailtab (caller).lock);
        Process_Capabilities.Insert
          (proctab (caller).caps,
           (capType      => Caps.CAP_NOTIFICATION_OBJECT,
            rights       => rights,
            authorityTag => badge,
            object       => (ref => Unsigned_64 (id), param => 0),
            gen          => gen), word);
        Spinlocks.exitCriticalSection (mailtab (caller).lock);
        return word;
    end insertHandle;

    procedure Create_Call (caller : ProcessID; cap : Capabilities.Capability;
                           request : Message; reply : out Message)
    is
        pragma Unreferenced (request);
        allowed : Boolean := False;
        raw : Table.Id;
        id : NO.Object_Id;
        gen : Unsigned_32;
        word : Unsigned_64;
    begin
        if Unsigned_64 (caller) /= cap.object.ref or else cap.gen /= generationOf (caller)
          or else not cap.rights (Caps.RIGHT_WRITE)
        then
            answer (reply, NA.Refused_Call);
            return;
        end if;
        Spinlocks.enterCriticalSection (chargeLock);
        if proctab (caller).notificationsHeld < NA.Per_Process_Limit then
            proctab (caller).notificationsHeld := proctab (caller).notificationsHeld + 1;
            allowed := True;
        end if;
        Spinlocks.exitCriticalSection (chargeLock);
        if not allowed then
            answer (reply, NA.Limit_Reached);
            return;
        end if;
        Table.Allocate (raw);
        if raw = 0 then
            Spinlocks.enterCriticalSection (chargeLock);
            proctab (caller).notificationsHeld := proctab (caller).notificationsHeld - 1;
            Spinlocks.exitCriticalSection (chargeLock);
            answer (reply, NA.No_Memory);
            return;
        end if;
        id := NO.Object_Id (raw);
        gen := Unsigned_32 (Table.Generation_Of (raw)) + 1;
        declare
            E : Kernel_Notification renames Table.Lookup (raw).E.all;
        begin
            Spinlocks.enterCriticalSection (E.Lock);
            NO.Create (E.State, NO.Owner_Ref (caller));
            E.Gen := gen;
            Spinlocks.exitCriticalSection (E.Lock);
        end;
        -- The Wait handle: Wait and Wake, unbadged, never copied.
        word := insertHandle (caller, id, gen,
                              [Caps.RIGHT_READ | Caps.RIGHT_WRITE | Caps.RIGHT_REVOKE => True,
                               others => False], 0);
        if not Capability_ABI.Is_Handle_Word (word) then
            endObject (id, gen);
            answer (reply, NA.Limit_Reached);
            return;
        end if;
        answer (reply, NA.Ok, word);
    end Create_Call;

    procedure bindLeaf (caller : ProcessID; leaf : Resolved; rootHandle : Unsigned_64;
                        reply : out Message)
    is
        root : constant Resolved := resolve (caller, rootHandle, Caps.RIGHT_WRITE);
        outcome : NO.Bind_Outcome := NO.Not_Allowed;
        told : NO.Signal_Result;
        ring : ThreadID := NO_THREAD;
        live : Boolean;
    begin
        if not root.OK or else root.Badge = 0 or else root.Badge > NA.Badge_Last then
            answer (reply, NA.Refused_Call);
            return;
        elsif root.Id = leaf.Id then
            answer (reply, NA.Not_Allowed);
            return;
        elsif not present (Unsigned_64 (root.Id)) or else not present (Unsigned_64 (leaf.Id)) then
            answer (reply, NA.Ended_Object);
            return;
        end if;
        declare
            L : Kernel_Notification renames Table.Lookup (Table.Id (leaf.Id)).E.all;
            R : Kernel_Notification renames Table.Lookup (Table.Id (root.Id)).E.all;
        begin
            -- The one place two notification locks are held: ascending slot.
            if leaf.Id < root.Id then
                Spinlocks.enterCriticalSection (L.Lock);
                Spinlocks.enterCriticalSection (R.Lock);
            else
                Spinlocks.enterCriticalSection (R.Lock);
                Spinlocks.enterCriticalSection (L.Lock);
            end if;
            live := current (L, leaf.Gen) and then current (R, root.Gen);
            if live then
                NO.Bind_Leaf (L.State, R.State, leaf.Id, (Id => root.Id, Gen => root.Gen),
                              root.Badge, outcome, told);
                act (told, ring);
            end if;
            Spinlocks.exitCriticalSection (L.Lock);
            Spinlocks.exitCriticalSection (R.Lock);
        end;
        if not live then
            answer (reply, NA.Ended_Object);
            return;
        end if;
        ringBound (ring);
        answer (reply, outcomeOf (outcome));
    end bindLeaf;

    -- Revoke: the object's generation advances (the table's ledger, so it
    -- never goes back, not even when the slot is reused); the waiter, which
    -- holds the old Wait right, wakes Revoked; the bound thread's binding is
    -- re-stamped; the revoker's handle is replaced by a re-stamped one.
    procedure revoke (caller : ProcessID; handle : Unsigned_64; cap : Capabilities.Capability;
                      reply : out Message)
    is
        id : constant NO.Object_Id := NO.Object_Id (cap.object.ref);
        E : Kernel_Notification renames Table.Lookup (Table.Id (id)).E.all;
        waiter : NO.Thread_Ref;
        saturated : Boolean := False;
        outcome : NA.Outcome := NA.Ok;
        gen : Unsigned_32 := 0;
        removed : Capabilities.Capability;
        closed, word : Unsigned_64;
    begin
        Spinlocks.enterCriticalSection (E.Lock);
        if not current (E, cap.gen) then
            outcome := staleOutcome (E);
        else
            Table.Invalidate (Table.Id (id), saturated);
            if saturated then
                outcome := NA.Limit_Reached;
            else
                gen := Unsigned_32 (Table.Generation_Of (Table.Id (id))) + 1;
                E.Gen := gen;
                NO.Revoke (E.State, waiter);
                if waiter /= NO.No_Thread then
                    Wait_Timers.Finish (ThreadID (waiter), Wait_Revoked);
                end if;
                if E.State.Bound /= NO.No_Thread
                  and then threadtab (ThreadID (E.State.Bound)).boundNotification = Unsigned_32 (id)
                then
                    threadtab (ThreadID (E.State.Bound)).boundGen := gen;
                end if;
            end if;
        end if;
        Spinlocks.exitCriticalSection (E.Lock);
        if outcome /= NA.Ok then
            answer (reply, outcome);
            return;
        end if;
        Spinlocks.enterCriticalSection (mailtab (caller).lock);
        Process_Capabilities.Close (proctab (caller).caps, handle, removed, closed);
        Spinlocks.exitCriticalSection (mailtab (caller).lock);
        word := insertHandle (caller, id, gen, cap.rights, cap.authorityTag);
        answer (reply, NA.Ok, word);
    end revoke;

    procedure Call (caller : ProcessID; handle : Unsigned_64; cap : Capabilities.Capability;
                    request : Message; reply : out Message)
    is
        label : constant Unsigned_64 := Unsigned_64 (request.tag.label);
        me : constant ThreadID := PerCPUData.getCurrentThread;
        id : NO.Object_Id;
        h : Resolved;
    begin
        if cap.capType /= Caps.CAP_NOTIFICATION_OBJECT or else cap.object.ref not in 1 .. NO.Max_Objects
          or else not cap.rights (Caps.RIGHT_READ) or else label = NA.Create_Label
          or else not NA.Is_Label (label)
        then
            answer (reply, NA.Refused_Call);
            return;
        end if;
        id := NO.Object_Id (cap.object.ref);
        h := (OK => True, Id => id, Gen => cap.gen, Badge => cap.authorityTag);
        if not present (cap.object.ref) then
            answer (reply, NA.Ended_Object);
            return;
        end if;
        if label = NA.Revoke_Label then
            if cap.rights (Caps.RIGHT_REVOKE) then
                revoke (caller, handle, cap, reply);
            else
                answer (reply, NA.Refused_Call);
            end if;
        elsif label = NA.Mint_Wake_Label then
            declare
                badge : constant Unsigned_64 := request.words (0);
                grant : constant Unsigned_64 := request.words (1);
                live : Boolean;
                word : Unsigned_64;
                why : NA.Outcome;
                E : Kernel_Notification renames Table.Lookup (Table.Id (id)).E.all;
            begin
                if badge = 0 or else badge > NA.Badge_Last or else grant > 1 then
                    answer (reply, NA.Not_Allowed);
                    return;
                end if;
                Spinlocks.enterCriticalSection (E.Lock);
                live := current (E, cap.gen);
                why := staleOutcome (E);
                Spinlocks.exitCriticalSection (E.Lock);
                if not live then
                    answer (reply, why);
                    return;
                end if;
                word := insertHandle
                  (caller, id, cap.gen,
                   [Caps.RIGHT_WRITE => True, Caps.RIGHT_GRANT => grant = 1, others => False],
                   badge);
                if Capability_ABI.Is_Handle_Word (word) then
                    answer (reply, NA.Ok, word);
                else
                    answer (reply, NA.Limit_Reached);
                end if;
            end;
        elsif label = NA.Bind_Leaf_Label then
            bindLeaf (caller, h, request.words (0), reply);
        else
            declare
                E : Kernel_Notification renames Table.Lookup (Table.Id (id)).E.all;
                outcome : NO.Bind_Outcome := NO.Bound_Now;
                told : NO.Signal_Result;
                removed : Boolean := False;
                ring : ThreadID := NO_THREAD;
                ended : Boolean := False;
                why : NA.Outcome := NA.Ended_Object;
            begin
                Spinlocks.enterCriticalSection (E.Lock);
                if not current (E, cap.gen) then
                    ended := True;
                    why := staleOutcome (E);
                elsif label = NA.Unbind_Leaf_Label then
                    NO.Unbind_Leaf (E.State);
                elsif label = NA.Bind_Thread_Label then
                    if threadtab (me).boundNotification /= 0 then
                        outcome := NO.Already;
                    else
                        NO.Bind_Thread (E.State, NO.Thread_Ref (me), outcome, told);
                        if outcome = NO.Bound_Now then
                            threadtab (me).boundNotification := Unsigned_32 (id);
                            threadtab (me).boundGen := cap.gen;
                        end if;
                        if told.What = NO.Ring_Bound then
                            ring := me;
                        end if;
                    end if;
                else
                    NO.Unbind_Thread (E.State, NO.Thread_Ref (me), removed);
                    if removed then
                        threadtab (me).boundNotification := 0;
                        threadtab (me).boundGen := 0;
                    else
                        outcome := NO.Not_Allowed;
                    end if;
                end if;
                Spinlocks.exitCriticalSection (E.Lock);
                if ended then
                    answer (reply, why);
                else
                    ringBound (ring);
                    answer (reply, outcomeOf (outcome));
                end if;
            end;
        end if;
    end Call;

    procedure Closed (cap : Capabilities.Capability) is
    begin
        if cap.capType = Caps.CAP_NOTIFICATION_OBJECT and then cap.rights (Caps.RIGHT_READ)
          and then cap.object.ref in 1 .. NO.Max_Objects
        then
            endObject (NO.Object_Id (cap.object.ref), cap.gen);
        end if;
    end Closed;

    procedure Release_Held (pid : ProcessID) is
        package Tables renames Process_Capabilities.Tables;
        use type Tables.Slot_Count;
        type Pending is record
            Id  : NO.Object_Id := NO.No_Object;
            Gen : Unsigned_32 := 0;
        end record;
        Chunk : constant := 16;
        found : array (1 .. Chunk) of Pending;
        count : Natural;
        cursor : Tables.Slot_Count := 0;
        high : Tables.Slot_Count;
        item : Caps.Capability;
    begin
        if pid = NO_PROCESS then
            return;
        end if;
        loop
            -- Collect a chunk under the table lock; end them without it
            -- (an object lock is never taken under a mailbox lock).
            count := 0;
            Spinlocks.enterCriticalSection (mailtab (pid).lock);
            high := Tables.High_Water (proctab (pid).caps);
            while cursor < high and then count < Chunk loop
                if Tables.Is_Live_At (proctab (pid).caps, cursor) then
                    item := Tables.Item_At (proctab (pid).caps, cursor);
                    if item.capType = Caps.CAP_NOTIFICATION_OBJECT
                      and then item.rights (Caps.RIGHT_READ)
                      and then item.object.ref in 1 .. NO.Max_Objects
                    then
                        count := count + 1;
                        found (count) := (Id => NO.Object_Id (item.object.ref), Gen => item.gen);
                    end if;
                end if;
                cursor := cursor + 1;
            end loop;
            Spinlocks.exitCriticalSection (mailtab (pid).lock);
            for i in 1 .. count loop
                endObject (found (i).Id, found (i).Gen);
            end loop;
            exit when cursor >= high;
        end loop;
    end Release_Held;

    -- t leaves the object it waits on: timed out (its heap lock held; t is
    -- readied if it blocked, or sees the timeout under the object's lock
    -- before it blocks), or reaped (cancel).
    procedure leave (t : ThreadID; cancel : Boolean) is
        id : constant Unsigned_64 := Unsigned_64 (threadtab (t).oneWaitObject);
        removed : Boolean;
    begin
        if present (id) then
            declare
                E : Kernel_Notification renames Table.Lookup (Table.Id (id)).E.all;
            begin
                Spinlocks.enterCriticalSection (E.Lock);
                if threadtab (t).oneWaitStatus = Wait_Pending
                  and then current (E, threadtab (t).oneWaitGen)
                then
                    NO.Cancel_Wait (E.State, NO.Thread_Ref (t), removed);
                    if cancel then
                        threadtab (t).oneWaitStatus := Wait_Ended;
                    else
                        Wait_Timers.Finish (t, Wait_Timed_Out);
                    end if;
                end if;
                Spinlocks.exitCriticalSection (E.Lock);
            end;
        end if;
    end leave;

    procedure Time_Out (t : ThreadID) is
    begin
        leave (t, cancel => False);
    end Time_Out;

    procedure Cancel (t : ThreadID) is
    begin
        leave (t, cancel => True);
        threadtab (t).oneWaitKind := No_One_Wait;
    end Cancel;

    procedure Unbind_Thread_Of (t : ThreadID) is
        id : constant Unsigned_64 := Unsigned_64 (threadtab (t).boundNotification);
        removed : Boolean;
    begin
        if id /= 0 and then present (id) then
            declare
                E : Kernel_Notification renames Table.Lookup (Table.Id (id)).E.all;
            begin
                Spinlocks.enterCriticalSection (E.Lock);
                if E.Gen = threadtab (t).boundGen then
                    NO.Unbind_Thread (E.State, NO.Thread_Ref (t), removed);
                end if;
                Spinlocks.exitCriticalSection (E.Lock);
            end;
        end if;
        threadtab (t).boundNotification := 0;
        threadtab (t).boundGen := 0;
    end Unbind_Thread_Of;

end Process.Notifications;
