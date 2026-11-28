pragma Ada_2022;
with Capabilities;
with Process_Capabilities;
with Platform_Power;
with Firmware_Snapshot_Export;
with Trace_Object;
with Boot_Timeline_Marks;
with Process_List_Object;
with Test_Console_Object;
with Debug_Sessions;
with VM_Objects;
with Spinlocks;
with Notification_ABI;
with Process.Notifications;

package body Kernel_Object_Calls is
    use type Capabilities.CapabilityType;

    procedure Call (Caller  : Process.ProcessID;
                    Handle  : Unsigned_64;
                    Request : Process.Message;
                    Reply   : out Process.Message;
                    Result  : out Outcome)
    is
        Found : Process_Capabilities.Found;
    begin
        Reply := Process.NULL_MESSAGE;
        Result := Not_A_Kernel_Object;
        -- A snapshot of the entry, under the caller's mailbox lock (as every
        -- handle lookup): kind, generation, rights.
        Spinlocks.enterCriticalSection (Process.tableLock (Caller));
        Found := Process_Capabilities.Find_Any (Process.proctab (Caller).caps, Handle);
        Spinlocks.exitCriticalSection (Process.tableLock (Caller));
        if not Found.OK then
            return;
        end if;
        case Found.Cap.capType is
            when Capabilities.CAP_PLATFORM_POWER =>
                Platform_Power.Call
                  (Writable => Found.Cap.rights (Capabilities.RIGHT_WRITE),
                   Request  => Request,
                   Reply    => Reply);
                Result := Answered;
            when Capabilities.CAP_FIRMWARE_SNAPSHOT =>
                Firmware_Snapshot_Export.Call
                  (Caller   => Caller,
                   Readable => Found.Cap.rights (Capabilities.RIGHT_READ),
                   Request  => Request,
                   Reply    => Reply);
                Result := Answered;
            -- A notification handle answers one kernel operation: marking
            -- a boot milestone its role owns (Boot_Timeline_Marks).
            when Capabilities.CAP_NOTIFICATION =>
                if Boot_Timeline_Marks.Is_Mark (Request) then
                    Boot_Timeline_Marks.Call (Caller, Found.Cap, Request, Reply);
                    Result := Answered;
                end if;
            when Capabilities.CAP_PROCESS_LIST =>
                Process_List_Object.Call
                  (Caller   => Caller,
                   Readable => Found.Cap.rights (Capabilities.RIGHT_READ),
                   Request  => Request,
                   Reply    => Reply);
                Result := Answered;
            when Capabilities.CAP_TRACE =>
                Trace_Object.Call
                  (Writable => Found.Cap.rights (Capabilities.RIGHT_WRITE),
                   Request  => Request,
                   Reply    => Reply);
                Result := Answered;
            -- A target's Control mints and revokes debug sessions
            -- (DBG-001); any other label on it is not this path's.
            when Capabilities.CAP_PROCESS =>
                -- A process's own handle creates notifications charged to
                -- it (Notification_ABI.Create).
                if Unsigned_64 (Request.tag.label) = Notification_ABI.Create_Label then
                    Process.Notifications.Create_Call (Caller, Found.Cap, Request, Reply);
                    Result := Answered;
                    return;
                end if;
                declare
                    Ours : Boolean;
                begin
                    Debug_Sessions.Control_Call (Caller, Found.Cap, Request, Reply, Ours);
                    if Ours then
                        Result := Answered;
                    end if;
                end;
            when Capabilities.CAP_DEBUG_OBSERVE | Capabilities.CAP_DEBUG_PAUSE |
                 Capabilities.CAP_DEBUG_MODIFY | Capabilities.CAP_DEBUG_INSTRUMENT =>
                Debug_Sessions.Call (Caller, Found.Cap, Request, Reply);
                Result := Answered;
            -- Virtual machines (VMM-001, VM_ABI): the creation authority,
            -- a VM and a vCPU.
            when Capabilities.CAP_VM_CREATE | Capabilities.CAP_VM |
                 Capabilities.CAP_VCPU =>
                VM_Objects.Call (Caller, Found.Cap, Request, Reply);
                Result := Answered;
            when Capabilities.CAP_NOTIFICATION_OBJECT =>
                Process.Notifications.Call (Caller, Handle, Found.Cap, Request, Reply);
                Result := Answered;
            when Capabilities.CAP_TEST_CONSOLE =>
                Test_Console_Object.Call
                  (Caller   => Caller,
                   Writable => Found.Cap.rights (Capabilities.RIGHT_WRITE),
                   Request  => Request,
                   Reply    => Reply);
                Result := Answered;
            when others =>
                null;
        end case;
    end Call;
end Kernel_Object_Calls;
