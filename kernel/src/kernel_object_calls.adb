pragma Ada_2022;
with Capabilities;
with Process_Capabilities;
with Platform_Power;
with Spinlocks;

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
        Spinlocks.enterCriticalSection (Process.mailtab (Caller).lock);
        Found := Process_Capabilities.Find_Any (Process.proctab (Caller).caps, Handle);
        Spinlocks.exitCriticalSection (Process.mailtab (Caller).lock);
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
            when others =>
                null;
        end case;
    end Call;
end Kernel_Object_Calls;
