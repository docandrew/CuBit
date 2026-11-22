with Build;
with cpuid;
with Msr_Safe;
with Publication_Pages;
with TextIO; use TextIO;

package body User_Wait is

    -- Every CPU set so far accepted the control MSR.
    Accepted : Boolean := False with Atomic;

    procedure Setup_This_CPU (Boot_CPU : Boolean) is
    begin
        if Boot_CPU then
            -- The recovering MSR path itself, checked on every boot.
            if Msr_Safe.Absent_Register_Recovered then
                println ("msr-safe: an absent MSR faulted and was recovered");
            else
                println ("msr-safe: an absent MSR read without a fault");
            end if;
            Accepted := Build.User_Wait_Pause and then cpuid.leaf7ecx.hasWAITPKG;
            if Build.User_Wait_Pause and then not cpuid.leaf7ecx.hasWAITPKG then
                println ("user-wait: WAITPKG absent: no futex pause");
            end if;
        end if;
        if not Accepted then
            return;
        end if;
        if not Msr_Safe.Write (IA32_UMWAIT_CONTROL, Control_Value) then
            Accepted := False;
            -- Already published by the boot CPU: withdraw it.
            Publication_Pages.Set_User_Wait_Cycles (0);
            println ("user-wait: a CPU refused IA32_UMWAIT_CONTROL: no futex pause");
        end if;
    end Setup_This_CPU;

    function Published_Cycles return Unsigned_64 is
      (if Accepted then Maximum_Cycles else 0);

end User_Wait;
