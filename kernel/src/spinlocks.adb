with System.Machine_Code; use System.Machine_Code;
with PerCPUData;
with TLB_Shootdown;
with Trace;
with x86;

-- Only this adapter touches concurrent memory and CPU exclusion. Locks owns
-- the proved sequential policy; compare/exchange commits a proposed transition.
package body Spinlocks with SPARK_Mode => Off is
    use type Locks.State;
    use type Locks.Acquire_Result;
    use type Interfaces.Unsigned_64;

    procedure Initialize (S : out Spinlock; Name : Lock_Name := null) is
    begin
        S := (Name => Name, others => <>);
    end Initialize;

    function isLocked (S : Spinlock) return Boolean is
      (Locks.Is_Locked (S.Owner));

    function ownedBy (S : Spinlock; CPU : Locks.CPU_ID) return Boolean is
      (Locks.Owned_By (S.Owner, CPU));

    function Compare_Exchange
      (S : in out Spinlock; Expected, Desired : Locks.State) return Boolean
    is
        Observed : Locks.State := Expected;
    begin
        -- Operate directly on the atomic record component, never on a scalar
        -- in-out parameter that could be copied in/out around an asm wrapper.
        -- Locked cmpxchg + memory clobber supply hardware/compiler ordering.
        Asm ("lock; cmpxchgl %2, %0",
             Outputs => (Locks.State'Asm_Output ("+m", S.Owner),
                         Locks.State'Asm_Output ("+a", Observed)),
             Inputs => Locks.State'Asm_Input ("r", Desired),
             Clobber => "cc,memory", Volatile => True);
        return Observed = Expected;
    end Compare_Exchange;

    procedure enterCriticalSection (S : in out Spinlock) is
        CPU : Locks.CPU_ID;
        Before, After : Locks.State;
        Result : Locks.Acquire_Result;
        Wait_Started : Interfaces.Unsigned_64 := 0;
        Measuring : Boolean;
    begin
        PerCPUData.pushCLI;
        CPU := PerCPUData.getCPUNumber;
        Measuring := Trace.IsEnabled;
        if Measuring then
            Wait_Started := x86.rdtsc;
        end if;
        loop
            Before := S.Owner;
            After := Before;
            Locks.Acquire (After, CPU, Result);
            case Result is
                when Locks.Acquired =>
                    exit when Compare_Exchange (S, Before, After);
                when Locks.Reentrant =>
                    -- Fatal kernel misuse, not a recoverable lock failure.
                    -- Do not call TextIO or allocate while reporting it.
                    raise SpinLockException with "Recursive CPU spinlock acquisition";
                when Locks.Contended =>
                    null;
            end case;
            -- Read-spin while held; issue another locked RMW only when free.
            -- IF is clear, so service shootdowns even without a maskable IPI.
            TLB_Shootdown.Service (CPU);
            Asm ("pause", Volatile => True);
        end loop;
        S.Measure_Hold := Measuring;
        if Measuring then
            S.Hold_Started := x86.rdtsc;
            Trace.ObserveDuration (Trace.EVENT_LOCK_WAIT,
                                   S.Hold_Started - Wait_Started);
        end if;
    end enterCriticalSection;

    procedure exitCriticalSection (S : in out Spinlock) is
        Before : constant Locks.State := S.Owner;
        After : Locks.State := Before;
        Success : Boolean;
    begin
        Locks.Release (After, PerCPUData.getCPUNumber, Success);
        if not Success then
            raise SpinLockException with "Spinlock release without CPU ownership";
        end if;
        if S.Measure_Hold then
            Trace.ObserveDuration (Trace.EVENT_LOCK_HOLD,
                                   x86.rdtsc - S.Hold_Started);
            S.Measure_Hold := False;
        end if;
        if not Compare_Exchange (S, Before, After) then
            raise SpinLockException with "Spinlock ownership changed during release";
        end if;
        PerCPUData.popCLI;
    end exitCriticalSection;
end Spinlocks;
