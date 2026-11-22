with Interfaces; use Interfaces;
with System.Machine_Code; use System.Machine_Code;
with IPI;
with PerCPUData;
with Spinlocks;
with Time;
with TLB_Context;
with x86;

package body TLB_Shootdown is
    use TLB_Reclamation;
    type Epoch_Array is array (CPU_Index) of Epoch with Atomic_Components;
    type Online_Array is array (CPU_Index) of Boolean with Atomic_Components;
    Requested : Epoch_Array := (others => 0);
    Completed : Epoch_Array := (others => 0);
    Online : Online_Array := (others => False);
    Round : State := Initial_State;
    Round_Lock : Spinlocks.Spinlock;
    Shootdown_Failure : exception;

    procedure Register_CPU (CPU : CPU_Index) is
    begin
        -- Registration occurs once during boot, before this CPU runs tasks.
        TLB_Context.Flush_All;
        Online (CPU) := True;
    end Register_CPU;

    function This_CPU_Online return Boolean is
      (Online (PerCPUData.getCPUNumber));

    procedure Service (CPU : CPU_Index) is
        Required : constant Epoch := Requested (CPU);
    begin
        if Completed (CPU) /= Required then
            -- Memory clobber on the flush keeps the acknowledgement after
            -- it. Every tag: the unmapped address space need not be the one
            -- this CPU runs, yet its cached translations may survive here.
            TLB_Context.Flush_All;
            Completed (CPU) := Required;
        end if;
    end Service;

    procedure Invalidate_All is
        Targets : CPU_Set;
        Seen : Acknowledgments;
        OK : Boolean;
        Self : CPU_Index;
        Started : Unsigned_64;
        Timeout_Ticks : Unsigned_64;
    begin
        -- The lock also orders all preceding PTE writes before request
        -- publication. Spinlock waiters service requests without taking locks.
        Spinlocks.enterCriticalSection (Round_Lock);
        Self := PerCPUData.getCPUNumber;
        for CPU in CPU_Index loop
            Targets (CPU) := Online (CPU);
        end loop;
        if not Targets (Self) then
            raise Shootdown_Failure with "TLB requester is not online";
        end if;
        Begin_Round (Round, Targets, OK);
        if not OK then
            raise Shootdown_Failure with "TLB epoch exhausted or round still active";
        end if;
        for CPU in CPU_Index loop
            if Targets (CPU) then
                Requested (CPU) := Ticket (Round);
            end if;
        end loop;
        Service (Self);
        -- Shorthand broadcast does not assume APIC IDs equal logical CPU IDs.
        IPI.broadcastReschedule;
        Started := x86.rdtsc;
        Timeout_Ticks := Time.tscPerDuration * (5 * Time.Seconds);
        loop
            for CPU in CPU_Index loop
                Seen (CPU) := Completed (CPU);
            end loop;
            Observe (Round, Seen);
            Take_Completion (Round, OK);
            exit when OK;
            if x86.rdtsc - Started > Timeout_Ticks then
                -- Do not release pins, reuse slots, or free backing storage.
                raise Shootdown_Failure with "TLB acknowledgment timeout; reclamation refused";
            end if;
            Service (Self);
            Asm ("pause", Volatile => True);
        end loop;
        Spinlocks.exitCriticalSection (Round_Lock);
    end Invalidate_All;
end TLB_Shootdown;
