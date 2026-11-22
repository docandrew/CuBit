with TLB_Reclamation;

-- Trusted x86/SMP adapter for TLB_Reclamation. No CPU hot-unplug support.
-- Each round drops every address-space tag's translations on every online
-- CPU (TLB_Context.Flush_All), not only the tag a CPU runs.
package TLB_Shootdown is
    procedure Register_CPU (CPU : TLB_Reclamation.CPU_Index);
    -- This CPU takes part in rounds: Invalidate_All may be called here.
    function This_CPU_Online return Boolean;
    -- Lock-free, allocation-free, called with maskable interrupts disabled.
    procedure Service (CPU : TLB_Reclamation.CPU_Index);
    -- Called after removing mappings, before releasing their lifetime pins.
    -- Covers all online CPUs, including other threads sharing an address space.
    -- Returns only after every target flushed; timeout is fatal, never success.
    procedure Invalidate_All;
end TLB_Shootdown;
