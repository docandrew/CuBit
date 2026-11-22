-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- Virtual machines as kernel objects (VMM-001 slice 1, docs/hypervisor.md
-- section 4): the VM creation authority, the VM and the vCPU, reached by
-- Call on their handles (VM_ABI). Mechanism only: AMD SVM entry and exit,
-- nested page tables built from frames the monitor pins, the exits the
-- kernel keeps (4.3: host interrupts and NMIs, the interrupt window, HLT),
-- event injection, and teardown. Every other exit returns to the monitor
-- as Run's reply.
--
-- @description
-- Guest RAM in this slice. The monitor commits memory of its own and names
-- it in Add_RAM; the kernel walks the monitor's tables, pins each frame
-- the monitor owns (BuddyAllocator.pinOwnedFrame, as debug views do) and
-- maps it into the VM's nested tables. So a frame the VM maps is always
-- the monitor's own, pinned for as long as the nested tables name it: the
-- monitor freeing or unmapping it only defers the free (Frame_Pins). Each
-- VM has exactly one monitor, for life, so "owned by the monitor and named
-- in this VM's ledger" is the model's Guest (v) (tests/vm-ownership).
-- The kernel never maps a guest frame itself (KERN-014). Add_RAM is
-- refused once a vCPU has run (no nested-table change under a running
-- guest in this slice).
--
-- Tags. One ASID per vCPU; its TLB entries are flushed (TLB_CONTROL) before
-- its first entry, after a load on a CPU other than its last, and after
-- Set_State changes CR0, CR3, CR4 or EFER (docs/hypervisor.md 4.7a).
-- A destroyed vCPU's ASID is reused only by a vCPU that flushes it first.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Capabilities;
with Process;

package VM_Objects with SPARK_Mode => Off is

    -- Each CPU, after its paging and per-CPU data exist: enable SVM when
    -- the boot report found it usable (Virtualization_Report).
    procedure Setup_This_CPU;

    -- Whether this kernel runs guests (SVM enabled on the boot CPU).
    function Available return Boolean;

    -- Kernel_Object_Calls: a Call on a CAP_VM_CREATE, CAP_VM or CAP_VCPU.
    procedure Call (Caller  : Process.ProcessID;
                    Cap     : Capabilities.Capability;
                    Request : Process.Message;
                    Reply   : out Process.Message);

    -- The reaper: a monitor ended; destroy its VMs (while its page tables
    -- still exist, so the pins are released before its frames are freed).
    procedure Process_Ended (Pid : Process.ProcessID);

    -- Timer tick (BSP): wake vCPUs whose halt reached its deadline.
    procedure Expire_Halts (Now_Ms : Unsigned_64);
end VM_Objects;
