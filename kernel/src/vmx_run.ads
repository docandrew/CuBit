-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The Intel VMX back end of VM_Objects (VMM-001 slice 1, the N150's).
--
-- @description
-- VM_Objects keeps every vCPU's guest state in one state page laid out as
-- an SVM VMCB (SVM_Layout), whichever back end runs it, so admission, state
-- access and injection are one code path. On VMX the state page is a
-- software copy: Enter writes it into the vCPU's VMCS, runs the guest once
-- and reads it back, translating the exit into the SVM codes and
-- information words the state page carries (exit reason to exit code, the
-- I/O and EPT-violation qualifications to SVM's EXITINFO forms, IDT
-- vectoring to EXITINTINFO, VM-entry interruption from EVENTINJ). The VMCS
-- is VMCLEARed after every exit, so the thread may move to another CPU
-- before its next entry (no VMCS is ever active on two CPUs: vCPU_Bound).
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;

package VMX_Run with SPARK_Mode => Off is

    -- This CPU: IA32_FEATURE_CONTROL, CR0/CR4 fixed bits, CR4.VMXE and
    -- VMXON on a region of its own. OK False leaves VMX off here.
    procedure Enable_This_CPU (CPU : Natural; OK : out Boolean);

    -- The shared I/O and MSR bitmaps (every port and MSR exits but the
    -- MSRs the VMCS or its MSR lists switch, SPEC_CTRL reads and PRED_CMD
    -- writes). Two MSR bitmaps: one where SPEC_CTRL writes exit, one where
    -- they reach the MSR (Guest_Speculation).
    procedure Prepare_Maps (OK : out Boolean);

    -- A fresh VMCS page: the revision identifier, launch state clear.
    procedure Prepare_VMCS (VMCS : System.Address);

    -- Run the guest once on this CPU (interrupts disabled throughout).
    procedure Enter
      (State       : System.Address;    -- the SVM-layout state page
       VMCS        : System.Address;    -- this vCPU's VMCS region
       Lists       : System.Address;    -- this vCPU's MSR-list page
       GPRs        : System.Address;    -- rbx .. r15 (VM_Objects' order)
       EPT_Root    : Unsigned_64;
       VPID        : Unsigned_64;
       Flush_Tag   : Boolean;
       Spec_Ctrl   : System.Address;    -- the vCPU's SPEC_CTRL block (VM_Objects)
       Pass_Spec_Ctrl_Writes : Boolean);
end VMX_Run;
