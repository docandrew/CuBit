-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary Boot report of the CPU's hardware virtualization (VMM-001 slice 0).
--
-- @description Reads the VMX capability MSRs (through Msr_Safe, so a MSR the
-- CPU refuses reads as absent instead of faulting) or the SVM CPUID leaves,
-- decodes them with Virtualization_Features, and prints a few "virt:" lines:
-- the verdict against docs/hypervisor.md's requirements, the features the
-- design uses or may use, and the raw values for the record. It enables
-- nothing: no VMXON, no CR4.VMXE, no EFER.SVME, no IA32_FEATURE_CONTROL
-- write.
-------------------------------------------------------------------------------
with Interfaces;
with Virtualization_Features;

package Virtualization_Report with SPARK_Mode => Off is
    -- BSP, after cpuid.setupCPUID; interrupts may be on or off.
    procedure Report;

    -- What Report found (VMM-001 slice 1 runs guests only where SVM is
    -- usable; the VMX back end comes with the laptop slice).
    function SVM_Usable return Boolean;
    function SVM_ASIDs return Interfaces.Unsigned_32;
    -- V_SPEC_CTRL: VMRUN and #VMEXIT switch the guest's SPEC_CTRL.
    function SVM_Virtual_Spec_Ctrl return Boolean;
    function VMX_Usable return Boolean;
    -- The capability MSRs as read at boot (TRUE controls when offered).
    function VMX_Values return Virtualization_Features.VMX_Raw;
end Virtualization_Report;
