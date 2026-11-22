-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- A guest's own IA32_SPEC_CTRL (VMM-001, docs/hypervisor.md 4.3 and 9.2):
-- which bits a guest may set, what a guest write of the MSR does, and how
-- each entry and exit switches the MSR.
--
-- @description
-- The kernel switches SPEC_CTRL per vCPU: the guest's value is loaded at
-- entry and saved at exit, and the host's value is back before the host
-- executes any return or indirect branch (svm_run.asm, vmx_entry.asm). On
-- an SVM CPU with V_SPEC_CTRL (CPUID 8000_000Ah EDX bit 20) VMRUN and
-- #VMEXIT do that from the VMCB, so the kernel does nothing at all.
--
-- As Linux KVM does, a guest's writes exit until its first non-zero one:
-- until then the kernel knows the guest's value without reading the MSR
-- at exit, and a guest that never uses SPEC_CTRL costs two compares per
-- entry. The kernel decides each of those writes itself (Guest_Write): a
-- bit the host CPU does not report is #GP in the guest. Once the guest has
-- written a non-zero value its writes go straight to the MSR, where the
-- CPU itself refuses any bit it lacks; the exit then reads the MSR back.
--
-- Sources for every bit (named below, never a bare number): Intel SDM
-- Vol. 4, Table 2-2 (IA32_SPEC_CTRL, IA32_PRED_CMD) and Vol. 2A, CPUID
-- leaf 7; AMD APM Vol. 3, Appendix E (CPUID 8000_0008h EBX, 8000_0021h
-- EAX), AMD's "Speculative Store Bypass Disable" and "Automatic IBRS"
-- documents; cross-checked against Linux's asm/msr-index.h and
-- asm/cpufeatures.h (words 7, 13, 18, 20 and 21).
--
-- Proved (gnatprove level 2, tests/vm-units):
--  * Writable_Bits holds bit N exactly when the host CPU reports the
--    feature behind SPEC_CTRL bit N, and no bit SPEC_CTRL does not define;
--  * Guest_Write accepts a value only when the host has the MSR and every
--    bit of the value is writable; it passes the guest's writes through
--    exactly when it accepts a non-zero value;
--  * Switch_Mode leaves the MSR alone exactly when the host lacks it or
--    the CPU switches it (V_SPEC_CTRL), and reads it back at exit whenever
--    the guest may have written it without an exit;
--  * Guest_EFER_Bits adds AutoIBRS only on SVM, only when the host CPU
--    reports it.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Guest_Speculation with
    SPARK_Mode => On,
    Pure
is
    -- MSR numbers (Intel SDM Vol. 4, Table 2-2).
    IA32_SPEC_CTRL : constant := 16#0000_0048#;
    IA32_PRED_CMD  : constant := 16#0000_0049#;   -- write-only commands

    subtype Bit_Number is Natural range 0 .. 63;
    subtype CPUID_Bit is Natural range 0 .. 31;

    function Bit (Value : Unsigned_64; N : Bit_Number) return Boolean is
      ((Value and Shift_Left (1, N)) /= 0);
    function Bit (Value : Unsigned_32; N : CPUID_Bit) return Boolean is
      ((Value and Shift_Left (1, N)) /= 0);

    -- A SPEC_CTRL value, and the bits a guest may set in one.
    subtype Control_Value is Unsigned_64;
    subtype Control_Mask is Unsigned_64;

    -- IA32_SPEC_CTRL bits (msr-index.h SPEC_CTRL_*).
    Control_IBRS        : constant Bit_Number := 0;
    Control_STIBP       : constant Bit_Number := 1;
    Control_SSBD        : constant Bit_Number := 2;
    Control_IPRED_DIS_U : constant Bit_Number := 3;
    Control_IPRED_DIS_S : constant Bit_Number := 4;
    Control_RRSBA_DIS_U : constant Bit_Number := 5;
    Control_RRSBA_DIS_S : constant Bit_Number := 6;
    Control_PSFD        : constant Bit_Number := 7;
    Control_DDPD_U      : constant Bit_Number := 8;
    Control_BHI_DIS_S   : constant Bit_Number := 10;

    -- CPUID leaves.
    Structured_Leaf       : constant := 7;             -- subleaves 0 and 2
    Structured_Subleaf_2  : constant := 2;
    Extended_Identifiers_Leaf : constant := 16#8000_0008#;
    Extended_Features_2_Leaf  : constant := 16#8000_0021#;

    -- CPUID.(7,0):EDX (cpufeatures.h word 18).
    Leaf_7_IBRS_IBPB : constant CPUID_Bit := 26;   -- SPEC_CTRL.IBRS and PRED_CMD
    Leaf_7_STIBP     : constant CPUID_Bit := 27;
    Leaf_7_SSBD      : constant CPUID_Bit := 31;
    -- CPUID.(7,2):EDX (word 21).
    Leaf_7_2_PSFD       : constant CPUID_Bit := 0;
    Leaf_7_2_IPRED_CTRL : constant CPUID_Bit := 1;
    Leaf_7_2_RRSBA_CTRL : constant CPUID_Bit := 2;
    Leaf_7_2_DDPD_U     : constant CPUID_Bit := 3;
    Leaf_7_2_BHI_CTRL   : constant CPUID_Bit := 4;
    -- CPUID 8000_0008h EBX (word 13).
    Extended_IBRS  : constant CPUID_Bit := 14;
    Extended_STIBP : constant CPUID_Bit := 15;
    Extended_SSBD  : constant CPUID_Bit := 24;
    Extended_PSFD  : constant CPUID_Bit := 28;
    -- CPUID 8000_0021h EAX (word 20).
    Extended_2_AutoIBRS : constant CPUID_Bit := 8;

    -- EFER.AIBRSE (AMD APM Vol. 2, 3.1.7).
    EFER_AutoIBRS_Bit : constant Bit_Number := 21;

    -- What the host CPU reports, each word 0 when its leaf is beyond the
    -- CPU's maximum (the caller reads only leaves the CPU has).
    type Host_CPUID is record
        Leaf_7_EDX   : Unsigned_32 := 0;
        Leaf_7_2_EDX : Unsigned_32 := 0;
        Extended_EBX : Unsigned_32 := 0;   -- 8000_0008h
        Extended_2_EAX : Unsigned_32 := 0; -- 8000_0021h
    end record;

    -- The feature behind SPEC_CTRL bit N, as the host reports it.
    function Offered (H : Host_CPUID; N : Bit_Number) return Boolean is
      (case N is
         when Control_IBRS =>
            Bit (H.Leaf_7_EDX, Leaf_7_IBRS_IBPB) or else Bit (H.Extended_EBX, Extended_IBRS),
         when Control_STIBP =>
            Bit (H.Leaf_7_EDX, Leaf_7_STIBP) or else Bit (H.Extended_EBX, Extended_STIBP),
         when Control_SSBD =>
            Bit (H.Leaf_7_EDX, Leaf_7_SSBD) or else Bit (H.Extended_EBX, Extended_SSBD),
         when Control_IPRED_DIS_U | Control_IPRED_DIS_S =>
            Bit (H.Leaf_7_2_EDX, Leaf_7_2_IPRED_CTRL),
         when Control_RRSBA_DIS_U | Control_RRSBA_DIS_S =>
            Bit (H.Leaf_7_2_EDX, Leaf_7_2_RRSBA_CTRL),
         when Control_PSFD =>
            Bit (H.Leaf_7_2_EDX, Leaf_7_2_PSFD) or else Bit (H.Extended_EBX, Extended_PSFD),
         when Control_DDPD_U =>
            Bit (H.Leaf_7_2_EDX, Leaf_7_2_DDPD_U),
         when Control_BHI_DIS_S =>
            Bit (H.Leaf_7_2_EDX, Leaf_7_2_BHI_CTRL),
         when others => False);

    function Writable_Bits (H : Host_CPUID) return Control_Mask
    with Post => (for all N in Bit_Number =>
                    Bit (Writable_Bits'Result, N) = Offered (H, N));

    -- The MSR exists when any of its bits does (Linux's MSR_SPEC_CTRL).
    function Present (Mask : Control_Mask) return Boolean is (Mask /= 0);

    -- A value the kernel may load: no bit outside the mask. A monitor's
    -- Set_State is held to this (0 always passes, as a state round trip of
    -- a host without the MSR needs).
    function Value_Allowed (Mask : Control_Mask; Value : Control_Value) return Boolean is
      ((Value and not Mask) = 0);

    -- A guest WRMSR of SPEC_CTRL that exited to the kernel.
    type Write_Verdict is
      (Fault,              -- #GP in the guest; the value is not kept
       Keep_Intercepting,  -- the value (0) is kept; writes still exit
       Pass_Writes);       -- the value is kept; later writes go to the MSR
    function Guest_Write (Mask : Control_Mask; Value : Control_Value) return Write_Verdict
    with Post =>
      ((Guest_Write'Result /= Fault) = (Present (Mask) and then Value_Allowed (Mask, Value)))
      and then ((Guest_Write'Result = Pass_Writes) =
                  (Present (Mask) and then Value_Allowed (Mask, Value) and then Value /= 0));

    -- What one entry and exit do to the MSR (the assembly's mode word).
    type Switch is
      (No_Switch,      -- host lacks the MSR, or the CPU switches it
       Load_Guest,     -- guest value loaded at entry if it differs from the host's
       Load_And_Save); -- the same, and read back at exit: the guest may have written it
    for Switch use (No_Switch => 0, Load_Guest => 1, Load_And_Save => 2);
    function Switch_Mode
      (Mask : Control_Mask; Hardware_Switched, Writes_Passed : Boolean) return Switch
    with Post =>
      ((Switch_Mode'Result = No_Switch) = (not Present (Mask) or else Hardware_Switched))
      and then (if Present (Mask) and then not Hardware_Switched and then Writes_Passed
                then Switch_Mode'Result = Load_And_Save);

    -- The EFER bits a monitor may give the guest, beyond Base: AutoIBRS on
    -- SVM when the host CPU has it (VMRUN loads the guest's EFER, #VMEXIT
    -- restores the host's, so it is the guest's own).
    function Guest_EFER_Bits (Base : Unsigned_64; H : Host_CPUID; SVM : Boolean) return Unsigned_64
    with Post =>
      (Bit (Guest_EFER_Bits'Result, EFER_AutoIBRS_Bit) =
         (Bit (Base, EFER_AutoIBRS_Bit)
          or else (SVM and then Bit (H.Extended_2_EAX, Extended_2_AutoIBRS))))
      and then (for all N in Bit_Number =>
                  (if N /= EFER_AutoIBRS_Bit then
                     Bit (Guest_EFER_Bits'Result, N) = Bit (Base, N)));
end Guest_Speculation;
