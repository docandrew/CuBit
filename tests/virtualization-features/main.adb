--  VMM-001 slice 0: hosted tests of Virtualization_Features. Each case
--  builds raw MSR or CPUID values from the named bit positions, decodes
--  them and checks the features and the verdict; a requirement removed
--  alone must give exactly its own refusal.
pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Virtualization_Features; use Virtualization_Features;

procedure Main is
   Failures : Natural := 0;
   Checks   : Natural := 0;

   procedure Check (Ok : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Ok then
         Failures := Failures + 1;
         Put_Line ("virtualization-features: FAIL " & What);
      end if;
   end Check;

   function B (N : Bit_Number) return Unsigned_64 is (Shift_Left (1, N));
   --  A control the CPU allows to be 1 (allowed-1 half).
   function A1 (N : Control_Bit) return Unsigned_64 is (B (Allowed_One_Base + N));

   type Field_Kind is (Pin, Proc, Proc2, Exit_Ctl, Entry_Ctl, EPT_VPID, Basic);

   --  Every requirement met, on a CPU like the ones the design targets.
   function Full return VMX_Raw is
     ((Feature_Control => B (Feature_Control_Lock) or B (Feature_Control_VMX_Outside_SMX),
       Basic => Shift_Left (Memory_Type_Write_Back, Basic_Memory_Type_Low)
                or Shift_Left (4096, Basic_Region_Size_Low) or B (Basic_True_Controls),
       Pin => A1 (Pin_External_Interrupt_Exiting) or A1 (Pin_NMI_Exiting)
              or A1 (Pin_Virtual_NMIs) or A1 (Pin_Preemption_Timer)
              or A1 (Pin_Posted_Interrupts),
       Proc => A1 (Proc_Interrupt_Window) or A1 (Proc_HLT_Exiting)
               or A1 (Proc_TPR_Shadow) or A1 (Proc_NMI_Window)
               or A1 (Proc_IO_Bitmaps) or A1 (Proc_Monitor_Trap_Flag)
               or A1 (Proc_MSR_Bitmaps) or A1 (Proc_Pause_Exiting)
               or A1 (Proc_Secondary_Controls),
       Exit_Controls => A1 (Exit_Host_Address_Space_Size)
                        or A1 (Exit_Acknowledge_Interrupt) or A1 (Exit_Load_EFER),
       Entry_Controls => A1 (Entry_IA32E_Mode_Guest) or A1 (Entry_Load_EFER),
       Misc => 5 or B (Misc_Activity_HLT),
       Proc2 => A1 (Proc2_EPT) or A1 (Proc2_VPID) or A1 (Proc2_Unrestricted_Guest)
                or A1 (Proc2_Virtual_Interrupt_Delivery)
                or A1 (Proc2_Mode_Based_Execute),
       EPT_VPID => B (EPT_Walk_4) or B (EPT_Write_Back) or B (EPT_Pages_2M)
                   or B (EPT_Pages_1G) or B (EPT_INVEPT) or B (EPT_INVEPT_Single)
                   or B (EPT_INVEPT_All) or B (VPID_INVVPID)
                   or B (VPID_INVVPID_Single) or B (VPID_INVVPID_All),
       Proc3 => 0));

   function Without (Raw : VMX_Raw; K : Field_Kind; Bit_Mask : Unsigned_64) return VMX_Raw is
      R : VMX_Raw := Raw;
   begin
      case K is
         when Pin       => R.Pin := R.Pin and not Bit_Mask;
         when Proc      => R.Proc := R.Proc and not Bit_Mask;
         when Proc2     => R.Proc2 := R.Proc2 and not Bit_Mask;
         when Exit_Ctl  => R.Exit_Controls := R.Exit_Controls and not Bit_Mask;
         when Entry_Ctl => R.Entry_Controls := R.Entry_Controls and not Bit_Mask;
         when EPT_VPID  => R.EPT_VPID := R.EPT_VPID and not Bit_Mask;
         when Basic     => R.Basic := R.Basic and not Bit_Mask;
      end case;
      return R;
   end Without;

   procedure Expect (Raw : VMX_Raw; V : VMX_Verdict_Kind; What : String) is
      Got : constant VMX_Verdict_Kind := VMX_Verdict (Decode_VMX (Raw));
   begin
      Check (Got = V, What & ": expected " & V'Image & ", got " & Got'Image);
   end Expect;

   F : VMX_Features;
   R : VMX_Raw;
begin
   -- VMX: the full set is usable, and its features decode as set.
   F := Decode_VMX (Full);
   Check (VMX_Verdict (F) = Usable, "full VMX set is usable");
   Check (F.VMCS_Bytes = 4096, "VMCS region size field");
   Check (F.Timer_Rate_Shift = 5, "preemption timer rate field");
   Check (F.EPT_1G and F.EPT_2M and F.Posted_Interrupts and F.Mode_Based_Execute
          and F.Virtual_Interrupt_Delivery and F.Activity_HLT, "optional features decode");
   Check (not F.VMCS_Shadowing and not F.Page_Modification_Logging
          and not F.IPI_Virtualization, "absent features decode as absent");

   -- Each requirement removed alone gives exactly its own refusal.
   Expect (Without (Full, Basic, B (Basic_True_Controls)), No_True_Controls, "TRUE controls");
   Expect (Without (Full, Basic, Shift_Left (Memory_Type_Write_Back, Basic_Memory_Type_Low)),
           No_Write_Back_Regions, "write-back VMCS");
   Expect (Without (Full, Pin, A1 (Pin_External_Interrupt_Exiting)),
           No_External_Interrupt_Exiting, "external-interrupt exiting");
   Expect (Without (Full, Pin, A1 (Pin_NMI_Exiting)), No_NMI_Exiting, "NMI exiting");
   Expect (Without (Full, Proc, A1 (Proc_HLT_Exiting)), No_HLT_Exiting, "HLT exiting");
   Expect (Without (Full, Proc, A1 (Proc_IO_Bitmaps)), No_IO_Bitmaps, "I/O bitmaps");
   Expect (Without (Full, Proc, A1 (Proc_MSR_Bitmaps)), No_MSR_Bitmaps, "MSR bitmaps");
   Expect (Without (Full, Proc, A1 (Proc_Interrupt_Window)), No_Interrupt_Window, "interrupt window");
   Expect (Without (Full, Proc, A1 (Proc_NMI_Window)), No_NMI_Window, "NMI window");
   Expect (Without (Full, Proc, A1 (Proc_Secondary_Controls)), No_Secondary_Controls,
           "secondary controls");
   Expect (Without (Full, Proc2, A1 (Proc2_EPT)), No_EPT, "EPT");
   Expect (Without (Full, Proc2, A1 (Proc2_Unrestricted_Guest)), No_Unrestricted_Guest,
           "unrestricted guest");
   Expect (Without (Full, Proc2, A1 (Proc2_VPID)), No_VPID, "VPID");
   Expect (Without (Full, EPT_VPID, B (EPT_Walk_4)), No_EPT_Walk_4, "4-level EPT");
   Expect (Without (Full, EPT_VPID, B (EPT_Write_Back)), No_EPT_Write_Back, "write-back EPT");
   Expect (Without (Full, EPT_VPID, B (EPT_INVEPT_Single)), No_INVEPT_Single, "single INVEPT");
   Expect (Without (Full, EPT_VPID, B (EPT_INVEPT)), No_INVEPT_Single, "INVEPT itself");
   Expect (Without (Full, EPT_VPID, B (VPID_INVVPID_Single)), No_INVVPID_Single, "single INVVPID");
   Expect (Without (Full, Exit_Ctl, A1 (Exit_Host_Address_Space_Size)), No_Host_64, "64-bit host");
   Expect (Without (Full, Exit_Ctl, A1 (Exit_Acknowledge_Interrupt)), No_Acknowledge_Interrupt,
           "acknowledge on exit");
   Expect (Without (Full, Entry_Ctl, A1 (Entry_IA32E_Mode_Guest)), No_Guest_64, "64-bit guest");
   Expect (Without (Full, Exit_Ctl, A1 (Exit_Load_EFER)), No_EFER_Load, "EFER load on exit");
   Expect (Without (Full, Entry_Ctl, A1 (Entry_Load_EFER)), No_EFER_Load, "EFER load on entry");

   -- Firmware: locked without VMX is refused; unlocked is ours to set.
   R := Full;
   R.Feature_Control := B (Feature_Control_Lock);
   Expect (R, Disabled_By_Firmware, "locked without VMX outside SMX");
   R.Feature_Control := 0;
   Expect (R, Usable, "unlocked feature control");

   -- A control the CPU only requires (allowed-0 half) is not one it offers.
   R := Full;
   R.Proc2 := R.Proc2 and not A1 (Proc2_EPT);
   R.Proc2 := R.Proc2 or B (Proc2_EPT);
   Expect (R, No_EPT, "EPT in the allowed-0 half only");

   -- Without the secondary controls, the secondary MSR's bits mean nothing.
   R := Without (Full, Proc, A1 (Proc_Secondary_Controls));
   F := Decode_VMX (R);
   Check (not F.EPT and not F.VPID and not F.Unrestricted_Guest and not F.INVEPT_Single,
          "secondary features need the secondary controls");

   -- The EPT/VPID capabilities need EPT (or VPID) itself.
   R := Without (Full, Proc2, A1 (Proc2_EPT));
   F := Decode_VMX (R);
   Check (not F.EPT_1G and not F.INVEPT_Single and F.INVVPID_Single,
          "EPT capabilities need EPT, VPID capabilities do not");

   -- Tertiary controls: only behind the primary control that announces them.
   R := Full;
   R.Proc3 := B (Proc3_IPI_Virtualization) or B (Proc3_HLAT);
   Check (not Decode_VMX (R).IPI_Virtualization, "tertiary bits without the tertiary control");
   R.Proc := R.Proc or A1 (Proc_Tertiary_Controls);
   Check (Decode_VMX (R).IPI_Virtualization and Decode_VMX (R).HLAT,
          "tertiary bits with the tertiary control");

   -- SVM, from Zen 5's flags as Linux reports them on Chungus (2026-11-20):
   -- npt lbrv nrip_save tsc_scale vmcb_clean flushbyasid decodeassists
   -- pausefilter pfthreshold avic v_vmsave_vmload vgif x2avic v_spec_ctrl vnmi.
   declare
      function S (N : Bit_Number) return Unsigned_32 is (Unsigned_32 (B (N)));
      Zen5 : constant SVM_Raw :=
        (Extended_ECX => S (Extended_ECX_SVM),
         Has_SVM_Leaf => True, Revision => 1, ASIDs => 16#8000#,
         Features => S (SVM_Nested_Paging) or S (SVM_LBR_Virtualization)
                     or S (SVM_Next_RIP_Save) or S (SVM_TSC_Rate)
                     or S (SVM_VMCB_Clean) or S (SVM_Flush_By_ASID)
                     or S (SVM_Decode_Assists) or S (SVM_Pause_Filter)
                     or S (SVM_Pause_Threshold) or S (SVM_AVIC)
                     or S (SVM_VMSAVE_Virtualization) or S (SVM_Virtual_GIF)
                     or S (SVM_X2AVIC) or S (SVM_Virtual_Spec_Ctrl)
                     or S (SVM_Virtual_NMI),
         Has_Memory_Leaf => True, Memory_Features => 0,
         VM_CR => 0, VM_CR_Read => True);
      G : SVM_Features := Decode_SVM (Zen5);
      Z : SVM_Raw;
   begin
      Check (SVM_Verdict (G) = Usable, "Zen 5 SVM is usable");
      Check (G.X2AVIC and G.Virtual_NMI and G.Decode_Assists and G.VMCB_Clean
             and G.Flush_By_ASID and not G.GMET and not G.SEV_SNP, "Zen 5 features");
      Check (G.ASIDs = 16#8000# and G.Revision = 1, "revision and ASIDs");
      Z := Zen5; Z.VM_CR := B (VM_CR_SVM_Disabled);
      Check (SVM_Verdict (Decode_SVM (Z)) = Disabled_By_Firmware, "VM_CR.SVMDIS");
      Z := Zen5; Z.Features := Z.Features and not S (SVM_Nested_Paging);
      Check (SVM_Verdict (Decode_SVM (Z)) = No_Nested_Paging, "no NPT");
      Z := Zen5; Z.Features := Z.Features and not S (SVM_Next_RIP_Save);
      Check (SVM_Verdict (Decode_SVM (Z)) = No_Next_RIP_Save, "no next-RIP save");
      Z := Zen5; Z.ASIDs := 1;
      Check (SVM_Verdict (Decode_SVM (Z)) = No_ASIDs, "only the host's ASID");
      Z := Zen5; Z.Has_SVM_Leaf := False;
      Check (SVM_Verdict (Decode_SVM (Z)) = No_Nested_Paging
             and not Decode_SVM (Z).X2AVIC, "no SVM leaf");
      Z := Zen5; Z.Extended_ECX := 0;
      G := Decode_SVM (Z);
      Check (SVM_Verdict (G) = No_SVM and not G.Nested_Paging, "no SVM bit");
      Z := Zen5; Z.Memory_Features := S (Memory_SEV) or S (Memory_SEV_SNP);
      Check (Decode_SVM (Z).SEV_SNP and Decode_SVM (Z).SEV, "SEV bits");
   end;

   -- Independent oracle: raw values written as literals from the SDM and
   -- APM bit numbers (and, for SVM, from Linux's cpufeatures.h word 15 and
   -- Chungus's /proc/cpuinfo flags), never from the unit's own constants.
   declare
      Literal_VMX : constant VMX_Raw :=
        (Feature_Control => 16#5#,                       -- lock, VMX outside SMX
         Basic           => 16#0098_1000_0000_0000#,     -- TRUE ctls, WB, 4096 bytes
         Pin             => 16#0000_0089_0000_0000#,     -- 0 ext-int, 3 NMI, 7 posted
         Proc            => 16#9240_0084_0000_0000#,     -- 2, 7, 22, 25, 28, 31
         Exit_Controls   => 16#0020_8200_0000_0000#,     -- 9 host-64, 15 ack, 21 EFER
         Entry_Controls  => 16#0000_8200_0000_0000#,     -- 9 guest-64, 15 EFER
         Misc            => 0,
         Proc2           => 16#0000_00A2_0000_0000#,     -- 1 EPT, 5 VPID, 7 UG
         EPT_VPID        => 16#0000_0201_0210_4040#,     -- 6, 14, 20, 25, 32, 41
         Proc3           => 0);
      L : constant VMX_Features := Decode_VMX (Literal_VMX);
      --  Chungus (Ryzen 9 9950X3D): npt lbrv svm_lock nrip_save tsc_scale
      --  vmcb_clean flushbyasid decodeassists pausefilter pfthreshold avic
      --  v_vmsave_vmload vgif x2avic v_spec_ctrl vnmi = bits 0-7, 10, 12,
      --  13, 15, 16, 18, 20, 25.
      Literal_SVM : constant SVM_Raw :=
        (Extended_ECX => 16#4#, Has_SVM_Leaf => True, Revision => 1,
         ASIDs => 16#8000#, Features => 16#0215_B4FF#,
         Has_Memory_Leaf => False, Memory_Features => 0,
         VM_CR => 0, VM_CR_Read => True);
      M : constant SVM_Features := Decode_SVM (Literal_SVM);
   begin
      Check (VMX_Verdict (L) = Usable, "literal VMX set is usable");
      Check (L.EPT and L.VPID and L.Unrestricted_Guest and L.Posted_Interrupts
             and L.INVEPT_Single and L.INVVPID_Single and L.EPT_Walk_4
             and L.EPT_Write_Back and L.VMCS_Bytes = 4096, "literal VMX features");
      Check (not L.EPT_1G and not L.Mode_Based_Execute and not L.Virtual_NMIs
             and not L.Preemption_Timer and not L.TPR_Shadow, "literal VMX absences");
      Check (SVM_Verdict (M) = Usable, "literal Zen 5 SVM is usable");
      Check (M.Nested_Paging and M.LBR_Virtualization and M.Lock and M.Next_RIP_Save
             and M.TSC_Rate and M.VMCB_Clean and M.Flush_By_ASID and M.Decode_Assists
             and M.Pause_Filter and M.Pause_Threshold and M.AVIC
             and M.VMSAVE_Virtualization and M.Virtual_GIF and M.X2AVIC
             and M.Virtual_Spec_Ctrl and M.Virtual_NMI, "literal Zen 5 features");
      Check (not M.GMET and not M.Bus_Lock_Threshold and not M.Idle_HLT, "literal Zen 5 absences");
      --  The two required SVM bits alone: 0 npt, 3 nrip_save.
      declare
         Minimal : SVM_Raw := Literal_SVM;
      begin
         Minimal.Features := 16#9#;
         Check (SVM_Verdict (Decode_SVM (Minimal)) = Usable
                and not Decode_SVM (Minimal).TSC_Rate
                and not Decode_SVM (Minimal).LBR_Virtualization, "literal minimal SVM");
      end;
   end;

   if Failures = 0 then
      Put_Line ("virtualization-features:" & Checks'Image & " checks PASS");
   else
      Put_Line ("virtualization-features:" & Failures'Image & " of" & Checks'Image & " FAILED");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;
