--  VMM-001 slice 1: hosted tests of Guest_Tables, VM_Admission and
--  Guest_Speculation, with literal oracles from the AMD APM (NPT is the
--  long-mode format), the SDM's event-injection rules and the SPEC_CTRL bit
--  numbers of SDM Vol. 4 Table 2-2.
pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Guest_Tables; use Guest_Tables;
with VM_Admission; use VM_Admission;
with Guest_Speculation;
with VM_ABI;

procedure Main is
   Failures, Checks : Natural := 0;
   procedure Check (Ok : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Ok then
         Failures := Failures + 1;
         Put_Line ("vm-units: FAIL " & What);
      end if;
   end Check;
   T : Run_Table := [others => <>];
   type Sample_List is array (1 .. 4) of Unsigned_64;
   Samples : constant Sample_List := [16#7FFF_FFFF_F000#, 16#1234_5678_9000#, 16#1000#, 0];
begin
   -- Nested-table encodings, literal: P | R/W | U/S = 7, NX = bit 63.
   Check (Table_Entry (16#1234_5000#) = 16#1234_5007#, "table entry literal");
   Check (Leaf (16#ABC_D000#, True, True) = 16#ABC_D007#, "writable executable leaf");
   Check (Leaf (16#ABC_D000#, False, False) = 16#8000_0000_0ABC_D005#, "read-only NX leaf");
   Check (Index_Of (16#0000_7FFF_FFFF_F000#, 4) = 255 and then Index_Of (16#1000#, 1) = 1
          and then Index_Of (16#20_0000#, 2) = 1 and then Index_Of (16#4000_0000#, 3) = 1,
          "indices");
   Check (Compose (1, 2, 3, 4) = 16#0080_8060_4000#, "compose");
   for A of Samples loop
      Check (Compose (Index_Of (A, 4), Index_Of (A, 3), Index_Of (A, 2), Index_Of (A, 1)) = A,
             "indices recompose the address");
   end loop;
   Check (Well_Formed (Leaf (16#1000#, True, False)) and then not Well_Formed (16#80#),
          "well-formed rejects PS");

   -- Ranges.
   Check (Range_Admission (T, 0, 0, 0, 2 * 4096) = Admitted, "first run");
   T (1) := (First => 0, Pages => 2);
   Check (Range_Admission (T, 1, 2, 4096, 4096) = Overlapping, "overlap");
   Check (Range_Admission (T, 1, 2, 8192, 4096) = Admitted, "adjacent");
   Check (Range_Admission (T, 1, 2, 100, 4096) = Bad_Range, "unaligned");
   Check (Range_Admission (T, 1, 2, 8192, 0) = Bad_Range, "empty");
   Check (Range_Admission (T, 1, 2, 2 ** 48 - 4096, 8192) = Bad_Range, "past the space");
   Check (Range_Admission (T, 1, VM_ABI.Guest_Page_Ceiling - 1, 8192, 8192) = Too_Large,
          "page ceiling");

   -- Injection.
   Check (Injection_Valid (VM_ABI.External, 32, False), "external 32");
   Check (not Injection_Valid (VM_ABI.External, 31, False), "external below 32");
   Check (not Injection_Valid (VM_ABI.External, 48, True), "external with error code");
   Check (Injection_Valid (VM_ABI.Hardware_Exception, 13, True), "#GP with code");
   Check (not Injection_Valid (VM_ABI.Hardware_Exception, 13, False), "#GP without code");
   Check (Injection_Valid (VM_ABI.Hardware_Exception, 6, False), "#UD");
   Check (not Injection_Valid (VM_ABI.Hardware_Exception, 6, True), "#UD with code");
   Check (not Injection_Valid (VM_ABI.Hardware_Exception, 2, False), "vector 2 as exception");
   Check (not Injection_Valid (VM_ABI.Hardware_Exception, 15, False), "reserved 15");
   Check (Injection_Valid (VM_ABI.Hardware_Exception, 14, True), "#PF with code");
   Check (Injection_Valid (VM_ABI.NMI, 2, False), "NMI");


   -- Guest SPEC_CTRL: writable bits, literal (SDM Vol. 4: IBRS 0, STIBP 1,
   -- SSBD 2, IPRED_DIS 3-4, RRSBA_DIS 5-6, PSFD 7, DDPD_U 8, BHI_DIS_S 10).
   declare
      package S renames Guest_Speculation;
      use type S.Write_Verdict, S.Switch;
      None : constant S.Host_CPUID := (others => 0);
      --  Zen 5 as KVM shows it to CuBit on Chungus: 8000_0008h EBX IBPB,
      --  IBRS, STIBP, STIBP always on, SSBD, PSFD, and leaf 7 EDX's
      --  SPEC_CTRL, STIBP and SSBD bits that KVM sets for AMD hosts.
      Zen : constant S.Host_CPUID :=
        (Leaf_7_EDX => 16#8C00_0000#, Leaf_7_2_EDX => 0,
         Extended_EBX => 16#1102_D000#, Extended_2_EAX => 16#0000_0100#);
      Everything : constant S.Host_CPUID := (others => 16#FFFF_FFFF#);
      Bare_AMD : constant S.Host_CPUID :=
        (Leaf_7_EDX => 0, Leaf_7_2_EDX => 0, Extended_EBX => 16#0100_0000#, Extended_2_EAX => 0);
      Intel_Controls : constant S.Host_CPUID :=
        (Leaf_7_EDX => 16#0400_0000#, Leaf_7_2_EDX => 16#0000_0016#,
         Extended_EBX => 0, Extended_2_EAX => 0);
      Zen_Mask : constant Unsigned_64 := S.Writable_Bits (Zen);
   begin
      Check (S.Writable_Bits (None) = 0 and then not S.Present (S.Writable_Bits (None)),
             "no SPEC_CTRL without its CPUID bits");
      Check (Zen_Mask = 16#87#, "Zen 5 (KVM view): IBRS, STIBP, SSBD, PSFD");
      Check (S.Writable_Bits (Everything) = 16#5FF#, "every defined bit and no other");
      Check (S.Writable_Bits (Bare_AMD) = 16#04#, "AMD SSBD alone");
      Check (S.Writable_Bits (Intel_Controls) = 16#479#,
             "Intel IBRS, IPRED, RRSBA and BHI controls: bits 0, 3-6, 10");
      Check (S.Guest_Write (Zen_Mask, 0) = S.Keep_Intercepting, "a zero write keeps intercepting");
      Check (S.Guest_Write (Zen_Mask, 16#04#) = S.Pass_Writes, "SSBD is passed through");
      Check (S.Guest_Write (Zen_Mask, 16#08#) = S.Fault, "IPRED_DIS_U on Zen 5 is #GP");
      Check (S.Guest_Write (Zen_Mask, 2 ** 63) = S.Fault, "bit 63 is #GP");
      Check (S.Guest_Write (0, 0) = S.Fault, "a write without the MSR is #GP, even 0");
      Check (S.Value_Allowed (0, 0) and then not S.Value_Allowed (Zen_Mask, 16#200#),
             "Set_State: 0 always, an undefined bit never");
      Check (S.Switch_Mode (Zen_Mask, False, False) = S.Load_Guest
             and then S.Switch_Mode (Zen_Mask, False, True) = S.Load_And_Save
             and then S.Switch_Mode (Zen_Mask, True, True) = S.No_Switch
             and then S.Switch_Mode (0, False, True) = S.No_Switch,
             "switch modes");
      Check (S.Switch'Enum_Rep (S.Load_And_Save) = 2, "the assembly's mode encoding");
      Check (S.Guest_EFER_Bits (16#DD01#, Zen, True) = 16#20_DD01#
             and then S.Guest_EFER_Bits (16#DD01#, Zen, False) = 16#DD01#
             and then S.Guest_EFER_Bits (16#DD01#, None, True) = 16#DD01#,
             "EFER.AIBRSE (bit 21) only on SVM with AutoIBRS");
   end;

   if Failures = 0 then
      Put_Line ("vm-units:" & Checks'Image & " checks PASS");
   else
      Put_Line ("vm-units:" & Failures'Image & " of" & Checks'Image & " FAILED");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;
