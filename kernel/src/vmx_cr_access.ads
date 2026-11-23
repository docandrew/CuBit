pragma Ada_2022;
with Interfaces; use Interfaces;

--  VMX control-register access exits (VMM-001). VMX pins some CR0 and CR4
--  bits (IA32_VMX_CR0_FIXED0: NE; IA32_VMX_CR4_FIXED0: VMXE). VMX_Run owns
--  them through the guest/host masks, and the guest sees its own values
--  through the read shadows. A guest write that changes an owned bit exits
--  with reason 28 instead of taking effect. The first one in a Linux guest
--  is each AP's `mov %eax, %cr0` in the trampoline (CR0_STATE sets NE),
--  which image 33 answered #UD, triple-faulting the guest.
--
--  This unit decides such an access the way the processor would: MOV to
--  CR0/CR4, CLTS and LMSW, with the architectural #GP checks (Intel SDM
--  vol. 2B "MOV-Move to/from Control Registers", "LMSW", "CLTS"; vol. 3A
--  2.5) and EFER.LMA following CR0.PG (vol. 3A 10.8.5). The exit
--  qualification's layout is vol. 3C table 28-3. Pure; no hardware access.
--  What it does not model goes back to the monitor as before (#UD).
package VMX_CR_Access with SPARK_Mode => On, Pure is

   --  CR0 (vol. 3A 2.5).
   CR0_PE : constant Unsigned_64 := 2 ** 0;
   CR0_MP : constant Unsigned_64 := 2 ** 1;
   CR0_EM : constant Unsigned_64 := 2 ** 2;
   CR0_TS : constant Unsigned_64 := 2 ** 3;
   CR0_ET : constant Unsigned_64 := 2 ** 4;
   CR0_NE : constant Unsigned_64 := 2 ** 5;
   CR0_WP : constant Unsigned_64 := 2 ** 16;
   CR0_AM : constant Unsigned_64 := 2 ** 18;
   CR0_NW : constant Unsigned_64 := 2 ** 29;
   CR0_CD : constant Unsigned_64 := 2 ** 30;
   CR0_PG : constant Unsigned_64 := 2 ** 31;
   CR0_Defined : constant Unsigned_64 :=
     CR0_PE or CR0_MP or CR0_EM or CR0_TS or CR0_ET or CR0_NE or CR0_WP
     or CR0_AM or CR0_NW or CR0_CD or CR0_PG;
   --  LMSW loads only the machine status word's low four bits.
   MSW_Bits : constant Unsigned_64 := CR0_PE or CR0_MP or CR0_EM or CR0_TS;
   --  Bits whose change invalidates the guest's translations.
   CR0_Paging_Bits : constant Unsigned_64 := CR0_PE or CR0_WP or CR0_CD or CR0_PG;

   --  CR4 (vol. 3A 2.5).
   CR4_PSE   : constant Unsigned_64 := 2 ** 4;
   CR4_PAE   : constant Unsigned_64 := 2 ** 5;
   CR4_PGE   : constant Unsigned_64 := 2 ** 7;
   CR4_VMXE  : constant Unsigned_64 := 2 ** 13;
   CR4_PCIDE : constant Unsigned_64 := 2 ** 17;
   CR4_SMEP  : constant Unsigned_64 := 2 ** 20;
   CR4_SMAP  : constant Unsigned_64 := 2 ** 21;
   CR4_Paging_Bits : constant Unsigned_64 :=
     CR4_PSE or CR4_PAE or CR4_PGE or CR4_PCIDE or CR4_SMEP or CR4_SMAP;

   --  EFER (vol. 3A 2.2.1).
   EFER_LME : constant Unsigned_64 := 2 ** 8;
   EFER_LMA : constant Unsigned_64 := 2 ** 10;

   Low_32 : constant Unsigned_64 := 16#FFFF_FFFF#;

   --  The exit qualification (vol. 3C table 28-3).
   type Access_Kind is (Move_To_CR, Move_From_CR, Clear_TS, Load_MSW);
   for Access_Kind use (Move_To_CR => 0, Move_From_CR => 1, Clear_TS => 2, Load_MSW => 3);
   subtype CR_Number is Unsigned_64 range 0 .. 15;
   subtype GPR_Number is Natural range 0 .. 15;   --  0 RAX .. 15 R15, the VMX encoding

   function Number_Of (Qualification : Unsigned_64) return CR_Number is
     (Qualification and 16#F#);
   function Kind_Of (Qualification : Unsigned_64) return Access_Kind is
     (case Shift_Right (Qualification, 4) and 3 is
        when 0 => Move_To_CR, when 1 => Move_From_CR,
        when 2 => Clear_TS, when others => Load_MSW);
   function Register_Of (Qualification : Unsigned_64) return GPR_Number is
     (GPR_Number (Shift_Right (Qualification, 8) and 16#F#));
   function MSW_Source_Of (Qualification : Unsigned_64) return Unsigned_64 is
     (Shift_Right (Qualification, 16) and 16#FFFF#);

   type Outcome_Kind is (Applied, General_Protection, Not_Modelled);

   type Decision is record
      Outcome : Outcome_Kind;
      CR0, CR4, EFER : Unsigned_64;   --  the guest's view after the access
      Flush : Boolean;                --  a translation-relevant bit changed
   end record;

   --  A CR0 value the processor would accept (vol. 2B MOV to CR0 #GP(0)).
   function Valid_CR0 (CR0, CR4, EFER : Unsigned_64) return Boolean is
     ((CR0 and not Low_32) = 0
      and then ((CR0 and CR0_PG) = 0 or else (CR0 and CR0_PE) /= 0)
      and then ((CR0 and CR0_NW) = 0 or else (CR0 and CR0_CD) /= 0)
      and then ((CR0 and CR0_PG) = 0 or else (EFER and EFER_LME) = 0
                or else (CR4 and CR4_PAE) /= 0));

   --  EFER.LMA is EFER.LME and CR0.PG once paging is set (vol. 3A 10.8.5).
   function Long_Mode_Follows (CR0, EFER : Unsigned_64) return Unsigned_64 is
     ((if (CR0 and CR0_PG) /= 0 and then (EFER and EFER_LME) /= 0
       then EFER or EFER_LMA else EFER and not EFER_LMA));

   --  Decide one control-register access exit. Source is the register the
   --  qualification names (MOV to CR). CR4_Allowed is the CR4 bits the
   --  guest may hold (IA32_VMX_CR4_FIXED1 without VMXE: CPUID does not
   --  offer VMX to the guest).
   function Decide (Qualification, Source, CR0, CR4, EFER, CR4_Allowed : Unsigned_64)
                    return Decision
     with Post =>
       --  Anything not applied changes nothing.
       (if Decide'Result.Outcome /= Applied then
          Decide'Result.CR0 = CR0 and then Decide'Result.CR4 = CR4
          and then Decide'Result.EFER = EFER and then not Decide'Result.Flush)
       --  Only CR0 and CR4 accesses are ever applied.
       and then (if Decide'Result.Outcome = Applied then
                   Number_Of (Qualification) in 0 | 4
                   and then Kind_Of (Qualification) /= Move_From_CR)
       --  An applied CR0 access leaves CR4 alone and a valid CR0, with
       --  EFER.LMA following CR0.PG and EFER's other bits untouched.
       and then (if Decide'Result.Outcome = Applied and then Number_Of (Qualification) = 0 then
                   Decide'Result.CR4 = CR4
                   and then Valid_CR0 (Decide'Result.CR0, CR4, EFER)
                   and then Decide'Result.EFER = Long_Mode_Follows (Decide'Result.CR0, EFER)
                   and then (Decide'Result.CR0 and not CR0_Defined) = 0)
       --  LMSW sets PE but never clears it (vol. 2B LMSW).
       and then (if Decide'Result.Outcome = Applied
                   and then Number_Of (Qualification) = 0
                   and then Kind_Of (Qualification) = Load_MSW
                   and then (CR0 and CR0_PE) /= 0
                 then (Decide'Result.CR0 and CR0_PE) /= 0)
       --  An applied CR4 access leaves CR0 and EFER alone, never sets VMXE
       --  or a bit outside CR4_Allowed, and keeps PAE while in long mode.
       and then (if Decide'Result.Outcome = Applied and then Number_Of (Qualification) = 4 then
                   Decide'Result.CR0 = CR0 and then Decide'Result.EFER = EFER
                   and then (Decide'Result.CR4 and CR4_VMXE) = 0
                   and then (Decide'Result.CR4 and not CR4_Allowed) = 0
                   and then ((EFER and EFER_LMA) = 0 or else (Decide'Result.CR4 and CR4_PAE) /= 0));
end VMX_CR_Access;
