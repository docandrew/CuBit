--  Hosted test of VMX_CR_Access (VMM-001): the Linux AP trampoline's CR0
--  sequence, every #GP rule, CLTS and LMSW, and every combination of CR0's
--  defined bits against an independent reference.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with VMX_CR_Access; use VMX_CR_Access;

procedure Main is
   Checks : Natural := 0;
   Failed : Natural := 0;

   procedure Check (OK : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not OK then
         Failed := Failed + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   --  Qualification: CR number, access type << 4, register << 8, MSW << 16.
   function Q (CR : Unsigned_64; Kind : Access_Kind; Register : Unsigned_64 := 0;
               MSW : Unsigned_64 := 0) return Unsigned_64 is
     (CR or Shift_Left (Unsigned_64 (Access_Kind'Enum_Rep (Kind)), 4)
      or Shift_Left (Register, 8) or Shift_Left (MSW, 16));

   --  IA32_VMX_CR4_FIXED1 on the N150 (all of these architecturally
   --  defined bits), less VMXE.
   Allowed : constant Unsigned_64 := 16#003F_FFFF# and not CR4_VMXE;
   Reset_CR0 : constant Unsigned_64 := 16#6000_0010#;   --  vol. 3A 10.1.1: CD NW ET
   --  arch/x86/include/asm/processor-flags.h CR0_STATE:
   --  PE MP ET NE WP AM PG.
   CR0_State : constant Unsigned_64 := 16#8005_0033#;
   D : Decision;
begin
   --  The AP trampoline (arch/x86/realmode/rm/trampoline_64.S): LMSW sets PE.
   D := Decide (Q (0, Load_MSW, MSW => 1), 0, Reset_CR0, 0, 0, Allowed);
   Check (D.Outcome = Applied and then D.CR0 = Reset_CR0 + 1 and then D.Flush, "LMSW sets PE");
   --  ... then CR4.PAE, EFER.LME, and MOV CR0_STATE from EAX: long mode.
   D := Decide (Q (0, Move_To_CR, 0), CR0_State, Reset_CR0 + 1, CR4_PAE, EFER_LME, Allowed);
   Check (D.Outcome = Applied and then D.CR0 = CR0_State, "CR0_STATE applied");
   Check (D.EFER = (EFER_LME or EFER_LMA), "LMA follows PG with LME");
   Check (D.CR4 = CR4_PAE and then D.Flush, "CR4 kept, translations flushed");
   --  The same write without PAE is #GP.
   D := Decide (Q (0, Move_To_CR, 0), CR0_State, Reset_CR0 + 1, 0, EFER_LME, Allowed);
   Check (D.Outcome = General_Protection and then D.CR0 = Reset_CR0 + 1, "PG with LME without PAE");
   --  PG without PE; NW without CD; high bits.
   D := Decide (Q (0, Move_To_CR), CR0_PG, 0, 0, 0, Allowed);
   Check (D.Outcome = General_Protection, "PG without PE");
   D := Decide (Q (0, Move_To_CR), CR0_NW or CR0_PE, 0, 0, 0, Allowed);
   Check (D.Outcome = General_Protection, "NW without CD");
   D := Decide (Q (0, Move_To_CR), 2 ** 32 or CR0_PE, 0, 0, 0, Allowed);
   Check (D.Outcome = General_Protection, "CR0 bit 32");
   --  Reserved low bits are dropped, ET forced.
   D := Decide (Q (0, Move_To_CR), CR0_PE or 2 ** 6 or 2 ** 17, 0, 0, 0, Allowed);
   Check (D.Outcome = Applied and then D.CR0 = (CR0_PE or CR0_ET), "reserved low bits dropped");
   --  Paging off clears LMA.
   D := Decide (Q (0, Move_To_CR), CR0_PE or CR0_ET, CR0_State, CR4_PAE,
                EFER_LME or EFER_LMA, Allowed);
   Check (D.Outcome = Applied and then D.EFER = EFER_LME, "PG off clears LMA");
   --  CLTS; LMSW never clears PE; LMSW leaves NE and PG.
   D := Decide (Q (0, Clear_TS), 0, CR0_State or CR0_TS, CR4_PAE, EFER_LME or EFER_LMA, Allowed);
   Check (D.Outcome = Applied and then D.CR0 = CR0_State and then not D.Flush, "CLTS");
   D := Decide (Q (0, Load_MSW, MSW => 0), 0, CR0_State, CR4_PAE, EFER_LME or EFER_LMA, Allowed);
   Check (D.Outcome = Applied and then D.CR0 = (CR0_State and not (CR0_MP)), "LMSW keeps PE");
   --  Register numbers from the qualification.
   Check (Register_Of (Q (0, Move_To_CR, 13)) = 13, "register field");
   --  Not modelled: reads, CR3, CR8, CLTS on CR4.
   D := Decide (Q (0, Move_From_CR), 0, CR0_State, 0, 0, Allowed);
   Check (D.Outcome = Not_Modelled and then D.CR0 = CR0_State, "MOV from CR0");
   D := Decide (Q (3, Move_To_CR), 16#1000#, CR0_State, 0, 0, Allowed);
   Check (D.Outcome = Not_Modelled, "CR3");
   D := Decide (Q (8, Move_To_CR), 1, CR0_State, 0, 0, Allowed);
   Check (D.Outcome = Not_Modelled, "CR8");
   D := Decide (Q (4, Clear_TS), 0, CR0_State, 0, 0, Allowed);
   Check (D.Outcome = Not_Modelled, "CLTS on CR4");
   --  CR4: VMXE refused, a bit outside FIXED1 refused, PAE kept in long mode.
   D := Decide (Q (4, Move_To_CR), CR4_PAE or CR4_VMXE, CR0_State, CR4_PAE, EFER_LME or EFER_LMA, Allowed);
   Check (D.Outcome = General_Protection, "CR4.VMXE");
   D := Decide (Q (4, Move_To_CR), CR4_PAE or 2 ** 40, CR0_State, CR4_PAE, EFER_LME or EFER_LMA, Allowed);
   Check (D.Outcome = General_Protection, "CR4 outside FIXED1");
   D := Decide (Q (4, Move_To_CR), 0, CR0_State, CR4_PAE, EFER_LME or EFER_LMA, Allowed);
   Check (D.Outcome = General_Protection, "CR4.PAE off in long mode");
   D := Decide (Q (4, Move_To_CR), CR4_PAE or CR4_PGE, CR0_State, CR4_PAE, EFER_LME or EFER_LMA, Allowed);
   Check (D.Outcome = Applied and then D.CR4 = (CR4_PAE or CR4_PGE) and then D.Flush
          and then D.CR0 = CR0_State, "CR4.PGE on");

   --  Every combination of CR0's eleven defined bits, LME and PAE, against
   --  a reference written from the SDM's #GP list.
   declare
      Bits : constant array (0 .. 10) of Unsigned_64 :=
        [CR0_PE, CR0_MP, CR0_EM, CR0_TS, CR0_ET, CR0_NE, CR0_WP, CR0_AM, CR0_NW, CR0_CD, CR0_PG];
      Value : Unsigned_64;
   begin
      for Pattern in 0 .. 2 ** 11 - 1 loop
         Value := 0;
         for B in Bits'Range loop
            if (Pattern / 2 ** B) mod 2 = 1 then
               Value := Value or Bits (B);
            end if;
         end loop;
         for LME in Boolean loop
            for PAE in Boolean loop
               declare
                  EFER : constant Unsigned_64 := (if LME then EFER_LME else 0);
                  CR4 : constant Unsigned_64 := (if PAE then CR4_PAE else 0);
                  Faults : constant Boolean :=
                    ((Value and CR0_PG) /= 0 and then (Value and CR0_PE) = 0)
                    or else ((Value and CR0_NW) /= 0 and then (Value and CR0_CD) = 0)
                    or else ((Value and CR0_PG) /= 0 and then LME and then not PAE);
                  Long : constant Boolean := (Value and CR0_PG) /= 0 and then LME;
               begin
                  D := Decide (Q (0, Move_To_CR), Value, Reset_CR0, CR4, EFER, Allowed);
                  if Faults then
                     Check (D.Outcome = General_Protection, "reference #GP" & Pattern'Image);
                  else
                     Check (D.Outcome = Applied and then D.CR0 = (Value or CR0_ET)
                            and then D.CR4 = CR4
                            and then ((D.EFER and EFER_LMA) /= 0) = Long,
                            "reference applied" & Pattern'Image);
                  end if;
               end;
            end loop;
         end loop;
      end loop;
   end;

   Put_Line ("vmx-cr-access:" & Checks'Image & " checks," & Failed'Image & " failed");
   if Failed /= 0 then
      raise Program_Error;
   end if;
end Main;
