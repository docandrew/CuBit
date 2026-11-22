--  KERN-004: hosted tests of the platform power object's proved units:
--  Firmware_Tables.Power_Registers (FADT decode), Platform_Power_Policy
--  (request decode, admission, the register plan, PM1 and sleep-control
--  values). Exhaustive where the domain is small.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Tables; use Firmware_Tables;
with Firmware_Tables.Power_Registers;
with Platform_Power_ABI;
with Platform_Power_Policy;

procedure Power_Tests is
   package PR  renames Firmware_Tables.Power_Registers;
   package ABI renames Platform_Power_ABI;
   package Pol renames Platform_Power_Policy;
   use type PR.Register, PR.Description, PR.Sleep_Model, PR.Register_Space;
   use type ABI.Refusal, ABI.Power_State, Pol.Step;

   Failures : Natural := 0;
   Checks   : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   ---------------------------------------------------------------------------
   --  A FADT built field by field (ACPI 6.5, Table 5.9).
   ---------------------------------------------------------------------------
   Revision_3_Length : constant := 244;
   Revision_6_Length : constant := 276;
   Reset_Supported   : constant Unsigned_32 := 2 ** 10;
   Reduced_ACPI      : constant Unsigned_32 := 2 ** 20;
   IO_Space     : constant Byte := 1;
   Memory_Space : constant Byte := 0;
   PCI_Space    : constant Byte := 2;

   subtype Table is Bytes (1 .. Revision_6_Length);

   procedure Put (T : in out Table; Offset : Natural; Value : Unsigned_64; Width : Positive) is
   begin
      for I in 0 .. Width - 1 loop
         T (T'First + Offset + I) := Byte (Shift_Right (Value, 8 * I) and 16#FF#);
      end loop;
   end Put;

   procedure Put_GAS (T : in out Table; Offset : Natural; Space, Width, Shift, Access_Size : Byte;
                      Address : Unsigned_64) is
   begin
      T (T'First + Offset) := Space;
      T (T'First + Offset + 1) := Width;
      T (T'First + Offset + 2) := Shift;
      T (T'First + Offset + 3) := Access_Size;
      Put (T, Offset + 4, Address, 8);
   end Put_GAS;

   procedure Seal (T : in out Table; Length : Positive) is
      Sum : Byte := 0;
   begin
      T (1 .. 4) := [Character'Pos ('F'), Character'Pos ('A'), Character'Pos ('C'), Character'Pos ('P')];
      Put (T, 4, Unsigned_64 (Length), 4);
      T (T'First + 8) := 6;
      T (T'First + 9) := 0;
      for I in 1 .. Length loop
         Sum := Sum + T (I);
      end loop;
      T (T'First + 9) := 0 - Sum;
   end Seal;

   --  QEMU q35: PM1a_CNT 0x604 (legacy and extended), no PM1b, reset 0xCF9 = 6.
   function Q35 (Length : Positive := Revision_3_Length) return Table is
      T : Table := [others => 0];
   begin
      Put (T, 64, 16#604#, 4);
      T (T'First + 89) := 2;
      Put (T, 112, Unsigned_64 (Reset_Supported), 4);
      Put_GAS (T, 116, IO_Space, 8, 0, 1, 16#CF9#);
      T (T'First + 128) := 6;
      Put_GAS (T, 172, IO_Space, 16, 0, 2, 16#604#);
      Seal (T, Length);
      return T;
   end Q35;

   function Decode (T : Table; Length : Positive := Revision_3_Length) return PR.Description is
     (PR.Decode (T (1 .. Length)));

   procedure Test_Registers is
      D : PR.Description;
      T : Table;
   begin
      D := Decode (Q35);
      Check (D.Model = PR.PM1A_Only, "q35: PM1a only");
      Check (D.PM1A_Control = (True, PR.System_IO, 16#604#, 2), "q35: PM1a 0x604");
      Check (D.Reset = (True, PR.System_IO, 16#CF9#, 1) and D.Reset_Value = 6, "q35: reset 0xCF9 = 6");
      Check (D.SMI_Command = 0, "q35 fixture: no SMI command port");

      --  The IdeaPad Slim 3i (captured FACP): SMI_CMD 0xB2, and the reset
      --  register is that port, reset value 0xFB (a software-SMI reset).
      T := Q35; Put (T, 48, 16#B2#, 4); Put_GAS (T, 116, IO_Space, 8, 0, 1, 16#B2#);
      T (T'First + 128) := 16#FB#; Seal (T, Revision_3_Length);
      D := Decode (T);
      Check (D.SMI_Command = 16#B2# and D.Reset = (True, PR.System_IO, 16#B2#, 1)
             and D.Reset_Value = 16#FB#, "IdeaPad: SMI_CMD 0xB2, reset 0xFB to 0xB2");
      --  A SMI_CMD past the port space is not a port.
      T := Q35; Put (T, 48, 16#1_0000#, 4); Seal (T, Revision_3_Length);
      Check (Decode (T).SMI_Command = 0, "SMI_CMD past 0xFFFF ignored");

      --  The extended form wins when it names a register.
      T := Q35; Put_GAS (T, 172, IO_Space, 16, 0, 2, 16#B004#); Seal (T, Revision_3_Length);
      Check (Decode (T).PM1A_Control.Address = 16#B004#, "X_PM1a preferred");
      --  A zero extended form falls back to the legacy port.
      T := Q35; Put_GAS (T, 172, 0, 0, 0, 0, 0); Seal (T, Revision_3_Length);
      Check (Decode (T).PM1A_Control = (True, PR.System_IO, 16#604#, 2), "legacy PM1a when X is zero");
      --  An extended form of the wrong width falls back too.
      T := Q35; Put_GAS (T, 172, IO_Space, 32, 0, 3, 16#B004#); Seal (T, Revision_3_Length);
      Check (Decode (T).PM1A_Control.Address = 16#604#, "X_PM1a of 32 bits not used");
      --  A nonzero bit offset is never a whole register.
      T := Q35; Put_GAS (T, 172, IO_Space, 16, 1, 2, 16#B004#); Seal (T, Revision_3_Length);
      Check (Decode (T).PM1A_Control.Address = 16#604#, "X_PM1a with a bit offset not used");
      --  An ACPI 1.0 table (116 bytes): legacy fields only, no reset.
      T := Q35; Seal (T, 116);
      D := Decode (T, 116);
      Check (D.PM1A_Control = (True, PR.System_IO, 16#604#, 2) and not D.Reset.Present,
             "ACPI 1.0 FADT: legacy PM1a, no reset register");
      --  PM1_CNT_LEN below two: no legacy control block.
      T := Q35; T (T'First + 89) := 1; Put_GAS (T, 172, 0, 0, 0, 0, 0); Seal (T, Revision_3_Length);
      Check (Decode (T).Model = PR.No_Sleep_Registers, "PM1_CNT_LEN 1: no PM1a");
      --  PM1b from the legacy field.
      T := Q35; Put (T, 68, 16#704#, 4); Seal (T, Revision_3_Length);
      D := Decode (T);
      Check (D.Model = PR.PM1A_And_B and D.PM1B_Control = (True, PR.System_IO, 16#704#, 2),
             "PM1a and PM1b");
      --  A 16-bit port must fit below 64 KiB.
      T := Q35; Put (T, 64, 16#FFFF#, 4); Put_GAS (T, 172, 0, 0, 0, 0, 0); Seal (T, Revision_3_Length);
      Check (Decode (T).Model = PR.No_Sleep_Registers, "PM1a at 0xFFFF refused");
      T := Q35; Put (T, 64, 16#FFFE#, 4); Put_GAS (T, 172, 0, 0, 0, 0, 0); Seal (T, Revision_3_Length);
      Check (Decode (T).PM1A_Control.Address = 16#FFFE#, "PM1a at 0xFFFE kept");
      T := Q35; Put_GAS (T, 172, IO_Space, 16, 0, 2, 16#1_0000#); Seal (T, Revision_3_Length);
      Check (Decode (T).PM1A_Control.Address = 16#604#, "X_PM1a beyond the port space not used");
      --  Memory space is described, not refused here.
      T := Q35; Put_GAS (T, 172, Memory_Space, 16, 0, 2, 16#FED0_0000#); Seal (T, Revision_3_Length);
      Check (Decode (T).PM1A_Control = (True, PR.System_Memory, 16#FED0_0000#, 2), "X_PM1a in memory");
      --  No reset without RESET_REG_SUP.
      T := Q35; Put (T, 112, 0, 4); Seal (T, Revision_3_Length);
      Check (not Decode (T).Reset.Present, "no reset register without RESET_REG_SUP");
      --  A reset register in PCI configuration space: present, Other_Space.
      T := Q35; Put_GAS (T, 116, PCI_Space, 8, 0, 1, 16#CF9#); Seal (T, Revision_3_Length);
      Check (Decode (T).Reset.Space = PR.Other_Space, "PCI reset register is other space");
      --  Reset of the wrong width.
      T := Q35; Put_GAS (T, 116, IO_Space, 16, 0, 2, 16#CF9#); Seal (T, Revision_3_Length);
      Check (not Decode (T).Reset.Present, "16-bit reset register refused");
      --  Hardware-reduced: the sleep control register, never PM1.
      T := Q35; Put (T, 112, Unsigned_64 (Reduced_ACPI or Reset_Supported), 4);
      Put_GAS (T, 244, IO_Space, 8, 0, 1, 16#1804#); Seal (T, Revision_6_Length);
      D := Decode (T, Revision_6_Length);
      Check (D.Model = PR.Hardware_Reduced and D.Sleep_Control = (True, PR.System_IO, 16#1804#, 1)
             and not D.PM1A_Control.Present, "hardware-reduced sleep control");
      --  Hardware-reduced without a sleep control register (short table).
      T := Q35; Put (T, 112, Unsigned_64 (Reduced_ACPI), 4); Seal (T, Revision_3_Length);
      Check (Decode (T).Model = PR.No_Sleep_Registers, "reduced without sleep control");
      --  Unadmitted tables.
      T := Q35; T (T'First + 9) := T (T'First + 9) + 1;
      Check (Decode (T) = PR.No_Description, "bad checksum: nothing");
      T := Q35; T (1) := Character'Pos ('X'); Seal (T, Revision_3_Length);
      T (1) := Character'Pos ('X');
      Check (Decode (T) = PR.No_Description, "wrong signature: nothing");
      T := Q35; Seal (T, 100);
      Check (Decode (T, 100) = PR.No_Description, "truncated before Flags: nothing");
      Check (PR.Decode (Q35 (Revision_3_Length) (1 .. 200)) = PR.No_Description,
             "declared length beyond the data: nothing");
   end Test_Registers;

   ---------------------------------------------------------------------------
   --  Requests.
   ---------------------------------------------------------------------------
   Enter : constant Unsigned_32 := ABI.Operation'Enum_Rep (ABI.Enter_State);

   function Code (S : ABI.Power_State) return Unsigned_64 is (ABI.Power_State'Enum_Rep (S));

   procedure Test_Decode is
      R : Pol.Request;
   begin
      for State_Word in Unsigned_64'(0) .. 20 loop
         for A in Unsigned_64'(0) .. 9 loop
            for B in Unsigned_64'(0) .. 9 loop
               R := Pol.Decode (True, Enter, ABI.Request_Words, State_Word, A, B, 0);
               declare
                  Known : constant Boolean :=
                    (for some S in ABI.Power_State => Code (S) = State_Word);
                  Is_Reset : constant Boolean := State_Word = Code (ABI.Reset);
                  Expect_Valid : constant Boolean :=
                    Known and then A <= 7 and then B <= 7
                    and then (if Is_Reset then A = 0 and B = 0);
               begin
                  Check (R.Valid = Expect_Valid, "decode validity state" & State_Word'Image &
                         A'Image & B'Image);
                  if R.Valid then
                     Check (Code (R.State) = State_Word and R.Type_A = A and R.Type_B = B,
                            "decode fields");
                  elsif not Known then
                     Check (R.Why = ABI.Malformed, "unknown state is Malformed");
                  else
                     Check (R.Why = ABI.Sleep_Type_Out_Of_Range, "wide sleep type refused");
                  end if;
               end;
            end loop;
         end loop;
      end loop;
      R := Pol.Decode (False, Enter, ABI.Request_Words, Code (ABI.S5), 0, 0, 0);
      Check (not R.Valid and then R.Why = ABI.Not_Permitted, "no Write: Not_Permitted");
      R := Pol.Decode (True, Enter + 1, ABI.Request_Words, Code (ABI.S5), 0, 0, 0);
      Check (not R.Valid and then R.Why = ABI.Unknown_Operation, "unknown label");
      R := Pol.Decode (True, 0, ABI.Request_Words, Code (ABI.S5), 0, 0, 0);
      Check (not R.Valid and then R.Why = ABI.Unknown_Operation, "label 0");
      for L in Unsigned_8'(0) .. 4 loop
         R := Pol.Decode (True, Enter, L, Code (ABI.S5), 0, 0, 0);
         Check (R.Valid = (L = ABI.Request_Words), "length" & L'Image);
      end loop;
      R := Pol.Decode (True, Enter, ABI.Request_Words, Code (ABI.S5), 0, 0, 1);
      Check (not R.Valid and then R.Why = ABI.Malformed, "nonzero unused word");
      R := Pol.Decode (True, Enter, ABI.Request_Words, Code (ABI.S5), Unsigned_64'Last, 0, 0);
      Check (not R.Valid and then R.Why = ABI.Sleep_Type_Out_Of_Range, "huge SLP_TYPa");
      R := Pol.Decode (True, Enter, ABI.Request_Words, Unsigned_64'Last, 0, 0, 0);
      Check (not R.Valid and then R.Why = ABI.Malformed, "huge state");
   end Test_Decode;

   ---------------------------------------------------------------------------
   --  Admission and the plan.
   ---------------------------------------------------------------------------
   function Request (S : ABI.Power_State; A, B : Unsigned_64 := 0) return Pol.Request is
     (Pol.Decode (True, Enter, ABI.Request_Words, Code (S), A, B, 0));

   procedure Test_Plan is
      use type Pol.Step_List;
      Q : constant PR.Description := Decode (Q35);
      T : Table;
      Pair, Reduced, Memory_Reduced, None : PR.Description;
      P : Pol.Plan_Type;
      A : Pol.Admission;
   begin
      T := Q35; Put (T, 68, 16#704#, 4); Seal (T, Revision_3_Length); Pair := Decode (T);
      T := Q35; Put (T, 112, Unsigned_64 (Reduced_ACPI), 4);
      Put_GAS (T, 244, IO_Space, 8, 0, 1, 16#1804#); Seal (T, Revision_6_Length);
      Reduced := Decode (T, Revision_6_Length);
      T := Q35; Put (T, 112, Unsigned_64 (Reduced_ACPI), 4);
      Put_GAS (T, 244, Memory_Space, 8, 0, 1, 16#FE00_0000#); Seal (T, Revision_6_Length);
      Memory_Reduced := Decode (T, Revision_6_Length);
      T := Q35; Put (T, 64, 0, 4); Put (T, 112, 0, 4); Put_GAS (T, 172, 0, 0, 0, 0, 0);
      Seal (T, Revision_3_Length); None := Decode (T);

      for S in ABI.S3 .. ABI.S4 loop
         A := Pol.Admit (Request (S, 5, 5), Q);
         Check (not A.Admitted and then A.Why = ABI.Not_Yet, "S3/S4 Not_Yet");
      end loop;
      Check (Pol.Admit (Request (ABI.S5), Q).Admitted, "q35 S5 admitted");
      Check (Pol.Admit (Request (ABI.Reset), Q).Admitted, "q35 Reset admitted");
      A := Pol.Admit (Request (ABI.S5), None);
      Check (not A.Admitted and then A.Why = ABI.No_Sleep_Registers, "no registers: S5 refused");
      A := Pol.Admit (Request (ABI.Reset), None);
      Check (not A.Admitted and then A.Why = ABI.No_Reset_Register, "no reset register");
      A := Pol.Admit (Request (ABI.S5), Memory_Reduced);
      Check (not A.Admitted and then A.Why = ABI.Unsupported_Register, "memory sleep control refused");

      P := Pol.Plan (Request (ABI.S5, 5), Q);
      Check (P.Count = 6 and then P.Steps (1 .. 6) =
               [Pol.Drain_Console, Pol.Park_CPUs, Pol.Read_A, Pol.Stage_A, Pol.Enable_A, Pol.Settle],
             "q35 S5 plan");
      P := Pol.Plan (Request (ABI.S5, 5, 6), Pair);
      Check (P.Count = 9 and then P.Steps (1 .. 9) =
               [Pol.Drain_Console, Pol.Park_CPUs, Pol.Read_B, Pol.Read_A, Pol.Stage_B,
                Pol.Stage_A, Pol.Enable_B, Pol.Enable_A, Pol.Settle],
             "pair S5 plan: PM1b before PM1a");
      P := Pol.Plan (Request (ABI.S5, 5), Reduced);
      Check (P.Count = 4 and then P.Steps (1 .. 4) =
               [Pol.Drain_Console, Pol.Park_CPUs, Pol.Reduced_Enable, Pol.Settle],
             "reduced S5 plan");
      P := Pol.Plan (Request (ABI.Reset), Q);
      Check (P.Count = 4 and then P.Steps (1 .. 4) =
               [Pol.Drain_Console, Pol.Park_CPUs, Pol.Write_Reset, Pol.Settle],
             "reset plan");
   end Test_Plan;

   ---------------------------------------------------------------------------
   --  Register values, exhaustive.
   ---------------------------------------------------------------------------
   procedure Test_Values is
      V : Unsigned_16;
   begin
      for Observed in Unsigned_16 loop
         for Kind in Pol.Sleep_Type loop
            for Enable in Boolean loop
               V := Pol.PM1_Value (Observed, Kind, Enable);
               --  Reference: ACPI 4.8.3.2.1. Keep everything but GBL_RLS (2),
               --  SLP_TYP (10 .. 12) and SLP_EN (13).
               if V /= ((Observed and 16#C3FB#) or Shift_Left (Unsigned_16 (Kind), 10)
                        or (if Enable then 16#2000# else 0))
               then
                  Check (False, "PM1 value" & Observed'Image & Kind'Image & Enable'Image);
               end if;
            end loop;
         end loop;
      end loop;
      Check (True, "PM1 values: all 1,048,576");
      for Kind in Pol.Sleep_Type loop
         Check (Pol.Reduced_Value (Kind) = Shift_Left (Unsigned_8 (Kind), 2) + 16#20#,
                "reduced value" & Kind'Image);
      end loop;
   end Test_Values;

begin
   Test_Registers;
   Test_Decode;
   Test_Plan;
   Test_Values;
   if Failures = 0 then
      Put_Line ("platform power: PASS checks=" & Checks'Image);
   else
      Put_Line ("platform power: FAIL" & Failures'Image & " of" & Checks'Image);
      raise Program_Error;
   end if;
end Power_Tests;
