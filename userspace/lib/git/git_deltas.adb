------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The delta applier (see the spec).
------------------------------------------------------------------------------
pragma Ada_2022;

package body Git_Deltas with SPARK_Mode is
   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;
   use type Interfaces.Unsigned_64;
   CONTINUATION : constant := 16#80#;
   GROUP : constant := 128;
   GROUP_BITS : constant := 7;
   --  gitformat-pack: copy arguments 0 .. 3 are offset bytes, 4 .. 6 size
   --  bytes; a size of zero means 65536.
   OFFSET_ARGUMENTS : constant := 4;
   ZERO_SIZE_MEANS : constant := 16#10000#;
   BYTE_BITS : constant := 8;

   procedure Start (A : out Applier; Base_Bytes : Byte_Count; Ceiling : Byte_Count) is
   begin
      A := (Base_Bytes => Base_Bytes, Ceiling => Ceiling, others => <>);
   end Start;

   procedure Apply
     (A : in out Applier;
      Base : Byte_Array;
      Input : Byte_Array;
      In_Next : in out Positive;
      Output : in out Byte_Array;
      Out_Next : in out Positive;
      Result : out Progress)
   is
      procedure Refuse (Why : Refusal) with
        Pre => Why /= No_Refusal,
        Post => A = (A'Old with delta Phase => Failed_Phase, Why => Why) and then Result = Refused,
        Global => (In_Out => A, Output => Result)
      is
      begin
         A.Phase := Failed_Phase;
         A.Why := Why;
         Result := Refused;
      end Refuse;
   begin
      Result := Need_Input;
      if A.Phase = Failed_Phase then
         Result := Refused;
         return;
      end if;
      loop
         pragma Loop_Invariant (Valid (A) and then not Failed (A));
         pragma Loop_Invariant (A.Base_Bytes = A.Base_Bytes'Loop_Entry and then A.Ceiling = A.Ceiling'Loop_Entry);
         pragma Loop_Invariant (In_Next in In_Next'Loop_Entry .. Input'Last + 1);
         pragma Loop_Invariant (Out_Next in Out_Next'Loop_Entry .. Output'Last + 1);
         pragma Loop_Invariant (A.Done = A.Done'Loop_Entry + Byte_Count (Out_Next - Out_Next'Loop_Entry));
         pragma Loop_Invariant
           (for all I in Output'Range =>
              (if I < Out_Next'Loop_Entry or else I >= Out_Next then Output (I) = Output'Loop_Entry (I)));
         case A.Phase is
            when Base_Size | Target_Size =>
               if In_Next > Input'Last then
                  return;
               end if;
               declare
                  C : constant Byte := Input (In_Next);
               begin
                  In_Next := In_Next + 1;
                  if A.Shift > MAXIMUM_SHIFT then
                     Refuse (Bad_Size);
                     return;
                  end if;
                  A.Value := A.Value or Interfaces.Shift_Left (U64 (C mod GROUP), A.Shift);
                  A.Shift := A.Shift + GROUP_BITS;
                  if C < CONTINUATION then
                     if A.Value > U64 (Byte_Count'Last) then
                        Refuse (Bad_Size);
                        return;
                     elsif A.Phase = Base_Size then
                        if Byte_Count (A.Value) /= A.Base_Bytes then
                           Refuse (Base_Size_Mismatch);
                           return;
                        end if;
                        A.Phase := Target_Size;
                     elsif Byte_Count (A.Value) > A.Ceiling then
                        Refuse (Result_Too_Large);
                        return;
                     else
                        A.Target := Byte_Count (A.Value);
                        pragma Assert (A.Target <= A.Ceiling);
                        A.Sizes := True;
                        A.Phase := Instruction;
                     end if;
                     A.Value := 0;
                     A.Shift := 0;
                  end if;
               end;

            when Instruction =>
               if In_Next > Input'Last then
                  return;
               elsif A.Done = A.Target then
                  Refuse (Extra_Data);
                  return;
               end if;
               declare
                  C : constant Byte := Input (In_Next);
               begin
                  In_Next := In_Next + 1;
                  if C >= CONTINUATION then
                     A.Op := C;
                     A.Argument := 0;
                     A.Copy_Offset := 0;
                     A.Copy_Size := 0;
                     A.Phase := Copy_Arguments;
                  elsif C = 0 then
                     Refuse (Reserved_Instruction);
                     return;
                  elsif Byte_Count (C) > A.Target - A.Done then
                     Refuse (Result_Overrun);
                     return;
                  else
                     A.Remaining := Byte_Count (C);
                     A.Phase := Inserting;
                  end if;
               end;

            when Copy_Arguments =>
               while A.Argument < COPY_ARGUMENT_BYTES
                 and then (Interfaces.Shift_Right (A.Op, A.Argument) and 1) = 0
               loop
                  pragma Loop_Invariant (Valid (A) and then A.Phase = Copy_Arguments);
                  pragma Loop_Invariant (A.Base_Bytes = A.Base_Bytes'Loop_Entry and then A.Ceiling = A.Ceiling'Loop_Entry
                                         and then A.Done = A.Done'Loop_Entry);
                  A.Argument := A.Argument + 1;
               end loop;
               if A.Argument = COPY_ARGUMENT_BYTES then
                  declare
                     Given : constant Byte_Count := Byte_Count (A.Copy_Size);
                     Size : constant Byte_Count := (if Given = 0 then ZERO_SIZE_MEANS else Given);
                     From : constant Byte_Count := Byte_Count (A.Copy_Offset);
                  begin
                     if From > A.Base_Bytes or else Size > A.Base_Bytes - From then
                        Refuse (Copy_Outside_Base);
                        return;
                     elsif Size > A.Target - A.Done then
                        Refuse (Result_Overrun);
                        return;
                     end if;
                     A.Source := From;
                     A.Remaining := Size;
                     A.Phase := Copying;
                  end;
               elsif In_Next > Input'Last then
                  return;
               else
                  if A.Argument < OFFSET_ARGUMENTS then
                     A.Copy_Offset := A.Copy_Offset
                       or Interfaces.Shift_Left (U32 (Input (In_Next)), BYTE_BITS * A.Argument);
                  else
                     A.Copy_Size := A.Copy_Size
                       or Interfaces.Shift_Left (U32 (Input (In_Next)), BYTE_BITS * (A.Argument - OFFSET_ARGUMENTS));
                  end if;
                  In_Next := In_Next + 1;
                  A.Argument := A.Argument + 1;
               end if;

            when Copying =>
               while A.Remaining > 0 and then Out_Next <= Output'Last loop
                  pragma Loop_Invariant (Valid (A) and then A.Phase = Copying);
                  pragma Loop_Invariant (A.Base_Bytes = A.Base_Bytes'Loop_Entry and then A.Ceiling = A.Ceiling'Loop_Entry);
                  pragma Loop_Invariant (Out_Next in Out_Next'Loop_Entry .. Output'Last);
                  pragma Loop_Invariant (A.Done = A.Done'Loop_Entry + Byte_Count (Out_Next - Out_Next'Loop_Entry));
                  pragma Loop_Invariant
                    (for all I in Output'Range =>
                       (if I < Out_Next'Loop_Entry or else I >= Out_Next then Output (I) = Output'Loop_Entry (I)));
                  Output (Out_Next) := Base (Base'First + Natural (A.Source));
                  Out_Next := Out_Next + 1;
                  A.Source := A.Source + 1;
                  A.Done := A.Done + 1;
                  A.Remaining := A.Remaining - 1;
                  exit when A.Remaining = 0;
               end loop;
               if A.Remaining = 0 then
                  A.Phase := Instruction;
               else
                  Result := Output_Full;
                  return;
               end if;

            when Inserting =>
               while A.Remaining > 0 and then Out_Next <= Output'Last and then In_Next <= Input'Last loop
                  pragma Loop_Invariant (Valid (A) and then A.Phase = Inserting);
                  pragma Loop_Invariant (A.Base_Bytes = A.Base_Bytes'Loop_Entry and then A.Ceiling = A.Ceiling'Loop_Entry);
                  pragma Loop_Invariant (Out_Next in Out_Next'Loop_Entry .. Output'Last);
                  pragma Loop_Invariant (In_Next in In_Next'Loop_Entry .. Input'Last);
                  pragma Loop_Invariant (A.Done = A.Done'Loop_Entry + Byte_Count (Out_Next - Out_Next'Loop_Entry));
                  pragma Loop_Invariant
                    (for all I in Output'Range =>
                       (if I < Out_Next'Loop_Entry or else I >= Out_Next then Output (I) = Output'Loop_Entry (I)));
                  Output (Out_Next) := Input (In_Next);
                  Out_Next := Out_Next + 1;
                  In_Next := In_Next + 1;
                  A.Done := A.Done + 1;
                  A.Remaining := A.Remaining - 1;
                  exit when A.Remaining = 0;
               end loop;
               if A.Remaining = 0 then
                  A.Phase := Instruction;
               elsif Out_Next > Output'Last then
                  Result := Output_Full;
                  return;
               else
                  return;
               end if;

            when Failed_Phase =>
               Result := Refused;
               return;
         end case;
      end loop;
   end Apply;

   procedure Finish (A : in out Applier; Result : out Progress) is
   begin
      if A.Phase = Failed_Phase then
         Result := Refused;
      elsif A.Phase = Instruction and then A.Done = A.Target then
         Result := Finished;
      else
         A.Phase := Failed_Phase;
         A.Why := Truncated;
         Result := Refused;
      end if;
   end Finish;
end Git_Deltas;
