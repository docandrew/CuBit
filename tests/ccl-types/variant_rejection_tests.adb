with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Types; use CCL.Types;
with CCL.Types.Encoding;
with CCL.VM; use CCL.VM;
with CCL.Format;
with CCL.Ownership;
with CCL.Bounded_Stacks;

procedure Variant_Rejection_Tests is
   R : Registry;
   Ref, Other : Type_Reference;
   Defined_Result : Definition_Result;
   Good, P : Program;
   Checked : Validated_Program;
   Error : Validation_Error;
   Data, Bad : CCL.Format.Byte_Array;
   Length : CCL.Format.Module_Length;
   Format_Error : CCL.Format.Format_Error;
   Limits : CCL.Format.Resource_Limits;
   use type CCL.Format.Format_Error;
   type Tiny_Index is mod 4;
   package Stacks is new CCL.Bounded_Stacks (Tiny_Index, Integer, 0);
   Stack : Stacks.Stack;
   Stack_Result : Stacks.Operation_Result;
   Value : Integer;
   use type Stacks.Operation_Result;
   procedure Reject (Label : String; Expected : Validation_Error) is
   begin
      Verify (P, Checked, Error);
      if Error /= Expected then Put_Line (Label & ": " & Error'Image & " expected " & Expected'Image); end if;
      pragma Assert (Error = Expected and not Is_Valid (Checked));
   end Reject;
   procedure Corrupt (Position : CCL.Format.Byte_Index; Byte : Unsigned_8) is
   begin
      Bad := Data; Bad (Position) := Byte;
      CCL.Format.Decode (Bad, Length, Checked, Limits, Format_Error, Error);
      if Format_Error = CCL.Format.Format_Valid then Put_Line ("accepted corrupt byte" & Position'Image); end if;
      pragma Assert (Format_Error /= CCL.Format.Format_Valid and not Is_Valid (Checked));
   end Corrupt;
   Schema : constant Natural := CCL.Format.HEADER_SIZE;
   Dispatch : constant Natural := Schema + CCL.Format.DATA_TYPE_SIZE * 2;
begin
   -- Exercise wrapped/full ADT indices as well as underflow.
   Stacks.Peek_At (Stack, 0, Value, Stack_Result);
   pragma Assert (Stack_Result = Stacks.Stack_Empty);
   for I in 1 .. 4 loop
      Stacks.Push (Stack, I, Stack_Result);
      pragma Assert (Stack_Result = Stacks.Stack_Ok);
   end loop;
   for I in 0 .. 3 loop
      Stacks.Peek_At (Stack, Unsigned_32 (I), Value, Stack_Result);
      pragma Assert (Stack_Result = Stacks.Stack_Ok and Value = 4 - I);
   end loop;
   Stacks.Peek_At (Stack, 4, Value, Stack_Result);
   pragma Assert (Stack_Result = Stacks.Stack_Empty);
   Stacks.Peek_At (Stack, Unsigned_32'Last, Value, Stack_Result);
   pragma Assert (Stack_Result = Stacks.Stack_Empty);
   Stacks.Pop (Stack, Value, Stack_Result);
   Stacks.Push (Stack, 42, Stack_Result);
   Stacks.Peek_At (Stack, 0, Value, Stack_Result);
   pragma Assert (Stack_Result = Stacks.Stack_Ok and Value = 42);

   Define (R, (Identifier => Named ("Reading"), Form => Sum, Count => 2,
               Parts => [1 => (Named ("Value"), Integer_Type),
                         2 => (Named ("Unavailable"), Unit_Type), others => <>]), Ref, Defined_Result);
   pragma Assert (Defined_Result = Defined);
   Define (R, (Identifier => Named ("Other"), Form => Sum, Count => 1,
               Parts => [1 => (Named ("Nope"), Unit_Type), others => <>]), Other, Defined_Result);
   pragma Assert (Defined_Result = Defined);
   Good.Data_Types := R;
   Good.Length := 7;
   Good.Matches_Length := 1;
   Good.Matches (0) := (Data_Type => Ref, Targets => [1 => 3, 2 => 5, others => 0]);
   Good.Code (0) := (Op => Push_Integer, Immediate => 42, others => <>);
   Good.Code (1) := (Op => Make_Variant, Data_Type => Ref, Alternative => 1, others => <>);
   Good.Code (2) := (Op => Switch_Variant, Immediate => 0, others => <>);
   Good.Code (3) := (Op => Copy_Stack, Immediate => 0, others => <>);
   Good.Code (4) := (Op => Jump, Target => 6, others => <>);
   Good.Code (5) := (Op => Push_Integer, others => <>);
   Good.Code (6) := (Op => Halt, others => <>);
   -- Both alternatives must produce identical stack shapes at a join.
   P := Good; Reject ("payload not removed", Inconsistent_Stack);
   Good.Code (3) := (Op => Jump, Target => 4, others => <>);
   Verify (Good, Checked, Error);
   pragma Assert (Error = Valid);
   P := Good; P.Code (1).Alternative := 3; Reject ("invalid tag", Invalid_Data_Type);
   P := Good; P.Code (1).Data_Type := Invalid_Type; Reject ("missing schema", Invalid_Data_Type);
   P := Good; P.Code (0) := (Op => Push_Boolean, others => <>); Reject ("wrong payload", Type_Mismatch);
   P := Good; P.Matches (0).Data_Type := Other; P.Matches (0).Targets (2) := 0;
   Reject ("wrong nominal dispatch", Type_Mismatch);
   P := Good; P.Matches (0).Targets (2) := 0; Reject ("missing alternative", Backward_Jump);
   P := Good; P.Matches (0).Targets (1) := 7; Reject ("target outside code", Invalid_Jump_Target);
   P := Good; P.Matches (0).Targets (3) := 3; Reject ("unused target", Invalid_Match);
   P := Good; P.Code (2).Immediate := -1; Reject ("negative table", Invalid_Match);
   P := Good; P.Code (2).Immediate := 1; Reject ("table outside count", Invalid_Match);
   P := Good; P.Code (3) := (Op => Not_Boolean, others => <>); Reject ("reinterpret payload", Type_Mismatch);
   P := Good; P.Code (3) := (Op => Equal_Variant, Data_Type => Ref, others => <>);
   Reject ("compare payload sums as tags", Invalid_Data_Type);
   P := Good; P.Code (3) := (Op => Copy_Stack, Immediate => 1, others => <>);
   Reject ("copy below stack", Stack_Underflow);
   P := Good; P.Code (3) := (Op => Drop_Under_Top, others => <>);
   Reject ("missing retained result", Stack_Underflow);
   P := Good; P.Code (0).Data_Type := Ref; Reject ("metadata on scalar", Invalid_Data_Type);
   -- Nominal identities also participate in stack joins.
   P := (others => <>); P.Data_Types := R; P.Length := 6;
   P.Code (0) := (Op => Push_Boolean, others => <>);
   P.Code (1) := (Op => Jump_If_False, Target => 4, others => <>);
   P.Code (2) := (Op => Make_Variant, Data_Type => Ref, Alternative => 2, others => <>);
   P.Code (3) := (Op => Jump, Target => 5, others => <>);
   P.Code (4) := (Op => Make_Variant, Data_Type => Other, Alternative => 1, others => <>);
   Reject ("nominal branch join", Inconsistent_Stack);
   -- Moving a restricted value onto the stack never grants a copy permission.
   P := (others => <>); P.Data_Types := R; P.Length := 3;
   P.Types_Length := 1; P.Types (0).Mode := CCL.Ownership.Must_Handle;
   P.Locals_Length := 1;
   P.Code (0) := (Op => Move_Local, others => <>);
   P.Code (1) := (Op => Copy_Stack, others => <>);
   Reject ("copy transferred resource", Invalid_Ownership);
   P.Code (1) := (Op => Make_Variant, Data_Type => Ref, Alternative => 1, others => <>);
   Reject ("box transferred resource", Invalid_Ownership);
   P.Code (1) := (Op => Drop, others => <>);
   Reject ("discard transferred resource", Invalid_Ownership);
   P.Length := 5;
   P.Code (1) := (Op => Push_Integer, Immediate => 0, others => <>);
   P.Code (2) := (Op => Add_Integer, others => <>);
   P.Code (3) := (Op => Copy_Stack, others => <>);
   Reject ("launder resource through arithmetic", Invalid_Ownership);
   P.Code (2) := (Op => Drop_Under_Top, others => <>);
   Reject ("discard under result", Invalid_Ownership);
   P := (others => <>); P.Length := MAX_STACK_DEPTH + 2;
   for I in 0 .. MAX_STACK_DEPTH - 1 loop
      P.Code (Instruction_Index (I)) := (Op => Push_Integer, others => <>);
   end loop;
   P.Code (MAX_STACK_DEPTH) := (Op => Copy_Stack, others => <>);
   Reject ("copy into full stack", Stack_Overflow);
   -- Every match alternative must discharge the same ownership obligations.
   P := Good; P.Length := 8;
   P.Types_Length := 1; P.Types (0).Mode := CCL.Ownership.Must_Handle;
   P.Types (0).Dispositions_Length := 1;
   P.Types (0).Dispositions (0) := (Verb => 1, others => <>);
   P.Locals_Length := 1;
   P.Code (3) := (Op => Apply_Local_Disposition, Verb => 1, others => <>);
   P.Code (4) := (Op => Jump, Target => 7, others => <>);
   P.Code (5) := (Op => Apply_Local_Disposition, Verb => 1, others => <>);
   P.Code (6) := (Op => Push_Integer, others => <>);
   Verify (P, Checked, Error);
   pragma Assert (Error = Valid);
   P.Code (5) := (Op => Jump, Target => 6, others => <>);
   Reject ("unhandled resource in one match arm", Invalid_Ownership);
   -- Check canonical schema records and match tables in untrusted modules.
   CCL.Format.Encode (Good, (1024, 4096, 1), Data, Length, Format_Error, Error);
   pragma Assert (Format_Error = CCL.Format.Format_Valid);
   Corrupt (CCL.Format.VERSION_OFFSET, 3);
   Corrupt (CCL.Format.DATA_TYPE_COUNT_OFFSET, 33);
   Corrupt (CCL.Format.MATCH_COUNT_OFFSET, 17);
   Corrupt (Schema, 33);
   Corrupt (Schema + 8, 1); -- nonzero name padding
   Corrupt (Schema + CCL.Types.Encoding.Shape_Offset, 0);
   Corrupt (Schema + CCL.Types.Encoding.Count_Offset, 17);
   Corrupt (Schema + CCL.Types.Encoding.Reserved_Offset, 1);
   Corrupt (Schema + CCL.Types.Encoding.Parts_Offset + CCL.Types.Encoding.Name_Size, 255);
   Corrupt (Schema + CCL.Types.Encoding.Parts_Offset + CCL.Types.Encoding.Name_Size, Unsigned_8 (Ref));
   Corrupt (Schema + CCL.Types.Encoding.Parts_Offset + CCL.Types.Encoding.Name_Size, Unsigned_8 (String_Type));
   Corrupt (Schema + CCL.Types.Encoding.Parts_Offset + 2 * CCL.Types.Encoding.Part_Size, 1);
   Corrupt (Dispatch + CCL.Format.MATCH_TYPE_OFFSET, 255);
   Corrupt (Dispatch + CCL.Format.MATCH_RESERVED_OFFSET, 1);
   Corrupt (Dispatch + CCL.Format.MATCH_TARGETS_OFFSET, 0);
   Corrupt (Dispatch + CCL.Format.MATCH_TARGETS_OFFSET + 1, 1);
   for Last in 0 .. Length - 1 loop
      CCL.Format.Decode (Data, Last, Checked, Limits, Format_Error, Error);
      pragma Assert (Format_Error /= CCL.Format.Format_Valid and not Is_Valid (Checked));
   end loop;
   Put_Line ("CCLB variants: hostile schemas, nominal joins, exhaustive dispatch, ownership and stack bounds PASS");
end Variant_Rejection_Tests;
