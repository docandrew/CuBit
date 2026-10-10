with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Objects;
with AML_References;
with Test_Namespace;
with Timer_Verification;
procedure Copy_Value_Tests is
   package Owner renames Test_Namespace.Owned;
   use Owner;
   use type Integer_Value;
   use type AML_Objects.Allocation_Status;
   use type Test_Namespace.State;
   Clock : Timer_Verification.Clock_State;
   Foreign_Arena : Arena;
   Source, Foreign, Stale : AML_References.Object_Handle;
   Ref : Owner.Reference;
   Value, Copied : Datum;
   ID : AML_Objects.Object_ID;
   Allocation : AML_Objects.Allocation_Status;
   Status : Execution_Status;
   OK : Boolean;
   Before : Test_Namespace.State;
   Result : Execution_Result;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   Reset (Clock.Arena, OK); Check (OK);
   Reset (Foreign_Arena, OK); Check (OK);
   Append (Clock.Arena, [3,4], ID, Allocation); Check (Allocation = AML_Objects.Allocated);
   Make_Source (Clock.Arena, ID, Source, OK); Check (OK);
   Append (Foreign_Arena, [3,4], ID, Allocation); Check (Allocation = AML_Objects.Allocated);
   Make_Source (Foreign_Arena, ID, Foreign, OK); Check (OK);
   Read_Source (Clock.Arena, Source, Value, Status); Check (Status = Returned);
   -- Cached descriptor fields never control the copied object's actual type.
   Value.Object.Type_Code := 1;
   Value.Object.Size := Natural'Last;
   Clone_Value (Clock.Arena, Bits_64, Value, Copied, Status);
   Check (Status = Returned and then Copied.Value_Kind = Object_Datum
     and then Copied.Object.Type_Code = 3 and then Copied.Object.Size = 2
     and then Copied.Object.ID /= Value.Object.ID);
   Before := Snapshot (Clock.Arena);
   Value.Object.ID := Natural'Last;
   Clone_Value (Clock.Arena, Bits_64, Value, Copied, Status);
   Check (Status = Unsupported_Value and Snapshot (Clock.Arena) = Before);
   Value.Object.ID := AML_References.Source (Foreign);
   Value.Object.Source := Foreign;
   Clone_Value (Clock.Arena, Bits_64, Value, Copied, Status);
   Check (Status = Unsupported_Value and Snapshot (Clock.Arena) = Before);
   Make_Index (Clock.Arena, Source, 0, Ref, Status); Check (Status = Returned);
   Clone_Value (Clock.Arena, Bits_64, (Value_Kind => Reference_Datum, Ref => Ref), Copied, Status);
   Check (Status = Returned and then Copied.Value_Kind = Integer_Datum and then Copied.Number = 3);
   -- CopyObject replaces an argument holding an Index reference; the original
   -- byte must remain unchanged. This avoids ObjectType(DerefOf(...)), whose
   -- evaluator support is a separate incomplete feature.
   Timer_Verification.Run ([16#9D#,16#0A#,42,16#68#,16#A4#,16#68#], Bits_64,
     4, Clock, Result,
     [0 => (Value_Kind => Reference_Datum, Ref => Ref),
      others => (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer)], 1);
   Check (Result.Status = Returned and then Result.Value = 42);
   Resolve_Value (Clock.Arena, Ref, Value, Status);
   Check (Status = Returned and then Value.Value_Kind = Integer_Datum and then Value.Number = 3);
   Stale := Source;
   Reset (Clock.Arena, OK); Check (OK);
   Append (Clock.Arena, [3,4], ID, Allocation); Check (Allocation = AML_Objects.Allocated);
   Read_Source (Foreign_Arena, Foreign, Value, Status); Check (Status = Returned);
   Value.Object.Source := Stale;
   Before := Snapshot (Clock.Arena);
   Clone_Value (Clock.Arena, Bits_64, Value, Copied, Status);
   Check (Status = Unsupported_Value and Snapshot (Clock.Arena) = Before);
   Ada.Text_IO.Put_Line ("COPY-VALUE-CHECK: PASS" & Checks'Image);
end Copy_Value_Tests;
