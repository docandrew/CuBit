with Test_Namespace; use Test_Namespace;

with AML_Objects.Byte_References; use AML_Objects.Byte_References;
with AML_Objects;
with AML_Objects.Package_References;
with AML_Decode; use AML_Decode;
with Ada.Text_IO;
with AML_Execute; use AML_Execute;
with Ada.Command_Line;
with AML_References;
procedure Direct_Store_Tests is
 package Test_Owner renames Test_Namespace.Owned;
 use Test_Owner;
 use type Byte;
 use type Integer_Value;
 use type AML_Objects.Usage;
 A, B : Arena;
 Element_Ref, Alias_Ref : Test_Owner.Reference;
 Element_ID, Package_ID : AML_Objects.Object_ID;
 Element_Status : AML_Objects.Package_References.Result_Status;
 use type AML_Objects.Package_References.Result_Status;
 R, Later, String_Ref : Test_Owner.Reference;
 OK : Boolean; Result : Load_Status; Status : Result_Status; V : Byte;
 Copy : State := Empty;
 Checks : Natural := 0;
 subtype Case_Index is Positive range 1 .. 5;
 type Numbers is array (Case_Index) of Integer_Value;
 Inputs : constant Numbers := [0, 255, 256, 16#1_0000_1234#, Integer_Value'Last];
 Usage_Before : AML_Objects.Usage;
 Write_Result : Execution_Status;
 Resolved : Datum;
 Package_Fixture : constant Bytes := [16#08#,16#50#,16#4B#,16#47#,16#30#,16#12#,6,2,16#0A#,1,16#0A#,2];
 String_Fixture : constant Bytes := [16#08#,16#53#,16#54#,16#52#,16#30#,16#0D#,97,98,0];
 Second_Fixture : Bytes (1 .. 11);
 -- Name(BUF0, Buffer(2){3,4})
 Fixture : constant Bytes := [16#08#,16#42#,16#55#,16#46#,16#30#,
   16#11#,16#05#,16#0A#,16#02#,16#03#,16#04#];
 procedure Check (C : Boolean) is
 begin if not C then raise Program_Error with Natural'Image (Checks); end if;
 Checks := Checks + 1; end Check;
begin
 if Ada.Command_Line.Argument_Count = 1 then
   Check (Ada.Command_Line.Argument (1) /= "--negative-control");
 end if;
 Read (A, R, V, OK); Check (not OK and V = 0);
 Write (A, R, 99, OK); Check (not OK);
 Reset (A, OK); Check (OK);
 Load (A, Fixture, Bits_64, Result); Check (Result = Loaded);
 Copy := Snapshot (A); Check (Count (Copy) = 1);
 Make (A, Data_Object (Copy, 1), 0, R, Status); Check (Status = Ready);
 Read (A, R, V, OK); Check (OK and V = 3);
 Write (A, R, 9, OK); Check (OK);
 Check (Buffer_Data (Copy, 1) (1) = 3);
 Check (Buffer_Data (Snapshot (A), 1) (1) = 9);
 Second_Fixture := Fixture; Second_Fixture (5) := 16#31#;
 Load (A, Second_Fixture, Bits_64, Result); Check (Result = Loaded);
 Check (Count (Snapshot (A)) = 2);
 Read (A, R, V, OK); Check (OK and V = 9);
 Make (A, Data_Object (Snapshot (A), 2), 1, Later, Status); Check (Status = Ready);
 Write (A, Later, 44, OK); Check (OK);
 Read (A, R, V, OK); Check (OK and V = 9);
 Check (Buffer_Data (Snapshot (A), 2) (2) = 44);
 Check (Count (Copy) = 1 and Buffer_Data (Copy, 1) (1) = 3);
 Load (A, String_Fixture, Bits_64, Result); Check (Result = Loaded);
 Make (A, Data_Object (Snapshot (A), 3), 1, String_Ref, Status); Check (Status = Ready);
 Read (A, String_Ref, V, OK); Check (OK and V = 98);
 Write (A, String_Ref, 99, OK); Check (OK);
 Check (String_Data (Snapshot (A), 3) = "ac");
 Read (A, R, V, OK); Check (OK and V = 9);
 Reset (B, OK); Check (OK);
 Load (B, Fixture, Bits_64, Result); Check (Result = Loaded);
 Read (B, R, V, OK); Check (not OK and V = 0);
 Write (B, R, 99, OK); Check (not OK);
 Check (Buffer_Data (Snapshot (B), 1) (1) = 3);
 Load (A, Fixture, Bits_64, Result); Check (Result /= Loaded);
 Read (A, R, V, OK); Check (OK and V = 9);
 Reset (A, OK); Check (OK);
 Load (A, Fixture, Bits_64, Result); Check (Result = Loaded);
 Read (A, String_Ref, V, OK); Check (not OK and V = 0);
 Read (A, R, V, OK); Check (not OK);
 Write (A, R, 99, OK); Check (not OK);
 Check (Buffer_Data (Snapshot (A), 1) (1) = 3);
 Load (A, Package_Fixture, Bits_64, Result); Check (Result = Loaded);
 Package_ID := Data_Object (Snapshot (A), 2);
 Make_Element (A, Package_ID, 0, Element_Ref, Element_Status);
 Check (Element_Status = AML_Objects.Package_References.Ready);
 Alias_Ref := Element_Ref;
 Read_Element (A, Element_Ref, Element_ID, OK); Check (OK and Element_ID /= 0);
 Write_Element (A, Element_Ref, 0, OK); Check (OK);
 Read_Element (A, Alias_Ref, Element_ID, OK); Check (OK and Element_ID = 0);
 Write_Element (A, Element_Ref, AML_Objects.Object_ID'Last, OK); Check (not OK);
 Read_Element (A, Alias_Ref, Element_ID, OK); Check (OK and Element_ID = 0);
 Read (A, Element_Ref, V, OK); Check (not OK and V = 0);
 Write (A, Element_Ref, 99, OK); Check (not OK);
 Make (A, Data_Object (Snapshot (A), 1), 0, R, Status); Check (Status = Ready);
 Read_Element (A, R, Element_ID, OK); Check (not OK and Element_ID = 0);
 Write_Element (A, R, 0, OK); Check (not OK);
 Read_Element (B, Element_Ref, Element_ID, OK); Check (not OK and Element_ID = 0);
 Write_Element (B, Element_Ref, 0, OK); Check (not OK);
 Reset (A, OK); Check (OK);
 Load (A, Package_Fixture, Bits_64, Result); Check (Result = Loaded);
 Read_Element (A, Element_Ref, Element_ID, OK); Check (not OK and Element_ID = 0);
 Write_Element (A, Alias_Ref, 0, OK); Check (not OK);
 Store_Integer (A, Alias_Ref, 42, Write_Result); Check (Write_Result = Unsupported_Value);
 Make_Element (A, Data_Object (Snapshot (A), 1), 0, Element_Ref, Element_Status);
 Check (Element_Status = AML_Objects.Package_References.Ready);
 Copy := Snapshot (B);
 Store_Integer (B, Element_Ref, 42, Write_Result); Check (Write_Result = Unsupported_Value);
 Check (Snapshot (B) = Copy);
 Store_Integer (A, Element_Ref, Integer_Value'Last, Write_Result); Check (Write_Result = Returned);
 Resolve_Value (A, Element_Ref, Resolved, Write_Result);
 Check (Write_Result = Returned and then Resolved.Value_Kind = Integer_Datum and then Resolved.Number = Integer_Value'Last);
 Load (A, Fixture, Bits_64, Result); Check (Result = Loaded);
 Make (A, Data_Object (Snapshot (A), 2), 0, R, Status); Check (Status = Ready);
 Store_Integer (A, R, 16#1234#, Write_Result); Check (Write_Result = Returned);
 Read (A, R, V, OK); Check (OK and V = 16#34#);
 -- Byte stores preserve storage usage; package integer stores preserve
 -- source origin/value independently of AML width, as before this change.
 for Width in Integer_Width loop
   for Origin in Integer_Origin loop
     for Number of Inputs loop
       Usage_Before := Values_Used (A);
       Store_Reference_Value (A, R, Width, (Integer_Datum, Number, Origin), Write_Result);
       Check (Write_Result = Returned and then Valid (A));
       Read (A, R, V, OK); Check (OK and then V = Byte (Number mod 256));
       Check (Values_Used (A) = Usage_Before);
       Store_Reference_Value (A, Element_Ref, Width, (Integer_Datum, Number, Origin), Write_Result);
       Check (Write_Result = Returned and then Valid (A));
       Resolve_Value (A, Element_Ref, Resolved, Write_Result);
       Check (Write_Result = Returned and then Resolved.Value_Kind = Integer_Datum
         and then Resolved.Number = Number and then Resolved.Origin = Origin);
     end loop;
   end loop;
 end loop;
 -- Byte-index stores use the first byte of a valid string/buffer source.
 Load (A, String_Fixture, Bits_64, Result); Check (Result = Loaded);
 declare
   Source, Foreign : Datum;
   H : AML_References.Object_Handle;
 begin
   Make_Source (A, Data_Object (Snapshot (A), 3), H, OK); Check (OK);
   Read_Source (A, H, Source, Write_Result); Check (Write_Result = Returned);
   Store_Reference_Value (A, R, Bits_64, Source, Write_Result); Check (Write_Result = Returned);
   Read (A, R, V, OK); Check (OK and V = 97);
   Make_Source (B, Data_Object (Snapshot (B), 1), H, OK); Check (OK);
   Read_Source (B, H, Foreign, Write_Result); Check (Write_Result = Returned);
   Store_Reference_Value (A, R, Bits_64, Foreign, Write_Result);
   Check (Write_Result = Unsupported_Value and then Valid (A));
   Read (A, R, V, OK); Check (OK and V = 97);
 end;
 declare
   Small : Arena (Object_Capacity => 4, Byte_Capacity => 64, Element_Capacity => 4);
   Slot : Test_Owner.Reference;
 begin
   Reset (Small, OK); Check (OK);
   Load (Small, Package_Fixture, Bits_64, Result); Check (Result = Loaded);
   Make_Element (Small, Data_Object (Snapshot (Small), 1), 0, Slot, Element_Status);
   Check (Element_Status = AML_Objects.Package_References.Ready);
   Store_Integer (Small, Slot, 7, Write_Result); Check (Write_Result = Returned);
   Store_Integer (Small, Slot, 9, Write_Result);
   Check (Write_Result = Value_Limit and then Valid (Small));
   Resolve_Value (Small, Slot, Resolved, Write_Result);
   Check (Write_Result = Returned and then Resolved.Value_Kind = Integer_Datum
     and then Resolved.Number = 7);
 end;
 Ada.Text_IO.Put_Line ("Owned namespace explicit checks:" & Natural'Image (Checks) & " PASS");
end Direct_Store_Tests;
