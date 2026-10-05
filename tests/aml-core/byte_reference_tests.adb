with AML_Decode; use AML_Decode;
with AML_Objects; use AML_Objects;
with AML_Objects.Byte_References; use AML_Objects.Byte_References;
with Ada.Text_IO;
with Ada.Command_Line;
procedure Byte_Reference_Tests is
 use type Byte;
 use type Integer_Value;
 Store : State := Empty;
 S_ID, B_ID, I_ID : Object_ID;
 Allocation : Allocation_Status;
 S, B, Bad : Reference;
 Status : Result_Status;
 Value : Byte;
 Checks : Natural := 0;
 procedure Check (C : Boolean) is
 begin if not C then raise Program_Error with Natural'Image (Checks); end if;
 Checks := Checks + 1; end Check;
begin
 if Ada.Command_Line.Argument_Count = 1 then
   Check (Ada.Command_Line.Argument (1) /= "--negative-control");
 end if;
 New_Bytes (Store, Buffer_Object, [1 => 55], B_ID, Allocation); Check (Allocation = Allocated);
 New_Bytes (Store, String_Object, [1 => 97, 2 => 98, 3 => 99], S_ID, Allocation); Check (Allocation = Allocated);
 Make (Store, S_ID, 1, S, Status); Check (Status = Ready);
 Make (Store, B_ID, 0, B, Status); Check (Status = Ready);
 for V in Byte loop
   Write (Store, S, V, Status); Check (Status = Ready);
   Read (Store, S, Value, Status); Check (Status = Ready and Value = V);
   Read (Store, B, Value, Status); Check (Status = Ready and Value = 55);
   declare
      Data : constant Bytes := Byte_Data (Store, S_ID);
   begin
      Check (Data (Data'First) = 97 and Data (Data'Last) = 99);
   end;
 end loop;
 New_Integer (Store, 42, I_ID, Allocation); Check (Allocation = Allocated);
 Make (Store, I_ID, 0, Bad, Status); Check (Status = Wrong_Kind);
 Write (Store, Bad, 9, Status); Check (Status = Invalid_Reference);
 Check (Integer_Data (Store, I_ID) = 42);
 Make (Store, S_ID, 3, Bad, Status); Check (Status = Out_Of_Bounds);
 Make (Store, S_ID, Integer_Value'Last, Bad, Status); Check (Status = Out_Of_Bounds);
 Make (Store, 0, 0, Bad, Status); Check (Status = Invalid_Object);
 Ada.Text_IO.Put_Line ("Byte references explicit checks:" & Natural'Image (Checks) & " PASS");
end Byte_Reference_Tests;
