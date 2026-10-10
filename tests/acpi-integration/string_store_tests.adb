with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_References;
with AML_Objects.Byte_References;
with Test_Namespace; use Test_Namespace;
procedure String_Store_Tests is
   use Owned;
   use type Integer_Value;
   use type Byte;
   use type AML_Objects.Allocation_Status;
   use type AML_Objects.Byte_References.Result_Status;
   A, B : Arena;
   OK : Boolean;
   Loaded_Result : Load_Status;
   Status : Execution_Status;
   Left, Right, Bad, Foreign, Fresh : Datum;
   Target, H : AML_References.Object_Handle;
   Prior : State;
   Good_Ref, Lost_Ref : Reference;
   Ref_Status : AML_Objects.Byte_References.Result_Status;
   Octet : Byte;
   Unused_ID : AML_Objects.Object_ID;
   Allocated : AML_Objects.Allocation_Status;
   Checks : Natural := 0;
   Fixture : constant Bytes :=
     [16#08#,65,65,65,65,16#0D#,97,98,99,0,
      16#08#,66,66,66,66,16#0D#,49,48,0,
      16#08#,67,67,67,67,16#11#,4,16#0A#,1,1];
   procedure Check (C : Boolean) is
   begin Checks := Checks + 1; if not C then raise Program_Error with Checks'Image; end if; end Check;
   procedure Fetch (Node : Node_ID; Item : out Datum) is
   begin Make_Source (A, Data_Object (Snapshot (A), Node), H, OK); Check (OK);
      Read_Source (A, H, Item, Status); Check (Status = Returned); end Fetch;
   procedure Reject (Item : Datum) is
   begin Prior := Snapshot (A); Store_String (A, Target, Item, Status);
      Check (Status = Unsupported_Value and then Snapshot (A) = Prior); end Reject;
begin
   Reset (A, OK); Check (OK); Reset (B, OK); Check (OK);
   Load (A, Fixture, Bits_64, Loaded_Result); Check (Loaded_Result = Loaded);
   Load (B, Fixture, Bits_64, Loaded_Result); Check (Loaded_Result = Loaded);
   Fetch (1, Left); Fetch (2, Right); Fetch (3, Bad); Target := Left.Object.Source;
   Reject (Bad); Reject ((Integer_Datum, 3, AML_Decode.Ordinary_Integer));
   Bad := Right; Bad.Object.ID := Left.Object.ID; Reject (Bad);
   Make_Source (B, Data_Object (Snapshot (B), 2), H, OK); Check (OK);
   Read_Source (B, H, Foreign, Status); Check (Status = Returned); Reject (Foreign);
   Prior := Snapshot (A); Store_String (A, Target, Left, Status);
   Check (Status = Returned and then Snapshot (A) = Prior);
   Make (A, Left.Object.ID, 1, Good_Ref, Ref_Status);
   Check (Ref_Status = AML_Objects.Byte_References.Ready);
   Make (A, Left.Object.ID, 2, Lost_Ref, Ref_Status);
   Check (Ref_Status = AML_Objects.Byte_References.Ready);
   Bad := Right; Bad.Object.Type_Code := 0; Bad.Object.Size := Natural'Last;
   Store_String (A, Target, Bad, Status); Check (Status = Returned);
   Check (Data_Object (Snapshot (A), 1) = Left.Object.ID);
   Read (A, Good_Ref, Octet, OK); Check (OK and then Octet = 48);
   Read (A, Lost_Ref, Octet, OK); Check (not OK and then Octet = 0);
   Prior := Snapshot (A); Write (A, Lost_Ref, 99, OK);
   Check (not OK and then Snapshot (A) = Prior);
   Fresh := Left; Refresh_Value (A, Fresh, Status);
   Check (Status = Returned and then Fresh.Object.Size = 2 and then Fresh.Object.Type_Code = 2);
   Check (Left.Object.Size = 3);
   Check (Fresh.Object.Conversion_32.Value = 16 and then Fresh.Object.Conversion_64.Value = 16);
   Check (Left.Object.Conversion_32.Value = 16#ABC#);
   Fetch (2, Bad); Check (Bad.Object = Right.Object);
   Bad := Foreign; Refresh_Value (A, Bad, Status);
   Check (Status = Unsupported_Value and then Bad = Foreign);
   Append (A, [1 .. AML_Objects.Max_Bytes - Values_Used (A).Bytes => 0], Unused_ID, Allocated);
   Check (Allocated = AML_Objects.Allocated);
   Prior := Snapshot (A); Store_String (A, Target, Right, Status);
   Check (Status = Value_Limit and then Snapshot (A) = Prior);
   Clone_Value (A, Bits_64, Right, Fresh, Status);
   Check (Status = Value_Limit and then Snapshot (A) = Prior);
   Store_String (A, Target, Left, Status);
   Check (Status = Returned and then Snapshot (A) = Prior);
   Reset (A, OK); Check (OK); Prior := Snapshot (A);
   Store_String (A, Target, Right, Status);
   Check (Status = Unsupported_Value and then Snapshot (A) = Prior);
   Fresh := Left; Refresh_Value (A, Fresh, Status);
   Check (Status = Unsupported_Value and then Fresh = Left);
   Ada.Text_IO.Put_Line ("STRING-STORE PASS" & Checks'Image);
end String_Store_Tests;
