with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_String_Order; use AML_String_Order;
with AML_References;
with Test_Namespace; use Test_Namespace;
procedure String_Compare_Tests is
   use Owned;
   use type Byte;
   use type Integer_Value;
   A, B : Arena;
   OK : Boolean;
   Loaded_Result : Load_Status;
   Status : Execution_Status;
   L, R, Bad, Foreign : Datum;
   H : AML_References.Object_Handle;
   V : Integer_Value;
   Prior : State;
   Checks : Natural := 0;
   Fixture : constant Bytes :=
     [16#08#,65,65,65,65,16#0D#,97,98,99,0,
      16#08#,66,66,66,66,16#0D#,97,98,99,100,0,
      16#08#,67,67,67,67,16#11#,4,16#0A#,1,1];
   procedure Check (C : Boolean) is
   begin Checks := Checks + 1; if not C then raise Program_Error with Checks'Image; end if; end Check;
   procedure Fetch (Node : Node_ID; Item : out Datum) is
   begin Make_Source (A, Data_Object (Snapshot (A), Node), H, OK); Check (OK);
      Read_Source (A, H, Item, Status); Check (Status = Returned); end Fetch;
   procedure Reject (Left, Right : Datum; Op : Byte := 16#93#) is
   begin Compare_Byte_Values (A, Op, Left, Right, Bits_64, V, Status);
      Check (Status = Unsupported_Value and then V = 0 and then Snapshot (A) = Prior); end Reject;
begin
   for X in Byte loop
      for Y in Byte loop
         Check (Compare ([X], [Y]) = (if X < Y then Less elsif X > Y then Greater else Equal));
      end loop;
   end loop;
   Check (Compare ([1 .. 0 => 0], [1 .. 0 => 0]) = Equal);
   Check (Compare ([1 .. 0 => 0], [1]) = Less);
   Check (Compare ([1], [1,0]) = Less);
   Check (Compare ([1,2], [1,1,255]) = Greater);
   declare High : constant Bytes (Positive'Last - 2 .. Positive'Last) := [97,98,99]; begin
      Check (Compare (High, [97,98,99]) = Equal);
      Check (Compare (High, [97,98,100]) = Less);
      Check (Compare ([97,98,98], High) = Less);
   end;
   Reset (A, OK); Check (OK); Reset (B, OK); Check (OK);
   Load (A, Fixture, Bits_64, Loaded_Result); Check (Loaded_Result = Loaded);
   Load (B, Fixture, Bits_64, Loaded_Result); Check (Loaded_Result = Loaded);
   Fetch (1, L); Fetch (2, R); Fetch (3, Bad); Prior := Snapshot (A);
   for W in Integer_Width loop
      for Op in Byte range 16#93# .. 16#95# loop
         Compare_Byte_Values (A, Op, L, R, W, V, Status);
         Check (Status = Returned and then V =
           (if Op = 16#95# then (if W = Bits_32 then 16#FFFF_FFFF# else Integer_Value'Last) else 0));
         Check (Snapshot (A) = Prior);
      end loop;
   end loop;
   -- Mixed primitives are now admitted; retain all authority rejection cases.
   Compare_Byte_Values (A, 16#95#, Bad, R, Bits_64, V, Status);
   Check (Status = Returned and then V = Integer_Value'Last and then Snapshot (A) = Prior);
   Compare_Byte_Values (A, 16#94#, L, Bad, Bits_64, V, Status);
   Check (Status = Returned and then V = Integer_Value'Last and then Snapshot (A) = Prior);
   Reject ((Integer_Datum, 7, AML_Decode.Ordinary_Integer), R);
   Compare_Byte_Values (A, 16#94#, L, (Integer_Datum, 7, AML_Decode.Ordinary_Integer), Bits_64, V, Status);
   Check (Status = Returned and then V = Integer_Value'Last and then Snapshot (A) = Prior);
   Reject (L, R, 0);
   Bad := L; Bad.Object.ID := R.Object.ID; Reject (Bad, R);
   Make_Source (B, Data_Object (Snapshot (B), 1), H, OK); Check (OK);
   Read_Source (B, H, Foreign, Status); Check (Status = Returned); Reject (Foreign, R);
   Bad := L; Bad.Object.Type_Code := 0; Bad.Object.Size := Natural'Last;
   Compare_Byte_Values (A, 16#93#, Bad, L, Bits_64, V, Status);
   Check (Status = Returned and then V = Integer_Value'Last);
   Reset (A, OK); Check (OK); Load (A, Fixture, Bits_64, Loaded_Result); Check (Loaded_Result = Loaded);
   Prior := Snapshot (A); Reject (L, R);
   Ada.Text_IO.Put_Line ("STRING-COMPARE: PASS" & Checks'Image);
end String_Compare_Tests;
