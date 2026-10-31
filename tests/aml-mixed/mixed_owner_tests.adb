with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Objects;
with AML_References;
with Test_Namespace; use Test_Namespace;
procedure Mixed_Owner_Tests is
   use Owned;
   use type Integer_Value;
   use type AML_Objects.Allocation_Status;
   A, B : Arena;
   OK : Boolean;
   Loaded_Result : Load_Status;
   Status : Execution_Status;
   Text, Buffer_Value, Bad, Foreign, Package_Value : Datum;
   H : AML_References.Object_Handle;
   V : Integer_Value;
   Prior : State;
   ID : AML_Objects.Object_ID;
   Allocation : AML_Objects.Allocation_Status;
   Checks : Natural := 0;
   Fixture : constant Bytes :=
     [16#08#,65,65,65,65,16#0D#,48,120,49,50,0,
      16#08#,66,66,66,66,16#11#,4,16#0A#,1,16#12#,
      16#08#,67,67,67,67,16#12#,2,0];
   procedure Check (C : Boolean) is
   begin Checks := Checks + 1; if not C then raise Program_Error with Checks'Image; end if; end Check;
   procedure Fetch (Node : Node_ID; Item : out Datum) is
   begin Make_Source (A, Data_Object (Snapshot (A), Node), H, OK); Check (OK);
      Read_Source (A, H, Item, Status); Check (Status = Returned); end Fetch;
   procedure Reject (Left, Right : Datum) is
   begin
      V := 123;
      Compare_Byte_Values (A, 16#93#, Left, Right, Bits_64, V, Status);
      Check (Status = Unsupported_Value and then V = 0 and then Snapshot (A) = Prior);
   end Reject;
   procedure Equal_Mixed is
   begin
      for W in Integer_Width loop
         Compare_Byte_Values (A, 16#93#, Text, Buffer_Value, W, V, Status);
         Check (Status = Returned and then V =
           (if W = Bits_32 then 16#FFFF_FFFF# else Integer_Value'Last));
         Check (Snapshot (A) = Prior);
      end loop;
   end Equal_Mixed;
begin
   Reset (A, OK); Check (OK); Reset (B, OK); Check (OK);
   Load (A, Fixture, Bits_64, Loaded_Result); Check (Loaded_Result = Loaded);
   Load (B, Fixture, Bits_64, Loaded_Result); Check (Loaded_Result = Loaded);
   Fetch (1, Text); Fetch (2, Buffer_Value); Fetch (3, Package_Value);
   Prior := Snapshot (A); Equal_Mixed;
   Bad := Buffer_Value; Bad.Object.ID := Text.Object.ID; Reject (Text, Bad);
   Bad := Text; Bad.Object.ID := Buffer_Value.Object.ID; Reject (Bad, Buffer_Value);
   Reject (Text, Package_Value); Reject (Package_Value, Text);
   Reject (Text, (Reference_Datum, AML_References.No_Reference));
   Make_Source (B, Data_Object (Snapshot (B), 2), H, OK); Check (OK);
   Read_Source (B, H, Foreign, Status); Check (Status = Returned);
   Reject (Text, Foreign); Reject (Foreign, Text);
   Bad := Buffer_Value; Bad.Object.Type_Code := 0; Bad.Object.Size := Natural'Last;
   Bad.Object.Conversion_32.Value := 999; Bad.Object.Conversion_64.Value := 999;
   Compare_Byte_Values (A, 16#93#, Text, Bad, Bits_64, V, Status);
   Check (Status = Returned and then V = Integer_Value'Last and then Snapshot (A) = Prior);
   -- Fill only the object pool; compare still allocates no object or bytes.
   loop
      Append (A, [1 .. 0 => 0], ID, Allocation);
      exit when Allocation /= AML_Objects.Allocated;
   end loop;
   Prior := Snapshot (A); Equal_Mixed;
   Reject (Text, Foreign);
   Reset (A, OK); Check (OK);
   Load (A, Fixture, Bits_64, Loaded_Result); Check (Loaded_Result = Loaded);
   Prior := Snapshot (A); Reject (Text, Buffer_Value);
   Fetch (1, Text); Fetch (2, Buffer_Value);
   -- Independently exhaust byte backing while leaving object slots free.
   declare
      Padding : constant Bytes (1 .. AML_Objects.Max_Bytes - Values_Used (A).Bytes) := [others => 0];
   begin
      Append (A, Padding, ID, Allocation);
      Check (Allocation = AML_Objects.Allocated);
   end;
   Prior := Snapshot (A); Equal_Mixed;
   Append (A, [0], ID, Allocation);
   Check (Allocation /= AML_Objects.Allocated and then Snapshot (A) = Prior);
   Equal_Mixed;
   Ada.Text_IO.Put_Line ("MIXED-OWNER: PASS" & Checks'Image);
end Mixed_Owner_Tests;
