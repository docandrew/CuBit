with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Objects;
with AML_Table_Backing;
procedure Debug_Owned_Tests is
   package NS is new AML_Namespace (16, AML_Delays.Unavailable_Provider);
   use NS; use NS.Owned;
   use type AML_Objects.Usage;
   use type Integer_Value;
   A : Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Result : Execution_Result;
   Loaded_Status : Load_Status;
   OK : Boolean;
   Checks : Natural := 0;
   Debug : constant Bytes := [Extended_Op, Debug_Extension];
   procedure Check (Value : Boolean) is
   begin
      Checks := Checks + 1;
      if not Value then raise Program_Error with Checks'Image & Result.Status'Image; end if;
   end Check;
   procedure Run (Code : Bytes; Width : Integer_Width) is
   begin
      Reset (A, OK); Check (OK);
      Load (A, Bytes'(16#14#, Byte (6 + Code'Length),84,69,83,84,0) & Code,
            Width, Loaded_Status);
      Check (Loaded_Status = Loaded);
      Invoke (A, Input, 1, [others => <>], 0, 100, Result);
   end Run;
   procedure Compare (Source : Bytes; Width : Integer_Width; Kind, Size : Natural) is
      Before : AML_Objects.Usage;
      Charged : Natural;
   begin
      Run (Bytes'(1=>16#A4#) & Source, Width);
      Check (Result.Status = Object_Returned);
      Before := Values_Used (A); Charged := Result.Charged;
      Run (Bytes'(16#A4#,16#70#) & Source & Debug, Width);
      Check (Result.Status = Object_Returned and then Result.Object.Type_Code = Kind
             and then Result.Object.Size = Size);
      Check (Values_Used (A) = Before); -- target adds no allocation/copy
      Check (Result.Charged = Charged + 1); -- normal Store opcode charge
   end Compare;
begin
   for Width in Integer_Width loop
      Compare ([16#0D#,65,0], Width, 2, 1);
      Compare ([16#11#,3,1,16#AA#], Width, 3, 1);
      Compare ([16#12#,4,1,16#0A#,7], Width, 4, 1);
      -- Exact string from the upstream STRT failure, with disabled output.
      Run (Bytes'(16#70#,16#0D#,54,52,45,98,105,116,32,109,111,100,101,0)
           & Debug & Bytes'(16#A4#,1), Width);
      Check (Result.Status = Returned and then Result.Value = 1);
   end loop;
   Ada.Text_IO.Put_Line ("DEBUG OWNED: PASS" & Checks'Image);
end Debug_Owned_Tests;
