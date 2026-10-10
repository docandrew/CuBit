with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
procedure Revision_Tests is
   use type Integer_Value;
   use type Byte;
   package NS is new AML_Namespace (16, AML_Delays.Unavailable_Provider);
   use NS; use NS.Owned;
   A : Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Result : Execution_Result;
   Loaded_Status : Load_Status;
   OK : Boolean;
   Checks : Natural := 0;
   Rev : constant Bytes := [Extended_Op, Revision_Extension];
   procedure Check (Value : Boolean) is
   begin
      Checks := Checks + 1;
      if not Value then raise Program_Error with Checks'Image & Result.Status'Image; end if;
   end Check;
   procedure Run (Code : Bytes; Width : Integer_Width; Fuel : Natural := 100) is
   begin
      Reset (A, OK); Check (OK);
      Load (A, Bytes'(16#14#, Byte (6 + Code'Length),84,69,83,84,0) & Code,
            Width, Loaded_Status);
      Check (Loaded_Status = Loaded);
      Invoke (A, Input, 1, [others => <>], 0, Fuel, Result);
   end Run;
   procedure Returns (Code : Bytes; Width : Integer_Width; Value : Integer_Value) is
   begin
      Run (Code, Width);
      Check (Result.Status = Returned and then Result.Value = Value);
   end Returns;
   procedure Rejected (Code : Bytes; Width : Integer_Width; Expected : Execution_Status) is
   begin
      Run (Code, Width);
      Check (Result.Status = Expected);
   end Rejected;
begin
   for Width in Integer_Width loop
      declare
         R : Integer_Result := Read_Integer (Rev & Bytes'(16#AA#,16#BB#), Width);
         High : constant Bytes (Natural'Last - 1 .. Natural'Last) := Rev;
      begin
         Check (R.Kind = Accepted and then R.Value = Interpreter_Revision and then R.Consumed = Revision_Bytes);
         R := Read_Integer (High, Width);
         Check (R.Kind = Accepted and then R.Value = 1 and then R.Consumed = 2);
         R := Read_Integer (Bytes'(1 => Extended_Op), Width);
         Check (R.Kind = Truncated);
         R := Read_Integer ([Extended_Op,16#31#], Width);
         Check (R.Kind = Unsupported);
      end;
      Returns (Bytes'(1=>16#A4#) & Rev, Width, 1);
      Check (Result.Origin = Ordinary_Integer);
      Check (Result.Charged = 2);
      Run (Bytes'(1=>16#A4#) & Rev, Width, 1);
      Check (Result.Status = Budget_Exceeded);
      Returns (Bytes'(1=>16#70#) & Rev & Bytes'(16#60#,16#70#,16#0A#,9,16#60#,16#A4#,16#60#), Width, 9);
      Returns (Bytes'(16#A4#,16#72#) & Rev & Bytes'(16#0A#,2,0), Width, 3);
      -- Static computational data and Buffer count use the same revision.
      Returns (Bytes'(16#08#,84,69,77,80) & Rev & Bytes'(16#A4#,84,69,77,80), Width, 1);
      Check (Result.Origin = Ordinary_Integer);
      Returns ([16#08#,84,69,77,80,16#11#,4,16#5B#,16#30#,16#AA#,
                16#A4#,16#87#,84,69,77,80], Width, 1);
      Rejected (Bytes'(16#70#,1) & Rev, Width, Unsupported);
      Rejected (Bytes'(1=>16#75#) & Rev, Width, Unsupported);
      Rejected (Bytes'(16#9D#,1) & Rev, Width, Bad_Name);
      Rejected (Bytes'(16#A4#,16#71#) & Rev, Width, Unsupported_Value);
      Rejected ([16#A4#,16#5B#], Width, Truncated);
      Rejected ([16#08#,84,69,77,80,16#11#,2,16#5B#], Width, Truncated);
      Returns (Bytes'(1=>16#A4#) & Rev, Width, 1);
      Reset (A, OK); Check (OK);
      Load (A, Bytes'(16#08#,82,69,86,48) & Rev &
            Bytes'(16#14#,11,84,69,83,84,0,16#A4#,82,69,86,48), Width, Loaded_Status);
      Check (Loaded_Status = Loaded);
      Invoke (A, Input, 2, [others => <>], 0, 100, Result);
      Check (Result.Status = Returned and then Result.Value = 1 and then Result.Origin = Ordinary_Integer);
      Returns ([16#08#,84,69,77,80,16#12#,4,1,16#5B#,16#30#,
                16#A4#,16#83#,16#88#,84,69,77,80,0,0], Width, 1);
      declare
         High : constant Bytes (Natural'Last - 4 .. Natural'Last) := [16#11#,4,16#5B#,16#30#,16#AA#];
         B : constant Buffer_Result := Read_Buffer (High, Width);
         T : constant Buffer_Result := Read_Buffer ([16#11#,2,16#5B#], Width);
      begin
         Check (B.Kind = Accepted and then B.Length = 1 and then B.Content (1) = 16#AA#);
         Check (T.Kind = Truncated);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("REVISION: PASS" & Checks'Image);
end Revision_Tests;
