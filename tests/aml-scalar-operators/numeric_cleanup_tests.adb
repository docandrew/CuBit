with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
procedure Numeric_Cleanup_Tests is
   use type Bytes;
   use type Integer_Value;
   package N is new AML_Namespace (32, AML_Delays.Unavailable_Provider);
   use N; use N.Owned;
   A : Arena;
   Input : aliased AML_Table_Backing.State (1, 32);
   Checks : Natural := 0;
   function Text (Value : String) return Bytes is
      Result : Bytes (1 .. Value'Length);
   begin
      for I in Result'Range loop Result (I) := Character'Pos (Value (Value'First + I - 1)); end loop;
      return Result;
   end Text;
   function Method (Name : String; Code : Bytes) return Bytes is
     ([16#14#, Byte (Code'Length + 6)] & Text (Name) & [0] & Code);
   Reset_Source : constant Bytes := [16#9D#, 16#0A#, 16#11#] & Text ("SRC0");
   Read_Source : constant Bytes := [16#83#, 16#71#] & Text ("SRC0");
   Data : constant Bytes :=
     [16#08#] & Text ("SRC0") & [16#0A#, 16#11#, 16#08#] & Text ("BUF0") &
     [16#11#, 5, 16#0A#, 2, 0, 0] &
     Method ("BADM", [16#70#, 16#0A#, 16#22#] & Text ("SRC0") & [16#A4#, 16#0A#, 99]) &
     Method ("MUTG", [16#70#, 16#0A#, 16#22#] & Text ("SRC0") & [16#A4#, 1]) &
     Method ("FAIL", Reset_Source & [16#A4#, 16#72#, 1] & Read_Source &
       [16#88#] & Text ("BUF0") & Text ("BADM") & [0]) &
     Method ("GOOD", Reset_Source & [16#A4#, 16#72#] & Read_Source & Text ("MUTG") & [0]) &
     Method ("SRCV", [16#A4#] & Text ("SRC0"));
   procedure Check (Good : Boolean; Detail : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Detail; end if;
   end Check;
   procedure Call (Name : String; Fuel : Natural; Result : out Execution_Result) is
      Node : constant Node_ID := Child (Snapshot (A), Root, Name);
   begin
      Check (Node /= Root, "find " & Name);
      Invoke (A, Input, Node, [others => (Integer_Datum, 0, Ordinary_Integer)], 0, Fuel, Result);
      Check (Frame_Root_Count (A) = 0, "cleanup " & Name);
   end Call;
begin
   for Width in Integer_Width loop
      declare OK : Boolean; Loaded_Status : Load_Status; Result : Execution_Result; Full_Charge : Natural; begin
         Reset (A, OK); Check (OK, "reset");
         Load (A, Data, Width, Loaded_Status); Check (Loaded_Status = Loaded, "load");
         Call ("FAIL", 1000, Result);
         Check (Result.Status = Unsupported_Value, "computed Index failure after mutation");
         Call ("SRCV", 1000, Result);
         Check (Result.Status = Returned and then Result.Value = 16#22#, "failed target preserves source mutation");
         Call ("GOOD", 1000, Result);
         Check (Result.Status = Returned and then Result.Value = 16#23#, "success after target failure");
         Check (Result.Charged > 0, "nonzero measured fuel"); Full_Charge := Result.Charged;
         Call ("GOOD", Full_Charge - 1, Result);
         Check (Result.Status = Budget_Exceeded, "short fuel failure");
         Call ("GOOD", 1000, Result);
         Check (Result.Status = Returned and then Result.Value = 16#23#, "repeat success after fuel failure");
         Call ("FAIL", 1000, Result);
         Check (Result.Status = Unsupported_Value, "repeat computed target failure");
         Call ("GOOD", 1000, Result);
         Check (Result.Status = Returned and then Result.Value = 16#23#, "repeat recovery");
      end;
   end loop;
   Ada.Text_IO.Put_Line ("AML-NUMERIC-CLEANUP: PASS" & Checks'Image);
end Numeric_Cleanup_Tests;
