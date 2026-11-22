pragma Ada_2022;
with AML_Table_Sources;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Delays;
with AML_Namespace;
with AML_Table_Backing;
procedure Table_Session_Tests is
   package NS is new AML_Namespace
     (Capacity => 32, Perform_Delay => AML_Delays.Unavailable_Provider);
   use type NS.Load_Status;
   use type NS.Bind_Status;
   use type AML_Decode.Bytes;
   use type Integer_Value;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   function Enc (Text : String) return Bytes is
      Result : Bytes (1 .. Text'Length);
   begin
      for I in Result'Range loop
         Result (I) := Character'Pos (Text (Text'First + I - 1));
      end loop;
      return Result;
   end Enc;
   -- All method bodies fit the canonical one-byte AML PkgLength form.
   function Method_Data (Name : String; Flags : Byte; Body_Data : Bytes)
      return Bytes is
     ([16#14#, Byte (6 + Body_Data'Length)] & Enc (Name) & [Flags] & Body_Data);
   Code : constant Bytes :=
     Method_Data ("READ", 16#28#, [16#A4#] & Enc ("FLD0")) &
     Method_Data ("NEST", 16#18#, [16#A4#] & Enc ("READ")) &
     Method_Data ("LOW0", 8, [16#A4#, 1]) &
     Method_Data ("ORDR", 16#28#, [16#A4#] & Enc ("LOW0")) &
     Method_Data ("FAIL", 16#29#, [16#78#, 1, 16#68#, 0, 0, 16#A4#, 1]) &
     Method_Data ("VOID", 16#28#, [16#A3#]) &
     Method_Data ("WRIT", 16#28#, [16#70#, 1] & Enc ("FLD0") & [16#A4#, 0]);
   Tree : NS.State;
   Loaded : NS.Load_Status;
   Bound : NS.Bind_Status;
   Region_Node, Field_Node : NS.Node_ID;
   Input : aliased AML_Table_Backing.State (1, 64);
   Original_Bytes : Bytes (1 .. 64);
   Result : Execution_Result;
   Nested_Charge : Natural;
   procedure Run (Name : String; Expected : Execution_Status;
                  Value : Integer_Value := 0; Budget : Natural := 100;
                  Argument_Count : Natural := 0) is
      Method_Node : constant NS.Node_ID := NS.Child (Tree, NS.Root, Name);
      Prior_Count : constant NS.Node_ID := NS.Count (Tree);
      Prior_Code : constant Bytes := NS.Method_Data (Tree, Method_Node);
   begin
      NS.Invoke_With_Tables
        (Tree, Input, Method_Node, [others => 0], Argument_Count, Budget, Result);
      Check (Result.Status = Expected);
      Check (Result.Charged <= Budget);
      if Expected = Returned then Check (Result.Value = Value); end if;
      Check (Input.Data.Bytes = Original_Bytes);
      Check (NS.Count (Tree) = Prior_Count);
      Check (NS.Method_Data (Tree, Method_Node) = Prior_Code);
   end Run;
begin
   -- Public owned-byte backing construction, not physical-memory admission.
   Input.Count := 1;
   Input.Tables (1) := (Offset => 0, Extent => 64);
   Input.Data.Bytes := [others => 16#A5#];
   Input.Data.Bytes (37) := 16#D6#;
   Original_Bytes := Input.Data.Bytes;
   Check (AML_Table_Backing.Valid_Span (Input, 1));
   for Width in Integer_Width loop
      Tree := NS.Empty;
      NS.Load_Names (Tree, Code, Width, Loaded);
      Check (Loaded = NS.Loaded);
      NS.Bind_Table_Region
        (Tree, NS.Root, "REG0", (Item => (AML_Table_Sources.Boot_Source, 1), Extent => 64), Region_Node, Bound);
      Check (Bound = NS.Bound and then Region_Node > NS.Root);
      NS.Bind_Table_Field
        (Tree, NS.Root, "FLD0", ((Item => (AML_Table_Sources.Boot_Source, 1), Extent => 64), 36 * 8 + 1, 5),
         Field_Node, Bound);
      Check (Bound = NS.Bound and then Field_Node > NS.Root);
      -- D6 bits 1..5 are 01011 = 11, independent of AML integer width.
      for Repeat in 1 .. 3 loop
         Run ("READ", Returned, 11);
         Run ("NEST", Returned, 11);
         Nested_Charge := Result.Charged;
         Check (Nested_Charge > 1);
         Run ("NEST", Budget_Exceeded, Budget => Nested_Charge - 1);
         Run ("NEST", Returned, 11);
         Run ("ORDR", Mutex_Order);
         Run ("LOW0", Returned, 1);
         Run ("FAIL", Division_By_Zero, Argument_Count => 1);
         Run ("NEST", Returned, 11);
         Run ("VOID", No_Return);
         Run ("NEST", Returned, 11);
         Run ("WRIT", Unsupported);
         Run ("NEST", Returned, 11);
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("TABLE-SESSION: PASS" & Checks'Image);
end Table_Session_Tests;
