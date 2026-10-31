with Firmware_Tables;
with Ada.Text_IO;
with AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
with AML_Table_Sources;
procedure Field_Dereference_Tests is
   use AML_Decode;
   use type AML_Execute.Execution_Status;
   use type Integer_Value;
   package N is new AML_Namespace (16, AML_Delays.Unavailable_Provider);
   use type N.Load_Status;
   use type N.Bind_Status;
   A : N.Owned.Arena;
   Input : aliased AML_Table_Backing.State (1, Firmware_Tables.Table_Header_Size);
   Good : Boolean;
   Loaded : N.Load_Status;
   Bound : N.Bind_Status;
   Region, Field_Node : N.Node_ID;
   Direct, Indirect, Explicit_Ref, Argument_Ref : AML_Execute.Execution_Result;
   Checks : Natural := 0;
   type Natural_Array is array (Positive range <>) of Natural;
   Source : constant AML_Table_Sources.Source :=
     (Kind => AML_Table_Sources.Boot_Source, Boot => 1);
   Code : constant Bytes :=
     [16#14#,11,68,73,82,48,0,16#A4#,70,76,68,48,
      16#14#,16,73,78,68,48,0,16#5B#,16#12#,70,76,68,48,16#60#,16#A4#,16#83#,16#60#,
      16#14#,13,82,69,70,48,0,16#A4#,16#83#,16#71#,70,76,68,48,
      16#14#,16,65,82,69,70,1,16#5B#,16#12#,70,76,68,48,16#68#,16#A4#,16#83#,16#68#];
   procedure Check (Value : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not Value then raise Program_Error with Why; end if;
   end Check;
   procedure Invoke (Name : String; Result : out AML_Execute.Execution_Result;
                     Budget : Natural := 100) is
   begin
      N.Owned.Invoke (A, Input, N.Child (N.Owned.Snapshot (A), N.Root, Name),
        [others => (AML_Execute.Integer_Datum, 0, Ordinary_Integer)], (if Name = "AREF" then 1 else 0), Budget, Result);
      Check (Result.Charged <= Budget, "bounded operation charge");
   end Invoke;
   procedure Agree (Expected, Actual : AML_Execute.Execution_Result) is
   begin
      Check (Actual.Status = Expected.Status, "direct/reference status agreement " & Expected.Status'Image & " vs " & Actual.Status'Image);
      if Expected.Status = AML_Execute.Returned then
         Check (Actual.Value = Expected.Value, "direct/reference integer agreement");
      elsif Expected.Status = AML_Execute.Object_Returned then
         Check (Actual.Object.Type_Code = Expected.Object.Type_Code and then
                Actual.Object.Size = Expected.Object.Size and then
                Actual.Object.Conversion_32.Value = Expected.Object.Conversion_32.Value and then
                Actual.Object.Conversion_64.Value = Expected.Object.Conversion_64.Value,
                "direct/reference Buffer agreement");
      end if;
   end Agree;
begin
   Input.Count := 1;
   Input.Tables (1) := (Offset => 0, Extent => Firmware_Tables.Table_Header_Size);
   Input.Data.Bytes (1) := 16#44#;
   for Width in Integer_Width loop
      for Bits of Natural_Array'([8, 72]) loop
         N.Owned.Reset (A, Good); Check (Good, "reset");
         N.Owned.Load (A, Code, Width, Loaded); Check (Loaded = N.Loaded, "load methods");
         N.Owned.Bind_Table_Region (A, N.Root, "REG0", (Source, Firmware_Tables.Table_Header_Size), Region, Bound);
         Check (Bound = N.Bound, "bind owned table region");
         N.Owned.Bind_Table_Field (A, N.Root, "FLD0",
           ((Source, Firmware_Tables.Table_Header_Size), 0, Bits), Field_Node, Bound);
         Check (Bound = N.Bound, "bind owned field");
         Input.Count := 1;
         Invoke ("DIR0", Direct);
         Check (Direct.Status = (if Bits = 8 then AML_Execute.Returned else AML_Execute.Object_Returned),
           "direct field control must read the owned bytes");
         if Bits = 8 then Check (Direct.Value = 16#44#, "owned byte control"); end if;
         Invoke ("IND0", Indirect); Agree (Direct, Indirect);
         Invoke ("AREF", Argument_Ref); Agree (Direct, Argument_Ref);
         Invoke ("REF0", Explicit_Ref);
         Check (Explicit_Ref.Status = AML_Execute.Metadata_Returned, "direct reference retains field identity");
         Invoke ("IND0", Indirect, 0);
         Check (Indirect.Status = AML_Execute.Budget_Exceeded, "zero budget refusal");
         Input.Count := 0;
         Invoke ("DIR0", Direct);
         Check (Direct.Status = AML_Execute.Unsupported_Value, "missing backing refusal");
         Invoke ("IND0", Indirect); Agree (Direct, Indirect);
         Invoke ("AREF", Argument_Ref); Agree (Direct, Argument_Ref);
         Invoke ("REF0", Explicit_Ref);
         Check (Explicit_Ref.Status = AML_Execute.Metadata_Returned, "unread reference retains field identity without backing");
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("field dereference checks=" & Checks'Image);
end Field_Dereference_Tests;
