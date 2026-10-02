with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with Namespace_Instance;
with AML_Integers;
procedure Call_Tests is
   package NS renames Namespace_Instance;
   use type NS.Load_Status;
   use type NS.State;
   use type Integer_Value;
   Checks : Natural := 0;
   function Enc (Text : String) return Bytes is
      Result : Bytes (1 .. Text'Length);
   begin
      for I in Result'Range loop Result (I) := Character'Pos (Text (Text'First + I - 1)); end loop;
      return Result;
   end Enc;
   function Package_Data (Op : Byte; Body_Data : Bytes) return Bytes is
      Encoding : constant Positive :=
        (if Body_Data'Length + 1 < 64 then 1
         elsif Body_Data'Length + 2 < 4096 then 2 else 3);
      Size : constant Natural := Body_Data'Length + Encoding;
      Header : Bytes (1 .. Encoding);
   begin
      if Encoding = 1 then
         Header (1) := Byte (Size);
      else
         Header (1) := Byte (64 * (Encoding - 1) + Size mod 16);
         Header (2) := Byte ((Size / 16) mod 256);
         if Encoding = 3 then Header (3) := Byte (Size / 4096); end if;
      end if;
      return [Op] & Header & Body_Data;
   end Package_Data;
   function Method_Data (Name : String; Flags : Byte; Body_Data : Bytes) return Bytes is
     (Package_Data (16#14#, Enc (Name) & [Flags] & Body_Data));
   Data : constant Bytes :=
     [8] & Enc ("NUM0") & [16#0A#, 9] &
     Method_Data ("WRIT", 1, [16#70#, 16#68#] & Enc ("NUM0") & [16#A4#] & Enc ("NUM0")) &
     Method_Data ("NEST", 0, [16#A4#, 16#72#] & Enc ("NUM0WRIT") & [16#0A#, 7, 0]) &
     Method_Data ("STOR", 0, [16#A4#, 16#72#] & Enc ("NUM0") & [16#70#, 1] & Enc ("NUM0") & [0]) &
     Method_Data ("CARG", 0, [16#A4#] & Enc ("SUM2NUM0WRIT") & [16#0A#, 7]) &
     Method_Data ("PART", 0, [16#70#, 1] & Enc ("NUM0") & [16#78#, 1, 0, 0, 0]) &
     Method_Data ("MISS", 0, [16#70#, 1] & Enc ("NOPE")) &
     Method_Data ("IDEN", 1, [16#A4#,16#68#]) &
     Method_Data ("SUM2", 2, [16#A4#,16#72#,16#68#,16#69#,0]) &
     Method_Data ("ZERO", 0, [16#70#,16#0A#,9,16#60#,16#A4#,1]) &
     Method_Data ("VOID", 0, [16#A3#]) &
     Method_Data ("SELF", 0, [16#A4#] & Enc ("SELF")) &
     Method_Data ("DREC", 1, [16#A0#,11,16#68#,16#A4#] & Enc ("DREC") &
       [16#74#,16#68#,1,0,16#A4#,1]) &
     Method_Data ("CALL", 2, [16#A4#] & Enc ("SUM2IDEN") & [16#68#] & Enc ("IDEN") & [16#69#]) &
     Method_Data ("LOCL", 1, [16#70#,16#68#,16#60#] & Enc ("ZERO") & [16#A4#,16#60#]) &
     Method_Data ("ORDR", 0, [16#A4#] & Enc ("SUM2") & [16#72#,1,1,16#60#,16#60#]) &
     Method_Data ("VSTM", 0, Enc ("VOID") & [16#A4#,1]) &
     Method_Data ("VEXP", 0, [16#A4#] & Enc ("VOID")) &
     Method_Data ("VARG", 0, [16#A4#] & Enc ("IDENVOID")) &
     Method_Data ("LAST", 7, [16#A4#,16#6E#]) &
     Method_Data ("SEVN", 0, [16#A4#] & Enc ("LAST") & [0,1,16#0A#,2,16#0A#,3,16#0A#,4,16#0A#,5,16#0A#,6]) &
     Method_Data ("SER0", 8, [16#A4#,1]) &
     Method_Data ("SCAL", 0, [16#A4#] & Enc ("SER0")) &
     Method_Data ("BADC", 0, [16#A4#] & Enc ("IDEN"));
   Tree : NS.State;
   Loaded : NS.Load_Status;
   R, Fuel_Result : Execution_Result;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Expect (Name : String; Count : Natural; Args : Arguments;
                     Status : Execution_Status; Value : Integer_Value := 0) is
   begin
      R := NS.Invoke (Tree, NS.Child (Tree, NS.Root, Name), Args, Count, 10_000);
      Check (R.Status = Status and then (if Status = Returned then R.Value = Value));
      if R.Status = Returned then
         Fuel_Result := NS.Invoke (Tree, NS.Child (Tree, NS.Root, Name), Args, Count, R.Charged - 1);
         Check (Fuel_Result.Status = Budget_Exceeded);
      end if;
   end Expect;
begin
   for W in Integer_Width loop
      Tree := NS.Empty;
      NS.Load_Names (Tree, Data, W, Loaded);
      Check (Loaded = NS.Loaded);
      Expect ("CALL", 2, [0 => 5, 1 => 7, others => 0], Returned, 12);
      Expect ("LOCL", 1, [0 => 123, others => 0], Returned, 123);
      Expect ("ORDR", 0, [others => 0], Returned, 4);
      Expect ("VSTM", 0, [others => 0], Returned, 1);
      Expect ("VEXP", 0, [others => 0], Missing_Result);
      Expect ("VARG", 0, [others => 0], Missing_Result);
      Expect ("SEVN", 0, [others => 0], Returned, 6);
      Expect ("SCAL", 0, [others => 0], Returned, 1);
      Expect ("BADC", 0, [others => 0], Truncated);
      Expect ("SELF", 0, [others => 0], Call_Limit);
      for N in 0 .. 40 loop
         Expect ("DREC", 1, [0 => Integer_Value (N), others => 0],
                 (if N <= 32 then Returned else Call_Limit), 1);
      end loop;
      declare
         Number : constant NS.Node_ID := NS.Child (Tree, NS.Root, "NUM0");
         Writer : constant NS.Node_ID := NS.Child (Tree, NS.Root, "WRIT");
         type Names is array (Positive range <>) of String (1 .. 4);
      begin
         for Name of Names'("NEST", "STOR", "CARG") loop
            NS.Set_Integer (Tree, Number, 9);
            NS.Invoke_Mutable (Tree, NS.Child (Tree, NS.Root, Name), [others => 0], 0, 1000, R);
            Check (R.Status = Returned and then R.Value = (if Name = "STOR" then 10 else 16));
            Check (NS.Integer_Data (Tree, Number) = (if Name = "STOR" then 1 else 7));
         end loop;
         NS.Invoke_Mutable (Tree, Writer, [0 => Integer_Value'Last, others => 0], 1, 1000, R);
         Check (R.Status = Returned and then R.Value = AML_Integers.Normalize (Integer_Value'Last, W));
         Check (NS.Integer_Data (Tree, Number) = AML_Integers.Normalize (Integer_Value'Last, W));
         NS.Invoke_Mutable (Tree, Writer, [others => 0], 1, 0, R);
         Check (R.Status = Budget_Exceeded and NS.Integer_Data (Tree, Number) = AML_Integers.Normalize (Integer_Value'Last, W));
         NS.Invoke_Mutable (Tree, NS.Child (Tree, NS.Root, "PART"), [others => 0], 0, 1000, R);
         Check (R.Status = Division_By_Zero and NS.Integer_Data (Tree, Number) = 1);
         NS.Invoke_Mutable (Tree, NS.Child (Tree, NS.Root, "MISS"), [others => 0], 0, 1000, R);
         Check (R.Status = Unknown_Name and NS.Integer_Data (Tree, Number) = 1);
         R := NS.Invoke (Tree, Writer, [0 => 9, others => 0], 1, 1000);
         Check (R.Status = Unsupported and NS.Integer_Data (Tree, Number) = 1);
      end;
      -- Dynamic methods: creation, recursion lifetime, errors and reclamation.
      declare
         Temp : constant Bytes := Method_Data ("TEMP", 0, [16#A4#, 16#0A#, 42]);
         Root_Temp : constant Bytes := Method_Data ("\TEMP", 0, [16#A4#, 16#0A#, 7]);
         Before : NS.State;
      begin
         Tree := NS.Empty;
         NS.Load_Names (Tree,
           Method_Data ("MAK0", 8, Temp & [16#A4#] & Enc ("TEMP")) &
           Method_Data ("DUP0", 8, Temp & Temp & [16#A4#, 0]) &
           Method_Data ("ERR0", 8, Temp & [16#78#, 1, 0, 0, 0]) &
           Method_Data ("RECU", 9,
             Package_Data (16#A0#, [16#68#] & Enc ("RECU") & [0, 16#A4#] & Enc ("\TEMP")) &
             Root_Temp & [16#A4#, 0]) &
           Method_Data ("MAKE", 8, Root_Temp & [16#A4#, 0]) &
           Method_Data ("GONE", 8, Enc ("MAKE") & [16#A4#] & Enc ("\TEMP")) &
           Method_Data ("MIDL", 9,
             Package_Data (16#A0#, [16#68#] & Enc ("HELP") &
               Method_Data ("\DEAD", 0, [16#A4#, 1]) & [16#A4#] & Enc ("\LIVE")) &
             Method_Data ("\LIVE", 0, [16#A4#, 16#0A#, 9]) & [16#A4#, 0]) &
           Method_Data ("HELP", 8, Method_Data ("\DEAD", 0, [16#A4#, 0]) & Enc ("MIDL") & [0, 16#A4#, 0]),
           W, Loaded);
         Check (Loaded = NS.Loaded);
         Before := Tree;
         for Repeat in 1 .. 100 loop
            NS.Invoke_Mutable (Tree, NS.Child (Tree, NS.Root, "MAK0"), [others => 0], 0, 1000, R);
            Check (R.Status = Returned and then R.Value = 42);
            Check (Tree = Before);
         end loop;
         for Fuel in 0 .. 8 loop
            NS.Invoke_Mutable (Tree, NS.Child (Tree, NS.Root, "MAK0"), [others => 0], 0, Fuel, R);
            Check (R.Status in Returned | Budget_Exceeded);
            Check (Tree = Before);
         end loop;
         NS.Invoke_Mutable (Tree, NS.Child (Tree, NS.Root, "DUP0"), [others => 0], 0, 1000, R);
         Check (R.Status = Duplicate_Name and Tree = Before);
         NS.Invoke_Mutable (Tree, NS.Child (Tree, NS.Root, "ERR0"), [others => 0], 0, 1000, R);
         Check (R.Status = Division_By_Zero and Tree = Before);
         NS.Invoke_Mutable (Tree, NS.Child (Tree, NS.Root, "RECU"), [0 => 1, others => 0], 1, 1000, R);
         Check (R.Status = Returned and then R.Value = 7 and then Tree = Before);
         NS.Invoke_Mutable (Tree, NS.Child (Tree, NS.Root, "GONE"), [others => 0], 0, 1000, R);
         Check (R.Status = Unknown_Name and Tree = Before);
         NS.Invoke_Mutable (Tree, NS.Child (Tree, NS.Root, "MIDL"), [0 => 1, others => 0], 1, 1000, R);
         Check (R.Status = Returned and then R.Value = 9 and then Tree = Before);
      end;
      declare
         Before : NS.State;
         N : String (1 .. 4) := "M000";
         Partial : constant Bytes := [16#14#, 7, 16#54#, 16#45#, 16#4D#, 16#50#, 0];
      begin
         Tree := NS.Empty;
         for I in 0 .. 127 loop
            N (2) := Character'Val (Character'Pos ('0') + I / 100);
            N (3) := Character'Val (Character'Pos ('0') + (I / 10) mod 10);
            N (4) := Character'Val (Character'Pos ('0') + I mod 10);
            NS.Load_Names (Tree, Method_Data (N, 8,
              Method_Data ("TEMP", 0, [16#A4#, 1]) & [16#A4#, 0]), W, Loaded);
            Check (Loaded = NS.Loaded);
         end loop;
         Before := Tree;
         NS.Invoke_Mutable (Tree, 1, [others => 0], 0, 1000, R);
         Check (R.Status = Namespace_Limit and Tree = Before);
         Tree := NS.Empty;
         NS.Load_Names (Tree, Method_Data ("MAIN", 8,
           Method_Data ("TEMP", 0, Bytes'(1 .. 65_520 => 16#A3#)) & [16#A4#, 0]), W, Loaded);
         Check (Loaded = NS.Loaded);
         Before := Tree;
         NS.Invoke_Mutable (Tree, 1, [others => 0], 0, 1000, R);
         Check (R.Status = Namespace_Limit and Tree = Before);
         -- Malformed declaration packages never leave a partially named node.
         for Size in 0 .. 6 loop
            Tree := NS.Empty;
            NS.Load_Names (Tree, Method_Data ("MAIN", 8,
              Partial (1 .. Size)), W, Loaded);
            Check (Loaded = NS.Loaded);
            Before := Tree;
            NS.Invoke_Mutable (Tree, 1, [others => 0], 0, 1000, R);
            Check (R.Status in No_Return | Bad_Package and Tree = Before);
         end loop;
      end;
      declare
         Before : NS.State;
      begin
         Tree := NS.Empty;
         NS.Load_Names (Tree, [8] & Enc ("NUM0") & [16#0A#, 9] &
           Method_Data ("FAIL", 8, Method_Data ("TEMP", 0, [16#A4#, 1]) &
             [16#70#, 16#0A#, 7] & Enc ("NUM0") & [16#78#, 1, 0, 0, 0]) &
           Method_Data ("CLSH", 8, Method_Data ("\NUM0", 0, [16#A4#, 1])) &
           Method_Data ("MISS", 8, Method_Data ("^^TEMP", 0, [16#A4#, 1])), W, Loaded);
         Check (Loaded = NS.Loaded);
         Before := Tree;
         NS.Set_Integer (Before, 1, 7);
         NS.Invoke_Mutable (Tree, 2, [others => 0], 0, 1000, R);
         Check (R.Status = Division_By_Zero and Tree = Before);
         NS.Invoke_Mutable (Tree, 3, [others => 0], 0, 1000, R);
         Check (R.Status = Duplicate_Name and Tree = Before);
         NS.Invoke_Mutable (Tree, 4, [others => 0], 0, 1000, R);
         Check (R.Status = Unknown_Name and Tree = Before);
      end;
      -- Exhaustive call-order matrix, including ignored nonserialized levels.
      for Caller_Level in Sync_Level loop
         for Callee_Level in Sync_Level loop
            for Serialized in Boolean loop
               Tree := NS.Empty;
               NS.Load_Names (Tree,
                 Method_Data ("PARE", Byte (8 + 16 * Caller_Level), [16#A4#] & Enc ("CHLD")) &
                 Method_Data ("CHLD", Byte ((if Serialized then 8 else 0) + 16 * Callee_Level), [16#A4#, 1]),
                 W, Loaded);
               Check (Loaded = NS.Loaded);
               NS.Invoke_Mutable (Tree, 1, [others => 0], 0, 1000, R);
               Check (R.Status = (if Serialized and Caller_Level > Callee_Level then Mutex_Order else Returned));
               if R.Status = Returned then Check (R.Value = 1); end if;
            end loop;
            Tree := NS.Empty;
            NS.Load_Names (Tree,
              Method_Data ("PARE", Byte (8 + 16 * Caller_Level), [16#A4#] & Enc ("BRDG")) &
              Method_Data ("BRDG", Byte (16 * Callee_Level), [16#A4#] & Enc ("LEAF")) &
              Method_Data ("LEAF", Byte (8 + 16 * Callee_Level), [16#A4#, 1]), W, Loaded);
            Check (Loaded = NS.Loaded);
            NS.Invoke_Mutable (Tree, 1, [others => 0], 0, 1000, R);
            Check (R.Status = (if Caller_Level > Callee_Level then Mutex_Order else Returned));
         end loop;
      end loop;
      Tree := NS.Empty;
      NS.Load_Names (Tree,
        Method_Data ("PARE", 16#18#, Enc ("HIGH") & [16#A4#] & Enc ("LOW0")) &
        Method_Data ("HIGH", 16#78#, [16#A4#, 1]) &
        Method_Data ("LOW0", 16#28#, [16#A4#, 1]) &
        Method_Data ("RREC", 16#59#, [16#A0#, 11, 16#68#, 16#A4#] & Enc ("RREC") &
          [16#74#, 16#68#, 1, 0, 16#A4#, 1]) &
        Method_Data ("LOOP", 16#18#, [16#A4#] & Enc ("UP00")) &
        Method_Data ("UP00", 16#28#, [16#A4#] & Enc ("LOOP")), W, Loaded);
      Check (Loaded = NS.Loaded);
      NS.Invoke_Mutable (Tree, 1, [others => 0], 0, 1000, R);
      Check (R.Status = Returned and then R.Value = 1);
      NS.Invoke_Mutable (Tree, 4, [0 => 12, others => 0], 1, 1000, R);
      Check (R.Status = Returned and then R.Value = 1);
      NS.Invoke_Mutable (Tree, 5, [others => 0], 0, 1000, R);
      Check (R.Status = Mutex_Order);
      NS.Invoke_Mutable (Tree, 3, [others => 0], 0, 1000, R);
      Check (R.Status = Returned and then R.Value = 1);
   end loop;
   Ada.Text_IO.Put_Line ("AML-CALL-CHECK: PASS" & Checks'Image);
end Call_Tests;
