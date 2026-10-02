pragma Ada_2022;
with AML_Integers;
with AML_Logic;
package body AML_Execute with SPARK_Mode is
   use AML_Decode;
   use type AML_Decode.Byte;
   use type AML_Coercions.Conversion_Status;
   function Method_Level (Flags : Byte) return Sync_Level is
     (if (Flags and 8) /= 0 then Natural (Flags / 16) else 0);
   procedure Execute_With_Input
     (Code : Bytes; Width : Integer_Width;
      Args : Value_Arguments; Argument_Count : Natural; Budget : Natural;
      Input : aliased Read_Context; Environment : in out Context; Scope : Natural; Result_Out : out Execution_Result;
      Calls_Left : Call_Budget := 32; Current_Sync : Sync_Level := 0)
   is
      Allowed : Boolean;
      procedure Execute_Body
        with Pre => Context_Valid (Environment) and then not Result_Out'Constrained,
             Post => Context_Valid (Environment) and then Result_Out.Charged <= Budget,
             Always_Terminates,
             Subprogram_Variant => (Decreases => Calls_Left, Decreases => Natural'(2))
      is
      subtype Failure_Status is Execution_Status range No_Return .. Value_Limit;
      function Failure (Status : Failure_Status; Charged : Natural)
         return Execution_Result is ((Status => Status, Charged => Charged));
      type Local_Array is array (Natural range 0 .. 7) of Datum;
      type Local_Ready_Array is array (Natural range 0 .. 7) of Boolean;
      type Argument_Ready_Array is array (Natural range 0 .. 6) of Boolean;
      Locals : Local_Array := [others => (Is_Object => False, Number => 0)];
      Ready : Local_Ready_Array := [others => False];
      Parameters : Value_Arguments := Args;
      Supplied : Argument_Ready_Array := [for I in 0 .. 6 => I < Argument_Count];
      Offset : Natural := 0;
      Charged : Natural := 0;
      Limit : Natural := Code'Length;
      type Block_Frame is record
         Outer_Limit : Natural := 0;
         Resume : Natural := 0;
         Restart : Natural := 0;
         Is_Loop : Boolean := False;
      end record;
      Blocks : array (Positive range 1 .. 64) of Block_Frame;
      Block_Depth : Natural range 0 .. 64 := 0;
      Op : Byte;
      Value : Datum;
      State : Execution_Status;
      procedure Assign_Slot (Target : Byte; Item : Datum)
        with Global => (In_Out => (Locals, Ready, Parameters, Supplied)),
          Pre => Target in 16#60# .. 16#6E#,
          Post =>
            (if Target <= 16#67# then
               Locals = (Locals'Old with delta Natural (Target - 16#60#) => Item)
               and then Ready = (Ready'Old with delta Natural (Target - 16#60#) => True)
               and then Parameters = Parameters'Old and then Supplied = Supplied'Old
             else
               Parameters = (Parameters'Old with delta Natural (Target - 16#68#) => Item)
               and then Supplied = (Supplied'Old with delta Natural (Target - 16#68#) => True)
               and then Locals = Locals'Old and then Ready = Ready'Old)
      is
      begin
         if Target <= 16#67# then
            Locals (Natural (Target - 16#60#)) := Item;
            Ready (Natural (Target - 16#60#)) := True;
         else
            Parameters (Natural (Target - 16#68#)) := Item;
            Supplied (Natural (Target - 16#68#)) := True;
         end if;
      end Assign_Slot;
      -- Argument replacement is invocation-local;
      -- Datum currently cannot contain an AML RefOf/Index reference.
      procedure Write_Target (Item : Datum; Status : out Execution_Status)
        with Global => (Input => (Code, Limit, Scope, Width),
                        In_Out => (Offset, Locals, Ready, Parameters, Supplied, Environment)),
          Pre => Offset <= Limit and then Limit <= Code'Length and then Context_Valid (Environment),
          Post => Context_Valid (Environment) and then Offset <= Limit and then Offset >= Offset'Old
            and then Status in Returned | Truncated | Unsupported | Unknown_Name
            and then (if Status /= Returned then
              Offset = Offset'Old and then Locals = Locals'Old and then Ready = Ready'Old
              and then Parameters = Parameters'Old and then Supplied = Supplied'Old)
            and then (if Status = Returned then Offset > Offset'Old)
      is
         Target : Byte;
         Path : AML_Names.Name_Result;
         Written_Status : Write_Status;
         use type AML_Names.Parse_Status;
      begin
         if Offset = Limit then Status := Truncated; return; end if;
         Target := Code (Code'First + Offset);
         if Target in 16#60# .. 16#6E# then
            Assign_Slot (Target, Item);
         elsif Target not in 0 | 1 | 16#FF# then
            if not AML_Names.Lead (Target) and then Target not in 16#5C# | 16#5E# | 16#2E# | 16#2F# then
               Status := Unsupported; return;
            end if;
            Path := AML_Names.Read_Name
              (Code (Code'First + Offset .. Code'First + (Limit - 1)));
            if Path.Kind = AML_Names.Truncated then Status := Truncated; return; end if;
            if Path.Kind /= AML_Names.Accepted or else Path.Count = 0 then
               Status := Unsupported; return;
            end if;
            Write (Environment, Scope, Path, Width, Item, Written_Status);
            case Written_Status is
               when Write_Missing => Status := Unknown_Name; return;
               when Write_Unsupported => Status := Unsupported; return;
               when Written => null;
            end case;
            Offset := Offset + Path.Consumed;
            Status := Returned; return;
         end if;
         Offset := Offset + 1;
         Status := Returned;
      end Write_Target;
      subtype Slot_Number is Natural range 0 .. 15;
      -- Local/Arg reads are deferred until sibling expressions complete.
      -- Named integer reads capture their value when first evaluated.
      procedure Resolve_Slot (Slot : Slot_Number; Item : in out Datum; Status : out Execution_Status)
        with Global => (Input => (Locals, Ready, Parameters, Supplied)),
          Pre => not Item'Constrained,
          Post => Status in Returned | Uninitialized | Missing_Argument
            and then (if Slot = 0 then Item = Item'Old and then Status = Returned)
      is
      begin
         Status := Returned;
         if Slot in 1 .. 8 then
            if not Ready (Slot - 1) then Status := Uninitialized; return; end if;
            Item := Locals (Slot - 1);
         elsif Slot > 8 then
            if not Supplied (Slot - 9) then Status := Missing_Argument; return; end if;
            Item := Parameters (Slot - 9);
         end if;
      end Resolve_Slot;
      procedure Operand (V : out Datum; S : out Execution_Status; Allow_No_Return : Boolean := False; Require_Integer : Boolean := False)
        with Global =>
               (Input => (Input, Code, Width, Budget, Limit,
                          Scope, Calls_Left, Current_Sync),
                In_Out => (Offset, Charged, Locals, Ready, Parameters, Supplied, Environment)),
             Always_Terminates,
             Subprogram_Variant => (Decreases => Calls_Left, Decreases => Natural'(1)),
             Pre => Context_Valid (Environment) and then not V'Constrained and then Offset <= Limit and then Limit <= Code'Length and then Charged <= Budget,
             Post => Context_Valid (Environment) and then Offset <= Limit and then Limit <= Code'Length and then Charged <= Budget
               and then Charged >= Charged'Old and then Offset >= Offset'Old
               and then S /= Object_Returned
      is
         B : Byte;
         Literal : Integer_Result;
         Path : AML_Names.Name_Result;
         Bound : Binding_Result;
         use type AML_Names.Parse_Status;
         type Argument_Slots is array (Natural range 0 .. 6) of Slot_Number;
         type Frame is record
            Op : Byte := 16#72#;
            Left : Datum := (Is_Object => False, Number => 0);
            Left_Slot : Slot_Number := 0;
            Has_Left : Boolean := False;
            Is_Call : Boolean := False;
            Method_ID : Natural := 0;
            Parameters : Natural range 0 .. 7 := 0;
            Given : Natural range 0 .. 7 := 0;
            Actuals : Value_Arguments := [others => (Is_Object => False, Number => 0)];
            Slots : Argument_Slots := [others => 0];
         end record;
         Stack : array (Positive range 1 .. 64) of Frame;
         Depth : Natural range 0 .. 64 := 0;
         Have_Value : Boolean;
         Pending_Slot : Slot_Number := 0;
         Left_Item, Argument_Item : Datum;
         Conversion : AML_Coercions.Result;
         Text : String_Result;
         Buffer_Item : Buffer_Result;
         function Integer_Expected return Boolean is
           ((Depth = 0 and Require_Integer) or else
            (Depth > 0 and then not Stack (Depth).Is_Call and then
             (AML_Integers.Supported (Stack (Depth).Op)
              or else Stack (Depth).Op in 16#90# .. 16#92#
              or else (AML_Logic.Supported (Stack (Depth).Op) and Stack (Depth).Has_Left))));
         Remainder_Value : Integer_Value := 0;
         procedure Inspect (Query : Byte; Value : out Integer_Value; Status : out Execution_Status)
           with Global => (Input => (Input, Code, Limit, Budget, Width, Ready, Locals, Parameters, Supplied,
                                     Scope), In_Out => (Environment, Offset, Charged)),
                Pre => Context_Valid (Environment) and then Offset <= Limit and then Limit <= Code'Length and then Charged <= Budget,
                Post => Context_Valid (Environment) and then Offset <= Limit and then Charged <= Budget
                  and then Offset >= Offset'Old and then Charged >= Charged'Old
                  and then Status /= Object_Returned
         is
            Source : Byte;
            Type_Code : Natural range 0 .. 16 := 0;
            Size : Natural := 0;
            Name : AML_Names.Name_Result;
            Object : Binding_Result;
         begin
            Value := 0; Status := Returned;
            if Charged = Budget then Status := Budget_Exceeded; return; end if;
            Charged := Charged + 1;
            if Offset = Limit then Status := Truncated; return; end if;
            Source := Code (Code'First + Offset);
            if Source in 16#60# .. 16#67# then
               Offset := Offset + 1;
               if Ready (Natural (Source - 16#60#)) then
                  if Locals (Natural (Source - 16#60#)).Is_Object then
                     Type_Code := Locals (Natural (Source - 16#60#)).Object.Type_Code;
                     Size := Locals (Natural (Source - 16#60#)).Object.Size;
                  else Type_Code := 1; end if;
               else Type_Code := 0; end if;
            elsif Source in 16#68# .. 16#6E# then
               Offset := Offset + 1;
               if Supplied (Natural (Source - 16#68#)) then
                  if Parameters (Natural (Source - 16#68#)).Is_Object then
                     Type_Code := Parameters (Natural (Source - 16#68#)).Object.Type_Code;
                     Size := Parameters (Natural (Source - 16#68#)).Object.Size;
                  else Type_Code := 1; end if;
               else Type_Code := 0; end if;
            elsif Source = 16#5B# then
               Offset := Offset + 1;
               if Offset = Limit then Status := Truncated; return; end if;
               if Code (Code'First + Offset) /= 16#31# then Status := Unsupported; return; end if;
               Offset := Offset + 1;
               Type_Code := 16; -- Debug object, no debug output or evaluation.
            elsif AML_Names.Lead (Source) or else Source in 16#5C# | 16#5E# | 16#2E# | 16#2F# then
               Name := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + (Limit - 1)));
               if Name.Kind /= AML_Names.Accepted then
                  Status := (if Name.Kind = AML_Names.Truncated then Truncated else Bad_Name);
                  return;
               end if;
               Offset := Offset + Name.Consumed;
               Lookup (Environment, Input, Scope, Name, Width, Inspect_Binding, Object);
               case Object.Status is
                  when Failed_Binding => Status := (if Object.Failure in No_Return .. Value_Limit then Object.Failure else Unsupported_Value); return;
                  when Missing_Binding => Status := Unknown_Name; return;
                  when Integer_Binding => Type_Code := 1;
                  when Method_Binding => Type_Code := 8;
                  when Non_Integer_Binding => Type_Code := Object.Object.Type_Code; Size := Object.Object.Size;
               end case;
            else
               Status := Unsupported; return;
            end if;
            if Query = 16#8E# then
               Value := Integer_Value (Type_Code);
            elsif Type_Code = 1 then
               -- ACPICA implements the implicit Integer-to-Buffer conversion
               -- by returning the table's integer byte width.
               Value := (if Width = Bits_32 then 4 else 8);
            elsif Type_Code in 2 .. 4 then
               Value := Integer_Value (Size);
            else
               Status := (if Type_Code = 0 then Uninitialized else Unsupported_Value);
            end if;
         end Inspect;
         procedure Dispatch
           (ID : Natural; Actuals : Value_Arguments; Count : Natural;
            Need_Value : Boolean; Value : out Datum;
            Status : out Execution_Status)
           with Global => (Input => (Input, Calls_Left, Budget, Current_Sync), In_Out => (Charged, Environment)),
                Always_Terminates,
                Subprogram_Variant => (Decreases => Calls_Left, Decreases => Natural'(0)),
                Pre => Context_Valid (Environment) and then not Value'Constrained and then Count <= 7 and then Charged > 0 and then Charged <= Budget,
                Post => Context_Valid (Environment) and then Charged <= Budget and then Charged >= Charged'Old
                  and then Status /= Object_Returned
         is
            Result : Execution_Result;
         begin
            Value := (Is_Object => False, Number => 0);
            if Calls_Left = 0 then Status := Call_Limit; return; end if;
            declare
            Method : constant Method_Definition := Get_Method (Environment, ID);
            begin
            if not Method.Exists then Status := Invalid_Method; return; end if;
            if Natural (Method.Flags and 7) /= Count then
               Status := Argument_Mismatch; return;
            end if;
            if (Method.Flags and 8) /= 0 and then Current_Sync > Method_Level (Method.Flags) then
               Status := Mutex_Order; return;
            end if;
            Execute_With_Input
              (Method.Code (1 .. Method.Length), Method.Width, Actuals, Count,
               Budget - Charged, Input, Environment, Method.Scope, Result, Calls_Left - 1,
               (if (Method.Flags and 8) /= 0 then Method_Level (Method.Flags) else Current_Sync));
            Charged := Charged + Result.Charged;
            Status := Result.Status;
            if Result.Status = Returned then
               Value := (Is_Object => False, Number => Result.Value);
            elsif Result.Status = Object_Returned then
               Value := (Is_Object => True, Object => Result.Object);
               Status := Returned;
            elsif Result.Status = No_Return and Need_Value then
               Status := Missing_Result;
            end if;
            end;
         end Dispatch;
      begin
         V := (Is_Object => False, Number => 0);
         S := Returned;
         loop
            pragma Loop_Invariant (Context_Valid (Environment));
            pragma Loop_Invariant (S /= Object_Returned);
            pragma Loop_Invariant (Offset <= Limit and then Limit <= Code'Length and then Charged <= Budget);
            pragma Loop_Invariant (Offset >= Offset'Loop_Entry);
            pragma Loop_Invariant (Charged >= Charged'Loop_Entry);
            pragma Loop_Variant (Decreases => Budget - Charged);
            if Charged = Budget then S := Budget_Exceeded; return; end if;
            Charged := Charged + 1;
            if Offset = Limit then S := Truncated; return; end if;
            B := Code (Code'First + Offset);
            Have_Value := True;
            Pending_Slot := 0;
            if B = 16#70# or AML_Integers.Supported (B) or AML_Logic.Supported (B) then
               if Depth = 64 then S := Expression_Limit; return; end if;
               Depth := Depth + 1;
               Stack (Depth) := (Op => B, others => <>);
               Offset := Offset + 1;
            else
               if B in 16#87# | 16#8E# then
                  Offset := Offset + 1;
                  declare
                     N : Integer_Value;
                  begin
                     Inspect (B, N, S);
                     V := (Is_Object => False, Number => N);
                  end;
                  if S /= Returned then return; end if;
               elsif B in 16#60# .. 16#6E# then
                  Offset := Offset + 1;
                  Pending_Slot := Natural (B - 16#5F#);
                  V := (Is_Object => False, Number => 0);
               elsif B in 16#0D# | 16#11# and then Integer_Expected then
                  if B = 16#0D# then
                     Text := Read_String (Code (Code'First + Offset .. Code'First + (Limit - 1)));
                     if Text.Kind /= Accepted then
                        S := (if Text.Kind = AML_Decode.Truncated then Truncated else Unsupported_Value);
                        return;
                     end if;
                     Offset := Offset + Text.Consumed;
                     declare
                        L : constant Natural := Text.Length;
                        Data : Bytes (1 .. L);
                     begin
                        for I in Data'Range loop Data (I) := Character'Pos (Text.Text (I)); end loop;
                        Conversion := AML_Coercions.From_String (Data, Width);
                     end;
                  else
                     Buffer_Item := Read_Buffer (Code (Code'First + Offset .. Code'First + (Limit - 1)), Width);
                     if Buffer_Item.Kind /= Accepted then
                        S := (if Buffer_Item.Kind = AML_Decode.Truncated then Truncated else Unsupported_Value);
                        return;
                     end if;
                     Offset := Offset + Buffer_Item.Consumed;
                     Conversion := AML_Coercions.From_Buffer
                       (Buffer_Item.Content (1 .. Buffer_Item.Length), Width);
                  end if;
                  if Conversion.Status /= AML_Coercions.Converted then S := Empty_Buffer; return; end if;
                  V := (Is_Object => False, Number => Conversion.Value);
               elsif B in 16#0D# | 16#11# then
                  if Allow_No_Return and Depth = 0 then S := Unsupported; return; end if;
                  if B = 16#0D# then
                     Text := Read_String (Code (Code'First + Offset .. Code'First + (Limit - 1)));
                     if Text.Kind /= Accepted then
                        S := (if Text.Kind = AML_Decode.Truncated then Truncated else Unsupported_Value);
                        return;
                     end if;
                     Offset := Offset + Text.Consumed;
                     declare
                        L : constant Natural := Text.Length;
                        Data : Bytes (1 .. L);
                     begin
                        for I in Data'Range loop Data (I) := Character'Pos (Text.Text (I)); end loop;
                        Materialize (Environment, String_Literal, Data, Bound);
                     end;
                  else
                     Buffer_Item := Read_Buffer (Code (Code'First + Offset .. Code'First + (Limit - 1)), Width);
                     if Buffer_Item.Kind /= Accepted then
                        S := (if Buffer_Item.Kind = AML_Decode.Truncated then Truncated else Unsupported_Value);
                        return;
                     end if;
                     Offset := Offset + Buffer_Item.Consumed;
                     Materialize (Environment, Buffer_Literal,
                       Buffer_Item.Content (1 .. Buffer_Item.Length), Bound);
                  end if;
                  if Bound.Status = Failed_Binding then
                     S := (if Bound.Failure in No_Return .. Value_Limit then Bound.Failure else Unsupported_Value);
                     return;
                  elsif Bound.Status /= Non_Integer_Binding or else Bound.Object.ID = 0
                    or else Bound.Object.Type_Code /= (if B = 16#0D# then 2 else 3)
                  then S := Unsupported_Value; return; end if;
                  V := (Is_Object => True, Object => Bound.Object);
               elsif AML_Names.Lead (B) or else B in 16#5C# | 16#5E# | 16#2E# | 16#2F# then
                  Path := AML_Names.Read_Name
                    (Code (Code'First + Offset .. Code'First + (Limit - 1)));
                  if Path.Kind /= AML_Names.Accepted then
                     S := (if Path.Kind = AML_Names.Truncated then Truncated else Bad_Name);
                     return;
                  end if;
                  Offset := Offset + Path.Consumed;
                  Lookup (Environment, Input, Scope, Path, Width, Evaluate_Binding, Bound);
                  case Bound.Status is
                     when Failed_Binding => S := (if Bound.Failure in No_Return .. Value_Limit then Bound.Failure else Unsupported_Value); return;
                     when Integer_Binding =>
                        if Allow_No_Return and Depth = 0 then S := Unsupported; return; end if;
                        V := (Is_Object => False, Number => Bound.Value);
                     when Method_Binding =>
                        if Bound.Parameters = 0 then
                           Dispatch (Bound.Method_ID, [others => (Is_Object => False, Number => 0)], 0,
                                     not Allow_No_Return or Depth > 0, V, S);
                           if S /= Returned then return; end if;
                        else
                           if Depth = 64 then S := Expression_Limit; return; end if;
                           Depth := Depth + 1;
                           Stack (Depth) := (Is_Call => True,
                             Method_ID => Bound.Method_ID, Parameters => Bound.Parameters,
                             others => <>);
                           Have_Value := False;
                        end if;
                     when Missing_Binding => S := Unknown_Name; return;
                     when Non_Integer_Binding =>
                        if Bound.Object.Type_Code not in 2 .. 4 or else Bound.Object.ID = 0 then
                           S := Unsupported_Value; return;
                        end if;
                        if Allow_No_Return and Depth = 0 then S := Unsupported; return; end if;
                        V := (Is_Object => True, Object => Bound.Object);
                  end case;
               else
                  Literal := Read_Integer (Code (Code'First + Offset .. Code'First + (Limit - 1)), Width);
                  if Literal.Kind /= Accepted then
                     S := (if Literal.Kind = AML_Decode.Truncated then
                             AML_Execute.Truncated else AML_Execute.Unsupported);
                     return;
                  end if;
                  V := (Is_Object => False, Number => Literal.Value);
                  Offset := Offset + Literal.Consumed;
               end if;
               if Have_Value then
               loop
                  pragma Loop_Invariant (Context_Valid (Environment));
                  pragma Loop_Invariant (S /= Object_Returned);
                  pragma Loop_Invariant (Charged > 0 and then Charged <= Budget);
                  pragma Loop_Invariant (Charged >= Charged'Loop_Entry);
                  pragma Loop_Invariant (Offset <= Limit and then Limit <= Code'Length);
                  pragma Loop_Invariant (Offset >= Offset'Loop_Entry);
                  pragma Loop_Variant (Decreases => Depth);
                  if Depth = 0 then
                     Resolve_Slot (Pending_Slot, V, S);
                     if S /= Returned then return; end if;
                     if V.Is_Object and Require_Integer then
                        Conversion := (if Width = Bits_32 then V.Object.Conversion_32 else V.Object.Conversion_64);
                        case Conversion.Status is
                           when AML_Coercions.Converted => V := (Is_Object => False, Number => Conversion.Value);
                           when AML_Coercions.Empty_Buffer => S := Empty_Buffer; return;
                           when AML_Coercions.Not_Convertible => S := Unsupported_Value; return;
                        end case;
                     end if;
                     return;
                  end if;
                  if Stack (Depth).Is_Call then
                     if Stack (Depth).Given >= Stack (Depth).Parameters then
                        S := Argument_Mismatch; return;
                     end if;
                     Stack (Depth).Actuals (Stack (Depth).Given) := V;
                     Stack (Depth).Slots (Stack (Depth).Given) := Pending_Slot;
                     Stack (Depth).Given := Stack (Depth).Given + 1;
                     if Stack (Depth).Given < Stack (Depth).Parameters then exit; end if;
                     for I in 0 .. Stack (Depth).Given - 1 loop
                        Argument_Item := Stack (Depth).Actuals (I);
                        Resolve_Slot (Stack (Depth).Slots (I), Argument_Item, S);
                        if S /= Returned then return; end if;
                        Stack (Depth).Actuals (I) := Argument_Item;
                     end loop;
                     Dispatch (Stack (Depth).Method_ID, Stack (Depth).Actuals,
                               Stack (Depth).Given, not Allow_No_Return or Depth > 1, V, S);
                     if S /= Returned then return; end if;
                  elsif Stack (Depth).Op = 16#70# then
                     Resolve_Slot (Pending_Slot, V, S);
                     if S /= Returned then return; end if;
                     Write_Target (V, S);
                     if S /= Returned then return; end if;
                  else
                  if not (AML_Logic.Unary (Stack (Depth).Op) or AML_Integers.Unary (Stack (Depth).Op))
                    and then not Stack (Depth).Has_Left then
                     Stack (Depth).Left := V;
                     Stack (Depth).Left_Slot := Pending_Slot;
                     Stack (Depth).Has_Left := True;
                     exit;
                  end if;
                  Left_Item := Stack (Depth).Left;
                  Resolve_Slot (Stack (Depth).Left_Slot, Left_Item, S);
                  if S /= Returned then return; end if;
                  Resolve_Slot (Pending_Slot, V, S);
                  if S /= Returned then return; end if;
                  if Left_Item.Is_Object then
                     if Stack (Depth).Op in 16#93# .. 16#95# then S := Unsupported_Value; return; end if;
                     Conversion := (if Width = Bits_32 then Left_Item.Object.Conversion_32 else Left_Item.Object.Conversion_64);
                     case Conversion.Status is
                        when AML_Coercions.Converted => Left_Item := (Is_Object => False, Number => Conversion.Value);
                        when AML_Coercions.Empty_Buffer => S := Empty_Buffer; return;
                        when AML_Coercions.Not_Convertible => S := Unsupported_Value; return;
                     end case;
                  end if;
                  if V.Is_Object then
                     Conversion := (if Width = Bits_32 then V.Object.Conversion_32 else V.Object.Conversion_64);
                     case Conversion.Status is
                        when AML_Coercions.Converted => V := (Is_Object => False, Number => Conversion.Value);
                        when AML_Coercions.Empty_Buffer => S := Empty_Buffer; return;
                        when AML_Coercions.Not_Convertible => S := Unsupported_Value; return;
                     end case;
                  end if;
                  if AML_Logic.Supported (Stack (Depth).Op) then
                     V := (Is_Object => False, Number => AML_Logic.Apply (Stack (Depth).Op, Left_Item.Number, V.Number, Width));
                  else
                     if not AML_Integers.Supported (Stack (Depth).Op) then
                        S := Unsupported; return;
                     end if;
                     if Stack (Depth).Op in 16#78# | 16#85# and then V.Number = 0 then
                        S := Division_By_Zero; return;
                     end if;
                     if Stack (Depth).Op = 16#78# then
                        Remainder_Value := Left_Item.Number mod V.Number;
                     end if;
                     V := (Is_Object => False, Number => AML_Integers.Apply (Stack (Depth).Op, Left_Item.Number, V.Number, Width));
                     for Target in 1 .. (if Stack (Depth).Op = 16#78# then 2 else 1) loop
                        pragma Loop_Invariant (Context_Valid (Environment));
                        pragma Loop_Invariant (Offset <= Limit);
                        pragma Loop_Invariant (Offset >= Offset'Loop_Entry);
                        Write_Target
                          ((if Stack (Depth).Op = 16#78# and Target = 1 then
                              (Is_Object => False, Number => Remainder_Value) else V), S);
                        if S /= Returned then return; end if;
                     end loop;
                  end if;
                  end if;
                  Pending_Slot := 0;
                  Depth := Depth - 1;
               end loop;
               end if;
            end if;
         end loop;
      end Operand;
   begin
      loop
         pragma Loop_Invariant (Context_Valid (Environment));
         pragma Loop_Invariant (Offset <= Limit and then Limit <= Code'Length and then Charged <= Budget);
         pragma Loop_Invariant
           (for all I in 1 .. Block_Depth =>
              Blocks (I).Resume <= Blocks (I).Outer_Limit and then
              Blocks (I).Restart <= Blocks (I).Outer_Limit and then
              Blocks (I).Outer_Limit <= Code'Length);
         pragma Loop_Variant (Decreases => Budget - Charged, Decreases => Block_Depth);
         if Offset = Limit then
            exit when Block_Depth = 0;
            Offset := (if Blocks (Block_Depth).Is_Loop then
                         Blocks (Block_Depth).Restart else Blocks (Block_Depth).Resume);
            Limit := Blocks (Block_Depth).Outer_Limit;
            Block_Depth := Block_Depth - 1;
         else
         if Charged = Budget then Result_Out := Failure (Budget_Exceeded, Charged); return; end if;
         Charged := Charged + 1;
         Op := Code (Code'First + Offset);
         Offset := Offset + 1;
         case Op is
            when 16#5B# =>
               if Offset = Limit then Result_Out := Failure (Truncated, Charged); return; end if;
               if Code (Code'First + Offset) /= 16#81# then
                  Result_Out := Failure (Unsupported, Charged); return;
               end if;
               Offset := Offset + 1;
               if Offset = Limit then Result_Out := Failure (Bad_Package, Charged); return; end if;
               declare
                  use type AML_Names.Parse_Status;
                  P : constant Package_Result := Read_Package
                    (Code (Code'First + Offset .. Code'First + (Limit - 1)));
                  Finish : Natural;
                  Region : AML_Names.Name_Result;
                  Flags : Byte;
                  Declared_Status : Execution_Status;
               begin
                  if P.Kind /= Accepted then Result_Out := Failure (Bad_Package, Charged); return; end if;
                  Finish := Offset + P.Extent;
                  Offset := Offset + P.Encoding_Bytes;
                  if Offset = Finish then Result_Out := Failure (Bad_Name, Charged); return; end if;
                  Region := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + (Finish - 1)));
                  if Region.Kind /= AML_Names.Accepted then Result_Out := Failure (Bad_Name, Charged); return; end if;
                  Offset := Offset + Region.Consumed;
                  if Offset = Finish then Result_Out := Failure (Truncated, Charged); return; end if;
                  Flags := Code (Code'First + Offset);
                  Offset := Offset + 1;
                  -- Charge every FieldList byte before any namespace mutation.
                  if Finish - Offset > Budget - Charged then
                     Result_Out := Failure (Budget_Exceeded, Charged); return;
                  end if;
                  Charged := Charged + (Finish - Offset);
                  if Offset = Finish then
                     Define_Fields (Environment, Scope, Region, Flags, [], Declared_Status);
                  else
                     Define_Fields (Environment, Scope, Region, Flags,
                       Code (Code'First + Offset .. Code'First + (Finish - 1)), Declared_Status);
                  end if;
                  if Declared_Status /= Returned then
                     Result_Out := Failure
                       ((if Declared_Status in Failure_Status then Declared_Status else Unsupported), Charged);
                     return;
                  end if;
                  Offset := Finish;
               end;
            when 16#14# => -- Method: declaration takes effect when executed.
               declare
                  P : Package_Result;
                  Path : AML_Names.Name_Result;
                  Finish : Natural;
                  Flags : Byte;
                  Declared_Status : Declaration_Status;
                  use type AML_Names.Parse_Status;
               begin
                  if Offset = Limit then Result_Out := Failure (Bad_Package, Charged); return; end if;
                  P := Read_Package (Code (Code'First + Offset .. Code'First + (Limit - 1)));
                  if P.Kind /= Accepted then Result_Out := Failure (Bad_Package, Charged); return; end if;
                  Finish := Offset + P.Extent;
                  Offset := Offset + P.Encoding_Bytes;
                  if Offset = Finish then Result_Out := Failure (Truncated, Charged); return; end if;
                  Path := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + (Finish - 1)));
                  if Path.Kind /= AML_Names.Accepted or else Path.Count = 0 then
                     Result_Out := Failure (Unsupported, Charged); return;
                  end if;
                  Offset := Offset + Path.Consumed;
                  if Offset = Finish then Result_Out := Failure (Truncated, Charged); return; end if;
                  Flags := Code (Code'First + Offset);
                  Offset := Offset + 1;
                  if Offset = Finish then
                     Define_Method (Environment, Scope, Path, Flags, Width, [], Declared_Status);
                  else
                     Define_Method (Environment, Scope, Path, Flags, Width,
                       Code (Code'First + Offset .. Code'First + (Finish - 1)), Declared_Status);
                  end if;
                  case Declared_Status is
                     when Declared => null;
                     when Declaration_Duplicate => Result_Out := Failure (Duplicate_Name, Charged); return;
                     when Declaration_Missing => Result_Out := Failure (Unknown_Name, Charged); return;
                     when Declaration_Full => Result_Out := Failure (Namespace_Limit, Charged); return;
                     when Declaration_Unsupported => Result_Out := Failure (Unsupported, Charged); return;
                  end case;
                  Offset := Finish;
               end;
            when 16#A0# | 16#A2# => --  If / While
               declare
                  P : Package_Result;
                  If_End, Else_Start, Resume, Outer : Natural;
                  Has_Else : Boolean := False;
                  Start : constant Natural := Offset - 1;
                  Is_Loop : constant Boolean := Op = 16#A2#;
               begin
                  if Offset = Limit then Result_Out := Failure (Bad_Package, Charged); return; end if;
                  P := Read_Package (Code (Code'First + Offset .. Code'First + (Limit - 1)));
                  if P.Kind /= Accepted then Result_Out := Failure (Bad_Package, Charged); return; end if;
                  If_End := Offset + P.Extent;
                  Offset := Offset + P.Encoding_Bytes;
                  Resume := If_End;
                  Else_Start := If_End;
                  if not Is_Loop and then If_End < Limit and then Code (Code'First + If_End) = 16#A1# then
                     Has_Else := True;
                     Else_Start := If_End + 1;
                     if Else_Start = Limit then Result_Out := Failure (Bad_Package, Charged); return; end if;
                     P := Read_Package (Code (Code'First + Else_Start .. Code'First + (Limit - 1)));
                     if P.Kind /= Accepted then Result_Out := Failure (Bad_Package, Charged); return; end if;
                     Resume := Else_Start + P.Extent;
                     Else_Start := Else_Start + P.Encoding_Bytes;
                  end if;
                  Outer := Limit;
                  Limit := If_End;
                  Operand (Value, State, Require_Integer => True);
                  if State /= Returned then Result_Out := Failure (State, Charged); return; end if;
                  --  Predicate conversion observes the AML integer width.
                  if Value.Is_Object then Result_Out := Failure (Unsupported_Value, Charged); return; end if;
                  Value := (Is_Object => False, Number => AML_Integers.Normalize (Value.Number, Width));
                  if Value.Number = 0 then
                     if Has_Else then
                        Offset := Else_Start;
                        Limit := Resume;
                     else
                        Offset := Resume;
                        Limit := Outer;
                     end if;
                  end if;
                  if Value.Number /= 0 or Has_Else then
                     if Block_Depth = 64 then Result_Out := Failure (Block_Limit, Charged); return; end if;
                     Block_Depth := Block_Depth + 1;
                     Blocks (Block_Depth) :=
                       (Outer_Limit => Outer, Resume => Resume,
                        Restart => Start, Is_Loop => Is_Loop);
                  end if;
               end;
            when 16#A5# | 16#9F# => --  Break / Continue, nearest loop only.
               declare
                  Found : Boolean := False;
               begin
                  for I in reverse 1 .. Block_Depth loop
                     if Blocks (I).Is_Loop then
                        Offset := (if Op = 16#A5# then Blocks (I).Resume
                                   else Blocks (I).Restart);
                        Limit := Blocks (I).Outer_Limit;
                        Block_Depth := I - 1;
                        Found := True;
                        exit;
                     end if;
                  end loop;
                  if not Found then Result_Out := Failure (Invalid_Control, Charged); return; end if;
               end;
            when 16#A3# => null; --  Noop
            when 16#A4# | 16#70# => --  Return / Store
               Operand (Value, State);
               if State /= Returned then Result_Out := Failure (State, Charged); return; end if;
               if Op = 16#A4# then
                  if Value.Is_Object then
                     Result_Out := (Status => Object_Returned, Charged => Charged, Object => Value.Object); return;
                  end if;
                  Result_Out := (Status => Returned, Charged => Charged, Value => Value.Number); return;
               end if;
               Write_Target (Value, State);
               if State /= Returned then Result_Out := Failure (State, Charged); return; end if;
            when 16#72# | 16#74# | 16#77# .. 16#82# | 16#85#
               | 16#87# | 16#8E# | 16#90# .. 16#95# =>
               Offset := Offset - 1;
               Operand (Value, State);
               if State /= Returned then Result_Out := Failure (State, Charged); return; end if;
            when 16#41# .. 16#5A# | 16#5F# | 16#5C# | 16#5E# | 16#2E# | 16#2F# =>
               Offset := Offset - 1;
               Operand (Value, State, Allow_No_Return => True);
               if State /= Returned and State /= No_Return then
                  Result_Out := Failure (State, Charged); return;
               end if;
            when others => Result_Out := Failure (Unsupported, Charged); return;
         end case;
         end if;
      end loop;
      Result_Out := Failure (No_Return, Charged); return;
      end Execute_Body;
   begin
      Begin_Call (Environment, Scope, Allowed);
      if not Allowed then
         Result_Out := (Status => Namespace_Limit, Charged => 0); return;
      end if;
      Execute_Body;
      End_Call (Environment, Scope);
   end Execute_With_Input;

   procedure Execute_Typed
     (Code : Bytes; Width : Integer_Width;
      Args : Value_Arguments; Argument_Count : Natural; Budget : Natural;
      Environment : in out Context; Scope : Natural; Result_Out : out Execution_Result;
      Calls_Left : Call_Budget := 32; Current_Sync : Sync_Level := 0)
   is
      type No_Input is limited null record;
      Empty : aliased No_Input;
      procedure Read_Binding
        (Environment : in out Context; Input : aliased No_Input; Scope : Natural;
         Path : AML_Names.Name_Result; Width : Integer_Width;
         Purpose : Binding_Purpose; Binding : out Binding_Result)
        with Pre => Context_Valid (Environment) and then not Binding'Constrained,
             Post => Context_Valid (Environment)
      is
         pragma Unreferenced (Input, Purpose);
      begin
         Binding := Lookup (Environment, Scope, Path, Width);
      end Read_Binding;
      procedure Reject_Fields
        (Environment : in out Context; Scope : Natural; Region : AML_Names.Name_Result;
         Flags : Byte; Entries : Bytes; Status : out Execution_Status)
        with Pre => Context_Valid (Environment), Post => Context_Valid (Environment)
      is
         pragma Unreferenced (Scope, Region, Flags, Entries);
      begin
         Status := Unsupported;
      end Reject_Fields;
      procedure Reject_Literal
        (Environment : in out Context; Kind : Literal_Kind; Data : Bytes;
         Binding : out Binding_Result)
        with Pre => Context_Valid (Environment) and then not Binding'Constrained,
             Post => Context_Valid (Environment)
      is
         pragma Unreferenced (Kind, Data);
      begin
         Binding := (Status => Failed_Binding, Failure => Unsupported_Value);
      end Reject_Literal;
      procedure Execute is new Execute_With_Input
        (Context, No_Input, Context_Valid, Read_Binding, Get_Method, Write,
         Begin_Call, End_Call, Define_Method, Reject_Fields, Reject_Literal);
   begin
      Execute (Code, Width, Args, Argument_Count, Budget, Empty, Environment,
               Scope, Result_Out, Calls_Left, Current_Sync);
   end Execute_Typed;

   function Run_Typed
     (Code : Bytes; Width : Integer_Width;
      Args : Value_Arguments; Argument_Count : Natural; Budget : Natural;
      Environment : Context; Scope : Natural; Calls_Left : Call_Budget := 32; Current_Sync : Sync_Level := 0)
      return Execution_Result
   is
      procedure Reject_Write
        (Environment : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
         Width : Integer_Width; Item : Datum; Status : out Write_Status)
        with Pre => Context_Valid (Environment), Post => Context_Valid (Environment)
      is
         pragma Unreferenced (Scope, Path, Width, Item);
      begin
         Status := Write_Unsupported;
      end Reject_Write;
      procedure Begin_Read (Environment : in out Context; Scope : Natural; Allowed : out Boolean)
        with Pre => Context_Valid (Environment), Post => Context_Valid (Environment)
      is
         pragma Unreferenced (Scope);
      begin
         Allowed := True;
      end Begin_Read;
      procedure End_Read (Environment : in out Context; Scope : Natural)
        with Pre => Context_Valid (Environment), Post => Context_Valid (Environment)
      is
         pragma Unreferenced (Scope);
      begin
         null;
      end End_Read;
      procedure Reject_Definition
        (Environment : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
         Flags : Byte; Width : Integer_Width; Code : Bytes; Status : out Declaration_Status)
        with Pre => Context_Valid (Environment), Post => Context_Valid (Environment)
      is
         pragma Unreferenced (Scope, Path, Flags, Width, Code);
      begin
         Status := Declaration_Unsupported;
      end Reject_Definition;
      procedure Execute is new Execute_Typed
        (Context, Context_Valid, Lookup, Get_Method, Reject_Write, Begin_Read, End_Read, Reject_Definition);
      Copy : Context := Environment;
      Result : Execution_Result;
   begin
      Execute (Code, Width, Args, Argument_Count, Budget, Copy, Scope, Result, Calls_Left, Current_Sync);
      return Result;
   end Run_Typed;

   function As_Values (Args : Arguments) return Value_Arguments is
      Result : Value_Arguments := [others => (Is_Object => False, Number => 0)];
   begin
      for I in Args'Range loop
         Result (I) := (Is_Object => False, Number => Args (I));
         pragma Loop_Invariant (for all J in Args'First .. I =>
           not Result (J).Is_Object and then Result (J).Number = Args (J));
      end loop;
      return Result;
   end As_Values;

   function Run_Bound
     (Code : Bytes; Width : Integer_Width;
      Args : Arguments; Argument_Count : Natural; Budget : Natural;
      Environment : Context; Scope : Natural; Calls_Left : Call_Budget := 32; Current_Sync : Sync_Level := 0)
      return Execution_Result
   is
      function Execute is new Run_Typed (Context, Context_Valid, Lookup, Get_Method);
   begin
      return Execute (Code, Width, As_Values (Args), Argument_Count, Budget,
                      Environment, Scope, Calls_Left, Current_Sync);
   end Run_Bound;

   type Empty_Context is null record;
   function No_Binding
     (Environment : Empty_Context; Scope : Natural; Path : AML_Names.Name_Result;
      Width : AML_Decode.Integer_Width)
      return Binding_Result
   is
      pragma Unreferenced (Environment, Scope, Path, Width);
   begin
      return (Status => Missing_Binding);
   end No_Binding;
   function No_Method (Environment : Empty_Context; ID : Natural)
      return Method_Definition
   is
      pragma Unreferenced (Environment, ID);
   begin
      return (Exists => False, Length => 0);
   end No_Method;
   function Empty_Valid (Environment : Empty_Context) return Boolean
     with Post => Empty_Valid'Result
   is
      pragma Unreferenced (Environment);
   begin
      return True;
   end Empty_Valid;
   function Unbound is new Run_Bound (Empty_Context, Empty_Valid, No_Binding, No_Method);
   function Run
     (Code : Bytes; Width : Integer_Width;
      Args : Arguments; Argument_Count : Natural; Budget : Natural)
      return Execution_Result is
     (Unbound (Code, Width, Args, Argument_Count, Budget, (null record), 0));
end AML_Execute;
