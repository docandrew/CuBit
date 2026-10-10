pragma Ada_2022;
with AML_Delays;
with AML_Data;
with AML_Frame_Handles;
with AML_References;
with Ada.Text_IO;
with AML_Execute; use AML_Execute;
with AML_Decode; use AML_Decode;
with AML_Names;
with AML_Integers;
procedure Readonly_Input_Tests is
   use type AML_Names.Parse_Status;
   use type Integer_Value;
   use type Byte;
   type Mutable is limited record
      Active : Natural := 0;
   end record;
   type Backing (Length : Positive) is limited record
      Data : Bytes (1 .. Length) := [others => 0];
   end record;
   function Valid (Environment : Mutable) return Boolean is (Environment.Active <= 33);
   procedure Lookup
     (Environment : in out Mutable; Input : aliased Backing; Scope : Natural;
      Path : AML_Names.Name_Result; Width : Integer_Width;
      Purpose : Binding_Purpose; Binding : out Binding_Result)
   is
      pragma Unreferenced (Environment, Scope);
   begin
      if Path.Kind /= AML_Names.Accepted or else Path.Count /= 1 then
         Binding := (Status => Missing_Binding); return;
      elsif Path.Parts (1) = "FLD0" and then Purpose = Inspect_Binding then
         Binding := (Status => Non_Integer_Binding, Object => (Type_Code => 5, others => <>)); return;
      elsif Path.Parts (1) = "FLD0" then
         Binding := (Status => Integer_Binding,
                 Value => AML_Integers.Normalize
                   (16#1_0000_0000# + Integer_Value (Input.Data (Input.Length)), Width), Origin => AML_Decode.Ordinary_Integer); return;
      elsif Path.Parts (1) = "MTH0" then
         Binding := (Status => Method_Binding, Method_ID => 1, Parameters => 0); return;
      elsif Path.Parts (1) = "MTH1" then
         Binding := (Status => Method_Binding, Method_ID => 2, Parameters => 0); return;
      else
         Binding := (Status => Missing_Binding); return;
      end if;
   end Lookup;
   function Get_Method (Environment : Mutable; ID : Natural) return Method_Definition is
      pragma Unreferenced (Environment);
   begin
      if ID = 1 then
         return (Exists => True, Length => 5, Code => [16#A4#, 70, 76, 68, 48],
                 Width => Bits_64, Flags => 0, Scope => 1);
      elsif ID = 2 then
         return (Exists => True, Length => 5, Code => [16#A4#, 77, 84, 72, 48],
                 Width => Bits_64, Flags => 0, Scope => 2);
      else
         return (Exists => False, Length => 0);
      end if;
   end Get_Method;
   procedure Write
     (Environment : in out Mutable; Scope : Natural; Path : AML_Names.Name_Result;
      Width : Integer_Width; Item : Datum; Status : out Write_Status) is
      pragma Unreferenced (Environment, Scope, Path, Width, Item);
   begin
      Status := Write_Unsupported;
   end Write;
   procedure Begin_Call (Environment : in out Mutable; Scope : Natural; Allowed : out Boolean) is
      pragma Unreferenced (Scope);
   begin
      Allowed := Environment.Active < 33;
      if Allowed then Environment.Active := Environment.Active + 1; end if;
   end Begin_Call;
   procedure End_Call (Environment : in out Mutable; Scope : Natural) is
      pragma Unreferenced (Scope);
   begin
      Environment.Active := Environment.Active - 1;
   end End_Call;
   procedure Define_Method
     (Environment : in out Mutable; Scope : Natural; Path : AML_Names.Name_Result;
      Flags : Byte; Width : Integer_Width; Code : Bytes; Status : out Declaration_Status) is
      pragma Unreferenced (Environment, Scope, Path, Flags, Width, Code);
   begin
      Status := Declaration_Unsupported;
   end Define_Method;
   procedure Reject_Fields
     (Environment : in out Mutable; Scope : Natural; Region : AML_Names.Name_Result;
      Flags : AML_Decode.Byte; Entries : AML_Decode.Bytes; Status : out Execution_Status)
   is
      pragma Unreferenced (Environment, Scope, Region, Flags, Entries);
   begin
      Status := Unsupported;
   end Reject_Fields;
   procedure Reject_Literal
     (Environment : in out Mutable; Scope : Natural; Kind : Literal_Kind; Width : Integer_Width; Data : Bytes; Binding : out Binding_Result)
   is
      pragma Unreferenced (Environment, Scope, Kind, Width, Data);
   begin
      Binding := (Status => Failed_Binding, Failure => Unsupported_Value);
   end Reject_Literal;
   procedure Reject_Region
     (Environment : in out Mutable; Scope : Natural; Path : AML_Names.Name_Result;
      Token : out Natural; Status : out Execution_Status)
   is
      pragma Unreferenced (Environment, Scope, Path);
   begin
      Token := 0; Status := Unsupported;
   end Reject_Region;
   procedure Reject_Completion
     (Environment : in out Mutable; Input : aliased Backing; Token : Natural;
      Width : Integer_Width; Signature, OEM, Table_ID : Datum; Status : out Execution_Status)
   is
      pragma Unreferenced (Environment, Input, Token, Width, Signature, OEM, Table_ID);
   begin
      Status := Unsupported;
   end Reject_Completion;
   procedure No_Timer (Environment : in out Mutable; Value : out Integer_Value; Available : out Boolean) is
      pragma Unreferenced (Environment);
   begin Value := 0; Available := False; end No_Timer;
   -- This read-only fixture owns no object arena; new object callbacks
   -- reject requests explicitly rather than fabricating reference authority.
   procedure Resolve_Reference
     (Environment : in out Mutable; Ref : AML_References.Reference;
      Value : out Datum; Status : out Execution_Status)
   is
      pragma Unreferenced (Environment, Ref);
   begin Value := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer); Status := Unsupported_Value; end Resolve_Reference;
   procedure Create_Index
     (Environment : in out Mutable; Source : AML_References.Object_Handle;
      Index : Integer_Value; Ref : out AML_References.Reference; Status : out Execution_Status)
   is
      pragma Unreferenced (Environment, Source, Index);
   begin Ref := AML_References.No_Reference; Status := Unsupported_Value; end Create_Index;
   procedure Store_Reference
     (Environment : in out Mutable; Ref : AML_References.Reference; Width : Integer_Width;
      Item : Datum; Status : out Execution_Status; Mode : Reference_Store_Mode)
   is
      pragma Unreferenced (Environment, Ref, Width, Item, Mode);
   begin Status := Unsupported_Value; end Store_Reference;
   procedure Clone_Value
     (Environment : in out Mutable; Width : Integer_Width; Item : Datum;
      Copy : out Datum; Status : out Execution_Status)
   is
      pragma Unreferenced (Environment, Width, Item);
   begin Copy := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer); Status := Unsupported_Value; end Clone_Value;
   -- This fixture owns no arena; comparison cannot authenticate object handles.
   procedure Compare_Objects
     (Environment : in out Mutable; Op : Byte; Left, Right : Datum;
      Width : Integer_Width; Value : out Integer_Value; Status : out Execution_Status)
   is
      pragma Unreferenced (Environment, Op, Left, Right, Width);
   begin Value := 0; Status := Unsupported_Value; end Compare_Objects;
   procedure Keep_Value
     (E : Mutable; Item : in out Datum; Status : out Execution_Status)
   is
      pragma Unreferenced (E, Item);
   begin Status := Returned; end Keep_Value;
   -- This focused fixture has no owned arena and cannot issue reference domains.
   procedure No_Invocation
     (Environment : in out Mutable;
      Domain : out AML_Frame_Handles.Invocation_Domain;
      Status : out Invocation_Status)
   is
      pragma Unreferenced (Environment);
   begin
      Domain := AML_Frame_Handles.No_Domain;
      Status := Unsupported_Context;
   end No_Invocation;
   procedure Reject_Name
     (Environment : in out Mutable; Scope : Natural;
      Path : AML_Names.Name_Result; Width : Integer_Width; Data : Bytes;
      Consumed : out Natural; Status : out Execution_Status)
   is
      pragma Unreferenced (Environment, Scope, Path, Width, Data);
   begin
      Consumed := 0;
      Status := Unsupported;
   end Reject_Name;
   procedure Reject_Copy_Attachment
     (Environment : in out Mutable; Destination : Copy_Destination;
      Width : Integer_Width; Item : Datum; Copy : out Datum;
      Status : out Execution_Status)
   is
      pragma Unreferenced (Environment, Destination, Width, Item);
   begin
      Copy := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
      Status := Unsupported_Value;
   end Reject_Copy_Attachment;
   procedure Describe_Identity
     (E : Mutable; Ref : AML_References.Reference;
      Result : out AML_Execute.Reference_Metadata)
     with Pre => not Result'Constrained
   is
      pragma Unreferenced (E, Ref);
   begin Result := (Kind => AML_Execute.Continue_Reference); end Describe_Identity;
   procedure Reserve_Dynamic_Name
     (E : in out Mutable; Scope : Natural; Path : AML_Names.Name_Result;
      Token : out Boolean; Status : out AML_Execute.Execution_Status)
   is
      pragma Unreferenced (E, Scope, Path);
   begin Token := False; Status := AML_Execute.Unsupported_Value; end Reserve_Dynamic_Name;
   procedure Complete_Dynamic_Buffer
     (E : in out Mutable; Token : Boolean; Width : AML_Decode.Integer_Width;
      Initializer : AML_Decode.Bytes; Count : AML_Data.Count_Result;
      Status : out AML_Execute.Execution_Status)
   is
      pragma Unreferenced (E, Token, Width, Initializer, Count);
   begin Status := AML_Execute.Unsupported_Value; end Complete_Dynamic_Buffer;
   procedure Abort_Dynamic_Name
     (E : in out Mutable; Token : Boolean; Status : out AML_Execute.Execution_Status)
   is
      pragma Unreferenced (E, Token);
   begin Status := AML_Execute.Unsupported_Value; end Abort_Dynamic_Name;


   procedure Reject_Explicit_Integer
        (Environment : Mutable; Width : AML_Decode.Integer_Width;
         Item : in out AML_Execute.Datum; Status : out AML_Execute.Execution_Status)
      is
         pragma Unreferenced (Environment, Width);
      begin
         Status := (if Item.Value_Kind = AML_Execute.Integer_Datum then
           AML_Execute.Returned else AML_Execute.Unsupported_Value);
      end Reject_Explicit_Integer;
      procedure Disabled_Debug
     (Environment : Mutable; Scope, Position : Natural;
      Width : AML_Decode.Integer_Width; Value : AML_Execute.Datum)
   is
      pragma Unreferenced (Environment, Scope, Position, Width, Value);
   begin null; end Disabled_Debug;
   -- This focused caller has no owned concatenation implementation.
   procedure Reject_Concatenation
     (Environment : in out Mutable; Width : AML_Decode.Integer_Width;
      Left, Right : AML_Execute.Concatenation_Operand;
      Destination : AML_Execute.Concatenation_Destination;
      Result_Value, Cell_Value : out AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
   is
      pragma Unreferenced (Environment, Width, Left, Right, Destination);
   begin
      Result_Value := (Value_Kind => AML_Execute.Integer_Datum, Number => 0,
                       Origin => AML_Decode.Ordinary_Integer);
      Cell_Value := Result_Value;
      Status := AML_Execute.Unsupported_Value;
   end Reject_Concatenation;
   procedure Reject_To_Buffer
     (Environment : in out Mutable; Width : AML_Decode.Integer_Width;
      Item : AML_Execute.Datum;
      Destination : AML_Execute.Concatenation_Destination;
      Result_Value, Cell_Value : out AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
   is
      pragma Unreferenced (Environment, Width, Item, Destination);
   begin
      Result_Value := (Value_Kind => AML_Execute.Integer_Datum, Number => 0,
                       Origin => AML_Decode.Ordinary_Integer);
      Cell_Value := Result_Value;
      Status := AML_Execute.Unsupported_Value;
   end Reject_To_Buffer;
   procedure Reject_Mid
     (Environment : in out Mutable; Width : AML_Decode.Integer_Width;
      Item : AML_Execute.Datum; Start, Count : AML_Decode.Integer_Value;
      Destination : AML_Execute.Concatenation_Destination;
      Result_Value, Cell_Value : out AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
   is
      pragma Unreferenced (Environment, Width, Item, Start, Count, Destination);
   begin
      Result_Value := (Value_Kind => AML_Execute.Integer_Datum, Number => 0,
                       Origin => AML_Decode.Ordinary_Integer);
      Cell_Value := Result_Value;
      Status := AML_Execute.Unsupported_Value;
   end Reject_Mid;
   procedure Reject_To_String
     (Environment : in out Mutable; Width : AML_Decode.Integer_Width;
      Item : AML_Execute.Datum; Length : AML_Decode.Integer_Value;
      Destination : AML_Execute.Concatenation_Destination;
      Result_Value, Cell_Value : out AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
   is
      pragma Unreferenced (Environment, Width, Item, Length, Destination);
   begin
      Result_Value := (Value_Kind => AML_Execute.Integer_Datum, Number => 0,
                       Origin => AML_Decode.Ordinary_Integer);
      Cell_Value := Result_Value;
      Status := AML_Execute.Unsupported_Value;
   end Reject_To_String;
   procedure Reject_Format_String
     (Environment : in out Mutable; Width : AML_Decode.Integer_Width;
      Mode : AML_Execute.Explicit_String_Mode; Item : AML_Execute.Datum;
      Destination : AML_Execute.Concatenation_Destination;
      Result_Value, Cell_Value : out AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
   is
      pragma Unreferenced (Environment, Width, Mode, Item, Destination);
   begin
      Result_Value := (Value_Kind => AML_Execute.Integer_Datum, Number => 0,
                       Origin => AML_Decode.Ordinary_Integer);
      Cell_Value := Result_Value;
      Status := AML_Execute.Unsupported_Value;
   end Reject_Format_String;
   procedure Reject_Resources
     (Environment : in out Mutable; Width : AML_Decode.Integer_Width;
      Left, Right : AML_Execute.Datum;
      Destination : AML_Execute.Concatenation_Destination;
      Result_Value, Cell_Value : out AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
   is
      pragma Unreferenced (Environment, Width, Left, Right, Destination);
   begin
      Result_Value := (Value_Kind => AML_Execute.Integer_Datum, Number => 0,
                       Origin => AML_Decode.Ordinary_Integer);
      Cell_Value := Result_Value;
      Status := AML_Execute.Unsupported_Value;
   end Reject_Resources;
   procedure Reject_Match
     (Environment : Mutable; Width : AML_Decode.Integer_Width;
      Package_Value, Match_1, Match_2 : AML_Execute.Datum;
      Operation_1, Operation_2 : AML_Execute.Match_Operation;
      Start : AML_Decode.Integer_Value; Max_Visited : AML_Execute.Match_Visit_Count;
      Value : out AML_Decode.Integer_Value; Visited : out AML_Execute.Match_Visit_Count;
      Status : out AML_Execute.Execution_Status)
   is
      pragma Unreferenced (Environment, Width, Package_Value, Match_1, Match_2, Operation_1, Operation_2, Start, Max_Visited);
   begin
      Value := 0; Visited := 0; Status := AML_Execute.Unsupported_Value;
   end Reject_Match;

   procedure Unavailable_Delay (Environment : in out Mutable; Item : AML_Delays.Request; Result : out AML_Delays.Outcome) is
      pragma Unreferenced (Environment);
   begin
      AML_Delays.Unavailable_Provider (Item, Result);
   end Unavailable_Delay;
   procedure Execute is new Execute_With_Input
     (Mutable, Backing, Valid, Lookup, Get_Method, Write, Begin_Call, End_Call, Define_Method, Reject_Fields, Reject_Literal, Reject_Region, Reject_Completion, No_Timer, Resolve_Reference, Create_Index, Store_Reference, Clone_Value, Compare_Objects, Keep_Value, No_Invocation, Reject_Name, Reject_Copy_Attachment, Describe_Identity, Boolean, False, Reserve_Dynamic_Name, Complete_Dynamic_Buffer, Abort_Dynamic_Name, Disabled_Debug, Reject_Explicit_Integer,
      Concatenate_And_Attach => Reject_Concatenation,
      To_Buffer_And_Attach => Reject_To_Buffer, Mid_And_Attach => Reject_Mid, To_String_And_Attach => Reject_To_String, Format_String_And_Attach => Reject_Format_String, Match_Package => Reject_Match, Concatenate_Resources_And_Attach => Reject_Resources, Wait_For_Delay => Unavailable_Delay);
   Input : aliased Backing (1_048_577);
   Environment : Mutable;
   Result : Execution_Result;
   Checks : Natural := 0;
   procedure Check (B : Boolean) is
   begin
      Checks := Checks + 1;
      if not B then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   for V in Byte loop
      Input.Data (Input.Length) := V;
      for Width in Integer_Width loop
         Execute ([16#A4#, 70, 76, 68, 48], Width, [others => <>], 0, 10,
                  Input, Environment, 0, Result);
         Check (Result.Status = Returned and then Result.Value =
           AML_Integers.Normalize (16#1_0000_0000# + Integer_Value (V), Width));
         Check (Environment.Active = 0);
      end loop;
      Execute ([16#A4#, 16#8E#, 70, 76, 68, 48], Bits_64, [others => <>], 0, 10,
               Input, Environment, 0, Result);
      Check (Result.Status = Returned and then Result.Value = 5);
      -- Both recursive calls must receive this same immutable backing object.
      Execute ([16#A4#, 77, 84, 72, 49], Bits_64, [others => <>], 0, 20,
               Input, Environment, 0, Result);
      Check (Result.Status = Returned and then Result.Value = 16#1_0000_0000# + Integer_Value (V));
      Check (Environment.Active = 0 and then Input.Data (Input.Length) = V);
   end loop;
   for Budget in 0 .. 8 loop
      Execute ([16#A4#, 77, 84, 72, 49], Bits_64, [others => <>], 0, Budget,
               Input, Environment, 0, Result);
      Check (Result.Charged <= Budget and then Environment.Active = 0);
      Check (Result.Status in Returned | Budget_Exceeded);
   end loop;
   Execute ([16#A4#, 77, 84, 72, 49], Bits_64, [others => <>], 0, 20,
            Input, Environment, 0, Result, Calls_Left => 0);
   Check (Result.Status = Call_Limit and then Environment.Active = 0);
   Ada.Text_IO.Put_Line ("AML read-only input checks:" & Checks'Image);
end Readonly_Input_Tests;
