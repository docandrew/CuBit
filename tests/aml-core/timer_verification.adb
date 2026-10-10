with AML_Delays;
with AML_Data;
with AML_Frame_Handles;
with AML_References;
with AML_Names;
package body Timer_Verification with SPARK_Mode is
   use type AML_Clock.Sample_Status;
   use AML_Execute;
   use AML_Decode;
   type Empty_Input is limited null record;
   function Valid (E : Clock_State) return Boolean
     with Post => Valid'Result
   is
      pragma Unreferenced (E);
   begin return True; end Valid;
   procedure Lookup
     (E : in out Clock_State; Input : aliased Empty_Input; Scope : Natural;
      Path : AML_Names.Name_Result; Width : Integer_Width;
      Purpose : Binding_Purpose; Binding : out Binding_Result)
     with Pre => Valid (E) and not Binding'Constrained, Post => Valid (E)
   is
      pragma Unreferenced (Input, Scope, Path, Width, Purpose);
   begin Binding := (Status => Missing_Binding); end Lookup;
   function Get_Method (E : Clock_State; ID : Natural) return Method_Definition is
      pragma Unreferenced (E, ID);
   begin return (Exists => False, Length => 0); end Get_Method;
   procedure Write
     (E : in out Clock_State; Scope : Natural; Path : AML_Names.Name_Result;
      Width : Integer_Width; Item : Datum; Status : out Write_Status)
     with Pre => Valid (E), Post => Valid (E)
   is
      pragma Unreferenced (Scope, Path, Width, Item);
   begin Status := Write_Unsupported; end Write;
   procedure Begin_Call (E : in out Clock_State; Scope : Natural; Allowed : out Boolean)
     with Pre => Valid (E), Post => Valid (E)
   is
      pragma Unreferenced (Scope);
   begin
      Allowed := E.Active < 33;
      if Allowed then E.Active := E.Active + 1; end if;
   end Begin_Call;
   procedure End_Call (E : in out Clock_State; Scope : Natural)
     with Pre => Valid (E), Post => Valid (E)
   is
      pragma Unreferenced (Scope);
   begin if E.Active > 0 then E.Active := E.Active - 1; end if; end End_Call;
   procedure Define_Method
     (E : in out Clock_State; Scope : Natural; Path : AML_Names.Name_Result;
      Flags : Byte; Width : Integer_Width; Code : Bytes; Status : out Declaration_Status)
     with Pre => Valid (E), Post => Valid (E)
   is
      pragma Unreferenced (Scope, Path, Flags, Width, Code);
   begin Status := Declaration_Unsupported; end Define_Method;
   procedure Fields
     (E : in out Clock_State; Scope : Natural; Region : AML_Names.Name_Result;
      Flags : Byte; Entries : Bytes; Status : out Execution_Status)
     with Pre => Valid (E), Post => Valid (E)
   is
      pragma Unreferenced (Scope, Region, Flags, Entries);
   begin Status := Unsupported; end Fields;
   procedure Literal
     (E : in out Clock_State; Scope : Natural; Kind : Literal_Kind; Width : Integer_Width; Data : Bytes; Binding : out Binding_Result)
     with Pre => Valid (E) and not Binding'Constrained, Post => Valid (E)
   is
      pragma Unreferenced (Scope, Kind, Width, Data);
   begin Binding := (Status => Failed_Binding, Failure => Unsupported_Value); end Literal;
   procedure Reserve
     (E : in out Clock_State; Scope : Natural; Path : AML_Names.Name_Result;
      Token : out Natural; Status : out Execution_Status)
     with Pre => Valid (E), Post => Valid (E)
   is
      pragma Unreferenced (Scope, Path);
   begin Token := 0; Status := Unsupported; end Reserve;
   procedure Complete
     (E : in out Clock_State; Input : aliased Empty_Input; Token : Natural;
      Width : Integer_Width; Signature, OEM, Table_ID : Datum; Status : out Execution_Status)
     with Pre => Valid (E), Post => Valid (E)
   is
      pragma Unreferenced (Input, Token, Width, Signature, OEM, Table_ID);
   begin Status := Unsupported; end Complete;
   procedure Read_Timer
     (E : in out Clock_State; Value : out Integer_Value; Available : out Boolean)
     with Pre => Valid (E), Post => Valid (E)
   is
      Status : AML_Clock.Sample_Status;
   begin
      AML_Clock.Observe (E.Adapter, E.Microseconds, E.Available, Value, Status);
      Available := Status = AML_Clock.Accepted;
      if E.Reads < Natural'Last then E.Reads := E.Reads + 1; end if;
   end Read_Timer;
   -- This clock-only fixture owns no object arena; new object callbacks
   -- reject requests explicitly rather than fabricating reference authority.
   procedure Resolve_Reference
     (E : in out Clock_State; Ref : AML_References.Reference;
      Value : out Datum; Status : out Execution_Status)
   is
      pragma Unreferenced (E, Ref);
   begin Value := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer); Status := Unsupported_Value; end Resolve_Reference;
   procedure Create_Index
     (E : in out Clock_State; Source : AML_References.Object_Handle;
      Index : Integer_Value; Ref : out AML_References.Reference; Status : out Execution_Status)
   is
      pragma Unreferenced (E, Source, Index);
   begin Ref := AML_References.No_Reference; Status := Unsupported_Value; end Create_Index;
   procedure Store_Reference
     (E : in out Clock_State; Ref : AML_References.Reference; Width : Integer_Width;
      Item : Datum; Status : out Execution_Status; Mode : Reference_Store_Mode)
   is
      pragma Unreferenced (E, Ref, Width, Item, Mode);
   begin Status := Unsupported_Value; end Store_Reference;
   procedure Clone_Value
     (E : in out Clock_State; Width : Integer_Width; Item : Datum;
      Copy : out Datum; Status : out Execution_Status)
   is
      pragma Unreferenced (E, Width, Item);
   begin Copy := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer); Status := Unsupported_Value; end Clone_Value;
   -- This fixture owns no arena; comparison cannot authenticate object handles.
   procedure Compare_Objects
     (E : in out Clock_State; Op : Byte; Left, Right : Datum;
      Width : Integer_Width; Value : out Integer_Value; Status : out Execution_Status)
   is
      pragma Unreferenced (E, Op, Left, Right, Width);
   begin Value := 0; Status := Unsupported_Value; end Compare_Objects;
   procedure Keep_Value
     (E : Clock_State; Item : in out Datum; Status : out Execution_Status)
   is
      pragma Unreferenced (E, Item);
   begin Status := Returned; end Keep_Value;
   -- This focused fixture has no owned arena and cannot issue reference domains.
   procedure No_Invocation
     (Environment : in out Clock_State;
      Domain : out AML_Frame_Handles.Invocation_Domain;
      Status : out Invocation_Status)
   is
      pragma Unreferenced (Environment);
   begin
      Domain := AML_Frame_Handles.No_Domain;
      Status := Unsupported_Context;
   end No_Invocation;
   procedure Reject_Name
     (Environment : in out Clock_State; Scope : Natural;
      Path : AML_Names.Name_Result; Width : Integer_Width; Data : Bytes;
      Consumed : out Natural; Status : out Execution_Status)
   is
      pragma Unreferenced (Environment, Scope, Path, Width, Data);
   begin
      Consumed := 0;
      Status := Unsupported;
   end Reject_Name;
   procedure Reject_Copy_Attachment
     (Environment : in out Clock_State; Destination : Copy_Destination;
      Width : Integer_Width; Item : Datum; Copy : out Datum;
      Status : out Execution_Status)
   is
      pragma Unreferenced (Environment, Destination, Width, Item);
   begin
      Copy := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
      Status := Unsupported_Value;
   end Reject_Copy_Attachment;
   procedure Describe_Identity
     (E : Clock_State; Ref : AML_References.Reference;
      Result : out AML_Execute.Reference_Metadata)
     with Pre => not Result'Constrained
   is
      pragma Unreferenced (E, Ref);
   begin Result := (Kind => AML_Execute.Continue_Reference); end Describe_Identity;
   procedure Reserve_Dynamic_Name
     (E : in out Clock_State; Scope : Natural; Path : AML_Names.Name_Result;
      Token : out Boolean; Status : out AML_Execute.Execution_Status)
   is
      pragma Unreferenced (E, Scope, Path);
   begin Token := False; Status := AML_Execute.Unsupported_Value; end Reserve_Dynamic_Name;
   procedure Complete_Dynamic_Buffer
     (E : in out Clock_State; Token : Boolean; Width : AML_Decode.Integer_Width;
      Initializer : AML_Decode.Bytes; Count : AML_Data.Count_Result;
      Status : out AML_Execute.Execution_Status)
   is
      pragma Unreferenced (E, Token, Width, Initializer, Count);
   begin Status := AML_Execute.Unsupported_Value; end Complete_Dynamic_Buffer;
   procedure Abort_Dynamic_Name
     (E : in out Clock_State; Token : Boolean; Status : out AML_Execute.Execution_Status)
   is
      pragma Unreferenced (E, Token);
   begin Status := AML_Execute.Unsupported_Value; end Abort_Dynamic_Name;


   procedure Reject_Explicit_Integer
        (Environment : Clock_State; Width : AML_Decode.Integer_Width;
         Item : in out AML_Execute.Datum; Status : out AML_Execute.Execution_Status)
      is
         pragma Unreferenced (Environment, Width);
      begin
         Status := (if Item.Value_Kind = AML_Execute.Integer_Datum then
           AML_Execute.Returned else AML_Execute.Unsupported_Value);
      end Reject_Explicit_Integer;
      procedure Disabled_Debug
     (Environment : Clock_State; Scope, Position : Natural;
      Width : AML_Decode.Integer_Width; Value : AML_Execute.Datum)
   is
      pragma Unreferenced (Environment, Scope, Position, Width, Value);
   begin null; end Disabled_Debug;
   -- This focused caller has no owned concatenation implementation.
   procedure Reject_Concatenation
     (Environment : in out Clock_State; Width : AML_Decode.Integer_Width;
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
     (Environment : in out Clock_State; Width : AML_Decode.Integer_Width;
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
     (Environment : in out Clock_State; Width : AML_Decode.Integer_Width;
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
     (Environment : in out Clock_State; Width : AML_Decode.Integer_Width;
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
     (Environment : in out Clock_State; Width : AML_Decode.Integer_Width;
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
     (Environment : in out Clock_State; Width : AML_Decode.Integer_Width;
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
     (Environment : Clock_State; Width : AML_Decode.Integer_Width;
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

   procedure Unavailable_Delay (Environment : in out Clock_State; Item : AML_Delays.Request; Result : out AML_Delays.Outcome) is
      pragma Unreferenced (Environment);
   begin
      AML_Delays.Unavailable_Provider (Item, Result);
   end Unavailable_Delay;
   procedure Execute is new Execute_With_Input
     (Clock_State, Empty_Input, Valid, Lookup, Get_Method, Write,
      Begin_Call, End_Call, Define_Method, Fields, Literal, Reserve, Complete, Read_Timer, Resolve_Reference, Create_Index, Store_Reference, Clone_Value, Compare_Objects, Keep_Value, No_Invocation, Reject_Name, Reject_Copy_Attachment, Describe_Identity, Boolean, False, Reserve_Dynamic_Name, Complete_Dynamic_Buffer, Abort_Dynamic_Name, Disabled_Debug, Reject_Explicit_Integer,
      Concatenate_And_Attach => Reject_Concatenation,
      To_Buffer_And_Attach => Reject_To_Buffer, Mid_And_Attach => Reject_Mid, To_String_And_Attach => Reject_To_String, Format_String_And_Attach => Reject_Format_String, Match_Package => Reject_Match, Concatenate_Resources_And_Attach => Reject_Resources, Wait_For_Delay => Unavailable_Delay);
   procedure Run
     (Code : Bytes; Width : Integer_Width; Budget : Natural;
      Clock : in out Clock_State; Result : out Execution_Result)
   is
      Input : aliased Empty_Input;
   begin
      Execute (Code, Width, [others => <>], 0, Budget, Input, Clock, 0, Result);
   end Run;
end Timer_Verification;
