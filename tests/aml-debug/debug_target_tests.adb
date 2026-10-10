pragma Ada_2022;
with AML_Delays;
with AML_Data;
with AML_Namespace;
with AML_Objects;
with AML_Frame_Handles;
with AML_References;
with Ada.Text_IO;
with AML_Execute; use AML_Execute;
with AML_Decode; use AML_Decode;
with AML_Names;
procedure Debug_Target_Tests is
   package NS is new AML_Namespace (4, AML_Delays.Unavailable_Provider);
   Reference_Arena : NS.Owned.Arena;
   Reference_Reads : Natural := 0;
   use type AML_References.Reference;
   use type AML_Objects.Allocation_Status;
   use type AML_Names.Parse_Status;
   use type Integer_Value;
   use type Byte;
   type Mutable is record
      Clock : Integer_Value := 0;
      Reads : Natural := 0;
      Clock_Available : Boolean := True;
      Active : Natural := 0;
      Reserved : Boolean := False;
      Seen, Completed, Reservations : Natural := 0;
      Reject : Boolean := False;
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
      pragma Unreferenced (Input, Scope, Width, Purpose);
      ID : Natural := 0;
   begin
      if Path.Kind = AML_Names.Accepted and then Path.Count = 1 then
         if Path.Parts (1) = "TMR0" then
            Binding := (Status => Method_Binding, Method_ID => 4, Parameters => 0);
            return;
         end if;
         if Path.Parts (1) = "SIG0" then ID := 1;
         elsif Path.Parts (1) = "OEM0" then ID := 2;
         elsif Path.Parts (1) = "TAB0" then ID := 3;
         elsif Path.Parts (1) = "REG0" and then Environment.Reserved then
            Binding := (Status => Failed_Binding, Failure => Uninitialized); return;
         end if;
      end if;
      if ID = 0 then Binding := (Status => Missing_Binding); return; end if;
      if not Environment.Reserved then raise Program_Error with "lookup before reservation"; end if;
      Environment.Seen := Environment.Seen * 10 + ID;
      Binding := (Status => Method_Binding, Method_ID => ID, Parameters => 0);
   end Lookup;
   function Get_Method (Environment : Mutable; ID : Natural) return Method_Definition is
      pragma Unreferenced (Environment);
   begin
      if ID = 4 then
         return (Exists => True, Length => 3, Code => [16#A4#,16#5B#,16#33#],
                 Width => Bits_64, Flags => 0, Scope => ID);
      end if;
      if ID in 1 .. 3 then
         return (Exists => True, Length => 3, Code => [16#A4#, 16#0A#, Byte (ID)],
                 Width => Bits_64, Flags => 0, Scope => ID);
      end if;
      return (Exists => False, Length => 0);
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
   begin
      Environment.Active := Environment.Active - 1;
      if Scope = 0 then Environment.Reserved := False; end if;
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
   procedure Reserve_Region
     (Environment : in out Mutable; Scope : Natural; Path : AML_Names.Name_Result;
      Token : out Natural; Status : out Execution_Status)
   is
      pragma Unreferenced (Scope);
   begin
      Token := 0;
      if Path.Kind /= AML_Names.Accepted or else Path.Count /= 1 or else Path.Parts (1) /= "REG0" then
         Status := Bad_Name;
      elsif Environment.Reserved then Status := Duplicate_Name;
      else
         Environment.Reserved := True;
         Environment.Reservations := Environment.Reservations + 1;
         Token := 1; Status := Returned;
      end if;
   end Reserve_Region;
   procedure Complete_Region
     (Environment : in out Mutable; Input : aliased Backing; Token : Natural;
      Width : Integer_Width; Signature, OEM, Table_ID : Datum; Status : out Execution_Status)
   is
      pragma Unreferenced (Input, Width);
      function Number (D : Datum) return Natural is
        (if D.Value_Kind = Object_Datum then D.Object.ID else Natural (D.Number));
   begin
      if not Environment.Reserved or else Token /= 1 then raise Program_Error; end if;
      if Number (Signature) /= 1 or else Number (OEM) /= 2 or else Number (Table_ID) /= 3 then
         raise Program_Error with "selector order or values";
      end if;
      if Environment.Reject then Status := Unknown_Name; return; end if;
      Environment.Completed := Environment.Completed + 1;
      Status := Returned;
   end Complete_Region;
   procedure Read_Timer
     (Environment : in out Mutable; Value : out Integer_Value; Available : out Boolean)
   is
   begin
      Environment.Reads := Environment.Reads + 1;
      Value := Environment.Clock;
      Available := Environment.Clock_Available;
      Environment.Clock := Environment.Clock + 1;
   end Read_Timer;
   -- This clock-only fixture owns no object arena; new object callbacks
   -- reject requests explicitly rather than fabricating reference authority.
   procedure Resolve_Reference
     (Environment : in out Mutable; Ref : AML_References.Reference;
      Value : out Datum; Status : out Execution_Status)
   is
      pragma Unreferenced (Environment, Ref);
   begin Reference_Reads := Reference_Reads + 1; Value := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer); Status := Unsupported_Value; end Resolve_Reference;
   procedure Create_Index
     (Environment : in out Mutable; Source : AML_References.Object_Handle;
      Index : Integer_Value; Ref : out AML_References.Reference; Status : out Execution_Status)
   is
      pragma Unreferenced (Environment);
   begin NS.Owned.Make_Index (Reference_Arena, Source, Index, Ref, Status); end Create_Index;
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


   subtype Capture_Count is Natural range 0 .. 4;
   Emissions : Capture_Count := 0;
   Dropped : Natural := 0;
   Enabled : Boolean := True;
   Values : array (Positive range 1 .. 4) of Datum;
   Positions : array (Positive range 1 .. 4) of Natural := [others => 0];
   procedure Observe_Debug
     (Environment : Mutable; Scope, Position : Natural;
      Width : AML_Decode.Integer_Width; Value : AML_Execute.Datum)
   is
      pragma Unreferenced (Scope, Width);
   begin
      if not Valid (Environment) then raise Program_Error; end if;
      if not Enabled then return; end if;
      if Emissions = Capture_Count'Last then
         if Dropped < Natural'Last then Dropped := Dropped + 1; end if;
         return;
      end if;
      Emissions := Emissions + 1;
      Values (Emissions) := Value; Positions (Emissions) := Position;
   end Observe_Debug;
   procedure Explicit_Integer
     (E : Mutable; Width : Integer_Width; Item : in out Datum; Status : out Execution_Status)
   is
      pragma Unreferenced (E, Width);
   begin
      Status := (if Item.Value_Kind = Integer_Datum then Returned else Unsupported_Value);
   end Explicit_Integer;
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
     (Mutable, Backing, Valid, Lookup, Get_Method, Write, Begin_Call, End_Call,
      Define_Method, Reject_Fields, Reject_Literal, Reserve_Region, Complete_Region, Read_Timer, Resolve_Reference, Create_Index, Store_Reference, Clone_Value, Compare_Objects, Keep_Value, No_Invocation, Reject_Name, Reject_Copy_Attachment, Describe_Identity, Boolean, False, Reserve_Dynamic_Name, Complete_Dynamic_Buffer, Abort_Dynamic_Name, Observe_Debug, Explicit_Integer,
      Concatenate_And_Attach => Reject_Concatenation,
      To_Buffer_And_Attach => Reject_To_Buffer, Mid_And_Attach => Reject_Mid, To_String_And_Attach => Reject_To_String, Format_String_And_Attach => Reject_Format_String, Match_Package => Reject_Match, Concatenate_Resources_And_Attach => Reject_Resources, Wait_For_Delay => Unavailable_Delay);
   Input : aliased Backing (1);
   Environment : Mutable;
   Result : Execution_Result;
   Checks : Natural := 0;
   procedure Check (B : Boolean) is
   begin
      Checks := Checks + 1;
      if not B then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Run (Code : Bytes; Width : Integer_Width := Bits_64; Budget : Natural := 100) is
   begin
      Execute (Code, Width, [others => <>], 0, Budget, Input, Environment, 0, Result);
      Check (Result.Charged <= Budget and Environment.Active = 0);
   end Run;
   procedure Clear is
   begin
      Emissions := 0; Dropped := 0; Enabled := True;
      Environment := (Clock => 42, others => <>);
   end Clear;
   Debug : constant Bytes := [Extended_Op, Debug_Extension];
   Timer : constant Bytes := [16#5B#,16#33#];
   Store_Timer : constant Bytes := Bytes'(16#A4#,16#70#) & Timer & Debug;
begin
   for Width in Integer_Width loop
      Clear; Run (Bytes'(16#A4#,16#70#,16#0A#,42) & Debug, Width);
      Check (Result.Status = Returned and then Result.Value = 42);
      Check (Emissions = 1 and then Values (1).Number = 42 and then Positions (1) = 4);
      Clear; Run (Store_Timer, Width);
      Check (Result.Status = Returned and then Result.Value = 42);
      Check (Environment.Reads = 1 and Emissions = 1);
      Clear; Run (Bytes'(16#A4#,16#70#,84,77,82,48) & Debug, Width);
      Check (Result.Status = Returned and then Result.Value = 42);
      Check (Environment.Reads = 1 and Emissions = 1);
      Clear; Run (Bytes'(16#A4#,16#72#,16#0A#,7,16#0A#,8) & Debug, Width);
      Check (Result.Status = Returned and then Result.Value = 15);
      Check (Emissions = 1 and then Values (1).Number = 15);
      Clear; Run (Bytes'(16#A4#,16#78#,16#0A#,7,16#0A#,3) & Debug & Debug, Width);
      Check (Result.Status = Returned and then Result.Value = 2);
      Check (Emissions = 2 and then Values (1).Number = 1 and then Values (2).Number = 2);
      Clear; Run (Bytes'(16#A4#,16#78#,1,0) & Debug & Debug, Width);
      Check (Result.Status = Division_By_Zero and Emissions = 0);
      Clear; Run (Bytes'(16#70#,16#60#) & Debug, Width);
      Check (Result.Status = Uninitialized and Emissions = 0);
      Clear; Environment.Clock_Available := False; Run (Store_Timer, Width);
      Check (Result.Status = Unsupported and Environment.Reads = 1 and Emissions = 0);
      Clear; Run (Bytes'(1=>16#70#) & Timer & Bytes'(1=>Extended_Op), Width);
      Check (Result.Status = Truncated and Environment.Reads = 1 and Emissions = 0);
      Clear; Run (Bytes'(1=>16#70#) & Timer & Bytes'(Extended_Op,16#32#), Width);
      Check (Result.Status = Unsupported and Environment.Reads = 1 and Emissions = 0);
      Clear; Run (Bytes'(16#A4#,16#8E#) & Debug, Width);
      Check (Result.Status = Returned and then Result.Value = 16 and then Emissions = 0);
      Clear; Run (Bytes'(16#A4#,16#87#) & Debug, Width);
      Check (Result.Status = Unsupported_Value and Emissions = 0);
      for Budget in 0 .. 8 loop
         declare
            Prior : Mutable;
            Expected : Execution_Result;
         begin
            Clear; Run (Store_Timer, Width, Budget);
            Prior := Environment; Expected := Result;
            Clear; Enabled := False; Run (Store_Timer, Width, Budget);
            Check (Result = Expected and Environment = Prior and Emissions = 0);
         end;
      end loop;
      Clear;
      declare
         OK : Boolean;
         ID : AML_Objects.Object_ID;
         Allocation : AML_Objects.Allocation_Status;
         Source : AML_References.Object_Handle;
         Args : Value_Arguments := [others => <>];
      begin
         NS.Owned.Reset (Reference_Arena, OK); Check (OK);
         NS.Owned.Append (Reference_Arena, [1=>42], ID, Allocation);
         Check (Allocation = AML_Objects.Allocated);
         NS.Owned.Make_Source (Reference_Arena, ID, Source, OK); Check (OK);
         Args (0) := (Value_Kind => Object_Datum,
           Object => (Source => Source, ID => Natural (ID), Type_Code => 3, Size => 1, others => <>));
         Reference_Reads := 0;
         Execute (Bytes'(16#A4#,16#88#,16#68#,0) & Debug, Width, Args, 1,
                  100, Input, Environment, 0, Result);
         Check (Result.Status = Reference_Returned);
         Check (Emissions = 1 and then Values (1).Value_Kind = Reference_Datum
                and then Values (1).Ref = Result.Ref);
         Check (Reference_Reads = 0); -- observe descriptor, never dereference for output
      end;
      Clear;
      for I in 1 .. 6 loop
         Run (Store_Timer, Width);
         Check (Result.Status = Returned and then Result.Value = Integer_Value (41+I));
      end loop;
      Check (Emissions = 4 and Dropped = 2 and Environment.Reads = 6);
   end loop;
   Ada.Text_IO.Put_Line ("DEBUG TARGETS: PASS" & Checks'Image);
end Debug_Target_Tests;
