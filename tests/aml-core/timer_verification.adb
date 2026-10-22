with AML_Byte_Sources;
with AML_Container_Headers;
with AML_Declaration_Contexts;
with AML_Buffer_Layouts;
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
   procedure Reject_Buffer_Literal
     (E : in out Clock_State; Scope : Natural;
      Length : AML_Buffer_Layouts.Buffer_Length; Initializer : Bytes;
      Binding : out Binding_Result) is
      pragma Unreferenced (E, Scope, Length, Initializer);
   begin
      Binding := (Status => Failed_Binding, Failure => Unsupported_Value);
   end Reject_Buffer_Literal;
   procedure Literal
     (E : in out Clock_State; Scope : Natural; Kind : Literal_Kind; Width : Integer_Width; Data : Bytes;
         Source : aliased AML_Byte_Sources.Storage; Source_First : Natural; Binding : out Binding_Result)
     with Pre => Valid (E) and not Binding'Constrained, Post => Valid (E)
   is
         pragma Unreferenced (Source, Source_First);
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
     (E : in out Clock_State; Ref : AML_References.Reference; Width : Integer_Width;
      Value : out Datum; Status : out Execution_Status)
   is
      pragma Unreferenced (Width);
      pragma Unreferenced (E, Ref);
   begin Value := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer); Status := Unsupported_Value; end Resolve_Reference;
   procedure Reject_Buffer_Field
     (E : in out Clock_State; Scope : Natural; Path : AML_Names.Name_Result;
      Width : Integer_Width; Source : Datum; Bit_Offset, Bit_Length : Integer_Value;
      Always_Buffer : Boolean; Status : out Execution_Status)
   is
      pragma Unreferenced (E, Scope, Path, Width, Source, Bit_Offset, Bit_Length, Always_Buffer);
   begin Status := Unsupported_Value; end Reject_Buffer_Field;
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
   procedure Reject_Notify
     (Environment : in out Clock_State; Target : AML_References.Reference;
      Value : AML_Decode.Integer_Value; Status : out AML_Execute.Execution_Status)
   is
      pragma Unreferenced (Environment, Target, Value);
   begin
      Status := AML_Execute.Unsupported;
   end Reject_Notify;
   procedure Reject_Metadata_Value
     (Environment : Clock_State; Item : AML_Execute.Metadata_Value;
      Result : out AML_Execute.Reference_Metadata)
   is
      pragma Unreferenced (Environment, Item);
   begin
      Result := (Kind => AML_Execute.Invalid_Reference);
   end Reject_Metadata_Value;
         -- Method-only explicit declaration leases. No module lifetime is enabled.
      Execute_Declaration_Lease_Store : AML_Declaration_Contexts.State := AML_Declaration_Contexts.Empty;
      use type AML_Declaration_Contexts.Request;
      use type AML_Declaration_Contexts.Lease;
      type Execute_Declaration_Name_Token is record
         Owner : AML_Declaration_Contexts.Lease := AML_Declaration_Contexts.No_Lease;
         Original : Boolean := False;
      end record;
      Execute_Declaration_No_Name : constant Execute_Declaration_Name_Token := (others => <>);
      function Execute_Declaration_Valid (Environment : Clock_State; Lease : AML_Declaration_Contexts.Lease) return Boolean
      is
         pragma Unreferenced (Environment);
      begin
         return AML_Declaration_Contexts.Is_Live (Execute_Declaration_Lease_Store, Lease);
      end Execute_Declaration_Valid;
      procedure Execute_Declaration_Enter
        (Environment : in out Clock_State; Intent : AML_Declaration_Contexts.Request; Scope : Natural;
         Frame : AML_Frame_Handles.Frame_Handle; Lease : out AML_Declaration_Contexts.Lease; Allowed : out Boolean) is
      begin
         Lease := AML_Declaration_Contexts.No_Lease; Allowed := False;
         if Intent /= AML_Declaration_Contexts.Method_Request then return; end if;
         Begin_Call (Environment, Scope, Allowed);
         if not Allowed then return; end if;
         AML_Declaration_Contexts.Open (Execute_Declaration_Lease_Store, Intent, Frame, Scope, 0, Lease, Allowed);
         if not Allowed then End_Call (Environment, Scope); end if;
      end Execute_Declaration_Enter;
      procedure Execute_Declaration_Leave (Environment : in out Clock_State; Lease : AML_Declaration_Contexts.Lease) is
      begin
         if not Execute_Declaration_Valid (Environment, Lease) then return; end if;
         End_Call (Environment, AML_Declaration_Contexts.Scope_Of (Lease));
         AML_Declaration_Contexts.Close (Execute_Declaration_Lease_Store, Lease);
      end Execute_Declaration_Leave;
      procedure Execute_Declaration_Method
        (Environment : in out Clock_State; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result; Flags : AML_Decode.Byte; Width : AML_Decode.Integer_Width; Code : AML_Decode.Bytes; Status : out AML_Execute.Declaration_Status) is
      begin
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then
            Status := AML_Execute.Declaration_Unsupported; return;
         end if;
         Define_Method (Environment, Scope, Path, Flags, Width, Code, Status);
      end Execute_Declaration_Method;
      procedure Execute_Declaration_Fields
        (Environment : in out Clock_State; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Region : AML_Names.Name_Result; Flags : AML_Decode.Byte; Entries : AML_Decode.Bytes; Status : out AML_Execute.Execution_Status) is
      begin
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then
            Status := AML_Execute.Unsupported_Value; return;
         end if;
         Fields (Environment, Scope, Region, Flags, Entries, Status);
      end Execute_Declaration_Fields;
      procedure Execute_Declaration_Buffer_Field
        (Environment : in out Clock_State; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width; Source : AML_Execute.Datum; Bit_Offset, Bit_Length : AML_Decode.Integer_Value; Always_Buffer : Boolean; Status : out AML_Execute.Execution_Status) is
      begin
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then
            Status := AML_Execute.Unsupported_Value; return;
         end if;
         Reject_Buffer_Field (Environment, Scope, Path, Width, Source, Bit_Offset, Bit_Length, Always_Buffer, Status);
      end Execute_Declaration_Buffer_Field;
      procedure Execute_Declaration_Name
        (Environment : in out Clock_State; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width; Data : AML_Decode.Bytes;
         Source : aliased AML_Byte_Sources.Storage; Source_First : Natural; Consumed : out Natural; Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Source, Source_First);
      begin
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then
            Consumed := 0; Status := AML_Execute.Unsupported_Value; return;
         end if;
         Reject_Name (Environment, Scope, Path, Width, Data, Consumed, Status);
      end Execute_Declaration_Name;
      procedure Execute_Declaration_Reserve_Region
        (Environment : in out Clock_State; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result;
         Token : out AML_Declaration_Contexts.Region_Reservation; Status : out AML_Execute.Execution_Status) is
         Node : Natural;
      begin
         Token := AML_Declaration_Contexts.No_Region; Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then return; end if;
         Reserve (Environment, Scope, Path, Node, Status);
         if Status in AML_Execute.Returned and then Node /= 0 then
            Token := AML_Declaration_Contexts.Reserve_Region (Lease, Node, 0);
         end if;
      end Execute_Declaration_Reserve_Region;
      procedure Execute_Declaration_Complete_Region
        (Environment : in out Clock_State; Lease : AML_Declaration_Contexts.Lease; Input : aliased Empty_Input;
         Token : AML_Declaration_Contexts.Region_Reservation; Width : AML_Decode.Integer_Width;
         Signature, OEM, Table_ID : AML_Execute.Datum; Status : out AML_Execute.Execution_Status) is
         Node : constant Natural := AML_Declaration_Contexts.Node_Of (Token);
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else not AML_Declaration_Contexts.Issued_By (Token, Lease) then return; end if;
         Complete (Environment, Input, Node, Width, Signature, OEM, Table_ID, Status);
      end Execute_Declaration_Complete_Region;
      procedure Execute_Declaration_Reserve_Name
        (Environment : in out Clock_State; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result;
         Token : out Execute_Declaration_Name_Token; Status : out AML_Execute.Execution_Status) is
      begin
         Token := Execute_Declaration_No_Name; Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then return; end if;
         Reserve_Dynamic_Name (Environment, Scope, Path, Token.Original, Status);
         if Status in AML_Execute.Returned then Token.Owner := Lease; end if;
      end Execute_Declaration_Reserve_Name;
      procedure Execute_Declaration_Complete_Name
        (Environment : in out Clock_State; Lease : AML_Declaration_Contexts.Lease; Token : Execute_Declaration_Name_Token;
         Width : AML_Decode.Integer_Width; Initializer : AML_Decode.Bytes; Count : AML_Data.Count_Result;
         Status : out AML_Execute.Execution_Status) is
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else Token.Owner /= Lease then return; end if;
         Complete_Dynamic_Buffer (Environment, Token.Original, Width, Initializer, Count, Status);
      end Execute_Declaration_Complete_Name;
      procedure Execute_Declaration_Abort_Name
        (Environment : in out Clock_State; Lease : AML_Declaration_Contexts.Lease; Token : Execute_Declaration_Name_Token;
         Status : out AML_Execute.Execution_Status) is
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else Token.Owner /= Lease then return; end if;
         Abort_Dynamic_Name (Environment, Token.Original, Status);
      end Execute_Declaration_Abort_Name;
      procedure Reject_Alias
     (Environment : in out Clock_State; Lease : AML_Declaration_Contexts.Lease;
      Scope : Natural; Source_Path, Target_Path : AML_Names.Name_Result;
      Status : out AML_Execute.Execution_Status) is
      pragma Unreferenced (Environment, Lease, Scope, Source_Path, Target_Path);
   begin Status := AML_Execute.Unsupported; end Reject_Alias;
   procedure Execute_Unsupported_Container_Enter
        (Environment : in out Clock_State; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Path : AML_Names.Name_Result;
         Metadata : AML_Container_Headers.Attributes; New_Scope : out Natural;
         Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease, Path, Metadata);
      begin
         New_Scope := Scope; Status := AML_Execute.Unsupported;
      end Execute_Unsupported_Container_Enter;
      procedure Execute_Unsupported_Container_Leave
        (Environment : in out Clock_State; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Restored_Scope : out Natural; Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease);
      begin
         Restored_Scope := Scope; Status := AML_Execute.Unsupported;
      end Execute_Unsupported_Container_Leave;
      procedure Provenance_Execute_Source_Resolve
        (Environment : in out Clock_State; Ref : AML_References.Reference;
         Width : AML_Decode.Integer_Width; Value : out AML_Execute.Datum;
         Provenance : out AML_Execute.Scalar_Provenance;
         Status : out AML_Execute.Execution_Status) is
      begin
         Provenance := AML_Execute.Detached_Provenance;
         Resolve_Reference (Environment, Ref, Width, Value, Status);
      end Provenance_Execute_Source_Resolve;
      procedure Provenance_Execute_Source_Read_Integer
        (Environment : Clock_State; Source : AML_References.Object_Handle;
         Value : out AML_Decode.Integer_Value; Origin : out AML_Decode.Integer_Origin;
         Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Source);
      begin
         Value := 0; Origin := AML_Decode.Ordinary_Integer;
         Status := AML_Execute.Unsupported_Value;
      end Provenance_Execute_Source_Read_Integer;
      procedure Provenance_Execute_Source_Read_Integer_Normalize
        (Environment : in out Clock_State; Source : AML_References.Object_Handle;
         Width : AML_Decode.Integer_Width; Value : out AML_Decode.Integer_Value;
         Origin : out AML_Decode.Integer_Origin; Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Source, Width);
      begin
         Value := 0; Origin := AML_Decode.Ordinary_Integer;
         Status := AML_Execute.Unsupported_Value;
      end Provenance_Execute_Source_Read_Integer_Normalize;
      procedure Provenance_Execute_Source_Read_Integer_Normalize_Census_Begin
        (Environment : Clock_State; Source : AML_References.Object_Handle;
         Status : out AML_Execute.Owner_Census_Status) is
         pragma Unreferenced (Environment, Source);
      begin Status := AML_Execute.Census_Invalid; end Provenance_Execute_Source_Read_Integer_Normalize_Census_Begin;
      procedure Provenance_Execute_Source_Read_Integer_Normalize_Census_Add
        (Environment : Clock_State; Value : AML_Execute.Datum;
         Status : out AML_Execute.Owner_Census_Status) is
         pragma Unreferenced (Environment, Value);
      begin Status := AML_Execute.Census_Invalid; end Provenance_Execute_Source_Read_Integer_Normalize_Census_Add;
      procedure Provenance_Execute_Source_Read_Integer_Normalize_Census_Finish
        (Environment : Clock_State; Status : out AML_Execute.Owner_Census_Status) is
         pragma Unreferenced (Environment);
      begin Status := AML_Execute.Census_Invalid; end Provenance_Execute_Source_Read_Integer_Normalize_Census_Finish;

      procedure Package_Adapter_Begin
        (Environment : in out Clock_State; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Count, Child_Byte_Bound : Natural;
         Parent : Natural; Name : Execute_Declaration_Name_Token;
         Width : AML_Decode.Integer_Width; Token : out Natural;
         Binding : out AML_Execute.Binding_Result) is
         pragma Unreferenced (Environment, Lease, Scope, Count, Child_Byte_Bound, Parent, Name, Width);
      begin
         Token := 0;
         Binding := (Status => AML_Execute.Failed_Binding, Failure => AML_Execute.Unsupported_Value);
      end Package_Adapter_Begin;
      procedure Package_Adapter_Append
        (Environment : in out Clock_State; Lease : AML_Declaration_Contexts.Lease;
         Token : in out Natural; Item : AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease, Token, Item);
      begin
         Status := AML_Execute.Unsupported_Value;
      end Package_Adapter_Append;
      procedure Package_Adapter_Defer
        (Environment : Clock_State; Lease : AML_Declaration_Contexts.Lease;
         Token : in out Natural; Scope : Natural;
         Path : AML_Names.Name_Result; Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease, Token, Scope, Path);
      begin
         Status := AML_Execute.Unsupported_Value;
      end Package_Adapter_Defer;
      procedure Package_Adapter_Finish
        (Environment : in out Clock_State; Lease : AML_Declaration_Contexts.Lease;
         Token : in out Natural; Binding : out AML_Execute.Binding_Result) is
         pragma Unreferenced (Environment, Lease, Token);
      begin
         Binding := (Status => AML_Execute.Failed_Binding, Failure => AML_Execute.Unsupported_Value);
      end Package_Adapter_Finish;
      -- Rejected builders never own an allocation or root to unwind.
      procedure Package_Adapter_Abort
        (Environment : in out Clock_State; Lease : AML_Declaration_Contexts.Lease;
         Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease);
      begin
         Status := AML_Execute.Returned;
      end Package_Adapter_Abort;
      procedure Execute_Source is new AML_Execute.Execute_From_Source
        (Clock_State,
         Empty_Input,
         Valid,
         Lookup,
         Get_Method,
         Write,
         Execute_Declaration_Enter,
         Execute_Declaration_Leave,
         Execute_Declaration_Method,
         Execute_Declaration_Fields,
         Literal,
         Execute_Declaration_Reserve_Region,
         Execute_Declaration_Complete_Region,
         Read_Timer,
         Provenance_Execute_Source_Resolve,
         Provenance_Execute_Source_Read_Integer,
         Provenance_Execute_Source_Read_Integer_Normalize,
         Provenance_Execute_Source_Read_Integer_Normalize_Census_Begin, Provenance_Execute_Source_Read_Integer_Normalize_Census_Add, Provenance_Execute_Source_Read_Integer_Normalize_Census_Finish,
         Execute_Declaration_Buffer_Field,
         Create_Index,
         Store_Reference,
         Clone_Value,
         Compare_Objects,
         Keep_Value,
         No_Invocation,
         Execute_Declaration_Name,
         Reject_Copy_Attachment,
         Describe_Identity,
         Execute_Declaration_Name_Token,
         Execute_Declaration_No_Name,
         Execute_Declaration_Reserve_Name,
         Execute_Declaration_Complete_Name,
         Execute_Declaration_Abort_Name,
         Disabled_Debug,
         Reject_Explicit_Integer,
         Concatenate_And_Attach => Reject_Concatenation,
         To_Buffer_And_Attach => Reject_To_Buffer,
         Mid_And_Attach => Reject_Mid,
         To_String_And_Attach => Reject_To_String,
         Format_String_And_Attach => Reject_Format_String,
         Match_Package => Reject_Match,
         Concatenate_Resources_And_Attach => Reject_Resources,
         Wait_For_Delay => Unavailable_Delay,
         Queue_Notify => Reject_Notify,
         Describe_Metadata_Value => Reject_Metadata_Value,
         Materialize_Buffer => Reject_Buffer_Literal,
         Define_Alias => Reject_Alias,
         Enter_Container => Execute_Unsupported_Container_Enter,
         Leave_Container => Execute_Unsupported_Container_Leave,
         Runtime_Package_Token => Natural,
         No_Runtime_Package => 0,
         Begin_Runtime_Package => Package_Adapter_Begin,
         Append_Runtime_Package => Package_Adapter_Append,
         Defer_Runtime_Package_Name => Package_Adapter_Defer,
         Finish_Runtime_Package => Package_Adapter_Finish,
         Abort_Runtime_Packages => Package_Adapter_Abort);
      -- Test array admission boundary; production owned paths pass their storage directly.
      procedure Execute
        (Code : AML_Decode.Bytes; Width : AML_Decode.Integer_Width;
         Args : AML_Execute.Value_Arguments; Argument_Count, Budget : Natural;
         Input : aliased Empty_Input; Environment : in out Clock_State; Scope : Natural;
         Result_Out : out AML_Execute.Execution_Result;
         Calls_Left : AML_Execute.Call_Budget := 32;
         Current_Sync : AML_Execute.Sync_Level := 0;
         Request : AML_Declaration_Contexts.Request := AML_Declaration_Contexts.Method_Request;
         Implementation : AML_Execute.Method_Implementation := AML_Execute.AML_Implementation)
      is
         Source : aliased constant AML_Byte_Sources.Storage :=
           (Length => Code'Length, Bytes => Code);
      begin
         Execute_Source (Code, Width, Args, Argument_Count, Budget, Input, Environment,
           Scope, Result_Out, Source, 0, Calls_Left, Current_Sync, Request, Implementation);
      end Execute;
   procedure Run
     (Code : Bytes; Width : Integer_Width; Budget : Natural;
      Clock : in out Clock_State; Result : out Execution_Result)
   is
      Input : aliased Empty_Input;
   begin
      Execute (Code, Width, [others => <>], 0, Budget, Input, Clock, 0, Result);
   end Run;
end Timer_Verification;
