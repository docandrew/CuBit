pragma Ada_2022;
with AML_Container_Headers;
with AML_Declaration_Contexts;
with AML_Buffer_Layouts;
with AML_Delays;
with AML_Data;
with AML_Objects;
with AML_Identity.Issuer;
with AML_Frame_Handles;
with AML_References;
with AML_Root_Slots;
with Ada.Text_IO;
with AML_Execute;
with AML_Byte_Sources; use AML_Execute;
with AML_Decode; use AML_Decode;
with AML_Names;
with AML_Integers;
procedure Held_Publication_Tests is
   use type AML_Decode.Bytes;
   use type AML_Names.Parse_Status;
   use type AML_References.Object_Handle;
   use type AML_References.Reference_Kind;
   use type AML_Objects.Allocation_Status;
   use type Byte;
   use type Integer_Value;
   package FH renames AML_Frame_Handles;
   package RS renames AML_Root_Slots;
   use type RS.Held_Root;
   type Held_Flags is array (RS.Held_Root) of Boolean;
   use type FH.Frame_Handle;
   use type FH.Cell_ID;
   use type FH.Frame_Index;
   use type FH.Invocation_Serial;
   type Flags is array (FH.Cell_ID) of Boolean;
   type Values is array (FH.Cell_ID) of Datum;
   type Root_Row is record
      Frame : FH.Frame_Handle := FH.No_Frame;
      Initialized : Flags := [others => False];
      Data : Values := [others => (Integer_Datum,0,Ordinary_Integer)];
   end record;
   type Root_Rows is array (FH.Frame_Index range 1 .. FH.Max_Active_Frames) of Root_Row;
   type Return_Phase is (No_Return_Publication, Own_Return_Published, Parent_Return_Published);
   type Return_Phases is array (FH.Frame_Index range 1 .. FH.Max_Active_Frames) of Return_Phase;
   type Mutable is limited record
      Return_Order : Return_Phases := [others => No_Return_Publication];
      Held_Returns : Natural := 0;
      Last_Return_Parent : FH.Frame_Handle := FH.No_Frame;
      Region_Mode, Fail_Region : Boolean := False;
      Region_Roots : Held_Flags := [others => False];
      Selector_Calls, Region_Completions, Region_Clears, Cleanup_Roots : Natural := 0;
      Roots : Root_Rows;
      Writes, Clears, Cross_Writes : Natural := 0;
      Serial : FH.Invocation_Serial := 0;
      Deny : Boolean := False;
      Active : Natural := 0;
      Handoffs : Natural := 0;
      Expected : Boolean := False;
      Expected_Success : Boolean := False;
      Ordered : Boolean := True;
      Owner : AML_Identity.Identity;
      Values : AML_Objects.State := AML_Objects.Empty;
      Expected_Source : AML_References.Object_Handle := AML_References.No_Object_Handle;
   end record;
   type Backing (Length : Positive) is limited record
      Data : Bytes (1 .. Length) := [others => 0];
   end record;
   function Valid (Environment : Mutable) return Boolean is (Environment.Active <= 33 and then AML_Objects.Valid (Environment.Values));
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
         Binding := (Status => Method_Binding, Method_ID => 1, Parameters => 1); return;
      elsif Path.Parts (1) = "MTH1" then
         Binding := (Status => Method_Binding, Method_ID => 2, Parameters => 1); return;
      elsif Path.Parts (1) = "MTH2" then
         Binding := (Status => Method_Binding, Method_ID => 3, Parameters => 1); return;
      else
         Binding := (Status => Missing_Binding); return;
      end if;
   end Lookup;
   function Get_Method (Environment : Mutable; ID : Natural) return Method_Definition is
      pragma Unreferenced (Environment);
   begin
      if ID = 1 then
         return (Exists => True, Implementation => AML_Execute.AML_Implementation, Length => 2, Code => (Length => 2, Bytes => [16#A4#,16#68#]),
                 Width => Bits_64, Flags => 1, Scope => 1);
      elsif ID = 2 then
         return (Exists => True, Implementation => AML_Execute.AML_Implementation, Length => 6, Code => (Length => 6, Bytes => [16#A4#,77,84,72,48,16#68#]),
                 Width => Bits_64, Flags => 1, Scope => 2);
      elsif ID = 3 then
         return (Exists => True, Implementation => AML_Execute.AML_Implementation, Length => 6, Code => (Length => 6, Bytes => [16#70#,16#0A#,9,16#68#,16#A4#,1]),
                 Width => Bits_64, Flags => 1, Scope => 3);
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
   begin
      if not Environment.Roots (FH.Frame_Index (Environment.Active + 1)).Initialized (FH.Arg_0) then
         raise Program_Error with "argument root missing before Begin_Call";
      end if;
      declare Arg : constant Datum := Environment.Roots
        (FH.Frame_Index (Environment.Active + 1)).Data (FH.Arg_0); begin
         if Scope = 3 then
            if Arg.Value_Kind /= Reference_Datum
              or else AML_References.Kind (Arg.Ref) /= AML_References.Frame_Cell
              or else not FH.Matches (AML_References.Frame_Item (Arg.Ref),
                Environment.Roots (FH.Frame_Index (Environment.Active)).Frame)
            then raise Program_Error with "wrong caller reference root"; end if;
         elsif Arg.Value_Kind /= Object_Datum
           or else Arg.Object.Source /= Environment.Expected_Source
         then raise Program_Error with "wrong argument object root"; end if;
      end;
      if Environment.Active > 0 and then
        (not Environment.Roots (FH.Frame_Index (Environment.Active)).Initialized (FH.Arg_0)
         or else Environment.Roots (FH.Frame_Index (Environment.Active)).Data (FH.Arg_0).Value_Kind /= Object_Datum)
      then raise Program_Error with "suspended caller argument root lost"; end if;
      if Environment.Region_Mode and then Scope = 1 then
         Environment.Selector_Calls := Environment.Selector_Calls + 1;
         for Root in RS.Region_Signature .. RS.Region_Table_ID loop
            if Environment.Region_Roots (Root) /=
              (RS.Held_Root'Pos (Root) < Environment.Selector_Calls)
            then raise Program_Error with "selector roots not preserved between operands"; end if;
         end loop;
      end if;
      Allowed := Environment.Active < 33 and then not Environment.Deny;
      if Allowed then Environment.Active := Environment.Active + 1; end if;
   end Begin_Call;
   procedure End_Call (Environment : in out Mutable; Scope : Natural) is
   begin
      if Scope = 1 or else (Scope = 2 and then Environment.Expected) then
         if FH.Index_Of (Environment.Last_Return_Parent) /= FH.Frame_Index (Environment.Active - 1) then
            raise Program_Error with "return bridge missing before End_Call";
         end if;
      end if;
      if Environment.Active = 1 then
         Environment.Ordered := Environment.Ordered and then
           Environment.Handoffs = (if Environment.Expected then 1 else 0);
      else Environment.Ordered := Environment.Ordered and then Environment.Handoffs = 0;
      end if;
      if Environment.Expected_Success or else
        Environment.Return_Order (FH.Frame_Index (Environment.Active)) /= No_Return_Publication
      then
         if Environment.Return_Order (FH.Frame_Index (Environment.Active)) /=
           (if Environment.Active = 1 then Own_Return_Published else Parent_Return_Published)
         then raise Program_Error with "incomplete ordered return publication"; end if;
      end if;
      Environment.Return_Order (FH.Frame_Index (Environment.Active)) := No_Return_Publication;
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
   procedure Reject_Buffer_Literal
     (Environment : in out Mutable; Scope : Natural;
      Length : AML_Buffer_Layouts.Buffer_Length; Initializer : Bytes;
      Binding : out Binding_Result) is
      pragma Unreferenced (Environment, Scope, Length, Initializer);
   begin
      Binding := (Status => Failed_Binding, Failure => Unsupported_Value);
   end Reject_Buffer_Literal;
   procedure Reject_Literal
     (Environment : in out Mutable; Scope : Natural; Kind : Literal_Kind; Width : Integer_Width; Data : Bytes;
         Source : aliased AML_Byte_Sources.Storage; Source_First : Natural; Binding : out Binding_Result)
   is
         pragma Unreferenced (Source, Source_First);
      pragma Unreferenced (Environment, Scope, Kind, Width, Data);
   begin
      Binding := (Status => Failed_Binding, Failure => Unsupported_Value);
   end Reject_Literal;
   procedure Test_Region
     (Environment : in out Mutable; Scope : Natural; Path : AML_Names.Name_Result;
      Token : out Natural; Status : out Execution_Status)
   is
      pragma Unreferenced (Environment, Scope, Path);
   begin
      Token := 1; Status := Returned;
   end Test_Region;
   procedure Test_Completion
     (Environment : in out Mutable; Input : aliased Backing; Token : Natural;
      Width : Integer_Width; Signature, OEM, Table_ID : Datum; Status : out Execution_Status)
   is
      pragma Unreferenced (Input, Token, Width);
   begin
      -- Controlled lifecycle fixture only: no table match, region binding or HW.
      if (for some Root in RS.Region_Signature .. RS.Region_Table_ID => not Environment.Region_Roots (Root))
        or else Signature.Value_Kind /= Object_Datum or else OEM.Value_Kind /= Object_Datum
        or else Table_ID.Value_Kind /= Object_Datum
      then raise Program_Error with "missing complete-region roots"; end if;
      Environment.Region_Completions := Environment.Region_Completions + 1;
      Status := (if Environment.Fail_Region then Unsupported_Value else Returned);
   end Test_Completion;
   procedure No_Timer (Environment : in out Mutable; Value : out Integer_Value; Available : out Boolean) is
      pragma Unreferenced (Environment);
   begin Value := 0; Available := False; end No_Timer;
   -- This fixture accepts authenticated borrowed inputs; allocation and
   -- unrelated object operations are deliberately rejected by these stubs.
   procedure Resolve_Reference
     (Environment : in out Mutable; Ref : AML_References.Reference; Width : Integer_Width;
      Value : out Datum; Status : out Execution_Status)
   is
      pragma Unreferenced (Width);
      pragma Unreferenced (Environment, Ref);
   begin Value := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer); Status := Unsupported_Value; end Resolve_Reference;
   procedure Reject_Buffer_Field
     (Environment : in out Mutable; Scope : Natural; Path : AML_Names.Name_Result;
      Width : Integer_Width; Source : Datum; Bit_Offset, Bit_Length : Integer_Value;
      Always_Buffer : Boolean; Status : out Execution_Status)
   is
      pragma Unreferenced (Environment, Scope, Path, Width, Source, Bit_Offset, Bit_Length, Always_Buffer);
   begin Status := Unsupported_Value; end Reject_Buffer_Field;
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
   -- String comparison is outside this ordering test.
   procedure Compare_Objects
     (Environment : in out Mutable; Op : Byte; Left, Right : Datum;
      Width : Integer_Width; Value : out Integer_Value; Status : out Execution_Status)
   is
      pragma Unreferenced (Environment, Op, Left, Right, Width);
   begin Value := 0; Status := Unsupported_Value; end Compare_Objects;
   procedure Keep_Value
     (E : Mutable; Item : in out Datum; Status : out Execution_Status)
   is
   begin
      Status := Returned;
      if Item.Value_Kind = Object_Datum and then
        (not AML_References.Belongs_To (Item.Object.Source, E.Owner)
         or else Item.Object.ID /= AML_References.Source (Item.Object.Source)
         or else not AML_Objects.Matches_Address (E.Values, AML_References.Address (Item.Object.Source)))
      then Status := Unsupported_Value; end if;
   end Keep_Value;
   -- This controlled fixture supplies real unique domains for frame references.
   procedure No_Invocation
     (Environment : in out Mutable;
      Domain : out AML_Frame_Handles.Invocation_Domain;
      Status : out Invocation_Status)
   is
   begin
      Environment.Serial := Environment.Serial + 1;
      Domain := FH.Bind_Domain (Environment.Owner, Environment.Serial);
      Status := Available;
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
   procedure Handoff (E : in out Mutable; Result : Execution_Result) is
   begin
      E.Ordered := E.Ordered and then E.Active = 1 and then E.Handoffs = 0
        and then Result.Status = Object_Returned
        and then Result.Object.Source = E.Expected_Source
        and then AML_Objects.Matches_Address (E.Values, AML_References.Address (Result.Object.Source));
      E.Handoffs := E.Handoffs + 1;
   end Handoff;
   procedure Publish
     (E : in out Mutable; Frame : FH.Frame_Handle; Cell : FH.Cell_ID;
      Initialized : Boolean; Value : Datum)
   is
      Index : constant FH.Frame_Index := FH.Index_Of (Frame);
   begin
      if Index = 0 then raise Program_Error with "absent frame root"; end if;
      if not FH.Belongs_To (Frame, FH.Bind_Domain (E.Owner, E.Serial)) then
         raise Program_Error with "wrong invocation domain on frame root";
      end if;
      if E.Roots (Index).Frame /= Frame then
         if (for some C in FH.Cell_ID => E.Roots (Index).Initialized (C)) then
            raise Program_Error with "live frame root overwritten by another generation";
         end if;
         E.Roots (Index).Frame := Frame;
      end if;
      E.Roots (Index).Initialized (Cell) := Initialized;
      E.Roots (Index).Data (Cell) := Value;
      if Initialized then
         E.Writes := E.Writes + 1;
         if Natural (Index) < E.Active then
            E.Cross_Writes := E.Cross_Writes + 1;
            if Cell /= FH.Local_0 or else Value.Value_Kind /= Integer_Datum or else Value.Number /= 9 then
               raise Program_Error with "incorrect suspended caller root update";
            end if;
         end if;
      else
         E.Clears := E.Clears + 1;
         if Value /= Datum'(Integer_Datum,0,Ordinary_Integer) then
            raise Program_Error with "uncleared frame root payload";
         end if;
      end if;
   end Publish;
   procedure Publish_Held
     (E : in out Mutable; Frame : FH.Frame_Handle; Root : RS.Held_Root;
      Initialized : Boolean; Value : Datum) is
   begin
      if Frame /= E.Roots (FH.Index_Of (Frame)).Frame or else
        not FH.Belongs_To (Frame, FH.Bind_Domain (E.Owner, E.Serial))
      then raise Program_Error with "held root frame identity mismatch"; end if;
      if Root = RS.Returned_Value then
         if not Initialized then raise Program_Error with "empty return publication"; end if;
         if Value.Value_Kind = Object_Datum then
            if Value /= Datum'(Object_Datum,
              (Source => E.Expected_Source, ID => AML_References.Source (E.Expected_Source),
               Type_Code => 3, Size => 1, others => <>))
            then raise Program_Error with "wrong complete Buffer return payload"; end if;
         elsif Value.Value_Kind = Integer_Datum then
            declare
               Current : constant Root_Row := E.Roots (FH.Frame_Index (E.Active));
               Expected : constant Datum :=
                 (if E.Active = 1 and then Current.Initialized (FH.Local_0)
                  then Current.Data (FH.Local_0) else (Integer_Datum, 1, AML_Constant));
            begin
               if Value /= Expected then raise Program_Error with "wrong integer return payload"; end if;
            end;
         else raise Program_Error with "unexpected return payload kind"; end if;
         if Natural (FH.Index_Of (Frame)) = E.Active then
            if E.Return_Order (FH.Frame_Index (E.Active)) /= No_Return_Publication then
               raise Program_Error with "duplicate own return publication";
            end if;
            E.Return_Order (FH.Frame_Index (E.Active)) := Own_Return_Published;
         elsif E.Active > 1 and then Natural (FH.Index_Of (Frame)) = E.Active - 1 then
            if E.Return_Order (FH.Frame_Index (E.Active)) /= Own_Return_Published then
               raise Program_Error with "parent publication before own return";
            end if;
            E.Return_Order (FH.Frame_Index (E.Active)) := Parent_Return_Published;
            E.Held_Returns := E.Held_Returns + 1;
            E.Last_Return_Parent := Frame;
         else raise Program_Error with "wrong return bridge frame"; end if;
      elsif Root = RS.Reference_Read then
         -- This fixture resolves only live frame-cell references internally.
         -- The reader clears its borrowed root even when no root was needed.
         if Natural (FH.Index_Of (Frame)) /= E.Active or else Initialized
           or else Value /= Datum'(Integer_Datum, 0, Ordinary_Integer)
         then raise Program_Error with "invalid frame reference root clear"; end if;
      else
         if Initialized and then (Value.Value_Kind /= Object_Datum
           or else Value.Object.Source /= E.Expected_Source)
         then raise Program_Error with "wrong held selector payload"; end if;
         if E.Active /= 1 or else FH.Index_Of (Frame) /= 1 then
            raise Program_Error with "wrong selector root frame";
         end if;
         E.Region_Roots (Root) := Initialized;
         if not Initialized then
            if Value /= Datum'(Integer_Datum,0,Ordinary_Integer) then
               raise Program_Error with "uncleared selector payload";
            end if;
            E.Region_Clears := E.Region_Clears + 1;
         end if;
      end if;
   end Publish_Held;
   procedure Close_Held (E : in out Mutable; Frame : FH.Frame_Handle) is
   begin
      if FH.Index_Of (Frame) = 1 then
         for Root in RS.Region_Signature .. RS.Region_Table_ID loop
            if E.Region_Roots (Root) then E.Cleanup_Roots := E.Cleanup_Roots + 1; end if;
         end loop;
         E.Region_Roots := [others => False];
      end if;
   end Close_Held;

   procedure Unavailable_Delay (Environment : in out Mutable; Item : AML_Delays.Request; Result : out AML_Delays.Outcome) is
      pragma Unreferenced (Environment);
   begin
      AML_Delays.Unavailable_Provider (Item, Result);
   end Unavailable_Delay;
   procedure Reject_Notify
     (Environment : in out Mutable; Target : AML_References.Reference;
      Value : AML_Decode.Integer_Value; Status : out AML_Execute.Execution_Status)
   is
      pragma Unreferenced (Environment, Target, Value);
   begin
      Status := AML_Execute.Unsupported;
   end Reject_Notify;
   procedure Reject_Metadata_Value
     (Environment : Mutable; Item : AML_Execute.Metadata_Value;
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
      function Execute_Declaration_Valid (Environment : Mutable; Lease : AML_Declaration_Contexts.Lease) return Boolean
      is
         pragma Unreferenced (Environment);
      begin
         return AML_Declaration_Contexts.Is_Live (Execute_Declaration_Lease_Store, Lease);
      end Execute_Declaration_Valid;
      procedure Execute_Declaration_Enter
        (Environment : in out Mutable; Intent : AML_Declaration_Contexts.Request; Scope : Natural;
         Frame : AML_Frame_Handles.Frame_Handle; Lease : out AML_Declaration_Contexts.Lease; Allowed : out Boolean) is
      begin
         Lease := AML_Declaration_Contexts.No_Lease; Allowed := False;
         if Intent /= AML_Declaration_Contexts.Method_Request then return; end if;
         Begin_Call (Environment, Scope, Allowed);
         if not Allowed then return; end if;
         AML_Declaration_Contexts.Open (Execute_Declaration_Lease_Store, Intent, Frame, Scope, 0, Lease, Allowed);
         if not Allowed then End_Call (Environment, Scope); end if;
      end Execute_Declaration_Enter;
      procedure Execute_Declaration_Leave (Environment : in out Mutable; Lease : AML_Declaration_Contexts.Lease) is
      begin
         if not Execute_Declaration_Valid (Environment, Lease) then return; end if;
         End_Call (Environment, AML_Declaration_Contexts.Scope_Of (Lease));
         AML_Declaration_Contexts.Close (Execute_Declaration_Lease_Store, Lease);
      end Execute_Declaration_Leave;
      procedure Execute_Declaration_Method
        (Environment : in out Mutable; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result; Flags : AML_Decode.Byte; Width : AML_Decode.Integer_Width; Code : AML_Decode.Bytes; Status : out AML_Execute.Declaration_Status) is
      begin
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then
            Status := AML_Execute.Declaration_Unsupported; return;
         end if;
         Define_Method (Environment, Scope, Path, Flags, Width, Code, Status);
      end Execute_Declaration_Method;
      procedure Execute_Declaration_Fields
        (Environment : in out Mutable; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Region : AML_Names.Name_Result; Flags : AML_Decode.Byte; Entries : AML_Decode.Bytes; Status : out AML_Execute.Execution_Status) is
      begin
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then
            Status := AML_Execute.Unsupported_Value; return;
         end if;
         Reject_Fields (Environment, Scope, Region, Flags, Entries, Status);
      end Execute_Declaration_Fields;
      procedure Execute_Declaration_Buffer_Field
        (Environment : in out Mutable; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width; Source : AML_Execute.Datum; Bit_Offset, Bit_Length : AML_Decode.Integer_Value; Always_Buffer : Boolean; Status : out AML_Execute.Execution_Status) is
      begin
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then
            Status := AML_Execute.Unsupported_Value; return;
         end if;
         Reject_Buffer_Field (Environment, Scope, Path, Width, Source, Bit_Offset, Bit_Length, Always_Buffer, Status);
      end Execute_Declaration_Buffer_Field;
      procedure Execute_Declaration_Name
        (Environment : in out Mutable; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width; Data : AML_Decode.Bytes;
         Source : aliased AML_Byte_Sources.Storage; Source_First : Natural; Consumed : out Natural; Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Source, Source_First);
      begin
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then
            Consumed := 0; Status := AML_Execute.Unsupported_Value; return;
         end if;
         Reject_Name (Environment, Scope, Path, Width, Data, Consumed, Status);
      end Execute_Declaration_Name;
      procedure Execute_Declaration_Reserve_Region
        (Environment : in out Mutable; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result;
         Token : out AML_Declaration_Contexts.Region_Reservation; Status : out AML_Execute.Execution_Status) is
         Node : Natural;
      begin
         Token := AML_Declaration_Contexts.No_Region; Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then return; end if;
         Test_Region (Environment, Scope, Path, Node, Status);
         if Status in AML_Execute.Returned and then Node /= 0 then
            Token := AML_Declaration_Contexts.Reserve_Region (Lease, Node, 0);
         end if;
      end Execute_Declaration_Reserve_Region;
      procedure Execute_Declaration_Complete_Region
        (Environment : in out Mutable; Lease : AML_Declaration_Contexts.Lease; Input : aliased Backing;
         Token : AML_Declaration_Contexts.Region_Reservation; Width : AML_Decode.Integer_Width;
         Signature, OEM, Table_ID : AML_Execute.Datum; Status : out AML_Execute.Execution_Status) is
         Node : constant Natural := AML_Declaration_Contexts.Node_Of (Token);
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else not AML_Declaration_Contexts.Issued_By (Token, Lease) then return; end if;
         Test_Completion (Environment, Input, Node, Width, Signature, OEM, Table_ID, Status);
      end Execute_Declaration_Complete_Region;
      procedure Execute_Declaration_Reserve_Name
        (Environment : in out Mutable; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result;
         Token : out Execute_Declaration_Name_Token; Status : out AML_Execute.Execution_Status) is
      begin
         Token := Execute_Declaration_No_Name; Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then return; end if;
         Reserve_Dynamic_Name (Environment, Scope, Path, Token.Original, Status);
         if Status in AML_Execute.Returned then Token.Owner := Lease; end if;
      end Execute_Declaration_Reserve_Name;
      procedure Execute_Declaration_Complete_Name
        (Environment : in out Mutable; Lease : AML_Declaration_Contexts.Lease; Token : Execute_Declaration_Name_Token;
         Width : AML_Decode.Integer_Width; Initializer : AML_Decode.Bytes; Count : AML_Data.Count_Result;
         Status : out AML_Execute.Execution_Status) is
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else Token.Owner /= Lease then return; end if;
         Complete_Dynamic_Buffer (Environment, Token.Original, Width, Initializer, Count, Status);
      end Execute_Declaration_Complete_Name;
      procedure Execute_Declaration_Abort_Name
        (Environment : in out Mutable; Lease : AML_Declaration_Contexts.Lease; Token : Execute_Declaration_Name_Token;
         Status : out AML_Execute.Execution_Status) is
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else Token.Owner /= Lease then return; end if;
         Abort_Dynamic_Name (Environment, Token.Original, Status);
      end Execute_Declaration_Abort_Name;
      procedure Reject_Alias
     (Environment : in out Mutable; Lease : AML_Declaration_Contexts.Lease;
      Scope : Natural; Source_Path, Target_Path : AML_Names.Name_Result;
      Status : out AML_Execute.Execution_Status) is
      pragma Unmodified (Environment);
      pragma Unreferenced (Lease, Scope, Source_Path, Target_Path);
   begin Status := AML_Execute.Unsupported; end Reject_Alias;
   procedure Execute_Unsupported_Container_Enter
        (Environment : in out Mutable; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Path : AML_Names.Name_Result;
         Metadata : AML_Container_Headers.Attributes; New_Scope : out Natural;
         Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease, Path, Metadata);
      begin
         New_Scope := Scope; Status := AML_Execute.Unsupported;
      end Execute_Unsupported_Container_Enter;
      procedure Execute_Unsupported_Container_Leave
        (Environment : in out Mutable; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Restored_Scope : out Natural; Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease);
      begin
         Restored_Scope := Scope; Status := AML_Execute.Unsupported;
      end Execute_Unsupported_Container_Leave;
      procedure Provenance_Execute_Source_Resolve
        (Environment : in out Mutable; Ref : AML_References.Reference;
         Width : AML_Decode.Integer_Width; Value : out AML_Execute.Datum;
         Provenance : out AML_Execute.Scalar_Provenance;
         Status : out AML_Execute.Execution_Status) is
      begin
         Provenance := AML_Execute.Detached_Provenance;
         Resolve_Reference (Environment, Ref, Width, Value, Status);
      end Provenance_Execute_Source_Resolve;
      procedure Provenance_Execute_Source_Read_Integer
        (Environment : Mutable; Source : AML_References.Object_Handle;
         Value : out AML_Decode.Integer_Value; Origin : out AML_Decode.Integer_Origin;
         Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Source);
      begin
         Value := 0; Origin := AML_Decode.Ordinary_Integer;
         Status := AML_Execute.Unsupported_Value;
      end Provenance_Execute_Source_Read_Integer;
      procedure Provenance_Execute_Source_Read_Integer_Normalize
        (Environment : in out Mutable; Source : AML_References.Object_Handle;
         Width : AML_Decode.Integer_Width; Value : out AML_Decode.Integer_Value;
         Origin : out AML_Decode.Integer_Origin; Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Source, Width);
      begin
         Value := 0; Origin := AML_Decode.Ordinary_Integer;
         Status := AML_Execute.Unsupported_Value;
      end Provenance_Execute_Source_Read_Integer_Normalize;
      procedure Provenance_Execute_Source_Read_Integer_Normalize_Census_Begin
        (Environment : Mutable; Source : AML_References.Object_Handle;
         Status : out AML_Execute.Owner_Census_Status) is
         pragma Unreferenced (Environment, Source);
      begin Status := AML_Execute.Census_Invalid; end Provenance_Execute_Source_Read_Integer_Normalize_Census_Begin;
      procedure Provenance_Execute_Source_Read_Integer_Normalize_Census_Add
        (Environment : Mutable; Value : AML_Execute.Datum;
         Status : out AML_Execute.Owner_Census_Status) is
         pragma Unreferenced (Environment, Value);
      begin Status := AML_Execute.Census_Invalid; end Provenance_Execute_Source_Read_Integer_Normalize_Census_Add;
      procedure Provenance_Execute_Source_Read_Integer_Normalize_Census_Finish
        (Environment : Mutable; Status : out AML_Execute.Owner_Census_Status) is
         pragma Unreferenced (Environment);
      begin Status := AML_Execute.Census_Invalid; end Provenance_Execute_Source_Read_Integer_Normalize_Census_Finish;

      procedure Package_Adapter_Begin
        (Environment : in out Mutable; Lease : AML_Declaration_Contexts.Lease;
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
        (Environment : in out Mutable; Lease : AML_Declaration_Contexts.Lease;
         Token : in out Natural; Item : AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease, Token, Item);
      begin
         Status := AML_Execute.Unsupported_Value;
      end Package_Adapter_Append;
      procedure Package_Adapter_Defer
        (Environment : Mutable; Lease : AML_Declaration_Contexts.Lease;
         Token : in out Natural; Scope : Natural;
         Path : AML_Names.Name_Result; Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease, Token, Scope, Path);
      begin
         Status := AML_Execute.Unsupported_Value;
      end Package_Adapter_Defer;
      procedure Package_Adapter_Finish
        (Environment : in out Mutable; Lease : AML_Declaration_Contexts.Lease;
         Token : in out Natural; Binding : out AML_Execute.Binding_Result) is
         pragma Unreferenced (Environment, Lease, Token);
      begin
         Binding := (Status => AML_Execute.Failed_Binding, Failure => AML_Execute.Unsupported_Value);
      end Package_Adapter_Finish;
      -- Rejected builders never own an allocation or root to unwind.
      procedure Package_Adapter_Abort
        (Environment : in out Mutable; Lease : AML_Declaration_Contexts.Lease;
         Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease);
      begin
         Status := AML_Execute.Returned;
      end Package_Adapter_Abort;
      procedure Execute_Source is new AML_Execute.Execute_From_Source
        (Mutable,
         Backing,
         Valid,
         Lookup,
         Get_Method,
         Write,
         Execute_Declaration_Enter,
         Execute_Declaration_Leave,
         Execute_Declaration_Method,
         Execute_Declaration_Fields,
         Reject_Literal,
         Execute_Declaration_Reserve_Region,
         Execute_Declaration_Complete_Region,
         No_Timer,
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
         Handoff_Result => Handoff,
         Publish_Frame_Root => Publish,
         Close_Frame_Root => Close_Held,
         Publish_Held_Root => Publish_Held,
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
         Input : aliased Backing; Environment : in out Mutable; Scope : Natural;
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
   Input : aliased Backing (1);
   Environment : Mutable;
   Result : Execution_Result;
   Arguments : Value_Arguments := [others => (Integer_Datum,0,Ordinary_Integer)];
   ID : AML_Objects.Object_ID;
   Alloc : AML_Objects.Allocation_Status;
   Issued : Boolean;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image & " region completions" & Natural'Image (Environment.Region_Completions) & " clears" & Natural'Image (Environment.Region_Clears) & " cleanup" & Natural'Image (Environment.Cleanup_Roots); end if; end Check;
   procedure Run (Code : Bytes; Expected_Status : Execution_Status; Calls : Call_Budget := 32) is
   begin
      Check ((for all Phase of Environment.Return_Order => Phase = No_Return_Publication));
      Environment.Return_Order := [others => No_Return_Publication];
      Environment.Handoffs := 0;
      Environment.Held_Returns := 0; Environment.Last_Return_Parent := FH.No_Frame;
      Environment.Selector_Calls := 0; Environment.Region_Completions := 0;
      Environment.Region_Clears := 0; Environment.Cleanup_Roots := 0;
      Environment.Writes := 0; Environment.Clears := 0; Environment.Cross_Writes := 0;
      Environment.Expected := Expected_Status = Object_Returned;
      Environment.Expected_Success := Expected_Status in Returned | Object_Returned | Reference_Returned | Metadata_Returned;
      Environment.Ordered := True;
      Execute (Code, Bits_64, Arguments, 1, 100, Input, Environment, 0, Result, Calls_Left => Calls);
      Check (Result.Status = Expected_Status);
      Check (Environment.Ordered and then Environment.Active = 0);
      Check (Environment.Handoffs = (if Environment.Expected then 1 else 0));
      Check (Environment.Writes > 0 and then Environment.Clears >= 15);
      Check ((for all F in Environment.Roots'Range =>
        (for all C in FH.Cell_ID => not Environment.Roots (F).Initialized (C)
          and then Environment.Roots (F).Data (C) = Datum'(Integer_Datum,0,Ordinary_Integer))));
   end Run;
begin
   AML_Identity.Issuer.Issue (Environment.Owner, Issued); Check (Issued);
   AML_Objects.New_Bytes (Environment.Values, AML_Objects.Buffer_Object, [16#55#], ID, Alloc);
   Check (Alloc = AML_Objects.Allocated);
   Environment.Expected_Source := AML_References.Bind_Object
     (Environment.Owner, AML_Objects.Address_Of (Environment.Values, ID));
   Arguments (0) := (Object_Datum, (ID => ID, Type_Code => 3, Size => 1,
     Source => Environment.Expected_Source, others => <>));
   Run ([16#A4#,16#68#], Object_Returned);
   Run ([16#A4#,77,84,72,48,16#68#], Object_Returned);
   Run ([16#A4#,77,84,72,49,16#68#], Object_Returned);
   Run ([77,84,72,48,16#68#,16#A4#,1], Returned);
   Run ([16#A4#,77,84,72,49,16#68#], Call_Limit, 1);
   Run ([16#A4#,16#68#], Object_Returned, 0);
   Run (Bytes'(1 .. 0 => 0), No_Return);
   Run ([16#FE#], Unsupported);
   Run ([16#70#,1,16#60#,16#A4#,16#60#], Returned);
   Check (Result.Value = 1 and then Environment.Writes = 2);
   Run ([16#70#,1,16#60#,77,84,72,50,16#71#,16#60#,16#A4#,16#60#], Returned);
   Check (Result.Value = 9 and then Environment.Cross_Writes = 1 and then Environment.Writes = 4);
   -- A second call reuses the nested slot with a new frame generation.
   Run ([16#70#,1,16#60#,77,84,72,50,16#71#,16#60#,
     77,84,72,50,16#71#,16#60#,16#A4#,16#60#], Returned);
   Check (Result.Value = 9 and then Environment.Cross_Writes = 2 and then Environment.Writes = 6);
   Environment.Deny := True;
   Run ([16#A4#,16#68#], Namespace_Limit);
   Check (Environment.Writes = 1 and then Environment.Clears = 15);
   Environment.Deny := False; Environment.Region_Mode := True;
   declare
      Prefix : constant Bytes := [16#5B#,16#88#,82,71,78,48];
      Selector : constant Bytes := [77,84,72,48,16#68#];
      Code : constant Bytes := Prefix & Selector & Selector & Selector & Bytes'(16#A4#,1);
   begin
      Run (Code, Returned);
      Check (Environment.Held_Returns = 3 and then Environment.Selector_Calls = 3
        and then Environment.Region_Completions = 1 and then Environment.Region_Clears = 3
        and then Environment.Cleanup_Roots = 0);
      Environment.Fail_Region := True; Run (Code, Unsupported_Value);
      Check (Environment.Region_Completions = 1 and then Environment.Region_Clears = 3
        and then Environment.Cleanup_Roots = 0);
      Environment.Fail_Region := False; Run (Prefix & Selector & Bytes'(1 => 16#FE#), Unsupported);
      Check (Environment.Region_Completions = 0 and then Environment.Region_Clears = 3
        and then Environment.Cleanup_Roots = 0);
   end;
   Ada.Text_IO.Put_Line ("HELD ROOT PUBLICATION" & Checks'Image);
end Held_Publication_Tests;
