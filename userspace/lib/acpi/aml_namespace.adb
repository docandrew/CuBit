pragma Ada_2022;
with AML_Mixed_Comparison;
with AML_Resource_Templates;
with AML_Explicit_Formatting;
with AML_Slices;
with AML_Concatenation;
with AML_Identity.Issuer;
with AML_Objects.Copies;
with AML_Objects.Reclamation;
with AML_Root_Slots;
with AML_Coercions.Strings;
with Firmware_Tables.Identifiers;
with AML_Fields;
with AML_Coercions;
with AML_Integers;
package body AML_Namespace with SPARK_Mode is
   use type AML_Execute.Datum_Kind;
   use type AML_Execute.Binding_Purpose;
   use type AML_References.Node_Position;
   use type AML_Decode.Bytes;
   use type AML_Objects.State;
   use type AML_Execute.Declaration_Status;
   use type AML_Execute.Execution_Status;
   subtype Code_Storage is AML_Decode.Bytes (1 .. Aggregate_Method_Capacity);
   procedure Append_Code
     (Store : in out Code_Storage; Used : in out Aggregate_Method_Count;
      Data : AML_Decode.Bytes; Start : out Aggregate_Method_Count)
     with Pre => Data'Length <= Store'Length - Used,
          Post => Start = Used'Old and then Used = Used'Old + Data'Length
            and then Store (1 .. Used'Old) = Store'Old (1 .. Used'Old)
            and then (if Data'Length > 0 then Store (Start + 1 .. Used) = Data)
   is
      use type AML_Decode.Byte;
   begin
      Start := Used;
      for I in 1 .. Data'Length loop
         pragma Loop_Invariant
           (for all J in 1 .. Used => Store (J) = Store'Loop_Entry (J));
         pragma Loop_Invariant
           (for all J in 1 .. I - 1 => Store (Used + J) = Data (Data'First + (J - 1)));
         Store (Used + I) := Data (Data'First + (I - 1));
      end loop;
      Used := Used + Data'Length;
   end Append_Code;
   function Count (Tree : State) return Node_ID is (Tree.Used);
   function Last_Incarnation (Tree : State) return AML_References.Node_Incarnation is (Tree.Last_Stamp);
   function Incarnation_Of (Tree : State; Node : Node_ID) return AML_References.Node_Incarnation is
     (Tree.Items (Node).Incarnation);
   function Present (Tree : State; Node : Node_ID) return Boolean is
     (Node = Root or else Tree.Items (Node).Alive);
   function Method_Usage (Tree : State) return Aggregate_Method_Count is (Tree.Code_Used);
   function Method_Data (Tree : State; Node : Node_ID) return AML_Decode.Bytes is
     (if Tree.Items (Node).Method_Size = 0 then [1 .. 0 => 0]
      else Tree.Code (Tree.Items (Node).Method_Offset + 1 ..
                      Tree.Items (Node).Method_Offset + Tree.Items (Node).Method_Size));
   function Value_Usage (Tree : State) return AML_Objects.Usage is
     (AML_Objects.Usage_Of (Tree.Values));
   function Value_Store (Tree : State) return AML_Objects.State is (Tree.Values);
   function Data_Object (Tree : State; Node : Node_ID) return AML_Objects.Object_ID is
     (Tree.Items (Node).Object_Ref);
   function Integer_Updated
     (Tree, Prior : State; Node : Node_ID; Value : AML_Decode.Integer_Value)
      return Boolean is
     (Tree.Last_Stamp = Prior.Last_Stamp and then Tree.Used = Prior.Used and then Tree.Items = Prior.Items
      and then Tree.Code = Prior.Code and then Tree.Code_Used = Prior.Code_Used
      and then AML_Objects.Integer_Updated
        (Tree.Values, Prior.Values, Prior.Items (Node).Object_Ref, Value));
   procedure Set_Integer
     (Tree : in out State; Node : Node_ID; Value : AML_Decode.Integer_Value) is
   begin
      AML_Objects.Set_Integer (Tree.Values, Tree.Items (Node).Object_Ref, Value);
   end Set_Integer;
   function Parent (Tree : State; Node : Node_ID) return Node_ID is
     (if Node = Root then Root else Tree.Items (Node).Up);
   function Name (Tree : State; Node : Node_ID) return AML_Names.Segment is
     (Tree.Items (Node).Part);
   function Cleanup_Frame (Tree, Prior : State) return Boolean is
     (Tree.Last_Stamp >= Prior.Last_Stamp
      and then Tree = (Prior with delta Last_Stamp => Tree.Last_Stamp));
   function Allocating_Cleanup_Frame (Tree, Prior : State) return Boolean is
     (Tree.Last_Stamp >= Prior.Last_Stamp
      and then Tree = (Prior with delta Last_Stamp => Tree.Last_Stamp, Values => Tree.Values)
      and then AML_Objects.Extends (Tree.Values, Prior.Values));
   function Empty return State is
     ((Used => 0, Items => [others => (Up => Root, Part => "____", others => <>)],
       Values => AML_Objects.Empty, others => <>));

   function Child
     (Tree : State; Scope : Node_ID; Part : AML_Names.Segment) return Node_ID
   is
   begin
      for I in 1 .. Tree.Used loop
         pragma Loop_Invariant
           (for all J in 1 .. I - 1 =>
              not Tree.Items (J).Alive or else Tree.Items (J).Up /= Scope or else Tree.Items (J).Part /= Part);
         if Tree.Items (I).Alive and then Tree.Items (I).Up = Scope and then Tree.Items (I).Part = Part then
            return I;
         end if;
      end loop;
      return Root;
   end Child;

   function Resolve
     (Tree : State; Scope : Node_ID; Path : AML_Names.Name_Result)
      return Lookup_Result
   is
      Base : Node_ID := Scope;
      Next : Node_ID;
   begin
      if Path.Kind /= AML_Names.Accepted then
         return (Status => Invalid_Path);
      end if;
      if Path.Rooted and then Path.Parents /= 0 then
         return (Status => Invalid_Path);
      end if;
      for I in 1 .. Path.Count loop
         if not AML_Names.Valid (Path.Parts (I)) then
            return (Status => Invalid_Path);
         end if;
      end loop;
      if Path.Rooted then
         Base := Root;
      end if;
      for I in 1 .. Path.Parents loop
         pragma Loop_Invariant (Base <= Count (Tree));
         if Base = Root then
            return (Status => Above_Root);
         end if;
         Base := Parent (Tree, Base);
      end loop;
      if not Path.Rooted and then Path.Parents = 0 and then Path.Count = 1 then
         loop
            pragma Loop_Invariant (Base <= Count (Tree));
            pragma Loop_Variant (Decreases => Base);
            Next := Child (Tree, Base, Path.Parts (1));
            if Next /= Root then
               return (Status => Found, Node => Next);
            elsif Base = Root then
               return (Status => Not_Found);
            end if;
            Base := Parent (Tree, Base);
         end loop;
      end if;
      for I in 1 .. Path.Count loop
         pragma Loop_Invariant (Base <= Count (Tree));
         pragma Loop_Invariant
           (if I > 1 then Base /= Root and then
              Name (Tree, Base) = Path.Parts (I - 1));
         Next := Child (Tree, Base, Path.Parts (I));
         if Next = Root then
            return (Status => Not_Found);
         end if;
         Base := Next;
      end loop;
      return (Status => Found, Node => Base);
   end Resolve;

   function Insert_Frame (Tree, Prior : State) return Boolean is
     (Tree.Values = Prior.Values and then Tree.Code = Prior.Code
      and then Tree.Code_Used = Prior.Code_Used and then
      (for all I in 1 .. Prior.Used => Tree.Items (I) = Prior.Items (I)));
   procedure Insert
     (Tree : in out State; Scope : Node_ID; Part : AML_Names.Segment;
      Node : out Node_ID; Result : out Insert_Status)
   is
   begin
      Node := Root;
      if not AML_Names.Valid (Part) then
         Result := Invalid_Name;
      elsif Child (Tree, Scope, Part) /= Root then
         Result := Duplicate;
      elsif Tree.Used = Capacity or else Tree.Last_Stamp = Max_Node_Incarnation then
         Result := Full;
      else
         Tree.Used := Tree.Used + 1;
         Tree.Last_Stamp := Tree.Last_Stamp + 1;
         Tree.Items (Tree.Used) := (Up => Scope, Part => Part, Incarnation => Tree.Last_Stamp, others => <>);
         Node := Tree.Used;
         Result := Inserted;
      end if;
   end Insert;
   function Kind (Tree : State; Node : Node_ID) return Object_Kind is
     (if Node = Root then Scope_Object else Tree.Items (Node).Object_Type);
   function Mutex_Data (Tree : State; Node : Node_ID) return Mutex_Metadata is
     (Raw_Flags => Tree.Items (Node).Mutex_Flags,
      Level => Sync_Level (Natural (Tree.Items (Node).Mutex_Flags) mod (Natural (Sync_Level'Last) + 1)),
      Canonical => Natural (Tree.Items (Node).Mutex_Flags) <= Natural (Sync_Level'Last));
   function Processor_Data (Tree : State; Node : Node_ID) return Processor_Attributes is
     (Tree.Items (Node).Processor_Info);
   function Power_Data (Tree : State; Node : Node_ID) return Power_Attributes is
     (Tree.Items (Node).Power_Info);
   function Static_Scope (Object_Type : Object_Kind) return Boolean is
     (Object_Type in Scope_Object | Device_Object | Power_Resource_Object
        | Processor_Object | Thermal_Zone_Object);
   function Declaration_Scope (Object_Type : Object_Kind) return Boolean is
     (Static_Scope (Object_Type) or else Object_Type = Method_Object);
   function Operation_Region_Data (Tree : State; Node : Node_ID) return Operation_Region_Attributes is
     (Tree.Items (Node).Operation_Info);
   function Region_Field_Data (Tree : State; Node : Node_ID) return Region_Field_Attributes is
     (Tree.Items (Node).Region_Field_Info);
   function Region_Data (Tree : State; Node : Node_ID) return Table_Region is
     (Tree.Items (Node).Table_Binding.Region);
   function Field_Data (Tree : State; Node : Node_ID) return Table_Field is
     (Tree.Items (Node).Table_Binding);
   procedure Bind_Table_Field
     (Tree : in out State; Scope : Node_ID; Part : AML_Names.Segment;
      Field : Table_Field; Node : out Node_ID; Result : out Bind_Status;
      Owner : Node_ID := Root)
   is
      Added : Insert_Status;
   begin
      Node := Root;
      Result := Binding_Invalid;
      if not Present (Tree, Scope) or else
        not Declaration_Scope (Kind (Tree, Scope)) or else
        (Owner /= Root and then (not Present (Tree, Owner) or else
          Kind (Tree, Owner) /= Method_Object or else Tree.Items (Owner).Active_Calls = 0)) or else
        not AML_Field_Data.Fits (Field.Region.Extent, Field.Offset, Field.Bits)
      then return; end if;
      Insert (Tree, Scope, Part, Node, Added);
      case Added is
         when Duplicate => Result := Binding_Duplicate;
         when Full => Result := Binding_Full;
         when Invalid_Name => null;
         when Inserted =>
            Tree.Items (Node).Table_Binding := Field;
            Tree.Items (Node).Object_Type := Table_Field_Object;
            Tree.Items (Node).Owner := Owner;
            Result := Bound;
      end case;
   end Bind_Table_Field;
   procedure Bind_Table_Region
     (Tree : in out State; Scope : Node_ID; Part : AML_Names.Segment;
      Region : Table_Region; Node : out Node_ID; Result : out Bind_Status;
      Owner : Node_ID := Root)
   is
   begin
      Bind_Table_Field (Tree, Scope, Part, (Region => Region, others => <>), Node, Result, Owner);
      if Result = Bound then Tree.Items (Node).Object_Type := Table_Region_Object; end if;
   end Bind_Table_Region;
   function Has_Integer (Tree : State; Node : Node_ID) return Boolean is
     (Node /= Root and then Tree.Items (Node).Object_Type = Integer_Object);
   function Integer_Data (Tree : State; Node : Node_ID)
      return AML_Decode.Integer_Value is (AML_Objects.Integer_Data (Tree.Values, Tree.Items (Node).Object_Ref));

   function String_Data (Tree : State; Node : Node_ID) return String is
      Data : constant AML_Decode.Bytes :=
        AML_Objects.Byte_Data (Tree.Values, Tree.Items (Node).Object_Ref);
      Text : String (1 .. Data'Length);
   begin
      for I in Text'Range loop
         Text (I) := Character'Val (Data (Data'First + (I - 1)));
      end loop;
      return Text;
   end String_Data;

   function Buffer_Data (Tree : State; Node : Node_ID) return AML_Decode.Bytes is
      Source : constant AML_Decode.Bytes :=
        AML_Objects.Byte_Data (Tree.Values, Tree.Items (Node).Object_Ref);
      Result : constant AML_Decode.Bytes (1 .. Source'Length) := Source;
   begin
      return Result;
   end Buffer_Data;

   function Valid_Context (Tree : State) return Boolean is
     (Tree.Last_Stamp <= Max_Node_Incarnation and then Pending.Valid (Tree.Journal) and then AML_Objects.Valid (Tree.Values) and then
       (for all I in 1 .. Tree.Used => Tree.Items (I).Incarnation > 0
          and then Tree.Items (I).Incarnation <= Tree.Last_Stamp and then Tree.Items (I).Up < I and then Tree.Items (I).Owner < I and then
          (if Tree.Items (I).Initializing then
             Tree.Items (I).Alive and then Tree.Items (I).Owner > Root
             and then Tree.Items (Tree.Items (I).Owner).Alive
             and then Tree.Items (Tree.Items (I).Owner).Object_Type = Method_Object
             and then Tree.Items (Tree.Items (I).Owner).Active_Calls > 0
             and then Tree.Items (I).Object_Type in Uninitialized_Name_Object | Integer_Object
               | String_Object | Buffer_Object | Package_Object | Reference_Object) and then
          (if Tree.Items (I).Object_Type = Uninitialized_Name_Object then
             Tree.Items (I).Object_Ref = 0
             and then (not Tree.Items (I).Alive or else Tree.Items (I).Initializing)) and then
          (if Tree.Items (I).Object_Type = Region_Field_Object then
             Tree.Items (I).Region_Field_Info.Region > Root
             and then Tree.Items (I).Region_Field_Info.Region < I
             and then Tree.Items (Tree.Items (I).Region_Field_Info.Region).Object_Type = Operation_Region_Object
             and then Tree.Items (Tree.Items (I).Region_Field_Info.Region).Incarnation = Tree.Items (I).Region_Field_Info.Incarnation
             and then Field_Bit_Position (Tree.Items (I).Region_Field_Info.Bits) <=
               Field_Bit_Position'Last - Tree.Items (I).Region_Field_Info.Offset) and then
          (if Tree.Items (I).Object_Type = Table_Field_Object then
             AML_Field_Data.Fits (Tree.Items (I).Table_Binding.Region.Extent,
               Tree.Items (I).Table_Binding.Offset, Tree.Items (I).Table_Binding.Bits)) and then
          Tree.Items (I).Method_Offset <= Tree.Code_Used and then
          Tree.Items (I).Method_Size <= Tree.Code_Used - Tree.Items (I).Method_Offset and then
          (if Tree.Items (I).Object_Type in Integer_Object | String_Object | Buffer_Object | Package_Object | Reference_Object then
              AML_Objects.Is_Live (Tree.Values, Tree.Items (I).Object_Ref) and then
              AML_Objects.Kind (Tree.Values, Tree.Items (I).Object_Ref) =
                (case Tree.Items (I).Object_Type is
                   when Integer_Object => AML_Objects.Integer_Object,
                   when String_Object => AML_Objects.String_Object,
                   when Buffer_Object => AML_Objects.Buffer_Object,
                   when Reference_Object => AML_Objects.Reference_Object,
                   when others => AML_Objects.Package_Object))));
   type Count_Context is record
      Tree : State;
      Scope : Node_ID;
   end record;
   function Read_Package_Count
     (Environment : Count_Context; Data : AML_Decode.Bytes;
      Width : AML_Decode.Integer_Width) return AML_Data.Count_Result
     with Post => (if AML_Decode."=" (Read_Package_Count'Result.Kind, AML_Decode.Accepted) then
       Read_Package_Count'Result.Consumed <= Data'Length)
   is
      use type AML_Decode.Status;
      Literal : constant AML_Decode.Integer_Result := AML_Decode.Read_Integer (Data, Width);
      Path : AML_Names.Name_Result;
      Located : Lookup_Result;
   begin
      if Literal.Kind = AML_Decode.Accepted then
         return (AML_Decode.Accepted, Literal.Value, Literal.Consumed);
      elsif Literal.Kind /= AML_Decode.Unsupported then
         return (Kind => Literal.Kind, others => <>);
      end if;
      if not Valid_Context (Environment.Tree) or else Environment.Scope > Environment.Tree.Used then
         return (Kind => AML_Decode.Unsupported, others => <>);
      end if;
      Path := AML_Names.Read_Name (Data);
      if Path.Kind /= AML_Names.Accepted then return (Kind => AML_Decode.Unsupported, others => <>); end if;
      Located := Resolve (Environment.Tree, Environment.Scope, Path);
      if Located.Status /= Found or else not Has_Integer (Environment.Tree, Located.Node) then
         return (Kind => AML_Decode.Unsupported, others => <>);
      end if;
      return (AML_Decode.Accepted, Integer_Data (Environment.Tree, Located.Node), Path.Consumed);
   end Read_Package_Count;

   -- Static Buffer sizes admit current canonical values only. Package count
   -- policy deliberately remains separate; neither path executes AML here.
   function Read_Static_Buffer_Count
     (Environment : Count_Context; Data : AML_Decode.Bytes;
      Width : AML_Decode.Integer_Width) return AML_Data.Count_Result
     with Post => (if AML_Decode."=" (Read_Static_Buffer_Count'Result.Kind, AML_Decode.Accepted) then
       Read_Static_Buffer_Count'Result.Consumed <= Data'Length)
   is
      use type AML_Decode.Status;
      use type AML_Coercions.Conversion_Status;
      Prior : constant AML_Data.Count_Result := Read_Package_Count (Environment, Data, Width);
      Path : AML_Names.Name_Result;
      Located : Lookup_Result;
      ID : AML_Objects.Object_ID;
      Converted : AML_Coercions.Result;
   begin
      if Prior.Kind /= AML_Decode.Unsupported then return Prior; end if;
      if not Valid_Context (Environment.Tree) or else Environment.Scope > Environment.Tree.Used then
         return (Kind => AML_Decode.Unsupported, others => <>);
      end if;
      Path := AML_Names.Read_Name (Data);
      if Path.Kind /= AML_Names.Accepted or else Path.Count = 0 then
         return (Kind => AML_Decode.Unsupported, others => <>);
      end if;
      Located := Resolve (Environment.Tree, Environment.Scope, Path);
      if Located.Status /= Found or else Kind (Environment.Tree, Located.Node)
        not in String_Object | Buffer_Object then
         return (Kind => AML_Decode.Unsupported, others => <>);
      end if;
      ID := Environment.Tree.Items (Located.Node).Object_Ref;
      if not AML_Objects.Is_Live (Environment.Tree.Values, ID) then
         return (Kind => AML_Decode.Unsupported, others => <>);
      end if;
      if Kind (Environment.Tree, Located.Node) = String_Object
        and then AML_Objects.Kind (Environment.Tree.Values, ID) = AML_Objects.String_Object then
         Converted := AML_Coercions.From_String
           (AML_Objects.Byte_Data (Environment.Tree.Values, ID), Width);
      elsif Kind (Environment.Tree, Located.Node) = Buffer_Object
        and then AML_Objects.Kind (Environment.Tree.Values, ID) = AML_Objects.Buffer_Object then
         Converted := AML_Coercions.From_Buffer
           (AML_Objects.Byte_Data (Environment.Tree.Values, ID), Width);
      else return (Kind => AML_Decode.Unsupported, others => <>);
      end if;
      if Converted.Status /= AML_Coercions.Converted then
         return (Kind => AML_Decode.Unsupported, others => <>);
      end if;
      return (AML_Decode.Accepted, Converted.Value, Path.Consumed);
   end Read_Static_Buffer_Count;

   procedure Defer_Package_Member
     (Environment : in out Count_Context; Package_ID : AML_Objects.Object_ID;
      Element : Natural; Data : AML_Decode.Bytes; Result : out AML_Data.Member_Result)
   is
      Path : constant AML_Names.Name_Result := AML_Names.Read_Name (Data);
      Status : Pending.Append_Status;
      use type Pending.Append_Status;
   begin
      Pending.Append (Environment.Tree.Journal, Package_ID, Element,
                      Environment.Scope, Path, Status);
      if Status = Pending.Appended then
         Result := (AML_Decode.Accepted, AML_Data.Deferred_Member, 0, Path.Consumed);
      elsif Status in Pending.Member_Limit | Pending.Segment_Limit then
         Result := (Kind => AML_Decode.Limit_Exceeded, others => <>);
      else Result := (Kind => AML_Decode.Malformed, others => <>); end if;
   end Defer_Package_Member;
   procedure Read_Package_Member
     (Environment : in out Count_Context; Package_ID : AML_Objects.Object_ID;
      Element : Natural; Data : AML_Decode.Bytes; Result : out AML_Data.Member_Result)
   is
      pragma Unreferenced (Package_ID, Element);
      Path : constant AML_Names.Name_Result := AML_Names.Read_Name (Data);
      Located : Lookup_Result;
   begin
      Result := (Kind => AML_Decode.Unsupported, others => <>);
      if Path.Kind /= AML_Names.Accepted or else Path.Count = 0
        or else not Valid_Context (Environment.Tree)
        or else Environment.Scope > Environment.Tree.Used then return; end if;
      Located := Resolve (Environment.Tree, Environment.Scope, Path);
      if Located.Status /= Found or else Environment.Tree.Items (Located.Node).Object_Type
        not in Integer_Object | String_Object | Buffer_Object | Package_Object | Reference_Object then return; end if;
      Result := (AML_Decode.Accepted, AML_Data.Resolved_Member,
                 Environment.Tree.Items (Located.Node).Object_Ref, Path.Consumed);
   end Read_Package_Member;
   -- Static packages still require a separate post-declaration binding phase.
   procedure Load_Bound_Data is new AML_Data.Load_Bound
     (Count_Context, Read_Package_Count, Defer_Package_Member);
   procedure Load_Runtime_Data is new AML_Data.Load_Bound
     (Count_Context, Read_Package_Count, Read_Package_Member);

   function Read_Binding
     (Tree : State; Scope : Natural; Path : AML_Names.Name_Result;
      Width : AML_Decode.Integer_Width)
      return AML_Execute.Binding_Result
   is
      use type AML_Decode.Byte;
      Located : Lookup_Result;
      Conversion_32, Conversion_64 : AML_Coercions.Result;
      pragma Unreferenced (Width);
   begin
      --  The generic callback boundary does not carry State's private type
      --  invariant. Check it explicitly before using namespace operations.
      if not Valid_Context (Tree) then
         return (Status => AML_Execute.Missing_Binding);
      end if;
      if Scope > Count (Tree) then
         return (Status => AML_Execute.Missing_Binding);
      end if;
      Located := Resolve (Tree, Node_ID (Scope), Path);
      if Located.Status /= Found then
         return (Status => AML_Execute.Missing_Binding);
      elsif Kind (Tree, Located.Node) in Uninitialized_Region_Object | Uninitialized_Name_Object then
         return (Status => AML_Execute.Failed_Binding, Failure => AML_Execute.Uninitialized);
      elsif Kind (Tree, Located.Node) in Operation_Region_Object | Region_Field_Object then
         -- Purpose-less legacy readers cannot turn metadata into a value.
         return (Status => AML_Execute.Failed_Binding, Failure => AML_Execute.Unsupported_Value);
      elsif Kind (Tree, Located.Node) = Method_Object then
         return (Status => AML_Execute.Method_Binding,
                 Method_ID => Natural (Located.Node),
                 Parameters => Natural (Tree.Items (Located.Node).Method_Flags mod 8));
      elsif Kind (Tree, Located.Node) = Reference_Object then
         return (Status => AML_Execute.Reference_Binding,
           Ref => AML_Objects.Reference_Data (Tree.Values, Tree.Items (Located.Node).Object_Ref));
      elsif not Has_Integer (Tree, Located.Node) then
         if Kind (Tree, Located.Node) = String_Object then
            Conversion_32 := AML_Coercions.From_String
              (AML_Objects.Byte_Data (Tree.Values, Tree.Items (Located.Node).Object_Ref), AML_Decode.Bits_32);
            Conversion_64 := AML_Coercions.From_String
              (AML_Objects.Byte_Data (Tree.Values, Tree.Items (Located.Node).Object_Ref), AML_Decode.Bits_64);
         elsif Kind (Tree, Located.Node) = Buffer_Object then
            Conversion_32 := AML_Coercions.From_Buffer
              (AML_Objects.Byte_Data (Tree.Values, Tree.Items (Located.Node).Object_Ref), AML_Decode.Bits_32);
            Conversion_64 := AML_Coercions.From_Buffer
              (AML_Objects.Byte_Data (Tree.Values, Tree.Items (Located.Node).Object_Ref), AML_Decode.Bits_64);
         end if;
         return (Status => AML_Execute.Non_Integer_Binding,
                 Object => (Source => AML_References.No_Object_Handle, ID => (if Kind (Tree, Located.Node) in String_Object | Buffer_Object | Package_Object
                   then Tree.Items (Located.Node).Object_Ref else 0),
                 Conversion_32 => Conversion_32, Conversion_64 => Conversion_64,
                 Type_Code => (case Kind (Tree, Located.Node) is
                   when String_Object => 2, when Buffer_Object => 3,
                   when Package_Object => 4, when Device_Object => 6,
                   when Event_Object => 7, when Mutex_Object => 9,
                   when Power_Resource_Object => 11, when Processor_Object => 12,
                   when Thermal_Zone_Object => 13,
                   when Table_Region_Object | Operation_Region_Object => 10, when Table_Field_Object | Region_Field_Object => 5,
                   when others => 0),
                 Size => (if Kind (Tree, Located.Node) in String_Object | Buffer_Object | Package_Object
                   then AML_Objects.Length (Tree.Values, Tree.Items (Located.Node).Object_Ref)
                   else 0)));
      end if;
      return (Status => AML_Execute.Integer_Binding,
              Value => Integer_Data (Tree, Located.Node),
              Origin => AML_Objects.Origin_Of (Tree.Values, Tree.Items (Located.Node).Object_Ref));
   end Read_Binding;
   function Read_Method (Tree : State; ID : Natural)
      return AML_Execute.Method_Definition
   is
   begin
      if not Valid_Context (Tree) or else ID > Tree.Used then
         return (Exists => False, Length => 0);
      end if;
      if not Present (Tree, Node_ID (ID)) or else Kind (Tree, Node_ID (ID)) /= Method_Object then
         return (Exists => False, Length => 0);
      end if;
      return (Exists => True, Code => Method_Data (Tree, Node_ID (ID)),
              Length => Tree.Items (ID).Method_Size,
              Width => Tree.Items (ID).Method_Width,
              Flags => Tree.Items (ID).Method_Flags, Scope => ID);
   end Read_Method;
   function Execute_Bound is new AML_Execute.Run_Bound (State, Valid_Context, Read_Binding, Read_Method);

   procedure Write_Binding
     (Tree : in out State; Scope : Natural; Path : AML_Names.Name_Result;
      Width : AML_Decode.Integer_Width; Item : AML_Execute.Datum;
      Status : out AML_Execute.Write_Status)
     with Pre => Valid_Context (Tree), Post => Valid_Context (Tree)
   is
      Located : Lookup_Result;
   begin
      Status := AML_Execute.Write_Unsupported;
      if not Valid_Context (Tree) then return; end if;
      if Scope > Tree.Used then Status := AML_Execute.Write_Missing; return; end if;
      Located := Resolve (Tree, Node_ID (Scope), Path);
      if Located.Status /= Found then Status := AML_Execute.Write_Missing; return; end if;
      if not Has_Integer (Tree, Located.Node) or else Item.Value_Kind /= AML_Execute.Integer_Datum then return; end if;
      Set_Integer (Tree, Located.Node, AML_Integers.Normalize (Item.Number, Width));
      Status := AML_Execute.Written;
   end Write_Binding;
   procedure Begin_Method (Tree : in out State; Scope : Natural; Allowed : out Boolean)
     with Pre => Valid_Context (Tree), Post => Valid_Context (Tree)
   is
   begin
      Allowed := False;
      if Scope = 0 or else Scope > Tree.Used or else not Tree.Items (Scope).Alive
        or else Tree.Items (Scope).Object_Type /= Method_Object
        or else Tree.Items (Scope).Active_Calls = Natural'Last
      then
         return;
      end if;
      Tree.Items (Scope).Active_Calls := Tree.Items (Scope).Active_Calls + 1;
      Allowed := True;
   end Begin_Method;

   -- Keep tombstone identity and topology, but detach data payloads. Dead
   -- interior names must not keep objects live or expose a data accessor once
   -- their storage can be reclaimed. Region/field metadata retains its own
   -- cross-entry invariants and is deliberately not retagged here.
   function Retired_Entry (Item : Entry_Record) return Entry_Record is
     (if Item.Object_Type in Integer_Object | String_Object | Buffer_Object |
          Package_Object | Reference_Object then
        (Item with delta Alive => False, Initializing => False,
          Object_Type => Scope_Object, Object_Ref => 0)
      else (Item with delta Alive => False, Initializing => False));

   procedure End_Method (Tree : in out State; Scope : Natural)
     with Pre => Valid_Context (Tree),
       Post => Valid_Context (Tree) and then Tree.Values = Tree'Old.Values
         and then Last_Incarnation (Tree) = Last_Incarnation (Tree'Old)
         and then Tree.Used <= Tree'Old.Used and then Tree.Code_Used <= Tree'Old.Code_Used
         and then (if Scope > 0 and then Scope <= Tree'Old.Used
           and then Tree'Old.Items (Scope).Active_Calls = 1 then
             (for all I in 1 .. Tree.Used =>
                not Tree.Items (I).Alive or else Tree.Items (I).Owner /= Scope))
   is
      Kept_Code : Aggregate_Method_Count := 0;
   begin
      if Scope = 0 or else Scope > Tree.Used or else Tree.Items (Scope).Active_Calls = 0 then return; end if;
      if Tree.Items (Scope).Active_Calls > 1 then
         Tree.Items (Scope).Active_Calls := Tree.Items (Scope).Active_Calls - 1;
         return;
      end if;
      -- Keep the final owner active until every provisional marker it owns
      -- has been cleared, preserving Valid_Context during the cleanup loop.
      -- ACPICA removes this method's subtree and objects it created elsewhere.
      -- Ascending IDs visit parents first, propagating deletion to descendants.
      for I in 1 .. Tree.Used loop
         pragma Loop_Invariant (Valid_Context (Tree));
         pragma Loop_Invariant
           (for all J in 1 .. I - 1 => not Tree.Items (J).Alive or else Tree.Items (J).Owner /= Scope);
         if Tree.Items (I).Owner /= Root and then
           (Tree.Items (I).Owner = Scope or else Tree.Items (I).Up = Scope or else
            (Tree.Items (I).Up /= Root and then not Tree.Items (Tree.Items (I).Up).Alive))
         then
            Tree.Items (I) := Retired_Entry (Tree.Items (I));
         end if;
      end loop;
      Tree.Items (Scope).Active_Calls := 0;
      -- Keep interior dead slots reserved so active method IDs never move.
      while Tree.Used > 0 and then not Tree.Items (Tree.Used).Alive loop
         pragma Loop_Invariant (Valid_Context (Tree));
         pragma Loop_Variant (Decreases => Tree.Used);
         pragma Loop_Invariant (Tree.Used <= Tree'Loop_Entry.Used);
         pragma Loop_Invariant
           (for all I in 1 .. Tree.Used => not Tree.Items (I).Alive or else Tree.Items (I).Owner /= Scope);
         Tree.Items (Tree.Used) := (others => <>);
         Tree.Used := Tree.Used - 1;
      end loop;
      for I in 1 .. Tree.Used loop
         pragma Loop_Invariant (Kept_Code <= Tree.Code_Used);
         pragma Loop_Invariant
           (for all J in 1 .. I - 1 => Tree.Items (J).Method_Offset <= Kept_Code
             and then Tree.Items (J).Method_Size <= Kept_Code - Tree.Items (J).Method_Offset);
         Kept_Code := Aggregate_Method_Count'Max
           (Kept_Code, Tree.Items (I).Method_Offset + Tree.Items (I).Method_Size);
      end loop;
      if Kept_Code < Tree.Code_Used then
      for I in Kept_Code + 1 .. Tree.Code_Used loop
         pragma Loop_Invariant (Tree.Used = Tree'Loop_Entry.Used);
         pragma Loop_Invariant (Tree.Items = Tree'Loop_Entry.Items);
         pragma Loop_Invariant (Tree.Values = Tree'Loop_Entry.Values);
         pragma Loop_Invariant (Tree.Code_Used = Tree'Loop_Entry.Code_Used);
         Tree.Code (I) := 0;
      end loop;
      end if;
      Tree.Code_Used := Kept_Code;
   end End_Method;

   procedure Name_Target
     (Tree : State; Scope : Natural; Path : AML_Names.Name_Result;
      Base : out Node_ID; Status : out AML_Execute.Execution_Status)
   is
   begin
      Base := Root; Status := AML_Execute.Bad_Name;
      if Scope = 0 or else Scope > Tree.Used or else not Tree.Items (Scope).Alive
        or else Tree.Items (Scope).Object_Type /= Method_Object
        or else Tree.Items (Scope).Active_Calls = 0
      then Status := AML_Execute.Unsupported_Value; return; end if;
      if Path.Kind /= AML_Names.Accepted or else Path.Count = 0
        or else (Path.Rooted and Path.Parents /= 0) then return; end if;
      for I in 1 .. Path.Count loop
         if not AML_Names.Valid (Path.Parts (I)) then return; end if;
      end loop;
      Base := (if Path.Rooted then Root else Node_ID (Scope));
      Status := AML_Execute.Unknown_Name;
      for I in 1 .. Path.Parents loop
         if Base = Root then return; end if;
         Base := Parent (Tree, Base);
      end loop;
      for I in 1 .. Path.Count - 1 loop
         Base := Child (Tree, Base, Path.Parts (I));
         if Base = Root then return; end if;
      end loop;
      if not Declaration_Scope (Kind (Tree, Base)) then return; end if;
      if Child (Tree, Base, Path.Parts (Path.Count)) /= Root then
         Status := AML_Execute.Duplicate_Name; return;
      end if;
      Status := AML_Execute.Returned;
   end Name_Target;

   procedure Define_Runtime_Name
     (Tree : in out State; Owner : AML_Identity.Identity; Scope : Natural; Path : AML_Names.Name_Result;
      Width : AML_Decode.Integer_Width; Data : AML_Decode.Bytes;
      Consumed : out Natural; Status : out AML_Execute.Execution_Status)
     with Pre => Valid_Context (Tree),
       Post => Valid_Context (Tree) and then Consumed <= Data'Length
         and then (if Status /= AML_Execute.Returned then Tree = Tree'Old and Consumed = 0
           else Consumed > 0 and then Tree.Used = Tree'Old.Used + 1
             and then Tree.Items (Tree.Used).Owner = Scope
             and then Tree.Items (Tree.Used).Alive)
   is
      Candidate : State := Tree;
      Environment : Count_Context := (Tree, Root);
      Base, Node : Node_ID;
      Added : Insert_Status;
      ID : AML_Objects.Object_ID;
      Used : Natural;
      Parsed : AML_Decode.Status;
      use type AML_Decode.Status;
      use type AML_Identity.Identity;
      use type AML_Objects.Allocation_Status;
   begin
      Consumed := 0;
      Name_Target (Tree, Scope, Path, Base, Status);
      if Status /= AML_Execute.Returned then return; end if;
      -- Insert into a candidate: quota/parse failure publishes neither node nor
      -- incarnation. Member paths bind in the candidate after root attachment.
      Insert (Candidate, Base, Path.Parts (Path.Count), Node, Added);
      case Added is
         when Duplicate => Status := AML_Execute.Duplicate_Name; return;
         when Full => Status := AML_Execute.Namespace_Limit; return;
         when Invalid_Name => Status := AML_Execute.Bad_Name; return;
         when Inserted => null;
      end case;
      Environment.Scope := Node_ID (Scope);
      Environment.Tree.Journal := Pending.Empty;
      Load_Bound_Data (Candidate.Values, Data, Width, Environment, ID, Used, Parsed);
      if Parsed /= AML_Decode.Accepted then
         Status := (case Parsed is
           when AML_Decode.Truncated => AML_Execute.Truncated,
           when AML_Decode.Limit_Exceeded => AML_Execute.Value_Limit,
           when others => AML_Execute.Unsupported_Value);
         return;
      end if;
      if AML_Objects.Kind (Candidate.Values, ID) = AML_Objects.Reference_Object then
         Status := AML_Execute.Unsupported_Value; return;
      end if;
      Candidate.Items (Node).Object_Type :=
        (case AML_Objects.Kind (Candidate.Values, ID) is
           when AML_Objects.Integer_Object => Integer_Object,
           when AML_Objects.String_Object => String_Object,
           when AML_Objects.Buffer_Object => Buffer_Object,
           when AML_Objects.Package_Object => Package_Object,
           when AML_Objects.Reference_Object => Reference_Object);
      Candidate.Items (Node).Owner := Node_ID (Scope);
      Candidate.Items (Node).Object_Ref := ID;
      -- The reserved node becomes resolvable only inside this valid candidate.
      -- Bind deferred package paths after attaching the completed root object,
      -- allowing a package to refer to itself without publishing partial state.
      for I in 1 .. Pending.Count (Environment.Tree.Journal) loop
         declare
            Member : constant Pending.Member_Result := Pending.Item (Environment.Tree.Journal, I);
            Located : Lookup_Result;
         begin
            if not Member.Found or else Member.Scope > Candidate.Used
              or else AML_Objects.Is_Live (Tree.Values, Member.Package_ID)
              or else not AML_Objects.Is_Live (Candidate.Values, Member.Package_ID)
              or else AML_Objects.Kind (Candidate.Values, Member.Package_ID) /= AML_Objects.Package_Object
              or else Member.Element >= AML_Objects.Length (Candidate.Values, Member.Package_ID)
            then Status := AML_Execute.Unsupported_Value; return; end if;
            Located := Resolve (Candidate, Member.Scope, Member.Path);
            if Located.Status /= Found or else Candidate.Items (Located.Node).Object_Type
              not in Integer_Object | String_Object | Buffer_Object | Package_Object
            then Status := AML_Execute.Unsupported_Value; return; end if;
            if Located.Node = Node then
               -- Only the node currently under construction stays a NAME
               -- reference. Existing data names retain captured-object binding.
               if Owner = AML_Identity.No_Identity then
                  Status := AML_Execute.Unsupported_Value; return;
               end if;
               declare
                  Leaf : AML_Objects.Object_ID;
                  Allocated : AML_Objects.Allocation_Status;
               begin
                  AML_Objects.New_Reference (Candidate.Values,
                    AML_References.Bind_Name_Member (Owner, AML_References.Node_Position (Node),
                      Candidate.Items (Node).Incarnation), Leaf, Allocated);
                  if Allocated /= AML_Objects.Allocated then
                     Status := AML_Execute.Value_Limit; return;
                  end if;
                  AML_Objects.Set_Element (Candidate.Values, Member.Package_ID, Member.Element, Leaf);
               end;
            else
               AML_Objects.Set_Element (Candidate.Values, Member.Package_ID,
                 Member.Element, Candidate.Items (Located.Node).Object_Ref);
            end if;
         end;
      end loop;
      Tree := Candidate; Consumed := Used; Status := AML_Execute.Returned;
   end Define_Runtime_Name;

   procedure Define_Method
     (Tree : in out State; Scope : Natural; Path : AML_Names.Name_Result;
      Flags : AML_Decode.Byte; Width : AML_Decode.Integer_Width;
      Code : AML_Decode.Bytes; Status : out AML_Execute.Declaration_Status)
     with Pre => Valid_Context (Tree),
       Post => Valid_Context (Tree) and then Tree.Values = Tree'Old.Values
         and then (if Status /= AML_Execute.Declared then Tree = Tree'Old
           else Tree.Used = Tree'Old.Used + 1 and then
             Tree.Items (Tree.Used).Owner = Scope and then Tree.Items (Tree.Used).Alive)
   is
      Base, Node : Node_ID;
      Added : Insert_Status;
      Start : Aggregate_Method_Count;
   begin
      Status := AML_Execute.Declaration_Unsupported;
      if Scope = 0 or else Scope > Tree.Used or else not Tree.Items (Scope).Alive
        or else Tree.Items (Scope).Active_Calls = 0
        or else Path.Kind /= AML_Names.Accepted or else Path.Count = 0
        or else (Path.Rooted and Path.Parents /= 0)
      then return; end if;
      for I in 1 .. Path.Count loop
         if not AML_Names.Valid (Path.Parts (I)) then return; end if;
      end loop;
      Base := (if Path.Rooted then Root else Node_ID (Scope));
      for I in 1 .. Path.Parents loop
         pragma Loop_Invariant (Base <= Tree.Used);
         if Base = Root then Status := AML_Execute.Declaration_Missing; return; end if;
         Base := Parent (Tree, Base);
      end loop;
      for I in 1 .. Path.Count - 1 loop
         pragma Loop_Invariant (Base <= Tree.Used);
         Base := Child (Tree, Base, Path.Parts (I));
         if Base = Root then Status := AML_Execute.Declaration_Missing; return; end if;
      end loop;
      if not Declaration_Scope (Kind (Tree, Base)) then
         Status := AML_Execute.Declaration_Missing; return;
      end if;
      if Code'Length > AML_Execute.Max_Method_Bytes or else
        Code'Length > Aggregate_Method_Capacity - Tree.Code_Used then
         Status := AML_Execute.Declaration_Full; return;
      end if;
      Insert (Tree, Base, Path.Parts (Path.Count), Node, Added);
      case Added is
         when Duplicate => Status := AML_Execute.Declaration_Duplicate; return;
         when Full => Status := AML_Execute.Declaration_Full; return;
         when Invalid_Name => return;
         when Inserted => null;
      end case;
      Append_Code (Tree.Code, Tree.Code_Used, Code, Start);
      Tree.Items (Node).Owner := Node_ID (Scope);
      Tree.Items (Node).Object_Type := Method_Object;
      Tree.Items (Node).Method_Offset := Start;
      Tree.Items (Node).Method_Size := Code'Length;
      Tree.Items (Node).Method_Flags := Flags;
      Tree.Items (Node).Method_Width := Width;
      Status := AML_Execute.Declared;
   end Define_Method;
   procedure Execute_Mutable is new AML_Execute.Execute_Typed
     (State, Valid_Context, Read_Binding, Read_Method, Write_Binding,
      Begin_Method, End_Method, Define_Method);

   procedure Invoke_Mutable
     (Tree : in out State; Node : Node_ID; Args : AML_Execute.Arguments;
      Argument_Count : Natural; Budget : Natural; Result : out AML_Execute.Execution_Result)
   is
      use type AML_Decode.Byte;
      Method : constant AML_Execute.Method_Definition := Read_Method (Tree, Node);
   begin
      if Pending_Members (Tree) /= 0 then
         Result := (Status => AML_Execute.Uninitialized, Charged => 0); return;
      end if;
      if not Method.Exists then
         Result := (Status => AML_Execute.Invalid_Method, Charged => 0); return;
      elsif Natural (Method.Flags and 7) /= Argument_Count then
         Result := (Status => AML_Execute.Argument_Mismatch, Charged => 0); return;
      end if;
      Execute_Mutable (Method.Code, Method.Width, AML_Execute.As_Values (Args),
        Argument_Count, Budget, Tree, Natural (Node), Result,
        Current_Sync => AML_Execute.Method_Level (Method.Flags));
   end Invoke_Mutable;

   -- Field declaration currently supports immutable DataTableRegion bindings.
   -- Unsupported access/connection forms fail explicitly before committing.
   procedure Define_Fields
     (Tree : in out State; Scope : Natural; Region : AML_Names.Name_Result;
      Flags : AML_Decode.Byte; Entries : AML_Decode.Bytes;
      Status : out AML_Execute.Execution_Status)
     with Pre => Valid_Context (Tree),
       Post => Valid_Context (Tree) and then Tree.Values = Tree'Old.Values
         and then (if Status /= AML_Execute.Returned then Tree = Tree'Old)
   is
      use type AML_Decode.Byte;
      use type AML_Decode.Status;
      use type AML_Fields.Entry_Kind;
      type Pending_Field is record
         Name : AML_Names.Segment := "____";
         Offset, Bits : Natural := 0;
      end record;
      Pending : array (Positive range 1 .. Capacity) of Pending_Field;
      Pending_Count : Node_ID := 0;
      Initial_Count : constant Node_ID := Tree.Used;
      Located : Lookup_Result;
      Binding : Table_Region;
      Offset, Bit_Offset : Natural := 0;
      Item : AML_Fields.Entry_Result;
   begin
      Status := AML_Execute.Unsupported;
      if Scope = 0 or else Scope > Tree.Used or else not Present (Tree, Node_ID (Scope))
        or else Kind (Tree, Node_ID (Scope)) /= Method_Object
        or else Tree.Items (Scope).Active_Calls = 0
        or else Flags not in 0 .. 1
      then return; end if;
      Located := Resolve (Tree, Node_ID (Scope), Region);
      if Located.Status /= Found or else Located.Node = Root
        or else not Present (Tree, Located.Node)
      then Status := AML_Execute.Unknown_Name; return; end if;
      if Kind (Tree, Located.Node) /= Table_Region_Object then return; end if;
      Binding := Region_Data (Tree, Located.Node);
      while Offset < Entries'Length loop
         pragma Loop_Invariant (Offset <= Entries'Length);
         pragma Loop_Invariant (Pending_Count <= Capacity - Initial_Count);
         pragma Loop_Invariant
           (for all I in 1 .. Pending_Count =>
              AML_Field_Data.Fits (Binding.Extent, Pending (I).Offset, Pending (I).Bits));
         pragma Loop_Variant (Decreases => Entries'Length - Offset);
         Item := AML_Fields.Read_Entry (Entries (Entries'First + Offset .. Entries'Last));
         if Item.Status /= AML_Decode.Accepted then
            Status := AML_Execute.Bad_Package; return;
         end if;
         case Item.Kind is
            when AML_Fields.Named_Field | AML_Fields.Reserved_Field =>
               if Natural (Item.Bits) > Natural'Last - Bit_Offset then
                  Status := AML_Execute.Bad_Package; return;
               end if;
               if Item.Kind = AML_Fields.Named_Field then
                  if not AML_Field_Data.Fits (Binding.Extent, Bit_Offset, Natural (Item.Bits)) then
                     Status := AML_Execute.Bad_Package; return;
                  end if;
                  if Child (Tree, Node_ID (Scope), Item.Name) /= Root then
                     Status := AML_Execute.Duplicate_Name; return;
                  end if;
                  for I in 1 .. Pending_Count loop
                     if Pending (I).Name = Item.Name then
                        Status := AML_Execute.Duplicate_Name; return;
                     end if;
                  end loop;
                  if Pending_Count = Capacity - Initial_Count then
                     Status := AML_Execute.Namespace_Limit; return;
                  end if;
                  Pending_Count := Pending_Count + 1;
                  Pending (Pending_Count) :=
                    (Name => Item.Name, Offset => Bit_Offset, Bits => Natural (Item.Bits));
               end if;
               Bit_Offset := Bit_Offset + Natural (Item.Bits);
            when AML_Fields.Access_Field =>
               if Item.Access_Type not in 0 .. 1 or else Item.Attribute /= 0 then return; end if;
            when others => return;
         end case;
         Offset := Offset + Item.Consumed;
      end loop;
      if AML_References.Node_Incarnation (Pending_Count) > Max_Node_Incarnation - Tree.Last_Stamp then
         Status := AML_Execute.Namespace_Limit; return;
      end if;
      -- All parsing and failure paths precede the commit. Only the new field
      -- descriptors are staged; value storage and method bytecode are not copied.
      for I in 1 .. Pending_Count loop
         pragma Loop_Invariant (Valid_Context (Tree));
         pragma Loop_Invariant (Tree.Used = Initial_Count + I - 1);
         Tree.Used := Tree.Used + 1;
         Tree.Last_Stamp := Tree.Last_Stamp + 1;
         Tree.Items (Tree.Used) :=
           (Incarnation => Tree.Last_Stamp, Up => Node_ID (Scope), Owner => Node_ID (Scope), Part => Pending (I).Name,
            Object_Type => Table_Field_Object,
            Table_Binding => (Region => Binding, Offset => Pending (I).Offset,
                              Bits => Pending (I).Bits), others => <>);
      end loop;
      Status := AML_Execute.Returned;
   end Define_Fields;

   procedure Materialize_Literal
     (Tree : in out State; Scope : Natural; Kind : AML_Execute.Literal_Kind; Width : AML_Decode.Integer_Width; Data : AML_Decode.Bytes;
      Binding : out AML_Execute.Binding_Result)
     with Pre => Valid_Context (Tree) and then not Binding'Constrained,
          Post => Valid_Context (Tree)
            and then Tree = (Tree'Old with delta Values => Tree.Values)
            and then Binding.Status in AML_Execute.Failed_Binding | AML_Execute.Non_Integer_Binding
            and then (if Binding.Status in AML_Execute.Failed_Binding then Tree = Tree'Old
              else AML_Objects.Is_Live (Tree.Values, Binding.Object.ID)
                and then Binding.Object.Size = AML_Objects.Length (Tree.Values, Binding.Object.ID)
                and then Binding.Object.Type_Code =
                  (case Kind is
                    when AML_Execute.String_Literal => 2,
                    when AML_Execute.Buffer_Literal => 3,
                    when AML_Execute.Package_Literal => 4))
            and then AML_Objects.Live_Count (Tree.Values) >= AML_Objects.Live_Count (Tree'Old.Values)
            and then (for all J in 1 .. AML_Objects.Slot_Bound (Tree'Old.Values) => (if AML_Objects.Is_Live (Tree'Old.Values, J) then
              AML_Objects.Kind (Tree.Values, J) = AML_Objects.Kind (Tree'Old.Values, J)
              and then AML_Objects.Length (Tree.Values, J) = AML_Objects.Length (Tree'Old.Values, J)))
   is
      use type AML_Execute.Literal_Kind;
      use type AML_Objects.Allocation_Status;
      ID : AML_Objects.Object_ID;
      Status : AML_Objects.Allocation_Status;
      C32, C64 : AML_Coercions.Result;
   begin
      if Kind = AML_Execute.Package_Literal then
         declare
            Candidate : AML_Objects.State := Tree.Values;
            Loaded : AML_Decode.Status;
            Consumed : Natural;
            Environment : Count_Context := (Tree, Node_ID'First);
            use type AML_Decode.Status;
         begin
            Binding := (Status => AML_Execute.Failed_Binding, Failure => AML_Execute.Unsupported_Value);
            if Data'Length = 0 or else Data (Data'First) not in 16#12# | 16#13# then return; end if;
            if Scope > Tree.Used then return; end if;
            Environment.Scope := Node_ID (Scope);
            Load_Runtime_Data (Candidate, Data, Width, Environment, ID, Consumed, Loaded);
            if Loaded /= AML_Decode.Accepted then
               if Loaded = AML_Decode.Limit_Exceeded then
                  Binding := (Status => AML_Execute.Failed_Binding, Failure => AML_Execute.Value_Limit);
               end if;
               return;
            end if;
            if Consumed /= Data'Length or else AML_Objects.Kind (Candidate, ID) /= AML_Objects.Package_Object then return; end if;
            Tree.Values := Candidate;
            Binding := (Status => AML_Execute.Non_Integer_Binding,
              Object => (Source => AML_References.No_Object_Handle, ID => ID, Type_Code => 4,
                Size => AML_Objects.Length (Tree.Values, ID), Conversion_32 => C32, Conversion_64 => C64));
            return;
         end;
      end if;
      AML_Objects.New_Bytes (Tree.Values,
        (if Kind = AML_Execute.String_Literal then AML_Objects.String_Object else AML_Objects.Buffer_Object),
        Data, ID, Status);
      if Status /= AML_Objects.Allocated then
         Binding := (Status => AML_Execute.Failed_Binding, Failure => AML_Execute.Value_Limit);
         return;
      end if;
      if Kind = AML_Execute.String_Literal then
         C32 := AML_Coercions.From_String (Data, AML_Decode.Bits_32);
         C64 := AML_Coercions.From_String (Data, AML_Decode.Bits_64);
      else
         C32 := AML_Coercions.From_Buffer (Data, AML_Decode.Bits_32);
         C64 := AML_Coercions.From_Buffer (Data, AML_Decode.Bits_64);
      end if;
      Binding := (Status => AML_Execute.Non_Integer_Binding,
        Object => (Source => AML_References.No_Object_Handle, ID => ID, Type_Code => (if Kind = AML_Execute.String_Literal then 2 else 3),
          Size => Data'Length, Conversion_32 => C32, Conversion_64 => C64));
   end Materialize_Literal;

   procedure Reserve_Region
     (Tree : in out State; Scope : Natural; Path : AML_Names.Name_Result;
      Token : out Natural; Status : out AML_Execute.Execution_Status)
     with Pre => Valid_Context (Tree), Post => Valid_Context (Tree)
   is
      Base, Node : Node_ID;
      Added : Insert_Status;
   begin
      Token := 0; Status := AML_Execute.Bad_Name;
      if Scope = 0 or else Scope > Tree.Used or else not Tree.Items (Scope).Alive
        or else Tree.Items (Scope).Active_Calls = 0
        or else Path.Kind /= AML_Names.Accepted or else Path.Count = 0
        or else (Path.Rooted and Path.Parents /= 0)
      then return; end if;
      for I in 1 .. Path.Count loop
         if not AML_Names.Valid (Path.Parts (I)) then return; end if;
      end loop;
      Base := (if Path.Rooted then Root else Node_ID (Scope));
      for I in 1 .. Path.Parents loop
         pragma Loop_Invariant (Base <= Tree.Used);
         if Base = Root then Status := AML_Execute.Unknown_Name; return; end if;
         Base := Parent (Tree, Base);
      end loop;
      for I in 1 .. Path.Count - 1 loop
         pragma Loop_Invariant (Base <= Tree.Used);
         Base := Child (Tree, Base, Path.Parts (I));
         if Base = Root then Status := AML_Execute.Unknown_Name; return; end if;
      end loop;
      if not Declaration_Scope (Kind (Tree, Base)) then
         Status := AML_Execute.Unknown_Name; return;
      end if;
      Insert (Tree, Base, Path.Parts (Path.Count), Node, Added);
      case Added is
         when Duplicate => Status := AML_Execute.Duplicate_Name; return;
         when Full => Status := AML_Execute.Namespace_Limit; return;
         when Invalid_Name => return;
         when Inserted => null;
      end case;
      Tree.Items (Node).Owner := Node_ID (Scope);
      Tree.Items (Node).Object_Type := Uninitialized_Region_Object;
      Token := Natural (Node);
      Status := AML_Execute.Returned;
   end Reserve_Region;

   procedure Complete_Region
     (Tree : in out State; Input : aliased AML_Table_Backing.State; Token : Natural;
      Width : AML_Decode.Integer_Width; Signature, OEM, Table_ID : AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
     with Pre => Valid_Context (Tree), Post => Valid_Context (Tree)
   is
      use type AML_Decode.Byte;
      type Selector is record
         Valid : Boolean := False;
         Length : Natural := 0;
         Prefix : String (1 .. 8) := [others => Character'Val (0)];
      end record;
      function Describe (Data : AML_Decode.Bytes) return Selector is
         Result : Selector := (Valid => True, others => <>);
      begin
         for I in Data'Range loop
            exit when Data (I) = 0;
            Result.Length := Result.Length + 1;
            if Result.Length <= 8 then
               Result.Prefix (Result.Length) := Character'Val (Data (I));
            end if;
            pragma Loop_Invariant (Result.Length <= I - Data'First + 1);
         end loop;
         return Result;
      end Describe;
      function Convert (Item : AML_Execute.Datum) return Selector is
      begin
         if Item.Value_Kind = AML_Execute.Reference_Datum then return (others => <>);
         elsif Item.Value_Kind = AML_Execute.Integer_Datum then
            return Describe (AML_Coercions.Strings.From_Integer (Item.Number, Width));
         elsif not AML_Objects.Is_Live (Tree.Values, Item.Object.ID) then
            return (others => <>);
         end if;
         declare
            ID : constant AML_Objects.Object_ID := Item.Object.ID;
         begin
            if AML_Objects.Kind (Tree.Values, ID) = AML_Objects.String_Object then
               return Describe (AML_Objects.Byte_Data (Tree.Values, ID));
            elsif AML_Objects.Kind (Tree.Values, ID) = AML_Objects.Buffer_Object then
               declare
                  Data : constant AML_Decode.Bytes := AML_Objects.Byte_Data (Tree.Values, ID);
               begin
                  if Data'Length > Natural'Last / 5 then return (others => <>); end if;
                  return Describe (AML_Coercions.Strings.From_Buffer (Data));
               end;
            elsif AML_Objects.Kind (Tree.Values, ID) = AML_Objects.Integer_Object then
               return Describe (AML_Coercions.Strings.From_Integer (AML_Objects.Integer_Data (Tree.Values, ID), Width));
            end if;
         end;
         return (others => <>);
      end Convert;
      Sig, Manufacturer, Model : Selector;
      Query : Firmware_Tables.Identifiers.Selection;
      Found_Table : Natural;
   begin
      Status := AML_Execute.Unsupported_Value;
      if Token = 0 or else Token > Tree.Used or else not Tree.Items (Token).Alive
        or else Tree.Items (Token).Object_Type /= Uninitialized_Region_Object
      then return; end if;
      Sig := Convert (Signature); Manufacturer := Convert (OEM); Model := Convert (Table_ID);
      if not Sig.Valid or else not Manufacturer.Valid or else not Model.Valid then return; end if;
      if Sig.Length < 4 then Status := AML_Execute.Bad_Name; return; end if;
      Query.Name := Sig.Prefix (1 .. 4);
      -- Table signatures are not AML object names: digits may lead, and
      -- the fourth character may be '!' (for example ASF!).
      for I in Query.Name'Range loop
         if Query.Name (I) not in 'A' .. 'Z' | '0' .. '9' | '_'
           and then not (I = 4 and then Query.Name (I) = '!')
         then Status := AML_Execute.Bad_Name; return; end if;
      end loop;
      if Manufacturer.Length > 6 or else Model.Length > 8 then return; end if;
      Query.Match_OEM := Manufacturer.Length /= 0;
      Query.OEM := Manufacturer.Prefix (1 .. 6);
      Query.Match_OEM_Table := Model.Length /= 0;
      Query.OEM_Table := Model.Prefix;
      Found_Table := AML_Table_Backing.Find_Table (Input, Query);
      if Found_Table = 0 or else not AML_Table_Backing.Valid_Span (Input, Found_Table) then
         Status := AML_Execute.Unknown_Name; return;
      end if;
      Tree.Items (Token).Table_Binding :=
        (Region => (Table => Found_Table, Extent => Input.Tables (Found_Table).Extent), others => <>);
      Tree.Items (Token).Object_Type := Table_Region_Object;
      Status := AML_Execute.Returned;
   end Complete_Region;

      procedure Lookup_With_Tables
        (Tree : in out State; Input : aliased AML_Table_Backing.State; Scope : Natural;
         Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width;
         Purpose : AML_Execute.Binding_Purpose; Binding : out AML_Execute.Binding_Result)
        with Pre => Valid_Context (Tree) and then not Binding'Constrained,
             Post => Valid_Context (Tree)
      is
         use type AML_Decode.Status;
         use type AML_Decode.Integer_Width;
         use type AML_Coercions.Conversion_Status;
         use type AML_Objects.Allocation_Status;
         Located : Lookup_Result;
         Data : AML_Field_Data.Read_Result;
         ID : AML_Objects.Object_ID;
         Status : AML_Objects.Allocation_Status;
         Converted : AML_Coercions.Result;
      begin
         if Purpose = AML_Execute.Namespace_Identity then
            Binding := (Status => AML_Execute.Failed_Binding, Failure => AML_Execute.Unsupported_Value); return;
         end if;
         Binding := Read_Binding (Tree, Scope, Path, Width);
         if Scope > Tree.Used then return; end if;
         Located := Resolve (Tree, Node_ID (Scope), Path);
         if Located.Status = Found and then Located.Node /= Root
           and then Kind (Tree, Located.Node) in Operation_Region_Object | Region_Field_Object
         then
            if Purpose = AML_Execute.Inspect_Binding then
               Binding := (Status => AML_Execute.Non_Integer_Binding,
                 Object => (Type_Code => (if Kind (Tree, Located.Node) = Operation_Region_Object then 10 else 5), others => <>));
            end if;
            return;
         end if;
         if Purpose = AML_Execute.Inspect_Binding then return; end if;
         if Located.Status /= Found or else Located.Node = Root
           or else not Present (Tree, Located.Node)
           or else Kind (Tree, Located.Node) /= Table_Field_Object
         then return; end if;
         declare
            Field : constant Table_Field := Field_Data (Tree, Located.Node);
         begin
            Binding := (Status => AML_Execute.Failed_Binding, Failure => AML_Execute.Unsupported_Value);
            Data := AML_Table_Backing.Read_Field
              (Input, Field.Region.Table, Field.Region.Extent, Field.Offset, Field.Bits);
            if Data.Status /= AML_Decode.Accepted or else
              Data.Length /= Field.Bits / 8 + (if Field.Bits mod 8 = 0 then 0 else 1)
            then return; end if;
            if Field.Bits <= (if Width = AML_Decode.Bits_32 then 32 else 64) then
               if Data.Length = 0 then
                  Binding := (Status => AML_Execute.Integer_Binding, Value => 0, Origin => AML_Decode.Ordinary_Integer);
               else
                  Converted := AML_Coercions.From_Buffer (Data.Content (1 .. Data.Length), Width);
                  if Converted.Status = AML_Coercions.Converted then
                     Binding := (Status => AML_Execute.Integer_Binding, Value => Converted.Value, Origin => AML_Decode.Ordinary_Integer);
                  end if;
               end if;
            else
               AML_Objects.New_Bytes (Tree.Values, AML_Objects.Buffer_Object,
                                     Data.Content (1 .. Data.Length), ID, Status);
               if Status /= AML_Objects.Allocated then
                  Binding := (Status => AML_Execute.Failed_Binding, Failure => AML_Execute.Value_Limit);
                  return;
               end if;
               Binding := (Status => AML_Execute.Non_Integer_Binding,
                 Object => (Source => AML_References.No_Object_Handle, ID => ID, Type_Code => 3, Size => Data.Length,
                   Conversion_32 => AML_Coercions.From_Buffer (Data.Content (1 .. Data.Length), AML_Decode.Bits_32),
                   Conversion_64 => AML_Coercions.From_Buffer (Data.Content (1 .. Data.Length), AML_Decode.Bits_64)));
            end if;
         end;
      end Lookup_With_Tables;
      procedure Read_Context_Timer
        (Environment : in out State; Value : out AML_Decode.Integer_Value;
         Available : out Boolean)
        with Pre => Valid_Context (Environment), Post => Valid_Context (Environment)
      is
         use type AML_Clock.Sample_Status;
         Raw : AML_Decode.Integer_Value;
         Ready : Boolean;
         Status : AML_Clock.Sample_Status;
      begin
         Read_Microseconds (Raw, Ready);
         AML_Clock.Observe (Environment.Timer_State, Raw, Ready, Value, Status);
         Available := Status = AML_Clock.Accepted;
      end Read_Context_Timer;
   procedure Invoke_With_Tables
     (Tree : in out State; Input : aliased AML_Table_Backing.State; Node : Node_ID;
      Args : AML_Execute.Arguments; Argument_Count : Natural; Budget : Natural;
      Result : out AML_Execute.Execution_Result)
   is
      use type AML_Decode.Byte;
      procedure Reject_Reference
        (Environment : in out State; Ref : AML_References.Reference;
         Value : out AML_Execute.Datum; Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Ref);
      begin Value := (Value_Kind => AML_Execute.Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer); Status := AML_Execute.Unsupported_Value; end Reject_Reference;
      procedure Reject_Index
        (Environment : in out State; Source : AML_References.Object_Handle;
         Index : AML_Decode.Integer_Value; Ref : out AML_References.Reference;
         Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Source, Index);
      begin Ref := AML_References.No_Reference; Status := AML_Execute.Unsupported_Value; end Reject_Index;
      procedure Reject_Store
        (Environment : in out State; Ref : AML_References.Reference;
         Width : AML_Decode.Integer_Width; Item : AML_Execute.Datum; Status : out AML_Execute.Execution_Status; Mode : AML_Execute.Reference_Store_Mode)
        with Pre => Valid_Context (Environment), Post => Valid_Context (Environment)
      is
         pragma Unreferenced (Ref, Width, Item, Mode);
      begin Status := AML_Execute.Unsupported_Value; end Reject_Store;
   procedure Clone_Value
        (Environment : in out State; Width : AML_Decode.Integer_Width;
         Item : AML_Execute.Datum; Copy : out AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status)
        with Pre => Valid_Context (Environment) and then not Copy'Constrained,
             Post => Valid_Context (Environment)
      is
      begin
         Copy := (Value_Kind => AML_Execute.Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
         Status := AML_Execute.Unsupported_Value;
         if Item.Value_Kind = AML_Execute.Integer_Datum then
            Copy.Number := AML_Integers.Normalize (Item.Number, Width);
            Copy.Origin := Item.Origin;
            Status := AML_Execute.Returned;
         end if;
      end Clone_Value;

      procedure Reject_Compare
        (Environment : in out State; Op : AML_Decode.Byte; Left, Right : AML_Execute.Datum;
         Width : AML_Decode.Integer_Width; Value : out AML_Decode.Integer_Value;
         Status : out AML_Execute.Execution_Status)
        with Pre => Valid_Context (Environment), Post => Valid_Context (Environment)
      is
         pragma Unreferenced (Op, Left, Right, Width);
      begin Value := 0; Status := AML_Execute.Unsupported_Value; end Reject_Compare;
      procedure Keep_Value
        (E : State; Item : in out AML_Execute.Datum; Status : out AML_Execute.Execution_Status)
      is
         pragma Unreferenced (E, Item);
      begin Status := AML_Execute.Returned; end Keep_Value;
      procedure No_Invocation
        (Environment : in out State; Domain : out AML_Frame_Handles.Invocation_Domain;
         Status : out AML_Execute.Invocation_Status)
      is
         pragma Unreferenced (Environment);
      begin Domain := AML_Frame_Handles.No_Domain; Status := AML_Execute.Unsupported_Context; end No_Invocation;
      procedure Reject_Name
        (E : in out State; Scope : Natural; Path : AML_Names.Name_Result;
         Width : AML_Decode.Integer_Width; Data : AML_Decode.Bytes;
         Consumed : out Natural; Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (E, Scope, Path, Width, Data);
      begin Consumed := 0; Status := AML_Execute.Unsupported_Value; end Reject_Name;
      procedure Reject_Copy_Attachment
        (Environment : in out State; Destination : AML_Execute.Copy_Destination;
         Width : AML_Decode.Integer_Width; Item : AML_Execute.Datum;
         Copy : out AML_Execute.Datum; Status : out AML_Execute.Execution_Status)
      is
         pragma Unreferenced (Environment, Destination, Width, Item);
      begin Copy := (Value_Kind => AML_Execute.Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
         Status := AML_Execute.Unsupported_Value; end Reject_Copy_Attachment;
      procedure Describe_Identity
        (E : State; Ref : AML_References.Reference;
         Result : out AML_Execute.Reference_Metadata)
        with Pre => not Result'Constrained
      is
         pragma Unreferenced (E, Ref);
      begin Result := (Kind => AML_Execute.Continue_Reference); end Describe_Identity;
      procedure Reserve_Dynamic_Name
        (E : in out State; Scope : Natural; Path : AML_Names.Name_Result;
         Token : out Boolean; Status : out AML_Execute.Execution_Status)
      is
         pragma Unreferenced (E, Scope, Path);
      begin Token := False; Status := AML_Execute.Unsupported_Value; end Reserve_Dynamic_Name;
      procedure Complete_Dynamic_Buffer
        (E : in out State; Token : Boolean; Width : AML_Decode.Integer_Width;
         Initializer : AML_Decode.Bytes; Count : AML_Data.Count_Result;
         Status : out AML_Execute.Execution_Status)
      is
         pragma Unreferenced (E, Token, Width, Initializer, Count);
      begin Status := AML_Execute.Unsupported_Value; end Complete_Dynamic_Buffer;
      procedure Abort_Dynamic_Name
        (E : in out State; Token : Boolean; Status : out AML_Execute.Execution_Status)
      is
         pragma Unreferenced (E, Token);
      begin Status := AML_Execute.Unsupported_Value; end Abort_Dynamic_Name;


      -- AML Debug output is explicitly disabled in this owned/unowned adapter.
      -- This is observation only; source evaluation remains in the executor.
      procedure Reject_Explicit_Integer
        (Environment : State; Width : AML_Decode.Integer_Width;
         Item : in out AML_Execute.Datum; Status : out AML_Execute.Execution_Status)
      is
         pragma Unreferenced (Environment, Width);
      begin
         Status := (if Item.Value_Kind = AML_Execute.Integer_Datum then
           AML_Execute.Returned else AML_Execute.Unsupported_Value);
      end Reject_Explicit_Integer;
      procedure Disabled_Debug
        (Environment : State; Scope, Position : Natural;
         Width : AML_Decode.Integer_Width; Value : AML_Execute.Datum)
      is
         pragma Unreferenced (Environment, Scope, Position, Width, Value);
      begin null; end Disabled_Debug;
      procedure Reject_Concatenation
        (Environment : in out State; Width : AML_Decode.Integer_Width;
         Left, Right : AML_Execute.Concatenation_Operand;
         Destination : AML_Execute.Concatenation_Destination;
         Result_Value, Cell_Value : out AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status)
      is
         pragma Unreferenced (Environment, Width, Left, Right, Destination);
      begin
         Result_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Cell_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Status := AML_Execute.Unsupported_Value;
      end Reject_Concatenation;
      procedure Reject_To_Buffer
        (Environment : in out State; Width : AML_Decode.Integer_Width;
         Item : AML_Execute.Datum;
         Destination : AML_Execute.Concatenation_Destination;
         Result_Value, Cell_Value : out AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status)
      is
         pragma Unreferenced (Environment, Width, Item, Destination);
      begin
         Result_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Cell_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Status := AML_Execute.Unsupported_Value;
      end Reject_To_Buffer;
      procedure Reject_Mid
        (Environment : in out State; Width : AML_Decode.Integer_Width;
         Item : AML_Execute.Datum; Start, Count : AML_Decode.Integer_Value;
         Destination : AML_Execute.Concatenation_Destination;
         Result_Value, Cell_Value : out AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status)
      is
         pragma Unreferenced (Environment, Width, Item, Start, Count, Destination);
      begin
         Result_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Cell_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Status := AML_Execute.Unsupported_Value;
      end Reject_Mid;
      procedure Reject_To_String
        (Environment : in out State; Width : AML_Decode.Integer_Width;
         Item : AML_Execute.Datum; Length : AML_Decode.Integer_Value;
         Destination : AML_Execute.Concatenation_Destination;
         Result_Value, Cell_Value : out AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status)
      is
         pragma Unreferenced (Environment, Width, Item, Length, Destination);
      begin
         Result_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Cell_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Status := AML_Execute.Unsupported_Value;
      end Reject_To_String;
      procedure Reject_Format_String
        (Environment : in out State; Width : AML_Decode.Integer_Width;
         Mode : AML_Execute.Explicit_String_Mode; Item : AML_Execute.Datum;
         Destination : AML_Execute.Concatenation_Destination;
         Result_Value, Cell_Value : out AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status)
      is
         pragma Unreferenced (Environment, Width, Mode, Item, Destination);
      begin
         Result_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Cell_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Status := AML_Execute.Unsupported_Value;
      end Reject_Format_String;
      procedure Reject_Match
        (Environment : State; Width : AML_Decode.Integer_Width;
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

      procedure Reject_Resources
        (Environment : in out State; Width : AML_Decode.Integer_Width;
         Left, Right : AML_Execute.Datum;
         Destination : AML_Execute.Concatenation_Destination;
         Result_Value, Cell_Value : out AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Width, Left, Right, Destination);
      begin
         Result_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Cell_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Status := AML_Execute.Unsupported_Value;
      end Reject_Resources;

      procedure Delay_Adapter_1 (Environment : in out State; Item : AML_Delays.Request; Result : out AML_Delays.Outcome) is
         pragma Unreferenced (Environment);
      begin
         Perform_Delay (Item, Result);
      end Delay_Adapter_1;
      procedure Execute is new AML_Execute.Execute_With_Input
        (State, AML_Table_Backing.State, Valid_Context, Lookup_With_Tables, Read_Method, Write_Binding,
         Begin_Method, End_Method, Define_Method, Define_Fields, Materialize_Literal, Reserve_Region, Complete_Region, Read_Context_Timer, Reject_Reference, Reject_Index, Reject_Store, Clone_Value, Reject_Compare, Keep_Value, No_Invocation, Reject_Name, Reject_Copy_Attachment, Describe_Identity, Boolean, False, Reserve_Dynamic_Name, Complete_Dynamic_Buffer, Abort_Dynamic_Name, Disabled_Debug, Reject_Explicit_Integer, Reject_Concatenation, Reject_To_Buffer, Reject_Mid, Reject_To_String, Reject_Format_String, Reject_Match, Reject_Resources, Wait_For_Delay => Delay_Adapter_1);
      Method : constant AML_Execute.Method_Definition := Read_Method (Tree, Node);
   begin
      if Pending_Members (Tree) /= 0 then
         Result := (Status => AML_Execute.Uninitialized, Charged => 0); return;
      end if;
      if not Method.Exists then
         Result := (Status => AML_Execute.Invalid_Method, Charged => 0); return;
      elsif Natural (Method.Flags and 7) /= Argument_Count then
         Result := (Status => AML_Execute.Argument_Mismatch, Charged => 0); return;
      end if;
      Execute (Method.Code, Method.Width, AML_Execute.As_Values (Args),
        Argument_Count, Budget, Input, Tree, Natural (Node), Result,
        Current_Sync => AML_Execute.Method_Level (Method.Flags));
   end Invoke_With_Tables;



   function Invoke
     (Tree : State; Node : Node_ID; Args : AML_Execute.Arguments;
      Argument_Count : Natural; Budget : Natural) return AML_Execute.Execution_Result
   is
      use type AML_Decode.Byte;
   begin
      if Pending_Members (Tree) /= 0 then
         return (Status => AML_Execute.Uninitialized, Charged => 0);
      end if;
      if Kind (Tree, Node) /= Method_Object then
         return (Status => AML_Execute.Invalid_Method, Charged => 0);
      elsif Natural (Tree.Items (Node).Method_Flags and 7) /= Argument_Count then
         return (Status => AML_Execute.Argument_Mismatch, Charged => 0);
      end if;
      return Execute_Bound
        (Method_Data (Tree, Node),
         Tree.Items (Node).Method_Width, Args, Argument_Count, Budget, Tree, Natural (Node),
         Current_Sync => AML_Execute.Method_Level (Tree.Items (Node).Method_Flags));
   end Invoke;

   function Pending_Members (Tree : State) return Natural is
     (Pending.Count (Tree.Journal));
   function Initialization_Frame (Tree, Prior : State) return Boolean is
      use type AML_Object_Identifiers.Slot_Incarnation;
   begin
      if AML_Objects.Slot_Bound (Tree.Values) /= AML_Objects.Slot_Bound (Prior.Values)
        or else AML_Objects.Incarnation_Limit (Tree.Values) /= AML_Objects.Incarnation_Limit (Prior.Values)
        or else Tree /= (Prior with delta Values => Tree.Values, Journal => Tree.Journal)
        or else AML_Objects.Usage_Of (Tree.Values) /= AML_Objects.Usage_Of (Prior.Values)
      then return False; end if;
      for ID in 1 .. AML_Objects.Max_Objects loop
         if AML_Objects.Is_Live (Tree.Values, ID) /= AML_Objects.Is_Live (Prior.Values, ID)
           or else AML_Objects.Last_Incarnation (Tree.Values, ID) /= AML_Objects.Last_Incarnation (Prior.Values, ID)
         then return False; end if;
         if AML_Objects.Is_Live (Prior.Values, ID) then
         if AML_Objects.Kind (Tree.Values, ID) /= AML_Objects.Kind (Prior.Values, ID)
           or else AML_Objects.Length (Tree.Values, ID) /= AML_Objects.Length (Prior.Values, ID)
         then return False; end if;
         case AML_Objects.Kind (Prior.Values, ID) is
            when AML_Objects.Reference_Object =>
               if AML_Objects.Reference_Data (Tree.Values, ID) /= AML_Objects.Reference_Data (Prior.Values, ID)
               then return False; end if;
            when AML_Objects.Integer_Object =>
               if AML_Objects.Integer_Data (Tree.Values, ID) /= AML_Objects.Integer_Data (Prior.Values, ID)
                 or else AML_Objects.Origin_Of (Tree.Values, ID) /= AML_Objects.Origin_Of (Prior.Values, ID) then return False; end if;
            when AML_Objects.String_Object | AML_Objects.Buffer_Object =>
               if AML_Objects.Byte_Data (Tree.Values, ID) /= AML_Objects.Byte_Data (Prior.Values, ID) then return False; end if;
            when AML_Objects.Package_Object =>
               for Offset in 0 .. AML_Objects.Length (Prior.Values, ID) - 1 loop
                  declare Pending_Element : Boolean := False; begin
                     for I in 1 .. Pending.Count (Prior.Journal) loop
                        declare M : constant Pending.Member_Result := Pending.Item (Prior.Journal, I); begin
                           if M.Package_ID = ID and then M.Element = Offset then Pending_Element := True; end if;
                        end;
                     end loop;
                     if not Pending_Element and then AML_Objects.Element (Tree.Values, ID, Offset) /= AML_Objects.Element (Prior.Values, ID, Offset)
                     then return False; end if;
                  end;
               end loop;
         end case;
         end if;
      end loop;
      return True;
   end Initialization_Frame;
   procedure Initialize_Members (Tree : in out State; Report : out Initialization_Report) is
      Candidate : State := Tree;
      Located : Lookup_Result;
   begin
      Report := (others => 0);
      for I in 1 .. Pending.Count (Candidate.Journal) loop
         declare
            Member : constant Pending.Member_Result := Pending.Item (Candidate.Journal, I);
         begin
            -- All coordinates originated in the transactional loader. Check
            -- again before publication; no journal ID is authority.
            if Member.Scope > Candidate.Used
              or else not AML_Objects.Is_Live (Candidate.Values, Member.Package_ID)
              or else AML_Objects.Kind (Candidate.Values, Member.Package_ID) /= AML_Objects.Package_Object
              or else Member.Element >= AML_Objects.Length (Candidate.Values, Member.Package_ID)
            then
               Report.Unsupported := Report.Unsupported + 1;
            else
               Located := Resolve (Candidate, Member.Scope, Member.Path);
               if Located.Status /= Found then
                  Report.Missing := Report.Missing + 1;
               elsif Candidate.Items (Located.Node).Object_Type not in
                 Integer_Object | String_Object | Buffer_Object | Package_Object | Reference_Object
               then
                  Report.Unsupported := Report.Unsupported + 1;
               else
                  AML_Objects.Set_Element (Candidate.Values, Member.Package_ID,
                    Member.Element, Candidate.Items (Located.Node).Object_Ref);
                  Report.Bound := Report.Bound + 1;
               end if;
            end if;
         end;
      end loop;
      -- Failed entries remain uninitialized and are not retried after later
      -- namespace changes. Repeated initialization is an idempotent no-op.
      Pending.Clear (Candidate.Journal);
      Tree := Candidate;
   end Initialize_Members;

   procedure Load_Names
     (Tree : in out State; Data : AML_Decode.Bytes;
      Width : AML_Decode.Integer_Width; Result : out Load_Status)
   is
      use type AML_Decode.Byte;
      use type AML_Decode.Status;
      use type AML_Objects.Allocation_Status;
      Object_Ref : AML_Objects.Object_ID;
      Allocation : AML_Objects.Allocation_Status;
      Candidate : State := Tree;
      Offset : Natural := 0;
      Path : AML_Names.Name_Result;
      Value : AML_Decode.Integer_Result;
      Text : AML_Decode.String_Result;
      Buffer_Item : AML_Decode.Buffer_Result;
      Scope, Node : Node_ID;
      Added : Insert_Status;
      type Frame is record
         Limit : Natural;
         Scope : Node_ID;
      end record;
      Frames : array (Natural range 0 .. 64) of Frame :=
        [others => (Limit => Data'Length, Scope => Root)];
      Depth : Natural range 0 .. 64 := 0;
      Op : AML_Decode.Byte;
      Extended_Opcode : constant AML_Decode.Byte := 16#5B#;
      Mutex_Opcode : constant AML_Decode.Byte := 16#01#;
      Event_Opcode : constant AML_Decode.Byte := 16#02#;
      Device_Opcode : constant AML_Decode.Byte := 16#82#;
      Processor_Opcode : constant AML_Decode.Byte := 16#83#;
      Power_Opcode : constant AML_Decode.Byte := 16#84#;
      Thermal_Opcode : constant AML_Decode.Byte := 16#85#;
      Processor_Tail_Bytes : constant Positive := 6;
      Power_Tail_Bytes : constant Positive := 3;
      Address_Bytes : constant Positive := 4;
      Octet_Radix : constant Positive := 256;
      Is_Scope, Is_Device, Is_Method, Is_Event, Is_Mutex : Boolean;
      Is_Processor, Is_Power, Is_Thermal, Is_Container, Is_Operation, Is_Field : Boolean;
      Region_Opcode : constant AML_Decode.Byte := 16#80#;
      Field_Opcode : constant AML_Decode.Byte := 16#81#;
      Limit : Natural;
      Package_Info : AML_Decode.Package_Result;
      Located : Lookup_Result;
      Code_Start : Aggregate_Method_Count;
      procedure Static_Fields (Cursor : in out Natural; Finish : Natural; Parent : Node_ID; Status : out Load_Status) is
         use type AML_Fields.Entry_Kind;
         Region_Path : AML_Names.Name_Result;
         Region_Node : Lookup_Result;
         Info : Region_Field_Attributes;
         Item : AML_Fields.Entry_Result;
         Field_Node : Node_ID;
         Inserted_Status : Insert_Status;
      begin
         Status := Bad_Package;
         if Cursor = Finish then return; end if;
         Region_Path := AML_Names.Read_Name (Data (Data'First + Cursor .. Data'First + (Finish - 1)));
         if Region_Path.Kind /= AML_Names.Accepted or else Region_Path.Count = 0 then Status := Bad_Name; return; end if;
         Cursor := Cursor + Region_Path.Consumed;
         Region_Node := Resolve (Candidate, Parent, Region_Path);
         if Region_Node.Status /= Found or else Region_Node.Node = Root then Status := Missing_Scope; return; end if;
         if Kind (Candidate, Region_Node.Node) /= Operation_Region_Object then Status := Unsupported_Opcode; return; end if;
         if Cursor = Finish then return; end if;
         Info.Region := Region_Node.Node;
         Info.Incarnation := Candidate.Items (Info.Region).Incarnation;
         Info.Raw_Flags := Data (Data'First + Cursor);
         Info.Access_Type := Info.Raw_Flags and 16#0F#;
         Cursor := Cursor + 1;
         while Cursor < Finish loop
            Item := AML_Fields.Read_Entry (Data (Data'First + Cursor .. Data'First + (Finish - 1)));
            if Item.Status /= AML_Decode.Accepted then return; end if;
            case Item.Kind is
               when AML_Fields.Named_Field | AML_Fields.Reserved_Field =>
                  if Field_Bit_Position (Item.Bits) > Field_Bit_Position'Last - Info.Offset then return; end if;
                  if Item.Kind = AML_Fields.Named_Field then
                     Insert (Candidate, Parent, Item.Name, Field_Node, Inserted_Status);
                     case Inserted_Status is
                        when Duplicate => Status := Duplicate_Name; return;
                        when Full => Status := Storage_Full; return;
                        when Invalid_Name => Status := Bad_Name; return;
                        when Inserted => null;
                     end case;
                     Info.Bits := Item.Bits;
                     Candidate.Items (Field_Node).Region_Field_Info := Info;
                     Candidate.Items (Field_Node).Object_Type := Region_Field_Object;
                  end if;
                  Info.Offset := Info.Offset + Field_Bit_Position (Item.Bits);
               when AML_Fields.Access_Field | AML_Fields.Extended_Access_Field =>
                  Info.Access_Type := Item.Access_Type;
                  Info.Attribute := Item.Attribute;
                  -- AccessField has no third byte: psargs.c packs zero there;
                  -- dsfield.c replaces, rather than retains, AccessLength.
                  Info.Access_Length := Item.Access_Length;
               when AML_Fields.Name_Connection | AML_Fields.Buffer_Connection =>
                  Status := Unsupported_Opcode; return;
            end case;
            Cursor := Cursor + Item.Consumed;
         end loop;
         Status := Loaded;
      end Static_Fields;
   begin
      Result := Loaded;
      loop
         pragma Loop_Invariant (Offset <= Data'Length);
         pragma Loop_Invariant (Tree = Tree'Loop_Entry);
         pragma Loop_Invariant
           (Valid_Context (Candidate));
         pragma Loop_Invariant
           (for all I in 0 .. Depth =>
              Offset <= Frames (I).Limit and then
              Frames (I).Limit <= Data'Length and then
              Frames (I).Scope <= Candidate.Used);
         pragma Loop_Invariant
           (for all I in 0 .. Depth =>
              (for all J in I .. Depth => Frames (J).Limit <= Frames (I).Limit));
         pragma Loop_Variant
           (Decreases => Data'Length - Offset, Decreases => Depth);
         if Offset = Frames (Depth).Limit then
            exit when Depth = 0;
            Depth := Depth - 1;
         else
            Limit := Frames (Depth).Limit;
            Op := Data (Data'First + Offset);
            Offset := Offset + 1;
            Is_Scope := Op = 16#10#;
            Is_Device := False;
            Is_Event := False;
            Is_Mutex := False;
            Is_Processor := False;
            Is_Power := False;
            Is_Thermal := False;
            Is_Operation := False; Is_Field := False;
            Is_Method := Op = 16#14#;
            if Op = Extended_Opcode and then Offset < Limit then
               Is_Device := Data (Data'First + Offset) = Device_Opcode;
               Is_Event := Data (Data'First + Offset) = Event_Opcode;
               Is_Mutex := Data (Data'First + Offset) = Mutex_Opcode;
               Is_Processor := Data (Data'First + Offset) = Processor_Opcode;
               Is_Power := Data (Data'First + Offset) = Power_Opcode;
               Is_Thermal := Data (Data'First + Offset) = Thermal_Opcode;
               Is_Operation := Data (Data'First + Offset) = Region_Opcode;
               Is_Field := Data (Data'First + Offset) = Field_Opcode;
               Offset := Offset + 1;
            end if;
            Is_Container := Is_Device or Is_Processor or Is_Power or Is_Thermal;
            if not Is_Scope and then not Is_Container and then not Is_Method
              and then not Is_Event and then not Is_Mutex and then not Is_Operation and then not Is_Field and then Op /= 16#08# then
               Result := Unsupported_Opcode; return;
            end if;
            if Is_Scope or Is_Container or Is_Method or Is_Field then
               if Offset = Limit then Result := Bad_Package; return; end if;
               Package_Info := AML_Decode.Read_Package
                 (Data (Data'First + Offset .. Data'First + (Limit - 1)));
               if Package_Info.Kind /= AML_Decode.Accepted then
                  Result := Bad_Package; return;
               end if;
               Limit := Offset + Package_Info.Extent;
               Offset := Offset + Package_Info.Encoding_Bytes;
            end if;
            if Is_Field then
               Static_Fields (Offset, Limit, Frames (Depth).Scope, Result);
               if Result /= Loaded then return; end if;
            else
            if Offset = Limit then Result := Bad_Name; return; end if;
            Path := AML_Names.Read_Name
              (Data (Data'First + Offset .. Data'First + (Limit - 1)));
            if Path.Kind /= AML_Names.Accepted then
               Result := Bad_Name; return;
            end if;
            Offset := Offset + Path.Consumed;
            if Is_Scope then
               Located := Resolve (Candidate, Frames (Depth).Scope, Path);
               if Located.Status /= Found then
                  Result := Missing_Scope; return;
               end if;
               Node := Located.Node;
               if not Static_Scope (Kind (Candidate, Node)) then
                  Result := Missing_Scope; return;
               end if;
            else
               if Path.Count = 0 then Result := Bad_Name; return; end if;
               Scope := (if Path.Rooted then Root else Frames (Depth).Scope);
               for I in 1 .. Path.Parents loop
                  pragma Loop_Invariant (Scope <= Candidate.Used);
                  if Scope = Root then Result := Missing_Scope; return; end if;
                  Scope := Parent (Candidate, Scope);
               end loop;
               for I in 1 .. Path.Count - 1 loop
                  pragma Loop_Invariant (Scope <= Candidate.Used);
                  Scope := Child (Candidate, Scope, Path.Parts (I));
                  if Scope = Root or else not Static_Scope (Kind (Candidate, Scope)) then
                     Result := Missing_Scope; return;
                  end if;
               end loop;
               Insert (Candidate, Scope, Path.Parts (Path.Count), Node, Added);
               case Added is
                  when Duplicate => Result := Duplicate_Name; return;
                  when Full => Result := Storage_Full; return;
                  when Invalid_Name => Result := Bad_Name; return;
                  when Inserted => null;
               end case;
            end if;
            if Is_Device then
               Candidate.Items (Node).Object_Type := Device_Object;
            elsif Is_Thermal then
               Candidate.Items (Node).Object_Type := Thermal_Zone_Object;
            elsif Is_Processor then
               if Limit - Offset < Processor_Tail_Bytes then Result := Bad_Package; return; end if;
               Candidate.Items (Node).Object_Type := Processor_Object;
               Candidate.Items (Node).Processor_Info.ID := Data (Data'First + Offset);
               Candidate.Items (Node).Processor_Info.PBlock_Address := 0;
               for I in 0 .. Address_Bytes - 1 loop
                  Candidate.Items (Node).Processor_Info.PBlock_Address :=
                    Candidate.Items (Node).Processor_Info.PBlock_Address +
                    Processor_Block_Address (Data (Data'First + Offset + 1 + I)) *
                    Processor_Block_Address (Octet_Radix) ** I;
               end loop;
               Candidate.Items (Node).Processor_Info.PBlock_Length :=
                 Data (Data'First + (Offset + (Processor_Tail_Bytes - 1)));
               Offset := Offset + Processor_Tail_Bytes;
            elsif Is_Power then
               if Limit - Offset < Power_Tail_Bytes then Result := Bad_Package; return; end if;
               Candidate.Items (Node).Object_Type := Power_Resource_Object;
               Candidate.Items (Node).Power_Info :=
                 (System_Level => Data (Data'First + Offset),
                  Order => Resource_Order (Data (Data'First + Offset + 1)) +
                    Resource_Order (Data (Data'First + Offset + 2)) * Resource_Order (Octet_Radix));
               Offset := Offset + Power_Tail_Bytes;
            end if;
            if Is_Method then
               if Offset = Limit then Result := Bad_Method; return; end if;
               Candidate.Items (Node).Method_Flags := Data (Data'First + Offset);
               Candidate.Items (Node).Method_Width := Width;
               Offset := Offset + 1;
               if Limit - Offset > AML_Execute.Max_Method_Bytes or else
                 Limit - Offset > Aggregate_Method_Capacity - Candidate.Code_Used then
                  Result := Value_Limit; return;
               end if;
               Candidate.Items (Node).Object_Type := Method_Object;
               if Offset < Limit then
                  Append_Code (Candidate.Code, Candidate.Code_Used,
                    Data (Data'First + Offset .. Data'First + (Limit - 1)), Code_Start);
               else
                  -- At Positive'Last an empty slice's lower bound would
                  -- overflow. No bytes need appending for an empty method.
                  Code_Start := Candidate.Code_Used;
               end if;
               Candidate.Items (Node).Method_Offset := Code_Start;
               Candidate.Items (Node).Method_Size := Limit - Offset;
               pragma Assert
                 (if Offset < Limit then Method_Data (Candidate, Node) =
                    Data (Data'First + Offset .. Data'First + (Limit - 1))
                  else Method_Data (Candidate, Node)'Length = 0);
               Offset := Limit;
            elsif Is_Operation then
               if Offset = Limit then Result := Bad_Integer; return; end if;
               Candidate.Items (Node).Operation_Info.Space := Region_Space_ID (Data (Data'First + Offset));
               Candidate.Items (Node).Operation_Info.Width := Width;
               Offset := Offset + 1;
               for Part in 1 .. 2 loop
                  if Offset = Limit then Result := Bad_Integer; return; end if;
                  Value := AML_Decode.Read_Integer (Data (Data'First + Offset .. Data'First + (Limit - 1)), Width);
                  if Value.Kind /= AML_Decode.Accepted then
                     Result := (if Value.Kind = AML_Decode.Unsupported then Unsupported_Opcode else Bad_Integer); return;
                  end if;
                  if Part = 1 then Candidate.Items (Node).Operation_Info.Address := Value.Value;
                  else Candidate.Items (Node).Operation_Info.Length := Value.Value; end if;
                  Offset := Offset + Value.Consumed;
               end loop;
               Candidate.Items (Node).Object_Type := Operation_Region_Object;
            elsif Is_Event then
               Candidate.Items (Node).Object_Type := Event_Object;
            elsif Is_Mutex then
               if Offset = Limit then Result := Bad_Integer; return; end if;
               Candidate.Items (Node).Object_Type := Mutex_Object;
               Candidate.Items (Node).Mutex_Flags := Data (Data'First + Offset);
               Offset := Offset + 1;
            elsif Is_Scope or Is_Container then
               if Depth = 64 then Result := Nesting_Limit; return; end if;
               Depth := Depth + 1;
               Frames (Depth) := (Limit => Limit, Scope => Node);
            else
               if Offset = Limit then Result := Bad_Integer; return; end if;
               if Data (Data'First + Offset) in 16#12# | 16#13# then
                  declare
                     Used : Natural;
                     Parsed : AML_Decode.Status;
                     Environment : Count_Context := (Candidate, Frames (Depth).Scope);
                  begin
                     Load_Bound_Data (Candidate.Values,
                       Data (Data'First + Offset .. Data'First + (Limit - 1)), Width,
                       Environment, Object_Ref, Used, Parsed);
                     if Parsed /= AML_Decode.Accepted then
                        Result := (if Parsed = AML_Decode.Limit_Exceeded then Value_Limit else Bad_Package);
                        return;
                     end if;
                     if AML_Objects.Kind (Candidate.Values, Object_Ref) /= AML_Objects.Package_Object then
                        Result := Bad_Package; return;
                     end if;
                     Offset := Offset + Used;
                     Candidate.Items (Node).Object_Ref := Object_Ref;
                     Candidate.Journal := Environment.Tree.Journal;
                     Candidate.Items (Node).Object_Type := Package_Object;
                  end;
               elsif Data (Data'First + Offset) = 16#0D# then
                  Text := AML_Decode.Read_String
                    (Data (Data'First + Offset .. Data'First + (Limit - 1)));
                  if Text.Kind /= AML_Decode.Accepted then
                     Result := (if Text.Kind = AML_Decode.Limit_Exceeded then
                                  Value_Limit else Bad_String);
                     return;
                  end if;
                  Offset := Offset + Text.Consumed;
                  declare
                     Text_Length : constant Natural := Text.Length;
                     Data_Bytes : AML_Decode.Bytes (1 .. Text_Length);
                  begin
                     for I in Data_Bytes'Range loop
                        Data_Bytes (I) := Character'Pos (Text.Text (I));
                     end loop;
                     AML_Objects.New_Bytes (Candidate.Values, AML_Objects.String_Object,
                                            Data_Bytes, Object_Ref, Allocation);
                  end;
                  if Allocation /= AML_Objects.Allocated then Result := Value_Limit; return; end if;
                  Candidate.Items (Node).Object_Ref := Object_Ref;
                  Candidate.Items (Node).Object_Type := String_Object;
               elsif Data (Data'First + Offset) = 16#11# then
                  Buffer_Item := AML_Decode.Read_Buffer
                    (Data (Data'First + Offset .. Data'First + (Limit - 1)), Width);
                  if Buffer_Item.Kind = AML_Decode.Unsupported then
                     -- Only a bounded, already defined canonical value is admitted.
                     -- No methods, fields, or other TermArgs execute while loading.
                     declare
                        Bytes : AML_Decode.Bytes renames
                          Data (Data'First + Offset .. Data'First + (Limit - 1));
                        Header : constant AML_Decode.Package_Result :=
                          AML_Decode.Read_Package (Bytes (Bytes'First + 1 .. Bytes'Last));
                     begin
                        if Header.Kind = AML_Decode.Accepted
                          and then Header.Encoding_Bytes < Header.Extent then
                           declare
                              Count : constant AML_Data.Count_Result := Read_Static_Buffer_Count
                                ((Candidate, Frames (Depth).Scope),
                                 Bytes (Bytes'First + (1 + Header.Encoding_Bytes) ..
                                        Bytes'First + Header.Extent), Width);
                           begin
                              if Count.Kind = AML_Decode.Accepted then
                                 Buffer_Item := AML_Decode.Read_Buffer_With_Count
                                   (Bytes, Width, Count.Value, Count.Consumed);
                              end if;
                           end;
                        end if;
                     end;
                  end if;
                  if Buffer_Item.Kind /= AML_Decode.Accepted then
                     Result := (if Buffer_Item.Kind = AML_Decode.Limit_Exceeded then
                                  Value_Limit elsif Buffer_Item.Kind = AML_Decode.Unsupported then
                                  Unsupported_Opcode else Bad_Buffer);
                     return;
                  end if;
                  Offset := Offset + Buffer_Item.Consumed;
                  AML_Objects.New_Bytes (Candidate.Values, AML_Objects.Buffer_Object,
                    Buffer_Item.Content (1 .. Buffer_Item.Length), Object_Ref, Allocation);
                  if Allocation /= AML_Objects.Allocated then Result := Value_Limit; return; end if;
                  Candidate.Items (Node).Object_Ref := Object_Ref;
                  Candidate.Items (Node).Object_Type := Buffer_Object;
               else
                  Value := AML_Decode.Read_Integer
                    (Data (Data'First + Offset .. Data'First + (Limit - 1)), Width);
                  if Value.Kind /= AML_Decode.Accepted then
                     Result := Bad_Integer; return;
                  end if;
                  Offset := Offset + Value.Consumed;
                  AML_Objects.New_Integer (Candidate.Values, Value.Value, Object_Ref, Allocation,
                    AML_Decode.Literal_Origin (Data (Data'First + (Offset - Value.Consumed))));
                  if Allocation /= AML_Objects.Allocated then Result := Value_Limit; return; end if;
                  Candidate.Items (Node).Object_Ref := Object_Ref;
                  Candidate.Items (Node).Object_Type := Integer_Object;
               end if;
            end if;
            end if; -- non-Field declaration
         end if;
      end loop;
      Tree := Candidate;
   end Load_Names;
package body Owned with SPARK_Mode is
   function Snapshot (A : Arena) return State is (A.Tree);
   function Target (R : Reference) return AML_Objects.Object_ID is
     (AML_References.Target (R));
   function Offset (R : Reference) return Natural is
     (AML_References.Offset (R));
   function Byte_At (A : Arena; R : Reference) return AML_Decode.Byte is
     (AML_Objects.Stored_Byte (A.Tree.Values, Target (R), Offset (R)));
   function Valid (A : Arena) return Boolean is
     (Frame_Pins.Valid (A.Frame_Roots) and then Pins.Valid (A.Retention) and then Pins.Owner (A.Retention) = A.Token
       and then Pins.Pending_Count (A.Retention) = 0
       and then A.Invocation_Issued <= Max_Invocations and then Valid_Context (A.Tree) and then
       (A.Token = AML_Identity.No_Identity or else AML_Identity.Issuer.Is_Issued (A.Token)));
   function Frame_Root_Count (A : Arena) return Natural is (Frame_Pins.Count (A.Frame_Roots));
   function Node_Count (A : Arena) return Node_ID is (Count (A.Tree));
   function Present (A : Arena; Node : Node_ID) return Boolean is (Present (A.Tree, Node));
   function Kind (A : Arena; Node : Node_ID) return Object_Kind is (Kind (A.Tree, Node));
   function Mutex_Data (A : Arena; Node : Node_ID) return Mutex_Metadata is
     (Mutex_Data (A.Tree, Node));
   function Processor_Data (A : Arena; Node : Node_ID) return Processor_Attributes is
     (Processor_Data (A.Tree, Node));
   function Power_Data (A : Arena; Node : Node_ID) return Power_Attributes is
     (Power_Data (A.Tree, Node));
   function Operation_Region_Data (A : Arena; Node : Node_ID) return Operation_Region_Attributes is
     (Operation_Region_Data (A.Tree, Node));
   function Region_Field_Data (A : Arena; Node : Node_ID) return Region_Field_Attributes is
     (Region_Field_Data (A.Tree, Node));
   function Region_Data (A : Arena; Node : Node_ID) return Table_Region is (Region_Data (A.Tree, Node));
   function Field_Data (A : Arena; Node : Node_ID) return Table_Field is (Field_Data (A.Tree, Node));
   function Values_Used (A : Arena) return AML_Objects.Usage is (Value_Usage (A.Tree));
   function Methods_Used (A : Arena) return Aggregate_Method_Count is (Method_Usage (A.Tree));
   function Initialized (A : Arena) return Boolean is
     (A.Token /= AML_Identity.No_Identity);
   function Generation (A : Arena) return AML_Identity.Identity is (A.Token);
   function Model (A : Arena) return AML_Objects.State is (A.Tree.Values);
   function Matches (A : Arena; R : Reference) return Boolean is
     (AML_References.Belongs_To (R, A.Token) and then
       (case AML_References.Kind (R) is
          when AML_References.Absent | AML_References.Frame_Cell | AML_References.Name_Member => False,
          when AML_References.Named_Cell =>
            AML_References.Named_Node (R) > 0
            and then AML_References.Named_Node (R) <= AML_References.Node_Position (A.Tree.Used)
            and then A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Alive
            and then A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Incarnation = AML_References.Incarnation (R)
            and then A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Object_Type in
              Integer_Object | String_Object | Buffer_Object | Package_Object | Reference_Object | Uninitialized_Name_Object,
          when AML_References.Byte_Slot => AML_Objects.Byte_References.Is_Valid
            (A.Tree.Values, AML_References.Byte_Item (R)),
          when AML_References.Package_Slot => AML_Objects.Package_References.Is_Valid
            (A.Tree.Values, AML_References.Package_Item (R))));

   procedure Load (A : in out Arena; Data : AML_Decode.Bytes;
     Width : AML_Decode.Integer_Width; Result : out Load_Status) is
   begin
      Load_Names (A.Tree, Data, Width, Result);
   end Load;
   function Pending_Members (A : Arena) return Natural is
     (AML_Namespace.Pending_Members (A.Tree));
   procedure Initialize_Members (A : in out Arena; Report : out Initialization_Report) is
   begin
      Initialize_Members (A.Tree, Report);
   end Initialize_Members;
   procedure Reset (A : in out Arena; Success : out Boolean) is
      Token : AML_Identity.Identity;
   begin
      if Frame_Pins.Count (A.Frame_Roots) /= 0 then Success := False; return; end if;
      AML_Identity.Issuer.Issue (Token, Success);
      if Success then
         declare
            Result : Pins.Result_Status;
            use type Pins.Result_Status;
         begin
            if A.Token = AML_Identity.No_Identity then Pins.Bind (A.Retention, Token, Result);
            else Pins.Reset (A.Retention, Token, Result); end if;
            if Result /= Pins.Ready then raise Program_Error with "retention reset invariant"; end if;
         end;
         A.Token := Token; A.Tree := Empty; A.Invocation_Issued := 0;
      end if;
   end Reset;
   function Retention_Model (A : Arena) return Retention_State is
     (Retention_State (Pins.Snapshot (A.Retention)));
   function Retained_Count (A : Arena) return Retained_Root_Count is
     (Pins.Published_Count (A.Retention));
   function Retention_Added (A : Arena; Before : Retention_State;
                             Root : Retained_Root; Value : AML_Execute.Datum) return Boolean is
      use type Pins.Result_Status;
      Saved : constant Pins.Read_Result := Pins.Read (A.Retention, Root.Pin);
      Canonical : AML_Execute.Datum := Value;
      Result : AML_Execute.Execution_Status := AML_Execute.Returned;
   begin
      if Value.Value_Kind = AML_Execute.Object_Datum then
         if Value.Object.ID /= AML_References.Source (Value.Object.Source)
           or else not Has_Source (A, Value.Object.Source) then return False; end if;
         Read_Source (A, Value.Object.Source, Canonical, Result);
      end if;
      return Result = AML_Execute.Returned and then Saved.Status = Pins.Ready
        and then Saved.Value = Canonical and then Pins.Published_From_Reservation
        (Pins.Snapshot (A.Retention), Pins.Model (Before), Root.Pin, Saved.Value);
   end Retention_Added;
   function Retention_Removed (A : Arena; Before : Retention_State;
                               Root : Retained_Root) return Boolean is
     (Pins.Changed (Pins.Snapshot (A.Retention), Pins.Model (Before), Root.Pin,
       Pins.Published, Pins.Vacant,
       (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer), False));
   function Retention_Cleared (A : Arena; Before : Retention_State) return Boolean is
     (Pins.Bound (Pins.Snapshot (A.Retention), Pins.Model (Before), A.Token, True));
   function Retention_Read (A : Arena; Root : Retained_Root;
                            Value : AML_Execute.Datum;
                            Status : AML_Execute.Execution_Status) return Boolean is
      use AML_Execute;
      use type Pins.Result_Status;
      Saved : constant Pins.Read_Result := Pins.Read (A.Retention, Root.Pin);
      Canonical : Datum := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
      Expected : Execution_Status := Unsupported_Value;
   begin
      if Saved.Status = Pins.Ready then
         if Saved.Value.Value_Kind = Object_Datum then
            if Saved.Value.Object.ID = AML_References.Source (Saved.Value.Object.Source)
              and then Has_Source (A, Saved.Value.Object.Source) then
               Read_Source (A, Saved.Value.Object.Source, Canonical, Expected);
            end if;
         else Canonical := Saved.Value; Expected := Returned;
         end if;
      end if;
      return Status = Expected and then Value = Canonical;
   end Retention_Read;
   function Admitted_Descriptor (A : Arena; R : Reference) return Boolean is
     (AML_References.Well_Formed (R) and then
       (if AML_References.Kind (R) = AML_References.Frame_Cell then
          AML_Frame_Handles.Belongs_To (AML_References.Frame_Item (R),
             AML_Frame_Handles.Bind_Domain (A.Token, A.Invocation_Issued))
        else AML_References.Belongs_To (R, A.Token)));
   procedure Retain (A : in out Arena; Value : AML_Execute.Datum;
                     Root : out Retained_Root; Status : out Retain_Status) is
      use AML_Execute;
      use type Pins.Result_Status;
      Canonical : Datum := Value;
      Result : Execution_Status;
      Pin_Result : Pins.Result_Status;
      Pin : Pins.Token;
   begin
      Root := No_Retained_Root; Status := Invalid_Value;
      if not Initialized (A) then return; end if;
      if Value.Value_Kind = Object_Datum then
         if Value.Object.ID /= AML_References.Source (Value.Object.Source)
           or else not Has_Source (A, Value.Object.Source) then return; end if;
         Read_Source (A, Value.Object.Source, Canonical, Result);
         if Result /= Returned then return; end if;
      end if;
      if Canonical.Value_Kind = Reference_Datum and then
        not Admitted_Descriptor (A, Canonical.Ref) then return; end if;
      Pins.Reserve (A.Retention, Pin, Pin_Result);
      case Pin_Result is
         when Pins.Root_Limit => Status := Root_Limit; return;
         when Pins.Identity_Exhausted => Status := Identity_Exhausted; return;
         when Pins.Ready => null;
         when others => raise Program_Error with "retention reserve invariant";
      end case;
      -- No callback, allocation or publication can intervene between these two
      -- private registry operations. A failure is an invariant breach, fatal
      -- even with assertions suppressed; it is not a rollback-safe rejection.
      Pins.Publish (A.Retention, Pin, Canonical, Pin_Result);
      if Pin_Result /= Pins.Ready then raise Program_Error with "retention publish invariant"; end if;
      Root := (Pin => Pin); Status := Retained;
   end Retain;
   procedure Read_Retained (A : Arena; Root : Retained_Root;
                            Value : out AML_Execute.Datum;
                            Status : out AML_Execute.Execution_Status) is
      use AML_Execute;
      use type Pins.Result_Status;
      Saved : constant Pins.Read_Result := Pins.Read (A.Retention, Root.Pin);
   begin
      Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer); Status := Unsupported_Value;
      if Saved.Status /= Pins.Ready then return; end if;
      if Saved.Value.Value_Kind = Object_Datum then
         if Saved.Value.Object.ID /= AML_References.Source (Saved.Value.Object.Source)
           or else not Has_Source (A, Saved.Value.Object.Source) then return; end if;
         Read_Source (A, Saved.Value.Object.Source, Value, Status);
      else Value := Saved.Value; Status := Returned;
      end if;
   end Read_Retained;
   procedure Release (A : in out Arena; Root : in out Retained_Root;
                      Status : out Release_Status) is
      use type Pins.Result_Status;
      Result : Pins.Result_Status;
   begin
      Pins.Release (A.Retention, Root.Pin, Result);
      Status := (if Result = Pins.Ready then Released else Invalid_Root);
   end Release;
   procedure Bind_Table_Region
     (A : in out Arena; Scope : Node_ID; Part : AML_Names.Segment;
      Region : Table_Region; Node : out Node_ID; Result : out Bind_Status;
      Owner : Node_ID := Root) is
   begin
      AML_Namespace.Bind_Table_Region (A.Tree, Scope, Part, Region, Node, Result, Owner);
   end Bind_Table_Region;
   procedure Bind_Table_Field
     (A : in out Arena; Scope : Node_ID; Part : AML_Names.Segment;
      Field : Table_Field; Node : out Node_ID; Result : out Bind_Status;
      Owner : Node_ID := Root) is
   begin
      AML_Namespace.Bind_Table_Field (A.Tree, Scope, Part, Field, Node, Result, Owner);
   end Bind_Table_Field;
   procedure Append (A : in out Arena; Data : AML_Decode.Bytes;
     ID : out AML_Objects.Object_ID; Status : out AML_Objects.Allocation_Status) is
   begin
      AML_Objects.New_Bytes (A.Tree.Values, AML_Objects.Buffer_Object, Data, ID, Status);
   end Append;
   procedure Make (A : Arena; ID : AML_Objects.Object_ID;
     Index : AML_Decode.Integer_Value; R : out Reference;
     Status : out AML_Objects.Byte_References.Result_Status) is
      Item : AML_Objects.Byte_References.Reference;
   begin
      R := AML_References.No_Reference;
      if A.Token = AML_Identity.No_Identity then
         Status := AML_Objects.Byte_References.Invalid_Reference; return;
      end if;
      AML_Objects.Byte_References.Make (A.Tree.Values, ID, Index, Item, Status);
      if Status = AML_Objects.Byte_References.Ready then R := AML_References.Bind (A.Token, Item); end if;
   end Make;
   procedure Read (A : Arena; R : Reference; Value : out AML_Decode.Byte;
     Success : out Boolean) is
      Status : AML_Objects.Byte_References.Result_Status;
   begin
      Success := Matches (A, R) and then AML_References.Kind (R) = AML_References.Byte_Slot; Value := 0;
      if Success then AML_Objects.Byte_References.Read (A.Tree.Values, AML_References.Byte_Item (R), Value, Status);
         Success := Status = AML_Objects.Byte_References.Ready;
      end if;
   end Read;
   procedure Write (A : in out Arena; R : Reference; Value : AML_Decode.Byte;
     Success : out Boolean) is
      Status : AML_Objects.Byte_References.Result_Status;
   begin
      Success := Matches (A, R) and then AML_References.Kind (R) = AML_References.Byte_Slot;
      if Success then AML_Objects.Byte_References.Write (A.Tree.Values, AML_References.Byte_Item (R), Value, Status);
         Success := Status = AML_Objects.Byte_References.Ready;
      end if;
   end Write;
   function Element_At (A : Arena; R : Reference) return AML_Objects.Object_ID is
     (AML_Objects.Element (A.Tree.Values, Target (R), Offset (R)));
   procedure Make_Element (A : Arena; ID : AML_Objects.Object_ID;
     Index : AML_Decode.Integer_Value; R : out Reference;
     Status : out AML_Objects.Package_References.Result_Status) is
      Item : AML_Objects.Package_References.Reference;
   begin
      R := AML_References.No_Reference;
      if A.Token = AML_Identity.No_Identity then
         Status := AML_Objects.Package_References.Invalid_Reference; return;
      end if;
      AML_Objects.Package_References.Make (A.Tree.Values, ID, Index, Item, Status);
      if Status = AML_Objects.Package_References.Ready then R := AML_References.Bind (A.Token, Item); end if;
   end Make_Element;
   procedure Read_Element (A : Arena; R : Reference; Value : out AML_Objects.Object_ID;
     Success : out Boolean) is
      Status : AML_Objects.Package_References.Result_Status;
   begin
      Success := Matches (A, R) and then AML_References.Kind (R) = AML_References.Package_Slot;
      Value := 0;
      if Success then AML_Objects.Package_References.Read
        (A.Tree.Values, AML_References.Package_Item (R), Value, Status);
        Success := Status = AML_Objects.Package_References.Ready;
      end if;
   end Read_Element;
   procedure Write_Element (A : in out Arena; R : Reference; Value : AML_Objects.Object_ID;
     Success : out Boolean) is
      Status : AML_Objects.Package_References.Result_Status;
   begin
      Success := Matches (A, R) and then AML_References.Kind (R) = AML_References.Package_Slot;
      if Success then AML_Objects.Package_References.Write
        (A.Tree.Values, AML_References.Package_Item (R), Value, Status);
        Success := Status = AML_Objects.Package_References.Ready;
      end if;
   end Write_Element;

   procedure Store_Integer (A : in out Arena; R : Reference;
     Value : AML_Decode.Integer_Value; Status : out AML_Execute.Execution_Status)
   is
      Candidate : AML_Objects.State;
      ID : AML_Objects.Object_ID;
      Allocated : AML_Objects.Allocation_Status;
      Stored : AML_Objects.Package_References.Result_Status;
      OK : Boolean;
   begin
      Status := AML_Execute.Unsupported_Value;
      if not Matches (A, R) then return; end if;
      if AML_References.Kind (R) = AML_References.Byte_Slot then
         Write (A, R, AML_Decode.Byte (Value mod 256), OK);
         if OK then Status := AML_Execute.Returned; end if;
      elsif AML_References.Kind (R) = AML_References.Package_Slot then
         Candidate := A.Tree.Values;
         AML_Objects.New_Integer (Candidate, Value, ID, Allocated);
         if Allocated /= AML_Objects.Allocated then
            Status := AML_Execute.Value_Limit; return;
         end if;
         AML_Objects.Package_References.Write
           (Candidate, AML_References.Package_Item (R), ID, Stored);
         if Stored /= AML_Objects.Package_References.Ready then return; end if;
         A.Tree.Values := Candidate;
         Status := AML_Execute.Returned;
      end if;
   end Store_Integer;

   procedure Read_Source
     (A : Arena; Source : AML_References.Object_Handle;
      Value : out AML_Execute.Datum; Status : out AML_Execute.Execution_Status)
   is
      use AML_Execute;
      ID : constant AML_Objects.Object_ID := AML_References.Source (Source);
      C32, C64 : AML_Coercions.Result;
   begin
      Value := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
      Status := Unsupported_Value;
      if not Has_Source (A, Source) then return; end if;
      if AML_Objects.Kind (A.Tree.Values, ID) = AML_Objects.Reference_Object then
         Value := (Value_Kind => Reference_Datum, Ref => AML_Objects.Reference_Data (A.Tree.Values, ID));
      elsif AML_Objects.Kind (A.Tree.Values, ID) = AML_Objects.Integer_Object then
         Value := (Value_Kind => Integer_Datum, Number => AML_Objects.Integer_Data (A.Tree.Values, ID), Origin => AML_Objects.Origin_Of (A.Tree.Values, ID));
      else
         if AML_Objects.Kind (A.Tree.Values, ID) = AML_Objects.String_Object then
            C32 := AML_Coercions.From_String (AML_Objects.Byte_Data (A.Tree.Values, ID), AML_Decode.Bits_32);
            C64 := AML_Coercions.From_String (AML_Objects.Byte_Data (A.Tree.Values, ID), AML_Decode.Bits_64);
         elsif AML_Objects.Kind (A.Tree.Values, ID) = AML_Objects.Buffer_Object then
            C32 := AML_Coercions.From_Buffer (AML_Objects.Byte_Data (A.Tree.Values, ID), AML_Decode.Bits_32);
            C64 := AML_Coercions.From_Buffer (AML_Objects.Byte_Data (A.Tree.Values, ID), AML_Decode.Bits_64);
         end if;
         Value := (Value_Kind => Object_Datum,
           Object => (Source => AML_References.Bind_Object (A.Token, AML_Objects.Address_Of (A.Tree.Values, ID)), ID => ID, Type_Code =>
             (case AML_Objects.Kind (A.Tree.Values, ID) is
                when AML_Objects.String_Object => 2, when AML_Objects.Buffer_Object => 3,
                when AML_Objects.Package_Object => 4, when others => 0),
             Size => AML_Objects.Length (A.Tree.Values, ID),
             Conversion_32 => C32, Conversion_64 => C64));
      end if;
      Status := Returned;
   end Read_Source;

   function Referent_Object (A : Arena; R : Reference) return AML_Objects.Object_ID is
     (if AML_References.Kind (R) = AML_References.Named_Cell then
        A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Object_Ref
      else Element_At (A, R));
   function Name_Member_Matches (A : Arena; R : Reference) return Boolean is
     (AML_References.Kind (R) = AML_References.Name_Member
      and then AML_References.Belongs_To (R, A.Token)
      and then AML_References.Named_Node (R) > 0
      and then AML_References.Named_Node (R) <= AML_References.Node_Position (A.Tree.Used)
      and then A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Alive
      and then A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Incarnation = AML_References.Incarnation (R)
      and then A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Object_Type in
        Integer_Object | String_Object | Buffer_Object | Package_Object | Reference_Object | Uninitialized_Name_Object);
   procedure Resolve_Name_Member (A : Arena; R : Reference; Value : out AML_Execute.Datum;
     Status : out AML_Execute.Execution_Status)
   is
      ID : AML_Objects.Object_ID;
   begin
      Value := (Value_Kind => AML_Execute.Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
      Status := AML_Execute.Unsupported_Value;
      if not Name_Member_Matches (A, R) then return; end if;
      ID := A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Object_Ref;
      if ID = 0 then Status := AML_Execute.Uninitialized; return; end if;
      Read_Source (A, AML_References.Bind_Object (A.Token, AML_Objects.Address_Of (A.Tree.Values, ID)), Value, Status);
   end Resolve_Name_Member;
   procedure Resolve_Value (A : Arena; R : Reference; Value : out AML_Execute.Datum;
     Status : out AML_Execute.Execution_Status) is
      use AML_Execute;
      ID : AML_Objects.Object_ID;
   begin
      Value := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
      Status := Unsupported_Value;
      if not Matches (A, R) then return; end if;
      if AML_References.Kind (R) = AML_References.Named_Cell then
         ID := A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Object_Ref;
         if ID = 0 then Status := Uninitialized; return; end if;
         Read_Source (A, AML_References.Bind_Object (A.Token, AML_Objects.Address_Of (A.Tree.Values, ID)), Value, Status);
         return;
      end if;
      if AML_References.Kind (R) = AML_References.Byte_Slot then
         Value := (Value_Kind => Integer_Datum,
           Number => AML_Decode.Integer_Value (AML_Objects.Stored_Byte
             (A.Tree.Values, AML_References.Target (R), AML_References.Offset (R))), Origin => AML_Decode.Ordinary_Integer);
         Status := Returned; return;
      end if;
      ID := AML_Objects.Element (A.Tree.Values, AML_References.Target (R), AML_References.Offset (R));
      if ID = 0 then Status := Uninitialized; return; end if;
      Read_Source (A, AML_References.Bind_Object (A.Token, AML_Objects.Address_Of (A.Tree.Values, ID)), Value, Status);
   end Resolve_Value;

   function Has_Source (A : Arena; H : AML_References.Object_Handle) return Boolean is
     (AML_References.Belongs_To (H, A.Token)
       and then AML_Objects.Matches_Address (A.Tree.Values, AML_References.Address (H)));
   function Root_Target (A : Arena; R : Reference) return AML_Object_Identifiers.Object_Address is
      ID : AML_Objects.Object_ID := AML_Objects.No_Object;
   begin
      case AML_References.Kind (R) is
         when AML_References.Named_Cell =>
            if Matches (A, R) then
               ID := A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Object_Ref;
            end if;
         when AML_References.Name_Member =>
            if Name_Member_Matches (A, R) then
               ID := A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Object_Ref;
            end if;
         when AML_References.Byte_Slot | AML_References.Package_Slot =>
            if Matches (A, R) then ID := AML_References.Target (R); end if;
         when AML_References.Absent | AML_References.Frame_Cell => null;
      end case;
      return AML_Objects.Address_Of (A.Tree.Values, ID);
   end Root_Target;

   procedure Value_Root (A : Arena; Value : AML_Execute.Datum;
      Address : out AML_Object_Identifiers.Object_Address; Accepted : out Boolean) is
   begin
      Address := AML_Object_Identifiers.No_Address;
      Accepted := True;
      case Value.Value_Kind is
         when AML_Execute.Integer_Datum => null;
         when AML_Execute.Object_Datum =>
            Accepted := Value.Object.ID = AML_References.Source (Value.Object.Source)
              and then Has_Source (A, Value.Object.Source);
            if Accepted then Address := AML_References.Address (Value.Object.Source); end if;
         when AML_Execute.Reference_Datum =>
            Accepted := AML_References.Well_Formed (Value.Ref);
            if Accepted then Address := Root_Target (A, Value.Ref); end if;
      end case;
   end Value_Root;

   function Expression_Snapshot_Matches (A : Arena; Values : Root_Values;
      Roots : AML_Objects.Root_Snapshots.Snapshot) return Boolean is
      Expected, Actual : Object_Root_Set := [others => False];
      Address : AML_Object_Identifiers.Object_Address;
      Accepted : Boolean;
      Status : AML_Objects.Root_Snapshots.Result_Status;
      use type AML_Objects.Root_Snapshots.Result_Status;
      use type Object_Root_Set;
   begin
      if not Initialized (A) then return False; end if;
      for Value of Values loop
         Value_Root (A, Value, Address, Accepted);
         if not Accepted then return False; end if;
         if AML_Object_Identifiers.Present (Address) then
            Expected (AML_Object_Identifiers.Slot_Of (Address)) := True;
         end if;
      end loop;
      AML_Objects.Root_Snapshots.Resolve (Roots, A.Token, A.Tree.Values, Actual, Status);
      return Status = AML_Objects.Root_Snapshots.Ready and then Actual = Expected;
   end Expression_Snapshot_Matches;

   procedure Build_Expression_Snapshot (A : Arena; Values : Root_Values;
      Roots : out AML_Objects.Root_Snapshots.Snapshot; Status : out Snapshot_Build_Status) is
      package R renames AML_Objects.Root_Snapshots;
      use type R.Result_Status;
      Address : AML_Object_Identifiers.Object_Address;
      Accepted : Boolean;
      Build_Status : R.Result_Status;
   begin
      R.Begin_Build (Roots, A.Token, Build_Status);
      Status := Snapshot_Uninitialized_Owner;
      if not Initialized (A) then R.Reject (Roots); return; end if;
      Status := Snapshot_Invalid_Value;
      for Value of Values loop
         Value_Root (A, Value, Address, Accepted);
         if not Accepted then R.Reject (Roots); return; end if;
         if AML_Object_Identifiers.Present (Address) then
            R.Include (Roots, A.Token, Address, Build_Status);
            if Build_Status /= R.Ready then return; end if;
         end if;
      end loop;
      R.Finish (Roots, A.Token, Build_Status);
      if Build_Status = R.Ready then Status := Snapshot_Built; end if;
   end Build_Expression_Snapshot;

   procedure Gather_Owner_Roots
     (A : Arena; Extra : Root_Values; Seeds : out Object_Root_Set;
      Targets : out AML_Objects.Reachability.Reference_Targets;
      Status : out Root_Trace_Status)
   is
      use type Pins.Result_Status;
      procedure Add_Value (Value : AML_Execute.Datum; Accepted : out Boolean) is
         Address : AML_Object_Identifiers.Object_Address;
      begin
         Value_Root (A, Value, Address, Accepted);
         if AML_Object_Identifiers.Present (Address) then
            Seeds (AML_Object_Identifiers.Slot_Of (Address)) := True;
         end if;
      end Add_Value;
      Accepted : Boolean;
   begin
      Seeds := [others => False];
      Targets := [others => AML_Object_Identifiers.No_Address];
      Status := Uninitialized_Owner;
      if not Initialized (A) then return; end if;
      for Node in 1 .. A.Tree.Used loop
         if A.Tree.Items (Node).Alive and then A.Tree.Items (Node).Object_Type in
           Integer_Object | String_Object | Buffer_Object | Package_Object | Reference_Object
         then Seeds (A.Tree.Items (Node).Object_Ref) := True; end if;
      end loop;
      Status := Invalid_Pending_Root;
      for Index in 1 .. Pending.Count (A.Tree.Journal) loop
         declare Item : constant Pending.Member_Result := Pending.Item (A.Tree.Journal, Index); begin
            if not Item.Found or else not AML_Objects.Is_Live (A.Tree.Values, Item.Package_ID)
              or else AML_Objects.Kind (A.Tree.Values, Item.Package_ID) /= AML_Objects.Package_Object
              or else Item.Element >= AML_Objects.Length (A.Tree.Values, Item.Package_ID)
              or else Item.Scope > A.Tree.Used
              or else (Item.Scope /= Root and then not A.Tree.Items (Item.Scope).Alive)
            then return; end if;
            Seeds (Item.Package_ID) := True;
         end;
      end loop;
      Status := Invalid_Root_Value;
      for Index in Pins.Root_Index loop
         declare Saved : constant Pins.Read_Result := Pins.Published_At (A.Retention, Index); begin
            if Saved.Status = Pins.Ready then
               Add_Value (Saved.Value, Accepted);
               if not Accepted then return; end if;
            end if;
         end;
      end loop;
      for Index in Frame_Pins.Root_Index loop
         for Cell in AML_Frame_Handles.Cell_ID loop
            declare Saved : constant Frame_Pins.Read_Result := Frame_Pins.Read_Cell (A.Frame_Roots, Index, Cell); begin
               if Saved.Initialized then
                  Add_Value (Saved.Value, Accepted);
                  if not Accepted then return; end if;
               end if;
            end;
         end loop;
      end loop;
      for Index in Frame_Pins.Root_Index loop
         for Root in AML_Root_Slots.Held_Root loop
            declare Saved : constant Frame_Pins.Read_Result := Frame_Pins.Read_Held (A.Frame_Roots, Index, Root); begin
               if Saved.Initialized then
                  Add_Value (Saved.Value, Accepted);
                  if not Accepted then return; end if;
               end if;
            end;
         end loop;
      end loop;
      Status := Invalid_Expression_Root;
      for Index in Frame_Pins.Root_Index loop
         declare
            Snapshot_Seeds : Object_Root_Set;
            Snapshot_Status : AML_Objects.Root_Snapshots.Result_Status;
            use type AML_Objects.Root_Snapshots.Result_Status;
         begin
            AML_Objects.Root_Snapshots.Resolve
              (Frame_Pins.Read_Snapshot (A.Frame_Roots, Index), A.Token,
               A.Tree.Values, Snapshot_Seeds, Snapshot_Status);
            if Snapshot_Status /= AML_Objects.Root_Snapshots.Ready then return; end if;
            for ID in Seeds'Range loop Seeds (ID) := Seeds (ID) or Snapshot_Seeds (ID); end loop;
         end;
      end loop;
      Status := Invalid_Root_Value;
      for Value of Extra loop
         Add_Value (Value, Accepted);
         if not Accepted then return; end if;
      end loop;
      for ID in 1 .. AML_Objects.Slot_Bound (A.Tree.Values) loop
         if AML_Objects.Is_Live (A.Tree.Values, ID)
           and then AML_Objects.Kind (A.Tree.Values, ID) = AML_Objects.Reference_Object
         then Targets (ID) := Root_Target (A, AML_Objects.Reference_Data (A.Tree.Values, ID)); end if;
      end loop;
      Status := Roots_Traced;
   end Gather_Owner_Roots;

   function Owner_Roots_Traced
     (A : Arena; Extra : Root_Values; Keep : Object_Root_Set;
      Scratch : Root_Workspace) return Boolean
   is
      Seeds : Object_Root_Set;
      Targets : AML_Objects.Reachability.Reference_Targets;
      Status : Root_Trace_Status;
   begin
      Gather_Owner_Roots (A, Extra, Seeds, Targets, Status);
      return Status = Roots_Traced and then
        AML_Objects.Reachability.Exact_Closure (A.Tree.Values, Seeds, Targets, Keep, Scratch.Walk);
   end Owner_Roots_Traced;

   procedure Trace_Owner_Roots
     (A : Arena; Extra : Root_Values; Scratch : in out Root_Workspace;
      Keep : out Object_Root_Set; Status : out Root_Trace_Status)
   is
      use type AML_Objects.Reachability.Trace_Status;
      Walk_Status : AML_Objects.Reachability.Trace_Status;
   begin
      Keep := [others => False];
      Gather_Owner_Roots (A, Extra, Scratch.Seeds, Scratch.Targets, Status);
      if Status /= Roots_Traced then return; end if;
      AML_Objects.Reachability.Trace
        (A.Tree.Values, Scratch.Seeds, Scratch.Targets, Scratch.Walk, Keep, Walk_Status);
      if Walk_Status /= AML_Objects.Reachability.Traced then Status := Invalid_Root_Value; end if;
   end Trace_Owner_Roots;

   procedure Clone_Source
     (A : in out Arena; Source : AML_References.Object_Handle;
      Copy : out AML_References.Object_Handle;
      Status : out AML_Execute.Execution_Status)
   is
      ID : AML_Objects.Object_ID;
      Allocated : AML_Objects.Allocation_Status;
      Witness : AML_Objects.Copies.Copy_Witness;
   begin
      Copy := AML_References.No_Object_Handle;
      Status := AML_Execute.Unsupported_Value;
      if not Has_Source (A, Source) then return; end if;
      AML_Objects.Copies.Clone
        (A.Tree.Values, AML_References.Source (Source), ID, Allocated, Witness);
      if Allocated /= AML_Objects.Allocated then
         Status := AML_Execute.Value_Limit;
         return;
      end if;
      Copy := AML_References.Bind_Object (A.Token, AML_Objects.Address_Of (A.Tree.Values, ID));
      Status := AML_Execute.Returned;
   end Clone_Source;
   procedure Refresh_Value
     (A : Arena; Item : in out AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
   is
      Source : AML_References.Object_Handle;
   begin
      Status := AML_Execute.Returned;
      if Item.Value_Kind /= AML_Execute.Object_Datum then return; end if;
      Status := AML_Execute.Unsupported_Value;
      Source := Item.Object.Source;
      if Item.Object.ID /= AML_References.Source (Source)
        or else not Has_Source (A, Source)
      then return; end if;
      Read_Source (A, Source, Item, Status);
   end Refresh_Value;

   procedure Convert_To_Integer
     (A : Arena; Width : AML_Decode.Integer_Width;
      Item : in out AML_Execute.Datum; Status : out AML_Execute.Execution_Status)
   is
      use AML_Execute;
      Converted : AML_Coercions.Result;
   begin
      Status := Unsupported_Value;
      if Item.Value_Kind = Integer_Datum then Status := Returned; return; end if;
      if Item.Value_Kind /= Object_Datum or else not Has_Source (A, Item.Object.Source)
        or else Item.Object.ID /= AML_References.Source (Item.Object.Source) then return; end if;
      declare ID : constant AML_Objects.Object_ID := AML_References.Source (Item.Object.Source); begin
         case AML_Objects.Kind (A.Tree.Values, ID) is
            when AML_Objects.String_Object =>
               Converted := AML_Coercions.From_Explicit_String (AML_Objects.Byte_Data (A.Tree.Values, ID), Width);
            when AML_Objects.Buffer_Object =>
               Converted := AML_Coercions.From_Buffer (AML_Objects.Byte_Data (A.Tree.Values, ID), Width);
            when others => return;
         end case;
      end;
      case Converted.Status is
         when AML_Coercions.Converted =>
            Item := (Value_Kind => Integer_Datum, Number => Converted.Value, Origin => AML_Decode.Ordinary_Integer);
            Status := Returned;
         when AML_Coercions.Empty_Buffer => Status := Empty_Buffer;
         when AML_Coercions.Not_Convertible => null;
      end case;
   end Convert_To_Integer;

   procedure Store_String
     (A : in out Arena; Target : AML_References.Object_Handle;
      Item : AML_Execute.Datum; Status : out AML_Execute.Execution_Status)
   is
      Target_ID : constant AML_Objects.Object_ID := AML_References.Source (Target);
      Updated : AML_Objects.String_Update_Status;
      use type AML_Objects.String_Update_Status;
   begin
      Status := AML_Execute.Unsupported_Value;
      if not Initialized (A) or else not Has_Source (A, Target)
        or else Item.Value_Kind /= AML_Execute.Object_Datum
        or else not Has_Source (A, Item.Object.Source)
        or else Item.Object.ID /= AML_References.Source (Item.Object.Source)
        or else AML_Objects.Kind (A.Tree.Values, Target_ID) /= AML_Objects.String_Object
        or else AML_Objects.Kind (A.Tree.Values, Item.Object.ID) /= AML_Objects.String_Object
      then return; end if;
      if Target_ID = Item.Object.ID then
         Status := AML_Execute.Returned;
         return;
      end if;
      AML_Objects.Replace_String
        (A.Tree.Values, Target_ID, AML_Objects.Byte_Data (A.Tree.Values, Item.Object.ID), Updated);
      case Updated is
         when AML_Objects.String_Updated => Status := AML_Execute.Returned;
         when AML_Objects.String_Byte_Limit => Status := AML_Execute.Value_Limit;
         when others => null;
      end case;
   end Store_String;

   procedure Match_Package
     (A : Arena; Width : AML_Decode.Integer_Width;
      Package_Value, Match_1, Match_2 : AML_Execute.Datum;
      Operation_1, Operation_2 : AML_Execute.Match_Operation;
      Start : AML_Decode.Integer_Value; Max_Visited : AML_Execute.Match_Visit_Count;
      Value : out AML_Decode.Integer_Value; Visited : out AML_Execute.Match_Visit_Count;
      Status : out AML_Execute.Execution_Status)
   is
      use AML_Execute;
      use AML_Mixed_Comparison;
      Package_ID, Element_ID : AML_Objects.Object_ID;
      Element_Value : Datum;
      Read_Status : Execution_Status;
      function Authenticated (Item : Datum) return Boolean is
        (Item.Value_Kind = Object_Datum
         and then Has_Source (A, Item.Object.Source)
         and then Item.Object.ID = AML_References.Source (Item.Object.Source));
      function Primitive (Item : Datum) return Boolean is
        (Item.Value_Kind = Integer_Datum
         or else (Authenticated (Item) and then
           AML_Objects.Kind (A.Tree.Values, Item.Object.ID) in
             AML_Objects.String_Object | AML_Objects.Buffer_Object));
      function Kind_Of (Item : Datum) return Input_Kind is
        (if Item.Value_Kind = Integer_Datum then Integer_Input
         elsif AML_Objects.Kind (A.Tree.Values, Item.Object.ID) = AML_Objects.String_Object
         then String_Input else Buffer_Input)
        with Pre => Primitive (Item);
      function Number_Of (Item : Datum) return AML_Decode.Integer_Value is
        (if Item.Value_Kind = Integer_Datum then AML_Integers.Normalize (Item.Number, Width) else 0);
      function Data_Of (Item : Datum) return AML_Decode.Bytes is
        (if Item.Value_Kind = Integer_Datum then AML_Decode.Bytes'(1 .. 0 => 0)
         else AML_Objects.Byte_Data (A.Tree.Values, Item.Object.ID))
        with Pre => Primitive (Item);
      function Satisfies (Operation : Match_Operation; Match_Value, Element : Datum) return Boolean is
         Compared_Value : Result;
      begin
         if Operation = Always_True then return True; end if;
         if not Primitive (Element) then return False; end if;
         -- Match type directs conversion; comparison is Match versus Element.
         Compared_Value := Compare (Width, Kind_Of (Match_Value), Number_Of (Match_Value), Data_Of (Match_Value),
           Kind_Of (Element), Number_Of (Element), Data_Of (Element));
         if Compared_Value.Status /= Compared then return False; end if;
         return (case Operation is
           when Always_True => True,
           when Equal_To => Compared_Value.Value = 0,
           when Less_Or_Equal => Compared_Value.Value >= 0,
           when Less_Than => Compared_Value.Value > 0,
           when Greater_Or_Equal => Compared_Value.Value <= 0,
           when Greater_Than => Compared_Value.Value < 0);
      end Satisfies;
   begin
      Value := 0; Visited := 0; Status := Unsupported_Value;
      if Start > AML_Coercions.Maximum (Width)
        or else not Authenticated (Package_Value)
        or else AML_Objects.Kind (A.Tree.Values, Package_Value.Object.ID) /= AML_Objects.Package_Object
        or else not Primitive (Match_1) or else not Primitive (Match_2)
      then return; end if;
      Package_ID := Package_Value.Object.ID;
      if Start >= AML_Decode.Integer_Value (AML_Objects.Length (A.Tree.Values, Package_ID)) then
         Status := Package_Limit; return;
      end if;
      for Index in Natural (Start) .. AML_Objects.Length (A.Tree.Values, Package_ID) - 1 loop
         if Visited = Max_Visited then Status := Budget_Exceeded; return; end if;
         Visited := Visited + 1;
         Element_ID := AML_Objects.Element (A.Tree.Values, Package_ID, Index);
         if Element_ID /= AML_Objects.No_Object then
            Read_Source (A, AML_References.Bind_Object (A.Token,
              AML_Objects.Address_Of (A.Tree.Values, Element_ID)), Element_Value, Read_Status);
            if Read_Status /= Returned then return; end if;
            if Element_Value.Value_Kind = Reference_Datum
              and then AML_References.Kind (Element_Value.Ref) = AML_References.Name_Member
            then
               -- Only loader-created name wrappers resolve; arbitrary reference
               -- descriptors remain initialized data, including for MTR.
               declare Named_Value : Datum; begin
                  Resolve_Name_Member (A, Element_Value.Ref, Named_Value, Read_Status);
                  if Read_Status /= Returned then return; end if;
                  Element_Value := Named_Value;
               end;
            end if;
            if Satisfies (Operation_1, Match_1, Element_Value)
              and then Satisfies (Operation_2, Match_2, Element_Value)
            then Value := AML_Decode.Integer_Value (Index); Status := Returned; return; end if;
         end if;
      end loop;
      Value := AML_Integers.Normalize (AML_Decode.Integer_Value'Last, Width);
      Status := Returned;
   end Match_Package;

   procedure Compare_Byte_Values
     (A : Arena; Op : AML_Decode.Byte; Left, Right : AML_Execute.Datum;
      Width : AML_Decode.Integer_Width; Value : out AML_Decode.Integer_Value;
      Status : out AML_Execute.Execution_Status)
   is
      use AML_Mixed_Comparison;
      Outcome : Result;
      Left_Kind, Right_Kind : Input_Kind;
      Match : Boolean;
      function Byte_Object (Item : AML_Execute.Datum) return Boolean is
        (Item.Value_Kind = AML_Execute.Object_Datum
         and then Has_Source (A, Item.Object.Source)
         and then Item.Object.ID = AML_References.Source (Item.Object.Source)
         and then AML_Objects.Kind (A.Tree.Values, Item.Object.ID) in
           AML_Objects.String_Object | AML_Objects.Buffer_Object);
      function Kind_Of (Item : AML_Execute.Datum) return Input_Kind is
        (if AML_Objects.Kind (A.Tree.Values, Item.Object.ID) = AML_Objects.String_Object
         then String_Input else Buffer_Input)
        with Pre => Byte_Object (Item);
   begin
      Value := 0; Status := AML_Execute.Unsupported_Value;
      if Op not in 16#93# .. 16#95# or else not Byte_Object (Left) then return; end if;
      Left_Kind := Kind_Of (Left);
      if Right.Value_Kind = AML_Execute.Integer_Datum then
         Outcome := Compare
           (Width, Left_Kind, 0, AML_Objects.Byte_Data (A.Tree.Values, Left.Object.ID),
            Integer_Input, AML_Integers.Normalize (Right.Number, Width),
            AML_Decode.Bytes'(1 .. 0 => 0));
      elsif Byte_Object (Right) then
         Right_Kind := Kind_Of (Right);
         Outcome := Compare
           (Width, Left_Kind, 0, AML_Objects.Byte_Data (A.Tree.Values, Left.Object.ID),
            Right_Kind, 0, AML_Objects.Byte_Data (A.Tree.Values, Right.Object.ID));
      else
         return;
      end if;
      if Outcome.Status /= Compared then return; end if;
      Match := (case Op is when 16#93# => Outcome.Value = 0,
                           when 16#94# => Outcome.Value > 0,
                           when others => Outcome.Value < 0);
      if Match then Value := AML_Integers.Normalize (AML_Decode.Integer_Value'Last, Width); end if;
      Status := AML_Execute.Returned;
   end Compare_Byte_Values;
   procedure Clone_Value
     (A : in out Arena; Width : AML_Decode.Integer_Width;
      Item : AML_Execute.Datum; Copy : out AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
   is
      Value : AML_Execute.Datum := Item;
      Source : AML_References.Object_Handle;
      Copied : AML_References.Object_Handle;
   begin
      Copy := (Value_Kind => AML_Execute.Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
      Status := AML_Execute.Unsupported_Value;
      if not Initialized (A) then return; end if;
      if Value.Value_Kind = AML_Execute.Reference_Datum
        and then AML_References.Kind (Value.Ref) in AML_References.Byte_Slot | AML_References.Package_Slot
      then
         declare
            Ref : constant Reference := Value.Ref;
         begin Resolve_Value (A, Ref, Value, Status); end;
         if Status /= AML_Execute.Returned then return; end if;
      end if;
      if Value.Value_Kind = AML_Execute.Object_Datum then
         Source := Value.Object.Source;
         if Value.Object.ID /= AML_References.Source (Source) then return; end if;
         Read_Source (A, Source, Value, Status);
         if Status /= AML_Execute.Returned then return; end if;
      end if;
      if Value.Value_Kind = AML_Execute.Reference_Datum then
         if not AML_References.Well_Formed (Value.Ref) then return; end if;
         Copy := Value; Status := AML_Execute.Returned;
      elsif Value.Value_Kind = AML_Execute.Integer_Datum then
         Copy := (Value_Kind => AML_Execute.Integer_Datum,
                  Number => AML_Integers.Normalize (Value.Number, Width), Origin => Value.Origin);
         Status := AML_Execute.Returned;
      else
         Clone_Source (A, Value.Object.Source, Copied, Status);
         if Status = AML_Execute.Returned then Read_Source (A, Copied, Copy, Status); end if;
      end if;
   end Clone_Value;
   procedure Replace_Value
     (A : in out Arena; Scope : Natural; Path : AML_Names.Name_Result;
      Width : AML_Decode.Integer_Width; Item : AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
   is
      Located : Lookup_Result;
      Value : AML_Execute.Datum := Item;
      ID : AML_Objects.Object_ID;
      Allocated : AML_Objects.Allocation_Status;
      Copied : AML_References.Object_Handle;
      Tag : Object_Kind;
   begin
      Status := AML_Execute.Unsupported_Value;
      if not Initialized (A) then return; end if;
      if Path.Kind /= AML_Names.Accepted or else Path.Count = 0 then
         Status := AML_Execute.Bad_Name; return;
      end if;
      if Scope > A.Tree.Used then Status := AML_Execute.Unknown_Name; return; end if;
      Located := Resolve (A.Tree, Node_ID (Scope), Path);
      if Located.Status /= Found then Status := AML_Execute.Unknown_Name; return; end if;
      if A.Tree.Items (Located.Node).Object_Type not in
        Integer_Object | String_Object | Buffer_Object | Package_Object | Reference_Object | Uninitialized_Name_Object
      then return; end if;
      if Value.Value_Kind = AML_Execute.Reference_Datum
        and then AML_References.Kind (Value.Ref) in AML_References.Byte_Slot | AML_References.Package_Slot
      then
         declare
            Ref : constant Reference := Value.Ref;
         begin
            Resolve_Value (A, Ref, Value, Status);
         end;
         if Status /= AML_Execute.Returned then return; end if;
      end if;
      if Value.Value_Kind = AML_Execute.Object_Datum then
         if Value.Object.ID /= AML_References.Source (Value.Object.Source) then
            Status := AML_Execute.Unsupported_Value; return;
         end if;
         declare
            Source : constant AML_References.Object_Handle := Value.Object.Source;
         begin
            Read_Source (A, Source, Value, Status);
         end;
         if Status /= AML_Execute.Returned then return; end if;
      end if;
      if Value.Value_Kind = AML_Execute.Reference_Datum then
         if not AML_References.Well_Formed (Value.Ref) then return; end if;
         AML_Objects.New_Reference (A.Tree.Values, Value.Ref, ID, Allocated);
         if Allocated /= AML_Objects.Allocated then Status := AML_Execute.Value_Limit; return; end if;
         Tag := Reference_Object;
      elsif Value.Value_Kind = AML_Execute.Integer_Datum then
         AML_Objects.New_Integer
           (A.Tree.Values, AML_Integers.Normalize (Value.Number, Width), ID, Allocated, Value.Origin);
         if Allocated /= AML_Objects.Allocated then
            Status := AML_Execute.Value_Limit; return;
         end if;
         Tag := Integer_Object;
      else
         Clone_Source (A, Value.Object.Source, Copied, Status);
         if Status /= AML_Execute.Returned then return; end if;
         ID := AML_References.Source (Copied);
         Tag := (case AML_Objects.Kind (A.Tree.Values, ID) is
           when AML_Objects.Integer_Object => Integer_Object,
           when AML_Objects.String_Object => String_Object,
           when AML_Objects.Buffer_Object => Buffer_Object,
           when AML_Objects.Package_Object => Package_Object,
           when AML_Objects.Reference_Object => Reference_Object);
      end if;
      A.Tree.Items (Located.Node) :=
        (A.Tree.Items (Located.Node) with delta Object_Type => Tag, Object_Ref => ID);
      Status := AML_Execute.Returned;
   end Replace_Value;
   procedure Store_Named_Direct
     (A : in out Arena; Node : Node_ID; Width : AML_Decode.Integer_Width;
      Item : AML_Execute.Datum; Status : out AML_Execute.Execution_Status)
   is
      use type AML_Decode.Integer_Width;
      Value : constant AML_Execute.Datum := Item;
      Target, Source : AML_Objects.Object_ID;
      Source_Kind, Target_Kind : AML_Objects.Object_Kind;
      Number : AML_Decode.Integer_Value := 0;
      Converted : AML_Coercions.Result;
      Count : Natural := 0;
      Octets : constant Positive := (if Width = AML_Decode.Bits_32 then 4 else 8);
      Hex_Digits : constant String := "0123456789ABCDEF";
      Hex_Per_Byte : constant Positive := 2;
      Prefixed_Hex_Width : constant Positive := 5;
      String_Status : AML_Objects.String_Update_Status;
      Buffer_Status : AML_Objects.Buffer_Update_Status;
      use type AML_Coercions.Conversion_Status;
      use type AML_Objects.String_Update_Status;
      use type AML_Objects.Buffer_Update_Status;
      function Hex (N : AML_Decode.Byte) return AML_Decode.Byte is
        (AML_Decode.Byte (Character'Pos (Hex_Digits (Natural (N) + 1))));
   begin
      Status := AML_Execute.Unsupported_Value;
      if not Initialized (A) or else Node = Root or else Node > A.Tree.Used
        or else not A.Tree.Items (Node).Alive
        or else A.Tree.Items (Node).Object_Type not in
          Integer_Object | String_Object | Buffer_Object
      then return; end if;
      Target := A.Tree.Items (Node).Object_Ref;
      Target_Kind := AML_Objects.Kind (A.Tree.Values, Target);
      -- Pinned ACPICA Store-to-simple-target probes reject RefOf and both
      -- Index descriptor sources. Explicit DerefOf supplies a concrete value.
      if Value.Value_Kind = AML_Execute.Reference_Datum then return; end if;
      Source := AML_Objects.No_Object;
      if Value.Value_Kind = AML_Execute.Object_Datum then
         if not Has_Source (A, Value.Object.Source)
           or else Value.Object.ID /= AML_References.Source (Value.Object.Source)
         then return; end if;
         Source := Value.Object.ID;
         Source_Kind := AML_Objects.Kind (A.Tree.Values, Source);
         if Source_Kind not in AML_Objects.Integer_Object | AML_Objects.String_Object | AML_Objects.Buffer_Object then return; end if;
         if Source = Target then Status := AML_Execute.Returned; return; end if;
         if Source_Kind = AML_Objects.Integer_Object then
            Number := AML_Integers.Normalize (AML_Objects.Integer_Data (A.Tree.Values, Source), Width);
         end if;
      else
         Source_Kind := AML_Objects.Integer_Object;
         Number := AML_Integers.Normalize (Value.Number, Width);
      end if;
      if Target_Kind = AML_Objects.Integer_Object then
         if Source_Kind = AML_Objects.String_Object then
            Converted := AML_Coercions.From_String (AML_Objects.Byte_Data (A.Tree.Values, Source), Width);
         elsif Source_Kind = AML_Objects.Buffer_Object then
            Converted := AML_Coercions.From_Buffer (AML_Objects.Byte_Data (A.Tree.Values, Source), Width);
         else Converted := (Status => AML_Coercions.Converted, Value => Number);
         end if;
         if Converted.Status = AML_Coercions.Empty_Buffer then
            Status := AML_Execute.Empty_Buffer; return;
         elsif Converted.Status /= AML_Coercions.Converted then return; end if;
         Set_Integer (A.Tree, Node, Converted.Value);
         Status := AML_Execute.Returned; return;
      end if;
      if Source_Kind /= AML_Objects.Integer_Object then Count := AML_Objects.Length (A.Tree.Values, Source); end if;
      if Target_Kind = AML_Objects.String_Object then
         if Source_Kind = AML_Objects.Integer_Object then Count := Octets * Hex_Per_Byte;
         elsif Source_Kind = AML_Objects.Buffer_Object and then Count > 0 then
            if Count > (AML_Objects.Max_Bytes + 1) / Prefixed_Hex_Width then Status := AML_Execute.Value_Limit; return; end if;
            Count := Count * Prefixed_Hex_Width - 1;
         end if;
      else
         if Source_Kind = AML_Objects.Integer_Object then Count := Octets;
         elsif Source_Kind = AML_Objects.String_Object then
            if Count = AML_Objects.Max_Bytes then Status := AML_Execute.Value_Limit; return; end if;
            Count := Count + 1;
         end if;
      end if;
      -- Reject conversion extents before allocating a bounded local scratch.
      if Count > AML_Objects.Max_Bytes then Status := AML_Execute.Value_Limit; return; end if;
      if (Target_Kind = AML_Objects.String_Object
          or else AML_Objects.Length (A.Tree.Values, Target) = 0)
        and then Count > AML_Objects.Max_Bytes - AML_Objects.Byte_Count (A.Tree.Values)
      then Status := AML_Execute.Value_Limit; return; end if;
      declare
         Candidate : AML_Objects.State := A.Tree.Values;
         Data : AML_Decode.Bytes (1 .. Count) := [others => 0];
         B : AML_Decode.Byte;
         N : AML_Decode.Integer_Value := Number;
      begin
         if Target_Kind = AML_Objects.String_Object then
            if Source_Kind = AML_Objects.Integer_Object then
               for I in reverse Data'Range loop Data (I) := Hex (AML_Decode.Byte (N mod 16)); N := N / 16; end loop;
            elsif Source_Kind = AML_Objects.String_Object then
               for I in Data'Range loop Data (I) := AML_Objects.Stored_Byte (A.Tree.Values, Source, I - 1); end loop;
            else
               for I in 1 .. AML_Objects.Length (A.Tree.Values, Source) loop
                  B := AML_Objects.Stored_Byte (A.Tree.Values, Source, I - 1);
                  Data ((I - 1) * Prefixed_Hex_Width + 1) := Character'Pos ('0');
                  Data ((I - 1) * Prefixed_Hex_Width + 2) := Character'Pos ('x');
                  Data ((I - 1) * Prefixed_Hex_Width + 3) := Hex (B / 16);
                  Data ((I - 1) * Prefixed_Hex_Width + 4) := Hex (B mod 16);
                  if I < AML_Objects.Length (A.Tree.Values, Source) then Data (I * Prefixed_Hex_Width) := Character'Pos (' '); end if;
               end loop;
            end if;
         elsif Source_Kind = AML_Objects.Integer_Object then
            for I in Data'Range loop Data (I) := AML_Coercions.Octet (Number, I - 1); end loop;
         else
            for I in 1 .. AML_Objects.Length (A.Tree.Values, Source) loop Data (I) := AML_Objects.Stored_Byte (A.Tree.Values, Source, I - 1); end loop;
         end if;
         if Target_Kind = AML_Objects.String_Object then
            AML_Objects.Replace_String (Candidate, Target, Data, String_Status);
            if String_Status /= AML_Objects.String_Updated then Status := AML_Execute.Value_Limit; return; end if;
         else
            AML_Objects.Store_Buffer (Candidate, Target, Data, Buffer_Status);
            if Buffer_Status /= AML_Objects.Buffer_Updated then Status := AML_Execute.Value_Limit; return; end if;
         end if;
         A.Tree.Values := Candidate;
      end;
      Status := AML_Execute.Returned;
   end Store_Named_Direct;

   procedure Store_Reference_Value
     (A : in out Arena; R : Reference; Width : AML_Decode.Integer_Width;
      Item : AML_Execute.Datum; Status : out AML_Execute.Execution_Status;
      Mode : AML_Execute.Reference_Store_Mode := AML_Execute.Direct_Target)
   is
      Candidate : AML_Objects.State;
      ID : AML_Objects.Object_ID;
      Allocated : AML_Objects.Allocation_Status;
      Stored : AML_Objects.Package_References.Result_Status;
      Witness : AML_Objects.Copies.Copy_Witness;
   begin
      Status := AML_Execute.Unsupported_Value;
      if not Matches (A, R) then return; end if;
      if AML_References.Kind (R) = AML_References.Named_Cell then
         declare
            Node : constant Node_ID := Node_ID (AML_References.Named_Node (R));
            Path : constant AML_Names.Name_Result :=
              (Kind => AML_Names.Accepted, Rooted => False, Parents => 0, Count => 1,
               Parts => [1 => A.Tree.Items (Node).Part, others => "____"],
               Consumed => AML_Names.Segment'Length);
         begin
            if Mode = AML_Execute.Explicit_Result_Target then
               if A.Tree.Items (Node).Object_Type not in
                 Integer_Object | String_Object | Buffer_Object | Uninitialized_Name_Object
               then return; end if;
               if Item.Value_Kind /= AML_Execute.Integer_Datum then
                  if Item.Value_Kind /= AML_Execute.Object_Datum
                    or else not Has_Source (A, Item.Object.Source)
                    or else Item.Object.ID /= AML_References.Source (Item.Object.Source)
                    or else AML_Objects.Kind (A.Tree.Values, Item.Object.ID) not in AML_Objects.Buffer_Object | AML_Objects.String_Object
                  then return; end if;
               end if;
            end if;
            if Mode = AML_Execute.Direct_Target
              and then A.Tree.Items (Node).Object_Type in Integer_Object | String_Object | Buffer_Object
            then
               Store_Named_Direct (A, Node, Width, Item, Status);
            elsif Mode = AML_Execute.Explicit_Result_Target
              and then Item.Value_Kind = AML_Execute.Object_Datum
              and then ((A.Tree.Items (Node).Object_Type = Buffer_Object
                and then AML_Objects.Kind (A.Tree.Values, Item.Object.ID) = AML_Objects.Buffer_Object)
                or else (A.Tree.Items (Node).Object_Type = String_Object
                  and then AML_Objects.Kind (A.Tree.Values, Item.Object.ID) = AML_Objects.String_Object))
            then
               Store_Named_Direct (A, Node, Width, Item, Status);
            elsif Mode = AML_Execute.Explicit_Result_Target
              and then A.Tree.Items (Node).Object_Type = Integer_Object
              and then Item.Value_Kind = AML_Execute.Integer_Datum
            then
               -- exstoren: matching integer types copy only Value, retaining
               -- the destination object. Differing explicit types attach anew.
               Set_Integer (A.Tree, Node, AML_Integers.Normalize (Item.Number, Width));
               Status := AML_Execute.Returned;
            elsif Mode = AML_Execute.Explicit_Result_Target
              and then Item.Value_Kind = AML_Execute.Object_Datum
            then
               -- exstoren: differing fixed-target primitive types attach the
               -- authenticated source object, preserving result/target aliases.
               -- Matching types were copied above; Arg/CopyObject stay separate.
               A.Tree.Items (Node) := (A.Tree.Items (Node) with delta
                 Object_Type => (if AML_Objects.Kind (A.Tree.Values, Item.Object.ID) = AML_Objects.String_Object
                   then String_Object else Buffer_Object),
                 Object_Ref => Item.Object.ID);
               Status := AML_Execute.Returned;
            else
               Replace_Value (A, Natural (A.Tree.Items (Node).Up), Path, Width, Item, Status);
            end if;
         end;
      elsif AML_References.Kind (R) = AML_References.Byte_Slot then
         if Item.Value_Kind = AML_Execute.Integer_Datum then
            Store_Integer (A, R, AML_Integers.Normalize (Item.Number, Width), Status);
         elsif Item.Value_Kind = AML_Execute.Object_Datum then
            if not Has_Source (A, Item.Object.Source)
              or else Item.Object.ID /= AML_References.Source (Item.Object.Source)
            then return; end if;
            declare
               Source_ID : constant AML_Objects.Object_ID := AML_References.Source (Item.Object.Source);
            begin
               if AML_Objects.Kind (A.Tree.Values, Source_ID) not in AML_Objects.String_Object | AML_Objects.Buffer_Object then return; end if;
               if AML_Objects.Length (A.Tree.Values, Source_ID) = 0 then
                  if AML_Objects.Kind (A.Tree.Values, Source_ID) = AML_Objects.Buffer_Object then
                     Status := AML_Execute.Empty_Buffer; return;
                  end if;
                  Store_Integer (A, R, 0, Status);
               else
                  Store_Integer (A, R, AML_Decode.Integer_Value
                    (AML_Objects.Stored_Byte (A.Tree.Values, Source_ID, 0)), Status);
               end if;
            end;
         end if;
      elsif AML_References.Kind (R) = AML_References.Package_Slot then
         Candidate := A.Tree.Values;
         case Item.Value_Kind is
            when AML_Execute.Integer_Datum =>
               AML_Objects.New_Integer (Candidate, AML_Integers.Normalize (Item.Number, Width), ID, Allocated, Item.Origin);
            when AML_Execute.Reference_Datum =>
               if not AML_References.Well_Formed (Item.Ref) then return; end if;
               AML_Objects.New_Reference (Candidate, Item.Ref, ID, Allocated);
            when AML_Execute.Object_Datum =>
               if not Has_Source (A, Item.Object.Source)
                 or else Item.Object.ID /= AML_References.Source (Item.Object.Source)
               then return; end if;
               AML_Objects.Copies.Clone
                 (Candidate, AML_References.Source (Item.Object.Source), ID, Allocated, Witness);
         end case;
         if Allocated /= AML_Objects.Allocated then Status := AML_Execute.Value_Limit; return; end if;
         AML_Objects.Package_References.Write (Candidate, AML_References.Package_Item (R), ID, Stored);
         if Stored /= AML_Objects.Package_References.Ready then return; end if;
         A.Tree.Values := Candidate; Status := AML_Execute.Returned;
      end if;
   end Store_Reference_Value;
   procedure Copy_And_Attach
     (A : in out Arena; Destination : AML_Execute.Copy_Destination;
      Width : AML_Decode.Integer_Width; Item : AML_Execute.Datum;
      Copy : out AML_Execute.Datum; Status : out AML_Execute.Execution_Status)
   is
      Located : Lookup_Result;
      Node : Node_ID;
      Prior_Values : AML_Objects.State;
      Prior_Node : Entry_Record;
   begin
      Copy := (Value_Kind => AML_Execute.Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
      Status := AML_Execute.Unsupported_Value;
      if not Initialized (A) then return; end if;
      case Destination.Kind is
         when AML_Execute.Named_Destination =>
            if Destination.Path.Kind /= AML_Names.Accepted or else Destination.Path.Count = 0 then
               Status := AML_Execute.Bad_Name; return;
            end if;
            if Destination.Scope > A.Tree.Used then Status := AML_Execute.Unknown_Name; return; end if;
            Located := Resolve (A.Tree, Node_ID (Destination.Scope), Destination.Path);
            if Located.Status /= Found then Status := AML_Execute.Unknown_Name; return; end if;
            Node := Located.Node;
         when AML_Execute.Referenced_Destination =>
            if AML_References.Kind (Destination.Ref) /= AML_References.Named_Cell
              or else not Matches (A, Destination.Ref)
            then return; end if;
            Node := Node_ID (AML_References.Named_Node (Destination.Ref));
      end case;
      if A.Tree.Items (Node).Object_Type not in
        Integer_Object | String_Object | Buffer_Object | Package_Object | Reference_Object
        and then not (A.Tree.Items (Node).Object_Type = Uninitialized_Name_Object
          and then A.Tree.Items (Node).Alive and then A.Tree.Items (Node).Initializing)
      then return; end if;
      -- Operands have already executed. Only provisional value allocations lie
      -- inside this transaction; invocation and namespace issuers never rewind.
      Prior_Values := A.Tree.Values; Prior_Node := A.Tree.Items (Node);
      Clone_Value (A, Width, Item, Copy, Status);
      if Status = AML_Execute.Returned then
         case Destination.Kind is
            when AML_Execute.Named_Destination =>
               Replace_Value (A, Destination.Scope, Destination.Path, Width, Copy, Status);
            when AML_Execute.Referenced_Destination =>
               Store_Reference_Value (A, Destination.Ref, Width, Copy, Status, AML_Execute.Argument_Indirect_Target);
         end case;
      end if;
      if Status /= AML_Execute.Returned then
         pragma Assert (A.Tree.Items (Node) = Prior_Node);
         A.Tree.Values := Prior_Values;
         Copy := (Value_Kind => AML_Execute.Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
      end if;
   end Copy_And_Attach;
   procedure Concatenate_Resources_And_Attach
     (A : in out Arena; Width : AML_Decode.Integer_Width;
      Left, Right : AML_Execute.Datum;
      Destination : AML_Execute.Concatenation_Destination;
      Result_Value, Cell_Value : out AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
   is
      use AML_Execute;
      use type AML_Decode.Integer_Width;
      package Templates is new AML_Resource_Templates (AML_Objects.Max_Bytes);
      use type Templates.Build_Status;
      -- Conversion views include a String terminator; result capacity does not.
      pragma Compile_Time_Error (AML_Objects.Max_Bytes >= Positive'Last,
        "resource conversion view requires one spare index");
      subtype View_Length is Natural range 0 .. AML_Objects.Max_Bytes + 1;
      Node : Node_ID := 0;
      Target_Reference : Reference := AML_References.No_Reference;
      Prior_Values : AML_Objects.State;
      Prior_Node : Entry_Record;
      ID : AML_Objects.Object_ID := 0;
      Allocated : AML_Objects.Allocation_Status;
      Located : Lookup_Result;
      Accepted : Boolean;
      function Primitive (Item : Datum) return Boolean is
        (Item.Value_Kind = Integer_Datum
         or else (Item.Value_Kind = Object_Datum
           and then Has_Source (A, Item.Object.Source)
           and then Item.Object.ID = AML_References.Source (Item.Object.Source)
           and then AML_Objects.Kind (A.Tree.Values, Item.Object.ID) in
             AML_Objects.String_Object | AML_Objects.Buffer_Object));
      function Converted (Item : Datum) return AML_Decode.Bytes
        with Pre => Primitive (Item)
      is
         Length : constant View_Length :=
           (if Item.Value_Kind = Integer_Datum then
              (if Width = AML_Decode.Bits_32 then 4 else 8)
            else AML_Objects.Length (A.Tree.Values, Item.Object.ID) +
              (if AML_Objects.Kind (A.Tree.Values, Item.Object.ID) = AML_Objects.String_Object then 1 else 0));
         Data : AML_Decode.Bytes (1 .. Length) := [others => 0];
      begin
         if Item.Value_Kind = Integer_Datum then
            for I in Data'Range loop
               Data (I) := AML_Coercions.Octet (AML_Integers.Normalize (Item.Number, Width), I - 1);
            end loop;
         else
            for I in 1 .. AML_Objects.Length (A.Tree.Values, Item.Object.ID) loop
               Data (I) := AML_Objects.Stored_Byte (A.Tree.Values, Item.Object.ID, I - 1);
            end loop;
         end if;
         return Data;
      end Converted;
   begin
      Result_Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
      Cell_Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
      Status := Unsupported_Value;
      -- Both source admissions precede resource parsing; no cached metadata.
      if not Primitive (Right) or else not Primitive (Left) then return; end if;
      case Destination.Kind is
         when Detached_Result | Prepared_Cell_Copy => null;
         when Named_Attachment =>
            if Destination.Path.Kind /= AML_Names.Accepted or else Destination.Path.Count = 0 then Status := Bad_Name; return; end if;
            if Destination.Scope > A.Tree.Used then Status := Unknown_Name; return; end if;
            Located := Resolve (A.Tree, Node_ID (Destination.Scope), Destination.Path);
            if Located.Status /= Found then Status := Unknown_Name; return; end if;
            Node := Located.Node;
            Make_Named_Reference (A, Natural (Node), Target_Reference, Accepted);
            if not Accepted then return; end if;
         when Reference_Attachment =>
            if not Matches (A, Destination.Ref) then return; end if;
            Target_Reference := Destination.Ref;
            if AML_References.Kind (Destination.Ref) = AML_References.Named_Cell then
               Node := Node_ID (AML_References.Named_Node (Destination.Ref));
            end if;
      end case;
      declare
         Right_View : constant AML_Decode.Bytes := Converted (Right);
         Left_View : constant AML_Decode.Bytes := Converted (Left);
         Built : constant Templates.Build_Result := Templates.Build (Left_View, Right_View);
      begin
         case Built.Status is
            when Templates.Invalid_Resource_Type => Status := Invalid_Resource_Type;
            when Templates.Bad_Resource_Length => Status := Bad_Resource_Length;
            when Templates.Buffer_Length => Status := Resource_Buffer_Length;
            when Templates.No_End_Tag => Status := No_Resource_End_Tag;
            when Templates.Output_Limit => Status := Value_Limit;
            when Templates.Built =>
               Prior_Values := A.Tree.Values;
               if Node /= 0 then Prior_Node := A.Tree.Items (Node); end if;
               AML_Objects.New_Bytes (A.Tree.Values, AML_Objects.Buffer_Object,
                 Built.Data (1 .. Built.Length), ID, Allocated);
               if Allocated /= AML_Objects.Allocated then Status := Value_Limit; return; end if;
               Read_Source (A, AML_References.Bind_Object (A.Token,
                 AML_Objects.Address_Of (A.Tree.Values, ID)), Result_Value, Status);
               if Status = Returned then
                  case Destination.Kind is
                     when Detached_Result => null;
                     when Prepared_Cell_Copy => Cell_Value := Result_Value;
                     when Named_Attachment =>
                        Store_Reference_Value (A, Target_Reference, Width, Result_Value, Status, Direct_Target);
                     when Reference_Attachment =>
                        Store_Reference_Value (A, Target_Reference, Width, Result_Value, Status, Destination.Mode);
                  end case;
               end if;
               if Status /= Returned then
                  pragma Assert (Node = 0 or else A.Tree.Items (Node) = Prior_Node);
                  A.Tree.Values := Prior_Values;
                  Result_Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
                  Cell_Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
               end if;
         end case;
      end;
   end Concatenate_Resources_And_Attach;

   procedure Concatenate_And_Attach
     (A : in out Arena; Width : AML_Decode.Integer_Width;
      Left, Right : AML_Execute.Concatenation_Operand;
      Destination : AML_Execute.Concatenation_Destination;
      Result_Value, Cell_Value : out AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
   is
      use AML_Execute;
      package Builder is new AML_Concatenation (AML_Objects.Max_Bytes);
      use type Builder.Build_Status;
      use type Builder.Output_Kind;
      Left_Kind, Right_Kind : Builder.Input_Kind := Builder.Integer_Input;
      Left_Number, Right_Number : AML_Decode.Integer_Value := 0;
      Left_ID, Right_ID : AML_Objects.Object_ID := 0;
      type Descriptor_Kind is (No_Descriptor, Package_Descriptor, Reference_Descriptor, Device_Text, Region_Text, Event_Text, Mutex_Text, Power_Text, Processor_Text, Thermal_Text);
      Left_Descriptor, Right_Descriptor : Descriptor_Kind := No_Descriptor;
      Node : Node_ID := 0;
      Target_Reference : Reference := AML_References.No_Reference;
      Prior_Values : AML_Objects.State;
      Prior_Node : Entry_Record;
      ID : AML_Objects.Object_ID;
      Allocated : AML_Objects.Allocation_Status;
      Located : Lookup_Result;
      procedure Classify
        (Operand : Concatenation_Operand; Kind : out Builder.Input_Kind;
         Number : out AML_Decode.Integer_Value; Object_ID : out AML_Objects.Object_ID;
         Descriptor : out Descriptor_Kind; Accepted : out Boolean)
      is
         Metadata : Reference_Metadata;
      begin
         Kind := Builder.Integer_Input; Number := 0; Object_ID := 0;
         Descriptor := No_Descriptor; Accepted := False;
         if Operand.Kind = Namespace_Operand then
            Describe_Named_Identity (A, Operand.Identity, Metadata);
            if Metadata.Kind /= Metadata_Only then return; end if;
            if Operand.Descriptor = Device_Descriptor and then Metadata.Object_Type = Device_Metadata then
               Descriptor := Device_Text;
            elsif Operand.Descriptor = Region_Descriptor and then Metadata.Object_Type = Region_Metadata then
               Descriptor := Region_Text;
            elsif Operand.Descriptor = Event_Descriptor and then Metadata.Object_Type = Event_Metadata then
               Descriptor := Event_Text;
            elsif Operand.Descriptor = Mutex_Descriptor and then Metadata.Object_Type = AML_Execute.Mutex_Metadata then
               Descriptor := Mutex_Text;
            elsif Operand.Descriptor = Power_Descriptor and then Metadata.Object_Type = Power_Metadata then
               Descriptor := Power_Text;
            elsif Operand.Descriptor = Processor_Descriptor and then Metadata.Object_Type = Processor_Metadata then
               Descriptor := Processor_Text;
            elsif Operand.Descriptor = Thermal_Descriptor and then Metadata.Object_Type = Thermal_Metadata then
               Descriptor := Thermal_Text;
            else return; end if;
            Kind := Builder.String_Input;
         else
            case Operand.Value.Value_Kind is
               when Integer_Datum => Number := Operand.Value.Number;
               when Reference_Datum =>
                  if not AML_References.Well_Formed (Operand.Value.Ref) then return; end if;
                  if AML_References.Kind (Operand.Value.Ref) = AML_References.Frame_Cell then
                     if not AML_Frame_Handles.Belongs_To
                       (AML_References.Frame_Item (Operand.Value.Ref),
                        AML_Frame_Handles.Bind_Domain (A.Token, A.Invocation_Issued))
                     then return; end if;
                  elsif not AML_References.Belongs_To (Operand.Value.Ref, A.Token) then return;
                  end if;
                  case AML_References.Kind (Operand.Value.Ref) is
                     when AML_References.Named_Cell =>
                        if not Named_Identity_Matches (A, Operand.Value.Ref) then return; end if;
                     when AML_References.Byte_Slot | AML_References.Package_Slot =>
                        if not Matches (A, Operand.Value.Ref) then return; end if;
                     when AML_References.Name_Member =>
                        if not Name_Member_Matches (A, Operand.Value.Ref) then return; end if;
                     when AML_References.Frame_Cell => null;
                     when AML_References.Absent => return;
                  end case;
                  -- Frame liveness belongs to the executor registry. No
                  -- referent is read while rendering this descriptor.
                  Descriptor := Reference_Descriptor; Kind := Builder.String_Input;
               when Object_Datum =>
                  if not Has_Source (A, Operand.Value.Object.Source)
                    or else Operand.Value.Object.ID /= AML_References.Source (Operand.Value.Object.Source)
                  then return; end if;
                  Object_ID := AML_References.Source (Operand.Value.Object.Source);
                  case AML_Objects.Kind (A.Tree.Values, Object_ID) is
                     when AML_Objects.String_Object => Kind := Builder.String_Input;
                     when AML_Objects.Buffer_Object => Kind := Builder.Buffer_Input;
                     when AML_Objects.Package_Object => Descriptor := Package_Descriptor; Kind := Builder.String_Input;
                     when others => return;
                  end case;
            end case;
         end if;
         Accepted := True;
      end Classify;
      function Text_Bytes (Text : String) return AML_Decode.Bytes is
         Data : AML_Decode.Bytes (1 .. Text'Length);
      begin
         for I in Text'Range loop Data (I - Text'First + 1) := AML_Decode.Byte (Character'Pos (Text (I))); end loop;
         return Data;
      end Text_Bytes;
      function Data (Object_ID : AML_Objects.Object_ID; Descriptor : Descriptor_Kind) return AML_Decode.Bytes is
      begin
         case Descriptor is
            when Package_Descriptor => return Text_Bytes ("[Package Object]");
            when Reference_Descriptor => return Text_Bytes ("[Reference Object]");
            when Device_Text => return Text_Bytes ("[Device Object]");
            when Region_Text => return Text_Bytes ("[Region Object]");
            when Event_Text => return Text_Bytes ("[Event Object]");
            when Mutex_Text => return Text_Bytes ("[Mutex Object]");
            when Power_Text => return Text_Bytes ("[Power Object]");
            when Processor_Text => return Text_Bytes ("[Processor Object]");
            when Thermal_Text => return Text_Bytes ("[Thermal Object]");
            when No_Descriptor =>
               if Object_ID = 0 then return AML_Decode.Bytes'(1 .. 0 => 0); end if;
               return AML_Objects.Byte_Data (A.Tree.Values, Object_ID);
         end case;
      end Data;
      Accepted : Boolean;
   begin
      Result_Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
      Cell_Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
      Status := Unsupported_Value;
      if not Initialized (A) then return; end if;
      Classify (Left, Left_Kind, Left_Number, Left_ID, Left_Descriptor, Accepted);
      if not Accepted then return; end if;
      Classify (Right, Right_Kind, Right_Number, Right_ID, Right_Descriptor, Accepted);
      if not Accepted then return; end if;
      case Destination.Kind is
         when Detached_Result | Prepared_Cell_Copy => null;
         when Named_Attachment =>
            if Destination.Path.Kind /= AML_Names.Accepted or else Destination.Path.Count = 0 then Status := Bad_Name; return; end if;
            if Destination.Scope > A.Tree.Used then Status := Unknown_Name; return; end if;
            Located := Resolve (A.Tree, Node_ID (Destination.Scope), Destination.Path);
            if Located.Status /= Found then Status := Unknown_Name; return; end if;
            Node := Located.Node;
            Make_Named_Reference (A, Natural (Node), Target_Reference, Accepted);
            if not Accepted then return; end if;
         when Reference_Attachment =>
            if not Matches (A, Destination.Ref) then return; end if;
            Target_Reference := Destination.Ref;
            if AML_References.Kind (Destination.Ref) = AML_References.Named_Cell then
               Node := Node_ID (AML_References.Named_Node (Destination.Ref));
            end if;
      end case;
      Prior_Values := A.Tree.Values;
      if Node /= 0 then Prior_Node := A.Tree.Items (Node); end if;
      declare
         Built : constant Builder.Result := Builder.Build
           (Width, Left_Kind, Left_Number, Data (Left_ID, Left_Descriptor),
            Right_Kind, Right_Number, Data (Right_ID, Right_Descriptor));
      begin
         if Built.Status = Builder.Empty_Buffer then Status := Empty_Buffer; return; end if;
         if Built.Status = Builder.Length_Limit then Status := Value_Limit; return; end if;
         AML_Objects.New_Bytes
           (A.Tree.Values,
            (if Built.Kind = Builder.String_Output then AML_Objects.String_Object else AML_Objects.Buffer_Object),
            Built.Data (1 .. Built.Length), ID, Allocated);
      end;
      if Allocated /= AML_Objects.Allocated then Status := Value_Limit; return; end if;
      Read_Source (A, AML_References.Bind_Object (A.Token, AML_Objects.Address_Of (A.Tree.Values, ID)), Result_Value, Status);
      if Status = Returned then
         case Destination.Kind is
            when Detached_Result => null;
            when Prepared_Cell_Copy => Clone_Value (A, Width, Result_Value, Cell_Value, Status);
            when Named_Attachment => Store_Reference_Value (A, Target_Reference, Width, Result_Value, Status, Direct_Target);
            when Reference_Attachment => Store_Reference_Value (A, Target_Reference, Width, Result_Value, Status, Destination.Mode);
         end case;
      end if;
      if Status /= Returned then
         pragma Assert (Node = 0 or else A.Tree.Items (Node) = Prior_Node);
         A.Tree.Values := Prior_Values;
         Result_Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Cell_Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
      end if;
   end Concatenate_And_Attach;
   procedure To_Buffer_And_Attach
     (A : in out Arena; Width : AML_Decode.Integer_Width;
      Item : AML_Execute.Datum;
      Destination : AML_Execute.Concatenation_Destination;
      Result_Value, Cell_Value : out AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
   is
      use AML_Execute;
      use type AML_Decode.Integer_Width;
      Source_ID : AML_Objects.Object_ID := 0;
      Existing_Buffer : Boolean := False;
      Size : Natural := 0;
      Node : Node_ID := 0;
      Target_Reference : Reference := AML_References.No_Reference;
      Prior_Values : AML_Objects.State;
      Prior_Node : Entry_Record;
      ID : AML_Objects.Object_ID := 0;
      Allocated : AML_Objects.Allocation_Status;
      Located : Lookup_Result;
      Accepted : Boolean;
   begin
      Result_Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
      Cell_Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
      Status := Unsupported_Value;
      if not Initialized (A) then return; end if;
      case Item.Value_Kind is
         when Reference_Datum => return;
         when Integer_Datum => Size := (if Width = AML_Decode.Bits_32 then 4 else 8);
         when Object_Datum =>
            if not Has_Source (A, Item.Object.Source)
              or else Item.Object.ID /= AML_References.Source (Item.Object.Source)
            then return; end if;
            Source_ID := AML_References.Source (Item.Object.Source);
            case AML_Objects.Kind (A.Tree.Values, Source_ID) is
               when AML_Objects.Buffer_Object => Existing_Buffer := True;
               when AML_Objects.String_Object => Size := AML_Objects.Length (A.Tree.Values, Source_ID) + 1;
               when others => return;
            end case;
      end case;
      case Destination.Kind is
         when Detached_Result | Prepared_Cell_Copy => null;
         when Named_Attachment =>
            if Destination.Path.Kind /= AML_Names.Accepted or else Destination.Path.Count = 0 then Status := Bad_Name; return; end if;
            if Destination.Scope > A.Tree.Used then Status := Unknown_Name; return; end if;
            Located := Resolve (A.Tree, Node_ID (Destination.Scope), Destination.Path);
            if Located.Status /= Found then Status := Unknown_Name; return; end if;
            Node := Located.Node;
            Make_Named_Reference (A, Natural (Node), Target_Reference, Accepted);
            if not Accepted then return; end if;
         when Reference_Attachment =>
            if not Matches (A, Destination.Ref) then return; end if;
            Target_Reference := Destination.Ref;
            if AML_References.Kind (Destination.Ref) = AML_References.Named_Cell then
               Node := Node_ID (AML_References.Named_Node (Destination.Ref));
            end if;
      end case;
      Prior_Values := A.Tree.Values;
      if Node /= 0 then Prior_Node := A.Tree.Items (Node); end if;
      if Existing_Buffer then
         ID := Source_ID;
      else
         if Size > AML_Objects.Max_Bytes then Status := Value_Limit; return; end if;
         declare
            Data : AML_Decode.Bytes (1 .. Size) := [others => 0];
         begin
            if Item.Value_Kind = Integer_Datum then
               for I in Data'Range loop Data (I) := AML_Coercions.Octet (Item.Number, I - 1); end loop;
            else
               for I in 1 .. Size - 1 loop
                  Data (I) := AML_Objects.Stored_Byte (A.Tree.Values, Source_ID, I - 1);
               end loop;
            end if;
            AML_Objects.New_Bytes (A.Tree.Values, AML_Objects.Buffer_Object, Data, ID, Allocated);
         end;
         if Allocated /= AML_Objects.Allocated then Status := Value_Limit; return; end if;
      end if;
      Read_Source (A, AML_References.Bind_Object (A.Token, AML_Objects.Address_Of (A.Tree.Values, ID)), Result_Value, Status);
      if Status = Returned then
         case Destination.Kind is
            when Detached_Result => null;
            when Prepared_Cell_Copy =>
               if Existing_Buffer then Clone_Value (A, Width, Result_Value, Cell_Value, Status);
               else Cell_Value := Result_Value; end if;
            when Named_Attachment =>
               Store_Reference_Value (A, Target_Reference, Width, Result_Value, Status, Explicit_Result_Target);
            when Reference_Attachment =>
               Store_Reference_Value (A, Target_Reference, Width, Result_Value, Status, Destination.Mode);
         end case;
      end if;
      if Status /= Returned then
         pragma Assert (Node = 0 or else A.Tree.Items (Node) = Prior_Node);
         A.Tree.Values := Prior_Values;
         Result_Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Cell_Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
      end if;
   end To_Buffer_And_Attach;
   procedure Mid_And_Attach
     (A : in out Arena; Width : AML_Decode.Integer_Width;
      Item : AML_Execute.Datum; Start, Count : AML_Decode.Integer_Value;
      Destination : AML_Execute.Concatenation_Destination;
      Result_Value, Cell_Value : out AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
   is
      use AML_Execute;
      use type AML_Decode.Integer_Width;
      Source_ID : AML_Objects.Object_ID := 0;
      Source_Kind : AML_Objects.Object_Kind := AML_Objects.Buffer_Object;
      Source_Length : AML_Slices.Extent;
      Selected : AML_Slices.Slice_Range;
      Node : Node_ID := 0;
      Target_Reference : Reference := AML_References.No_Reference;
      Prior_Values : AML_Objects.State;
      Prior_Node : Entry_Record;
      ID : AML_Objects.Object_ID;
      Allocated : AML_Objects.Allocation_Status;
      Located : Lookup_Result;
      Accepted : Boolean;
   begin
      Result_Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
      Cell_Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
      Status := Unsupported_Value;
      if not Initialized (A)
        or else Start /= AML_Integers.Normalize (Start, Width)
        or else Count /= AML_Integers.Normalize (Count, Width)
      then return; end if;
      case Item.Value_Kind is
         when Reference_Datum => return;
         when Integer_Datum =>
            Source_Length := (if Width = AML_Decode.Bits_32 then 4 else 8);
         when Object_Datum =>
            if not Has_Source (A, Item.Object.Source)
              or else Item.Object.ID /= AML_References.Source (Item.Object.Source)
            then return; end if;
            Source_ID := AML_References.Source (Item.Object.Source);
            Source_Kind := AML_Objects.Kind (A.Tree.Values, Source_ID);
            if Source_Kind not in AML_Objects.String_Object | AML_Objects.Buffer_Object then return; end if;
            Source_Length := AML_Objects.Length (A.Tree.Values, Source_ID);
      end case;
      if Destination.Kind = Reference_Attachment and then Destination.Mode = Explicit_Result_Target then return; end if;
      Selected := AML_Slices.Select_Range (Source_Length, Start, Count);
      case Destination.Kind is
         when Detached_Result | Prepared_Cell_Copy => null;
         when Named_Attachment =>
            if Destination.Path.Kind /= AML_Names.Accepted or else Destination.Path.Count = 0 then Status := Bad_Name; return; end if;
            if Destination.Scope > A.Tree.Used then Status := Unknown_Name; return; end if;
            Located := Resolve (A.Tree, Node_ID (Destination.Scope), Destination.Path);
            if Located.Status /= Found then Status := Unknown_Name; return; end if;
            Node := Located.Node;
            Make_Named_Reference (A, Natural (Node), Target_Reference, Accepted);
            if not Accepted then return; end if;
         when Reference_Attachment =>
            if not Matches (A, Destination.Ref) then return; end if;
            Target_Reference := Destination.Ref;
            if AML_References.Kind (Destination.Ref) = AML_References.Named_Cell then
               Node := Node_ID (AML_References.Named_Node (Destination.Ref));
            end if;
      end case;
      Prior_Values := A.Tree.Values;
      if Node /= 0 then Prior_Node := A.Tree.Items (Node); end if;
      declare
         Data : AML_Decode.Bytes (1 .. Selected.Length);
         Number : constant AML_Decode.Integer_Value :=
           (if Item.Value_Kind = Integer_Datum then AML_Integers.Normalize (Item.Number, Width) else 0);
      begin
         for I in Data'Range loop
            if Item.Value_Kind = Integer_Datum then
               Data (I) := AML_Coercions.Octet (Number, Selected.Offset + (I - 1));
            else
               Data (I) := AML_Objects.Stored_Byte (A.Tree.Values, Source_ID, Selected.Offset + (I - 1));
            end if;
         end loop;
         AML_Objects.New_Bytes (A.Tree.Values, Source_Kind, Data, ID, Allocated);
      end;
      if Allocated /= AML_Objects.Allocated then Status := Value_Limit; return; end if;
      Read_Source (A, AML_References.Bind_Object (A.Token, AML_Objects.Address_Of (A.Tree.Values, ID)), Result_Value, Status);
      if Status = Returned then
         case Destination.Kind is
            when Detached_Result => null;
            when Prepared_Cell_Copy =>
               Cell_Value := Result_Value;
            when Named_Attachment =>
               Store_Reference_Value (A, Target_Reference, Width, Result_Value, Status, Direct_Target);
            when Reference_Attachment =>
               Store_Reference_Value (A, Target_Reference, Width, Result_Value, Status, Destination.Mode);
         end case;
      end if;
      if Status /= Returned then
         pragma Assert (Node = 0 or else A.Tree.Items (Node) = Prior_Node);
         A.Tree.Values := Prior_Values;
         Result_Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Cell_Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
      end if;
   end Mid_And_Attach;
   procedure To_String_And_Attach
     (A : in out Arena; Width : AML_Decode.Integer_Width;
      Item : AML_Execute.Datum; Length : AML_Decode.Integer_Value;
      Destination : AML_Execute.Concatenation_Destination;
      Result_Value, Cell_Value : out AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
   is
      use AML_Execute;
      use type AML_Decode.Integer_Width;
      Source_ID : AML_Objects.Object_ID := 0;
      Source_Kind : AML_Objects.Object_Kind := AML_Objects.Buffer_Object;
      Source_Length : AML_Slices.Extent;
      Selected_Length : AML_Slices.Extent := 0;
      Node : Node_ID := 0;
      Target_Reference : Reference := AML_References.No_Reference;
      Prior_Values : AML_Objects.State;
      Prior_Node : Entry_Record;
      ID : AML_Objects.Object_ID;
      Allocated : AML_Objects.Allocation_Status;
      Located : Lookup_Result;
      Accepted : Boolean;
      function Source_Byte (Offset : Natural) return AML_Decode.Byte is
        (if Item.Value_Kind = Integer_Datum then
           AML_Coercions.Octet (AML_Integers.Normalize (Item.Number, Width), Offset)
         else AML_Objects.Stored_Byte (A.Tree.Values, Source_ID, Offset));
   begin
      Result_Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
      Cell_Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
      Status := Unsupported_Value;
      if not Initialized (A)
        or else Length /= AML_Integers.Normalize (Length, Width)
      then return; end if;
      case Item.Value_Kind is
         when Reference_Datum => return;
         when Integer_Datum =>
            Source_Length := (if Width = AML_Decode.Bits_32 then 4 else 8);
         when Object_Datum =>
            if not Has_Source (A, Item.Object.Source)
              or else Item.Object.ID /= AML_References.Source (Item.Object.Source)
            then return; end if;
            Source_ID := AML_References.Source (Item.Object.Source);
            Source_Kind := AML_Objects.Kind (A.Tree.Values, Source_ID);
            if Source_Kind not in AML_Objects.String_Object | AML_Objects.Buffer_Object then return; end if;
            Source_Length := AML_Objects.Length (A.Tree.Values, Source_ID);
      end case;
      if Destination.Kind = Reference_Attachment and then Destination.Mode = Direct_Target then return; end if;
      -- String-to-Buffer appends NUL, which cannot extend the resulting
      -- String prefix. Scan canonical source bytes without an intermediate.
      while Selected_Length < Source_Length
        and then AML_Decode.Integer_Value (Selected_Length) < Length
        and then Source_Byte (Selected_Length) /= 0
      loop
         Selected_Length := Selected_Length + 1;
      end loop;
      case Destination.Kind is
         when Detached_Result | Prepared_Cell_Copy => null;
         when Named_Attachment =>
            if Destination.Path.Kind /= AML_Names.Accepted or else Destination.Path.Count = 0 then Status := Bad_Name; return; end if;
            if Destination.Scope > A.Tree.Used then Status := Unknown_Name; return; end if;
            Located := Resolve (A.Tree, Node_ID (Destination.Scope), Destination.Path);
            if Located.Status /= Found then Status := Unknown_Name; return; end if;
            Node := Located.Node;
            Make_Named_Reference (A, Natural (Node), Target_Reference, Accepted);
            if not Accepted then return; end if;
         when Reference_Attachment =>
            if not Matches (A, Destination.Ref) then return; end if;
            Target_Reference := Destination.Ref;
            if AML_References.Kind (Destination.Ref) = AML_References.Named_Cell then
               Node := Node_ID (AML_References.Named_Node (Destination.Ref));
            end if;
      end case;
      Prior_Values := A.Tree.Values;
      if Node /= 0 then Prior_Node := A.Tree.Items (Node); end if;
      declare
         Data : AML_Decode.Bytes (1 .. Selected_Length);
      begin
         for I in Data'Range loop Data (I) := Source_Byte (I - 1); end loop;
         AML_Objects.New_Bytes (A.Tree.Values, AML_Objects.String_Object, Data, ID, Allocated);
      end;
      if Allocated /= AML_Objects.Allocated then Status := Value_Limit; return; end if;
      Read_Source (A, AML_References.Bind_Object (A.Token, AML_Objects.Address_Of (A.Tree.Values, ID)), Result_Value, Status);
      if Status = Returned then
         case Destination.Kind is
            when Detached_Result => null;
            when Prepared_Cell_Copy =>
               Cell_Value := Result_Value;
            when Named_Attachment =>
               Store_Reference_Value (A, Target_Reference, Width, Result_Value, Status, Explicit_Result_Target);
            when Reference_Attachment =>
               Store_Reference_Value (A, Target_Reference, Width, Result_Value, Status, Destination.Mode);
         end case;
      end if;
      if Status /= Returned then
         pragma Assert (Node = 0 or else A.Tree.Items (Node) = Prior_Node);
         A.Tree.Values := Prior_Values;
         Result_Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Cell_Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
      end if;
   end To_String_And_Attach;
   procedure Format_String_And_Attach
     (A : in out Arena; Width : AML_Decode.Integer_Width;
      Mode : AML_Execute.Explicit_String_Mode; Item : AML_Execute.Datum;
      Destination : AML_Execute.Concatenation_Destination;
      Result_Value, Cell_Value : out AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
   is
      use AML_Execute;
      package Formatting is new AML_Explicit_Formatting (AML_Objects.Max_Bytes);
      use type Formatting.Build_Status;
      Selected_Mode : constant Formatting.Format_Mode :=
        (if Mode = Decimal_String then Formatting.Decimal_Format else Formatting.Hexadecimal_Format);
      Source_ID : AML_Objects.Object_ID := 0;
      Existing_String : Boolean := False;
      Node : Node_ID := 0;
      Target_Reference : Reference := AML_References.No_Reference;
      Prior_Values : AML_Objects.State;
      Prior_Node : Entry_Record;
      ID : AML_Objects.Object_ID := 0;
      Located : Lookup_Result;
      Accepted : Boolean;
      procedure Create (Formatted : Formatting.Result) is
         Allocated : AML_Objects.Allocation_Status;
      begin
         if Formatted.Status /= Formatting.Built then Status := Value_Limit; return; end if;
         AML_Objects.New_Bytes (A.Tree.Values, AML_Objects.String_Object, Formatted.Data, ID, Allocated);
         Status := (if Allocated = AML_Objects.Allocated then Returned else Value_Limit);
      end Create;
   begin
      Result_Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
      Cell_Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
      Status := Unsupported_Value;
      if not Initialized (A) then return; end if;
      case Item.Value_Kind is
         when Reference_Datum => return;
         when Integer_Datum => null;
         when Object_Datum =>
            if not Has_Source (A, Item.Object.Source)
              or else Item.Object.ID /= AML_References.Source (Item.Object.Source)
            then return; end if;
            Source_ID := AML_References.Source (Item.Object.Source);
            case AML_Objects.Kind (A.Tree.Values, Source_ID) is
               when AML_Objects.String_Object => Existing_String := True;
               when AML_Objects.Buffer_Object => null;
               when others => return;
            end case;
      end case;
      case Destination.Kind is
         when Detached_Result | Prepared_Cell_Copy => null;
         when Named_Attachment =>
            if Destination.Path.Kind /= AML_Names.Accepted or else Destination.Path.Count = 0 then Status := Bad_Name; return; end if;
            if Destination.Scope > A.Tree.Used then Status := Unknown_Name; return; end if;
            Located := Resolve (A.Tree, Node_ID (Destination.Scope), Destination.Path);
            if Located.Status /= Found then Status := Unknown_Name; return; end if;
            Node := Located.Node;
            Make_Named_Reference (A, Natural (Node), Target_Reference, Accepted);
            if not Accepted then return; end if;
         when Reference_Attachment =>
            if not Matches (A, Destination.Ref) then return; end if;
            Target_Reference := Destination.Ref;
            if AML_References.Kind (Destination.Ref) = AML_References.Named_Cell then
               Node := Node_ID (AML_References.Named_Node (Destination.Ref));
            end if;
      end case;
      Prior_Values := A.Tree.Values;
      if Node /= 0 then Prior_Node := A.Tree.Items (Node); end if;
      if Existing_String then
         ID := Source_ID;
      elsif Item.Value_Kind = Integer_Datum then
         Create (Formatting.From_Integer (Selected_Mode, Width, Item.Number));
         if Status /= Returned then return; end if;
      else
         Create (Formatting.From_Buffer (Selected_Mode, AML_Objects.Byte_Data (A.Tree.Values, Source_ID)));
         if Status /= Returned then return; end if;
      end if;
      Read_Source (A, AML_References.Bind_Object (A.Token, AML_Objects.Address_Of (A.Tree.Values, ID)), Result_Value, Status);
      if Status = Returned then
         case Destination.Kind is
            when Detached_Result => null;
            when Prepared_Cell_Copy =>
               if Existing_String then Clone_Value (A, Width, Result_Value, Cell_Value, Status);
               else Cell_Value := Result_Value; end if;
            when Named_Attachment =>
               Store_Reference_Value (A, Target_Reference, Width, Result_Value, Status, Explicit_Result_Target);
            when Reference_Attachment =>
               Store_Reference_Value (A, Target_Reference, Width, Result_Value, Status, Destination.Mode);
         end case;
      end if;
      if Status /= Returned then
         pragma Assert (Node = 0 or else A.Tree.Items (Node) = Prior_Node);
         A.Tree.Values := Prior_Values;
         Result_Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Cell_Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
      end if;
   end Format_String_And_Attach;
   function Invocation_Count (A : Arena) return AML_Frame_Handles.Invocation_Serial is
     (A.Invocation_Issued);
   procedure Begin_Invocation
     (A : in out Arena; Domain : out AML_Frame_Handles.Invocation_Domain;
      Status : out AML_Execute.Invocation_Status)
   is
   begin
      Domain := AML_Frame_Handles.No_Domain;
      Status := AML_Execute.Unsupported_Context;
      if not Initialized (A) then return; end if;
      if A.Invocation_Issued = Max_Invocations then Status := AML_Execute.Exhausted; return; end if;
      A.Invocation_Issued := A.Invocation_Issued + 1;
      Domain := AML_Frame_Handles.Bind_Domain (A.Token, A.Invocation_Issued);
      Status := AML_Execute.Available;
   end Begin_Invocation;
   function Reservation_Reference (Token : Name_Reservation) return Reference is (Token.Ref);
   function Reservation_Matches (A : Arena; Token : Name_Reservation) return Boolean is
     (AML_References.Kind (Token.Ref) = AML_References.Named_Cell
      and then Matches (A, Token.Ref)
      and then Token.Owner > Root and then Token.Owner <= A.Tree.Used
      and then A.Tree.Items (Token.Owner).Alive
      and then A.Tree.Items (Token.Owner).Object_Type = Method_Object
      and then A.Tree.Items (Token.Owner).Incarnation = Token.Owner_Stamp
      and then A.Tree.Items (Token.Owner).Active_Calls > 0
      and then A.Tree.Items (Node_ID (AML_References.Named_Node (Token.Ref))).Initializing
      and then A.Tree.Items (Node_ID (AML_References.Named_Node (Token.Ref))).Owner = Token.Owner);
   procedure Reserve_Name
     (A : in out Arena; Scope : Node_ID; Path : AML_Names.Name_Result;
      Token : out Name_Reservation; Status : out AML_Execute.Execution_Status)
   is
      Candidate : State := A.Tree;
      Base, Node : Node_ID;
      Added : Insert_Status;
   begin
      Token := No_Name_Reservation; Status := AML_Execute.Unsupported_Value;
      if not Initialized (A) then return; end if;
      Name_Target (A.Tree, Natural (Scope), Path, Base, Status);
      if Status /= AML_Execute.Returned then return; end if;
      Insert (Candidate, Base, Path.Parts (Path.Count), Node, Added);
      case Added is
         when Duplicate => Status := AML_Execute.Duplicate_Name; return;
         when Full => Status := AML_Execute.Namespace_Limit; return;
         when Invalid_Name => Status := AML_Execute.Bad_Name; return;
         when Inserted => null;
      end case;
      Candidate.Items (Node) := (Candidate.Items (Node) with delta
        Owner => Scope, Object_Type => Uninitialized_Name_Object, Initializing => True);
      A.Tree := Candidate;
      Token := (Ref => AML_References.Bind_Named (A.Token,
          AML_References.Node_Position (Node), A.Tree.Items (Node).Incarnation),
        Owner => Scope, Owner_Stamp => A.Tree.Items (Scope).Incarnation);
      Status := AML_Execute.Returned;
   end Reserve_Name;
   function Completion_Frame (Tree, Prior : State; Node : Node_ID) return Boolean is
     (Node > Root and then Node <= Prior.Used
      and then Tree = (Prior with delta Items => Tree.Items)
      and then (for all I in 1 .. Capacity =>
        (if I /= Node then Tree.Items (I) = Prior.Items (I)))
      and then Tree.Items (Node) = (Prior.Items (Node) with delta
        Initializing => False, Object_Type => Tree.Items (Node).Object_Type,
        Object_Ref => Tree.Items (Node).Object_Ref)
      and then (if Prior.Items (Node).Object_Type /= Uninitialized_Name_Object then
        Tree.Items (Node).Object_Type = Prior.Items (Node).Object_Type
        and then Tree.Items (Node).Object_Ref = Prior.Items (Node).Object_Ref));
   procedure Complete_Name
     (A : in out Arena; Token : Name_Reservation;
      Source : AML_References.Object_Handle; Status : out AML_Execute.Execution_Status)
   is
      Node : Node_ID;
      ID : AML_Objects.Object_ID;
      Tag : Object_Kind;
   begin
      Status := AML_Execute.Unsupported_Value;
      if not Reservation_Matches (A, Token) then return; end if;
      Node := Node_ID (AML_References.Named_Node (Token.Ref));
      if A.Tree.Items (Node).Object_Type = Uninitialized_Name_Object then
         if not Has_Source (A, Source) then return; end if;
         ID := AML_References.Source (Source);
         case AML_Objects.Kind (A.Tree.Values, ID) is
            when AML_Objects.Integer_Object => Tag := Integer_Object;
            when AML_Objects.String_Object => Tag := String_Object;
            when AML_Objects.Buffer_Object => Tag := Buffer_Object;
            when AML_Objects.Package_Object => Tag := Package_Object;
            when AML_Objects.Reference_Object => return;
         end case;
         A.Tree.Items (Node) := (A.Tree.Items (Node) with delta
           Object_Type => Tag, Object_Ref => ID, Initializing => False);
      else
         A.Tree.Items (Node).Initializing := False;
      end if;
      Status := AML_Execute.Returned;
   end Complete_Name;
   procedure Complete_Runtime_Buffer
     (A : in out Arena; Token : Name_Reservation;
      Width : AML_Decode.Integer_Width; Initializer : AML_Decode.Bytes;
      Count : AML_Data.Count_Result; Status : out AML_Execute.Execution_Status)
   is
      use type AML_Decode.Status;
      Layout : AML_Decode.Buffer_Count_Layout;
      Node : Node_ID;
   begin
      Status := AML_Execute.Unsupported_Value;
      if not Reservation_Matches (A, Token) or else Count.Kind /= AML_Decode.Accepted then return; end if;
      Layout := AML_Decode.Check_Buffer_Count_Span (Initializer, Count.Consumed);
      if Layout.Kind /= AML_Decode.Accepted or else Layout.Consumed /= Initializer'Length then return; end if;
      Node := Node_ID (AML_References.Named_Node (Token.Ref));
      if A.Tree.Items (Node).Object_Type /= Uninitialized_Name_Object then
         Complete_Name (A, Token, AML_References.No_Object_Handle, Status); return;
      end if;
      declare
         Buffer_Item : constant AML_Decode.Buffer_Result :=
           AML_Decode.Read_Buffer_With_Count (Initializer, Width, Count.Value, Count.Consumed);
      begin
      if Buffer_Item.Kind /= AML_Decode.Accepted then
         if Buffer_Item.Kind = AML_Decode.Limit_Exceeded then Status := AML_Execute.Value_Limit; end if;
         return;
      end if;
      declare
         Candidate : AML_Objects.State := A.Tree.Values;
         ID : AML_Objects.Object_ID;
         Allocated : AML_Objects.Allocation_Status;
      begin
      AML_Objects.New_Bytes (Candidate, AML_Objects.Buffer_Object,
        Buffer_Item.Content (1 .. Buffer_Item.Length), ID, Allocated);
      if Allocated /= AML_Objects.Allocated then Status := AML_Execute.Value_Limit; return; end if;
      -- One publication from the post-count arena; no pre-count state is restored.
      A.Tree.Values := Candidate;
      A.Tree.Items (Node) := (A.Tree.Items (Node) with delta
        Object_Type => Buffer_Object, Object_Ref => ID, Initializing => False);
      Status := AML_Execute.Returned;
      end;
      end;
   end Complete_Runtime_Buffer;
   function Abort_Frame (Tree, Prior : State; Node : Node_ID) return Boolean is
     (Node > Root and then Node <= Prior.Used and then Tree.Used <= Prior.Used
      and then Tree = (Prior with delta Used => Tree.Used, Items => Tree.Items)
      and then (for all I in 1 .. Tree.Used =>
        (if I = Node then Tree.Items (I) =
           Retired_Entry (Prior.Items (I))
         else Tree.Items (I) = Prior.Items (I)))
      and then (for all I in Tree.Used + 1 .. Prior.Used =>
        (I = Node or else not Prior.Items (I).Alive)
        and then Tree.Items (I) = Entry_Record'(Up => Root, Part => "____", others => <>))
      and then (for all I in Prior.Used + 1 .. Capacity => Tree.Items (I) = Prior.Items (I)));
   procedure Abort_Name
     (A : in out Arena; Token : Name_Reservation; Status : out AML_Execute.Execution_Status)
   is
      Node : Node_ID;
   begin
      Status := AML_Execute.Unsupported_Value;
      if not Reservation_Matches (A, Token) then return; end if;
      Node := Node_ID (AML_References.Named_Node (Token.Ref));
      A.Tree.Items (Node) := Retired_Entry (A.Tree.Items (Node));
      while A.Tree.Used > Root and then not A.Tree.Items (A.Tree.Used).Alive loop
         A.Tree.Items (A.Tree.Used) := (Up => Root, Part => "____", others => <>);
         A.Tree.Used := A.Tree.Used - 1;
      end loop;
      Status := AML_Execute.Returned;
   end Abort_Name;
   function Named_Identity_Matches (A : Arena; R : Reference) return Boolean is
     (AML_References.Belongs_To (R, A.Token)
      and then AML_References.Kind (R) = AML_References.Named_Cell
      and then AML_References.Named_Node (R) > 0
      and then AML_References.Named_Node (R) <= AML_References.Node_Position (A.Tree.Used)
      and then A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Alive
      and then A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Incarnation = AML_References.Incarnation (R));
   procedure Make_Named_Identity
     (A : Arena; Node : Natural; R : out Reference; Success : out Boolean)
   is
   begin
      R := AML_References.No_Reference; Success := False;
      if not Initialized (A) or else Node = 0 or else Node > A.Tree.Used
        or else not A.Tree.Items (Node).Alive then return; end if;
      R := AML_References.Bind_Named (A.Token, AML_References.Node_Position (Node), A.Tree.Items (Node).Incarnation);
      Success := True;
   end Make_Named_Identity;
   procedure Describe_Named_Identity
     (A : Arena; Ref : Reference; Result : out AML_Execute.Reference_Metadata)
   is
      use AML_Execute;
   begin
      Result := (Kind => Continue_Reference);
      if AML_References.Kind (Ref) /= AML_References.Named_Cell then return; end if;
      Result := (Kind => Invalid_Reference);
      if not Named_Identity_Matches (A, Ref) then return; end if;
      case A.Tree.Items (Node_ID (AML_References.Named_Node (Ref))).Object_Type is
         when Method_Object => Result := (Metadata_Only, Method_Metadata);
         when Device_Object => Result := (Metadata_Only, Device_Metadata);
         when Event_Object => Result := (Metadata_Only, Event_Metadata);
         when Mutex_Object => Result := (Metadata_Only, AML_Execute.Mutex_Metadata);
         when Power_Resource_Object => Result := (Metadata_Only, Power_Metadata);
         when Processor_Object => Result := (Metadata_Only, Processor_Metadata);
         when Thermal_Zone_Object => Result := (Metadata_Only, Thermal_Metadata);
         when Table_Field_Object | Region_Field_Object => Result := (Metadata_Only, Field_Metadata);
         when Table_Region_Object | Uninitialized_Region_Object | Operation_Region_Object => Result := (Metadata_Only, Region_Metadata);
         when Integer_Object | String_Object | Buffer_Object | Package_Object | Reference_Object | Uninitialized_Name_Object =>
            Result := (Kind => Continue_Reference);
         when Scope_Object => null;
      end case;
   end Describe_Named_Identity;
   procedure Make_Named_Reference
     (A : Arena; Node : Natural; R : out Reference; Success : out Boolean)
   is
   begin
      R := AML_References.No_Reference; Success := False;
      if not Initialized (A) or else Node = 0 or else Node > A.Tree.Used then return; end if;
      if not A.Tree.Items (Node).Alive or else A.Tree.Items (Node).Object_Type not in
        Integer_Object | String_Object | Buffer_Object | Package_Object | Reference_Object | Uninitialized_Name_Object then return; end if;
      R := AML_References.Bind_Named (A.Token, AML_References.Node_Position (Node), A.Tree.Items (Node).Incarnation);
      Success := True;
   end Make_Named_Reference;
   procedure Make_Source (A : Arena; ID : AML_Objects.Object_ID;
     H : out AML_References.Object_Handle; Success : out Boolean) is
   begin
      H := AML_References.No_Object_Handle;
      Success := A.Token /= AML_Identity.No_Identity and then AML_Objects.Is_Live (A.Tree.Values, ID);
      if Success then H := AML_References.Bind_Object (A.Token, AML_Objects.Address_Of (A.Tree.Values, ID)); end if;
   end Make_Source;
   procedure Make_Index (A : Arena; H : AML_References.Object_Handle;
     Index : AML_Decode.Integer_Value; R : out Reference;
     Status : out AML_Execute.Execution_Status) is
      ID : constant AML_Objects.Object_ID := AML_References.Source (H);
      Byte_Status : AML_Objects.Byte_References.Result_Status;
      Element_Status : AML_Objects.Package_References.Result_Status;
   begin
      R := AML_References.No_Reference; Status := AML_Execute.Unsupported_Value;
      if not Has_Source (A, H) then return; end if;
      case AML_Objects.Kind (A.Tree.Values, ID) is
         when AML_Objects.String_Object | AML_Objects.Buffer_Object =>
            Make (A, ID, Index, R, Byte_Status);
            if Byte_Status = AML_Objects.Byte_References.Ready then Status := AML_Execute.Returned; end if;
         when AML_Objects.Package_Object =>
            Make_Element (A, ID, Index, R, Element_Status);
            if Element_Status = AML_Objects.Package_References.Ready then Status := AML_Execute.Returned; end if;
         when AML_Objects.Integer_Object | AML_Objects.Reference_Object => null;
      end case;
   end Make_Index;
   type Allocation_Hook_Status is (Allocation_Allowed, Invalid_Census, Reclamation_Failed);
   procedure Skip_Collection (E : in out Arena; Extra : Root_Values;
      Status : out Allocation_Hook_Status) is
      pragma Unreferenced (E, Extra);
   begin Status := Allocation_Allowed; end Skip_Collection;
   generic
      with procedure Handoff (E : in out Arena; Result : AML_Execute.Execution_Result);
      with procedure Before_Allocation (E : in out Arena; Extra : Root_Values;
         Status : out Allocation_Hook_Status) is Skip_Collection;
   procedure Invoke_With_Handoff (A : in out Arena; Input : aliased AML_Table_Backing.State;
     Node : Node_ID; Args : AML_Execute.Value_Arguments; Argument_Count : Natural;
     Budget : Natural; Result : out AML_Execute.Execution_Result);
   procedure Invoke_With_Handoff (A : in out Arena; Input : aliased AML_Table_Backing.State;
     Node : Node_ID; Args : AML_Execute.Value_Arguments; Argument_Count : Natural;
     Budget : Natural; Result : out AML_Execute.Execution_Result) is
      use type AML_Execute.Binding_Status;
      Entry_Token : constant AML_Identity.Identity := A.Token;
      function Context_Valid (E : Arena) return Boolean is
        (Valid_Context (E.Tree) and then
          (E.Token = AML_Identity.No_Identity or else AML_Identity.Issuer.Is_Issued (E.Token))
          and then E.Token = Entry_Token);
      procedure Stamp (E : Arena; Binding : in out AML_Execute.Binding_Result) is
      begin
         if Binding.Status = AML_Execute.Non_Integer_Binding
           and then AML_Objects.Is_Live (E.Tree.Values, Binding.Object.ID)
         then Binding.Object.Source := AML_References.Bind_Object (E.Token, AML_Objects.Address_Of (E.Tree.Values, Binding.Object.ID)); end if;
      end Stamp;
      procedure Lookup (E : in out Arena; Backing : aliased AML_Table_Backing.State;
        Scope : Natural; Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width;
        Purpose : AML_Execute.Binding_Purpose; Binding : out AML_Execute.Binding_Result)
        with Pre => Context_Valid (E) and then not Binding'Constrained, Post => Context_Valid (E)
      is
         Located : Lookup_Result;
         Ref : Reference;
         OK : Boolean;
         Gate : Allocation_Hook_Status;
         use type AML_Decode.Integer_Width;
      begin
         if Purpose in AML_Execute.Reference_Target | AML_Execute.Namespace_Identity then
            Binding := (Status => AML_Execute.Missing_Binding);
            if Scope > E.Tree.Used then return; end if;
            Located := Resolve (E.Tree, Node_ID (Scope), Path);
            if Located.Status /= Found then return; end if;
            if Purpose = AML_Execute.Namespace_Identity then
               Make_Named_Identity (E, Natural (Located.Node), Ref, OK);
            else Make_Named_Reference (E, Natural (Located.Node), Ref, OK); end if;
            if OK then Binding := (Status => AML_Execute.Reference_Binding, Ref => Ref);
            else Binding := (Status => AML_Execute.Failed_Binding, Failure => AML_Execute.Unsupported_Value); end if;
            return;
         end if;
         if Purpose = AML_Execute.Evaluate_Binding and then Scope <= E.Tree.Used then
            Located := Resolve (E.Tree, Node_ID (Scope), Path);
            if Located.Status = Found and then Located.Node /= Root
              and then Present (E.Tree, Located.Node)
              and then Kind (E.Tree, Located.Node) = Table_Field_Object
              and then Field_Data (E.Tree, Located.Node).Bits >
                (if Width = AML_Decode.Bits_32 then 32 else 64)
            then
               Before_Allocation (E, [], Gate);
               if Gate /= Allocation_Allowed then
                  Binding := (AML_Execute.Failed_Binding, AML_Execute.Unsupported_Value); return;
               end if;
            end if;
         end if;
         Lookup_With_Tables (E.Tree, Backing, Scope, Path, Width, Purpose, Binding); Stamp (E, Binding);
      end Lookup;
      function Method (E : Arena; ID : Natural) return AML_Execute.Method_Definition is
        (Read_Method (E.Tree, ID));
      procedure Literal (E : in out Arena; Scope : Natural; Kind : AML_Execute.Literal_Kind; Width : AML_Decode.Integer_Width; Data : AML_Decode.Bytes;
        Binding : out AML_Execute.Binding_Result)
        with Pre => Context_Valid (E) and then not Binding'Constrained, Post => Context_Valid (E)
      is
         Gate : Allocation_Hook_Status;
      begin
         Before_Allocation (E, [], Gate);
         if Gate /= Allocation_Allowed then
            Binding := (AML_Execute.Failed_Binding, AML_Execute.Unsupported_Value); return;
         end if;
         Materialize_Literal (E.Tree, Scope, Kind, Width, Data, Binding); Stamp (E, Binding);
      end Literal;
      procedure Dereference (E : in out Arena; Ref : Reference;
        Value : out AML_Execute.Datum; Status : out AML_Execute.Execution_Status)
        with Pre => Context_Valid (E) and then not Value'Constrained, Post => Context_Valid (E)
      is
      begin
         if AML_References.Kind (Ref) = AML_References.Name_Member then
            Resolve_Name_Member (E, Ref, Value, Status);
         else Resolve_Value (E, Ref, Value, Status); end if;
      end Dereference;
      procedure Write (E : in out Arena; Scope : Natural; Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width; Item : AML_Execute.Datum; Status : out AML_Execute.Write_Status)
        with Pre => Context_Valid (E), Post => Context_Valid (E)
      is
         Located : Lookup_Result;
         Stored : AML_Execute.Execution_Status;
         Gate : Allocation_Hook_Status;
      begin
         Before_Allocation (E, [Item], Gate);
         if Gate /= Allocation_Allowed then Status := AML_Execute.Write_Unsupported; return; end if;
         if Item.Value_Kind = AML_Execute.Integer_Datum
           and then Scope <= E.Tree.Used
           and then Path.Kind = AML_Names.Accepted and then Path.Count > 0
         then
            Located := Resolve (E.Tree, Node_ID (Scope), Path);
            if Located.Status = Found
              and then E.Tree.Items (Located.Node).Alive
              and then E.Tree.Items (Located.Node).Initializing
              and then E.Tree.Items (Located.Node).Object_Type = Uninitialized_Name_Object
            then
               Replace_Value (E, Scope, Path, Width, Item, Stored);
               Status := (case Stored is
                 when AML_Execute.Returned => AML_Execute.Written,
                 when AML_Execute.Value_Limit => AML_Execute.Write_Value_Limit,
           when AML_Execute.Empty_Buffer => AML_Execute.Write_Empty_Buffer,
                 when AML_Execute.Unknown_Name => AML_Execute.Write_Missing,
                 when others => AML_Execute.Write_Unsupported);
               return;
            end if;
         end if;
         Status := AML_Execute.Write_Unsupported;
         if Scope > E.Tree.Used then Status := AML_Execute.Write_Missing; return; end if;
         Located := Resolve (E.Tree, Node_ID (Scope), Path);
         if Located.Status /= Found then Status := AML_Execute.Write_Missing; return; end if;
         Store_Named_Direct (E, Located.Node, Width, Item, Stored);
         Status := (case Stored is
           when AML_Execute.Returned => AML_Execute.Written,
           when AML_Execute.Value_Limit => AML_Execute.Write_Value_Limit,
           when AML_Execute.Empty_Buffer => AML_Execute.Write_Empty_Buffer,
           when AML_Execute.Unknown_Name => AML_Execute.Write_Missing,
           when others => AML_Execute.Write_Unsupported);
      end Write;
      procedure Begin_Call (E : in out Arena; Scope : Natural; Allowed : out Boolean)
        with Pre => Context_Valid (E), Post => Context_Valid (E)
      is
      begin Begin_Method (E.Tree, Scope, Allowed); end Begin_Call;
      procedure End_Call (E : in out Arena; Scope : Natural)
        with Pre => Context_Valid (E), Post => Context_Valid (E)
      is
      begin End_Method (E.Tree, Scope); end End_Call;
      procedure Define (E : in out Arena; Scope : Natural; Path : AML_Names.Name_Result; Flags : AML_Decode.Byte; Width : AML_Decode.Integer_Width; Code : AML_Decode.Bytes; Status : out AML_Execute.Declaration_Status)
        with Pre => Context_Valid (E), Post => Context_Valid (E)
      is
      begin Define_Method (E.Tree, Scope, Path, Flags, Width, Code, Status); end Define;
      procedure Fields (E : in out Arena; Scope : Natural; Region : AML_Names.Name_Result; Flags : AML_Decode.Byte; Entries : AML_Decode.Bytes; Status : out AML_Execute.Execution_Status)
        with Pre => Context_Valid (E), Post => Context_Valid (E)
      is
      begin Define_Fields (E.Tree, Scope, Region, Flags, Entries, Status); end Fields;
      procedure Reserve (E : in out Arena; Scope : Natural; Path : AML_Names.Name_Result; Token : out Natural; Status : out AML_Execute.Execution_Status)
        with Pre => Context_Valid (E), Post => Context_Valid (E)
      is
      begin Reserve_Region (E.Tree, Scope, Path, Token, Status); end Reserve;
      procedure Complete (E : in out Arena; Backing : aliased AML_Table_Backing.State; Token : Natural; Width : AML_Decode.Integer_Width; Signature, OEM, Table_ID : AML_Execute.Datum; Status : out AML_Execute.Execution_Status)
        with Pre => Context_Valid (E), Post => Context_Valid (E)
      is
      begin Complete_Region (E.Tree, Backing, Token, Width, Signature, OEM, Table_ID, Status); end Complete;
      procedure Timer (E : in out Arena; Value : out AML_Decode.Integer_Value; Available : out Boolean)
        with Pre => Context_Valid (E), Post => Context_Valid (E)
      is
      begin Read_Context_Timer (E.Tree, Value, Available); end Timer;
      procedure Index (E : in out Arena; Source : AML_References.Object_Handle; Position : AML_Decode.Integer_Value; Ref : out Reference; Status : out AML_Execute.Execution_Status)
        with Pre => Context_Valid (E), Post => Context_Valid (E)
      is
      begin Make_Index (E, Source, Position, Ref, Status); end Index;
      procedure Store_Reference
        (E : in out Arena; Ref : Reference; Width : AML_Decode.Integer_Width;
         Item : AML_Execute.Datum; Status : out AML_Execute.Execution_Status; Mode : AML_Execute.Reference_Store_Mode)
        with Pre => Context_Valid (E), Post => Context_Valid (E)
      is
         Gate : Allocation_Hook_Status;
      begin
         Before_Allocation (E, [Item, (AML_Execute.Reference_Datum, Ref)], Gate);
         if Gate /= Allocation_Allowed then Status := AML_Execute.Unsupported_Value; return; end if;
         Store_Reference_Value (E, Ref, Width, Item, Status, Mode);
      end Store_Reference;
      procedure Compare_Objects
        (E : in out Arena; Op : AML_Decode.Byte; Left, Right : AML_Execute.Datum;
         Width : AML_Decode.Integer_Width; Value : out AML_Decode.Integer_Value;
         Status : out AML_Execute.Execution_Status)
        with Pre => Context_Valid (E), Post => Context_Valid (E)
      is
      begin Compare_Byte_Values (E, Op, Left, Right, Width, Value, Status); end Compare_Objects;
      procedure Define_Name
        (E : in out Arena; Scope : Natural; Path : AML_Names.Name_Result;
         Width : AML_Decode.Integer_Width; Data : AML_Decode.Bytes;
         Consumed : out Natural; Status : out AML_Execute.Execution_Status)
        with Pre => Context_Valid (E), Post => Context_Valid (E)
      is
         Gate : Allocation_Hook_Status;
      begin
         Before_Allocation (E, [], Gate);
         if Gate /= Allocation_Allowed then Consumed := 0; Status := AML_Execute.Unsupported_Value; return; end if;
         Define_Runtime_Name (E.Tree, E.Token, Scope, Path, Width, Data, Consumed, Status);
      end Define_Name;
      procedure Reserve_Dynamic_Name
        (E : in out Arena; Scope : Natural; Path : AML_Names.Name_Result;
         Token : out Name_Reservation; Status : out AML_Execute.Execution_Status)
      is
      begin
         Token := No_Name_Reservation; Status := AML_Execute.Unsupported_Value;
         if Scope > E.Tree.Used then return; end if;
         Reserve_Name (E, Node_ID (Scope), Path, Token, Status);
      end Reserve_Dynamic_Name;
      procedure Complete_Dynamic_Buffer
        (E : in out Arena; Token : Name_Reservation; Width : AML_Decode.Integer_Width;
         Initializer : AML_Decode.Bytes; Count : AML_Data.Count_Result;
         Status : out AML_Execute.Execution_Status)
      is
         Gate : Allocation_Hook_Status;
      begin
         Before_Allocation (E, [(AML_Execute.Reference_Datum, Token.Ref)], Gate);
         if Gate /= Allocation_Allowed then Status := AML_Execute.Unsupported_Value; return; end if;
         Complete_Runtime_Buffer (E, Token, Width, Initializer, Count, Status);
      end Complete_Dynamic_Buffer;
      procedure Abort_Dynamic_Name
        (E : in out Arena; Token : Name_Reservation; Status : out AML_Execute.Execution_Status)
      is
      begin Abort_Name (E, Token, Status); end Abort_Dynamic_Name;

      -- AML Debug output is explicitly disabled in this owned/unowned adapter.
      -- This is observation only; source evaluation remains in the executor.
      procedure Disabled_Debug
        (Environment : Arena; Scope, Position : Natural;
         Width : AML_Decode.Integer_Width; Value : AML_Execute.Datum)
      is
         pragma Unreferenced (Environment, Scope, Position, Width, Value);
      begin null; end Disabled_Debug;
      Admitted_Domain : AML_Frame_Handles.Invocation_Domain := AML_Frame_Handles.No_Domain;
      procedure Begin_Root_Invocation
        (E : in out Arena; Domain : out AML_Frame_Handles.Invocation_Domain;
         Status : out AML_Execute.Invocation_Status)
      is
      begin
         Begin_Invocation (E, Domain, Status);
         Admitted_Domain := Domain;
      end Begin_Root_Invocation;
      function Root_Capacity (E : Arena) return Boolean is
        (Frame_Pins.Count (E.Frame_Roots) < Max_Frame_Roots);
      procedure Open_Root (E : in out Arena; Frame : AML_Frame_Handles.Frame_Handle) is
         Status : Frame_Pins.Result_Status;
         use type Frame_Pins.Result_Status;
      begin
         if not AML_Frame_Handles.Belongs_To (Frame, Admitted_Domain)
         then raise Program_Error with "frame root owner invariant"; end if;
         Frame_Pins.Reserve (E.Frame_Roots, Frame, Status);
         if Status /= Frame_Pins.Ready then raise Program_Error with "frame root admission invariant"; end if;
      end Open_Root;
      procedure Publish_Root
        (E : in out Arena; Frame : AML_Frame_Handles.Frame_Handle;
         Cell : AML_Frame_Handles.Cell_ID; Initialized : Boolean; Value : AML_Execute.Datum)
      is
         Status : Frame_Pins.Result_Status;
         use type Frame_Pins.Result_Status;
      begin
         Frame_Pins.Update (E.Frame_Roots, Frame, Cell, Initialized, Value, Status);
         if Status /= Frame_Pins.Ready then raise Program_Error with "frame root update invariant"; end if;
      end Publish_Root;
      procedure Publish_Held
        (E : in out Arena; Frame : AML_Frame_Handles.Frame_Handle;
         Root : AML_Root_Slots.Held_Root; Initialized : Boolean; Value : AML_Execute.Datum)
      is
         Status : Frame_Pins.Result_Status;
         use type Frame_Pins.Result_Status;
      begin
         Frame_Pins.Update_Held (E.Frame_Roots, Frame, Root, Initialized, Value, Status);
         if Status /= Frame_Pins.Ready then raise Program_Error with "held root update invariant"; end if;
      end Publish_Held;
      procedure Publish_Expression
        (E : in out Arena; Frame : AML_Frame_Handles.Frame_Handle;
         Values : AML_Execute.Expression_Values) is
         Roots : AML_Objects.Root_Snapshots.Snapshot;
         Built : Snapshot_Build_Status;
         Updated : Frame_Pins.Result_Status;
         use type Frame_Pins.Result_Status;
      begin
         Build_Expression_Snapshot (E, Values, Roots, Built);
         if Built /= Snapshot_Built then AML_Objects.Root_Snapshots.Reject (Roots); end if;
         -- Preserve rejected snapshots as well: a failed census must never
         -- silently reuse earlier roots or become a valid empty root set.
         Frame_Pins.Update_Snapshot (E.Frame_Roots, Frame, Roots, Updated);
         if Updated /= Frame_Pins.Ready then raise Program_Error with "expression frame invariant"; end if;
      end Publish_Expression;
      procedure Close_Root (E : in out Arena; Frame : AML_Frame_Handles.Frame_Handle) is
         Status : Frame_Pins.Result_Status;
         use type Frame_Pins.Result_Status;
      begin
         Frame_Pins.Release (E.Frame_Roots, Frame, Status);
         if Status /= Frame_Pins.Ready then raise Program_Error with "frame root close invariant"; end if;
      end Close_Root;
      procedure Clone_At_Entry
        (E : in out Arena; Width : AML_Decode.Integer_Width;
         Item : AML_Execute.Datum; Copy : out AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status) is
         Gate : Allocation_Hook_Status;
      begin
         Copy := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Before_Allocation (E, [Item], Gate);
         if Gate /= Allocation_Allowed then Status := AML_Execute.Unsupported_Value; return; end if;
         Clone_Value (E, Width, Item, Copy, Status);
      end Clone_At_Entry;
      procedure Copy_At_Entry
        (E : in out Arena; Destination : AML_Execute.Copy_Destination;
         Width : AML_Decode.Integer_Width; Item : AML_Execute.Datum;
         Copy : out AML_Execute.Datum; Status : out AML_Execute.Execution_Status) is
         Gate : Allocation_Hook_Status;
         use type AML_Execute.Copy_Destination_Kind;
      begin
         Copy := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         if Destination.Kind = AML_Execute.Referenced_Destination then
            Before_Allocation (E, [Item, (AML_Execute.Reference_Datum, Destination.Ref)], Gate);
         else Before_Allocation (E, [Item], Gate); end if;
         if Gate /= Allocation_Allowed then Status := AML_Execute.Unsupported_Value; return; end if;
         Copy_And_Attach (E, Destination, Width, Item, Copy, Status);
      end Copy_At_Entry;
      procedure Concatenate_At_Entry
        (E : in out Arena; Width : AML_Decode.Integer_Width;
         Left, Right : AML_Execute.Concatenation_Operand;
         Destination : AML_Execute.Concatenation_Destination;
         Result_Value, Cell_Value : out AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status) is
         Gate : Allocation_Hook_Status;
         use type AML_Execute.Concatenation_Operand_Kind;
         use type AML_Execute.Concatenation_Destination_Kind;
         function Root_Value (Operand : AML_Execute.Concatenation_Operand) return AML_Execute.Datum is
           (if Operand.Kind = AML_Execute.Data_Operand then Operand.Value
            else (AML_Execute.Reference_Datum, Operand.Identity));
      begin
         Result_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Cell_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         if Destination.Kind = AML_Execute.Reference_Attachment then
            Before_Allocation (E, [Root_Value (Left), Root_Value (Right),
              (AML_Execute.Reference_Datum, Destination.Ref)], Gate);
         else Before_Allocation (E, [Root_Value (Left), Root_Value (Right)], Gate); end if;
         if Gate /= Allocation_Allowed then Status := AML_Execute.Unsupported_Value; return; end if;
         Concatenate_And_Attach (E, Width, Left, Right, Destination, Result_Value, Cell_Value, Status);
      end Concatenate_At_Entry;
      procedure Resources_At_Entry
        (E : in out Arena; Width : AML_Decode.Integer_Width;
         Left, Right : AML_Execute.Datum;
         Destination : AML_Execute.Concatenation_Destination;
         Result_Value, Cell_Value : out AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status) is
         Gate : Allocation_Hook_Status;
         use type AML_Execute.Concatenation_Destination_Kind;
      begin
         Result_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Cell_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         if Destination.Kind = AML_Execute.Reference_Attachment then
            Before_Allocation (E, [Left, Right,
              (AML_Execute.Reference_Datum, Destination.Ref)], Gate);
         else
            Before_Allocation (E, [Left, Right], Gate);
         end if;
         if Gate /= Allocation_Allowed then
            Status := AML_Execute.Unsupported_Value;
            return;
         end if;
         Concatenate_Resources_And_Attach
           (E, Width, Left, Right, Destination, Result_Value, Cell_Value, Status);
      end Resources_At_Entry;

      procedure To_Buffer_At_Entry
        (E : in out Arena; W : AML_Decode.Integer_Width; Item : AML_Execute.Datum;
         Destination : AML_Execute.Concatenation_Destination;
         Result_Value, Cell_Value : out AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status)
      is
         Gate : Allocation_Hook_Status;
         use type AML_Execute.Concatenation_Destination_Kind;
      begin
         Result_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Cell_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         -- No allocation or collection is necessary for a detached Buffer.
         if Destination.Kind = AML_Execute.Detached_Result
           and then Item.Value_Kind = AML_Execute.Object_Datum
           and then Has_Source (E, Item.Object.Source)
           and then Item.Object.ID = AML_References.Source (Item.Object.Source)
           and then AML_Objects.Kind (E.Tree.Values, Item.Object.ID) = AML_Objects.Buffer_Object
         then
            To_Buffer_And_Attach (E, W, Item, Destination, Result_Value, Cell_Value, Status);
            return;
         end if;
         if Destination.Kind = AML_Execute.Reference_Attachment then
            Before_Allocation (E, [Item, (AML_Execute.Reference_Datum, Destination.Ref)], Gate);
         else Before_Allocation (E, [Item], Gate); end if;
         if Gate /= Allocation_Allowed then Status := AML_Execute.Unsupported_Value; return; end if;
         To_Buffer_And_Attach (E, W, Item, Destination, Result_Value, Cell_Value, Status);
      end To_Buffer_At_Entry;
      procedure Format_String_At_Entry
        (E : in out Arena; W : AML_Decode.Integer_Width; Mode : AML_Execute.Explicit_String_Mode; Item : AML_Execute.Datum;
         Destination : AML_Execute.Concatenation_Destination;
         Result_Value, Cell_Value : out AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status)
      is
         Gate : Allocation_Hook_Status;
         use type AML_Execute.Concatenation_Destination_Kind;
      begin
         Result_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Cell_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         -- No allocation or collection is necessary for a detached String.
         if Destination.Kind = AML_Execute.Detached_Result
           and then Item.Value_Kind = AML_Execute.Object_Datum
           and then Has_Source (E, Item.Object.Source)
           and then Item.Object.ID = AML_References.Source (Item.Object.Source)
           and then AML_Objects.Kind (E.Tree.Values, Item.Object.ID) = AML_Objects.String_Object
         then
            Format_String_And_Attach (E, W, Mode, Item, Destination, Result_Value, Cell_Value, Status);
            return;
         end if;
         if Destination.Kind = AML_Execute.Reference_Attachment then
            Before_Allocation (E, [Item, (AML_Execute.Reference_Datum, Destination.Ref)], Gate);
         else Before_Allocation (E, [Item], Gate); end if;
         if Gate /= Allocation_Allowed then Status := AML_Execute.Unsupported_Value; return; end if;
         Format_String_And_Attach (E, W, Mode, Item, Destination, Result_Value, Cell_Value, Status);
      end Format_String_At_Entry;
      procedure Mid_At_Entry
        (E : in out Arena; W : AML_Decode.Integer_Width; Item : AML_Execute.Datum; Start, Count : AML_Decode.Integer_Value;
         Destination : AML_Execute.Concatenation_Destination;
         Result_Value, Cell_Value : out AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status)
      is
         Gate : Allocation_Hook_Status;
         use type AML_Execute.Concatenation_Destination_Kind;
      begin
         Result_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Cell_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         if Destination.Kind = AML_Execute.Reference_Attachment then
            Before_Allocation (E, [Item, (AML_Execute.Reference_Datum, Destination.Ref)], Gate);
         else Before_Allocation (E, [Item], Gate); end if;
         if Gate /= Allocation_Allowed then Status := AML_Execute.Unsupported_Value; return; end if;
         Mid_And_Attach (E, W, Item, Start, Count, Destination, Result_Value, Cell_Value, Status);
      end Mid_At_Entry;
      procedure To_String_At_Entry
        (E : in out Arena; W : AML_Decode.Integer_Width; Item : AML_Execute.Datum; Length : AML_Decode.Integer_Value;
         Destination : AML_Execute.Concatenation_Destination;
         Result_Value, Cell_Value : out AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status)
      is
         Gate : Allocation_Hook_Status;
         use type AML_Execute.Concatenation_Destination_Kind;
      begin
         Result_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Cell_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         if Destination.Kind = AML_Execute.Reference_Attachment then
            Before_Allocation (E, [Item, (AML_Execute.Reference_Datum, Destination.Ref)], Gate);
         else Before_Allocation (E, [Item], Gate); end if;
         if Gate /= Allocation_Allowed then Status := AML_Execute.Unsupported_Value; return; end if;
         To_String_And_Attach (E, W, Item, Length, Destination, Result_Value, Cell_Value, Status);
      end To_String_At_Entry;


      procedure Delay_Adapter_2 (Environment : in out Arena; Item : AML_Delays.Request; Result : out AML_Delays.Outcome) is
         pragma Unreferenced (Environment);
      begin
         Perform_Delay (Item, Result);
      end Delay_Adapter_2;
      procedure Execute is new AML_Execute.Execute_With_Input
        (Arena, AML_Table_Backing.State, Context_Valid, Lookup, Method, Write,
         Begin_Call, End_Call, Define, Fields, Literal, Reserve, Complete, Timer,
         Dereference, Index, Store_Reference, Clone_At_Entry, Compare_Objects, Refresh_Value, Begin_Root_Invocation, Define_Name, Copy_At_Entry, Describe_Named_Identity, Name_Reservation, No_Name_Reservation, Reserve_Dynamic_Name, Complete_Dynamic_Buffer, Abort_Dynamic_Name, Disabled_Debug, Convert_To_Integer, Concatenate_At_Entry, To_Buffer_At_Entry, Mid_At_Entry, To_String_At_Entry, Format_String_At_Entry, Match_Package, Resources_At_Entry, Handoff_Result => Handoff,
         Publish_Frame_Root => Publish_Root, Frame_Root_Capacity => Root_Capacity,
         Open_Frame_Root => Open_Root, Close_Frame_Root => Close_Root, Publish_Held_Root => Publish_Held,
         Publish_Expression_Roots => Publish_Expression, Wait_For_Delay => Delay_Adapter_2);
      Definition : constant AML_Execute.Method_Definition := Method (A, Node);
   begin
      if Frame_Pins.Count (A.Frame_Roots) /= 0 then
         Result := (Status => AML_Execute.Unsupported_Value, Charged => 0); return;
      end if;
      if Pending_Members (A.Tree) /= 0 then
         Result := (Status => AML_Execute.Uninitialized, Charged => 0); return;
      end if;
      if not Definition.Exists then Result := (Status => AML_Execute.Invalid_Method, Charged => 0); return;
      elsif Natural (Definition.Flags and 7) /= Argument_Count then
         Result := (Status => AML_Execute.Argument_Mismatch, Charged => 0); return;
      end if;
      Execute (Definition.Code, Definition.Width, Args, Argument_Count, Budget,
        Input, A, Natural (Node), Result,
        Current_Sync => AML_Execute.Method_Level (Definition.Flags));
   end Invoke_With_Handoff;
   procedure Ignore_Result (E : in out Arena; Result : AML_Execute.Execution_Result) is
      pragma Unreferenced (E, Result);
   begin null; end Ignore_Result;
   procedure Invoke_Unretained is new Invoke_With_Handoff (Ignore_Result);
   procedure Invoke (A : in out Arena; Input : aliased AML_Table_Backing.State;
     Node : Node_ID; Args : AML_Execute.Value_Arguments; Argument_Count : Natural;
     Budget : Natural; Result : out AML_Execute.Execution_Result) is
   begin
      Invoke_Unretained (A, Input, Node, Args, Argument_Count, Budget, Result);
   end Invoke;
   function Retention_Reservation_Discarded (A : Arena; Before : Retention_State)
     return Boolean is
     (Pins.Discarded_Reservation (Pins.Snapshot (A.Retention), Pins.Model (Before)));
   generic
      with procedure Before_Allocation (E : in out Arena; Extra : Root_Values;
         Status : out Allocation_Hook_Status) is Skip_Collection;
   procedure Invoke_Retained_With_Hook
     (A : in out Arena; Input : aliased AML_Table_Backing.State;
      Node : Node_ID; Args : AML_Execute.Value_Arguments; Argument_Count : Natural;
      Budget : Natural; Result : out AML_Execute.Execution_Result;
      Result_Root : out Retained_Root; Retention : out Invocation_Retention_Status);
   procedure Invoke_Retained_With_Hook
     (A : in out Arena; Input : aliased AML_Table_Backing.State;
      Node : Node_ID; Args : AML_Execute.Value_Arguments; Argument_Count : Natural;
      Budget : Natural; Result : out AML_Execute.Execution_Result;
      Result_Root : out Retained_Root; Retention : out Invocation_Retention_Status)
   is
      use AML_Execute;
      use type Pins.Result_Status;
      Reserved_Root : Retained_Root;
      Admission : Retain_Status;
      Captured : Boolean := False;
      procedure Handoff (E : in out Arena; Returned_Result : Execution_Result) is
         Value : Datum := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Read_Status : Execution_Status;
         Pin_Status : Pins.Result_Status;
      begin
         if Returned_Result.Status = Object_Returned then
            if Returned_Result.Object.ID /= AML_References.Source (Returned_Result.Object.Source)
              or else not Has_Source (E, Returned_Result.Object.Source) then return; end if;
            Read_Source (E, Returned_Result.Object.Source, Value, Read_Status);
            if Read_Status /= Returned or else Value.Value_Kind /= Object_Datum then return; end if;
         elsif Returned_Result.Status = Reference_Returned then
            if not Admitted_Descriptor (E, Returned_Result.Ref) then return; end if;
            Value := (Reference_Datum, Returned_Result.Ref);
         else return;
         end if;
         Pins.Replace (E.Retention, Reserved_Root.Pin, Value, Pin_Status);
         if Pin_Status /= Pins.Ready then raise Program_Error with "retained result slot invariant"; end if;
         Captured := True;
      end Handoff;
      procedure Run is new Invoke_With_Handoff (Handoff, Before_Allocation);
      Removed : Release_Status;
   begin
      Result_Root := No_Retained_Root;
      if Frame_Pins.Count (A.Frame_Roots) /= 0 then
         Result := (Status => Unsupported_Value, Charged => 0);
         Retention := Invocation_Busy; return;
      end if;
      Result := (Status => Value_Limit, Charged => 0);
      Retain (A, (Integer_Datum, 0, AML_Decode.Ordinary_Integer), Reserved_Root, Admission);
      case Admission is
         when Root_Limit => Retention := Result_Root_Limit; return;
         when Identity_Exhausted => Retention := Result_Identity_Exhausted; return;
         when Invalid_Value => raise Program_Error with "retained invocation admission invariant";
         when Retained => null;
      end case;
      Run (A, Input, Node, Args, Argument_Count, Budget, Result);
      if Captured and then Result.Status in Object_Returned | Reference_Returned then
         Result_Root := Reserved_Root; Retention := Result_Retained;
      else
         Release (A, Reserved_Root, Removed);
         if Removed /= Released then raise Program_Error with "retained result release invariant"; end if;
         if Result.Status in Object_Returned | Reference_Returned then
            Result := (Status => Unsupported_Value, Charged => Result.Charged);
            Retention := Invalid_Result;
         else Retention := No_Root_Required;
         end if;
      end if;
   end Invoke_Retained_With_Hook;
   procedure Invoke_Retained
     (A : in out Arena; Input : aliased AML_Table_Backing.State;
      Node : Node_ID; Args : AML_Execute.Value_Arguments; Argument_Count : Natural;
      Budget : Natural; Result : out AML_Execute.Execution_Result;
      Result_Root : out Retained_Root; Retention : out Invocation_Retention_Status)
   is
      procedure Run is new Invoke_Retained_With_Hook;
   begin
      Run (A, Input, Node, Args, Argument_Count, Budget, Result, Result_Root, Retention);
   end Invoke_Retained;
   package body Collecting is
      function No_Value return Value_Handle is ((others => <>));
      function Valid (A : Arena) return Boolean is
        (Owned.Valid (A.Inner) and then
           (if A.State = Uninitialized then not Owned.Initialized (A.Inner)
            else Owned.Initialized (A.Inner)));
      function Retention_Model (A : Arena) return Retention_State is
        (Owned.Retention_Model (A.Inner));
      function Pin_Added (A : Arena; Before : Retention_State;
         Handle : Value_Handle) return Boolean is
         use type Pins.Result_Status;
         Saved : constant Pins.Read_Result := Pins.Read (A.Inner.Retention, Handle.Root.Pin);
      begin
         return Saved.Status = Pins.Ready and then Owned.Retention_Added
           (A.Inner, Before, Handle.Root, Saved.Value);
      end Pin_Added;
      function Pin_Removed (A : Arena; Before : Retention_State;
         Handle : Value_Handle) return Boolean is
        (Owned.Retention_Removed (A.Inner, Before, Handle.Root));
      function Pins_Cleared (A : Arena; Before : Retention_State) return Boolean is
        (Owned.Retention_Cleared (A.Inner, Before));
      function Reclamation_Metrics (A : Arena) return Collection_Statistics is (A.Collections);
      function Current (A : Arena) return Phase is (A.State);
      function Retained_Count (A : Arena) return Retained_Root_Count is
        (Owned.Retained_Count (A.Inner));
      function Node_Count (A : Arena) return Node_ID is (Owned.Node_Count (A.Inner));
      function Present (A : Arena; Node : Node_ID) return Boolean is
        (not A.In_Progress and then A.State /= Uninitialized
         and then Node <= Owned.Node_Count (A.Inner) and then Owned.Present (A.Inner, Node));
      function Admission (A : Arena) return Access_Status is
        (if A.In_Progress then Busy elsif A.State /= Ready then Wrong_Phase else Available);
      function Audit (A : Arena) return Audit_Value is ((Tree => Owned.Snapshot (A.Inner)));
      function Initialization_Frame (Current, Prior : Audit_Value) return Boolean is
        (AML_Namespace.Initialization_Frame (Current.Tree, Prior.Tree));
      function Generation (A : Arena) return AML_Identity.Identity is
        (Owned.Generation (A.Inner));
      function Observe_Usage (A : Arena) return Usage_Description is
        (Nodes => Owned.Node_Count (A.Inner), Values => Owned.Values_Used (A.Inner),
         Methods => Owned.Methods_Used (A.Inner), Pending => Owned.Pending_Members (A.Inner));
      procedure Find_Child (A : Arena; Parent : Node_ID; Part : AML_Names.Segment;
         Node : out Node_ID; Status : out Access_Status) is
      begin
         Node := Root;
         if A.In_Progress then Status := Busy; return;
         elsif A.State = Uninitialized then Status := Wrong_Phase; return;
         elsif not Present (A, Parent) then Status := Invalid_Value; return; end if;
         Node := AML_Namespace.Child (A.Inner.Tree, Parent, Part);
         Status := Available;
      end Find_Child;
      procedure Bind_Table_Region (A : in out Arena; Scope : Node_ID;
         Part : AML_Names.Segment; Region : Table_Region; Node : out Node_ID;
         Outcome : out Bind_Status; Status : out Access_Status; Owner : Node_ID := Root) is
      begin
         Node := Root; Outcome := Binding_Invalid;
         if A.In_Progress then Status := Busy; return;
         elsif A.State /= Loading then Status := Wrong_Phase; return;
         elsif not Present (A, Scope) or else not Present (A, Owner) then
            Status := Invalid_Value; return; end if;
         Owned.Bind_Table_Region (A.Inner, Scope, Part, Region, Node, Outcome, Owner);
         Status := Available;
      end Bind_Table_Region;
      procedure Bind_Table_Field (A : in out Arena; Scope : Node_ID;
         Part : AML_Names.Segment; Region_Node : Node_ID; Offset, Bits : Natural;
         Node : out Node_ID; Outcome : out Bind_Status; Status : out Access_Status;
         Owner : Node_ID := Root) is
      begin
         Node := Root; Outcome := Binding_Invalid;
         if A.In_Progress then Status := Busy; return;
         elsif A.State /= Loading then Status := Wrong_Phase; return;
         elsif not Present (A, Scope) or else not Present (A, Owner)
           or else Region_Node = Root or else not Present (A, Region_Node) then
            Status := Invalid_Value; return;
         elsif Owned.Kind (A.Inner, Region_Node) /= Table_Region_Object then
            Status := Wrong_Kind; return; end if;
         Owned.Bind_Table_Field (A.Inner, Scope, Part,
            (Owned.Region_Data (A.Inner, Region_Node), Offset, Bits), Node, Outcome, Owner);
         Status := Available;
      end Bind_Table_Field;
      procedure Read_Table_Field (A : Arena; Node : Node_ID;
         Field : out Table_Field; Status : out Access_Status) is
      begin
         Field := (others => <>);
         if A.In_Progress then Status := Busy; return;
         elsif A.State = Uninitialized then Status := Wrong_Phase; return;
         elsif Node = Root or else not Present (A, Node) then Status := Invalid_Value; return;
         elsif Owned.Kind (A.Inner, Node) /= Table_Field_Object then
            Status := Wrong_Kind; return; end if;
         Field := Owned.Field_Data (A.Inner, Node); Status := Available;
      end Read_Table_Field;
      procedure Reset (A : in out Arena; Status : out Access_Status) is
         Success : Boolean;
      begin
         if A.In_Progress then Status := Busy; return; end if;
         A.In_Progress := True;
         Owned.Reset (A.Inner, Success);
         if Success then A.State := Loading; Status := Available;
         else Status := Identity_Exhausted; end if;
         A.In_Progress := False;
      end Reset;
      procedure Initialize (A : in out Arena; Status : out Access_Status) is
      begin
         if A.In_Progress then Status := Busy;
         elsif A.State /= Uninitialized then Status := Wrong_Phase;
         else Reset (A, Status); end if;
      end Initialize;
      procedure Load (A : in out Arena; Data : AML_Decode.Bytes;
         Width : AML_Decode.Integer_Width; Outcome : out Load_Status;
         Status : out Access_Status) is
      begin
         Outcome := Unsupported_Opcode;
         if A.In_Progress then Status := Busy; return;
         elsif A.State /= Loading then Status := Wrong_Phase; return; end if;
         A.In_Progress := True;
         Owned.Load (A.Inner, Data, Width, Outcome);
         A.In_Progress := False; Status := Available;
      end Load;
      procedure Seal (A : in out Arena; Report : out Initialization_Report;
         Status : out Access_Status) is
      begin
         Report := (others => 0);
         if A.In_Progress then Status := Busy; return;
         elsif A.State /= Loading then Status := Wrong_Phase; return; end if;
         A.In_Progress := True;
         Owned.Initialize_Members (A.Inner, Report);
         A.State := Ready; A.In_Progress := False; Status := Available;
      end Seal;
      procedure Invoke (A : in out Arena; Input : aliased AML_Table_Backing.State;
         Node : Node_ID; Args : Arguments; Count : Argument_Count;
         Budget : Natural; Outcome : out Result; Status : out Access_Status) is
         Raw_Args : AML_Execute.Value_Arguments :=
           [others => (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer)];
         Raw : AML_Execute.Execution_Result;
         Root : Retained_Root;
         Retention : Invocation_Retention_Status;
         Read_Status : AML_Execute.Execution_Status;
         subtype Failure_Status is AML_Execute.Execution_Status range
           AML_Execute.No_Return .. AML_Execute.Execution_Status'Last;
         Local_Metrics : Collection_Statistics := A.Collections;
         Session_Active : Boolean := False;
         Roots : Root_Workspace;
         Reclaim_Scratch : AML_Objects.Reclamation.Workspace;
         Keep : Object_Root_Set;
         procedure Add (Counter : in out Collection_Count; Amount : Natural := 1) is
         begin
            if Collection_Count (Amount) > Collection_Count'Last - Counter then
               Counter := Collection_Count'Last;
            else Counter := Counter + Collection_Count (Amount); end if;
         end Add;
         procedure Collect_Before
           (E : in out Owned.Arena; Extra : Root_Values; Gate : out Allocation_Hook_Status) is
            Trace : Root_Trace_Status;
            Reclaimed : AML_Objects.Reclamation.Reclaim_Status;
            Before, After : AML_Objects.Usage;
            use type AML_Objects.Reclamation.Reclaim_Status;
         begin
            Add (Local_Metrics.Attempted);
            Gate := Invalid_Census;
            if not Session_Active or else not Owned.Valid (E) or else Frame_Root_Count (E) = 0 then
               Add (Local_Metrics.Rejected); return;
            end if;
            Trace_Owner_Roots (E, Extra, Roots, Keep, Trace);
            if Trace /= Roots_Traced then Add (Local_Metrics.Rejected); return; end if;
            Before := AML_Objects.Usage_Of (E.Tree.Values);
            AML_Objects.Reclamation.Reclaim (E.Tree.Values, E.Token,
              AML_Objects.Reclamation.Keep_Set (Keep), Reclaim_Scratch, Reclaimed);
            if Reclaimed /= AML_Objects.Reclamation.Reclaimed then
               Gate := Reclamation_Failed; Add (Local_Metrics.Rejected); return;
            end if;
            After := AML_Objects.Usage_Of (E.Tree.Values);
            Add (Local_Metrics.Completed);
            Add (Local_Metrics.Freed_Objects, Before.Objects - After.Objects);
            Add (Local_Metrics.Freed_Bytes, Before.Bytes - After.Bytes);
            Add (Local_Metrics.Freed_Elements, Before.Elements - After.Elements);
            Gate := Allocation_Allowed;
         end Collect_Before;
         procedure Run is new Invoke_Retained_With_Hook (Collect_Before);
      begin
         Outcome := (Status => AML_Execute.No_Return, Charged => 0);
         Status := Admission (A); if Status /= Available then return; end if;
         if Node > Owned.Node_Count (A.Inner) or else not Owned.Present (A.Inner, Node)
           or else Owned.Kind (A.Inner, Node) /= Method_Object then
            Outcome := (Status => AML_Execute.Invalid_Method, Charged => 0); return;
         end if;
         for I in 1 .. Count loop
            case Args (I - 1).Kind is
               when Immediate_Argument =>
                  Raw_Args (I - 1) := (AML_Execute.Integer_Datum,
                     Args (I - 1).Number, AML_Decode.Ordinary_Integer);
               when Retained_Argument =>
                  Owned.Read_Retained (A.Inner, Args (I - 1).Handle.Root,
                     Raw_Args (I - 1), Read_Status);
                  if Read_Status /= AML_Execute.Returned then Status := Invalid_Value; return; end if;
            end case;
         end loop;
         A.In_Progress := True; Session_Active := True;
         -- Caller pins remain live throughout the sole collecting session.
         Run (A.Inner, Input, Node, Raw_Args, Count, Budget,
            Raw, Root, Retention);
         A.Collections := Local_Metrics;
         A.In_Progress := False;
         case Retention is
            when Result_Root_Limit => Status := Root_Limit;
            when Result_Identity_Exhausted => Status := Identity_Exhausted;
            when Invocation_Busy => Status := Busy;
            when Invalid_Result => Status := Invalid_Value;
            when Result_Retained | No_Root_Required => Status := Available;
         end case;
         case Raw.Status is
            when AML_Execute.Returned => Outcome :=
               (AML_Execute.Returned, Raw.Charged, Raw.Value, Raw.Origin);
            when AML_Execute.Object_Returned =>
               Outcome := (AML_Execute.Object_Returned, Raw.Charged, (Root => Root));
            when AML_Execute.Reference_Returned =>
               Outcome := (AML_Execute.Reference_Returned, Raw.Charged, (Root => Root));
            when others => Outcome := (Failure_Status (Raw.Status), Raw.Charged);
         end case;
      end Invoke;
      procedure Release (A : in out Arena; Handle : in out Value_Handle;
         Status : out Access_Status) is
         Released : Release_Status;
      begin
         Status := Admission (A); if Status /= Available then return; end if;
         Owned.Release (A.Inner, Handle.Root, Released);
         Status := (if Released = Owned.Released then Available else Invalid_Value);
      end Release;
      procedure Fetch (A : Arena; Handle : Value_Handle; Value : out AML_Execute.Datum;
         Status : out Access_Status) with Pre => not Value'Constrained is
         Read_Status : AML_Execute.Execution_Status;
      begin
         Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Status := Admission (A); if Status /= Available then return; end if;
         Owned.Read_Retained (A.Inner, Handle.Root, Value, Read_Status);
         if Read_Status /= AML_Execute.Returned then Status := Invalid_Value; end if;
      end Fetch;
      procedure Describe (A : Arena; Handle : Value_Handle;
         Description : out Value_Description; Status : out Access_Status) is
         Value : AML_Execute.Datum;
      begin
         Description := (Integer_Description, 0, AML_Decode.Ordinary_Integer);
         Fetch (A, Handle, Value, Status); if Status /= Available then return; end if;
         case Value.Value_Kind is
            when AML_Execute.Integer_Datum =>
               Description := (Integer_Description, Value.Number, Value.Origin);
            when AML_Execute.Reference_Datum =>
               Description := (Reference_Description, AML_References.Kind (Value.Ref));
            when AML_Execute.Object_Datum =>
               case Value.Object.Type_Code is
                  when 2 => Description := (String_Description, Value.Object.Size);
                  when 3 => Description := (Buffer_Description, Value.Object.Size);
                  when 4 => Description := (Package_Description, Value.Object.Size);
                  when others => Status := Wrong_Kind;
               end case;
         end case;
      end Describe;
      procedure Read_Bytes (A : Arena; Handle : Value_Handle; Offset : Natural;
         Data : out AML_Decode.Bytes; Copied : out Natural; Status : out Access_Status) is
         Value : AML_Execute.Datum;
      begin
         Data := [others => 0]; Copied := 0;
         Fetch (A, Handle, Value, Status); if Status /= Available then return; end if;
         if Value.Value_Kind /= AML_Execute.Object_Datum
           or else Value.Object.Type_Code not in 2 | 3 then Status := Wrong_Kind; return; end if;
         if Offset > Value.Object.Size then Status := Out_Of_Bounds; return; end if;
         Copied := Natural'Min (Data'Length, Value.Object.Size - Offset);
         for I in 1 .. Copied loop
            Data (Data'First + (I - 1)) := AML_Objects.Stored_Byte
               (A.Inner.Tree.Values, Value.Object.ID, Offset + (I - 1));
         end loop;
      end Read_Bytes;
      procedure Save (A : in out Arena; Value : AML_Execute.Datum;
         Handle : out Value_Handle; Status : out Access_Status) is
         Retention : Retain_Status;
      begin
         Handle := No_Value;
         Owned.Retain (A.Inner, Value, Handle.Root, Retention);
         case Retention is
            when Retained => Status := Available;
            when Owned.Invalid_Value => Status := Invalid_Value;
            when Owned.Root_Limit => Status := Root_Limit;
            when Owned.Identity_Exhausted => Status := Identity_Exhausted;
         end case;
      end Save;
      procedure Observe_Named_Value (A : in out Arena; Node : Node_ID;
         Outcome : out Result; Status : out Access_Status) is
         Source : AML_References.Object_Handle;
         Value : AML_Execute.Datum;
         Success : Boolean;
         Read_Status : AML_Execute.Execution_Status;
         Handle : Value_Handle;
      begin
         Outcome := (Status => AML_Execute.No_Return, Charged => 0);
         if A.In_Progress then Status := Busy; return;
         elsif A.State = Uninitialized then Status := Wrong_Phase; return; end if;
         Status := Available;
         if Node = Root or else not Present (A, Node) then Status := Invalid_Value; return; end if;
         if Owned.Kind (A.Inner, Node) = Uninitialized_Name_Object then
            Status := Uninitialized_Element; return;
         elsif Owned.Kind (A.Inner, Node) not in Integer_Object | String_Object |
           Buffer_Object | Package_Object | Reference_Object then Status := Wrong_Kind; return; end if;
         Owned.Make_Source (A.Inner, AML_Namespace.Data_Object (A.Inner.Tree, Node), Source, Success);
         if not Success then Status := Invalid_Value; return; end if;
         Owned.Read_Source (A.Inner, Source, Value, Read_Status);
         if Read_Status /= AML_Execute.Returned then Status := Invalid_Value; return; end if;
         if Value.Value_Kind = AML_Execute.Integer_Datum then
            Outcome := (AML_Execute.Returned, 0, Value.Number, Value.Origin);
         else
            if A.State /= Ready then Status := Wrong_Phase; return; end if;
            Save (A, Value, Handle, Status);
            if Status = Available then
               if Value.Value_Kind = AML_Execute.Reference_Datum then
                  Outcome := (AML_Execute.Reference_Returned, 0, Handle);
               else Outcome := (AML_Execute.Object_Returned, 0, Handle); end if;
            end if;
         end if;
      end Observe_Named_Value;
      procedure Read_Element (A : in out Arena; Handle : Value_Handle;
         Index : Natural; Element : out Value_Handle; Status : out Access_Status) is
         Value, Child : AML_Execute.Datum;
         ID : AML_Objects.Object_ID;
         Source : AML_References.Object_Handle;
         Success : Boolean;
         Read_Status : AML_Execute.Execution_Status;
      begin
         Element := No_Value;
         Fetch (A, Handle, Value, Status); if Status /= Available then return; end if;
         if Value.Value_Kind /= AML_Execute.Object_Datum
           or else Value.Object.Type_Code /= 4 then Status := Wrong_Kind; return; end if;
         if Index >= Value.Object.Size then Status := Out_Of_Bounds; return; end if;
         ID := AML_Objects.Element (A.Inner.Tree.Values, Value.Object.ID, Index);
         if ID = AML_Objects.No_Object then Status := Uninitialized_Element; return; end if;
         Owned.Make_Source (A.Inner, ID, Source, Success);
         if not Success then raise Program_Error with "collecting element identity invariant"; end if;
         Owned.Read_Source (A.Inner, Source, Child, Read_Status);
         if Read_Status /= AML_Execute.Returned then
            raise Program_Error with "collecting element read invariant";
         end if;
         -- This nonallocating, callback-free borrow cannot cross a safe point.
         Save (A, Child, Element, Status);
      end Read_Element;
      procedure Dereference (A : in out Arena; Handle : Value_Handle;
         Value : out Value_Handle; Status : out Access_Status) is
         Saved, Target : AML_Execute.Datum;
         Read_Status : AML_Execute.Execution_Status;
      begin
         Value := No_Value;
         Fetch (A, Handle, Saved, Status); if Status /= Available then return; end if;
         if Saved.Value_Kind /= AML_Execute.Reference_Datum then Status := Wrong_Kind; return; end if;
         if AML_References.Kind (Saved.Ref) = AML_References.Name_Member then
            Owned.Resolve_Name_Member (A.Inner, Saved.Ref, Target, Read_Status);
         else Owned.Resolve_Value (A.Inner, Saved.Ref, Target, Read_Status); end if;
         if Read_Status = AML_Execute.Uninitialized then Status := Uninitialized_Element;
         elsif Read_Status /= AML_Execute.Returned then Status := Invalid_Value;
         else Save (A, Target, Value, Status); end if;
      end Dereference;
   end Collecting;
end Owned;

end AML_Namespace;
