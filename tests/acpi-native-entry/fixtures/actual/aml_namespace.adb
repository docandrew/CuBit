pragma Ada_2022;
with AML_Table_Readers;
with Interfaces;
with ACPI_Resource_Names;
with AML_Field_Datums;
with ACPI_FADT.Transactions;
with AML_Mutexes;
with AML_Persistent_Owner_Graph;
with AML_Declaration_Contexts;
with AML_Container_Headers;
with AML_Table_Code;
with AML_Buffer_Bits;
with AML_Buffer_Layouts;
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
with AML_Fields;
with AML_Coercions;
with AML_Metadata_Kinds;
package body AML_Namespace with SPARK_Mode is
   use type AML_Table_Readers.Selection_Status;
   use type AML_Execute.Method_Implementation;
   -- Semantic by-value snapshot at package elaboration. AML and later mutation
   -- of the generic actual cannot alter this owner's interface policy.
   Interface_Policy : constant AML_Interface_Policy.Policy := Supported_Interfaces;
   function Backing_Byte_Length (Input : aliased AML_Table_Backing.State) return Natural is
     (Input.Data.Bytes'Length) with Global => null;
   function Backing_Byte (Input : aliased AML_Table_Backing.State; Index : Natural)
      return AML_Decode.Byte is (Input.Data.Bytes (Input.Data.Bytes'First + Index))
      with Global => null, Pre => Index < Input.Data.Bytes'Length;
   package Table_Buffer_Initializers is new AML_Objects.Bit_Buffer_Initializers
     (AML_Table_Backing.State, Backing_Byte_Length, Backing_Byte);
   use type AML_Execute.Datum_Kind;
   use type AML_Execute.Metadata_Mode;
   use type AML_Execute.Reference_Metadata_Kind;
   use type AML_Metadata_Kinds.Snapshot_Description;
   use type AML_Metadata_Kinds.Kind;
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
   function Definition_Of (Tree : State; Node : Node_ID) return Definition_Identity is
      Current : Node_ID := Node;
   begin
      -- Temporary declaration ownership is already an acyclic forest.
      -- Follow it to the persistent declaration rather than the lexical parent.
      while Current /= Root and then Tree.Items (Current).Owner /= Root loop
         pragma Loop_Variant (Decreases => Current);
         Current := Tree.Items (Current).Owner;
      end loop;
      return (if Current = Root then No_Definition else Tree.Items (Current).Definition);
   end Definition_Of;
   procedure Enter_Definition (Tree : in out State;
      Prior, Definition : out Definition_Identity; Accepted : out Boolean) is
      Token : AML_Identity.Identity;
   begin
      Prior := Tree.Current_Definition; Definition := No_Definition;
      AML_Identity.Issuer.Issue (Token, Accepted);
      if Accepted then
         Definition := Definition_Identity (Token); Tree.Current_Definition := Definition;
      end if;
   end Enter_Definition;
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
     (Tree.Current_Definition = Prior.Current_Definition and then Tree.Root_Value = Prior.Root_Value and then Tree.Last_Stamp = Prior.Last_Stamp
      and then Tree.Used = Prior.Used and then Tree.Items = Prior.Items
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

   function Canonical_Node (Tree : State; Node : Node_ID) return Lookup_Result is
      Target : Node_ID;
   begin
      if Node = Root then return (Found, Root); end if;
      if not Tree.Items (Node).Alive then return (Status => Not_Found); end if;
      if Tree.Items (Node).Object_Type /= Alias_Object then return (Found, Node); end if;
      Target := Tree.Items (Node).Alias_Target;
      if Target = Root then
         if Tree.Items (Node).Alias_Stamp = 0 then return (Found, Root); end if;
      elsif Target <= Tree.Used and then Tree.Items (Target).Alive
        and then Tree.Items (Target).Incarnation = Tree.Items (Node).Alias_Stamp
        and then Tree.Items (Target).Object_Type /= Alias_Object
      then return (Found, Target);
      end if;
      return (Status => Not_Found);
   end Canonical_Node;
   function Canonical_Child
     (Tree : State; Scope : Node_ID; Part : AML_Names.Segment) return Lookup_Result
     with Pre => Scope <= Count (Tree)
   is
      Node : constant Node_ID := Child (Tree, Scope, Part);
   begin
      if Node = Root then return (Status => Not_Found); end if;
      return Canonical_Node (Tree, Node);
   end Canonical_Child;

   function Resolve
     (Tree : State; Scope : Node_ID; Path : AML_Names.Name_Result)
      return Lookup_Result
   is
      Base : Node_ID := Scope;
      Next, Prior_Binding : Node_ID := Root;
      Canonical : Lookup_Result;
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
      else
         Canonical := Canonical_Node (Tree, Base);
         if Canonical.Status /= Found then return Canonical; end if;
         Base := Canonical.Node;
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
           (if I > 1 then Prior_Binding /= Root
              and then Name (Tree, Prior_Binding) = Path.Parts (I - 1)
              and then Canonical_Node (Tree, Prior_Binding).Status = Found
              and then Canonical_Node (Tree, Prior_Binding).Node = Base);
         Next := Child (Tree, Base, Path.Parts (I));
         if Next = Root then
            return (Status => Not_Found);
         end if;
         if I = Path.Count then return (Status => Found, Node => Next); end if;
         Prior_Binding := Next;
         Canonical := Canonical_Node (Tree, Next);
         if Canonical.Status /= Found then return Canonical; end if;
         Base := Canonical.Node;
      end loop;
      return (Status => Found, Node => Base);
   end Resolve;

   function Resolve_Object
     (Tree : State; Scope : Node_ID; Path : AML_Names.Name_Result) return Lookup_Result
   is
      Binding : constant Lookup_Result := Resolve (Tree, Scope, Path);
   begin
      if Binding.Status /= Found then return Binding; end if;
      return Canonical_Node (Tree, Binding.Node);
   end Resolve_Object;

   function Insert_Frame (Tree, Prior : State) return Boolean is
     (Tree.Current_Definition = Prior.Current_Definition and then Tree.Root_Value = Prior.Root_Value and then Tree.Values = Prior.Values and then Tree.Code = Prior.Code
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
         Tree.Items (Tree.Used) := (Up => Scope, Part => Part, Incarnation => Tree.Last_Stamp,
           Definition => Tree.Current_Definition, others => <>);
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
        Field.Region.Item.Kind = AML_Table_Sources.No_Source or else
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
     ((Tree.Root_Value = 0 or else
          (AML_Objects.Is_Live (Tree.Values, Tree.Root_Value) and then
           AML_Objects.Kind (Tree.Values, Tree.Root_Value) in
             AML_Objects.Integer_Object | AML_Objects.String_Object |
             AML_Objects.Buffer_Object | AML_Objects.Package_Object |
             AML_Objects.Reference_Object | AML_Objects.Metadata_Snapshot_Object)) and then
       AML_Module_Frames.Valid (Tree.Module_Frames) and then
       Tree.Last_Stamp <= Max_Node_Incarnation and then Pending.Valid (Tree.Journal) and then AML_Objects.Valid (Tree.Values) and then
       (for all I in 1 .. Tree.Used => Tree.Items (I).Incarnation > 0
          and then Tree.Items (I).Incarnation <= Tree.Last_Stamp and then Tree.Items (I).Up < I and then Tree.Items (I).Owner < I and then
          (if Tree.Items (I).Initializing then
             Tree.Items (I).Alive and then
             (if Tree.Items (I).Owner = Root then
                AML_Module_Frames.Contains (Tree.Module_Frames, Tree.Items (I).Initializer_Frame)
              else Tree.Items (Tree.Items (I).Owner).Alive
                and then Tree.Items (Tree.Items (I).Owner).Object_Type = Method_Object
                and then Tree.Items (Tree.Items (I).Owner).Active_Calls > 0)
             and then (Tree.Items (I).Object_Type in Uninitialized_Name_Object | Integer_Object
               | String_Object | Buffer_Object | Package_Object | Reference_Object | Metadata_Value_Object
               or else (Tree.Items (I).Owner = Root
                 and then Tree.Items (I).Object_Type = Uninitialized_Region_Object))) and then
          (if Tree.Items (I).Object_Type = Alias_Object then
             Tree.Items (I).Object_Ref = 0
             and then Tree.Items (I).Alias_Target <= Tree.Used
             and then (if Tree.Items (I).Alias_Target = Root then Tree.Items (I).Alias_Stamp = 0
               else Tree.Items (I).Alias_Stamp > 0 and then Tree.Items (I).Alias_Stamp <= Tree.Last_Stamp)) and then
          (if Tree.Items (I).Object_Type = Uninitialized_Name_Object then
             Tree.Items (I).Object_Ref = 0
             and then (not Tree.Items (I).Alive or else Tree.Items (I).Initializing)) and then
          (if Tree.Items (I).Alive and then Tree.Items (I).Object_Type = Buffer_Field_Object then
             Tree.Items (I).Object_Ref = 0
             and then Tree.Items (I).Buffer_Field_Info.Bits > 0
             and then AML_Objects.Matches_Address (Tree.Values,
               AML_References.Address (Tree.Items (I).Buffer_Field_Info.Backing))
             and then AML_Objects.Kind (Tree.Values,
               AML_References.Source (Tree.Items (I).Buffer_Field_Info.Backing)) = AML_Objects.Buffer_Object) and then
          (if Tree.Items (I).Object_Type = Region_Field_Object then
             Valid_Field_Descriptor (Tree.Items, I)) and then
          (if Tree.Items (I).Object_Type = Table_Field_Object then
             Tree.Items (I).Table_Binding.Region.Item.Kind /= AML_Table_Sources.No_Source
             and then AML_Field_Data.Fits (Tree.Items (I).Table_Binding.Region.Extent,
               Tree.Items (I).Table_Binding.Offset, Tree.Items (I).Table_Binding.Bits)) and then
          (if Tree.Items (I).Implementation = AML_Execute.Interface_Query then
             Tree.Items (I).Alive and then Tree.Items (I).Object_Type = Method_Object
             and then Tree.Items (I).Owner = Root and then Tree.Items (I).Up = Root
             and then Tree.Items (I).Part = "_OSI" and then Natural (Tree.Items (I).Method_Flags) = 1
             and then Tree.Items (I).Method_Size = 0 and then Tree.Items (I).Method_Offset = 0) and then
          Tree.Items (I).Method_Offset <= Tree.Code_Used and then
          Tree.Items (I).Method_Size <= Tree.Code_Used - Tree.Items (I).Method_Offset and then
          (if Tree.Items (I).Object_Type = Mutex_Object and then Tree.Items (I).Object_Ref /= AML_Objects.No_Object then
             AML_Objects.Is_Live (Tree.Values, Tree.Items (I).Object_Ref) and then
             AML_Objects.Kind (Tree.Values, Tree.Items (I).Object_Ref) = AML_Objects.Mutex_Object and then
             Natural (AML_Objects.Mutex_Flags (Tree.Values, Tree.Items (I).Object_Ref)) = Natural (Tree.Items (I).Mutex_Flags)) and then
          (if Tree.Items (I).Object_Type in Integer_Object | String_Object | Buffer_Object | Package_Object | Reference_Object | Metadata_Value_Object | Event_Object then
              AML_Objects.Is_Live (Tree.Values, Tree.Items (I).Object_Ref) and then
              AML_Objects.Kind (Tree.Values, Tree.Items (I).Object_Ref) =
                (case Tree.Items (I).Object_Type is
                   when Integer_Object => AML_Objects.Integer_Object,
                   when String_Object => AML_Objects.String_Object,
                   when Buffer_Object => AML_Objects.Buffer_Object,
                   when Reference_Object => AML_Objects.Reference_Object,
                   when Metadata_Value_Object => AML_Objects.Metadata_Snapshot_Object,
                   when Event_Object => AML_Objects.Event_Object,
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
      Located := Resolve_Object (Environment.Tree, Environment.Scope, Path);
      if Located.Status /= Found or else Located.Node = Root or else not Has_Integer (Environment.Tree, Located.Node) then
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
      Located := Resolve_Object (Environment.Tree, Environment.Scope, Path);
      if Located.Status /= Found or else Located.Node = Root or else Kind (Environment.Tree, Located.Node)
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
   -- Static and runtime package names are recorded before binding. Runtime
   -- callers bind on their detached candidate after the complete parse.
   package String_Reader is new AML_Objects.String_Initializers
     (AML_Byte_Sources.Storage, AML_Byte_Sources.Source_Length, AML_Byte_Sources.Source_Byte);
   package String_Conversion is new AML_Coercions.String_Readers
     (AML_Byte_Sources.Storage, AML_Byte_Sources.Source_Length, AML_Byte_Sources.Source_Byte);
   procedure Load_Bound_Data is new AML_Data.Load_Bound_From_Source
     (Count_Context, Read_Package_Count, Defer_Package_Member);

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
      Located := Resolve_Object (Tree, Node_ID (Scope), Path);
      if Located.Status /= Found then
         return (Status => AML_Execute.Missing_Binding);
      elsif Located.Node = Root then
         return (Status => AML_Execute.Non_Integer_Binding,
           Object => (Type_Code => AML_Execute.Named_Metadata_Type'Enum_Rep (AML_Execute.Device_Metadata), others => <>));
      elsif Kind (Tree, Located.Node) in Uninitialized_Region_Object | Uninitialized_Table_Field_Object | Uninitialized_Name_Object then
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
                   when Event_Object => 7, when Mutex_Object => AML_Mutexes.Object_Type_Code,
                   when Power_Resource_Object => 11, when Processor_Object => 12,
                   when Thermal_Zone_Object => 13, when Buffer_Field_Object => 14,
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
      if Tree.Items (ID).Implementation = AML_Execute.Interface_Query then
         return (Exists => True, Implementation => AML_Execute.Interface_Query,
           Length => 0, Code => (Length => 0, Bytes => [1 .. 0 => 0]), Width => AML_Decode.Bits_64,
           Flags => Tree.Items (ID).Method_Flags, Scope => ID);
      end if;
      return (Exists => True, Implementation => AML_Execute.AML_Implementation,
              Code => (Length => Tree.Items (ID).Method_Size,
                       Bytes => Method_Data (Tree, Node_ID (ID))),
              Length => Tree.Items (ID).Method_Size,
              Width => Tree.Items (ID).Method_Width,
              Flags => Tree.Items (ID).Method_Flags, Scope => ID);
   end Read_Method;

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
      Located := Resolve_Object (Tree, Node_ID (Scope), Path);
      if Located.Status /= Found then Status := AML_Execute.Write_Missing; return; end if;
      if Located.Node = Root then return; end if;
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
          Package_Object | Reference_Object | Metadata_Value_Object | Buffer_Field_Object then
        (Item with delta Alive => False, Initializing => False,
          Object_Type => Scope_Object, Object_Ref => 0)
      else (Item with delta Alive => False, Initializing => False));

   procedure End_Method (Tree : in out State; Scope : Natural)
     with Pre => Valid_Context (Tree),
       Post => Valid_Context (Tree) and then Tree.Root_Value = Tree'Old.Root_Value
         and then Tree.Values = Tree'Old.Values
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

   function Declaration_Scopes_Valid
     (Tree : State; Lexical_Scope, Lifetime_Owner : Natural; Module_Declaration : Boolean)
      return Boolean is
     (Lexical_Scope <= Tree.Used
      and then (Lexical_Scope = Root or else Tree.Items (Node_ID (Lexical_Scope)).Alive)
      and then (if Module_Declaration then
        Lifetime_Owner = Root and then AML_Frame_Handles.Present (AML_Module_Frames.Top (Tree.Module_Frames))
       else Lifetime_Owner > Root and then Lifetime_Owner <= Tree.Used
        and then Tree.Items (Node_ID (Lifetime_Owner)).Alive
        and then Tree.Items (Node_ID (Lifetime_Owner)).Object_Type = Method_Object
        and then Tree.Items (Node_ID (Lifetime_Owner)).Active_Calls > 0));

   procedure Name_Target
     (Tree : State; Scope, Lifetime_Owner : Natural; Path : AML_Names.Name_Result;
      Base : out Node_ID; Status : out AML_Execute.Execution_Status;
      Module_Declaration : Boolean := False)
   is
   begin
      Base := Root; Status := AML_Execute.Bad_Name;
      if not Declaration_Scopes_Valid (Tree, Scope, Lifetime_Owner, Module_Declaration)
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
         declare Binding : constant Lookup_Result := Canonical_Child (Tree, Base, Path.Parts (I)); begin
            if Binding.Status /= Found then return; end if;
            Base := Binding.Node;
         end;
      end loop;
      if not Declaration_Scope (Kind (Tree, Base)) then return; end if;
      if Child (Tree, Base, Path.Parts (Path.Count)) /= Root then
         Status := AML_Execute.Duplicate_Name; return;
      end if;
      Status := AML_Execute.Returned;
   end Name_Target;

   Field_Byte_Bits : constant Positive := Natural (AML_Buffer_Bits.Bits_Per_Byte);
   Max_Field_Bits : constant Natural := AML_Objects.Max_Bytes * Field_Byte_Bits;
   subtype Field_Bit_Count is Positive range 1 .. Max_Field_Bits;
   Integer_32_Bits : constant Positive := 32;
   Integer_64_Bits : constant Positive := 64;
   function Field_Integer_Bits (Width : AML_Decode.Integer_Width) return Positive is
     (case Width is when AML_Decode.Bits_32 => Integer_32_Bits,
       when AML_Decode.Bits_64 => Integer_64_Bits);
   type Field_Result_Kind is (Scalar_Field, Buffer_Field);
   type Field_Result (Kind : Field_Result_Kind := Scalar_Field) is record
      case Kind is
         when Scalar_Field => Number : AML_Decode.Integer_Value := 0;
         when Buffer_Field => Object : AML_Objects.Object_ID := 0;
      end case;
   end record;

   -- Internal authority boundary for direct reads and package captures. The
   -- token is supplied only by an owning Arena; State-only callers have none.
   procedure Extract_Buffer_Field
     (Tree : in out State; Owner : AML_Identity.Identity; Node : Node_ID;
      Width : AML_Decode.Integer_Width; Result : out Field_Result;
      Status : out AML_Execute.Execution_Status)
     with Pre => Valid_Context (Tree) and then not Result'Constrained,
       Post => Valid_Context (Tree)
         and then (if Status /= AML_Execute.Returned then Tree = Tree'Old)
   is
      use type AML_Identity.Identity;
      use type AML_Decode.Byte;
      use type AML_Objects.Allocation_Status;
   begin
      Result := (Scalar_Field, 0);
      Status := AML_Execute.Unsupported_Value;
      if Owner = AML_Identity.No_Identity or else Node = Root
        or else Node > Tree.Used or else not Tree.Items (Node).Alive
        or else Tree.Items (Node).Object_Type /= Buffer_Field_Object
      then return; end if;
      declare
         Info : constant Buffer_Field_Descriptor := Tree.Items (Node).Buffer_Field_Info;
         ID : constant AML_Objects.Object_ID := AML_References.Source (Info.Backing);
      begin
         if not AML_References.Belongs_To (Info.Backing, Owner)
           or else not AML_Objects.Matches_Address
             (Tree.Values, AML_References.Address (Info.Backing))
           or else AML_Objects.Kind (Tree.Values, ID) /= AML_Objects.Buffer_Object
         then return; end if;
         if Info.Offset > AML_Decode.Integer_Value (Max_Field_Bits)
           or else Info.Bits > AML_Decode.Integer_Value (Max_Field_Bits)
           or else not AML_Buffer_Bits.Fits
             (AML_Objects.Length (Tree.Values, ID), Info.Offset, Info.Bits)
         then Status := AML_Execute.Buffer_Field_Limit; return; end if;
         declare
            First : constant Natural := Natural (Info.Offset);
            Bits : constant Field_Bit_Count := Field_Bit_Count (Info.Bits);
            Data : AML_Decode.Bytes (1 .. (Bits - 1) / Field_Byte_Bits + 1) := [others => 0];
         begin
            for K in 0 .. Bits - 1 loop
               if AML_Buffer_Bits.Bit_Set
                 (AML_Objects.Stored_Byte (Tree.Values, ID, (First + K) / Field_Byte_Bits),
                  (First + K) mod Field_Byte_Bits)
               then
                  Data (1 + K / Field_Byte_Bits) := Data (1 + K / Field_Byte_Bits)
                    or AML_Buffer_Bits.Mask (K mod Field_Byte_Bits);
               end if;
            end loop;
            if not Info.Always_Buffer and then Bits <= Field_Integer_Bits (Width) then
               for K in 0 .. Data'Length - 1 loop
                  Result.Number := Result.Number or AML_Decode.Integer_Value (Data (1 + K))
                    * 2 ** (K * Field_Byte_Bits);
               end loop;
            else
               declare
                  Candidate : AML_Objects.State := Tree.Values;
                  Result_ID : AML_Objects.Object_ID;
                  Allocated : AML_Objects.Allocation_Status;
               begin
                  AML_Objects.New_Bytes (Candidate, AML_Objects.Buffer_Object, Data, Result_ID, Allocated);
                  if Allocated /= AML_Objects.Allocated then Status := AML_Execute.Value_Limit; return; end if;
                  Tree.Values := Candidate;
                  Result := (Buffer_Field, Result_ID);
               end;
            end if;
            Status := AML_Execute.Returned;
         end;
      end;
   end Extract_Buffer_Field;

   function Namespace_Metadata_Kind (Tag : Object_Kind) return Boolean is
     (Tag in Scope_Object | Device_Object | Thermal_Zone_Object | Processor_Object |
       Power_Resource_Object | Event_Object | Mutex_Object);
   function Package_Metadata_Kind (Tag : Object_Kind) return Boolean is
     (Tag = Method_Object or else Namespace_Metadata_Kind (Tag));
   function Metadata_Identity_At
     (Tree : State; Owner : AML_Identity.Identity; Node : Node_ID)
      return AML_References.Reference is
     (if Node = Root then AML_References.Bind_Root (Owner)
      else AML_References.Bind_Named (Owner, AML_References.Node_Position (Node),
        Tree.Items (Node).Incarnation))
     with Pre => Node <= Tree.Used;
   procedure Bind_Runtime_Package_Members
     (Candidate : in out State; Prior_Values : AML_Objects.State;
      Journal : Pending.State; Owner : AML_Identity.Identity;
      Width : AML_Decode.Integer_Width; Self_Node : Node_ID;
      Allow_Reference : Boolean; Status : out AML_Execute.Execution_Status)
   is
      use type AML_Identity.Identity;
      use type AML_Objects.Allocation_Status;
   begin
      Status := AML_Execute.Unsupported_Value;
      for I in 1 .. Pending.Count (Journal) loop
         declare
            Member : constant Pending.Member_Result := Pending.Item (Journal, I);
            Located : Lookup_Result;
            Leaf : AML_Objects.Object_ID;
            Allocated : AML_Objects.Allocation_Status;
            Extracted : Field_Result;
         begin
            if not Member.Found or else Member.Scope > Candidate.Used
              or else AML_Objects.Is_Live (Prior_Values, Member.Package_ID)
              or else not AML_Objects.Is_Live (Candidate.Values, Member.Package_ID)
              or else AML_Objects.Kind (Candidate.Values, Member.Package_ID) /= AML_Objects.Package_Object
              or else Member.Element >= AML_Objects.Length (Candidate.Values, Member.Package_ID)
            then return; end if;
            Located := Resolve_Object (Candidate, Member.Scope, Member.Path);
            if Located.Status /= Found then return; end if;
            if Located.Node = Root or else Kind (Candidate, Located.Node) = Metadata_Value_Object
                 or else Package_Metadata_Kind (Kind (Candidate, Located.Node)) then
               if Owner = AML_Identity.No_Identity then return; end if;
               AML_Objects.New_Metadata (Candidate.Values,
                 Metadata_Identity_At (Candidate, Owner, Located.Node), Leaf, Allocated);
               if Allocated /= AML_Objects.Allocated then Status := AML_Execute.Value_Limit; return; end if;
            elsif Self_Node /= Root and then Located.Node = Self_Node then
               if Owner = AML_Identity.No_Identity then return; end if;
               -- The runtime self path defers NAME resolution: the node is
               -- resolved later, but no object was captured at NAME creation.
               AML_Objects.New_Reference (Candidate.Values,
                 AML_References.Bind_Name_Member (Owner, AML_References.Node_Position (Self_Node),
                   Candidate.Items (Self_Node).Incarnation,
                   AML_References.No_Object_Handle), Leaf, Allocated);
               if Allocated /= AML_Objects.Allocated then
                  Status := AML_Execute.Value_Limit; return;
               end if;
            elsif Candidate.Items (Located.Node).Object_Type = Buffer_Field_Object then
               Extract_Buffer_Field (Candidate, Owner, Located.Node, Width, Extracted, Status);
               if Status /= AML_Execute.Returned then return; end if;
               -- Restore the failure default before processing later members.
               Status := AML_Execute.Unsupported_Value;
               case Extracted.Kind is
                  when Scalar_Field =>
                     AML_Objects.New_Integer (Candidate.Values, Extracted.Number, Leaf, Allocated);
                     if Allocated /= AML_Objects.Allocated then
                        Status := AML_Execute.Value_Limit; return;
                     end if;
                  when Buffer_Field => Leaf := Extracted.Object;
               end case;
            elsif Candidate.Items (Located.Node).Object_Type in
              Integer_Object | String_Object | Buffer_Object | Package_Object
              or else (Allow_Reference and then
                Candidate.Items (Located.Node).Object_Type = Reference_Object)
            then Leaf := Candidate.Items (Located.Node).Object_Ref;
            else return;
            end if;
            AML_Objects.Set_Element (Candidate.Values, Member.Package_ID, Member.Element, Leaf);
         end;
      end loop;
      Status := AML_Execute.Returned;
   end Bind_Runtime_Package_Members;

   procedure Define_Runtime_Name_For_Context
     (Tree : in out State; Owner : AML_Identity.Identity; Scope, Lifetime_Owner : Natural; Path : AML_Names.Name_Result;
      Width : AML_Decode.Integer_Width; Data : AML_Decode.Bytes;
         Source : aliased AML_Byte_Sources.Storage; Source_First : Natural;
      Consumed : out Natural; Status : out AML_Execute.Execution_Status;
      Module_Declaration : Boolean := False)
     with Pre => Valid_Context (Tree),
       Post => Valid_Context (Tree) and then Consumed <= Data'Length
         and then (if Status /= AML_Execute.Returned then Tree = Tree'Old and Consumed = 0
           else Consumed > 0 and then Tree.Used = Tree'Old.Used + 1
             and then Tree.Items (Tree.Used).Owner = Lifetime_Owner
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
   begin
      Consumed := 0;
      Name_Target (Tree, Scope, Lifetime_Owner, Path, Base, Status, Module_Declaration);
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
      if not Module_Declaration then Environment.Tree.Journal := Pending.Empty; end if;
      Load_Bound_Data (Candidate.Values, Data, Source, Source_First, Width, Environment, ID, Used, Parsed);
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
           when AML_Objects.Event_Object => Event_Object,
           when AML_Objects.Mutex_Object => Mutex_Object,
           when AML_Objects.Reference_Object => Reference_Object,
           when AML_Objects.Metadata_Object | AML_Objects.Metadata_Snapshot_Object => Metadata_Value_Object);
      Candidate.Items (Node).Owner := Node_ID (Lifetime_Owner);
      Candidate.Items (Node).Object_Ref := ID;
      if Candidate.Items (Node).Object_Type = Mutex_Object then
         Candidate.Items (Node).Mutex_Flags := AML_Objects.Mutex_Flags (Candidate.Values, ID);
      end if;
      -- The reserved node becomes resolvable only inside this valid candidate.
      -- Bind deferred package paths after attaching the completed root object,
      -- allowing a package to refer to itself without publishing partial state.
      if Module_Declaration then
         Candidate.Journal := Environment.Tree.Journal;
      else
         Bind_Runtime_Package_Members
           (Candidate, Tree.Values, Environment.Tree.Journal, Owner, Width,
            Node, False, Status);
         if Status /= AML_Execute.Returned then return; end if;
      end if;
      Tree := Candidate; Consumed := Used; Status := AML_Execute.Returned;
   end Define_Runtime_Name_For_Context;

   procedure Define_Method_For_Context
     (Tree : in out State; Scope, Lifetime_Owner : Natural; Path : AML_Names.Name_Result;
      Flags : AML_Decode.Byte; Width : AML_Decode.Integer_Width;
      Code : AML_Decode.Bytes; Status : out AML_Execute.Declaration_Status;
      Module_Declaration : Boolean := False)
     with Pre => Valid_Context (Tree),
       Post => Valid_Context (Tree) and then Tree.Values = Tree'Old.Values
         and then (if Status /= AML_Execute.Declared then Tree = Tree'Old
           else Tree.Used = Tree'Old.Used + 1 and then
             Tree.Items (Tree.Used).Owner = Lifetime_Owner and then Tree.Items (Tree.Used).Alive)
   is
      Base, Node : Node_ID;
      Added : Insert_Status;
      Start : Aggregate_Method_Count;
   begin
      Status := AML_Execute.Declaration_Unsupported;
      if not Declaration_Scopes_Valid (Tree, Scope, Lifetime_Owner, Module_Declaration)
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
         declare Binding : constant Lookup_Result := Canonical_Child (Tree, Base, Path.Parts (I)); begin
            if Binding.Status /= Found then Status := AML_Execute.Declaration_Missing; return; end if;
            Base := Binding.Node;
         end;
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
      Tree.Items (Node).Owner := Node_ID (Lifetime_Owner);
      Tree.Items (Node).Object_Type := Method_Object;
      Tree.Items (Node).Method_Offset := Start;
      Tree.Items (Node).Method_Size := Code'Length;
      Tree.Items (Node).Method_Flags := Flags;
      Tree.Items (Node).Method_Width := Width;
      Status := AML_Execute.Declared;
   end Define_Method_For_Context;
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
   begin
      Define_Method_For_Context (Tree, Scope, Scope, Path, Flags, Width, Code, Status);
   end Define_Method;
   -- Each maintained procedural invocation owns this sidecar. No state or
   -- access value escapes; nested Dispatch uses the same package instance.
   generic
   package Method_Session is
      procedure Start (Ready : out Boolean);
      procedure Finish (Tree : State);
      procedure Admit (Tree : in out State;
        Domain : out AML_Frame_Handles.Invocation_Domain;
        Status : out AML_Execute.Invocation_Status);
      procedure Open_Frame (Tree : in out State; Frame : AML_Frame_Handles.Frame_Handle);
      procedure Close_Frame (Tree : in out State; Frame : AML_Frame_Handles.Frame_Handle);
      procedure Enter_Method (Tree : in out State; Method : Natural;
        Frame : AML_Frame_Handles.Frame_Handle; Status : in out AML_Execute.Execution_Status);
      procedure Leave_Method (Tree : in out State; Frame : AML_Frame_Handles.Frame_Handle;
        Status : in out AML_Execute.Execution_Status);
   end Method_Session;
   package body Method_Session is
      use type AML_Identity.Identity;
      type Method_Key is record
         Node : Node_ID := Root;
         Stamp : AML_References.Node_Incarnation := 0;
         Flags, Offset, Size : Natural := 0;
      end record;
      No_Method : constant Method_Key := (others => <>);
      type Admission is record
         Owner : AML_Identity.Identity := AML_Identity.No_Identity;
         Domain : AML_Frame_Handles.Invocation_Domain := AML_Frame_Handles.No_Domain;
         Frame : AML_Frame_Handles.Frame_Handle := AML_Frame_Handles.No_Frame;
         Method : Method_Key := No_Method;
         Level : Natural := 0;
         Registry_OK, Method_OK, Frame_OK, Rotating, Leaving : Boolean := False;
      end record;
      function Domain_OK (C : Admission; Owner : AML_Identity.Identity;
        Domain : AML_Frame_Handles.Invocation_Domain) return Boolean is
        (C.Registry_OK and then C.Owner = Owner and then C.Domain = Domain) with Global => null;
      function Object_OK (C : Admission; Owner : AML_Identity.Identity;
        Address : AML_Object_Identifiers.Object_Address; Level : Natural) return Boolean
        with Global => null is
         pragma Unreferenced (C, Owner, Address, Level);
      begin return False; end Object_OK;
      function Method_OK (C : Admission; Owner : AML_Identity.Identity;
        Key : Method_Key; Level : Natural) return Boolean is
        (C.Owner = Owner and then C.Registry_OK and then
          (if C.Leaving then True else C.Method_OK and then C.Method = Key and then C.Level = Level))
        with Global => null;
      function Frame_OK (C : Admission; Frame : AML_Frame_Handles.Frame_Handle) return Boolean is
        (C.Frame_OK and then C.Frame = Frame) with Global => null;
      function Epoch_OK (C : Admission; Old_Owner, New_Owner : AML_Identity.Identity) return Boolean is
        (C.Rotating and then C.Registry_OK and then Old_Owner = AML_Identity.No_Identity
          and then New_Owner = C.Owner) with Global => null;
      package Locks is new AML_Mutex_Invocations
        (Max_Explicit_Locks => 1, Max_Method_Locks => AML_Frame_Handles.Max_Active_Frames,
         Admission => Admission, Method_Key => Method_Key, No_Method => No_Method,
         Domain_Approved => Domain_OK, Mutex_Approved => Object_OK,
         Method_Approved => Method_OK, Frame_Approved => Frame_OK, Epoch_Approved => Epoch_OK);
      use type Locks.Outcome;
      use type Locks.Method_Token;
      subtype Slot is AML_Frame_Handles.Frame_Index range 1 .. AML_Frame_Handles.Max_Active_Frames;
      type Frame_Array is array (Slot) of AML_Frame_Handles.Frame_Handle;
      type Token_Array is array (Slot) of Locks.Method_Token;
      Registry : Locks.State;
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      Domain : AML_Frame_Handles.Invocation_Domain := AML_Frame_Handles.No_Domain;
      Frames : Frame_Array := [others => AML_Frame_Handles.No_Frame];
      Tokens : Token_Array := [others => Locks.No_Token];
      Admitted : Boolean := False;
      function Key_Of (Tree : State; Node : Node_ID) return Method_Key is
        ((Node, Tree.Items (Node).Incarnation, Natural (Tree.Items (Node).Method_Flags),
          Natural (Tree.Items (Node).Method_Offset), Tree.Items (Node).Method_Size));
      function Live_Key (Tree : State; Key : Method_Key) return Boolean is
        (Key.Node /= Root and then Key.Node <= Tree.Used and then Tree.Items (Key.Node).Alive
          and then Tree.Items (Key.Node).Object_Type = Method_Object
          and then Key_Of (Tree, Key.Node) = Key);
      function Live (Tree : State) return Boolean is
      begin
         if not Valid_Context (Tree) or else not Locks.Valid (Registry)
           or else Locks.Owner_Of (Registry) /= Owner then return False; end if;
         for I in 1 .. Locks.Method_Count (Registry) loop
            if not Live_Key (Tree, Locks.Method_At (Registry, I)) then return False; end if;
         end loop;
         for I in Slot loop
            if Tokens (I) /= Locks.No_Token and then
              (Frames (I) = AML_Frame_Handles.No_Frame
               or else Locks.Token_Frame (Tokens (I)) /= Frames (I)) then return False; end if;
         end loop;
         return True;
      end Live;
      function Is_Frame (Frame : AML_Frame_Handles.Frame_Handle) return Boolean is
        (AML_Frame_Handles.Belongs_To (Frame, Domain)
          and then AML_Frame_Handles.Index_Of (Frame) in Slot
          and then Frames (AML_Frame_Handles.Index_Of (Frame)) = Frame);
      function Evidence (Tree : State) return Admission is
        ((Owner => Owner, Domain => Domain, Registry_OK => Live (Tree), others => <>));
      function Result_Status (Value : Locks.Outcome) return AML_Execute.Execution_Status is
        (case Value is
           when Locks.Succeeded => AML_Execute.Returned,
           when Locks.Order_Error => AML_Execute.Mutex_Order,
           when Locks.Capacity_Limit | Locks.Depth_Limit | Locks.Token_Limit => AML_Execute.Value_Limit,
           when others => AML_Execute.Unsupported_Value);
      procedure Start (Ready : out Boolean) is
         C : Admission;
         Result : Locks.Outcome;
      begin
         Ready := False;
         if Owner /= AML_Identity.No_Identity then return; end if;
         AML_Identity.Issuer.Issue (Owner, Ready);
         if not Ready then return; end if;
         C := (Owner => Owner, Registry_OK => Locks.Valid (Registry), Rotating => True, others => <>);
         Locks.Rotate_Epoch (Registry, C, Owner, Result);
         Ready := Result = Locks.Succeeded;
         if Ready then Domain := AML_Frame_Handles.Bind_Domain (Owner, 1); end if;
      end Start;
      procedure Admit (Tree : in out State;
        Domain : out AML_Frame_Handles.Invocation_Domain;
        Status : out AML_Execute.Invocation_Status) is
         Result : Locks.Outcome;
      begin
         Domain := AML_Frame_Handles.No_Domain;
         Status := AML_Execute.Unsupported_Context;
         if Admitted or else not Live (Tree)
           or else not AML_Frame_Handles.Has_Authority (Method_Session.Domain) then return; end if;
         Locks.Begin_Invocation (Registry, Evidence (Tree), Owner, 1, Method_Session.Domain, Result);
         if Result = Locks.Succeeded then
            Admitted := True; Domain := Method_Session.Domain; Status := AML_Execute.Available;
         end if;
      end Admit;
      procedure Open_Frame (Tree : in out State; Frame : AML_Frame_Handles.Frame_Handle) is
      begin
         if not Admitted or else not Live (Tree) or else
           not AML_Frame_Handles.Belongs_To (Frame, Domain)
           or else AML_Frame_Handles.Index_Of (Frame) not in Slot
           or else Frames (AML_Frame_Handles.Index_Of (Frame)) /= AML_Frame_Handles.No_Frame then
            raise Program_Error with "maintained frame admission";
         end if;
         Frames (AML_Frame_Handles.Index_Of (Frame)) := Frame;
      end Open_Frame;
      procedure Close_Frame (Tree : in out State; Frame : AML_Frame_Handles.Frame_Handle) is
      begin
         if not Live (Tree) or else not Is_Frame (Frame)
           or else Tokens (AML_Frame_Handles.Index_Of (Frame)) /= Locks.No_Token then
            raise Program_Error with "maintained frame retirement";
         end if;
         Frames (AML_Frame_Handles.Index_Of (Frame)) := AML_Frame_Handles.No_Frame;
      end Close_Frame;
      procedure Enter_Method (Tree : in out State; Method : Natural;
        Frame : AML_Frame_Handles.Frame_Handle; Status : in out AML_Execute.Execution_Status) is
         C : Admission := Evidence (Tree);
         Result : Locks.Outcome;
         Token : Locks.Method_Token;
      begin
         Status := AML_Execute.Unsupported_Value;
         if not C.Registry_OK or else not Is_Frame (Frame) or else Method = 0 or else Method > Tree.Used
           or else not Tree.Items (Node_ID (Method)).Alive
           or else Tree.Items (Node_ID (Method)).Object_Type /= Method_Object then return; end if;
         C.Method := Key_Of (Tree, Node_ID (Method));
         if C.Method.Flags / 8 mod 2 = 0 then return; end if;
         C.Method_OK := True; C.Frame_OK := True; C.Frame := Frame; C.Level := C.Method.Flags / 16;
         if Tokens (AML_Frame_Handles.Index_Of (Frame)) /= Locks.No_Token then return; end if;
         Locks.Enter_Method (Registry, C, Domain, C.Method, C.Level, Frame, Token, Result);
         if Result = Locks.Succeeded then Tokens (AML_Frame_Handles.Index_Of (Frame)) := Token; end if;
         Status := Result_Status (Result);
      end Enter_Method;
      procedure Leave_Method (Tree : in out State; Frame : AML_Frame_Handles.Frame_Handle;
        Status : in out AML_Execute.Execution_Status) is
         C : Admission := Evidence (Tree);
         Result : Locks.Outcome;
      begin
         Status := AML_Execute.Unsupported_Value;
         if not C.Registry_OK or else not Is_Frame (Frame) then return; end if;
         C.Leaving := True; C.Frame_OK := True; C.Frame := Frame;
         Locks.Leave_Method (Registry, C, Domain, Tokens (AML_Frame_Handles.Index_Of (Frame)), Result);
         if Result = Locks.Succeeded then Tokens (AML_Frame_Handles.Index_Of (Frame)) := Locks.No_Token; end if;
         Status := Result_Status (Result);
      end Leave_Method;
      procedure Finish (Tree : State) is
         Result : Locks.Outcome;
      begin
         if not Admitted then return; end if;
         if not Live (Tree) or else (for some I in Slot => Frames (I) /= AML_Frame_Handles.No_Frame) then
            raise Program_Error with "maintained invocation retirement";
         end if;
         Locks.End_Invocation (Registry, Evidence (Tree), Domain, Result);
         if Result /= Locks.Succeeded then raise Program_Error with "maintained invocation close"; end if;
         Admitted := False;
      end Finish;
   end Method_Session;

   procedure Invoke_Mutable
     (Tree : in out State; Node : Node_ID; Args : AML_Execute.Arguments;
      Argument_Count : Natural; Budget : Natural; Result : out AML_Execute.Execution_Result)
   is
      use type AML_Decode.Byte;
      package Session is new Method_Session;
      Ready : Boolean;
      procedure Execute_Mutable is new AML_Execute.Execute_Session_Typed
        (State, Valid_Context, Read_Binding, Read_Method, Write_Binding,
         Begin_Method, End_Method, Define_Method,
         Session.Admit, Session.Open_Frame, Session.Close_Frame,
         Session.Enter_Method, Session.Leave_Method);
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
      if Method.Implementation /= AML_Execute.AML_Implementation then
         Result := (Status => AML_Execute.Invalid_Method, Charged => 0); return;
      end if;
      Session.Start (Ready);
      if not Ready then Result := (Status => AML_Execute.Value_Limit, Charged => 0); return; end if;
      Execute_Mutable (Method.Code.Bytes, Method.Width, AML_Execute.As_Values (Args),
        Argument_Count, Budget, Tree, Natural (Node), Result,
        Current_Sync => 0);
      Session.Finish (Tree);
   end Invoke_Mutable;

   -- Hardware field declarations retain untrusted metadata only. No handler
   -- authority is created by accepting the raw flags or address-space layout.
   procedure Define_Operation_Fields_For_Context
     (Tree : in out State; Scope, Lifetime_Owner : Node_ID;
      Region : Node_ID; Flags : AML_Decode.Byte; Entries : AML_Decode.Bytes;
      Status : out AML_Execute.Execution_Status; Data_Register : Node_ID := Root;
      Bank_Register : Node_ID := Root; Bank_Value : AML_Decode.Integer_Value := 0)
     with Pre => Valid_Context (Tree)
       and then Scope <= Tree.Used and then Lifetime_Owner <= Tree.Used
       and then Region > Root and then Region <= Tree.Used
       and then Present (Tree, Region)
       and then (if Data_Register = Root then Kind (Tree, Region) = Operation_Region_Object
         else Data_Register <= Tree.Used and then Present (Tree, Data_Register)
           and then Kind (Tree, Region) = Region_Field_Object
           and then Kind (Tree, Data_Register) = Region_Field_Object
           and then not Tree.Items (Data_Register).Initializing)
       and then not Tree.Items (Region).Initializing
       and then (if Bank_Register = Root then Bank_Value = 0
         else Data_Register = Root and then Bank_Register <= Tree.Used
           and then Present (Tree, Bank_Register)
           and then Kind (Tree, Bank_Register) = Region_Field_Object
           and then not Tree.Items (Bank_Register).Initializing),
       Post => Valid_Context (Tree) and then Tree.Values = Tree'Old.Values
         and then (if Status /= AML_Execute.Returned then Tree = Tree'Old)
   is
      use type AML_Decode.Byte;
      use type AML_Decode.Status;
      use type AML_Fields.Entry_Kind;
      type Pending_Field is record
         Name : AML_Names.Segment := "____";
         Info : Region_Field_Attributes;
         Existing : Node_ID := Root;
      end record;
      Pending : array (Positive range 1 .. Capacity) of Pending_Field;
      Pending_Count : Node_ID := 0;
      New_Count : Node_ID := 0;
      Initial_Count : constant Node_ID := Tree.Used;
      Cursor : Natural := 0;
      Info : Region_Field_Attributes :=
        (Organization => (if Data_Register /= Root then Indexed_Register_Field
          elsif Bank_Register /= Root then Banked_Region_Field else Direct_Region_Field),
         Region => (if Data_Register = Root then Region else Root),
         Incarnation => (if Data_Register = Root then Tree.Items (Region).Incarnation else 0),
         Index_Register => (if Data_Register = Root then Root else Region),
         Data_Register => Data_Register,
         Index_Incarnation => (if Data_Register = Root then 0 else Tree.Items (Region).Incarnation),
         Data_Incarnation => (if Data_Register = Root then 0 else Tree.Items (Data_Register).Incarnation),
         Bank_Register => Bank_Register,
         Bank_Incarnation => (if Bank_Register = Root then 0 else Tree.Items (Bank_Register).Incarnation),
         Bank_Value => Bank_Value,
         Raw_Flags => Flags, Access_Type => Flags and 16#0F#, others => <>);
      Item : AML_Fields.Entry_Result;
   begin
      Status := AML_Execute.Unsupported;
      while Cursor < Entries'Length loop
         pragma Loop_Invariant (Cursor <= Entries'Length);
         pragma Loop_Invariant (New_Count <= Capacity - Initial_Count);
         pragma Loop_Variant (Decreases => Entries'Length - Cursor);
         Item := AML_Fields.Read_Entry (Entries (Entries'First + Cursor .. Entries'Last));
         if Item.Status /= AML_Decode.Accepted then
            Status := AML_Execute.Bad_Package; return;
         end if;
         case Item.Kind is
            when AML_Fields.Named_Field | AML_Fields.Reserved_Field =>
               if Field_Bit_Position (Item.Bits) > Field_Bit_Position'Last - Info.Offset then
                  Status := AML_Execute.Bad_Package; return;
               end if;
               if Item.Kind = AML_Fields.Named_Field then
                  for I in 1 .. Pending_Count loop
                     if Pending (I).Name = Item.Name then
                        Status := AML_Execute.Duplicate_Name; return;
                     end if;
                  end loop;
                  if Pending_Count = Capacity then
                     Status := AML_Execute.Namespace_Limit; return;
                  end if;
                  Info.Bits := Item.Bits;
                  declare
                     Existing : constant Node_ID := Child (Tree, Scope, Item.Name);
                     Previous : Region_Field_Attributes;
                  begin
                     if Existing /= Root then
                        -- A loop may execute the same BankField declaration
                        -- again within its owning method. Reuse its identity
                        -- and update only the newly evaluated selector. A
                        -- different owner, kind or geometry is a collision.
                        if Bank_Register = Root
                          or else Kind (Tree, Existing) /= Region_Field_Object
                          or else Tree.Items (Existing).Owner /= Lifetime_Owner
                        then Status := AML_Execute.Duplicate_Name; return; end if;
                        Previous := Tree.Items (Existing).Region_Field_Info;
                        Previous.Bank_Value := Bank_Value;
                        if Previous /= Info then
                           Status := AML_Execute.Duplicate_Name; return;
                        end if;
                     else
                        if New_Count = Capacity - Initial_Count then
                           Status := AML_Execute.Namespace_Limit; return;
                        end if;
                        New_Count := New_Count + 1;
                     end if;
                     Pending_Count := Pending_Count + 1;
                     Pending (Pending_Count) := (Name => Item.Name, Info => Info, Existing => Existing);
                  end;
               end if;
               Info.Offset := Info.Offset + Field_Bit_Position (Item.Bits);
            when AML_Fields.Access_Field | AML_Fields.Extended_Access_Field =>
               Info.Access_Type := Item.Access_Type;
               Info.Attribute := Item.Attribute;
               Info.Access_Length := Item.Access_Length;
            when AML_Fields.Name_Connection | AML_Fields.Buffer_Connection =>
               return;
         end case;
         Cursor := Cursor + Item.Consumed;
      end loop;
      if AML_References.Node_Incarnation (New_Count) > Max_Node_Incarnation - Tree.Last_Stamp then
         Status := AML_Execute.Namespace_Limit; return;
      end if;
      -- Commit only after complete framing, arithmetic and duplicate checks.
      -- Operation-region bounds and flag semantics belong to future access
      -- admission; this matches the existing static metadata representation.
      for I in 1 .. Pending_Count loop
         pragma Loop_Invariant (Valid_Context (Tree));
         if Pending (I).Existing /= Root then
            Tree.Items (Pending (I).Existing).Region_Field_Info.Bank_Value := Bank_Value;
         else
            Tree.Used := Tree.Used + 1;
            Tree.Last_Stamp := Tree.Last_Stamp + 1;
            Tree.Items (Tree.Used) :=
              (Incarnation => Tree.Last_Stamp, Definition => Tree.Current_Definition, Up => Scope, Owner => Lifetime_Owner,
               Part => Pending (I).Name, Object_Type => Region_Field_Object,
               Region_Field_Info => Pending (I).Info, others => <>);
         end if;
      end loop;
      Status := AML_Execute.Returned;
   end Define_Operation_Fields_For_Context;

   -- Field declaration currently supports immutable DataTableRegion bindings.
   -- Unsupported access/connection forms fail explicitly before committing.
   procedure Define_Fields_For_Context
     (Tree : in out State; Scope, Lifetime_Owner : Natural; Region : AML_Names.Name_Result;
      Flags : AML_Decode.Byte; Entries : AML_Decode.Bytes;
      Status : out AML_Execute.Execution_Status;
      Module_Declaration : Boolean := False)
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
      -- Authenticated declarations may also describe initialized hardware
      -- regions. Preserve the original DataTable path and failure priorities.
      if Declaration_Scopes_Valid (Tree, Scope, Lifetime_Owner, Module_Declaration) then
         Located := Resolve_Object (Tree, Node_ID (Scope), Region);
         if Located.Status = Found and then Located.Node /= Root
           and then Present (Tree, Located.Node)
           and then Kind (Tree, Located.Node) = Operation_Region_Object
           and then not Tree.Items (Located.Node).Initializing
         then
            Define_Operation_Fields_For_Context
              (Tree, Node_ID (Scope), Node_ID (Lifetime_Owner), Located.Node,
               Flags, Entries, Status);
            return;
         end if;
      end if;
      Status := AML_Execute.Unsupported;
      if not Declaration_Scopes_Valid (Tree, Scope, Lifetime_Owner, Module_Declaration)
        or else Flags not in 0 .. 1
      then return; end if;
      Located := Resolve_Object (Tree, Node_ID (Scope), Region);
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
           (Incarnation => Tree.Last_Stamp, Definition => Tree.Current_Definition, Up => Node_ID (Scope), Owner => Node_ID (Lifetime_Owner), Part => Pending (I).Name,
            Object_Type => Table_Field_Object,
            Table_Binding => (Region => Binding, Offset => Pending (I).Offset,
                              Bits => Pending (I).Bits), others => <>);
      end loop;
      Status := AML_Execute.Returned;
   end Define_Fields_For_Context;
   procedure Define_Fields
     (Tree : in out State; Scope : Natural; Region : AML_Names.Name_Result;
      Flags : AML_Decode.Byte; Entries : AML_Decode.Bytes;
      Status : out AML_Execute.Execution_Status)
     with Pre => Valid_Context (Tree),
       Post => Valid_Context (Tree) and then Tree.Values = Tree'Old.Values
         and then (if Status /= AML_Execute.Returned then Tree = Tree'Old)
   is
   begin
      Define_Fields_For_Context (Tree, Scope, Scope, Region, Flags, Entries, Status);
   end Define_Fields;

   procedure Materialize_Buffer_Literal
     (Tree : in out State; Scope : Natural; Length : AML_Buffer_Layouts.Buffer_Length;
      Initializer : AML_Decode.Bytes; Binding : out AML_Execute.Binding_Result)
     with Pre => Valid_Context (Tree) and then not Binding'Constrained,
          Post => Valid_Context (Tree)
            and then Tree = (Tree'Old with delta Values => Tree.Values)
            and then (if Binding.Status in AML_Execute.Failed_Binding then Tree = Tree'Old)
   is
      use type AML_Objects.Allocation_Status;
      pragma Unreferenced (Scope);
      Effective : constant AML_Decode.Integer_Value := AML_Decode.Integer_Value'Max
        (Length, AML_Decode.Integer_Value (Initializer'Length));
      Prefix_Bytes : constant := 8;
      Prefix_Length : constant Natural := Natural (AML_Decode.Integer_Value'Min (Effective, Prefix_Bytes));
      Prefix : AML_Decode.Bytes (1 .. Prefix_Length) := [others => 0];
      ID : AML_Objects.Object_ID;
      Allocated : AML_Objects.Allocation_Status;
   begin
      Binding := (AML_Execute.Failed_Binding, AML_Execute.Value_Limit);
      AML_Objects.New_Buffer (Tree.Values, Length, Initializer, ID, Allocated);
      if Allocated /= AML_Objects.Allocated then return; end if;
      for I in 1 .. Natural'Min (Prefix_Length, Initializer'Length) loop
         Prefix (I) := Initializer (Initializer'First + (I - 1));
      end loop;
      Binding := (AML_Execute.Non_Integer_Binding,
        (ID => ID, Type_Code => 3, Size => AML_Objects.Length (Tree.Values, ID),
         Conversion_32 => AML_Coercions.From_Buffer (Prefix, AML_Decode.Bits_32),
         Conversion_64 => AML_Coercions.From_Buffer (Prefix, AML_Decode.Bits_64), others => <>));
   end Materialize_Buffer_Literal;

   procedure Materialize_Literal_With_Owner
     (Tree : in out State; Owner : AML_Identity.Identity; Scope : Natural; Kind : AML_Execute.Literal_Kind; Width : AML_Decode.Integer_Width; Data : AML_Decode.Bytes;
         Source : aliased AML_Byte_Sources.Storage; Source_First : Natural;
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
      Binding := (Status => AML_Execute.Failed_Binding, Failure => AML_Execute.Unsupported_Value);
      if not AML_Byte_Sources.Corresponds (Source, Source_First, Data) then return; end if;
      if Kind = AML_Execute.Package_Literal then
         declare
            Candidate : State := Tree;
            Bound : AML_Execute.Execution_Status;
            Loaded : AML_Decode.Status;
            Consumed : Natural;
            Environment : Count_Context := (Tree, Node_ID'First);
            use type AML_Decode.Status;
         begin
            Binding := (Status => AML_Execute.Failed_Binding, Failure => AML_Execute.Unsupported_Value);
            if Data'Length = 0 or else Data (Data'First) not in 16#12# | 16#13# then return; end if;
            if Scope > Tree.Used then return; end if;
            Environment.Scope := Node_ID (Scope);
            Environment.Tree.Journal := Pending.Empty;
            Load_Bound_Data (Candidate.Values, Data, Source, Source_First, Width, Environment, ID, Consumed, Loaded);
            if Loaded /= AML_Decode.Accepted then
               if Loaded = AML_Decode.Limit_Exceeded then
                  Binding := (Status => AML_Execute.Failed_Binding, Failure => AML_Execute.Value_Limit);
               end if;
               return;
            end if;
            if Consumed /= Data'Length or else AML_Objects.Kind (Candidate.Values, ID) /= AML_Objects.Package_Object then return; end if;
            Bind_Runtime_Package_Members
              (Candidate, Tree.Values, Environment.Tree.Journal, Owner, Width,
               Root, True, Bound);
            if Bound /= AML_Execute.Returned then
               Binding := (Status => AML_Execute.Failed_Binding, Failure => Bound);
               return;
            end if;
            Tree.Values := Candidate.Values;
            Binding := (Status => AML_Execute.Non_Integer_Binding,
              Object => (Source => AML_References.No_Object_Handle, ID => ID, Type_Code => 4,
                Size => AML_Objects.Length (Tree.Values, ID), Conversion_32 => C32, Conversion_64 => C64));
            return;
         end;
      end if;
      if Kind = AML_Execute.String_Literal then
         String_Reader.Allocate (Tree.Values, Source, Source_First, Data'Length, ID, Status);
      else
         AML_Objects.New_Bytes (Tree.Values, AML_Objects.Buffer_Object, Data, ID, Status);
      end if;
      if Status /= AML_Objects.Allocated then
         Binding := (Status => AML_Execute.Failed_Binding, Failure => AML_Execute.Value_Limit);
         return;
      end if;
      if Kind = AML_Execute.String_Literal then
         C32 := String_Conversion.From_String (Source, Source_First, Data'Length, AML_Decode.Bits_32);
         C64 := String_Conversion.From_String (Source, Source_First, Data'Length, AML_Decode.Bits_64);
      else
         C32 := AML_Coercions.From_Buffer (Data, AML_Decode.Bits_32);
         C64 := AML_Coercions.From_Buffer (Data, AML_Decode.Bits_64);
      end if;
      Binding := (Status => AML_Execute.Non_Integer_Binding,
        Object => (Source => AML_References.No_Object_Handle, ID => ID, Type_Code => (if Kind = AML_Execute.String_Literal then 2 else 3),
          Size => Data'Length, Conversion_32 => C32, Conversion_64 => C64));
   end Materialize_Literal_With_Owner;

   procedure Materialize_Literal
     (Tree : in out State; Scope : Natural; Kind : AML_Execute.Literal_Kind;
      Width : AML_Decode.Integer_Width; Data : AML_Decode.Bytes;
         Source : aliased AML_Byte_Sources.Storage; Source_First : Natural;
      Binding : out AML_Execute.Binding_Result)
   is
   begin
      Materialize_Literal_With_Owner
        (Tree, AML_Identity.No_Identity, Scope, Kind, Width, Data, Source, Source_First, Binding);
   end Materialize_Literal;

   procedure Reserve_Region_For_Context
     (Tree : in out State; Scope, Lifetime_Owner : Natural; Path : AML_Names.Name_Result;
      Token : out Natural; Status : out AML_Execute.Execution_Status;
      Module_Declaration : Boolean := False)
     with Pre => Valid_Context (Tree), Post => Valid_Context (Tree)
   is
      Base, Node : Node_ID;
      Added : Insert_Status;
   begin
      Token := 0; Status := AML_Execute.Bad_Name;
      if not Declaration_Scopes_Valid (Tree, Scope, Lifetime_Owner, Module_Declaration)
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
         declare Binding : constant Lookup_Result := Canonical_Child (Tree, Base, Path.Parts (I)); begin
            if Binding.Status /= Found then Status := AML_Execute.Unknown_Name; return; end if;
            Base := Binding.Node;
         end;
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
      Tree.Items (Node).Owner := Node_ID (Lifetime_Owner);
      Tree.Items (Node).Object_Type := Uninitialized_Region_Object;
      Token := Natural (Node);
      Status := AML_Execute.Returned;
   end Reserve_Region_For_Context;
   procedure Reserve_Region
     (Tree : in out State; Scope : Natural; Path : AML_Names.Name_Result;
      Token : out Natural; Status : out AML_Execute.Execution_Status)
     with Pre => Valid_Context (Tree), Post => Valid_Context (Tree)
   is
   begin
      Reserve_Region_For_Context (Tree, Scope, Scope, Path, Token, Status);
   end Reserve_Region;

   type Table_Selector is record
      Valid : Boolean := False;
      Length : Natural := 0;
      Prefix : String (1 .. 8) := [others => Character'Val (0)];
   end record;
   procedure Make_Table_Selection
     (Sig, Manufacturer, Model : Table_Selector;
      Query : out Firmware_Tables.Identifiers.Selection;
      Status : out AML_Execute.Execution_Status) is
   begin
      Query := (Name => "____", others => <>);
      Status := AML_Execute.Unsupported_Value;
      if not Sig.Valid or else not Manufacturer.Valid or else not Model.Valid then return; end if;
      if Sig.Length < 4 then Status := AML_Execute.Bad_Name; return; end if;
      Query.Name := Sig.Prefix (1 .. 4);
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
      Status := AML_Execute.Returned;
   end Make_Table_Selection;

   procedure Complete_Owned_Region_Selection
     (Tree : in out State; Input : aliased AML_Table_Backing.State;
      Copies : AML_Copied_Tables.State;
      Token : Node_ID; Query : Firmware_Tables.Identifiers.Selection;
      Status : out AML_Execute.Execution_Status) is
      Selected : AML_Table_Readers.Selection_Result;
   begin
      Status := AML_Execute.Unsupported_Value;
      if Token = Root or else Token > Tree.Used or else not Tree.Items (Token).Alive
        or else Tree.Items (Token).Object_Type /= Uninitialized_Region_Object
      then return; end if;
      Selected := AML_Table_Readers.Find_Table (Input, Copies, Query);
      if Selected.Status /= AML_Table_Readers.Table_Selected then
         Status := AML_Execute.Unknown_Name; return;
      end if;
      Tree.Items (Token).Table_Binding :=
        (Region => (Item => Selected.Item, Extent => Selected.Extent), others => <>);
      Tree.Items (Token).Object_Type := Table_Region_Object;
      Status := AML_Execute.Returned;
   end Complete_Owned_Region_Selection;
   procedure Complete_Region_Selection
     (Tree : in out State; Input : aliased AML_Table_Backing.State;
      Token : Node_ID; Query : Firmware_Tables.Identifiers.Selection;
      Status : out AML_Execute.Execution_Status) is
      Empty_Copies : AML_Copied_Tables.State (0, 0);
   begin
      Complete_Owned_Region_Selection (Tree, Input, Empty_Copies, Token, Query, Status);
   end Complete_Region_Selection;

   procedure Complete_Owned_Region
     (Tree : in out State; Input : aliased AML_Table_Backing.State;
      Copies : AML_Copied_Tables.State; Token : Natural;
      Width : AML_Decode.Integer_Width; Signature, OEM, Table_ID : AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
     with Pre => Valid_Context (Tree), Post => Valid_Context (Tree)
   is
      use type AML_Decode.Byte;
      function Describe (Data : AML_Decode.Bytes) return Table_Selector is
         Result : Table_Selector := (Valid => True, others => <>);
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
      function Convert (Item : AML_Execute.Datum) return Table_Selector is
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
      Sig, Manufacturer, Model : Table_Selector;
      Query : Firmware_Tables.Identifiers.Selection;
   begin
      Status := AML_Execute.Unsupported_Value;
      if Token = 0 or else Token > Tree.Used or else not Tree.Items (Token).Alive
        or else Tree.Items (Token).Object_Type /= Uninitialized_Region_Object
      then return; end if;
      Sig := Convert (Signature); Manufacturer := Convert (OEM); Model := Convert (Table_ID);
      Make_Table_Selection (Sig, Manufacturer, Model, Query, Status);
      if Status /= AML_Execute.Returned then return; end if;
      Complete_Owned_Region_Selection (Tree, Input, Copies, Node_ID (Token), Query, Status);
   end Complete_Owned_Region;
   procedure Complete_Region
     (Tree : in out State; Input : aliased AML_Table_Backing.State; Token : Natural;
      Width : AML_Decode.Integer_Width; Signature, OEM, Table_ID : AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
     with Pre => Valid_Context (Tree), Post => Valid_Context (Tree)
   is
      Empty_Copies : AML_Copied_Tables.State (0, 0);
   begin
      Complete_Owned_Region (Tree, Input, Empty_Copies, Token, Width, Signature, OEM, Table_ID, Status);
   end Complete_Region;

      procedure Lookup_With_Owned_Tables
        (Tree : in out State; Input : aliased AML_Table_Backing.State;
         Copies : AML_Copied_Tables.State; Scope : Natural;
         Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width;
         Purpose : AML_Execute.Binding_Purpose; Binding : out AML_Execute.Binding_Result)
        with Pre => Valid_Context (Tree) and then not Binding'Constrained,
             Post => Valid_Context (Tree)
      is
         use type AML_Decode.Integer_Width;
         use type AML_Coercions.Conversion_Status;
         use type AML_Objects.Allocation_Status;
         Located : Lookup_Result;
         Prefix : AML_Decode.Bytes (1 .. 8) := [others => 0];
         Length, Prefix_Length : Natural;
         ID : AML_Objects.Object_ID;
         Status : AML_Objects.Allocation_Status;
         Converted : AML_Coercions.Result;
         Read_Status : AML_Table_Readers.Read_Status;
         use type AML_Table_Readers.Read_Status;
         function Copy_Length (Source : aliased AML_Copied_Tables.Table_ID) return Natural is
           (AML_Copied_Tables.Length (Copies, Source))
           with Pre => AML_Copied_Tables.Valid (Copies) and then AML_Copied_Tables.Present (Copies, Source);
         function Copy_Byte (Source : aliased AML_Copied_Tables.Table_ID; Index : Natural)
           return AML_Decode.Byte is (AML_Copied_Tables.Byte_At (Copies, Source, Index))
           with Pre => AML_Copied_Tables.Valid (Copies) and then AML_Copied_Tables.Present (Copies, Source)
             and then Index < AML_Copied_Tables.Length (Copies, Source);
         package Copied_Buffer_Initializers is new AML_Objects.Bit_Buffer_Initializers
           (AML_Copied_Tables.Table_ID, Copy_Length, Copy_Byte);
      begin
         if Purpose = AML_Execute.Namespace_Identity then
            Binding := (Status => AML_Execute.Failed_Binding, Failure => AML_Execute.Unsupported_Value); return;
         end if;
         Binding := Read_Binding (Tree, Scope, Path, Width);
         if Scope > Tree.Used then return; end if;
         Located := Resolve_Object (Tree, Node_ID (Scope), Path);
         if Located.Status = Found and then Located.Node /= Root
           and then Kind (Tree, Located.Node) in Operation_Region_Object | Region_Field_Object | Uninitialized_Table_Field_Object
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
            View : constant AML_Table_Readers.Field_View :=
              (Field.Region.Item, Field.Region.Extent, Field.Offset, Field.Bits);
         begin
            Binding := (Status => AML_Execute.Failed_Binding, Failure => AML_Execute.Unsupported_Value);
            if not AML_Table_Readers.Valid_Field (Input, Copies, View) then return; end if;
            Length := AML_Table_Readers.Field_Length (View);
            Prefix_Length := Natural'Min (Prefix'Length, Length);
            for I in 1 .. Prefix_Length loop
               AML_Table_Readers.Read_Field_Byte (Input, Copies, View, I - 1, Prefix (I), Read_Status);
               if Read_Status /= AML_Table_Readers.Readable then return; end if;
            end loop;
            if Field.Bits <= (if Width = AML_Decode.Bits_32 then 32 else 64) then
               if Length = 0 then
                  Binding := (Status => AML_Execute.Integer_Binding, Value => 0, Origin => AML_Decode.Ordinary_Integer);
               else
                  Converted := AML_Coercions.From_Buffer (Prefix (1 .. Prefix_Length), Width);
                  if Converted.Status = AML_Coercions.Converted then
                     Binding := (Status => AML_Execute.Integer_Binding, Value => Converted.Value, Origin => AML_Decode.Ordinary_Integer);
                  end if;
               end if;
            else
               case Field.Region.Item.Kind is
                  when AML_Table_Sources.Boot_Source =>
                     Table_Buffer_Initializers.Allocate
                       (Tree.Values, Input, Input.Tables (Positive (Field.Region.Item.Boot)).Offset,
                        Field.Region.Extent, Field.Offset, Field.Bits, ID, Status);
                  when AML_Table_Sources.Copied_Source =>
                     declare
                        Source : aliased constant AML_Copied_Tables.Table_ID :=
                          AML_Copied_Tables.Table_ID (Field.Region.Item.Copied);
                     begin
                        Copied_Buffer_Initializers.Allocate
                          (Tree.Values, Source, 0, Field.Region.Extent,
                           Field.Offset, Field.Bits, ID, Status);
                     end;
                  when AML_Table_Sources.No_Source => return;
               end case;
               if Status /= AML_Objects.Allocated then
                  Binding := (Status => AML_Execute.Failed_Binding, Failure => AML_Execute.Value_Limit);
                  return;
               end if;
               Binding := (Status => AML_Execute.Non_Integer_Binding,
                 Object => (Source => AML_References.No_Object_Handle, ID => ID, Type_Code => 3, Size => Length,
                   Conversion_32 => AML_Coercions.From_Buffer (Prefix (1 .. Prefix_Length), AML_Decode.Bits_32),
                   Conversion_64 => AML_Coercions.From_Buffer (Prefix (1 .. Prefix_Length), AML_Decode.Bits_64)));
            end if;
         end;
      end Lookup_With_Owned_Tables;
      procedure Lookup_With_Tables
        (Tree : in out State; Input : aliased AML_Table_Backing.State; Scope : Natural;
         Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width;
         Purpose : AML_Execute.Binding_Purpose; Binding : out AML_Execute.Binding_Result)
        with Pre => Valid_Context (Tree) and then not Binding'Constrained,
             Post => Valid_Context (Tree)
      is
         Empty_Copies : AML_Copied_Tables.State (0, 0);
      begin
         Lookup_With_Owned_Tables (Tree, Input, Empty_Copies, Scope, Path, Width, Purpose, Binding);
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
      package Session is new Method_Session;
      Ready : Boolean;
      procedure Reject_Reference
        (Environment : in out State; Ref : AML_References.Reference;
         Width : AML_Decode.Integer_Width; Value : out AML_Execute.Datum; Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Ref, Width);
      begin Value := (Value_Kind => AML_Execute.Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer); Status := AML_Execute.Unsupported_Value; end Reject_Reference;
      procedure Reject_Buffer_Field
        (Environment : in out State; Scope : Natural; Path : AML_Names.Name_Result;
         Width : AML_Decode.Integer_Width; Source : AML_Execute.Datum;
         Bit_Offset, Bit_Length : AML_Decode.Integer_Value; Always_Buffer : Boolean;
         Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Scope, Path, Width, Source,
           Bit_Offset, Bit_Length, Always_Buffer);
      begin Status := AML_Execute.Unsupported_Value; end Reject_Buffer_Field;
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

      procedure Reject_Notify (Environment : in out State;
         Target : AML_References.Reference; Value : AML_Decode.Integer_Value;
         Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Target, Value);
      begin
         Status := AML_Execute.Unsupported;
      end Reject_Notify;
      procedure Reject_Metadata_Value
        (Environment : State; Item : AML_Execute.Metadata_Value;
         Result : out AML_Execute.Reference_Metadata) is
         pragma Unreferenced (Environment, Item);
      begin Result := (Kind => AML_Execute.Invalid_Reference); end Reject_Metadata_Value;
      procedure Delay_Adapter_1 (Environment : in out State; Item : AML_Delays.Request; Result : out AML_Delays.Outcome) is
         pragma Unreferenced (Environment);
      begin
         Perform_Delay (Item, Result);
      end Delay_Adapter_1;
      -- Explicit frame-bound leases distinguish module permanence from method cleanup.
      procedure Execute_Alias (Environment : in out State; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Source_Path, Target_Path : AML_Names.Name_Result;
         Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease, Scope, Source_Path, Target_Path);
      begin Status := AML_Execute.Unsupported; end Execute_Alias;
      Execute_Declaration_Lease_Store : AML_Declaration_Contexts.State := AML_Declaration_Contexts.Empty;
      use type AML_Declaration_Contexts.Request;
      use type AML_Declaration_Contexts.Lease;
      type Execute_Declaration_Name_Token is record
         Owner : AML_Declaration_Contexts.Lease := AML_Declaration_Contexts.No_Lease;
         Original : Boolean := False;
      end record;
      Execute_Declaration_No_Name : constant Execute_Declaration_Name_Token := (others => <>);
      function Execute_Declaration_Valid (Environment : State; Lease : AML_Declaration_Contexts.Lease) return Boolean
      is (AML_Declaration_Contexts.Is_Live (Execute_Declaration_Lease_Store, Lease) and then AML_Declaration_Contexts.Scope_Of (Lease) > 0 and then AML_Declaration_Contexts.Scope_Of (Lease) <= Environment.Used
         and then Environment.Items (Node_ID (AML_Declaration_Contexts.Scope_Of (Lease))).Alive
         and then Environment.Items (Node_ID (AML_Declaration_Contexts.Scope_Of (Lease))).Object_Type = Method_Object
         and then Environment.Items (Node_ID (AML_Declaration_Contexts.Scope_Of (Lease))).Active_Calls > 0
         and then AML_Declaration_Contexts.Stamp_Of (Lease) = Natural (Environment.Items (Node_ID (AML_Declaration_Contexts.Scope_Of (Lease))).Incarnation));
      procedure Execute_Declaration_Enter
        (Environment : in out State; Intent : AML_Declaration_Contexts.Request; Scope : Natural;
         Frame : AML_Frame_Handles.Frame_Handle; Lease : out AML_Declaration_Contexts.Lease; Allowed : out Boolean) is
      begin
         Lease := AML_Declaration_Contexts.No_Lease; Allowed := False;
         if Intent /= AML_Declaration_Contexts.Method_Request then return; end if;
         Begin_Method (Environment, Scope, Allowed);
         if not Allowed then return; end if;
         AML_Declaration_Contexts.Open (Execute_Declaration_Lease_Store, Intent, Frame, Scope, Natural (Environment.Items (Node_ID (Scope)).Incarnation), Lease, Allowed);
         if not Allowed then End_Method (Environment, Scope); end if;
      end Execute_Declaration_Enter;
      procedure Execute_Declaration_Leave (Environment : in out State; Lease : AML_Declaration_Contexts.Lease) is
      begin
         if not Execute_Declaration_Valid (Environment, Lease) then return; end if;
         End_Method (Environment, AML_Declaration_Contexts.Scope_Of (Lease));
         AML_Declaration_Contexts.Close (Execute_Declaration_Lease_Store, Lease);
      end Execute_Declaration_Leave;
      procedure Execute_Declaration_Method
        (Environment : in out State; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result; Flags : AML_Decode.Byte; Width : AML_Decode.Integer_Width; Code : AML_Decode.Bytes; Status : out AML_Execute.Declaration_Status) is
      begin
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then
            Status := AML_Execute.Declaration_Unsupported; return;
         end if;
         Define_Method (Environment, Scope, Path, Flags, Width, Code, Status);
      end Execute_Declaration_Method;
      procedure Execute_Declaration_Fields
        (Environment : in out State; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Region : AML_Names.Name_Result; Flags : AML_Decode.Byte; Entries : AML_Decode.Bytes; Status : out AML_Execute.Execution_Status) is
      begin
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then
            Status := AML_Execute.Unsupported_Value; return;
         end if;
         Define_Fields (Environment, Scope, Region, Flags, Entries, Status);
      end Execute_Declaration_Fields;
      procedure Execute_Declaration_Buffer_Field
        (Environment : in out State; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width; Source : AML_Execute.Datum; Bit_Offset, Bit_Length : AML_Decode.Integer_Value; Always_Buffer : Boolean; Status : out AML_Execute.Execution_Status) is
      begin
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then
            Status := AML_Execute.Unsupported_Value; return;
         end if;
         Reject_Buffer_Field (Environment, Scope, Path, Width, Source, Bit_Offset, Bit_Length, Always_Buffer, Status);
      end Execute_Declaration_Buffer_Field;
      procedure Execute_Declaration_Name
        (Environment : in out State; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width; Data : AML_Decode.Bytes;
         Source : aliased AML_Byte_Sources.Storage; Source_First : Natural; Consumed : out Natural; Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Source, Source_First);
      begin
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then
            Consumed := 0; Status := AML_Execute.Unsupported_Value; return;
         end if;
         Reject_Name (Environment, Scope, Path, Width, Data, Consumed, Status);
      end Execute_Declaration_Name;
      procedure Execute_Declaration_Reserve_Region
        (Environment : in out State; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result;
         Token : out AML_Declaration_Contexts.Region_Reservation; Status : out AML_Execute.Execution_Status) is
         Node : Natural;
      begin
         Token := AML_Declaration_Contexts.No_Region; Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then return; end if;
         Reserve_Region (Environment, Scope, Path, Node, Status);
         if Status in AML_Execute.Returned and then Node /= 0 then
            Token := AML_Declaration_Contexts.Reserve_Region (Lease, Node, Natural (Environment.Items (Node_ID (Node)).Incarnation));
         end if;
      end Execute_Declaration_Reserve_Region;
      procedure Execute_Declaration_Complete_Region
        (Environment : in out State; Lease : AML_Declaration_Contexts.Lease; Input : aliased AML_Table_Backing.State;
         Token : AML_Declaration_Contexts.Region_Reservation; Width : AML_Decode.Integer_Width;
         Signature, OEM, Table_ID : AML_Execute.Datum; Status : out AML_Execute.Execution_Status) is
         Node : constant Natural := AML_Declaration_Contexts.Node_Of (Token);
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else not AML_Declaration_Contexts.Issued_By (Token, Lease) then return; end if;
         if Node > Environment.Used or else not Environment.Items (Node_ID (Node)).Alive
           or else AML_Declaration_Contexts.Stamp_Of (Token) /= Natural (Environment.Items (Node_ID (Node)).Incarnation)
         then return; end if;
         Complete_Region (Environment, Input, Node, Width, Signature, OEM, Table_ID, Status);
      end Execute_Declaration_Complete_Region;
      procedure Execute_Declaration_Reserve_Name
        (Environment : in out State; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result;
         Token : out Execute_Declaration_Name_Token; Status : out AML_Execute.Execution_Status) is
      begin
         Token := Execute_Declaration_No_Name; Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then return; end if;
         Reserve_Dynamic_Name (Environment, Scope, Path, Token.Original, Status);
         if Status in AML_Execute.Returned then Token.Owner := Lease; end if;
      end Execute_Declaration_Reserve_Name;
      procedure Execute_Declaration_Complete_Name
        (Environment : in out State; Lease : AML_Declaration_Contexts.Lease; Token : Execute_Declaration_Name_Token;
         Width : AML_Decode.Integer_Width; Initializer : AML_Decode.Bytes; Count : AML_Data.Count_Result;
         Status : out AML_Execute.Execution_Status) is
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else Token.Owner /= Lease then return; end if;
         Complete_Dynamic_Buffer (Environment, Token.Original, Width, Initializer, Count, Status);
      end Execute_Declaration_Complete_Name;
      procedure Execute_Declaration_Abort_Name
        (Environment : in out State; Lease : AML_Declaration_Contexts.Lease; Token : Execute_Declaration_Name_Token;
         Status : out AML_Execute.Execution_Status) is
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else Token.Owner /= Lease then return; end if;
         Abort_Dynamic_Name (Environment, Token.Original, Status);
      end Execute_Declaration_Abort_Name;
      procedure Execute_Unsupported_Container_Enter
        (Environment : in out State; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Path : AML_Names.Name_Result;
         Metadata : AML_Container_Headers.Attributes; New_Scope : out Natural;
         Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease, Path, Metadata);
      begin
         New_Scope := Scope; Status := AML_Execute.Unsupported;
      end Execute_Unsupported_Container_Enter;
      procedure Execute_Unsupported_Container_Leave
        (Environment : in out State; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Restored_Scope : out Natural; Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease);
      begin
         Restored_Scope := Scope; Status := AML_Execute.Unsupported;
      end Execute_Unsupported_Container_Leave;
      procedure Provenance_Execute_Resolve
        (Environment : in out State; Ref : AML_References.Reference;
         Width : AML_Decode.Integer_Width; Value : out AML_Execute.Datum;
         Provenance : out AML_Execute.Scalar_Provenance;
         Status : out AML_Execute.Execution_Status) is
      begin
         Provenance := AML_Execute.Detached_Provenance;
         Reject_Reference (Environment, Ref, Width, Value, Status);
      end Provenance_Execute_Resolve;
      procedure Provenance_Execute_Read_Integer
        (Environment : State; Source : AML_References.Object_Handle;
         Value : out AML_Decode.Integer_Value; Origin : out AML_Decode.Integer_Origin;
         Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Source);
      begin
         Value := 0; Origin := AML_Decode.Ordinary_Integer;
         Status := AML_Execute.Unsupported_Value;
      end Provenance_Execute_Read_Integer;
      procedure Reject_Integer_Normalization
        (Environment : in out State; Source : AML_References.Object_Handle;
         Width : AML_Decode.Integer_Width; Value : out AML_Decode.Integer_Value;
         Origin : out AML_Decode.Integer_Origin; Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Source, Width);
      begin
         Value := 0; Origin := AML_Decode.Ordinary_Integer;
         Status := AML_Execute.Unsupported_Value;
      end Reject_Integer_Normalization;
      procedure Reject_Integer_Normalization_Census_Begin
        (Environment : State; Source : AML_References.Object_Handle;
         Status : out AML_Execute.Owner_Census_Status) is
         pragma Unreferenced (Environment, Source);
      begin Status := AML_Execute.Census_Invalid; end Reject_Integer_Normalization_Census_Begin;
      procedure Reject_Integer_Normalization_Census_Add
        (Environment : State; Value : AML_Execute.Datum;
         Status : out AML_Execute.Owner_Census_Status) is
         pragma Unreferenced (Environment, Value);
      begin Status := AML_Execute.Census_Invalid; end Reject_Integer_Normalization_Census_Add;
      procedure Reject_Integer_Normalization_Census_Finish
        (Environment : State; Status : out AML_Execute.Owner_Census_Status) is
         pragma Unreferenced (Environment);
      begin Status := AML_Execute.Census_Invalid; end Reject_Integer_Normalization_Census_Finish;

      procedure Package_Adapter_Begin
        (Environment : in out State; Lease : AML_Declaration_Contexts.Lease;
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
        (Environment : in out State; Lease : AML_Declaration_Contexts.Lease;
         Token : in out Natural; Item : AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease, Token, Item);
      begin
         Status := AML_Execute.Unsupported_Value;
      end Package_Adapter_Append;
      procedure Package_Adapter_Defer
        (Environment : State; Lease : AML_Declaration_Contexts.Lease;
         Token : in out Natural; Scope : Natural;
         Path : AML_Names.Name_Result; Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease, Token, Scope, Path);
      begin
         Status := AML_Execute.Unsupported_Value;
      end Package_Adapter_Defer;
      procedure Package_Adapter_Finish
        (Environment : in out State; Lease : AML_Declaration_Contexts.Lease;
         Token : in out Natural; Binding : out AML_Execute.Binding_Result) is
         pragma Unreferenced (Environment, Lease, Token);
      begin
         Binding := (Status => AML_Execute.Failed_Binding, Failure => AML_Execute.Unsupported_Value);
      end Package_Adapter_Finish;
      -- Rejected builders never own an allocation or root to unwind.
      procedure Package_Adapter_Abort
        (Environment : in out State; Lease : AML_Declaration_Contexts.Lease;
         Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease);
      begin
         Status := AML_Execute.Returned;
      end Package_Adapter_Abort;
      procedure Execute is new AML_Execute.Execute_From_Source
        (State,
         AML_Table_Backing.State,
         Valid_Context,
         Lookup_With_Tables,
         Read_Method,
         Write_Binding,
         Execute_Declaration_Enter,
         Execute_Declaration_Leave,
         Execute_Declaration_Method,
         Execute_Declaration_Fields,
         Materialize_Literal,
         Execute_Declaration_Reserve_Region,
         Execute_Declaration_Complete_Region,
         Read_Context_Timer,
         Provenance_Execute_Resolve,
         Provenance_Execute_Read_Integer,
         Reject_Integer_Normalization,
         Reject_Integer_Normalization_Census_Begin, Reject_Integer_Normalization_Census_Add, Reject_Integer_Normalization_Census_Finish,
         Execute_Declaration_Buffer_Field,
         Reject_Index,
         Reject_Store,
         Clone_Value,
         Reject_Compare,
         Keep_Value,
         Session.Admit,
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
         Reject_Concatenation,
         Reject_To_Buffer,
         Reject_Mid,
         Reject_To_String,
         Reject_Format_String,
         Reject_Match,
         Reject_Resources,
         Wait_For_Delay => Delay_Adapter_1,
         Queue_Notify => Reject_Notify,
         Describe_Metadata_Value => Reject_Metadata_Value,
         Materialize_Buffer => Materialize_Buffer_Literal,
         Define_Alias => Execute_Alias,
         Enter_Container => Execute_Unsupported_Container_Enter,
         Leave_Container => Execute_Unsupported_Container_Leave,
         Require_Invocation_Authority => True,
         Open_Frame_Root => Session.Open_Frame,
         Close_Frame_Root => Session.Close_Frame,
         Enter_Serialized_Method => Session.Enter_Method,
         Leave_Serialized_Method => Session.Leave_Method,
         Block_Capacity => Block_Capacity,
         Runtime_Package_Token => Natural,
         No_Runtime_Package => 0,
         Begin_Runtime_Package => Package_Adapter_Begin,
         Append_Runtime_Package => Package_Adapter_Append,
         Defer_Runtime_Package_Name => Package_Adapter_Defer,
         Finish_Runtime_Package => Package_Adapter_Finish,
         Abort_Runtime_Packages => Package_Adapter_Abort);
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
      if Method.Implementation /= AML_Execute.AML_Implementation then
         Result := (Status => AML_Execute.Invalid_Method, Charged => 0); return;
      end if;
      Session.Start (Ready);
      if not Ready then Result := (Status => AML_Execute.Value_Limit, Charged => 0); return; end if;
      Execute (Method.Code.Bytes, Method.Width, AML_Execute.As_Values (Args),
        Argument_Count, Budget, Input, Tree, Natural (Node), Result, Method.Code, 0,
        Current_Sync => 0);
      Session.Finish (Tree);
   end Invoke_With_Tables;



   procedure Invoke_Read_Only
     (Tree : State; Node : Node_ID; Args : AML_Execute.Arguments;
      Argument_Count : Natural; Budget : Natural; Result : out AML_Execute.Execution_Result)
   is
      use type AML_Decode.Byte;
      package Session is new Method_Session;
      Ready : Boolean;
      Copy : State := Tree;
      Method : constant AML_Execute.Method_Definition := Read_Method (Tree, Node);
      procedure Reject_Write
        (Environment : in out State; Scope : Natural; Path : AML_Names.Name_Result;
         Width : AML_Decode.Integer_Width; Item : AML_Execute.Datum;
         Status : out AML_Execute.Write_Status) is
         pragma Unreferenced (Environment, Scope, Path, Width, Item);
      begin Status := AML_Execute.Write_Unsupported; end Reject_Write;
      procedure Begin_Read (Environment : in out State; Scope : Natural; Allowed : out Boolean) is
         pragma Unreferenced (Environment, Scope);
      begin Allowed := True; end Begin_Read;
      procedure End_Read (Environment : in out State; Scope : Natural) is
         pragma Unreferenced (Environment, Scope);
      begin null; end End_Read;
      procedure Reject_Definition
        (Environment : in out State; Scope : Natural; Path : AML_Names.Name_Result;
         Flags : AML_Decode.Byte; Width : AML_Decode.Integer_Width;
         Code : AML_Decode.Bytes; Status : out AML_Execute.Declaration_Status) is
         pragma Unreferenced (Environment, Scope, Path, Flags, Width, Code);
      begin Status := AML_Execute.Declaration_Unsupported; end Reject_Definition;
      procedure Execute is new AML_Execute.Execute_Session_Typed
        (State, Valid_Context, Read_Binding, Read_Method, Reject_Write,
         Begin_Read, End_Read, Reject_Definition,
         Session.Admit, Session.Open_Frame, Session.Close_Frame,
         Session.Enter_Method, Session.Leave_Method);
   begin
      if Pending_Members (Tree) /= 0 then
         Result := (Status => AML_Execute.Uninitialized, Charged => 0); return;
      elsif not Method.Exists then
         Result := (Status => AML_Execute.Invalid_Method, Charged => 0); return;
      elsif Natural (Method.Flags and 7) /= Argument_Count then
         Result := (Status => AML_Execute.Argument_Mismatch, Charged => 0); return;
      elsif Method.Implementation /= AML_Execute.AML_Implementation then
         Result := (Status => AML_Execute.Invalid_Method, Charged => 0); return;
      end if;
      Session.Start (Ready);
      if not Ready then Result := (Status => AML_Execute.Value_Limit, Charged => 0); return; end if;
      Execute (Method.Code.Bytes, Method.Width, AML_Execute.As_Values (Args),
        Argument_Count, Budget, Copy, Natural (Node), Result);
      Session.Finish (Copy);
   end Invoke_Read_Only;

   function Pending_Members (Tree : State) return Natural is
     (Pending.Count (Tree.Journal));
   function Initialization_Frame (Tree, Prior : State) return Boolean is
      use type AML_Object_Identifiers.Slot_Incarnation;
      Added : Natural := 0;
      Before : constant AML_Objects.Usage := AML_Objects.Usage_Of (Prior.Values);
      After : constant AML_Objects.Usage := AML_Objects.Usage_Of (Tree.Values);
   begin
      if AML_Objects.Slot_Bound (Tree.Values) < AML_Objects.Slot_Bound (Prior.Values)
        or else AML_Objects.Incarnation_Limit (Tree.Values) /= AML_Objects.Incarnation_Limit (Prior.Values)
        or else Tree /= (Prior with delta Values => Tree.Values, Journal => Tree.Journal)
        or else After.Bytes /= Before.Bytes or else After.Elements /= Before.Elements
      then return False; end if;
      for ID in 1 .. AML_Objects.Max_Objects loop
         if not AML_Objects.Is_Live (Prior.Values, ID) and then AML_Objects.Is_Live (Tree.Values, ID) then
            if AML_Objects.Kind (Tree.Values, ID) /= AML_Objects.Metadata_Object then return False; end if;
            Added := Added + 1;
         elsif AML_Objects.Is_Live (Tree.Values, ID) /= AML_Objects.Is_Live (Prior.Values, ID)
           or else AML_Objects.Last_Incarnation (Tree.Values, ID) /= AML_Objects.Last_Incarnation (Prior.Values, ID)
         then return False; end if;
         if AML_Objects.Is_Live (Prior.Values, ID) then
         if AML_Objects.Kind (Tree.Values, ID) /= AML_Objects.Kind (Prior.Values, ID)
           or else AML_Objects.Length (Tree.Values, ID) /= AML_Objects.Length (Prior.Values, ID)
         then return False; end if;
         case AML_Objects.Kind (Prior.Values, ID) is
            when AML_Objects.Metadata_Object =>
               if AML_Objects.Metadata_Data (Tree.Values, ID) /= AML_Objects.Metadata_Data (Prior.Values, ID)
               then return False; end if;
            when AML_Objects.Metadata_Snapshot_Object =>
               if AML_Objects.Metadata_Snapshot_Data (Tree.Values, ID) /= AML_Objects.Metadata_Snapshot_Data (Prior.Values, ID)
               then return False; end if;
            when AML_Objects.Reference_Object =>
               if AML_Objects.Reference_Data (Tree.Values, ID) /= AML_Objects.Reference_Data (Prior.Values, ID)
               then return False; end if;
            when AML_Objects.Mutex_Object =>
               if Natural (AML_Objects.Mutex_Flags (Tree.Values, ID)) /= Natural (AML_Objects.Mutex_Flags (Prior.Values, ID)) then return False; end if;
            when AML_Objects.Event_Object =>
               if AML_Objects.Event_Units (Tree.Values, ID) /= AML_Objects.Event_Units (Prior.Values, ID) then return False; end if;
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
      return After.Objects >= Before.Objects and then Natural (After.Objects - Before.Objects) = Added;
   end Initialization_Frame;

   function Table_Initialization_Frame
     (Tree, Prior : State; Input : AML_Table_Backing.State) return Boolean
   is
      -- Construct only the permitted namespace delta, independently of the
      -- mutating resolver. Existing member framing checks Values and Journal.
      Resolved : State := Prior;
      Backing : aliased constant AML_Table_Backing.State := Input;
      Selected : Natural;
      Region : Node_ID;
   begin
      for I in 1 .. Prior.Used loop
         if Prior.Items (I).Static_Table_Region then
            if not Prior.Items (I).Alive
              or else Prior.Items (I).Object_Type /= Uninitialized_Region_Object
              or else Prior.Items (I).Static_Table_Field
            then return False; end if;
            Resolved.Items (I).Static_Table_Region := False;
            Selected := AML_Table_Backing.Find_Table
              (Backing, Prior.Items (I).Static_Table_Selection);
            if Selected /= 0 and then AML_Table_Backing.Valid_Span (Backing, Selected) then
               Resolved.Items (I).Object_Type := Table_Region_Object;
               Resolved.Items (I).Table_Binding :=
                 (Region => (Item => (AML_Table_Sources.Boot_Source, AML_Table_Sources.Boot_Table_ID (Selected)),
                   Extent => Backing.Tables (Selected).Extent), others => <>);
            end if;
         end if;
      end loop;
      for I in 1 .. Prior.Used loop
         if Prior.Items (I).Static_Table_Field then
            if not Prior.Items (I).Alive
              or else Prior.Items (I).Object_Type /= Uninitialized_Table_Field_Object
              or else Prior.Items (I).Static_Table_Region
            then return False; end if;
            Resolved.Items (I).Static_Table_Field := False;
            Region := Prior.Items (I).Region_Field_Info.Region;
            if Region /= Root and then Region <= Prior.Used
              and then Resolved.Items (Region).Alive
              and then Resolved.Items (Region).Incarnation = Prior.Items (I).Region_Field_Info.Incarnation
              and then Resolved.Items (Region).Object_Type = Table_Region_Object
              and then Prior.Items (I).Region_Field_Info.Offset <= Field_Bit_Position (Natural'Last)
              and then AML_Field_Data.Fits
                (Resolved.Items (Region).Table_Binding.Region.Extent,
                 Natural (Prior.Items (I).Region_Field_Info.Offset),
                 Natural (Prior.Items (I).Region_Field_Info.Bits))
            then
               Resolved.Items (I).Object_Type := Table_Field_Object;
               Resolved.Items (I).Table_Binding :=
                 (Region => Resolved.Items (Region).Table_Binding.Region,
                  Offset => Natural (Prior.Items (I).Region_Field_Info.Offset),
                  Bits => Natural (Prior.Items (I).Region_Field_Info.Bits));
            end if;
         end if;
      end loop;
      -- In particular identities, aliases, descriptors, code and all other
      -- entries must equal Prior; only the enumerated fields above may differ.
      return Initialization_Frame (Tree, Resolved);
   end Table_Initialization_Frame;
   procedure Initialize_Members_With_Owner
     (Tree : in out State; Owner : AML_Identity.Identity; Report : out Initialization_Report) is
      use type AML_Identity.Identity;
      use type AML_Objects.Allocation_Status;
      Candidate : State := Tree;
      Located : Lookup_Result;
      Leaf : AML_Objects.Object_ID;
      Allocated : AML_Objects.Allocation_Status;
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
               Located := Resolve_Object (Candidate, Member.Scope, Member.Path);
               if Located.Status /= Found then
                  Report.Missing := Report.Missing + 1;
               elsif Located.Node = Root or else Kind (Candidate, Located.Node) = Metadata_Value_Object
                 or else Package_Metadata_Kind (Kind (Candidate, Located.Node)) then
                  if Owner = AML_Identity.No_Identity then
                     Report.Unsupported := Report.Unsupported + 1;
                  else
                     AML_Objects.New_Metadata (Candidate.Values,
                       Metadata_Identity_At (Candidate, Owner, Located.Node), Leaf, Allocated);
                     if Allocated = AML_Objects.Allocated then
                        AML_Objects.Set_Element (Candidate.Values, Member.Package_ID, Member.Element, Leaf);
                        Report.Bound := Report.Bound + 1;
                     else Report.Unsupported := Report.Unsupported + 1; end if;
                  end if;
               elsif Candidate.Items (Located.Node).Object_Type not in
                 Integer_Object | String_Object | Buffer_Object | Package_Object | Reference_Object | Metadata_Value_Object
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
   end Initialize_Members_With_Owner;
   procedure Resolve_Static_Tables
     (Tree : in out State; Input : aliased AML_Table_Backing.State; Have_Input : Boolean;
      Report : out Initialization_Report) is
      Candidate : State := Tree;
      Status : AML_Execute.Execution_Status;
      Region : Node_ID;
   begin
      Report := (others => 0);
      for I in 1 .. Candidate.Used loop
         if Candidate.Items (I).Static_Table_Region then
            if Have_Input then
               declare
                  Query : constant Firmware_Tables.Identifiers.Selection :=
                    Candidate.Items (I).Static_Table_Selection;
               begin
                  Complete_Region_Selection (Candidate, Input, I, Query, Status);
               end;
            else Status := AML_Execute.Unsupported_Value; end if;
            if Status = AML_Execute.Returned then Report.Bound := Report.Bound + 1;
            elsif Status = AML_Execute.Unknown_Name then Report.Missing := Report.Missing + 1;
            else Report.Unsupported := Report.Unsupported + 1; end if;
            -- Match existing initialization policy: one attempt, explicit report.
            Candidate.Items (I).Static_Table_Region := False;
         end if;
      end loop;
      for I in 1 .. Candidate.Used loop
         if Candidate.Items (I).Static_Table_Field then
            Region := Candidate.Items (I).Region_Field_Info.Region;
            if Region = Root or else Region > Candidate.Used
              or else not Candidate.Items (Region).Alive
              or else Candidate.Items (Region).Incarnation /= Candidate.Items (I).Region_Field_Info.Incarnation
              or else Candidate.Items (Region).Object_Type /= Table_Region_Object
            then Report.Missing := Report.Missing + 1;
            elsif Candidate.Items (I).Region_Field_Info.Offset > Field_Bit_Position (Natural'Last)
              or else not AML_Field_Data.Fits (Candidate.Items (Region).Table_Binding.Region.Extent,
                Natural (Candidate.Items (I).Region_Field_Info.Offset),
                Natural (Candidate.Items (I).Region_Field_Info.Bits))
            then Report.Unsupported := Report.Unsupported + 1;
            else
               Candidate.Items (I).Table_Binding :=
                 (Region => Candidate.Items (Region).Table_Binding.Region,
                  Offset => Natural (Candidate.Items (I).Region_Field_Info.Offset),
                  Bits => Natural (Candidate.Items (I).Region_Field_Info.Bits));
               Candidate.Items (I).Object_Type := Table_Field_Object;
               Report.Bound := Report.Bound + 1;
            end if;
            Candidate.Items (I).Static_Table_Field := False;
         end if;
      end loop;
      Tree := Candidate;
   end Resolve_Static_Tables;

   procedure Initialize_Members (Tree : in out State; Report : out Initialization_Report) is
   begin Initialize_Members_With_Owner (Tree, AML_Identity.No_Identity, Report); end Initialize_Members;

   procedure Load_Names_From_Source
     (Tree : in out State; Data : AML_Decode.Bytes;
      Source : aliased AML_Byte_Sources.Storage; Source_First : Natural;
      Width : AML_Decode.Integer_Width; Result : out Load_Status)
   is
      use type AML_Decode.Byte;
      use type AML_Decode.Status;
      use type AML_Objects.Allocation_Status;
      Object_Ref : AML_Objects.Object_ID;
      Allocation : AML_Objects.Allocation_Status;
      Candidate : State := Tree;
      Offset : Natural := 0;
      Container_Start : Natural := 0;
      Container_Header : AML_Container_Headers.Header_Result;
      use type AML_Container_Headers.Truncation_Phase;
      Path : AML_Names.Name_Result;
      Alias_Source : Lookup_Result;
      Value : AML_Decode.Integer_Result;
      Text : AML_Decode.String_Span_Result;
      Buffer_Item : AML_Buffer_Layouts.Layout_Result;
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
      Alias_Opcode : constant AML_Decode.Byte := 16#06#;
      Extended_Opcode : constant AML_Decode.Byte := 16#5B#;
      Mutex_Opcode : constant AML_Decode.Byte := 16#01#;
      Event_Opcode : constant AML_Decode.Byte := 16#02#;
      Device_Opcode : constant AML_Decode.Byte := 16#82#;
      Processor_Opcode : constant AML_Decode.Byte := 16#83#;
      Power_Opcode : constant AML_Decode.Byte := 16#84#;
      Thermal_Opcode : constant AML_Decode.Byte := 16#85#;
      Is_Scope, Is_Device, Is_Method, Is_Event, Is_Mutex : Boolean;
      Is_Processor, Is_Power, Is_Thermal, Is_Container, Is_Operation, Is_Field, Is_Table : Boolean;
      Region_Opcode : constant AML_Decode.Byte := 16#80#;
      Field_Opcode : constant AML_Decode.Byte := 16#81#;
      Table_Opcode : constant AML_Decode.Byte := 16#88#;
      Limit : Natural;
      Package_Info : AML_Decode.Package_Result;
      Located : Lookup_Result;
      Code_Start : Aggregate_Method_Count;
      procedure Static_Table_Selectors (Cursor : in out Natural; Finish : Natural;
         Query : out Firmware_Tables.Identifiers.Selection; Status : out Load_Status) is
         type Selector_List is array (Positive range 1 .. 3) of Table_Selector;
         Selectors : Selector_List;
         Parsed : AML_Decode.String_Span_Result;
         Converted : AML_Execute.Execution_Status;
      begin
         Query := (Name => "____", others => <>);
         Status := Bad_String;
         for Index in Selectors'Range loop
            if Cursor = Finish then return; end if;
            if Data (Data'First + Cursor) /= 16#0D# then Status := Unsupported_Opcode; return; end if;
            Parsed := AML_Decode.Read_String_Span (Data (Data'First + Cursor .. Data'First + (Finish - 1)));
            if Parsed.Kind /= AML_Decode.Accepted then return; end if;
            Selectors (Index).Valid := True;
            Selectors (Index).Length := Parsed.Length;
            for J in 1 .. Natural'Min (Parsed.Length, Selectors (Index).Prefix'Length) loop
               Selectors (Index).Prefix (J) := Character'Val
                 (Data (Data'First + Cursor + Parsed.Payload_Offset + (J - 1)));
            end loop;
            Cursor := Cursor + Parsed.Consumed;
         end loop;
         Make_Table_Selection (Selectors (1), Selectors (2), Selectors (3), Query, Converted);
         Status := (if Converted = AML_Execute.Returned then Loaded else Bad_String);
      end Static_Table_Selectors;
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
         Region_Node := Resolve_Object (Candidate, Parent, Region_Path);
         if Region_Node.Status /= Found or else Region_Node.Node = Root then Status := Missing_Scope; return; end if;
         if Kind (Candidate, Region_Node.Node) /= Operation_Region_Object
           and then not Candidate.Items (Region_Node.Node).Static_Table_Region
         then Status := Unsupported_Opcode; return; end if;
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
                     if Candidate.Items (Info.Region).Static_Table_Region then
                        Candidate.Items (Field_Node).Object_Type := Uninitialized_Table_Field_Object;
                        Candidate.Items (Field_Node).Static_Table_Field := True;
                     else Candidate.Items (Field_Node).Object_Type := Region_Field_Object; end if;
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
      if not AML_Byte_Sources.Corresponds (Source, Source_First, Data) then
         Result := Bad_Package; return;
      end if;
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
            Container_Start := Offset;
            Op := Data (Data'First + Offset);
            Offset := Offset + 1;
            Is_Scope := Op = 16#10#;
            Is_Device := False;
            Is_Event := False;
            Is_Mutex := False;
            Is_Processor := False;
            Is_Power := False;
            Is_Thermal := False;
            Is_Operation := False; Is_Field := False; Is_Table := False;
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
               Is_Table := Data (Data'First + Offset) = Table_Opcode;
               Offset := Offset + 1;
            end if;
            Is_Container := Is_Device or Is_Processor or Is_Power or Is_Thermal;
            if not Is_Scope and then not Is_Container and then not Is_Method
              and then not Is_Event and then not Is_Mutex and then not Is_Operation and then not Is_Field and then not Is_Table and then Op /= 16#08# and then Op /= Alias_Opcode then
               Result := Unsupported_Opcode; return;
            end if;
            if Is_Scope or Is_Container then
               Container_Header := AML_Container_Headers.Read_Header
                 (Data (Data'First + Container_Start .. Data'First + (Limit - 1)));
               case Container_Header.Status is
                  when AML_Container_Headers.Accepted => null;
                  when AML_Container_Headers.Truncated =>
                     Result := (if Container_Header.Phase = AML_Container_Headers.Name_Phase
                       then Bad_Name else Bad_Package);
                     return;
                  when AML_Container_Headers.Bad_Name => Result := Bad_Name; return;
                  when AML_Container_Headers.Bad_Package => Result := Bad_Package; return;
                  when AML_Container_Headers.Unsupported => Result := Unsupported_Opcode; return;
               end case;
               -- Validate all header syntax before any namespace insertion.
               -- A short metadata tail takes precedence over owner errors.
               Path := Container_Header.Name;
               Limit := Container_Start + Container_Header.Extent;
               Offset := Container_Start + Container_Header.Body_Offset;
            elsif Is_Method or Is_Field then
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
            if not Is_Scope and then not Is_Container then
            if Offset = Limit then Result := Bad_Name; return; end if;
            Path := AML_Names.Read_Name
              (Data (Data'First + Offset .. Data'First + (Limit - 1)));
            if Path.Kind /= AML_Names.Accepted then
               Result := Bad_Name; return;
            end if;
            Offset := Offset + Path.Consumed;
            end if;
            if Op = Alias_Opcode then
               -- Resolve the source object, not its value; alias chains flatten
               -- to the same original node and incarnation.
               Alias_Source := Resolve_Object (Candidate, Frames (Depth).Scope, Path);
               if Alias_Source.Status /= Found then Result := Missing_Scope; return; end if;
               if Offset = Limit then Result := Bad_Name; return; end if;
               Path := AML_Names.Read_Name (Data (Data'First + Offset .. Data'First + (Limit - 1)));
               if Path.Kind /= AML_Names.Accepted or else Path.Count = 0 then Result := Bad_Name; return; end if;
               Offset := Offset + Path.Consumed;
            end if;
            if Is_Scope then
               Located := Resolve_Object (Candidate, Frames (Depth).Scope, Path);
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
                  declare Binding : constant Lookup_Result := Canonical_Child (Candidate, Scope, Path.Parts (I)); begin
                     if Binding.Status /= Found then Result := Missing_Scope; return; end if;
                     Scope := Binding.Node;
                  end;
                  if not Static_Scope (Kind (Candidate, Scope)) then Result := Missing_Scope; return; end if;
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
               Candidate.Items (Node).Object_Type := Processor_Object;
               Candidate.Items (Node).Processor_Info :=
                 (ID => AML_Decode.Byte (Container_Header.Metadata.Processor_ID),
                  PBlock_Address => Processor_Block_Address (Container_Header.Metadata.Block_Address),
                  PBlock_Length => AML_Decode.Byte (Container_Header.Metadata.Block_Length));
            elsif Is_Power then
               Candidate.Items (Node).Object_Type := Power_Resource_Object;
               Candidate.Items (Node).Power_Info :=
                 (System_Level => AML_Decode.Byte (Container_Header.Metadata.System_Level),
                  Order => Resource_Order (Container_Header.Metadata.Order));
            end if;
            if Op = Alias_Opcode then
               Candidate.Items (Node).Object_Type := Alias_Object;
               Candidate.Items (Node).Alias_Target := Alias_Source.Node;
               Candidate.Items (Node).Alias_Stamp :=
                 (if Alias_Source.Node = Root then 0 else Candidate.Items (Alias_Source.Node).Incarnation);
            elsif Is_Table then
               Static_Table_Selectors (Offset, Limit, Candidate.Items (Node).Static_Table_Selection, Result);
               if Result /= Loaded then return; end if;
               Candidate.Items (Node).Object_Type := Uninitialized_Region_Object;
               Candidate.Items (Node).Static_Table_Region := True;
            elsif Is_Method then
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
               AML_Objects.New_Event (Candidate.Values, Object_Ref, Allocation);
               if Allocation /= AML_Objects.Allocated then Result := Value_Limit; return; end if;
               Candidate.Items (Node).Object_Type := Event_Object;
               Candidate.Items (Node).Object_Ref := Object_Ref;
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
                       Data (Data'First + Offset .. Data'First + (Limit - 1)),
                       Source, Source_First + Offset, Width, Environment, Object_Ref, Used, Parsed);
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
                  Text := AML_Decode.Read_String_Span
                    (Data (Data'First + Offset .. Data'First + (Limit - 1)));
                  if Text.Kind /= AML_Decode.Accepted then
                     Result := (if Text.Kind = AML_Decode.Limit_Exceeded then
                                  Value_Limit else Bad_String);
                     return;
                  end if;
                  String_Reader.Allocate (Candidate.Values, Source,
                    Source_First + Offset + Text.Payload_Offset, Text.Length, Object_Ref, Allocation);
                  Offset := Offset + Text.Consumed;
                  if Allocation /= AML_Objects.Allocated then Result := Value_Limit; return; end if;
                  Candidate.Items (Node).Object_Ref := Object_Ref;
                  Candidate.Items (Node).Object_Type := String_Object;
               elsif Data (Data'First + Offset) = 16#11# then
                  Buffer_Item := AML_Buffer_Layouts.Read
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
                                 Buffer_Item := AML_Buffer_Layouts.With_Count
                                   (Bytes, Count.Value, Count.Consumed);
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
                  if Buffer_Item.Initializer_Length = 0 then
                     AML_Objects.New_Buffer (Candidate.Values, Buffer_Item.Effective_Length,
                       AML_Decode.Bytes'(1 .. 0 => 0), Object_Ref, Allocation);
                  else
                     AML_Objects.New_Buffer (Candidate.Values, Buffer_Item.Effective_Length,
                       Data (Data'First + (Offset + Buffer_Item.Initializer_Offset) ..
                             Data'First + (Offset + (Buffer_Item.Consumed - 1))),
                       Object_Ref, Allocation);
                  end if;
                  Offset := Offset + Buffer_Item.Consumed;
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
   end Load_Names_From_Source;
   procedure Load_Names
     (Tree : in out State; Data : AML_Decode.Bytes;
      Width : AML_Decode.Integer_Width; Result : out Load_Status)
   is
      Source : aliased constant AML_Byte_Sources.Storage :=
        (Length => Data'Length, Bytes => Data);
   begin
      Load_Names_From_Source (Tree, Data, Source, 0, Width, Result);
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
     (Mutex_Registry.Valid (A.Mutexes)
       and then AML_Identity."=" (Mutex_Registry.Owner_Of (A.Mutexes), A.Token)
       and then Frame_Pins.Valid (A.Frame_Roots) and then Pins.Valid (A.Retention) and then Pins.Owner (A.Retention) = A.Token
       and then Pins.Pending_Count (A.Retention) = 0
       and then A.Invocation_Issued <= Max_Invocations and then Valid_Context (A.Tree)
       and then AML_Module_Frames.Belongs_To (A.Tree.Module_Frames,
          AML_Frame_Handles.Bind_Domain (A.Token, A.Invocation_Issued))
       and then (for all I in 1 .. A.Tree.Used =>
         (if A.Tree.Items (I).Alive and then A.Tree.Items (I).Object_Type = Buffer_Field_Object then
            AML_References.Belongs_To (A.Tree.Items (I).Buffer_Field_Info.Backing, A.Token))) and then
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
          when AML_References.Absent | AML_References.Frame_Cell | AML_References.Name_Member | AML_References.Root_Namespace => False,
          when AML_References.Named_Cell =>
            AML_References.Named_Node (R) > 0
            and then AML_References.Named_Node (R) <= AML_References.Node_Position (A.Tree.Used)
            and then A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Alive
            and then A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Incarnation = AML_References.Incarnation (R)
            and then A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Object_Type in
              Integer_Object | String_Object | Buffer_Object | Package_Object | Reference_Object | Metadata_Value_Object | Uninitialized_Name_Object | Buffer_Field_Object,
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
      Initialize_Members_With_Owner (A.Tree, A.Token, Report);
   end Initialize_Members;
   function Mutex_Object_Live
     (A : Arena; Address : AML_Object_Identifiers.Object_Address; Level : Natural) return Boolean
   is
      ID : constant AML_Objects.Object_ID := AML_Object_Identifiers.Slot_Of (Address);
   begin
      return AML_Object_Identifiers.Present (Address)
        and then AML_Objects.Is_Live (A.Tree.Values, ID)
        and then AML_Object_Identifiers."=" (AML_Objects.Address_Of (A.Tree.Values, ID), Address)
        and then AML_Objects.Kind (A.Tree.Values, ID) = AML_Objects.Mutex_Object
        and then Natural (AML_Objects.Mutex_Level (A.Tree.Values, ID)) = Level;
   end Mutex_Object_Live;
   function Mutex_Method_Live (A : Arena; Key : Mutex_Method_Key; Level : Natural) return Boolean is
     (Key.Node /= Root and then Key.Node <= A.Tree.Used and then A.Tree.Items (Key.Node).Alive
      and then A.Tree.Items (Key.Node).Object_Type = Method_Object
      and then AML_References."=" (A.Tree.Items (Key.Node).Incarnation, Key.Stamp)
      and then Natural (A.Tree.Items (Key.Node).Method_Flags) = Key.Flags
      and then Natural (A.Tree.Items (Key.Node).Method_Offset) = Key.Offset
      and then Natural (A.Tree.Items (Key.Node).Method_Size) = Key.Size
      and then Natural (AML_Execute.Method_Level (A.Tree.Items (Key.Node).Method_Flags)) = Level);
   function Mutex_Frame_Live (A : Arena; Frame : AML_Frame_Handles.Frame_Handle) return Boolean is
     (AML_Frame_Handles.Present (Frame)
      and then AML_Frame_Handles.Belongs_To (Frame, AML_Frame_Handles.Bind_Domain (A.Token, A.Invocation_Issued))
      and then AML_Frame_Handles.Index_Of (Frame) in Mutex_Frame_Index
      and then AML_Frame_Handles."=" (A.Mutex_Frames (AML_Frame_Handles.Index_Of (Frame)), Frame));
   function Mutex_Registry_Live (A : Arena) return Boolean is
   begin
      if not Mutex_Registry.Valid (A.Mutexes)
        or else not AML_Identity."=" (Mutex_Registry.Owner_Of (A.Mutexes), A.Token) then return False; end if;
      for I in 1 .. Mutex_Registry.Held_Count (A.Mutexes) loop
         if not Mutex_Object_Live (A, Mutex_Registry.Held_Address (A.Mutexes, I),
             Mutex_Registry.Held_Level (A.Mutexes, I)) then return False; end if;
      end loop;
      for I in 1 .. Mutex_Registry.Method_Count (A.Mutexes) loop
         if not Mutex_Method_Live (A, Mutex_Registry.Method_At (A.Mutexes, I),
             Mutex_Registry.Method_Level (A.Mutexes, I)) then return False; end if;
      end loop;
      for I in A.Mutex_Method_Tokens'Range loop
         if not Mutex_Registry."=" (A.Mutex_Method_Tokens (I), Mutex_Registry.No_Token)
           and then not Mutex_Frame_Live (A, Mutex_Registry.Token_Frame (A.Mutex_Method_Tokens (I))) then return False; end if;
      end loop;
      return True;
   end Mutex_Registry_Live;
   function Mutex_Admission_For (A : Arena) return Mutex_Admission is
     (Owner => A.Token, Domain => AML_Frame_Handles.Bind_Domain (A.Token, A.Invocation_Issued),
      Registry_OK => Mutex_Registry_Live (A), others => <>);
   procedure Reset (A : in out Arena; Success : out Boolean) is
      Token : AML_Identity.Identity;
   begin
      if Frame_Pins.Count (A.Frame_Roots) /= 0 or else not Mutex_Registry_Live (A)
        or else Mutex_Registry.Active (A.Mutexes) then Success := False; return; end if;
      for I in A.Mutex_Frames'Range loop
         if AML_Frame_Handles.Present (A.Mutex_Frames (I))
           or else not Mutex_Registry."=" (A.Mutex_Method_Tokens (I), Mutex_Registry.No_Token)
         then Success := False; return; end if;
      end loop;
      AML_Identity.Issuer.Issue (Token, Success);
      if Success then
         declare
            Admission : Mutex_Admission := Mutex_Admission_For (A);
            Rotated : Mutex_Registry.Outcome;
            use type Mutex_Registry.Outcome;
         begin
            Admission.Kind := Epoch_Rotation; Admission.New_Owner := Token;
            Mutex_Registry.Rotate_Epoch (A.Mutexes, Admission, Token, Rotated);
            if Rotated /= Mutex_Registry.Succeeded then raise Program_Error with "mutex epoch rotation invariant"; end if;
         end;
         declare
            Result : Pins.Result_Status;
            use type Pins.Result_Status;
         begin
            if A.Token = AML_Identity.No_Identity then Pins.Bind (A.Retention, Token, Result);
            else Pins.Reset (A.Retention, Token, Result); end if;
            if Result /= Pins.Ready then raise Program_Error with "retention reset invariant"; end if;
         end;
         A.Token := Token; A.Tree := Empty; A.Invocation_Issued := 0;
         A.Notifications := (others => <>);
         A.Last_Fatal := (Present => False);
      end if;
   end Reset;
   function Fatal_Observation (A : Arena) return AML_Fatal.Observation is (A.Last_Fatal);
   function Module_Errors (A : Arena) return Module_Error_Observation is (A.Recovered_Module_Errors);
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
      elsif Value.Value_Kind = AML_Execute.Metadata_Datum then
         Refresh_Value (A, Canonical, Result);
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
   function Admitted_Descriptor (A : Arena; R : Reference) return Boolean;
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
         elsif Saved.Value.Value_Kind = Metadata_Datum then
            declare Normalized : Datum := Saved.Value; begin
               Refresh_Value (A, Normalized, Expected);
               if Expected = Returned then Canonical := Normalized; end if;
            end;
         else Canonical := Saved.Value; Expected := Returned;
         end if;
      end if;
      if Expected = Returned and then Canonical.Value_Kind = Reference_Datum
        and then not Admitted_Descriptor (A, Canonical.Ref)
      then Expected := Unsupported_Value; Canonical := (Integer_Datum, 0, AML_Decode.Ordinary_Integer); end if;
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
      if Canonical.Value_Kind = Metadata_Datum then
         Refresh_Value (A, Canonical, Result);
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
      elsif Saved.Value.Value_Kind = Metadata_Datum then
         declare Normalized : Datum := Saved.Value; begin
            Refresh_Value (A, Normalized, Status);
            if Status = Returned then Value := Normalized; end if;
         end;
      else Value := Saved.Value; Status := Returned;
      end if;
      if Status = Returned and then Value.Value_Kind = Reference_Datum
        and then not Admitted_Descriptor (A, Value.Ref)
      then Status := Unsupported_Value; Value := (Integer_Datum, 0, AML_Decode.Ordinary_Integer); end if;
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

   function Metadata_Matches (A : Arena; Item : AML_Execute.Metadata_Value) return Boolean is
   begin
      case Item.Mode is
         when AML_Execute.Live_Namespace =>
            return Named_Identity_Matches (A, Item.Namespace_Identity);
         when AML_Execute.Captured_Namespace =>
            return Has_Source (A, Item.Snapshot_Source)
              and then AML_Objects.Kind (A.Tree.Values,
                AML_References.Source (Item.Snapshot_Source)) = AML_Objects.Metadata_Snapshot_Object;
      end case;
   end Metadata_Matches;
   function Semantic_Reference_Readable (A : Arena; R : Reference) return Boolean is
      Description : AML_Execute.Reference_Metadata;
   begin
      if not Named_Identity_Matches (A, R) then return False; end if;
      Describe_Named_Identity (A, R, Description);
      return Description.Kind = AML_Execute.Metadata_Only
        or else (AML_References.Kind (R) = AML_References.Root_Namespace and then A.Tree.Root_Value /= 0)
        or else (Matches (A, R) and then A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Object_Type
          in Integer_Object | String_Object | Buffer_Object | Package_Object | Reference_Object | Metadata_Value_Object);
   end Semantic_Reference_Readable;
   procedure Describe_Metadata_Value
     (A : Arena; Item : AML_Execute.Metadata_Value; Result : out AML_Execute.Reference_Metadata) is
   begin
      Result := (Kind => AML_Execute.Invalid_Reference);
      if not Metadata_Matches (A, Item) then return; end if;
      case Item.Mode is
         when AML_Execute.Live_Namespace =>
            Describe_Named_Identity (A, Item.Namespace_Identity, Result);
         when AML_Execute.Captured_Namespace =>
            Result := (AML_Execute.Metadata_Only,
              AML_Objects.Metadata_Snapshot_Data (A.Tree.Values,
                AML_References.Source (Item.Snapshot_Source)).Object_Type);
      end case;
   end Describe_Metadata_Value;
   function Scalar_Resolved
     (A : Arena; Value : AML_Execute.Datum;
      Provenance : AML_Execute.Scalar_Provenance;
      Status : AML_Execute.Execution_Status) return Boolean
   is
   begin
      case Provenance.Kind is
         when AML_Execute.Detached_Scalar => return True;
         when AML_Execute.Owned_Integer_Scalar =>
            return Status = AML_Execute.Returned
              and then Value.Value_Kind = AML_Execute.Integer_Datum
              and then Has_Source (A, Provenance.Source)
              and then AML_Objects.Kind (A.Tree.Values,
                AML_References.Source (Provenance.Source)) = AML_Objects.Integer_Object
              and then Value.Number = AML_Objects.Integer_Data (A.Tree.Values,
                AML_References.Source (Provenance.Source))
              and then Value.Origin = AML_Objects.Origin_Of (A.Tree.Values,
                AML_References.Source (Provenance.Source));
      end case;
   end Scalar_Resolved;
   procedure Read_Integer_Source
     (A : Arena; Source : AML_References.Object_Handle;
      Value : out AML_Decode.Integer_Value; Origin : out AML_Decode.Integer_Origin;
      Status : out AML_Execute.Execution_Status)
   is
      ID : constant AML_Objects.Object_ID := AML_References.Source (Source);
   begin
      Value := 0; Origin := AML_Decode.Ordinary_Integer;
      Status := AML_Execute.Unsupported_Value;
      if not Has_Source (A, Source)
        or else AML_Objects.Kind (A.Tree.Values, ID) /= AML_Objects.Integer_Object
      then return; end if;
      Value := AML_Objects.Integer_Data (A.Tree.Values, ID);
      Origin := AML_Objects.Origin_Of (A.Tree.Values, ID);
      Status := AML_Execute.Returned;
   end Read_Integer_Source;
   function Integer_Normalization_Frame
     (Tree, Prior : State; ID : AML_Objects.Object_ID;
      Value : AML_Decode.Integer_Value) return Boolean is
     (Tree = (Prior with delta Values => Tree.Values)
      and then AML_Objects.Integer_Updated (Tree.Values, Prior.Values, ID, Value));
   procedure Normalize_Integer_Source
     (A : in out Arena; Source : AML_References.Object_Handle;
      Width : AML_Decode.Integer_Width; Value : out AML_Decode.Integer_Value;
      Origin : out AML_Decode.Integer_Origin; Status : out AML_Execute.Execution_Status)
   is
      ID : constant AML_Objects.Object_ID := AML_References.Source (Source);
   begin
      Read_Integer_Source (A, Source, Value, Origin, Status);
      if Status /= AML_Execute.Returned then return; end if;
      Value := AML_Integers.Normalize (Value, Width);
      AML_Objects.Set_Integer (A.Tree.Values, ID, Value);
   end Normalize_Integer_Source;
   procedure Resolve_Semantic_Reference
     (A : Arena; R : Reference; Value : out AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status) is
      Ignored : AML_Execute.Scalar_Provenance;
   begin
      Resolve_Semantic_Reference (A, R, Value, Ignored, Status);
   end Resolve_Semantic_Reference;
   procedure Resolve_Semantic_Reference
     (A : Arena; R : Reference; Value : out AML_Execute.Datum;
      Provenance : out AML_Execute.Scalar_Provenance;
      Status : out AML_Execute.Execution_Status) is
      Description : AML_Execute.Reference_Metadata;
   begin
      Provenance := AML_Execute.Detached_Provenance;
      Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
      Status := AML_Execute.Unsupported_Value;
      if not Named_Identity_Matches (A, R) then
         Resolve_Value (A, R, Value, Provenance, Status); return;
      end if;
      Describe_Named_Identity (A, R, Description);
      if Description.Kind = AML_Execute.Metadata_Only then
         Value := (AML_Execute.Metadata_Datum, (AML_Execute.Live_Namespace, R));
         Status := AML_Execute.Returned;
      elsif AML_References.Kind (R) = AML_References.Root_Namespace then
         if A.Tree.Root_Value /= 0 then
            Read_Source (A, AML_References.Bind_Object (A.Token,
              AML_Objects.Address_Of (A.Tree.Values, A.Tree.Root_Value)), Value, Provenance, Status);
         end if;
      else
         Resolve_Value (A, R, Value, Provenance, Status);
      end if;
   end Resolve_Semantic_Reference;
   procedure Read_Source
     (A : Arena; Source : AML_References.Object_Handle;
      Value : out AML_Execute.Datum; Status : out AML_Execute.Execution_Status) is
      Ignored : AML_Execute.Scalar_Provenance;
   begin
      Read_Source (A, Source, Value, Ignored, Status);
   end Read_Source;
   procedure Read_Source
     (A : Arena; Source : AML_References.Object_Handle;
      Value : out AML_Execute.Datum;
      Provenance : out AML_Execute.Scalar_Provenance; Status : out AML_Execute.Execution_Status)
   is
      use AML_Execute;
      ID : constant AML_Objects.Object_ID := AML_References.Source (Source);
      C32, C64 : AML_Coercions.Result;
   begin
      Provenance := AML_Execute.Detached_Provenance;
      Value := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
      Status := Unsupported_Value;
      if not Has_Source (A, Source) then return; end if;
      if AML_Objects.Kind (A.Tree.Values, ID) = AML_Objects.Reference_Object then
         Value := (Value_Kind => Reference_Datum, Ref => AML_Objects.Reference_Data (A.Tree.Values, ID));
      elsif AML_Objects.Kind (A.Tree.Values, ID) = AML_Objects.Metadata_Object then
         if not Semantic_Reference_Readable (A, AML_Objects.Metadata_Data (A.Tree.Values, ID)) then return; end if;
         Resolve_Semantic_Reference (A, AML_Objects.Metadata_Data (A.Tree.Values, ID), Value, Provenance, Status);
         return;
      elsif AML_Objects.Kind (A.Tree.Values, ID) = AML_Objects.Metadata_Snapshot_Object then
         Value := (Metadata_Datum, (Captured_Namespace, Source));
      elsif AML_Objects.Kind (A.Tree.Values, ID) = AML_Objects.Integer_Object then
         Value := (Value_Kind => Integer_Datum, Number => AML_Objects.Integer_Data (A.Tree.Values, ID), Origin => AML_Objects.Origin_Of (A.Tree.Values, ID));
         Provenance := (AML_Execute.Owned_Integer_Scalar, Source);
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
                when AML_Objects.Package_Object => 4, when AML_Objects.Event_Object => 7,
                when AML_Objects.Mutex_Object => AML_Mutexes.Object_Type_Code, when others => 0),
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
      and then (not AML_References.Has_Name_Member_Source (R)
        or else Has_Source (A, AML_References.Name_Member_Source (R)))
      and then AML_References.Named_Node (R) > 0
      and then AML_References.Named_Node (R) <= AML_References.Node_Position (A.Tree.Used)
      and then A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Alive
      and then A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Incarnation = AML_References.Incarnation (R)
      and then A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Object_Type in
        Integer_Object | String_Object | Buffer_Object | Package_Object | Reference_Object | Metadata_Value_Object | Uninitialized_Name_Object | Buffer_Field_Object);
   procedure Resolve_Name_Member (A : Arena; R : Reference; Value : out AML_Execute.Datum;
     Status : out AML_Execute.Execution_Status) is
      Ignored : AML_Execute.Scalar_Provenance;
   begin
      Resolve_Name_Member (A, R, Value, Ignored, Status);
   end Resolve_Name_Member;
   procedure Resolve_Name_Member (A : Arena; R : Reference; Value : out AML_Execute.Datum;
      Provenance : out AML_Execute.Scalar_Provenance;
     Status : out AML_Execute.Execution_Status)
   is
      ID : AML_Objects.Object_ID;
   begin
      Provenance := AML_Execute.Detached_Provenance;
      Value := (Value_Kind => AML_Execute.Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
      Status := AML_Execute.Unsupported_Value;
      if not Name_Member_Matches (A, R) then return; end if;
      ID := A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Object_Ref;
      if ID = 0 then Status := AML_Execute.Uninitialized; return; end if;
      Read_Source (A, AML_References.Bind_Object (A.Token, AML_Objects.Address_Of (A.Tree.Values, ID)), Value, Provenance, Status);
   end Resolve_Name_Member;
   function Metadata_Source_Resolved
     (A : Arena; ID : AML_Objects.Object_ID; Value : AML_Execute.Datum;
      Status : AML_Execute.Execution_Status) return Boolean
   is
      use type AML_References.Object_Handle;
      use AML_Execute;
      Ref : Reference;
      Description : Reference_Metadata;
      Current : AML_Objects.Object_ID;
   begin
      if AML_Objects.Kind (A.Tree.Values, ID) = AML_Objects.Metadata_Snapshot_Object then
         return Status = Returned and then Value = Datum'(Metadata_Datum,
           (Captured_Namespace, AML_References.Bind_Object (A.Token, AML_Objects.Address_Of (A.Tree.Values, ID))));
      end if;
      Ref := AML_Objects.Metadata_Data (A.Tree.Values, ID);
      if not Semantic_Reference_Readable (A, Ref) then
         return Status = Unsupported_Value and then Value = Datum'(Integer_Datum, 0, AML_Decode.Ordinary_Integer);
      end if;
      if Status /= Returned then return False; end if;
      Describe_Named_Identity (A, Ref, Description);
      if Description.Kind = Metadata_Only then
         return Value = Datum'(Metadata_Datum, (Live_Namespace, Ref));
      end if;
      Current := (if AML_References.Kind (Ref) = AML_References.Root_Namespace then A.Tree.Root_Value
        else A.Tree.Items (Node_ID (AML_References.Named_Node (Ref))).Object_Ref);
      if not AML_Objects.Is_Live (A.Tree.Values, Current) then return False; end if;
      case AML_Objects.Kind (A.Tree.Values, Current) is
         when AML_Objects.Integer_Object =>
            return Value = Datum'(Integer_Datum, AML_Objects.Integer_Data (A.Tree.Values, Current),
              AML_Objects.Origin_Of (A.Tree.Values, Current));
         when AML_Objects.Reference_Object =>
            return Value = Datum'(Reference_Datum, AML_Objects.Reference_Data (A.Tree.Values, Current));
         when AML_Objects.String_Object | AML_Objects.Buffer_Object | AML_Objects.Package_Object =>
            return Value.Value_Kind = Object_Datum and then Value.Object.ID = Current
              and then Value.Object.Source = AML_References.Bind_Object
                (A.Token, AML_Objects.Address_Of (A.Tree.Values, Current))
              and then Value.Object.Size = AML_Objects.Length (A.Tree.Values, Current)
              and then Value.Object.Type_Code = (case AML_Objects.Kind (A.Tree.Values, Current) is
                when AML_Objects.String_Object => 2, when AML_Objects.Buffer_Object => 3, when others => 4);
         when AML_Objects.Metadata_Object | AML_Objects.Metadata_Snapshot_Object | AML_Objects.Event_Object | AML_Objects.Mutex_Object => return False;
      end case;
   end Metadata_Source_Resolved;
   procedure Resolve_Value (A : Arena; R : Reference; Value : out AML_Execute.Datum;
     Status : out AML_Execute.Execution_Status) is
      Ignored : AML_Execute.Scalar_Provenance;
   begin
      Resolve_Value (A, R, Value, Ignored, Status);
   end Resolve_Value;
   procedure Resolve_Value (A : Arena; R : Reference; Value : out AML_Execute.Datum;
      Provenance : out AML_Execute.Scalar_Provenance;
     Status : out AML_Execute.Execution_Status) is
      use AML_Execute;
      ID : AML_Objects.Object_ID;
   begin
      Provenance := AML_Execute.Detached_Provenance;
      Value := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
      Status := Unsupported_Value;
      if not Matches (A, R) then return; end if;
      if AML_References.Kind (R) = AML_References.Named_Cell then
         ID := A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Object_Ref;
         if ID = 0 then Status := Uninitialized; return; end if;
         Read_Source (A, AML_References.Bind_Object (A.Token, AML_Objects.Address_Of (A.Tree.Values, ID)), Value, Provenance, Status);
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
      Read_Source (A, AML_References.Bind_Object (A.Token, AML_Objects.Address_Of (A.Tree.Values, ID)), Value, Provenance, Status);
   end Resolve_Value;

   function Has_Source (A : Arena; H : AML_References.Object_Handle) return Boolean is
     (AML_References.Belongs_To (H, A.Token)
       and then AML_Objects.Matches_Address (A.Tree.Values, AML_References.Address (H)));
   function Buffer_Field_Defined (Tree, Prior : State; Node : Node_ID;
      Source : AML_References.Object_Handle; Offset, Bits : AML_Decode.Integer_Value;
      Always_Buffer : Boolean) return Boolean is
     (Node = Prior.Used + 1 and then Tree.Used = Node
      and then Tree = (Prior with delta Items => Tree.Items, Used => Tree.Used,
        Last_Stamp => Tree.Last_Stamp)
      and then (for all I in 1 .. Prior.Used => Tree.Items (I) = Prior.Items (I))
      and then Tree.Items (Node).Alive and then not Tree.Items (Node).Initializing
      and then Tree.Items (Node).Object_Type = Buffer_Field_Object
      and then Tree.Items (Node).Object_Ref = 0
      and then Tree.Items (Node).Buffer_Field_Info =
        Buffer_Field_Descriptor'(Source, Offset, Bits, Always_Buffer));
   function Buffer_Field_Read_Frame (Tree, Prior : State) return Boolean is
     (Tree = (Prior with delta Values => Tree.Values)
      and then AML_Objects.Extends (Tree.Values, Prior.Values));
   function Buffer_Field_Write_Frame (Tree, Prior : State; Field : Reference)
      return Boolean
   is
      Node : constant Node_ID := Node_ID (AML_References.Named_Node (Field));
      Info : constant Buffer_Field_Descriptor := Prior.Items (Node).Buffer_Field_Info;
      ID : constant AML_Objects.Object_ID := AML_References.Source (Info.Backing);
   begin
      return Tree = (Prior with delta Values => Tree.Values)
        and then AML_Objects.Buffer_Stored (Tree.Values, Prior.Values, ID,
          AML_Objects.Byte_Data (Tree.Values, ID))
        and then (for all J in 0 .. AML_Objects.Length (Prior.Values, ID) - 1 =>
          (for all B in AML_Buffer_Bits.Bit_Index =>
            (if AML_Decode.Integer_Value (J) * AML_Buffer_Bits.Bits_Per_Byte + AML_Decode.Integer_Value (B) < Info.Offset
              or else AML_Decode.Integer_Value (J) * AML_Buffer_Bits.Bits_Per_Byte + AML_Decode.Integer_Value (B) - Info.Offset >= Info.Bits
             then AML_Buffer_Bits.Bit_Set (AML_Objects.Stored_Byte (Tree.Values, ID, J), B) =
               AML_Buffer_Bits.Bit_Set (AML_Objects.Stored_Byte (Prior.Values, ID, J), B))));
   end Buffer_Field_Write_Frame;
   procedure Define_Buffer_Field_For_Context
     (A : in out Arena; Scope, Lifetime_Owner : Node_ID; Path : AML_Names.Name_Result;
      Source : AML_References.Object_Handle; Bit_Offset, Bit_Length : AML_Decode.Integer_Value;
      Always_Buffer : Boolean; Field : out Reference; Status : out AML_Execute.Execution_Status; Module_Declaration : Boolean)
   is
      Base, Node : Node_ID;
      Added : Insert_Status;
   begin
      Field := AML_References.No_Reference; Status := AML_Execute.Unsupported_Value;
      if not Has_Source (A, Source) or else
        AML_Objects.Kind (A.Tree.Values, AML_References.Source (Source)) /= AML_Objects.Buffer_Object
      then return; end if;
      if Bit_Length = 0 then Status := AML_Execute.Invalid_Field_Length; return; end if;
      if not AML_Buffer_Bits.Fits (AML_Objects.Length (A.Tree.Values,
        AML_References.Source (Source)), Bit_Offset, Bit_Length)
      then Status := AML_Execute.Buffer_Field_Limit; return; end if;
      Name_Target (A.Tree, Natural (Scope), Natural (Lifetime_Owner), Path, Base, Status, Module_Declaration);
      if Status /= AML_Execute.Returned then return; end if;
      declare Candidate : State := A.Tree; begin
         Insert (Candidate, Base, Path.Parts (Path.Count), Node, Added);
         case Added is
            when Duplicate => Status := AML_Execute.Duplicate_Name; return;
            when Full => Status := AML_Execute.Namespace_Limit; return;
            when Invalid_Name => Status := AML_Execute.Bad_Name; return;
            when Inserted => null;
         end case;
         Candidate.Items (Node) := (Candidate.Items (Node) with delta
           Owner => Lifetime_Owner, Object_Type => Buffer_Field_Object, Object_Ref => 0,
           Buffer_Field_Info => (Source, Bit_Offset, Bit_Length, Always_Buffer));
         A.Tree := Candidate;
         Field := AML_References.Bind_Named (A.Token, AML_References.Node_Position (Node),
           A.Tree.Items (Node).Incarnation);
         Status := AML_Execute.Returned;
      end;
   end Define_Buffer_Field_For_Context;
   procedure Define_Buffer_Field
     (A : in out Arena; Scope : Node_ID; Path : AML_Names.Name_Result;
      Source : AML_References.Object_Handle; Bit_Offset, Bit_Length : AML_Decode.Integer_Value;
      Always_Buffer : Boolean; Field : out Reference; Status : out AML_Execute.Execution_Status) is
   begin
      Define_Buffer_Field_For_Context (A, Scope, Scope, Path, Source,
        Bit_Offset, Bit_Length, Always_Buffer, Field, Status, False);
   end Define_Buffer_Field;
   function Buffer_Field_Value_Defined
     (Tree, Prior : State; Scope : Natural; Width : AML_Decode.Integer_Width;
      Source : AML_Execute.Datum; Offset, Bits : AML_Decode.Integer_Value;
      Always_Buffer : Boolean) return Boolean
   is
      use type AML_References.Object_Handle;
      Node : constant Node_ID := Tree.Used;
      ID : AML_Objects.Object_ID;
      Source_ID : AML_Objects.Object_ID;
   begin
      if Node = Root or else not Buffer_Field_Defined
        (Tree, (Prior with delta Values => Tree.Values), Node,
         Tree.Items (Node).Buffer_Field_Info.Backing, Offset, Bits, Always_Buffer)
        or else Scope > Node_ID'Last
        or else Tree.Items (Node).Owner /= Node_ID (Scope)
        or else not AML_Objects.Extends (Tree.Values, Prior.Values)
      then return False; end if;
      ID := AML_References.Source (Tree.Items (Node).Buffer_Field_Info.Backing);
      if not AML_Objects.Is_Live (Tree.Values, ID)
        or else AML_Objects.Kind (Tree.Values, ID) /= AML_Objects.Buffer_Object
      then return False; end if;
      case Source.Value_Kind is
         when AML_Execute.Reference_Datum | AML_Execute.Metadata_Datum => return False;
         when AML_Execute.Integer_Datum =>
            return AML_Objects.Length (Tree.Values, ID) = Field_Integer_Bits (Width) / Field_Byte_Bits
              and then (for all J in 0 .. AML_Objects.Length (Tree.Values, ID) - 1 =>
                AML_Objects.Stored_Byte (Tree.Values, ID, J) =
                  AML_Coercions.Octet (AML_Integers.Normalize (Source.Number, Width), J));
         when AML_Execute.Object_Datum =>
            Source_ID := Source.Object.ID;
            if not AML_Objects.Is_Live (Prior.Values, Source_ID) then return False; end if;
            if AML_Objects.Kind (Prior.Values, Source_ID) = AML_Objects.Buffer_Object then
               return Tree.Items (Node).Buffer_Field_Info.Backing = Source.Object.Source;
            elsif AML_Objects.Kind (Prior.Values, Source_ID) = AML_Objects.String_Object then
               return AML_Objects.Length (Tree.Values, ID) = AML_Objects.Length (Prior.Values, Source_ID) + 1
                 and then AML_Objects.Stored_Byte (Tree.Values, ID, AML_Objects.Length (Tree.Values, ID) - 1) = 0
                 and then (for all J in 1 .. AML_Objects.Length (Prior.Values, Source_ID) =>
                   AML_Objects.Stored_Byte (Tree.Values, ID, J - 1) = AML_Objects.Stored_Byte (Prior.Values, Source_ID, J - 1));
            else return False; end if;
      end case;
   end Buffer_Field_Value_Defined;
   procedure Define_Buffer_Field_Value_For_Context
     (A : in out Arena; Scope, Lifetime_Owner : Natural; Path : AML_Names.Name_Result;
      Width : AML_Decode.Integer_Width; Source : AML_Execute.Datum;
      Bit_Offset, Bit_Length : AML_Decode.Integer_Value; Always_Buffer : Boolean;
      Status : out AML_Execute.Execution_Status; Module_Declaration : Boolean)
   is
      Converted, Unused : AML_Execute.Datum;
      Field : Reference;
      Prior_Values : constant AML_Objects.State := A.Tree.Values;
   begin
      Status := AML_Execute.Unsupported_Value;
      -- Operand conversion precedes declaration name/duplicate validation.
      To_Buffer_And_Attach (A, Width, Source,
        (Kind => AML_Execute.Detached_Result), Converted, Unused, Status);
      if Status /= AML_Execute.Returned then return; end if;
      if Scope > A.Tree.Used then
         Status := AML_Execute.Unknown_Name;
      else
         Define_Buffer_Field_For_Context (A, Node_ID (Scope), Node_ID (Lifetime_Owner), Path, Converted.Object.Source,
           Bit_Offset, Bit_Length, Always_Buffer, Field, Status, Module_Declaration);
      end if;
      if Status /= AML_Execute.Returned then A.Tree.Values := Prior_Values; end if;
   end Define_Buffer_Field_Value_For_Context;
   procedure Define_Buffer_Field_Value
     (A : in out Arena; Scope : Natural; Path : AML_Names.Name_Result;
      Width : AML_Decode.Integer_Width; Source : AML_Execute.Datum;
      Bit_Offset, Bit_Length : AML_Decode.Integer_Value; Always_Buffer : Boolean;
      Status : out AML_Execute.Execution_Status) is
   begin
      Define_Buffer_Field_Value_For_Context (A, Scope, Scope, Path, Width, Source,
        Bit_Offset, Bit_Length, Always_Buffer, Status, False);
   end Define_Buffer_Field_Value;
   function Buffer_Field_Matches (A : Arena; Field : Reference) return Boolean is
     (AML_References.Kind (Field) = AML_References.Named_Cell
      and then Matches (A, Field)
      and then A.Tree.Items (Node_ID (AML_References.Named_Node (Field))).Object_Type = Buffer_Field_Object
      and then Has_Source (A, A.Tree.Items (Node_ID (AML_References.Named_Node (Field))).Buffer_Field_Info.Backing));
   function Table_Field_Matches (A : Arena; Field : Reference) return Boolean is
     (AML_References.Kind (Field) = AML_References.Named_Cell
      and then Named_Identity_Matches (A, Field)
      and then A.Tree.Items (Node_ID (AML_References.Named_Node (Field))).Object_Type = Table_Field_Object);
   procedure Read_Buffer_Field
     (A : in out Arena; Field : Reference; Width : AML_Decode.Integer_Width;
      Value : out AML_Execute.Datum; Status : out AML_Execute.Execution_Status)
   is
      Extracted : Field_Result;
      Prior_Values : constant AML_Objects.State := A.Tree.Values;
   begin
      Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
      Status := AML_Execute.Unsupported_Value;
      if not Buffer_Field_Matches (A, Field) then return; end if;
      Extract_Buffer_Field (A.Tree, A.Token,
        Node_ID (AML_References.Named_Node (Field)), Width, Extracted, Status);
      if Status /= AML_Execute.Returned then return; end if;
      case Extracted.Kind is
         when Scalar_Field => Value.Number := Extracted.Number;
         when Buffer_Field =>
            Read_Source (A, AML_References.Bind_Object (A.Token,
              AML_Objects.Address_Of (A.Tree.Values, Extracted.Object)), Value, Status);
            if Status /= AML_Execute.Returned then
               A.Tree.Values := Prior_Values;
               Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
            end if;
      end case;
   end Read_Buffer_Field;
   procedure Write_Buffer_Field
     (A : in out Arena; Field : Reference; Width : AML_Decode.Integer_Width;
      Item : AML_Execute.Datum; Status : out AML_Execute.Execution_Status)
   is
      use type AML_Objects.Buffer_Update_Status;
      Source_ID : AML_Objects.Object_ID := AML_Objects.No_Object;
      Number : AML_Decode.Integer_Value := 0;
      function Source_Bit (K : Natural) return Boolean is
      begin
         if Item.Value_Kind = AML_Execute.Integer_Datum then
            return K < Field_Integer_Bits (AML_Decode.Bits_64) and then (Number / 2 ** K) mod 2 /= 0;
         elsif K / Field_Byte_Bits < AML_Objects.Length (A.Tree.Values, Source_ID) then
            return AML_Buffer_Bits.Bit_Set
              (AML_Objects.Stored_Byte (A.Tree.Values, Source_ID, K / Field_Byte_Bits), K mod Field_Byte_Bits);
         end if;
         return False;
      end Source_Bit;
   begin
      Status := AML_Execute.Unsupported_Value;
      if not Buffer_Field_Matches (A, Field) then return; end if;
      if Item.Value_Kind = AML_Execute.Integer_Datum then
         Number := AML_Integers.Normalize (Item.Number, Width);
      elsif Item.Value_Kind = AML_Execute.Object_Datum then
         if not Has_Source (A, Item.Object.Source)
           or else Item.Object.ID /= AML_References.Source (Item.Object.Source)
         then return; end if;
         Source_ID := Item.Object.ID;
         if AML_Objects.Kind (A.Tree.Values, Source_ID) not in AML_Objects.String_Object | AML_Objects.Buffer_Object
         then return; end if;
      else return; end if;
      declare
         Info : constant Buffer_Field_Descriptor :=
           A.Tree.Items (Node_ID (AML_References.Named_Node (Field))).Buffer_Field_Info;
         ID : constant AML_Objects.Object_ID := AML_References.Source (Info.Backing);
      begin
         if Info.Offset > AML_Decode.Integer_Value (Max_Field_Bits)
           or else Info.Bits > AML_Decode.Integer_Value (Max_Field_Bits)
           or else not AML_Buffer_Bits.Fits (AML_Objects.Length (A.Tree.Values, ID), Info.Offset, Info.Bits)
         then Status := AML_Execute.Buffer_Field_Limit; return; end if;
         declare
            First : constant Natural := Natural (Info.Offset);
            Bits : constant Positive := Positive (Info.Bits);
            Data : AML_Decode.Bytes := AML_Objects.Byte_Data (A.Tree.Values, ID);
            Candidate : AML_Objects.State := A.Tree.Values;
            Stored : AML_Objects.Buffer_Update_Status;
         begin
            -- Read every source bit from the unchanged arena; stage all writes.
            for K in 0 .. Bits - 1 loop
               declare J : constant Positive := Data'First + (First + K) / Field_Byte_Bits; begin
                  Data (J) := AML_Buffer_Bits.Replace_Bit (Data (J), (First + K) mod Field_Byte_Bits, Source_Bit (K));
               end;
            end loop;
            AML_Objects.Store_Buffer (Candidate, ID, Data, Stored);
            if Stored /= AML_Objects.Buffer_Updated then Status := AML_Execute.Value_Limit; return; end if;
            A.Tree.Values := Candidate; Status := AML_Execute.Returned;
         end;
      end;
   end Write_Buffer_Field;

   function Root_Target (A : Arena; R : Reference) return AML_Object_Identifiers.Object_Address is
      ID : AML_Objects.Object_ID := AML_Objects.No_Object;
   begin
      case AML_References.Kind (R) is
         when AML_References.Named_Cell =>
            if Matches (A, R) then
               if A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Object_Type = Buffer_Field_Object then
                  declare H : constant AML_References.Object_Handle :=
                    A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Buffer_Field_Info.Backing;
                  begin
                     if Has_Source (A, H) then return AML_References.Address (H); end if;
                     return AML_Object_Identifiers.No_Address;
                  end;
               end if;
               ID := A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Object_Ref;
            end if;
         when AML_References.Name_Member =>
            -- Only an actual captured object is a root. A deferred NAME has
            -- no captured edge; its live current value is rooted by the node.
            -- Node expiry invalidates reads without inventing ownership.
            if AML_References.Belongs_To (R, A.Token)
              and then AML_References.Has_Name_Member_Source (R)
              and then Has_Source (A, AML_References.Name_Member_Source (R))
            then return AML_References.Address (AML_References.Name_Member_Source (R)); end if;
         when AML_References.Byte_Slot | AML_References.Package_Slot =>
            if Matches (A, R) then ID := AML_References.Target (R); end if;
         when AML_References.Root_Namespace =>
            if Named_Identity_Matches (A, R) then ID := A.Tree.Root_Value; end if;
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
         when AML_Execute.Metadata_Datum =>
            Accepted := Metadata_Matches (A, Value.Metadata);
            if Accepted then
               case Value.Metadata.Mode is
                  when AML_Execute.Captured_Namespace =>
                     Address := AML_References.Address (Value.Metadata.Snapshot_Source);
                  when AML_Execute.Live_Namespace =>
                     Address := Root_Target (A, Value.Metadata.Namespace_Identity);
               end case;
            end if;
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
      if not Mutex_Registry_Live (A) then Status := Invalid_Root_Value; return; end if;
      for I in 1 .. Mutex_Registry.Held_Count (A.Mutexes) loop
         Seeds (AML_Object_Identifiers.Slot_Of (Mutex_Registry.Held_Address (A.Mutexes, I))) := True;
      end loop;
      if A.Tree.Root_Value /= 0 then Seeds (A.Tree.Root_Value) := True; end if;
      for Node in 1 .. A.Tree.Used loop
         if A.Tree.Items (Node).Alive and then A.Tree.Items (Node).Object_Type in
           Integer_Object | String_Object | Buffer_Object | Package_Object | Reference_Object | Metadata_Value_Object | Event_Object
         then Seeds (A.Tree.Items (Node).Object_Ref) := True;
         elsif A.Tree.Items (Node).Alive and then A.Tree.Items (Node).Object_Type = Mutex_Object
           and then A.Tree.Items (Node).Object_Ref /= AML_Objects.No_Object
         then Seeds (A.Tree.Items (Node).Object_Ref) := True;
         elsif A.Tree.Items (Node).Alive and then A.Tree.Items (Node).Object_Type = Buffer_Field_Object then
            if not Has_Source (A, A.Tree.Items (Node).Buffer_Field_Info.Backing) then
               Status := Invalid_Root_Value; return;
            end if;
            Seeds (AML_References.Source (A.Tree.Items (Node).Buffer_Field_Info.Backing)) := True;
         end if;
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

   procedure Semantic_Reference_Edge
        (A : Arena; R : Reference; Address : out AML_Object_Identifiers.Object_Address;
         Invalid, Ambiguous : out Boolean)
      is
      begin
         Address := AML_Object_Identifiers.No_Address;
         Invalid := not AML_References.Well_Formed (R);
         Ambiguous := False;
         if Invalid then return; end if;
         if AML_References.Kind (R) /= AML_References.Frame_Cell
           and then not AML_References.Belongs_To (R, A.Token)
         then Invalid := True; return; end if;
         case AML_References.Kind (R) is
            when AML_References.Byte_Slot | AML_References.Package_Slot =>
               if not Matches (A, R) then Invalid := True; return; end if;
               Address := AML_Objects.Address_Of (A.Tree.Values, AML_References.Target (R));
            when AML_References.Name_Member =>
               -- An authenticated no-capture NAME is a complete empty edge,
               -- including after its node expires. Reads check node lifetime.
               if not AML_References.Has_Name_Member_Source (R) then return; end if;
               if not Has_Source (A, AML_References.Name_Member_Source (R)) then
                  Invalid := True; return;
               end if;
               Address := AML_References.Address (AML_References.Name_Member_Source (R));
            when AML_References.Named_Cell | AML_References.Root_Namespace |
                 AML_References.Frame_Cell | AML_References.Absent => null;
         end case;
   end Semantic_Reference_Edge;
   procedure Seed_Semantic_Value
     (A : Arena; Value : AML_Execute.Datum; Scratch : in out Semantic_Owner_Workspace;
      Accepted : out Boolean) is
         Address : AML_Object_Identifiers.Object_Address := AML_Object_Identifiers.No_Address;
         Invalid, Ambiguous : Boolean;
      begin
         Accepted := True;
         case Value.Value_Kind is
            when AML_Execute.Integer_Datum => null;
            when AML_Execute.Object_Datum =>
               Accepted := Value.Object.ID = AML_References.Source (Value.Object.Source)
                 and then Has_Source (A, Value.Object.Source);
               if Accepted then Address := AML_References.Address (Value.Object.Source); end if;
            when AML_Execute.Reference_Datum =>
               Semantic_Reference_Edge (A, Value.Ref, Address, Invalid, Ambiguous);
               Accepted := not Invalid;
               Scratch.Incomplete := Scratch.Incomplete or Ambiguous;
            when AML_Execute.Metadata_Datum =>
               -- Metadata observation is not ownership of the mutable value.
               Accepted := Metadata_Matches (A, Value.Metadata);
         end case;
         if Accepted and then AML_Object_Identifiers.Present (Address) then
            Scratch.Seeds (AML_Object_Identifiers.Slot_Of (Address)) := True;
         end if;
   end Seed_Semantic_Value;
   procedure Begin_Integer_Owner_Scan
     (A : Arena; Source : AML_References.Object_Handle;
      Scratch : in out Semantic_Owner_Workspace; Result : out Persistent_Owner_Result)
   is
      use type Pins.Result_Status;
      Accepted : Boolean;
   begin
      Scratch.Phase := Scan_Collecting;
      Scratch.Owner := A.Token;
      Scratch.Source := Source;
      Scratch.Incomplete := False;
      Scratch.Seeds := [others => False];
      Scratch.Keep := [others => False];
      Scratch.Targets := [others => AML_Object_Identifiers.No_Address];
      Scratch.Invalid_Edges := [others => False];
      Scratch.Ambiguous_Edges := [others => False];
      Scratch.Problem := Persistent_Uninitialized;
      if not Initialized (A) then Result := Scratch.Problem; return; end if;
      Scratch.Problem := Persistent_Invalid_Source;
      if not Has_Source (A, Source)
        or else AML_Objects.Kind (A.Tree.Values, AML_References.Source (Source))
          /= AML_Objects.Integer_Object
      then Result := Scratch.Problem; return; end if;
      if A.Tree.Root_Value /= AML_Objects.No_Object then
         Scratch.Seeds (A.Tree.Root_Value) := True;
      end if;
      Scratch.Problem := Persistent_Invalid_Root;
      for Node in 1 .. A.Tree.Used loop
         if A.Tree.Items (Node).Alive then
            if A.Tree.Items (Node).Object_Type in Integer_Object | String_Object |
              Buffer_Object | Package_Object | Reference_Object | Metadata_Value_Object
            then
               Scratch.Seeds (A.Tree.Items (Node).Object_Ref) := True;
            elsif A.Tree.Items (Node).Object_Type = Buffer_Field_Object then
               if not Has_Source (A, A.Tree.Items (Node).Buffer_Field_Info.Backing) then Result := Scratch.Problem; return; end if;
               Scratch.Seeds (AML_References.Source (A.Tree.Items (Node).Buffer_Field_Info.Backing)) := True;
            end if;
         end if;
      end loop;
      for Index in Pins.Root_Index loop
         declare Saved : constant Pins.Read_Result := Pins.Published_At (A.Retention, Index); begin
            if Saved.Status = Pins.Ready then
               Seed_Semantic_Value (A, Saved.Value, Scratch, Accepted);
               if not Accepted then Result := Scratch.Problem; return; end if;
            end if;
         end;
      end loop;
      Scratch.Problem := (if Scratch.Incomplete then Persistent_Census_Incomplete
                          else Persistent_Scan_Collecting);
      Result := Scratch.Problem;
   end Begin_Integer_Owner_Scan;

   procedure Validate_Scan_Context
     (A : Arena; Scratch : in out Semantic_Owner_Workspace) is
   begin
      if Scratch.Problem /= Persistent_Scan_Collecting then return; end if;
      if Scratch.Owner /= A.Token or else not Initialized (A) then
         Scratch.Problem := Persistent_Invalid_Source;
      elsif not Has_Source (A, Scratch.Source) or else
        AML_Objects.Kind (A.Tree.Values, AML_References.Source (Scratch.Source)) /= AML_Objects.Integer_Object
      then Scratch.Problem := Persistent_Invalid_Source; end if;
   end Validate_Scan_Context;

   procedure Add_Transient_Owner
     (A : Arena; Value : AML_Execute.Datum;
      Scratch : in out Semantic_Owner_Workspace; Result : out Persistent_Owner_Result)
   is
      Accepted : Boolean;
   begin
      if Scratch.Phase /= Scan_Collecting then Result := Persistent_Invalid_Sequence; return; end if;
      Validate_Scan_Context (A, Scratch);
      if Scratch.Problem = Persistent_Scan_Collecting then
         if Value.Value_Kind = AML_Execute.Integer_Datum then
            Scratch.Problem := Persistent_Invalid_Root;
         else
            Seed_Semantic_Value (A, Value, Scratch, Accepted);
            if not Accepted then Scratch.Problem := Persistent_Invalid_Root;
            elsif Scratch.Incomplete then Scratch.Problem := Persistent_Census_Incomplete; end if;
         end if;
      end if;
      Result := Scratch.Problem;
   end Add_Transient_Owner;

   procedure Finish_Integer_Owner_Scan
     (A : Arena; Scratch : in out Semantic_Owner_Workspace;
      Result : out Persistent_Owner_Result)
   is
      use type AML_Objects.Reachability.Trace_Status;
      use type AML_Persistent_Owner_Graph.Result;
      Walk_Status : AML_Objects.Reachability.Trace_Status;
      Graph_Result : AML_Persistent_Owner_Graph.Result;
      Incomplete : Boolean := Scratch.Incomplete;
      Source : constant AML_References.Object_Handle := Scratch.Source;
   begin
      if Scratch.Phase /= Scan_Collecting then Result := Persistent_Invalid_Sequence; return; end if;
      Scratch.Phase := Scan_Finished;
      Validate_Scan_Context (A, Scratch);
      Result := Scratch.Problem;
      if Result /= Persistent_Scan_Collecting then return; end if;
      Result := Persistent_Invalid_Root;
      for ID in 1 .. AML_Objects.Slot_Bound (A.Tree.Values) loop
         if AML_Objects.Is_Live (A.Tree.Values, ID)
           and then AML_Objects.Kind (A.Tree.Values, ID) = AML_Objects.Reference_Object
         then
            Semantic_Reference_Edge (A, AML_Objects.Reference_Data (A.Tree.Values, ID),
              Scratch.Targets (ID), Scratch.Invalid_Edges (ID), Scratch.Ambiguous_Edges (ID));
         end if;
      end loop;
      AML_Objects.Reachability.Trace
        (A.Tree.Values, Scratch.Seeds, Scratch.Targets, Scratch.Walk, Scratch.Keep, Walk_Status);
      if Walk_Status /= AML_Objects.Reachability.Traced then return; end if;
      -- Validate all reached edges, even when the source was found early.
      Graph_Result := AML_Persistent_Owner_Graph.Classify
        (Scratch.Keep, Scratch.Invalid_Edges, Scratch.Ambiguous_Edges,
         AML_References.Source (Source), Incomplete);
      Result := Persistent_Invalid_Edge;
      if Graph_Result = AML_Persistent_Owner_Graph.Invalid_Reachable_Edge then return; end if;
      Incomplete := Graph_Result = AML_Persistent_Owner_Graph.Incomplete_Census;
      Result := Persistent_Invalid_Root;
      for Index in 1 .. Pending.Count (A.Tree.Journal) loop
         declare Item : constant Pending.Member_Result := Pending.Item (A.Tree.Journal, Index); begin
            if not Item.Found or else not AML_Objects.Is_Live (A.Tree.Values, Item.Package_ID)
              or else AML_Objects.Kind (A.Tree.Values, Item.Package_ID) /= AML_Objects.Package_Object
              or else Item.Element >= AML_Objects.Length (A.Tree.Values, Item.Package_ID)
              or else Item.Scope > A.Tree.Used
              or else (Item.Scope /= Root and then not A.Tree.Items (Item.Scope).Alive)
            then return; end if;
            -- Journal-only retention is not an authenticated semantic owner.
            if not Scratch.Keep (Item.Package_ID) then Incomplete := True; end if;
         end;
      end loop;
      if Incomplete then Result := Persistent_Census_Incomplete;
      elsif Scratch.Keep (AML_References.Source (Source)) then Result := Persistent_Owner_Found;
      else Result := No_Persistent_Owner_Found;
      end if;
   end Finish_Integer_Owner_Scan;

   procedure Scan_Persistent_Integer_Owners
     (A : Arena; Source : AML_References.Object_Handle;
      Scratch : in out Semantic_Owner_Workspace;
      Result : out Persistent_Owner_Result)
   is
   begin
      Begin_Integer_Owner_Scan (A, Source, Scratch, Result);
      Finish_Integer_Owner_Scan (A, Scratch, Result);
   end Scan_Persistent_Integer_Owners;

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
      if Allocated = AML_Objects.Unsupported_Copy then return; end if;
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
      if Item.Value_Kind = AML_Execute.Metadata_Datum then
         if not Metadata_Matches (A, Item.Metadata) then
            Status := AML_Execute.Unsupported_Value;
         elsif Item.Metadata.Mode = AML_Execute.Live_Namespace then
            declare
               Normalized : AML_Execute.Datum;
            begin
               Resolve_Semantic_Reference (A, Item.Metadata.Namespace_Identity, Normalized, Status);
               if Status = AML_Execute.Returned then Item := Normalized; end if;
            end;
         end if;
         return;
      end if;
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
      if Value.Value_Kind = AML_Execute.Metadata_Datum then
         if not Metadata_Matches (A, Value.Metadata) then return; end if;
         Copy := Value; Status := AML_Execute.Returned;
      elsif Value.Value_Kind = AML_Execute.Reference_Datum then
         if not Admitted_Descriptor (A, Value.Ref) then return; end if;
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
   procedure Copy_Value
     (A : in out Arena; Width : AML_Decode.Integer_Width;
      Item : AML_Execute.Datum; Copy : out AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
   is
      Value : AML_Execute.Datum := Item;
      Description : AML_Execute.Reference_Metadata;
      ID : AML_Objects.Object_ID;
      Allocated : AML_Objects.Allocation_Status;
   begin
      Copy := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
      Status := AML_Execute.Unsupported_Value;
      Refresh_Value (A, Value, Status);
      if Status /= AML_Execute.Returned then return; end if;
      Status := AML_Execute.Unsupported_Value;
      if Value.Value_Kind = AML_Execute.Metadata_Datum then
         Describe_Metadata_Value (A, Value.Metadata, Description);
         if Description.Kind /= AML_Execute.Metadata_Only then return; end if;
         AML_Objects.New_Metadata_Snapshot (A.Tree.Values,
           (Object_Type => Description.Object_Type), ID, Allocated);
         if Allocated in AML_Objects.Invalid_Metadata | AML_Objects.Unsupported_Copy then return; end if;
         if Allocated /= AML_Objects.Allocated then Status := AML_Execute.Value_Limit; return; end if;
         Copy := (AML_Execute.Metadata_Datum, (AML_Execute.Captured_Namespace,
           AML_References.Bind_Object (A.Token, AML_Objects.Address_Of (A.Tree.Values, ID))));
         Status := AML_Execute.Returned;
      elsif Value.Value_Kind = AML_Execute.Reference_Datum
        and then AML_References.Kind (Value.Ref) in AML_References.Byte_Slot | AML_References.Package_Slot
      then
         if Matches (A, Value.Ref) then Copy := Value; Status := AML_Execute.Returned; end if;
      else Clone_Value (A, Width, Value, Copy, Status); end if;
   end Copy_Value;
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
      if not Initialized (A) or else Value.Value_Kind = AML_Execute.Metadata_Datum then return; end if;
      if Path.Kind /= AML_Names.Accepted or else Path.Count = 0 then
         Status := AML_Execute.Bad_Name; return;
      end if;
      if Scope > A.Tree.Used then Status := AML_Execute.Unknown_Name; return; end if;
      Located := Resolve_Object (A.Tree, Node_ID (Scope), Path);
      if Located.Status /= Found then Status := AML_Execute.Unknown_Name; return; end if;
      if Located.Node = Root then return; end if;
      if A.Tree.Items (Located.Node).Object_Type not in
        Integer_Object | String_Object | Buffer_Object | Package_Object | Reference_Object | Metadata_Value_Object | Uninitialized_Name_Object
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
      if Value.Value_Kind = AML_Execute.Metadata_Datum then
         Status := AML_Execute.Unsupported_Value; return;
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
           when AML_Objects.Event_Object => Event_Object,
           when AML_Objects.Mutex_Object => Mutex_Object,
           when AML_Objects.Reference_Object => Reference_Object,
           when AML_Objects.Metadata_Object | AML_Objects.Metadata_Snapshot_Object => Metadata_Value_Object);
      end if;
      A.Tree.Items (Located.Node) :=
        (A.Tree.Items (Located.Node) with delta Object_Type => Tag, Object_Ref => ID);
      if Tag = Mutex_Object then
         A.Tree.Items (Located.Node).Mutex_Flags := AML_Objects.Mutex_Flags (A.Tree.Values, ID);
      end if;
      Status := AML_Execute.Returned;
   end Replace_Value;
   procedure Store_Named_Direct
     (A : in out Arena; Node : Node_ID; Width : AML_Decode.Integer_Width;
      Item : AML_Execute.Datum; Status : out AML_Execute.Execution_Status)
   is
      use type AML_Decode.Integer_Width;
      Value : constant AML_Execute.Datum := Item;
      Target, Source : AML_Objects.Object_ID;
      Copied : AML_References.Object_Handle;
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
          Integer_Object | String_Object | Buffer_Object | Package_Object
      then return; end if;
      Target := A.Tree.Items (Node).Object_Ref;
      Target_Kind := AML_Objects.Kind (A.Tree.Values, Target);
      if Target_Kind = AML_Objects.Package_Object then
         -- exstore: Package Store copies before attaching, even for self-store.
         -- Clone_Source commits only a complete copy; references and integer
         -- payloads/origins retain their existing clone semantics.
         if Value.Value_Kind /= AML_Execute.Object_Datum
           or else not Has_Source (A, Value.Object.Source)
           or else Value.Object.ID /= AML_References.Source (Value.Object.Source)
           or else AML_Objects.Kind (A.Tree.Values, Value.Object.ID) /= AML_Objects.Package_Object
         then return; end if;
         Clone_Source (A, Value.Object.Source, Copied, Status);
         if Status /= AML_Execute.Returned then return; end if;
         A.Tree.Items (Node).Object_Ref := AML_References.Source (Copied);
         return;
      end if;
      -- Pinned ACPICA Store-to-simple-target probes reject RefOf and both
      -- Index descriptor sources. Explicit DerefOf supplies a concrete value.
      if Value.Value_Kind in AML_Execute.Reference_Datum | AML_Execute.Metadata_Datum then return; end if;
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
      -- Every target policy writes the existing field; none retags its node.
      if Buffer_Field_Matches (A, R) then
         Write_Buffer_Field (A, R, Width, Item, Status); return;
      end if;
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
            if (Mode = AML_Execute.Direct_Target
                and then A.Tree.Items (Node).Object_Type in Integer_Object | String_Object | Buffer_Object | Package_Object)
              or else (Mode = AML_Execute.Argument_Indirect_Target
                and then A.Tree.Items (Node).Object_Type = Package_Object)
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
               AML_Objects.New_Integer (Candidate, Item.Number, ID, Allocated, Item.Origin);
            when AML_Execute.Metadata_Datum =>
               if not Metadata_Matches (A, Item.Metadata) then return; end if;
               case Item.Metadata.Mode is
                  when AML_Execute.Live_Namespace =>
                     AML_Objects.New_Metadata (Candidate, Item.Metadata.Namespace_Identity, ID, Allocated);
                  when AML_Execute.Captured_Namespace =>
                     AML_Objects.New_Metadata_Snapshot (Candidate,
                       AML_Objects.Metadata_Snapshot_Data (A.Tree.Values,
                         AML_References.Source (Item.Metadata.Snapshot_Source)), ID, Allocated);
               end case;
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
         if Allocated = AML_Objects.Unsupported_Copy then return; end if;
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
      Prior : constant State := A.Tree;
      ID : AML_Objects.Object_ID := 0;
      Allocated : AML_Objects.Allocation_Status;
      Tag : Object_Kind := Integer_Object;
   begin
      Copy := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
      Status := AML_Execute.Unsupported_Value;
      if not Initialized (A) then return; end if;
      case Destination.Kind is
         when AML_Execute.Named_Destination =>
            if Destination.Path.Kind /= AML_Names.Accepted then Status := AML_Execute.Bad_Name; return; end if;
            if Destination.Scope > A.Tree.Used then Status := AML_Execute.Unknown_Name; return; end if;
            Located := Resolve_Object (A.Tree, Node_ID (Destination.Scope), Destination.Path);
            if Located.Status /= Found then Status := AML_Execute.Unknown_Name; return; end if;
            Node := Located.Node;
         when AML_Execute.Referenced_Destination =>
            if not Named_Identity_Matches (A, Destination.Ref) then return; end if;
            Node := (if AML_References.Kind (Destination.Ref) = AML_References.Root_Namespace then Root
              else Node_ID (AML_References.Named_Node (Destination.Ref)));
      end case;
      if Node /= Root and then A.Tree.Items (Node).Object_Type not in
        Integer_Object | String_Object | Buffer_Object | Package_Object | Reference_Object |
        Metadata_Value_Object | Device_Object | Thermal_Zone_Object | Buffer_Field_Object | Uninitialized_Name_Object | Event_Object
      then return; end if;
      Copy_Value (A, Width, Item, Copy, Status);
      if Status = AML_Execute.Returned then
         if Node /= Root and then A.Tree.Items (Node).Object_Type = Buffer_Field_Object then
            Write_Buffer_Field (A, AML_References.Bind_Named (A.Token,
              AML_References.Node_Position (Node), A.Tree.Items (Node).Incarnation), Width, Copy, Status);
         else
            case Copy.Value_Kind is
               when AML_Execute.Integer_Datum =>
                  AML_Objects.New_Integer (A.Tree.Values, Copy.Number, ID, Allocated, Copy.Origin);
                  Tag := Integer_Object;
               when AML_Execute.Reference_Datum =>
                  AML_Objects.New_Reference (A.Tree.Values, Copy.Ref, ID, Allocated);
                  Tag := Reference_Object;
               when AML_Execute.Metadata_Datum =>
                  -- Copy_Value returns an authenticated, independently owned capture.
                  ID := AML_References.Source (Copy.Metadata.Snapshot_Source);
                  Allocated := AML_Objects.Allocated; Tag := Metadata_Value_Object;
               when AML_Execute.Object_Datum =>
                  ID := Copy.Object.ID; Allocated := AML_Objects.Allocated;
                  case AML_Objects.Kind (A.Tree.Values, ID) is
                     when AML_Objects.String_Object => Tag := String_Object;
                     when AML_Objects.Buffer_Object => Tag := Buffer_Object;
                     when AML_Objects.Package_Object => Tag := Package_Object;
                     when AML_Objects.Mutex_Object => return;
                     when AML_Objects.Event_Object =>
                        -- Event replacement is supported for actual named
                        -- objects. Root event payload semantics are separate.
                        if Node = Root then Status := AML_Execute.Unsupported_Value;
                        else Tag := Event_Object; end if;
                     when others => Status := AML_Execute.Unsupported_Value;
                  end case;
            end case;
            if Allocated /= AML_Objects.Allocated then Status := AML_Execute.Value_Limit; end if;
            if Status = AML_Execute.Returned then
               if Node = Root then A.Tree.Root_Value := ID;
               else A.Tree.Items (Node) := (A.Tree.Items (Node) with delta Object_Type => Tag, Object_Ref => ID); end if;
            end if;
         end if;
      end if;
      if Status /= AML_Execute.Returned then
         A.Tree := Prior;
         Copy := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
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
            Located := Resolve_Object (A.Tree, Node_ID (Destination.Scope), Destination.Path);
            if Located.Status /= Found then Status := Unknown_Name; return; end if;
            if Located.Node = Root then return; end if;
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
               when Metadata_Datum =>
                  Describe_Metadata_Value (A, Operand.Value.Metadata, Metadata);
                  if Metadata.Kind /= Metadata_Only then return; end if;
                  case Metadata.Object_Type is
                     when Device_Metadata => Descriptor := Device_Text;
                     when Thermal_Metadata => Descriptor := Thermal_Text;
                     when Processor_Metadata => Descriptor := Processor_Text;
                     when Power_Metadata => Descriptor := Power_Text;
                     when Event_Metadata => Descriptor := Event_Text;
                     when AML_Execute.Mutex_Metadata => Descriptor := Mutex_Text;
                     when Region_Metadata => Descriptor := Region_Text;
                     when others => return;
                  end case;
                  Kind := Builder.String_Input;
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
                     when AML_References.Named_Cell | AML_References.Root_Namespace =>
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
            Located := Resolve_Object (A.Tree, Node_ID (Destination.Scope), Destination.Path);
            if Located.Status /= Found then Status := Unknown_Name; return; end if;
            if Located.Node = Root then return; end if;
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
         when Reference_Datum | Metadata_Datum => return;
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
            Located := Resolve_Object (A.Tree, Node_ID (Destination.Scope), Destination.Path);
            if Located.Status /= Found then Status := Unknown_Name; return; end if;
            if Located.Node = Root then return; end if;
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
         when Reference_Datum | Metadata_Datum => return;
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
            Located := Resolve_Object (A.Tree, Node_ID (Destination.Scope), Destination.Path);
            if Located.Status /= Found then Status := Unknown_Name; return; end if;
            if Located.Node = Root then return; end if;
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
         when Reference_Datum | Metadata_Datum => return;
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
            Located := Resolve_Object (A.Tree, Node_ID (Destination.Scope), Destination.Path);
            if Located.Status /= Found then Status := Unknown_Name; return; end if;
            if Located.Node = Root then return; end if;
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
         when Reference_Datum | Metadata_Datum => return;
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
            Located := Resolve_Object (A.Tree, Node_ID (Destination.Scope), Destination.Path);
            if Located.Status /= Found then Status := Unknown_Name; return; end if;
            if Located.Node = Root then return; end if;
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
      and then (if Token.Owner = Root then
         AML_Frame_Handles.Present (AML_Module_Frames.Top (A.Tree.Module_Frames))
         and then Token.Module_Frame = AML_Module_Frames.Top (A.Tree.Module_Frames)
         and then A.Tree.Items (Node_ID (AML_References.Named_Node (Token.Ref))).Initializer_Frame = Token.Module_Frame
       else Token.Owner <= A.Tree.Used
         and then A.Tree.Items (Token.Owner).Alive
         and then A.Tree.Items (Token.Owner).Object_Type = Method_Object
         and then A.Tree.Items (Token.Owner).Incarnation = Token.Owner_Stamp
         and then A.Tree.Items (Token.Owner).Active_Calls > 0)
      and then A.Tree.Items (Node_ID (AML_References.Named_Node (Token.Ref))).Initializing
      and then A.Tree.Items (Node_ID (AML_References.Named_Node (Token.Ref))).Owner = Token.Owner);
   procedure Reserve_Name_For_Context
     (A : in out Arena; Scope, Lifetime_Owner : Node_ID; Path : AML_Names.Name_Result;
      Token : out Name_Reservation; Status : out AML_Execute.Execution_Status;
      Module_Declaration : Boolean)
   is
      Candidate : State := A.Tree;
      Base, Node : Node_ID;
      Added : Insert_Status;
   begin
      Token := No_Name_Reservation; Status := AML_Execute.Unsupported_Value;
      if not Initialized (A) then return; end if;
      Name_Target (A.Tree, Natural (Scope), Natural (Lifetime_Owner), Path, Base, Status, Module_Declaration);
      if Status /= AML_Execute.Returned then return; end if;
      Insert (Candidate, Base, Path.Parts (Path.Count), Node, Added);
      case Added is
         when Duplicate => Status := AML_Execute.Duplicate_Name; return;
         when Full => Status := AML_Execute.Namespace_Limit; return;
         when Invalid_Name => Status := AML_Execute.Bad_Name; return;
         when Inserted => null;
      end case;
      Candidate.Items (Node) := (Candidate.Items (Node) with delta
        Owner => Lifetime_Owner, Object_Type => Uninitialized_Name_Object, Initializing => True,
        Initializer_Frame => (if Module_Declaration then AML_Module_Frames.Top (A.Tree.Module_Frames) else AML_Frame_Handles.No_Frame));
      A.Tree := Candidate;
      Token := (Ref => AML_References.Bind_Named (A.Token,
          AML_References.Node_Position (Node), A.Tree.Items (Node).Incarnation),
        Owner => Lifetime_Owner, Owner_Stamp => (if Lifetime_Owner = Root then 0 else A.Tree.Items (Lifetime_Owner).Incarnation),
        Module_Frame => (if Module_Declaration then AML_Module_Frames.Top (A.Tree.Module_Frames) else AML_Frame_Handles.No_Frame));
      Status := AML_Execute.Returned;
   end Reserve_Name_For_Context;
   procedure Reserve_Name
     (A : in out Arena; Scope : Node_ID; Path : AML_Names.Name_Result;
      Token : out Name_Reservation; Status : out AML_Execute.Execution_Status) is
   begin
      Reserve_Name_For_Context (A, Scope, Scope, Path, Token, Status, False);
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
            when AML_Objects.Reference_Object | AML_Objects.Metadata_Object | AML_Objects.Metadata_Snapshot_Object | AML_Objects.Event_Object | AML_Objects.Mutex_Object => return;
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
      pragma Unreferenced (Width); -- Count has already been evaluated at this width.
      use type AML_Decode.Status;
      Layout : AML_Buffer_Layouts.Layout_Result;
      Node : Node_ID;
   begin
      Status := AML_Execute.Unsupported_Value;
      if not Reservation_Matches (A, Token) or else Count.Kind /= AML_Decode.Accepted then return; end if;
      Layout := AML_Buffer_Layouts.With_Count (Initializer, Count.Value, Count.Consumed);
      if Layout.Kind /= AML_Decode.Accepted or else Layout.Consumed /= Initializer'Length then return; end if;
      Node := Node_ID (AML_References.Named_Node (Token.Ref));
      if A.Tree.Items (Node).Object_Type /= Uninitialized_Name_Object then
         Complete_Name (A, Token, AML_References.No_Object_Handle, Status); return;
      end if;
      declare
         Candidate : AML_Objects.State := A.Tree.Values;
         ID : AML_Objects.Object_ID;
         Allocated : AML_Objects.Allocation_Status;
      begin
         if Layout.Initializer_Length = 0 then
            AML_Objects.New_Buffer (Candidate, Layout.Effective_Length,
              AML_Decode.Bytes'(1 .. 0 => 0), ID, Allocated);
         else
            AML_Objects.New_Buffer (Candidate, Layout.Effective_Length,
              Initializer (Initializer'First + Layout.Initializer_Offset ..
                           Initializer'First + (Layout.Consumed - 1)), ID, Allocated);
         end if;
         if Allocated /= AML_Objects.Allocated then Status := AML_Execute.Value_Limit; return; end if;
         -- One publication from the post-count arena; no pre-count state is restored.
         A.Tree.Values := Candidate;
         A.Tree.Items (Node) := (A.Tree.Items (Node) with delta
           Object_Type => Buffer_Object, Object_Ref => ID, Initializing => False);
         Status := AML_Execute.Returned;
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
      and then (if AML_References.Kind (R) = AML_References.Root_Namespace then
         AML_References.Well_Formed (R)
       else AML_References.Kind (R) = AML_References.Named_Cell
         and then AML_References.Named_Node (R) > 0
         and then AML_References.Named_Node (R) <= AML_References.Node_Position (A.Tree.Used)
         and then A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Alive
         and then A.Tree.Items (Node_ID (AML_References.Named_Node (R))).Incarnation = AML_References.Incarnation (R)));
   -- This effect is deliberately outside Tree/value transactions. Later AML
   -- failures do not retract an already admitted notification.
   procedure Queue_Notify (A : in out Arena; Target : Reference;
      Value : AML_Decode.Integer_Value; Status : out AML_Execute.Execution_Status)
   is
      Node, Cursor : Node_ID;
      Item : Notification_Observation;
      Reverse_Path : Notification_Path := [others => "____"];
   begin
      Status := AML_Execute.Unsupported_Value;
      if not Named_Identity_Matches (A, Target) then return; end if;
      Node := (if AML_References.Kind (Target) = AML_References.Root_Namespace then Root
               else Node_ID (AML_References.Named_Node (Target)));
      declare Description : AML_Execute.Reference_Metadata; begin
         Describe_Named_Identity (A, Target, Description);
         if Description.Kind /= AML_Execute.Metadata_Only then return; end if;
         case Description.Object_Type is
            when AML_Execute.Device_Metadata => Item.Target_Kind := Device_Notification;
            when AML_Execute.Processor_Metadata => Item.Target_Kind := Processor_Notification;
            when AML_Execute.Thermal_Metadata => Item.Target_Kind := Thermal_Notification;
            when others => return;
         end case;
      end;
      Status := AML_Execute.Returned;
      if not A.Notifications.Enabled then return; end if;
      if A.Notifications.Used = Max_Pending_Notifications then
         Status := AML_Execute.Notify_Limit; return;
      end if;
      Cursor := Node;
      while Cursor /= Root loop
         -- Parent is strictly lower, bounding a complete root-relative path.
         Item.Path_Length := Item.Path_Length + 1;
         Reverse_Path (Item.Path_Length) := A.Tree.Items (Cursor).Part;
         Cursor := A.Tree.Items (Cursor).Up;
      end loop;
      for I in 1 .. Item.Path_Length loop
         Item.Path (I) := Reverse_Path (Item.Path_Length - I + 1);
      end loop;
      Item.Node := Node;
      Item.Incarnation := (if Node = Root then 0 else AML_References.Incarnation (Target));
      -- ACPICA classifies UINT32, but delivers UINT16. The class cannot
      -- be recovered from the narrowed delivered number.
      Item.Class := (if Value mod Notification_Class_Modulus <= System_Last_Value
                     then System_Notify else Device_Notify);
      Item.Number := Value mod Notification_Value_Modulus;
      A.Notifications.Used := A.Notifications.Used + 1;
      A.Notifications.Entries (A.Notifications.Used) :=
        (Target => Target, Observation => Item);
   end Queue_Notify;
   procedure Make_Named_Identity
     (A : Arena; Node : Natural; R : out Reference; Success : out Boolean)
   is
      Canonical : Lookup_Result;
   begin
      R := AML_References.No_Reference; Success := False;
      if not Initialized (A) or else Node > A.Tree.Used then return; end if;
      Canonical := Canonical_Node (A.Tree, Node_ID (Node));
      if Canonical.Status /= Found then return; end if;
      if Canonical.Node = Root then R := AML_References.Bind_Root (A.Token);
      else
         R := AML_References.Bind_Named (A.Token, AML_References.Node_Position (Canonical.Node),
           A.Tree.Items (Canonical.Node).Incarnation);
      end if;
      Success := True;
   end Make_Named_Identity;
   procedure Describe_Named_Identity
     (A : Arena; Ref : Reference; Result : out AML_Execute.Reference_Metadata)
   is
      use AML_Execute;
   begin
      Result := (Kind => Continue_Reference);
      if AML_References.Kind (Ref) = AML_References.Root_Namespace then
         Result := (Kind => Invalid_Reference);
         if Named_Identity_Matches (A, Ref) then
            if A.Tree.Root_Value = 0 then Result := (Metadata_Only, Device_Metadata);
            elsif AML_Objects.Kind (A.Tree.Values, A.Tree.Root_Value) = AML_Objects.Metadata_Snapshot_Object then
               Result := (Metadata_Only, AML_Objects.Metadata_Snapshot_Data
                 (A.Tree.Values, A.Tree.Root_Value).Object_Type);
            else Result := (Kind => Continue_Reference); end if;
         end if;
         return;
      end if;
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
         when Buffer_Field_Object => Result := (Metadata_Only, Buffer_Field_Metadata);
         when Table_Field_Object | Region_Field_Object | Uninitialized_Table_Field_Object => Result := (Metadata_Only, Field_Metadata);
         when Table_Region_Object | Uninitialized_Region_Object | Operation_Region_Object => Result := (Metadata_Only, Region_Metadata);
         when Metadata_Value_Object =>
            Result := (Metadata_Only, AML_Objects.Metadata_Snapshot_Data (A.Tree.Values,
              A.Tree.Items (Node_ID (AML_References.Named_Node (Ref))).Object_Ref).Object_Type);
         when Integer_Object | String_Object | Buffer_Object | Package_Object | Reference_Object | Uninitialized_Name_Object =>
            Result := (Kind => Continue_Reference);
         when Scope_Object => Result := (Metadata_Only, Scope_Metadata);
         when Alias_Object => null;
      end case;
   end Describe_Named_Identity;
   procedure Make_Named_Reference
     (A : Arena; Node : Natural; R : out Reference; Success : out Boolean)
   is
      Canonical : Lookup_Result;
   begin
      R := AML_References.No_Reference; Success := False;
      if not Initialized (A) or else Node > A.Tree.Used then return; end if;
      Canonical := Canonical_Node (A.Tree, Node_ID (Node));
      if Canonical.Status /= Found or else Canonical.Node = Root then return; end if;
      if A.Tree.Items (Canonical.Node).Object_Type not in
        Integer_Object | String_Object | Buffer_Object | Package_Object | Reference_Object | Metadata_Value_Object | Uninitialized_Name_Object | Buffer_Field_Object | Region_Field_Object then return; end if;
      R := AML_References.Bind_Named (A.Token, AML_References.Node_Position (Canonical.Node),
        A.Tree.Items (Canonical.Node).Incarnation);
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
         when AML_Objects.Integer_Object | AML_Objects.Reference_Object | AML_Objects.Metadata_Object | AML_Objects.Metadata_Snapshot_Object | AML_Objects.Event_Object | AML_Objects.Mutex_Object => null;
      end case;
   end Make_Index;
   type Allocation_Hook_Status is (Allocation_Allowed, Invalid_Census, Reclamation_Failed);
   procedure Skip_Collection (E : in out Arena; Extra : Root_Values;
      Status : out Allocation_Hook_Status) is
      pragma Unreferenced (E, Extra);
   begin Status := Allocation_Allowed; end Skip_Collection;
   generic
      with procedure Before_Allocation (E : in out Arena; Extra : Root_Values;
         Status : out Allocation_Hook_Status);
      with procedure Read_Table_Binding
        (Tree : in out State; Input : aliased AML_Table_Backing.State; Scope : Natural;
         Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width;
         Purpose : AML_Execute.Binding_Purpose; Binding : out AML_Execute.Binding_Result)
        is Lookup_With_Tables;
   package Named_Readers is
      procedure Stamp (E : Arena; Binding : in out AML_Execute.Binding_Result);
      procedure Read_Field_At_Entry
        (E : in out Arena; Ref : Reference; Width : AML_Decode.Integer_Width;
         Value : out AML_Execute.Datum; Status : out AML_Execute.Execution_Status);
      procedure Lookup (E : in out Arena; Backing : aliased AML_Table_Backing.State;
        Scope : Natural; Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width;
        Purpose : AML_Execute.Binding_Purpose; Binding : out AML_Execute.Binding_Result)
        with Pre => Valid_Context (E.Tree) and then not Binding'Constrained, Post => Valid_Context (E.Tree);
   end Named_Readers;
   package body Named_Readers is
      use type AML_Execute.Binding_Status;
      procedure Stamp (E : Arena; Binding : in out AML_Execute.Binding_Result) is
      begin
         if Binding.Status = AML_Execute.Non_Integer_Binding
           and then AML_Objects.Is_Live (E.Tree.Values, Binding.Object.ID)
         then Binding.Object.Source := AML_References.Bind_Object (E.Token, AML_Objects.Address_Of (E.Tree.Values, Binding.Object.ID)); end if;
      end Stamp;
      procedure Read_Field_At_Entry
        (E : in out Arena; Ref : Reference; Width : AML_Decode.Integer_Width;
         Value : out AML_Execute.Datum; Status : out AML_Execute.Execution_Status)
      is
         Gate : Allocation_Hook_Status;
         Info : Buffer_Field_Descriptor;
      begin
         Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Status := AML_Execute.Unsupported_Value;
         if not Buffer_Field_Matches (E, Ref) then return; end if;
         Info := E.Tree.Items (Node_ID (AML_References.Named_Node (Ref))).Buffer_Field_Info;
         if Info.Always_Buffer or else Info.Bits > AML_Decode.Integer_Value (Field_Integer_Bits (Width)) then
            -- Root the authenticated namespace edge before reclaiming values.
            Before_Allocation (E, [(AML_Execute.Reference_Datum, Ref)], Gate);
            if Gate /= Allocation_Allowed then return; end if;
         end if;
         Read_Buffer_Field (E, Ref, Width, Value, Status);
      end Read_Field_At_Entry;
      procedure Lookup (E : in out Arena; Backing : aliased AML_Table_Backing.State;
        Scope : Natural; Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width;
        Purpose : AML_Execute.Binding_Purpose; Binding : out AML_Execute.Binding_Result)
      is
         Located : Lookup_Result;
         Ref : Reference;
         OK : Boolean;
         Gate : Allocation_Hook_Status;
         Value : AML_Execute.Datum;
         Read_Status : AML_Execute.Execution_Status;
         use type AML_Decode.Integer_Width;
      begin
         if Purpose in AML_Execute.Reference_Target | AML_Execute.Namespace_Identity then
            Binding := (Status => AML_Execute.Missing_Binding);
            if Scope > E.Tree.Used then return; end if;
            Located := Resolve_Object (E.Tree, Node_ID (Scope), Path);
            if Located.Status /= Found then return; end if;
            if Purpose = AML_Execute.Namespace_Identity then
               Make_Named_Identity (E, Natural (Located.Node), Ref, OK);
            else Make_Named_Reference (E, Natural (Located.Node), Ref, OK); end if;
            if OK then Binding := (Status => AML_Execute.Reference_Binding, Ref => Ref);
            else Binding := (Status => AML_Execute.Failed_Binding, Failure => AML_Execute.Unsupported_Value); end if;
            return;
         end if;
         if Purpose = AML_Execute.Evaluate_Binding and then Scope <= E.Tree.Used then
            Located := Resolve_Object (E.Tree, Node_ID (Scope), Path);
            if Located.Status = Found and then Located.Node /= Root
              and then (Kind (E.Tree, Located.Node) = Event_Object or else
                (Kind (E.Tree, Located.Node) = Mutex_Object and then
                 E.Tree.Items (Located.Node).Object_Ref /= AML_Objects.No_Object)) then
               Read_Source (E, AML_References.Bind_Object (E.Token,
                 AML_Objects.Address_Of (E.Tree.Values, E.Tree.Items (Located.Node).Object_Ref)), Value, Read_Status);
               if Read_Status = AML_Execute.Returned and then Value.Value_Kind = AML_Execute.Object_Datum then
                  Binding := (AML_Execute.Non_Integer_Binding, Value.Object);
               else Binding := (AML_Execute.Failed_Binding, AML_Execute.Unsupported_Value); end if;
               return;
            end if;
         end if;
         if Purpose in AML_Execute.Evaluate_Binding | AML_Execute.Inspect_Binding and then Scope <= E.Tree.Used then
            Located := Resolve_Object (E.Tree, Node_ID (Scope), Path);
            if Located.Status = Found and then
              (Located.Node = Root or else Kind (E.Tree, Located.Node) = Metadata_Value_Object
               or else Namespace_Metadata_Kind (Kind (E.Tree, Located.Node)))
            then
               Make_Named_Identity (E, Natural (Located.Node), Ref, OK);
               if not OK then Binding := (AML_Execute.Failed_Binding, AML_Execute.Unsupported_Value); return; end if;
               Resolve_Semantic_Reference (E, Ref, Value, Read_Status);
               if Read_Status /= AML_Execute.Returned then
                  Binding := (AML_Execute.Failed_Binding, Read_Status);
               else
                  case Value.Value_Kind is
                     when AML_Execute.Metadata_Datum => Binding := (AML_Execute.Metadata_Binding, Value.Metadata);
                     when AML_Execute.Integer_Datum => Binding := (AML_Execute.Integer_Binding, Value.Number, Value.Origin);
                     when AML_Execute.Object_Datum => Binding := (AML_Execute.Non_Integer_Binding, Value.Object);
                     when AML_Execute.Reference_Datum => Binding := (AML_Execute.Reference_Binding, Value.Ref);
                  end case;
               end if;
               return;
            end if;
            if Purpose = AML_Execute.Evaluate_Binding and then Located.Status = Found and then Located.Node /= Root
              and then Present (E.Tree, Located.Node)
              and then Kind (E.Tree, Located.Node) = Buffer_Field_Object
            then
               Make_Named_Reference (E, Natural (Located.Node), Ref, OK);
               if not OK then Binding := (AML_Execute.Failed_Binding, AML_Execute.Unsupported_Value); return; end if;
               Read_Field_At_Entry (E, Ref, Width, Value, Read_Status);
               if Read_Status /= AML_Execute.Returned then
                  Binding := (AML_Execute.Failed_Binding, Read_Status);
               elsif Value.Value_Kind = AML_Execute.Integer_Datum then
                  Binding := (AML_Execute.Integer_Binding, Value.Number, Value.Origin);
               else
                  Binding := (AML_Execute.Non_Integer_Binding, Value.Object);
               end if;
               return;
            end if;
            if Purpose = AML_Execute.Evaluate_Binding and then Located.Status = Found and then Located.Node /= Root
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
         Read_Table_Binding (E.Tree, Backing, Scope, Path, Width, Purpose, Binding); Stamp (E, Binding);
      end Lookup;
   end Named_Readers;
   type Null_Field_Context is null record;
   procedure Reject_Field_Operation
     (E : in out Arena; IO : in out Null_Field_Context; Ref : Reference;
      Width : AML_Decode.Integer_Width; For_Write : Boolean;
      Value : in out AML_Execute.Datum; Status : out AML_Execute.Execution_Status) is
      pragma Unreferenced (E, IO, Ref, Width, For_Write, Value);
   begin Status := AML_Execute.Unsupported_Value; end Reject_Field_Operation;

   generic
      type IO_Context is limited private;
      with procedure Field_Operation
        (E : in out Arena; IO : in out IO_Context; Ref : Reference;
         Width : AML_Decode.Integer_Width; For_Write : Boolean;
         Value : in out AML_Execute.Datum; Status : out AML_Execute.Execution_Status);
      with procedure Handoff (E : in out Arena; Result : AML_Execute.Execution_Result);
      with procedure Before_Allocation (E : in out Arena; Extra : Root_Values;
         Status : out Allocation_Hook_Status) is Skip_Collection;
      Nested_Loads_Enabled : Boolean := False;
      Restricted_Table_Loads_Enabled : Boolean := False;
      Module_Recovery_Enabled : Boolean := False;
      with procedure Prepare_Load
        (E : in out Arena; Input : aliased AML_Table_Backing.State;
         Lease : AML_Declaration_Contexts.Lease; Scope : Natural;
         Width : AML_Decode.Integer_Width; Operands : AML_Execute.Load_Table_Operands;
         Budget : Natural; Charged : in out Natural; Succeeded : in out Boolean;
         Status : in out AML_Execute.Execution_Status; Plan : in out AML_Execute.Load_Table_Plan) is null;
      with procedure Prepare_Source
        (E : in out Arena; Input : aliased AML_Table_Backing.State;
         Lease : AML_Declaration_Contexts.Lease; Scope : Natural;
         Width : AML_Decode.Integer_Width; Source : AML_Execute.Datum;
         Budget : Natural; Charged : in out Natural; Succeeded : in out Boolean;
         Status : in out AML_Execute.Execution_Status; Plan : in out AML_Execute.Load_Table_Plan) is null;
      with procedure Copy_Load
        (E : in out Arena; Input : aliased AML_Table_Backing.State;
         Plan : AML_Execute.Load_Table_Plan; Data : in out AML_Decode.Bytes;
         Status : in out AML_Execute.Execution_Status) is null;
      with procedure Begin_Load
        (E : in out Arena; Input : aliased AML_Table_Backing.State;
         Plan : AML_Execute.Load_Table_Plan; Status : in out AML_Execute.Execution_Status) is null;
      with procedure Finish_Load
        (E : in out Arena; Input : aliased AML_Table_Backing.State;
         Lease : AML_Declaration_Contexts.Lease; Plan : AML_Execute.Load_Table_Plan;
         Operands : AML_Execute.Load_Table_Operands; Module_Result : AML_Execute.Execution_Result;
         Budget : Natural; Charged : in out Natural; Succeeded : in out Boolean;
         Status : in out AML_Execute.Execution_Status) is null;
      with procedure Read_Table_Binding
        (Tree : in out State; Input : aliased AML_Table_Backing.State; Scope : Natural;
         Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width;
         Purpose : AML_Execute.Binding_Purpose; Binding : out AML_Execute.Binding_Result)
        is Lookup_With_Tables;
      with procedure Complete_Table_Binding
        (Tree : in out State; Input : aliased AML_Table_Backing.State; Token : Natural;
         Width : AML_Decode.Integer_Width; Signature, OEM, Table_ID : AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status) is Complete_Region;
   procedure Run_Context_Session (A : in out Arena; IO : in out IO_Context; Input : aliased AML_Table_Backing.State;
     Code : AML_Decode.Bytes; Source : aliased AML_Byte_Sources.Storage; Source_First : Natural; Width : AML_Decode.Integer_Width; Scope : Node_ID;
     Args : AML_Execute.Value_Arguments; Argument_Count : Natural;
     Budget : Natural; Current_Sync : AML_Execute.Sync_Level;
     Result : out AML_Execute.Execution_Result;
     Session_Request : AML_Declaration_Contexts.Request := AML_Declaration_Contexts.Method_Request;
     Implementation : AML_Execute.Method_Implementation := AML_Execute.AML_Implementation);
   procedure Run_Context_Session (A : in out Arena; IO : in out IO_Context; Input : aliased AML_Table_Backing.State;
     Code : AML_Decode.Bytes; Source : aliased AML_Byte_Sources.Storage; Source_First : Natural; Width : AML_Decode.Integer_Width; Scope : Node_ID;
     Args : AML_Execute.Value_Arguments; Argument_Count : Natural;
     Budget : Natural; Current_Sync : AML_Execute.Sync_Level;
     Result : out AML_Execute.Execution_Result;
     Session_Request : AML_Declaration_Contexts.Request := AML_Declaration_Contexts.Method_Request;
     Implementation : AML_Execute.Method_Implementation := AML_Execute.AML_Implementation) is
      use type AML_Execute.Binding_Status;
      use type AML_References.Object_Handle;
      Entry_Token : constant AML_Identity.Identity := A.Token;
      Owner_Workspace : Semantic_Owner_Workspace;
      function Census_Status (Result : Persistent_Owner_Result)
        return AML_Execute.Owner_Census_Status is
        (case Result is
           when Persistent_Scan_Collecting => AML_Execute.Census_Collecting,
           when Persistent_Owner_Found => AML_Execute.Census_Shared,
           when No_Persistent_Owner_Found => AML_Execute.Census_No_Owner,
           when Persistent_Census_Incomplete => AML_Execute.Census_Incomplete,
           when others => AML_Execute.Census_Invalid);
      procedure Owner_Census_Begin
        (Environment : Arena; Source : AML_References.Object_Handle;
         Status : out AML_Execute.Owner_Census_Status)
      is
         Result : Persistent_Owner_Result;
      begin
         Begin_Integer_Owner_Scan (Environment, Source, Owner_Workspace, Result);
         Status := Census_Status (Result);
      end Owner_Census_Begin;
      procedure Owner_Census_Add
        (Environment : Arena; Value : AML_Execute.Datum;
         Status : out AML_Execute.Owner_Census_Status)
      is
         Result : Persistent_Owner_Result;
      begin
         Add_Transient_Owner (Environment, Value, Owner_Workspace, Result);
         Status := Census_Status (Result);
      end Owner_Census_Add;
      procedure Owner_Census_Finish
        (Environment : Arena; Status : out AML_Execute.Owner_Census_Status)
      is
         Result : Persistent_Owner_Result;
      begin
         Finish_Integer_Owner_Scan (Environment, Owner_Workspace, Result);
         Status := Census_Status (Result);
      end Owner_Census_Finish;

      function Context_Valid (E : Arena) return Boolean is
        (Valid_Context (E.Tree) and then
          (E.Token = AML_Identity.No_Identity or else AML_Identity.Issuer.Is_Issued (E.Token))
          and then E.Token = Entry_Token
          and then (if AML_Frame_Handles.Present (AML_Module_Frames.Top (E.Tree.Module_Frames)) then
            (Nested_Loads_Enabled or else AML_Declaration_Contexts.Is_Module (Session_Request))
            and then AML_Module_Frames.Belongs_To (E.Tree.Module_Frames,
              AML_Frame_Handles.Bind_Domain (E.Token, E.Invocation_Issued))));
      package Readers is new Named_Readers (Before_Allocation, Read_Table_Binding);
      procedure Stamp (E : Arena; Binding : in out AML_Execute.Binding_Result)
        renames Readers.Stamp;
      function Hardware_Field (E : Arena; Ref : Reference) return Boolean is
        (AML_References.Kind (Ref) = AML_References.Named_Cell
         and then Named_Identity_Matches (E, Ref)
         and then E.Tree.Items (Node_ID (AML_References.Named_Node (Ref))).Object_Type = Region_Field_Object);
      procedure Read_Field_At_Entry
        (E : in out Arena; Ref : Reference; Width : AML_Decode.Integer_Width;
         Value : out AML_Execute.Datum; Status : out AML_Execute.Execution_Status) is
         Gate : Allocation_Hook_Status;
      begin
         if Hardware_Field (E, Ref) then
            Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
            if AML_Decode.Integer_Value
              (E.Tree.Items (Node_ID (AML_References.Named_Node (Ref))).Region_Field_Info.Bits)
              > AML_Decode.Integer_Value (Field_Integer_Bits (Width))
            then
               Before_Allocation (E, [], Gate);
               if Gate /= Allocation_Allowed then Status := AML_Execute.Unsupported_Value; return; end if;
            end if;
            Field_Operation (E, IO, Ref, Width, False, Value, Status);
         else Readers.Read_Field_At_Entry (E, Ref, Width, Value, Status); end if;
      end Read_Field_At_Entry;
      procedure Lookup (E : in out Arena; Backing : aliased AML_Table_Backing.State;
        Scope : Natural; Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width;
        Purpose : AML_Execute.Binding_Purpose; Binding : out AML_Execute.Binding_Result)
        with Pre => Context_Valid (E) and then not Binding'Constrained, Post => Context_Valid (E)
      is
         Located : Lookup_Result;
         Ref : Reference;
         OK : Boolean;
         Value : AML_Execute.Datum;
         Read_Status : AML_Execute.Execution_Status;
      begin
         if Purpose = AML_Execute.Evaluate_Binding and then Scope <= E.Tree.Used then
            Located := Resolve_Object (E.Tree, Node_ID (Scope), Path);
            if Located.Status = Found and then Located.Node /= Root
              and then E.Tree.Items (Located.Node).Object_Type = Region_Field_Object then
               Make_Named_Identity (E, Natural (Located.Node), Ref, OK);
               if not OK then Binding := (AML_Execute.Failed_Binding, AML_Execute.Unsupported_Value); return; end if;
               Read_Field_At_Entry (E, Ref, Width, Value, Read_Status);
               if Read_Status = AML_Execute.Returned then
                  case Value.Value_Kind is
                     when AML_Execute.Integer_Datum =>
                        Binding := (AML_Execute.Integer_Binding, Value.Number, Value.Origin);
                     when AML_Execute.Object_Datum =>
                        Binding := (AML_Execute.Non_Integer_Binding, Value.Object);
                     when others =>
                        Binding := (AML_Execute.Failed_Binding, AML_Execute.Unsupported_Value);
                  end case;
               else Binding := (AML_Execute.Failed_Binding, Read_Status); end if;
               return;
            end if;
         end if;
         Readers.Lookup (E, Backing, Scope, Path, Width, Purpose, Binding);
      end Lookup;
      function Method (E : Arena; ID : Natural) return AML_Execute.Method_Definition is
        (Read_Method (E.Tree, ID));
      procedure Buffer_Literal
        (E : in out Arena; Scope : Natural; Length : AML_Buffer_Layouts.Buffer_Length;
         Initializer : AML_Decode.Bytes; Binding : out AML_Execute.Binding_Result)
        with Pre => Context_Valid (E) and then not Binding'Constrained,
             Post => Context_Valid (E)
      is
         Gate : Allocation_Hook_Status;
      begin
         Before_Allocation (E, [], Gate);
         if Gate /= Allocation_Allowed then
            Binding := (AML_Execute.Failed_Binding, AML_Execute.Unsupported_Value); return;
         end if;
         Materialize_Buffer_Literal (E.Tree, Scope, Length, Initializer, Binding);
         Stamp (E, Binding);
      end Buffer_Literal;
      procedure Literal (E : in out Arena; Scope : Natural; Kind : AML_Execute.Literal_Kind; Width : AML_Decode.Integer_Width; Data : AML_Decode.Bytes;
         Source : aliased AML_Byte_Sources.Storage; Source_First : Natural;
        Binding : out AML_Execute.Binding_Result)
        with Pre => Context_Valid (E) and then not Binding'Constrained, Post => Context_Valid (E)
      is
         Gate : Allocation_Hook_Status;
      begin
         if not AML_Byte_Sources.Corresponds (Source, Source_First, Data) then
            Binding := (AML_Execute.Failed_Binding, AML_Execute.Unsupported_Value); return;
         end if;
         Before_Allocation (E, [], Gate);
         if Gate /= Allocation_Allowed then
            Binding := (AML_Execute.Failed_Binding, AML_Execute.Unsupported_Value); return;
         end if;
         Materialize_Literal_With_Owner (E.Tree, E.Token, Scope, Kind, Width, Data, Source, Source_First, Binding); Stamp (E, Binding);
      end Literal;
      procedure Dereference_With_Provenance (E : in out Arena; Ref : Reference;
        Width : AML_Decode.Integer_Width;
        Value : out AML_Execute.Datum; Provenance : out AML_Execute.Scalar_Provenance;
        Status : out AML_Execute.Execution_Status)
        with Pre => Context_Valid (E) and then not Value'Constrained, Post => Context_Valid (E)
      is
         Canonical : Reference := Ref;
      begin
         Provenance := AML_Execute.Detached_Provenance;
         if Name_Member_Matches (E, Ref) then
            Canonical := AML_References.Bind_Named (E.Token, AML_References.Named_Node (Ref),
              AML_References.Incarnation (Ref));
         end if;
         if Hardware_Field (E, Canonical) or else Buffer_Field_Matches (E, Canonical) then
            Read_Field_At_Entry (E, Canonical, Width, Value, Status);
         elsif AML_References.Kind (Ref) = AML_References.Name_Member then
            Resolve_Name_Member (E, Ref, Value, Provenance, Status);
         else Resolve_Semantic_Reference (E, Ref, Value, Provenance, Status); end if;
      end Dereference_With_Provenance;
      procedure Define_Field_At_Entry
        (E : in out Arena; Scope, Lifetime_Owner : Natural; Path : AML_Names.Name_Result;
         Width : AML_Decode.Integer_Width; Source : AML_Execute.Datum;
         Bit_Offset, Bit_Length : AML_Decode.Integer_Value; Always_Buffer : Boolean;
         Status : out AML_Execute.Execution_Status; Module_Declaration : Boolean)
      is
         Gate : Allocation_Hook_Status;
      begin
         -- Existing canonical Buffer capture allocates no value objects.
         if Source.Value_Kind = AML_Execute.Object_Datum
           and then Has_Source (E, Source.Object.Source)
           and then Source.Object.ID = AML_References.Source (Source.Object.Source)
           and then AML_Objects.Kind (E.Tree.Values, Source.Object.ID) = AML_Objects.Buffer_Object
         then
            Define_Buffer_Field_Value_For_Context (E, Scope, Lifetime_Owner, Path, Width, Source,
              Bit_Offset, Bit_Length, Always_Buffer, Status, Module_Declaration);
            return;
         end if;
         Before_Allocation (E, [Source], Gate);
         if Gate /= Allocation_Allowed then Status := AML_Execute.Unsupported_Value; return; end if;
         Define_Buffer_Field_Value_For_Context (E, Scope, Lifetime_Owner, Path, Width, Source,
           Bit_Offset, Bit_Length, Always_Buffer, Status, Module_Declaration);
      end Define_Field_At_Entry;
      procedure Write (E : in out Arena; Scope : Natural; Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width; Item : AML_Execute.Datum; Status : out AML_Execute.Write_Status)
        with Pre => Context_Valid (E), Post => Context_Valid (E)
      is
         Located : Lookup_Result;
         Stored : AML_Execute.Execution_Status;
         Gate : Allocation_Hook_Status;
         Field_Value : AML_Execute.Datum := Item;
         Ref : Reference;
         OK : Boolean;
      begin
         if Scope <= E.Tree.Used then
            Located := Resolve_Object (E.Tree, Node_ID (Scope), Path);
            if Located.Status = Found and then Located.Node /= Root
              and then E.Tree.Items (Located.Node).Object_Type = Region_Field_Object then
               Make_Named_Identity (E, Natural (Located.Node), Ref, OK);
               if not OK then Status := AML_Execute.Write_Unsupported; return; end if;
               Field_Operation (E, IO, Ref, Width, True, Field_Value, Stored);
               Status := (case Stored is
                  when AML_Execute.Returned => AML_Execute.Written,
                  when AML_Execute.Value_Limit => AML_Execute.Write_Value_Limit,
                  when AML_Execute.Field_Indeterminate => AML_Execute.Write_Field_Indeterminate,
                  when AML_Execute.Empty_Buffer => AML_Execute.Write_Empty_Buffer,
                  when others => AML_Execute.Write_Unsupported);
               return;
            end if;
         end if;
         Before_Allocation (E, [Item], Gate);
         if Gate /= Allocation_Allowed then Status := AML_Execute.Write_Unsupported; return; end if;
         if Item.Value_Kind = AML_Execute.Integer_Datum
           and then Scope <= E.Tree.Used
           and then Path.Kind = AML_Names.Accepted and then Path.Count > 0
         then
            Located := Resolve_Object (E.Tree, Node_ID (Scope), Path);
            if Located.Status = Found and then Located.Node /= Root
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
         Located := Resolve_Object (E.Tree, Node_ID (Scope), Path);
         if Located.Status /= Found then Status := AML_Execute.Write_Missing; return; end if;
         if Located.Node = Root then Status := AML_Execute.Write_Unsupported; return; end if;
         if E.Tree.Items (Located.Node).Object_Type = Buffer_Field_Object then
            Write_Buffer_Field (E, AML_References.Bind_Named (E.Token,
              AML_References.Node_Position (Located.Node), E.Tree.Items (Located.Node).Incarnation),
              Width, Item, Stored);
         else Store_Named_Direct (E, Located.Node, Width, Item, Stored); end if;
         Status := (case Stored is
           when AML_Execute.Returned => AML_Execute.Written,
           when AML_Execute.Buffer_Field_Limit => AML_Execute.Write_Buffer_Field_Limit,
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



      procedure Complete (E : in out Arena; Backing : aliased AML_Table_Backing.State; Token : Natural; Width : AML_Decode.Integer_Width; Signature, OEM, Table_ID : AML_Execute.Datum; Status : out AML_Execute.Execution_Status)
        with Pre => Context_Valid (E), Post => Context_Valid (E)
      is
      begin Complete_Table_Binding (E.Tree, Backing, Token, Width, Signature, OEM, Table_ID, Status); end Complete;
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
         Field_Value : AML_Execute.Datum := Item;
         Canonical : Reference := Ref;
      begin
         if Name_Member_Matches (E, Ref) then
            Canonical := AML_References.Bind_Named (E.Token, AML_References.Named_Node (Ref),
              AML_References.Incarnation (Ref));
         end if;
         if Hardware_Field (E, Canonical) then
            Field_Operation (E, IO, Canonical, Width, True, Field_Value, Status); return;
         end if;
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
        (E : in out Arena; Scope, Lifetime_Owner : Natural; Path : AML_Names.Name_Result;
         Width : AML_Decode.Integer_Width; Data : AML_Decode.Bytes;
         Source : aliased AML_Byte_Sources.Storage; Source_First : Natural;
         Consumed : out Natural; Status : out AML_Execute.Execution_Status; Module_Declaration : Boolean := False)
        with Pre => Context_Valid (E), Post => Context_Valid (E)
      is
         Gate : Allocation_Hook_Status;
      begin
         if not AML_Byte_Sources.Corresponds (Source, Source_First, Data) then
            Consumed := 0; Status := AML_Execute.Unsupported_Value; return;
         end if;
         Before_Allocation (E, [], Gate);
         if Gate /= Allocation_Allowed then Consumed := 0; Status := AML_Execute.Unsupported_Value; return; end if;
         Define_Runtime_Name_For_Context (E.Tree, E.Token, Scope, Lifetime_Owner,
           Path, Width, Data, Source, Source_First, Consumed, Status, Module_Declaration);
      end Define_Name;

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
         Outcome : Mutex_Registry.Outcome;
         use type Mutex_Registry.Outcome;
      begin
         Domain := AML_Frame_Handles.No_Domain;
         if not Mutex_Registry_Live (E) or else Mutex_Registry.Active (E.Mutexes) then
            Status := AML_Execute.Unsupported_Context; return;
         end if;
         Begin_Invocation (E, Domain, Status);
         if Status /= AML_Execute.Available then return; end if;
         Mutex_Registry.Begin_Invocation (E.Mutexes, Mutex_Admission_For (E),
           E.Token, E.Invocation_Issued, Domain, Outcome);
         if Outcome /= Mutex_Registry.Succeeded then
            Domain := AML_Frame_Handles.No_Domain;
            Status := AML_Execute.Unsupported_Context; return;
         end if;
         Admitted_Domain := Domain;
         E.Last_Fatal := (Present => False);
      end Begin_Root_Invocation;
      function Mutex_Status (Outcome : Mutex_Registry.Outcome) return AML_Execute.Execution_Status is
        (case Outcome is
           when Mutex_Registry.Succeeded => AML_Execute.Returned,
           when Mutex_Registry.Order_Error => AML_Execute.Mutex_Order,
           when Mutex_Registry.Not_Acquired => AML_Execute.Mutex_Not_Acquired,
           when Mutex_Registry.Capacity_Limit | Mutex_Registry.Depth_Limit | Mutex_Registry.Token_Limit => AML_Execute.Value_Limit,
           when others => AML_Execute.Unsupported_Value);
      procedure Enter_Serialized
        (E : in out Arena; Method : Natural; Frame : AML_Frame_Handles.Frame_Handle;
         Status : in out AML_Execute.Execution_Status)
      is
         Admission : Mutex_Admission := Mutex_Admission_For (E);
         Outcome : Mutex_Registry.Outcome;
         Token : Mutex_Registry.Method_Token;
         Key : Mutex_Method_Key;
         use type Mutex_Registry.Outcome;
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Admission.Registry_OK or else not Mutex_Frame_Live (E, Frame)
           or else Method = 0 or else Method > Natural (E.Tree.Used)
           or else not Mutex_Registry."=" (E.Mutex_Method_Tokens (AML_Frame_Handles.Index_Of (Frame)), Mutex_Registry.No_Token)
         then return; end if;
         declare
            Node : constant Node_ID := Node_ID (Method);
         begin
            if not E.Tree.Items (Node).Alive or else E.Tree.Items (Node).Object_Type /= Method_Object
              or else (Natural (E.Tree.Items (Node).Method_Flags) / 8) mod 2 = 0 then return; end if;
            Key := (Node, E.Tree.Items (Node).Incarnation,
              Natural (E.Tree.Items (Node).Method_Flags), Natural (E.Tree.Items (Node).Method_Offset),
              Natural (E.Tree.Items (Node).Method_Size));
            Admission.Kind := Method_Entry; Admission.Method := Key;
            Admission.Level := Natural (AML_Execute.Method_Level (E.Tree.Items (Node).Method_Flags));
            Admission.Target_OK := Mutex_Method_Live (E, Key, Admission.Level);
            Admission.Frame := Frame; Admission.Frame_OK := True;
         end;
         Mutex_Registry.Enter_Method (E.Mutexes, Admission, Admitted_Domain, Key,
           Admission.Level, Frame, Token, Outcome);
         Status := Mutex_Status (Outcome);
         if Outcome = Mutex_Registry.Succeeded then
            E.Mutex_Method_Tokens (AML_Frame_Handles.Index_Of (Frame)) := Token;
         end if;
      end Enter_Serialized;
      procedure Leave_Serialized
        (E : in out Arena; Frame : AML_Frame_Handles.Frame_Handle;
         Status : in out AML_Execute.Execution_Status)
      is
         Admission : Mutex_Admission := Mutex_Admission_For (E);
         Outcome : Mutex_Registry.Outcome;
         use type Mutex_Registry.Outcome;
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Admission.Registry_OK or else not Mutex_Frame_Live (E, Frame) then return; end if;
         Admission.Kind := Method_Leave; Admission.Frame := Frame; Admission.Frame_OK := True;
         Mutex_Registry.Leave_Method (E.Mutexes, Admission, Admitted_Domain,
           E.Mutex_Method_Tokens (AML_Frame_Handles.Index_Of (Frame)), Outcome);
         Status := Mutex_Status (Outcome);
         if Outcome = Mutex_Registry.Succeeded then
            E.Mutex_Method_Tokens (AML_Frame_Handles.Index_Of (Frame)) := Mutex_Registry.No_Token;
         end if;
      end Leave_Serialized;

      function Root_Capacity (E : Arena) return Boolean is
        (Frame_Pins.Count (E.Frame_Roots) < Max_Frame_Roots);
      procedure Open_Root (E : in out Arena; Frame : AML_Frame_Handles.Frame_Handle) is
         Status : Frame_Pins.Result_Status;
         use type Frame_Pins.Result_Status;
      begin
         if not AML_Frame_Handles.Belongs_To (Frame, Admitted_Domain)
         then raise Program_Error with "frame root owner invariant"; end if;
         if AML_Frame_Handles.Index_Of (Frame) not in E.Mutex_Frames'Range
           or else AML_Frame_Handles.Present (E.Mutex_Frames (AML_Frame_Handles.Index_Of (Frame)))
         then raise Program_Error with "mutex frame admission invariant"; end if;
         Frame_Pins.Reserve (E.Frame_Roots, Frame, Status);
         if Status /= Frame_Pins.Ready then raise Program_Error with "frame root admission invariant"; end if;
         E.Mutex_Frames (AML_Frame_Handles.Index_Of (Frame)) := Frame;
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
         if not Mutex_Frame_Live (E, Frame)
           or else not Mutex_Registry."="
             (E.Mutex_Method_Tokens (AML_Frame_Handles.Index_Of (Frame)), Mutex_Registry.No_Token)
         then raise Program_Error with "mutex frame close invariant"; end if;
         Frame_Pins.Release (E.Frame_Roots, Frame, Status);
         if Status /= Frame_Pins.Ready then raise Program_Error with "frame root close invariant"; end if;
         E.Mutex_Frames (AML_Frame_Handles.Index_Of (Frame)) := AML_Frame_Handles.No_Frame;
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
         Copy_Value (E, Width, Item, Copy, Status);
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
      -- Explicit frame-bound leases distinguish module permanence from method cleanup.
      Execute_Declaration_Lease_Store : AML_Declaration_Contexts.State := AML_Declaration_Contexts.Empty;
      use type AML_Declaration_Contexts.Request;
      use type AML_Declaration_Contexts.Lease;
      type Execute_Declaration_Name_Token is record
         Owner : AML_Declaration_Contexts.Lease := AML_Declaration_Contexts.No_Lease;
         Original : Name_Reservation := No_Name_Reservation;
      end record;
      Execute_Declaration_No_Name : constant Execute_Declaration_Name_Token := (others => <>);
      use type AML_Declaration_Contexts.Scope_Identity;
      function Live_Lexical_Identity
        (Environment : Arena; Item : AML_Declaration_Contexts.Scope_Identity) return Boolean is
        (AML_Declaration_Contexts.Present (Item)
         and then Natural (AML_Declaration_Contexts.Node_Of (Item)) <= Environment.Tree.Used
         and then (if Natural (AML_Declaration_Contexts.Node_Of (Item)) = Root then
           Item = AML_Declaration_Contexts.Root_Scope
          else Environment.Tree.Items (Node_ID (AML_Declaration_Contexts.Node_Of (Item))).Alive
           and then Natural (AML_Declaration_Contexts.Stamp_Of (Item)) =
             Natural (Environment.Tree.Items (Node_ID (AML_Declaration_Contexts.Node_Of (Item))).Incarnation)));
      -- Cleanup authenticates the frame even if its lexical entry was retired.
      function Execute_Module_Frame_Valid (Environment : Arena; Lease : AML_Declaration_Contexts.Lease) return Boolean is
        (AML_Declaration_Contexts.Is_Module (Lease)
         and then AML_Declaration_Contexts.Is_Live (Execute_Declaration_Lease_Store, Lease)
         and then AML_Frame_Handles.Belongs_To (AML_Declaration_Contexts.Frame_Of (Lease), Admitted_Domain)
         and then (Nested_Loads_Enabled or else AML_Declaration_Contexts.Is_Module (Session_Request))
         and then AML_Declaration_Contexts.Scope_Of (Lease) = Root
         and then AML_Declaration_Contexts.Stamp_Of (Lease) = Natural (Environment.Invocation_Issued)
         and then AML_Module_Frames.Top (Environment.Tree.Module_Frames) = AML_Declaration_Contexts.Frame_Of (Lease));
      function Execute_Declaration_Valid (Environment : Arena; Lease : AML_Declaration_Contexts.Lease) return Boolean
      is (AML_Declaration_Contexts.Is_Live (Execute_Declaration_Lease_Store, Lease)
        and then AML_Frame_Handles.Belongs_To (AML_Declaration_Contexts.Frame_Of (Lease), Admitted_Domain)
        and then (if AML_Declaration_Contexts.Is_Module (Lease) then
           Execute_Module_Frame_Valid (Environment, Lease)
           and then Live_Lexical_Identity (Environment, AML_Declaration_Contexts.Entry_Identity (Lease))
         else AML_Declaration_Contexts.Scope_Of (Lease) > 0
           and then AML_Declaration_Contexts.Scope_Of (Lease) <= Environment.Tree.Used
           and then Environment.Tree.Items (Node_ID (AML_Declaration_Contexts.Scope_Of (Lease))).Alive
           and then Environment.Tree.Items (Node_ID (AML_Declaration_Contexts.Scope_Of (Lease))).Object_Type = Method_Object
           and then Environment.Tree.Items (Node_ID (AML_Declaration_Contexts.Scope_Of (Lease))).Active_Calls > 0
           and then AML_Declaration_Contexts.Stamp_Of (Lease) = Natural (Environment.Tree.Items (Node_ID (AML_Declaration_Contexts.Scope_Of (Lease))).Incarnation)));
      procedure Recover_Module_Divide
        (E : in out Arena; Lease : AML_Declaration_Contexts.Lease;
         Resume_Offset, Charged : Natural; Continue_Execution : in out Boolean) is
      begin
         Continue_Execution := False;
         if not Module_Recovery_Enabled or else E.Token /= Entry_Token
           or else not AML_Declaration_Contexts.Is_Module (Lease)
           or else not Execute_Declaration_Valid (E, Lease)
           or else not Has_Definition (E.Tree.Current_Definition)
           or else E.Field_Uncertain.Present or else Charged > Budget
         then return; end if;
         if E.Recovered_Module_Errors.Count < Module_Error_Count'Last then
            E.Recovered_Module_Errors.Count := E.Recovered_Module_Errors.Count + 1;
         end if;
         E.Recovered_Module_Errors.Kind := Module_Division_By_Zero;
         E.Recovered_Module_Errors.Definition := E.Tree.Current_Definition;
         E.Recovered_Module_Errors.Resume_Offset := Resume_Offset;
         E.Recovered_Module_Errors.Charged := Charged;
         Continue_Execution := True;
      end Recover_Module_Divide;
      function Execute_Declaration_Lexical_Valid
        (Environment : Arena; Lease : AML_Declaration_Contexts.Lease; Scope : Natural) return Boolean is
        (Execute_Declaration_Valid (Environment, Lease)
         and then Live_Lexical_Identity (Environment,
           AML_Declaration_Contexts.Lexical_Top (Execute_Declaration_Lease_Store, Lease))
         and then Natural (AML_Declaration_Contexts.Node_Of
           (AML_Declaration_Contexts.Lexical_Top (Execute_Declaration_Lease_Store, Lease))) = Scope);
      procedure Execute_Container_Enter
        (Environment : in out Arena; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Path : AML_Names.Name_Result;
         Metadata : AML_Container_Headers.Attributes; New_Scope : out Natural;
         Status : out AML_Execute.Execution_Status)
      is
         Candidate : State := Environment.Tree;
         Located : Lookup_Result;
         Base, Node : Node_ID;
         Added : Insert_Status;
         Pushed : Boolean;
         use type AML_Container_Headers.Container_Kind;
      begin
         New_Scope := Scope; Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Lexical_Valid (Environment, Lease, Scope) then return; end if;
         if Metadata.Kind = AML_Container_Headers.Scope_Container then
            Located := Resolve_Object (Candidate, Node_ID (Scope), Path);
            if Located.Status /= Found then Status := AML_Execute.Unknown_Name; return; end if;
            Node := Located.Node;
            if not Static_Scope (Kind (Candidate, Node)) then Status := AML_Execute.Unsupported_Value; return; end if;
         else
            Name_Target (Candidate, Scope, AML_Declaration_Contexts.Scope_Of (Lease),
              Path, Base, Status, AML_Declaration_Contexts.Is_Module (Lease));
            if Status /= AML_Execute.Returned then return; end if;
            Insert (Candidate, Base, Path.Parts (Path.Count), Node, Added);
            case Added is
               when Duplicate => Status := AML_Execute.Duplicate_Name; return;
               when Full => Status := AML_Execute.Namespace_Limit; return;
               when Invalid_Name => Status := AML_Execute.Bad_Name; return;
               when Inserted => null;
            end case;
            Candidate.Items (Node).Owner := Node_ID (AML_Declaration_Contexts.Scope_Of (Lease));
            case Metadata.Kind is
               when AML_Container_Headers.Device_Container => Candidate.Items (Node).Object_Type := Device_Object;
               when AML_Container_Headers.Thermal_Zone_Container => Candidate.Items (Node).Object_Type := Thermal_Zone_Object;
               when AML_Container_Headers.Processor_Container =>
                  Candidate.Items (Node).Object_Type := Processor_Object;
                  Candidate.Items (Node).Processor_Info :=
                    (AML_Decode.Byte (Metadata.Processor_ID), Processor_Block_Address (Metadata.Block_Address),
                     AML_Decode.Byte (Metadata.Block_Length));
               when AML_Container_Headers.Power_Resource_Container =>
                  Candidate.Items (Node).Object_Type := Power_Resource_Object;
                  Candidate.Items (Node).Power_Info :=
                    (AML_Decode.Byte (Metadata.System_Level), Resource_Order (Metadata.Order));
               when AML_Container_Headers.Scope_Container => null;
            end case;
         end if;
         AML_Declaration_Contexts.Push_Scope
           (Execute_Declaration_Lease_Store, Lease,
            AML_Declaration_Contexts.Lexical_Top (Execute_Declaration_Lease_Store, Lease),
            AML_Declaration_Contexts.Make_Scope (AML_Declaration_Contexts.Scope_Node (Node),
              AML_Declaration_Contexts.Scope_Incarnation
                (if Node = Root then 0 else Natural (Candidate.Items (Node).Incarnation))), Pushed);
         if not Pushed then Status := AML_Execute.Block_Limit; return; end if;
         Environment.Tree := Candidate;
         New_Scope := Natural (Node); Status := AML_Execute.Returned;
      end Execute_Container_Enter;
      procedure Execute_Container_Leave
        (Environment : in out Arena; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Restored_Scope : out Natural; Status : out AML_Execute.Execution_Status)
      is
         pragma Unmodified (Environment);
         Candidate : AML_Declaration_Contexts.State := Execute_Declaration_Lease_Store;
         Popped : Boolean;
      begin
         Restored_Scope := Scope; Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Lexical_Valid (Environment, Lease, Scope) then return; end if;
         AML_Declaration_Contexts.Pop_Scope (Candidate, Lease,
           AML_Declaration_Contexts.Lexical_Top (Candidate, Lease), Popped);
         if not Popped or else not Live_Lexical_Identity
           (Environment, AML_Declaration_Contexts.Lexical_Top (Candidate, Lease)) then return; end if;
         Execute_Declaration_Lease_Store := Candidate;
         Restored_Scope := Natural (AML_Declaration_Contexts.Node_Of
           (AML_Declaration_Contexts.Lexical_Top (Candidate, Lease)));
         Status := AML_Execute.Returned;
      end Execute_Container_Leave;
      procedure Evaluate_Internal
        (Environment : Arena; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Implementation : AML_Execute.Method_Implementation;
         Args : AML_Execute.Value_Arguments; Count : Natural;
         Reply : in out AML_Execute.Internal_Method_Reply)
      is
         Data : AML_Decode.Bytes (1 .. AML_Interface_Policy.Max_Interface_Bytes) := [others => 0];
         Used : AML_Interface_Policy.Interface_Length := 0;
         ID : AML_Objects.Object_ID;
         Length : Natural;
         B : AML_Decode.Byte;
      begin
         Reply := (Status => AML_Execute.Internal_Rejected, Value => 0);
         if Implementation /= AML_Execute.Interface_Query
           or else not Execute_Declaration_Valid (Environment, Lease)
           or else AML_Declaration_Contexts.Is_Module (Lease)
           or else AML_Declaration_Contexts.Scope_Of (Lease) /= Scope
           or else Scope = 0 or else Scope > Environment.Tree.Used
           or else Environment.Tree.Items (Node_ID (Scope)).Implementation /= AML_Execute.Interface_Query
           or else Count /= 1 or else Args (0).Value_Kind /= AML_Execute.Object_Datum then return; end if;
         -- Authenticate the slot/incarnation/owner without materializing or
         -- numerically coercing the entire logical String through Read_Source.
         if not Has_Source (Environment, Args (0).Object.Source) then return; end if;
         ID := AML_References.Source (Args (0).Object.Source);
         if ID /= Args (0).Object.ID then return; end if;
         if not AML_Objects.Is_Live (Environment.Tree.Values, ID)
           or else AML_Objects.Kind (Environment.Tree.Values, ID) /= AML_Objects.String_Object then return; end if;
         Length := AML_Objects.Length (Environment.Tree.Values, ID);
         Reply := (Status => AML_Execute.Internal_Returned, Value => 0);
         -- ACPICA observes the C-string prefix, while SizeOf retains the full
         -- AML string length. Inspect at most max policy bytes plus terminator.
         for J in 1 .. Natural'Min (Length, AML_Interface_Policy.Max_Interface_Bytes + 1) loop
            B := AML_Objects.Stored_Byte (Environment.Tree.Values, ID, J - 1);
            exit when B = 0;
            if Used = AML_Interface_Policy.Max_Interface_Bytes then return; end if;
            Used := Used + 1;
            Data (Used) := B;
         end loop;
         if AML_Interface_Policy.Supports (Interface_Policy, Data (1 .. Used)) then
            Reply.Value := AML_Decode.Integer_Value'Last;
         end if;
      end Evaluate_Internal;
      procedure Execute_Declaration_Enter
        (Environment : in out Arena; Intent : AML_Declaration_Contexts.Request; Scope : Natural;
         Frame : AML_Frame_Handles.Frame_Handle; Lease : out AML_Declaration_Contexts.Lease; Allowed : out Boolean) is
      begin
         Lease := AML_Declaration_Contexts.No_Lease; Allowed := False;
         if not AML_Frame_Handles.Belongs_To (Frame, Admitted_Domain) then return; end if;
         if AML_Declaration_Contexts.Is_Module (Intent) then
            if not (Nested_Loads_Enabled or else AML_Declaration_Contexts.Is_Module (Session_Request)) or else Scope > Environment.Tree.Used then return; end if;
            if Scope /= Root and then (not Environment.Tree.Items (Node_ID (Scope)).Alive
              or else not Static_Scope (Environment.Tree.Items (Node_ID (Scope)).Object_Type)) then return; end if;
            AML_Declaration_Contexts.Open_Module (Execute_Declaration_Lease_Store, Frame,
              (if Scope = Root then AML_Declaration_Contexts.Root_Scope
               else AML_Declaration_Contexts.Make_Scope (AML_Declaration_Contexts.Scope_Node (Scope),
                 AML_Declaration_Contexts.Scope_Incarnation (Environment.Tree.Items (Node_ID (Scope)).Incarnation))),
              Environment.Invocation_Issued, Lease, Allowed);
            if Allowed then
               AML_Module_Frames.Push (Environment.Tree.Module_Frames, Frame, Admitted_Domain, Allowed);
               if not Allowed then
                  AML_Declaration_Contexts.Close (Execute_Declaration_Lease_Store, Lease);
                  Lease := AML_Declaration_Contexts.No_Lease;
               end if;
            end if;
         else
            if Intent /= AML_Declaration_Contexts.Method_Request then return; end if;
            Begin_Call (Environment, Scope, Allowed);
            if not Allowed then return; end if;
            AML_Declaration_Contexts.Open (Execute_Declaration_Lease_Store, Intent, Frame, Scope,
              Natural (Environment.Tree.Items (Node_ID (Scope)).Incarnation), Lease, Allowed);
            if not Allowed then End_Call (Environment, Scope); end if;
         end if;
      end Execute_Declaration_Enter;
      procedure Execute_Declaration_Leave (Environment : in out Arena; Lease : AML_Declaration_Contexts.Lease) is
      begin
         if not Execute_Declaration_Valid (Environment, Lease)
           and then not Execute_Module_Frame_Valid (Environment, Lease) then
            if AML_Declaration_Contexts.Is_Module (Lease)
              and then AML_Module_Frames.Contains (Environment.Tree.Module_Frames,
                AML_Declaration_Contexts.Frame_Of (Lease)) then
               raise Program_Error with "module frame order invariant";
            end if;
            AML_Declaration_Contexts.Close (Execute_Declaration_Lease_Store, Lease);
            return;
         end if;
         if AML_Declaration_Contexts.Is_Module (Lease) then
            -- Pending initialization markers cannot outlive the authenticated frame.
            for I in 1 .. Environment.Tree.Used loop
               if Environment.Tree.Items (I).Initializing and then Environment.Tree.Items (I).Owner = Root
                 and then Environment.Tree.Items (I).Initializer_Frame = AML_Declaration_Contexts.Frame_Of (Lease) then
                  Environment.Tree.Items (I) := Retired_Entry (Environment.Tree.Items (I));
               end if;
            end loop;
            declare
               Removed : Boolean;
            begin
               AML_Module_Frames.Pop (Environment.Tree.Module_Frames,
                 AML_Declaration_Contexts.Frame_Of (Lease), Removed);
               if not Removed then raise Program_Error with "module frame close invariant"; end if;
            end;
         else End_Call (Environment, AML_Declaration_Contexts.Scope_Of (Lease)); end if;
         AML_Declaration_Contexts.Close (Execute_Declaration_Lease_Store, Lease);
      end Execute_Declaration_Leave;
      procedure Execute_Alias (Environment : in out Arena; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Source_Path, Target_Path : AML_Names.Name_Result;
         Status : out AML_Execute.Execution_Status) is
         Candidate : State := Environment.Tree;
         Source : Lookup_Result;
         Base, Node : Node_ID;
         Added : Insert_Status;
      begin
         Status := AML_Execute.Unsupported;
         if not Execute_Declaration_Lexical_Valid (Environment, Lease, Scope)
           or else not AML_Declaration_Contexts.Is_Module (Lease) then return; end if;
         Source := Resolve_Object (Candidate, Node_ID (Scope), Source_Path);
         if Source.Status /= Found then Status := AML_Execute.Unknown_Name; return; end if;
         Name_Target (Candidate, Scope, AML_Declaration_Contexts.Scope_Of (Lease), Target_Path, Base, Status, True);
         if Status /= AML_Execute.Returned then return; end if;
         Insert (Candidate, Base, Target_Path.Parts (Target_Path.Count), Node, Added);
         case Added is
            when Duplicate => Status := AML_Execute.Duplicate_Name; return;
            when Full => Status := AML_Execute.Namespace_Limit; return;
            when Invalid_Name => Status := AML_Execute.Bad_Name; return;
            when Inserted => null;
         end case;
         Candidate.Items (Node).Object_Type := Alias_Object;
         Candidate.Items (Node).Alias_Target := Source.Node;
         Candidate.Items (Node).Alias_Stamp := (if Source.Node = Root then 0 else Candidate.Items (Source.Node).Incarnation);
         Environment.Tree := Candidate; Status := AML_Execute.Returned;
      end Execute_Alias;
      procedure Execute_Mutex
        (Environment : in out Arena; Item : AML_Execute.Datum;
         Action : AML_Mutexes.Operation; Timeout : AML_Mutexes.Timeout;
         Status : in out AML_Execute.Execution_Status)
      is
         pragma Unreferenced (Timeout); -- Uncontended only; no provider waits.
         ID : AML_Objects.Object_ID;
         Admission : Mutex_Admission := Mutex_Admission_For (Environment);
         Outcome : Mutex_Registry.Outcome;
         use type AML_Mutexes.Operation;
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Admission.Registry_OK then return; end if;
         if Item.Value_Kind = AML_Execute.Reference_Datum then
            if AML_References.Kind (Item.Ref) /= AML_References.Named_Cell
              or else not Named_Identity_Matches (Environment, Item.Ref) then return; end if;
            declare
               Node : constant Node_ID := Node_ID (AML_References.Named_Node (Item.Ref));
            begin
               if Node = Root or else Environment.Tree.Items (Node).Object_Type /= Mutex_Object then return; end if;
               -- The FACS Global Lock requires a real platform protocol.
               if Parent (Environment.Tree, Node) = Root and then Name (Environment.Tree, Node) = "_GL_" then return; end if;
               ID := Environment.Tree.Items (Node).Object_Ref;
               if not AML_Objects.Is_Live (Environment.Tree.Values, ID)
                 or else AML_Objects.Kind (Environment.Tree.Values, ID) /= AML_Objects.Mutex_Object
                 or else Natural (Environment.Tree.Items (Node).Mutex_Flags) /= Natural (AML_Objects.Mutex_Flags (Environment.Tree.Values, ID)) then return; end if;
            end;
         else
            if Item.Value_Kind /= AML_Execute.Object_Datum
              or else Item.Object.Type_Code /= AML_Mutexes.Object_Type_Code
              or else not Has_Source (Environment, Item.Object.Source)
              or else AML_References.Source (Item.Object.Source) /= Item.Object.ID then return; end if;
            ID := Item.Object.ID;
         end if;
         if not AML_Objects.Is_Live (Environment.Tree.Values, ID)
           or else AML_Objects.Kind (Environment.Tree.Values, ID) /= AML_Objects.Mutex_Object then return; end if;
         declare
            Global : constant Lookup_Result := Canonical_Child (Environment.Tree, Root, "_GL_");
         begin
            if Global.Status = Found and then Global.Node /= Root
              and then Environment.Tree.Items (Global.Node).Object_Ref = ID then return; end if;
         end;
         Admission.Address := AML_Objects.Address_Of (Environment.Tree.Values, ID);
         Admission.Level := Natural (AML_Objects.Mutex_Level (Environment.Tree.Values, ID));
         Admission.Target_OK := Mutex_Object_Live (Environment, Admission.Address, Admission.Level);
         if Action = AML_Mutexes.Acquire_Mutex then
            Mutex_Registry.Acquire_Uncontended (Environment.Mutexes, Admission,
              Admitted_Domain, Admission.Address, Admission.Level, False, Outcome);
         else
            Mutex_Registry.Release (Environment.Mutexes, Admission, Admitted_Domain,
              Admission.Address, Outcome);
         end if;
         Status := Mutex_Status (Outcome);
      end Execute_Mutex;
      procedure Execute_Event
        (Environment : in out Arena; Item : AML_Execute.Datum;
         Action : AML_Events.Action; Timeout : AML_Events.Timeout;
         Timed_Out : in out Boolean; Status : in out AML_Execute.Execution_Status)
      is
         ID : AML_Objects.Object_ID;
         Node : Node_ID;
         use type AML_Events.Action;
         Units : AML_Objects.Event_Unit_Count;
         Outcome : AML_Events.Timeout_Outcome;
         use type AML_Events.Timeout_Outcome;
      begin
         Timed_Out := False; Status := AML_Execute.Unsupported_Value;
         if Action = AML_Events.Wait_Event and then Item.Value_Kind = AML_Execute.Reference_Datum then
            -- Deferred namespace operand: the node's current object is rooted
            -- by its live namespace entry. No old object is captured here.
            if AML_References.Kind (Item.Ref) /= AML_References.Named_Cell
              or else not Named_Identity_Matches (Environment, Item.Ref) then return; end if;
            Node := Node_ID (AML_References.Named_Node (Item.Ref));
            if Node = Root or else Environment.Tree.Items (Node).Object_Type /= Event_Object then return; end if;
            ID := Environment.Tree.Items (Node).Object_Ref;
         else
            if Item.Value_Kind /= AML_Execute.Object_Datum
              or else not Has_Source (Environment, Item.Object.Source)
              or else AML_References.Source (Item.Object.Source) /= Item.Object.ID then return; end if;
            ID := Item.Object.ID;
         end if;
         if not AML_Objects.Is_Live (Environment.Tree.Values, ID)
           or else AML_Objects.Kind (Environment.Tree.Values, ID) /= AML_Objects.Event_Object then return; end if;
         Units := AML_Objects.Event_Units (Environment.Tree.Values, ID);
         case Action is
            when AML_Events.Signal_Event =>
               if Units = AML_Objects.Max_Event_Units then Status := AML_Execute.Value_Limit; return; end if;
               AML_Objects.Set_Event_Units (Environment.Tree.Values, ID, Units + 1);
            when AML_Events.Reset_Event =>
               AML_Objects.Set_Event_Units (Environment.Tree.Values, ID, 0);
            when AML_Events.Wait_Event =>
               if Units > 0 then
                  AML_Objects.Set_Event_Units (Environment.Tree.Values, ID, Units - 1);
               elsif Timeout = AML_Events.Poll_Only then Timed_Out := True;
               elsif Timeout = AML_Events.Wait_Forever then Status := AML_Execute.Unsupported; return;
               else
                  Wait_Event_Timeout (Timeout, Outcome);
                  case Outcome is
                     when AML_Events.Elapsed => Timed_Out := True;
                     when AML_Events.Unavailable => Status := AML_Execute.Unsupported; return;
                     when AML_Events.Failed => Status := AML_Execute.Delay_Failed; return;
                  end case;
               end if;
         end case;
         Status := AML_Execute.Returned;
      end Execute_Event;
      procedure Execute_Declaration_Event
        (Environment : in out Arena; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Path : AML_Names.Name_Result;
         Status : in out AML_Execute.Execution_Status)
      is
         Candidate : State := Environment.Tree;
         Object : AML_Objects.Object_ID;
         Allocated : AML_Objects.Allocation_Status;
         Base, Node : Node_ID;
         Added : Insert_Status;
         Gate : Allocation_Hook_Status;
         procedure Reserve_Name is
         begin
            Name_Target (Candidate, Scope, AML_Declaration_Contexts.Scope_Of (Lease),
              Path, Base, Status, AML_Declaration_Contexts.Is_Module (Lease));
            if Status /= AML_Execute.Returned then return; end if;
            Insert (Candidate, Base, Path.Parts (Path.Count), Node, Added);
            case Added is
               when Duplicate => Status := AML_Execute.Duplicate_Name;
               when Full => Status := AML_Execute.Namespace_Limit;
               when Invalid_Name => Status := AML_Execute.Bad_Name;
               when Inserted => Status := AML_Execute.Returned;
            end case;
         end Reserve_Name;
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Lexical_Valid (Environment, Lease, Scope) then return; end if;
         -- Reject namespace failures before collection; these leave the arena
         -- and its collection metrics unchanged.
         Reserve_Name;
         if Status /= AML_Execute.Returned then return; end if;
         Before_Allocation (Environment, [], Gate);
         if Gate /= Allocation_Allowed then Status := AML_Execute.Unsupported_Value; return; end if;
         -- Collection is real. Never restore the pre-collection snapshot.
         -- Rebuild and validate the transaction against the post-hook arena.
         Candidate := Environment.Tree;
         if not Execute_Declaration_Lexical_Valid (Environment, Lease, Scope) then
            Status := AML_Execute.Unsupported_Value; return;
         end if;
         Reserve_Name;
         if Status /= AML_Execute.Returned then return; end if;
         AML_Objects.New_Event (Candidate.Values, Object, Allocated);
         if Allocated /= AML_Objects.Allocated then Status := AML_Execute.Value_Limit; return; end if;
         Candidate.Items (Node).Owner := Node_ID (AML_Declaration_Contexts.Scope_Of (Lease));
         Candidate.Items (Node).Object_Type := Event_Object;
         Candidate.Items (Node).Object_Ref := Object;
         Environment.Tree := Candidate;
         Status := AML_Execute.Returned;
      end Execute_Declaration_Event;
      procedure Execute_Declaration_Mutex
        (Environment : in out Arena; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Path : AML_Names.Name_Result; Flags : AML_Decode.Byte;
         Status : in out AML_Execute.Execution_Status)
      is
         Candidate : State := Environment.Tree;
         Object : AML_Objects.Object_ID;
         Allocated : AML_Objects.Allocation_Status;
         Base, Node : Node_ID;
         Added : Insert_Status;
         Gate : Allocation_Hook_Status;
         procedure Reserve_Name is
         begin
            Name_Target (Candidate, Scope, AML_Declaration_Contexts.Scope_Of (Lease),
              Path, Base, Status, AML_Declaration_Contexts.Is_Module (Lease));
            if Status /= AML_Execute.Returned then return; end if;
            Insert (Candidate, Base, Path.Parts (Path.Count), Node, Added);
            case Added is
               when Duplicate => Status := AML_Execute.Duplicate_Name;
               when Full => Status := AML_Execute.Namespace_Limit;
               when Invalid_Name => Status := AML_Execute.Bad_Name;
               when Inserted => Status := AML_Execute.Returned;
            end case;
         end Reserve_Name;
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Lexical_Valid (Environment, Lease, Scope) then return; end if;
         if not AML_Mutexes.Canonical_Flags (Flags) then return; end if;
         -- Reject namespace failures before collection; these leave the arena
         -- and its collection metrics unchanged.
         Reserve_Name;
         if Status /= AML_Execute.Returned then return; end if;
         Before_Allocation (Environment, [], Gate);
         if Gate /= Allocation_Allowed then Status := AML_Execute.Unsupported_Value; return; end if;
         -- Collection is real. Never restore the pre-collection snapshot.
         -- Rebuild and validate the transaction against the post-hook arena.
         Candidate := Environment.Tree;
         if not Execute_Declaration_Lexical_Valid (Environment, Lease, Scope) then
            Status := AML_Execute.Unsupported_Value; return;
         end if;
         Reserve_Name;
         if Status /= AML_Execute.Returned then return; end if;
         AML_Objects.New_Mutex (Candidate.Values, Flags, Object, Allocated);
         if Allocated /= AML_Objects.Allocated then Status := AML_Execute.Value_Limit; return; end if;
         Candidate.Items (Node).Owner := Node_ID (AML_Declaration_Contexts.Scope_Of (Lease));
         Candidate.Items (Node).Object_Type := Mutex_Object;
         Candidate.Items (Node).Mutex_Flags := Flags;
         Candidate.Items (Node).Object_Ref := Object;
         Environment.Tree := Candidate;
         Status := AML_Execute.Returned;
      end Execute_Declaration_Mutex;
      procedure Execute_Declaration_Method
        (Environment : in out Arena; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result; Flags : AML_Decode.Byte; Width : AML_Decode.Integer_Width; Code : AML_Decode.Bytes; Status : out AML_Execute.Declaration_Status) is
      begin
         if not Execute_Declaration_Lexical_Valid (Environment, Lease, Scope) then
            Status := AML_Execute.Declaration_Unsupported; return;
         end if;
         Define_Method_For_Context (Environment.Tree, Scope, AML_Declaration_Contexts.Scope_Of (Lease), Path, Flags, Width, Code, Status, AML_Declaration_Contexts.Is_Module (Lease));
      end Execute_Declaration_Method;
      procedure Execute_Declaration_Fields
        (Environment : in out Arena; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Region : AML_Names.Name_Result; Flags : AML_Decode.Byte; Entries : AML_Decode.Bytes; Status : out AML_Execute.Execution_Status) is
      begin
         if not Execute_Declaration_Lexical_Valid (Environment, Lease, Scope) then
            Status := AML_Execute.Unsupported_Value; return;
         end if;
         Define_Fields_For_Context (Environment.Tree, Scope, AML_Declaration_Contexts.Scope_Of (Lease), Region, Flags, Entries, Status, AML_Declaration_Contexts.Is_Module (Lease));
      end Execute_Declaration_Fields;
      procedure Execute_Declaration_Indexed_Fields
        (Environment : in out Arena; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Index_Path, Data_Path : AML_Names.Name_Result;
         Flags : AML_Decode.Byte; Entries : AML_Decode.Bytes;
         Status : in out AML_Execute.Execution_Status) is
         Index_Item, Data_Item : Lookup_Result;
         Lifetime_Owner : Node_ID := Root;
         function Suitable (N : Node_ID) return Boolean is
           (N > Root and then Present (Environment.Tree, N)
            and then Kind (Environment.Tree, N) = Region_Field_Object
            and then not Environment.Tree.Items (N).Initializing
            and then (Environment.Tree.Items (N).Owner = Root
              or else Environment.Tree.Items (N).Owner = Lifetime_Owner));
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Lexical_Valid (Environment, Lease, Scope) then return; end if;
         Lifetime_Owner := Node_ID (AML_Declaration_Contexts.Scope_Of (Lease));
         Index_Item := Resolve_Object (Environment.Tree, Node_ID (Scope), Index_Path);
         Data_Item := Resolve_Object (Environment.Tree, Node_ID (Scope), Data_Path);
         if Index_Item.Status /= Found or else Data_Item.Status /= Found then
            Status := AML_Execute.Unknown_Name; return;
         end if;
         if not Suitable (Index_Item.Node) or else not Suitable (Data_Item.Node) then return; end if;
         Define_Operation_Fields_For_Context (Environment.Tree, Node_ID (Scope), Lifetime_Owner,
           Index_Item.Node, Flags, Entries, Status, Data_Item.Node);
      end Execute_Declaration_Indexed_Fields;
      procedure Execute_Declaration_Banked_Fields
        (Environment : in out Arena; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Region_Path, Bank_Path : AML_Names.Name_Result;
         Bank_Value : AML_Decode.Integer_Value; Flags : AML_Decode.Byte;
         Entries : AML_Decode.Bytes; Status : in out AML_Execute.Execution_Status) is
         Region_Item, Bank_Item : Lookup_Result;
         Lifetime_Owner : Node_ID := Root;
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Lexical_Valid (Environment, Lease, Scope) then return; end if;
         Lifetime_Owner := Node_ID (AML_Declaration_Contexts.Scope_Of (Lease));
         Region_Item := Resolve_Object (Environment.Tree, Node_ID (Scope), Region_Path);
         Bank_Item := Resolve_Object (Environment.Tree, Node_ID (Scope), Bank_Path);
         if Region_Item.Status /= Found or else Bank_Item.Status /= Found then
            Status := AML_Execute.Unknown_Name; return;
         end if;
         if Region_Item.Node = Root or else Bank_Item.Node = Root
           or else not Present (Environment.Tree, Region_Item.Node)
           or else not Present (Environment.Tree, Bank_Item.Node)
           or else Kind (Environment.Tree, Region_Item.Node) /= Operation_Region_Object
           or else Kind (Environment.Tree, Bank_Item.Node) /= Region_Field_Object
           or else Environment.Tree.Items (Region_Item.Node).Initializing
           or else Environment.Tree.Items (Bank_Item.Node).Initializing
         then return; end if;
         if (Environment.Tree.Items (Region_Item.Node).Owner /= Root
           and then Environment.Tree.Items (Region_Item.Node).Owner /= Lifetime_Owner)
           or else (Environment.Tree.Items (Bank_Item.Node).Owner /= Root
             and then Environment.Tree.Items (Bank_Item.Node).Owner /= Lifetime_Owner)
         then return; end if;
         Define_Operation_Fields_For_Context (Environment.Tree, Node_ID (Scope), Lifetime_Owner,
           Region_Item.Node, Flags, Entries, Status,
           Bank_Register => Bank_Item.Node, Bank_Value => Bank_Value);
      end Execute_Declaration_Banked_Fields;
      procedure Execute_Declaration_Buffer_Field
        (Environment : in out Arena; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width; Source : AML_Execute.Datum; Bit_Offset, Bit_Length : AML_Decode.Integer_Value; Always_Buffer : Boolean; Status : out AML_Execute.Execution_Status) is
      begin
         if not Execute_Declaration_Lexical_Valid (Environment, Lease, Scope) then
            Status := AML_Execute.Unsupported_Value; return;
         end if;
         Define_Field_At_Entry (Environment, Scope, AML_Declaration_Contexts.Scope_Of (Lease), Path, Width, Source, Bit_Offset, Bit_Length, Always_Buffer, Status, AML_Declaration_Contexts.Is_Module (Lease));
      end Execute_Declaration_Buffer_Field;
      procedure Execute_Declaration_Name
        (Environment : in out Arena; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width; Data : AML_Decode.Bytes;
         Source : aliased AML_Byte_Sources.Storage; Source_First : Natural; Consumed : out Natural; Status : out AML_Execute.Execution_Status) is
      begin
         if not Execute_Declaration_Lexical_Valid (Environment, Lease, Scope) then
            Consumed := 0; Status := AML_Execute.Unsupported_Value; return;
         end if;
         Define_Name (Environment, Scope, AML_Declaration_Contexts.Scope_Of (Lease), Path, Width, Data, Source, Source_First, Consumed, Status,
           AML_Declaration_Contexts.Is_Module (Lease));
      end Execute_Declaration_Name;
      procedure Execute_Declaration_Reserve_Region
        (Environment : in out Arena; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result;
         Token : out AML_Declaration_Contexts.Region_Reservation; Status : out AML_Execute.Execution_Status) is
         Node : Natural;
      begin
         Token := AML_Declaration_Contexts.No_Region; Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Lexical_Valid (Environment, Lease, Scope) then return; end if;
         Reserve_Region_For_Context (Environment.Tree, Scope, AML_Declaration_Contexts.Scope_Of (Lease), Path, Node, Status, AML_Declaration_Contexts.Is_Module (Lease));
         if Status in AML_Execute.Returned and then Node /= 0 then
            Token := AML_Declaration_Contexts.Reserve_Region (Lease, Node, Natural (Environment.Tree.Items (Node_ID (Node)).Incarnation));
         end if;
      end Execute_Declaration_Reserve_Region;
      procedure Execute_Declaration_Complete_Region
        (Environment : in out Arena; Lease : AML_Declaration_Contexts.Lease; Input : aliased AML_Table_Backing.State;
         Token : AML_Declaration_Contexts.Region_Reservation; Width : AML_Decode.Integer_Width;
         Signature, OEM, Table_ID : AML_Execute.Datum; Status : out AML_Execute.Execution_Status) is
         Node : constant Natural := AML_Declaration_Contexts.Node_Of (Token);
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else not AML_Declaration_Contexts.Issued_By (Token, Lease) then return; end if;
         if Node > Environment.Tree.Used or else not Environment.Tree.Items (Node_ID (Node)).Alive
           or else AML_Declaration_Contexts.Stamp_Of (Token) /= Natural (Environment.Tree.Items (Node_ID (Node)).Incarnation)
         then return; end if;
         Complete (Environment, Input, Node, Width, Signature, OEM, Table_ID, Status);
      end Execute_Declaration_Complete_Region;
      procedure Execute_Reserve_Operation_Region
        (Environment : in out Arena; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Path : AML_Names.Name_Result; Space : AML_Decode.Byte;
         Token : in out AML_Declaration_Contexts.Operation_Region_Reservation;
         Status : in out AML_Execute.Execution_Status)
      is
         Node : Natural;
      begin
         Token := AML_Declaration_Contexts.No_Operation_Region;
         Status := AML_Execute.Unsupported_Value;
         -- Executable module declarations use their authenticated module frame.
         if not Execute_Declaration_Lexical_Valid (Environment, Lease, Scope)
         then return; end if;
         Reserve_Region_For_Context (Environment.Tree, Scope,
           AML_Declaration_Contexts.Scope_Of (Lease), Path, Node, Status,
           AML_Declaration_Contexts.Is_Module (Lease));
         if Status = AML_Execute.Returned and then Node /= 0 then
            if AML_Declaration_Contexts.Is_Module (Lease) then
               Environment.Tree.Items (Node_ID (Node)) :=
                 (Environment.Tree.Items (Node_ID (Node)) with delta
                    Initializing => True,
                    Initializer_Frame => AML_Declaration_Contexts.Frame_Of (Lease));
            end if;
            Environment.Tree.Items (Node_ID (Node)).Operation_Info :=
              (Space => Region_Space_ID (Space), others => <>);
            Token := AML_Declaration_Contexts.Reserve_Operation_Region
              (Lease, Node, Natural (Environment.Tree.Items (Node_ID (Node)).Incarnation));
         end if;
      end Execute_Reserve_Operation_Region;
      procedure Execute_Complete_Operation_Region
        (Environment : in out Arena; Lease : AML_Declaration_Contexts.Lease;
         Token : AML_Declaration_Contexts.Operation_Region_Reservation;
         Width : AML_Decode.Integer_Width; Address, Length : AML_Decode.Integer_Value;
         Status : in out AML_Execute.Execution_Status)
      is
         Node : constant Natural := AML_Declaration_Contexts.Node_Of (Token);
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease)
           or else not AML_Declaration_Contexts.Issued_By (Token, Lease)
           or else Node = 0 or else Node > Environment.Tree.Used
         then return; end if;
         if not Environment.Tree.Items (Node_ID (Node)).Alive
           or else Environment.Tree.Items (Node_ID (Node)).Object_Type /= Uninitialized_Region_Object
           or else Environment.Tree.Items (Node_ID (Node)).Owner /=
             Node_ID (AML_Declaration_Contexts.Scope_Of (Lease))
           or else Natural (Environment.Tree.Items (Node_ID (Node)).Incarnation) /=
             AML_Declaration_Contexts.Stamp_Of (Token)
         then return; end if;
         if AML_Declaration_Contexts.Is_Module (Lease)
           and then (not Environment.Tree.Items (Node_ID (Node)).Initializing
             or else Environment.Tree.Items (Node_ID (Node)).Initializer_Frame /=
               AML_Declaration_Contexts.Frame_Of (Lease)
             or else AML_Module_Frames.Top (Environment.Tree.Module_Frames) /= AML_Declaration_Contexts.Frame_Of (Lease))
         then return; end if;
         -- Atomic metadata publication: no pending module marker survives.
         Environment.Tree.Items (Node_ID (Node)) :=
           (Environment.Tree.Items (Node_ID (Node)) with delta
              Operation_Info =>
                (Environment.Tree.Items (Node_ID (Node)).Operation_Info with delta
                   Address => Address, Length => Length, Width => Width),
              Object_Type => Operation_Region_Object, Initializing => False,
              Initializer_Frame => AML_Frame_Handles.No_Frame);
         Status := AML_Execute.Returned;
      end Execute_Complete_Operation_Region;
      procedure Execute_Declaration_Reserve_Name
        (Environment : in out Arena; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result;
         Token : out Execute_Declaration_Name_Token; Status : out AML_Execute.Execution_Status) is
      begin
         Token := Execute_Declaration_No_Name; Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Lexical_Valid (Environment, Lease, Scope) then return; end if;
         Reserve_Name_For_Context (Environment, Node_ID (Scope), Node_ID (AML_Declaration_Contexts.Scope_Of (Lease)), Path, Token.Original, Status, AML_Declaration_Contexts.Is_Module (Lease));
         if Status in AML_Execute.Returned then Token.Owner := Lease; end if;
      end Execute_Declaration_Reserve_Name;
      procedure Execute_Declaration_Complete_Name
        (Environment : in out Arena; Lease : AML_Declaration_Contexts.Lease; Token : Execute_Declaration_Name_Token;
         Width : AML_Decode.Integer_Width; Initializer : AML_Decode.Bytes; Count : AML_Data.Count_Result;
         Status : out AML_Execute.Execution_Status) is
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else Token.Owner /= Lease then return; end if;
         Complete_Dynamic_Buffer (Environment, Token.Original, Width, Initializer, Count, Status);
      end Execute_Declaration_Complete_Name;
      procedure Execute_Declaration_Abort_Name
        (Environment : in out Arena; Lease : AML_Declaration_Contexts.Lease; Token : Execute_Declaration_Name_Token;
         Status : out AML_Execute.Execution_Status) is
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else Token.Owner /= Lease then return; end if;
         Abort_Dynamic_Name (Environment, Token.Original, Status);
      end Execute_Declaration_Abort_Name;
      -- One live builder can occupy each expression continuation in every
      -- admitted call. This is session-local authority, not a second arena.
      Builder_Record_Limit : constant Positive :=
        AML_Execute.Max_Expression_Depth * (AML_Execute.Call_Budget'Last + 1);
      subtype Builder_Position is Natural range 0 .. Builder_Record_Limit;
      subtype Builder_Stamp is Natural;
      type Runtime_Package_Token is record
         Position : Builder_Position := 0;
         Stamp : Builder_Stamp := 0;
      end record;
      No_Runtime_Package : constant Runtime_Package_Token := (others => <>);
      type Runtime_Package_Record is record
         Live : Boolean := False;
         Group : Builder_Stamp := 0;
         Parent : Builder_Position := 0;
         Stamp : Builder_Stamp := 0;
         Lease : AML_Declaration_Contexts.Lease := AML_Declaration_Contexts.No_Lease;
         Source : AML_References.Object_Handle := AML_References.No_Object_Handle;
         Consumed_Elements : Natural := 0;
         Child_Byte_Bound : Natural := 0;
         Journal_Start : Pending.Member_Count := 0;
      end record;
      Builders : array (Positive range 1 .. Builder_Record_Limit)
        of Runtime_Package_Record := [others => <>];
      Last_Builder_Stamp : Builder_Stamp := 0;
      -- One journal for the entire owner session, not one per expression.
      -- Nested method constructions append a suffix and remove only that suffix.
      use type Pending.Append_Status;
      Builder_Journal : Pending.State := Pending.Empty;
      type Pending_Authority is record
         Destination : AML_References.Object_Handle := AML_References.No_Object_Handle;
         Scope_Stamp : Natural := 0;
      end record;
      Pending_Authorities : array (Positive range 1 .. Max_Pending_Members)
        of Pending_Authority := [others => <>];
      subtype Builder_Group_Depth is Natural range 0 .. AML_Execute.Call_Budget'Last + 1;
      type Builder_Group is record
         Identity : Builder_Stamp := 0;
         Root_Position : Builder_Position := 0;
         Checkpoint : Pending.Member_Count := 0;
         Name : Execute_Declaration_Name_Token := Execute_Declaration_No_Name;
         Width : AML_Decode.Integer_Width := AML_Decode.Bits_64;
      end record;
      Builder_Groups : array (Positive range 1 .. Builder_Group_Depth'Last)
        of Builder_Group := [others => <>];
      Group_Depth : Builder_Group_Depth := 0;
      procedure Restore_Builder_Journal (Checkpoint : Pending.Member_Count) is
         Prefix : Pending.State := Pending.Empty;
         Added : Pending.Append_Status;
      begin
         for I in 1 .. Checkpoint loop
            declare M : constant Pending.Member_Result := Pending.Item (Builder_Journal, I); begin
               pragma Assert (M.Found);
               Pending.Append (Prefix, M.Package_ID, M.Element, M.Scope, M.Path, Added);
               pragma Assert (Added = Pending.Appended);
            end;
         end loop;
         Builder_Journal := Prefix;
      end Restore_Builder_Journal;
      type Completed_Child_State is record
         Parent : Builder_Position := 0;
         Source : AML_References.Object_Handle := AML_References.No_Object_Handle;
         Group : Builder_Stamp := 0;
         Checkpoint : Pending.Member_Count := 0;
      end record;
      Completed_Child : Completed_Child_State := (others => <>);
      function Builder_Matches
        (E : Arena; Lease : AML_Declaration_Contexts.Lease;
         Token : Runtime_Package_Token) return Boolean is
        (Execute_Declaration_Valid (E, Lease)
         and then Group_Depth > 0
         and then Token.Position /= 0 and then Token.Stamp /= 0
         and then Builders (Token.Position).Live
         and then Builders (Token.Position).Group = Builder_Groups (Group_Depth).Identity
         and then Builders (Token.Position).Stamp = Token.Stamp
         and then Builders (Token.Position).Lease = Lease
         and then Has_Source (E, Builders (Token.Position).Source)
         and then AML_Objects.Kind (E.Tree.Values,
           AML_References.Source (Builders (Token.Position).Source)) = AML_Objects.Package_Object
         and then Builders (Token.Position).Consumed_Elements <= Builders (Token.Position).Child_Byte_Bound);
      procedure Begin_Runtime_Package
        (E : in out Arena; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Count, Child_Byte_Bound : Natural;
         Parent : Runtime_Package_Token; Name : Execute_Declaration_Name_Token;
         Width : AML_Decode.Integer_Width; Token : out Runtime_Package_Token;
         Binding : out AML_Execute.Binding_Result)
      is
         Position : Builder_Position := 0;
         Gate : Allocation_Hook_Status;
         ID : AML_Objects.Object_ID;
         Allocated : AML_Objects.Allocation_Status;
      begin
         Token := No_Runtime_Package;
         Binding := (AML_Execute.Failed_Binding, AML_Execute.Unsupported_Value);
         if not Execute_Declaration_Lexical_Valid (E, Lease, Scope) then return; end if;
         if Completed_Child.Group /= 0 then return; end if;
         if Parent /= No_Runtime_Package then
            if not Builder_Matches (E, Lease, Parent)
              or else Name /= Execute_Declaration_No_Name then return; end if;
         elsif Group_Depth = Builder_Group_Depth'Last then
            Binding := (AML_Execute.Failed_Binding, AML_Execute.Call_Limit); return;
         elsif Name /= Execute_Declaration_No_Name and then
           (Name.Owner /= Lease or else not Reservation_Matches (E, Name.Original)) then return;
         end if;
         if Count > AML_Objects.Max_Elements or else Last_Builder_Stamp = Builder_Stamp'Last then
            Binding := (AML_Execute.Failed_Binding, AML_Execute.Value_Limit); return;
         end if;
         for I in Builders'Range loop
            if not Builders (I).Live then Position := I; exit; end if;
         end loop;
         if Position = 0 then
            Binding := (AML_Execute.Failed_Binding, AML_Execute.Expression_Limit); return;
         end if;
         -- Caller publishes all suspended parents before this boundary.
         Before_Allocation (E, [], Gate);
         if Gate /= Allocation_Allowed
           or else not Execute_Declaration_Lexical_Valid (E, Lease, Scope) then return; end if;
         if Parent /= No_Runtime_Package and then not Builder_Matches (E, Lease, Parent) then return; end if;
         if Name /= Execute_Declaration_No_Name and then
           (Name.Owner /= Lease or else not Reservation_Matches (E, Name.Original)) then return; end if;
         AML_Objects.New_Package (E.Tree.Values, Count, ID, Allocated);
         if Allocated /= AML_Objects.Allocated then
            Binding := (AML_Execute.Failed_Binding, AML_Execute.Value_Limit); return;
         end if;
         Last_Builder_Stamp := Last_Builder_Stamp + 1;
         if Parent = No_Runtime_Package then
            Group_Depth := Group_Depth + 1;
            Builder_Groups (Group_Depth) :=
              (Identity => Last_Builder_Stamp, Root_Position => Position,
               Checkpoint => Pending.Count (Builder_Journal), Name => Name, Width => Width);
         end if;
         Builders (Position) :=
           (Live => True, Group => Builder_Groups (Group_Depth).Identity, Parent => Parent.Position,
            Stamp => Last_Builder_Stamp, Lease => Lease,
            Source => AML_References.Bind_Object (E.Token,
              AML_Objects.Address_Of (E.Tree.Values, ID)), Consumed_Elements => 0,
            Child_Byte_Bound => Child_Byte_Bound,
            Journal_Start => Pending.Count (Builder_Journal));
         Token := (Position, Last_Builder_Stamp);
         Binding := (AML_Execute.Non_Integer_Binding,
           (Source => Builders (Position).Source, ID => ID,
            Type_Code => 4, Size => Count, others => <>));
         -- No allocating callback occurs between this allocation and return.
         -- Execute must publish Binding before evaluating the next child.
      end Begin_Runtime_Package;
      procedure Append_Runtime_Package
        (E : in out Arena; Lease : AML_Declaration_Contexts.Lease;
         Token : in out Runtime_Package_Token; Item : AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status)
      is
         Gate : Allocation_Hook_Status;
         Parent, Child : AML_Objects.Object_ID;
         Allocated : AML_Objects.Allocation_Status;
         Root_Value : AML_Execute.Datum;
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Builder_Matches (E, Lease, Token) then return; end if;
         Parent := AML_References.Source (Builders (Token.Position).Source);
         if Last_Builder_Stamp = Builder_Stamp'Last
           or else Builders (Token.Position).Consumed_Elements = Builders (Token.Position).Child_Byte_Bound then
            Status := AML_Execute.Value_Limit; return;
         end if;
         -- Every supplied object must authenticate even when the element is
         -- discarded. Child_Byte_Bound is the already validated initializer
         -- window length; every parsed child consumes at least one byte.
         case Item.Value_Kind is
            when AML_Execute.Integer_Datum => null;
            when AML_Execute.Object_Datum =>
               if not Has_Source (E, Item.Object.Source)
                 or else AML_References.Source (Item.Object.Source) /= Item.Object.ID then return; end if;
               Child := AML_References.Source (Item.Object.Source);
               if AML_Objects.Kind (E.Tree.Values, Child) not in
                 AML_Objects.String_Object | AML_Objects.Buffer_Object | AML_Objects.Package_Object
                 or else Item.Object.Size /= AML_Objects.Length (E.Tree.Values, Child)
                 or else Item.Object.Type_Code /=
                   (case AML_Objects.Kind (E.Tree.Values, Child) is
                     when AML_Objects.String_Object => 2,
                     when AML_Objects.Buffer_Object => 3,
                     when AML_Objects.Package_Object => 4,
                     when others => 0) then return; end if;
            when AML_Execute.Reference_Datum | AML_Execute.Metadata_Datum => return;
         end case;
         if Completed_Child.Group /= 0 and then
           (Completed_Child.Parent /= Token.Position
             or else Completed_Child.Group /= Builders (Token.Position).Group
             or else Item.Value_Kind /= AML_Execute.Object_Datum
             or else Item.Object.Source /= Completed_Child.Source) then return; end if;
         -- VarPackage evaluates excess initializers for their effects, but does
         -- not store them. This callback is reached only AFTER child evaluation.
         -- The source frame retains Item until this nonallocating discard ends.
         if Builders (Token.Position).Consumed_Elements >= AML_Objects.Length (E.Tree.Values, Parent) then
            if Completed_Child.Group /= 0 then
               -- This completed nested subtree is not attached anywhere. Its
               -- deferred name records cannot become roots or later failures.
               Restore_Builder_Journal (Completed_Child.Checkpoint);
               Completed_Child := (others => <>);
            end if;
            Last_Builder_Stamp := Last_Builder_Stamp + 1;
            Builders (Token.Position).Consumed_Elements := Builders (Token.Position).Consumed_Elements + 1;
            Builders (Token.Position).Stamp := Last_Builder_Stamp;
            Token.Stamp := Last_Builder_Stamp;
            Status := AML_Execute.Returned;
            return;
         end if;
         case Item.Value_Kind is
            when AML_Execute.Integer_Datum =>
               Root_Value := (AML_Execute.Object_Datum,
                 (Source => Builders (Token.Position).Source, ID => Parent,
                  Type_Code => 4, Size => AML_Objects.Length (E.Tree.Values, Parent), others => <>));
               Before_Allocation (E, [Root_Value, Item], Gate);
               if Gate /= Allocation_Allowed or else not Builder_Matches (E, Lease, Token) then return; end if;
               AML_Objects.New_Integer (E.Tree.Values, Item.Number, Child, Allocated, Item.Origin);
               if Allocated /= AML_Objects.Allocated then Status := AML_Execute.Value_Limit; return; end if;
            when AML_Execute.Object_Datum =>
               if not Has_Source (E, Item.Object.Source)
                 or else AML_References.Source (Item.Object.Source) /= Item.Object.ID then return; end if;
               Child := AML_References.Source (Item.Object.Source);
               if AML_Objects.Kind (E.Tree.Values, Child) not in
                 AML_Objects.String_Object | AML_Objects.Buffer_Object | AML_Objects.Package_Object then return; end if;
            when AML_Execute.Reference_Datum | AML_Execute.Metadata_Datum =>
               -- Dedicated deferred-member handling is a separate operation.
               return;
         end case;
         AML_Objects.Set_Element (E.Tree.Values, Parent,
           Builders (Token.Position).Consumed_Elements, Child);
         Completed_Child := (others => <>);
         Last_Builder_Stamp := Last_Builder_Stamp + 1;
         Builders (Token.Position).Consumed_Elements := Builders (Token.Position).Consumed_Elements + 1;
         Builders (Token.Position).Stamp := Last_Builder_Stamp;
         Token.Stamp := Last_Builder_Stamp;
         Status := AML_Execute.Returned;
      end Append_Runtime_Package;
      procedure Defer_Runtime_Package_Name
        (E : Arena; Lease : AML_Declaration_Contexts.Lease;
         Token : in out Runtime_Package_Token; Scope : Natural;
         Path : AML_Names.Name_Result; Status : out AML_Execute.Execution_Status)
      is
         Parent : AML_Objects.Object_ID;
         Added : Pending.Append_Status;
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Builder_Matches (E, Lease, Token)
           or else not Execute_Declaration_Lexical_Valid (E, Lease, Scope)
           or else not Pending.Valid_Path (Path) then return; end if;
         if Last_Builder_Stamp = Builder_Stamp'Last
           or else Builders (Token.Position).Consumed_Elements = Builders (Token.Position).Child_Byte_Bound then
            Status := AML_Execute.Value_Limit; return;
         end if;
         if Completed_Child.Group /= 0 then return; end if;
         Parent := AML_References.Source (Builders (Token.Position).Source);
         if Builders (Token.Position).Consumed_Elements < AML_Objects.Length (E.Tree.Values, Parent) then
            Pending.Append (Builder_Journal, Parent, Builders (Token.Position).Consumed_Elements,
              Node_ID (Scope), Path, Added);
            if Added /= Pending.Appended then Status := AML_Execute.Value_Limit; return; end if;
            Pending_Authorities (Pending.Count (Builder_Journal)) :=
              (Destination => Builders (Token.Position).Source,
               Scope_Stamp => (if Scope = Root then 0 else Natural (E.Tree.Items (Node_ID (Scope)).Incarnation)));
         end if;
         Last_Builder_Stamp := Last_Builder_Stamp + 1;
         Builders (Token.Position).Consumed_Elements := Builders (Token.Position).Consumed_Elements + 1;
         Builders (Token.Position).Stamp := Last_Builder_Stamp;
         Token.Stamp := Last_Builder_Stamp;
         Status := AML_Execute.Returned;
      end Defer_Runtime_Package_Name;
      procedure Bind_Live_Builder_Members
        (E : Arena; Candidate : in out State; First : Natural;
         Owner : AML_Identity.Identity;
         Width : AML_Decode.Integer_Width; Self_Node : Node_ID;
         Allow_Reference : Boolean; Status : out AML_Execute.Execution_Status)
      is
      begin
         Status := AML_Execute.Unsupported_Value;
         for I in First .. Pending.Count (Builder_Journal) loop
            declare
               Member : constant Pending.Member_Result := Pending.Item (Builder_Journal, I);
               Located : Lookup_Result;
               Leaf : AML_Objects.Object_ID;
               Allocated : AML_Objects.Allocation_Status;
               Extracted : Field_Result;
            begin
               if not Member.Found or else Member.Scope > Candidate.Used
                 or else not Has_Source (E, Pending_Authorities (I).Destination)
                 or else AML_References.Source (Pending_Authorities (I).Destination) /= Member.Package_ID
                 or else (if Member.Scope /= Root then
                   not Candidate.Items (Member.Scope).Alive or else
                   Natural (Candidate.Items (Member.Scope).Incarnation) /= Pending_Authorities (I).Scope_Stamp)
                 or else not AML_Objects.Is_Live (Candidate.Values, Member.Package_ID)
                 or else AML_Objects.Kind (Candidate.Values, Member.Package_ID) /= AML_Objects.Package_Object
                 or else Member.Element >= AML_Objects.Length (Candidate.Values, Member.Package_ID)
               then return; end if;
               Located := Resolve_Object (Candidate, Member.Scope, Member.Path);
               if Located.Status /= Found then return; end if;
               if Located.Node = Root or else Kind (Candidate, Located.Node) = Metadata_Value_Object
                    or else Package_Metadata_Kind (Kind (Candidate, Located.Node)) then
                  if Owner = AML_Identity.No_Identity then return; end if;
                  AML_Objects.New_Metadata (Candidate.Values,
                    Metadata_Identity_At (Candidate, Owner, Located.Node), Leaf, Allocated);
                  if Allocated /= AML_Objects.Allocated then Status := AML_Execute.Value_Limit; return; end if;
               elsif Self_Node /= Root and then Located.Node = Self_Node then
                  if Owner = AML_Identity.No_Identity then return; end if;
                  -- The runtime self path defers NAME resolution: the node is
                  -- resolved later, but no object was captured at NAME creation.
                  AML_Objects.New_Reference (Candidate.Values,
                    AML_References.Bind_Name_Member (Owner, AML_References.Node_Position (Self_Node),
                      Candidate.Items (Self_Node).Incarnation,
                      AML_References.No_Object_Handle), Leaf, Allocated);
                  if Allocated /= AML_Objects.Allocated then
                     Status := AML_Execute.Value_Limit; return;
                  end if;
               elsif Candidate.Items (Located.Node).Object_Type = Buffer_Field_Object then
                  Extract_Buffer_Field (Candidate, Owner, Located.Node, Width, Extracted, Status);
                  if Status /= AML_Execute.Returned then return; end if;
                  -- Restore the failure default before processing later members.
                  Status := AML_Execute.Unsupported_Value;
                  case Extracted.Kind is
                     when Scalar_Field =>
                        AML_Objects.New_Integer (Candidate.Values, Extracted.Number, Leaf, Allocated);
                        if Allocated /= AML_Objects.Allocated then
                           Status := AML_Execute.Value_Limit; return;
                        end if;
                     when Buffer_Field => Leaf := Extracted.Object;
                  end case;
               elsif Candidate.Items (Located.Node).Object_Type in
                 Integer_Object | String_Object | Buffer_Object | Package_Object
                 or else (Allow_Reference and then
                   Candidate.Items (Located.Node).Object_Type = Reference_Object)
               then Leaf := Candidate.Items (Located.Node).Object_Ref;
               else return;
               end if;
               AML_Objects.Set_Element (Candidate.Values, Member.Package_ID, Member.Element, Leaf);
            end;
         end loop;
         Status := AML_Execute.Returned;
      end Bind_Live_Builder_Members;
      procedure Finish_Runtime_Package
        (E : in out Arena; Lease : AML_Declaration_Contexts.Lease;
         Token : in out Runtime_Package_Token; Binding : out AML_Execute.Binding_Result)
      is
         Gate : Allocation_Hook_Status;
         ID : AML_Objects.Object_ID;
         Root_Value : AML_Execute.Datum;
         Status : AML_Execute.Execution_Status;
         Added : Pending.Append_Status;
      begin
         Binding := (AML_Execute.Failed_Binding, AML_Execute.Unsupported_Value);
         if not Builder_Matches (E, Lease, Token) then return; end if;
         if Completed_Child.Group /= 0 then return; end if;
         -- Finish never orphans a live child. A completed child has already
         -- retired its record and must be attached/discarded by its parent.
         for I in Builders'Range loop
            if Builders (I).Live and then Builders (I).Group = Builders (Token.Position).Group
              and then (Builders (I).Parent = Token.Position
                or else (Token.Position = Builder_Groups (Group_Depth).Root_Position
                  and then I /= Token.Position)) then return; end if;
         end loop;
         ID := AML_References.Source (Builders (Token.Position).Source);
         Root_Value := (AML_Execute.Object_Datum,
           (Source => Builders (Token.Position).Source, ID => ID, Type_Code => 4,
            Size => AML_Objects.Length (E.Tree.Values, ID), others => <>));
         if Token.Position = Builder_Groups (Group_Depth).Root_Position then
            Before_Allocation (E, [Root_Value], Gate);
            if Gate /= Allocation_Allowed or else not Builder_Matches (E, Lease, Token) then return; end if;
            declare
               Group : constant Builder_Group := Builder_Groups (Group_Depth);
               Candidate : State := E.Tree; -- CURRENT, after all effects and collection.
               Self_Node : Node_ID := Root;
               Replaced_Name : Boolean := False;
            begin
               if Group.Name /= Execute_Declaration_No_Name then
                  if Group.Name.Owner /= Lease or else not Reservation_Matches (E, Group.Name.Original) then return; end if;
                  Self_Node := Node_ID (AML_References.Named_Node (Group.Name.Original.Ref));
                  Replaced_Name := Candidate.Items (Self_Node).Object_Type /= Uninitialized_Name_Object;
                  if Replaced_Name then
                     -- Count evaluation may have replaced the reserved name.
                     -- Complete its initialization without overwriting that value.
                     Candidate.Items (Self_Node).Initializing := False;
                  else
                     Candidate.Items (Self_Node) := (Candidate.Items (Self_Node) with delta
                       Object_Type => Package_Object, Object_Ref => ID, Initializing => False);
                  end if;
               end if;
               if Replaced_Name then
                  -- The constructed subtree is discarded. Its deferred members
                  -- must not enter the module journal or retain destination IDs.
                  -- Executed count effects and the replacement stay committed.
                  null;
               elsif AML_Declaration_Contexts.Is_Module (Lease) and then Self_Node /= Root then
                  for I in Group.Checkpoint + 1 .. Pending.Count (Builder_Journal) loop
                     declare M : constant Pending.Member_Result := Pending.Item (Builder_Journal, I); begin
                        if not M.Found or else not Has_Source (E, Pending_Authorities (I).Destination)
                          or else AML_References.Source (Pending_Authorities (I).Destination) /= M.Package_ID then return; end if;
                        Pending.Append (Candidate.Journal, M.Package_ID, M.Element, M.Scope, M.Path, Added);
                        if Added /= Pending.Appended then Binding := (AML_Execute.Failed_Binding, AML_Execute.Value_Limit); return; end if;
                     end;
                  end loop;
               else
                  Bind_Live_Builder_Members (E, Candidate, Group.Checkpoint + 1,
                    E.Token, Group.Width, Self_Node, Self_Node = Root, Status);
                  if Status /= AML_Execute.Returned then Binding := (AML_Execute.Failed_Binding, Status); return; end if;
               end if;
               E.Tree := Candidate;
               Restore_Builder_Journal (Group.Checkpoint);
               Builder_Groups (Group_Depth) := (others => <>);
               Group_Depth := Group_Depth - 1;
            end;
         else
            Completed_Child :=
              (Parent => Builders (Token.Position).Parent,
               Source => Builders (Token.Position).Source,
               Group => Builders (Token.Position).Group,
               Checkpoint => Builders (Token.Position).Journal_Start);
         end if;
         Binding := (AML_Execute.Non_Integer_Binding, Root_Value.Object);
         Builders (Token.Position) := (others => <>);
         Token := No_Runtime_Package;
      end Finish_Runtime_Package;
      procedure Abort_Runtime_Package
        (E : Arena; Lease : AML_Declaration_Contexts.Lease;
         Token : in out Runtime_Package_Token; Status : out AML_Execute.Execution_Status) is
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Builder_Matches (E, Lease, Token) then return; end if;
         -- Do not roll back the live arena or rewind the monotonic issuer.
         if Token.Position = Builder_Groups (Group_Depth).Root_Position then
            for I in Builders'Range loop
               if Builders (I).Live and then I /= Token.Position
                 and then Builders (I).Group = Builders (Token.Position).Group then
                  Builders (I) := (others => <>);
               end if;
            end loop;
            if Completed_Child.Group = Builders (Token.Position).Group then
               Completed_Child := (others => <>);
            end if;
            Restore_Builder_Journal (Builder_Groups (Group_Depth).Checkpoint);
            Builder_Groups (Group_Depth) := (others => <>);
            Group_Depth := Group_Depth - 1;
         end if;
         Builders (Token.Position) := (others => <>);
         Token := No_Runtime_Package;
         Status := AML_Execute.Returned;
      end Abort_Runtime_Package;
      procedure Abort_Runtime_Packages
        (E : in out Arena; Lease : AML_Declaration_Contexts.Lease;
         Status : out AML_Execute.Execution_Status)
      is
         pragma Unmodified (E);
         Token : Runtime_Package_Token;
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (E, Lease) then return; end if;
         while Group_Depth > 0 loop
            declare Position : constant Builder_Position := Builder_Groups (Group_Depth).Root_Position; begin
               exit when Builders (Position).Lease /= Lease;
               Token := (Position, Builders (Position).Stamp);
               Abort_Runtime_Package (E, Lease, Token, Status);
               if Status /= AML_Execute.Returned then return; end if;
            end;
         end loop;
         Status := AML_Execute.Returned;
      end Abort_Runtime_Packages;
      procedure Provenance_Execute_Resolve
        (Environment : in out Arena; Ref : AML_References.Reference;
         Width : AML_Decode.Integer_Width; Value : out AML_Execute.Datum;
         Provenance : out AML_Execute.Scalar_Provenance;
         Status : out AML_Execute.Execution_Status) is
      begin
         Provenance := AML_Execute.Detached_Provenance;
         Dereference_With_Provenance (Environment, Ref, Width, Value, Provenance, Status);
      end Provenance_Execute_Resolve;
      procedure Provenance_Execute_Read_Integer
        (Environment : Arena; Source : AML_References.Object_Handle;
         Value : out AML_Decode.Integer_Value; Origin : out AML_Decode.Integer_Origin;
         Status : out AML_Execute.Execution_Status) is
      begin
         Read_Integer_Source (Environment, Source, Value, Origin, Status);
      end Provenance_Execute_Read_Integer;
      procedure Prepare_Authenticated_Load
        (E : in out Arena; Input : aliased AML_Table_Backing.State;
         Lease : AML_Declaration_Contexts.Lease; Scope : Natural;
         Width : AML_Decode.Integer_Width; Operands : AML_Execute.Load_Table_Operands;
         Budget : Natural; Charged : in out Natural; Succeeded : in out Boolean;
         Status : in out AML_Execute.Execution_Status; Plan : in out AML_Execute.Load_Table_Plan) is
      begin
         if not Nested_Loads_Enabled or else not Execute_Declaration_Lexical_Valid (E, Lease, Scope) then
            Status := AML_Execute.Unsupported_Value; return;
         end if;
         Prepare_Load (E, Input, Lease, Scope, Width, Operands, Budget, Charged, Succeeded, Status, Plan);
      end Prepare_Authenticated_Load;
      procedure Prepare_Authenticated_Source
        (E : in out Arena; Input : aliased AML_Table_Backing.State;
         Lease : AML_Declaration_Contexts.Lease; Scope : Natural;
         Width : AML_Decode.Integer_Width; Source : AML_Execute.Datum;
         Budget : Natural; Charged : in out Natural; Succeeded : in out Boolean;
         Status : in out AML_Execute.Execution_Status; Plan : in out AML_Execute.Load_Table_Plan) is
         Value : AML_Execute.Datum;
      begin
         if not Nested_Loads_Enabled or else not Execute_Declaration_Lexical_Valid (E, Lease, Scope)
           or else Source.Value_Kind /= AML_Execute.Reference_Datum
         then Status := AML_Execute.Unsupported_Value; return; end if;
         if Buffer_Field_Matches (E, Source.Ref) or else Table_Field_Matches (E, Source.Ref) then
            -- Keep the owned field's identity; capture validates its backing
            -- and reads declared logical bytes without a second heap Buffer.
            Value := Source; Status := AML_Execute.Returned;
         else Resolve_Semantic_Reference (E, Source.Ref, Value, Status); end if;
         if Status /= AML_Execute.Returned then return; end if;
         Prepare_Source (E, Input, Lease, Scope, Width, Value, Budget, Charged, Succeeded, Status, Plan);
      end Prepare_Authenticated_Source;
      procedure Execute is new AML_Execute.Execute_From_Source
        (Arena,
         AML_Table_Backing.State,
         Context_Valid,
         Lookup,
         Method,
         Write,
         Execute_Declaration_Enter,
         Execute_Declaration_Leave,
         Execute_Declaration_Method,
         Execute_Declaration_Fields,
         Literal,
         Execute_Declaration_Reserve_Region,
         Execute_Declaration_Complete_Region,
         Timer,
         Provenance_Execute_Resolve,
         Provenance_Execute_Read_Integer,
         Normalize_Integer_Source,
         Owner_Census_Begin, Owner_Census_Add, Owner_Census_Finish,
         Execute_Declaration_Buffer_Field,
         Index,
         Store_Reference,
         Clone_At_Entry,
         Compare_Objects,
         Refresh_Value,
         Begin_Root_Invocation,
         Execute_Declaration_Name,
         Copy_At_Entry,
         Describe_Named_Identity,
         Execute_Declaration_Name_Token,
         Execute_Declaration_No_Name,
         Execute_Declaration_Reserve_Name,
         Execute_Declaration_Complete_Name,
         Execute_Declaration_Abort_Name,
         Disabled_Debug,
         Convert_To_Integer,
         Concatenate_At_Entry,
         To_Buffer_At_Entry,
         Mid_At_Entry,
         To_String_At_Entry,
         Format_String_At_Entry,
         Match_Package,
         Resources_At_Entry,
         Handoff_Result => Handoff,
         Publish_Frame_Root => Publish_Root,
         Frame_Root_Capacity => Root_Capacity,
         Open_Frame_Root => Open_Root,
         Close_Frame_Root => Close_Root,
         Publish_Held_Root => Publish_Held,
         Publish_Expression_Roots => Publish_Expression,
         Wait_For_Delay => Delay_Adapter_2,
         Queue_Notify => Queue_Notify,
         Describe_Metadata_Value => Describe_Metadata_Value,
         Materialize_Buffer => Buffer_Literal,
         Define_Alias => Execute_Alias,
         Enter_Container => Execute_Container_Enter,
         Leave_Container => Execute_Container_Leave,
         Evaluate_Internal_Method => Evaluate_Internal,
         Define_Event => Execute_Declaration_Event,
         Define_Mutex => Execute_Declaration_Mutex,
         Define_Indexed_Fields => Execute_Declaration_Indexed_Fields,
         Define_Banked_Fields => Execute_Declaration_Banked_Fields,
         Reserve_Operation_Region => Execute_Reserve_Operation_Region,
         Complete_Operation_Region => Execute_Complete_Operation_Region,
         Enter_Serialized_Method => Enter_Serialized,
         Leave_Serialized_Method => Leave_Serialized,
         Operate_Event => Execute_Event,
         Operate_Mutex => Execute_Mutex,
         Require_Invocation_Authority => True,
         Block_Capacity => Block_Capacity,
         Runtime_Package_Token => Runtime_Package_Token,
         No_Runtime_Package => No_Runtime_Package,
         Begin_Runtime_Package => Begin_Runtime_Package,
         Append_Runtime_Package => Append_Runtime_Package,
         Defer_Runtime_Package_Name => Defer_Runtime_Package_Name,
         Finish_Runtime_Package => Finish_Runtime_Package,
         Abort_Runtime_Packages => Abort_Runtime_Packages,
         Runtime_Package_Supported => True,
         Allow_Restricted_Table_Loads => Restricted_Table_Loads_Enabled,
         Request_Load_Table => Prepare_Authenticated_Load,
         Request_Load_Source => Prepare_Authenticated_Source,
         Copy_Load_Table_Body => Copy_Load,
         Begin_Load_Table_Module => Begin_Load,
         Finish_Load_Table => Finish_Load,
         Handle_Module_Divide_Error => Recover_Module_Divide);
   begin
      -- Unknown external completion survives tree rollback and Reset. Even
      -- the default rejecting-provider entry cannot restart AML implicitly.
      if A.Field_Uncertain.Present then
         Result := (AML_Execute.Field_Indeterminate, 0); return;
      end if;
      Execute (Code, Width, Args, Argument_Count, Budget, Input, A,
        Natural (Scope), Result, Source, Source_First, Current_Sync => Current_Sync, Request => Session_Request, Implementation => Implementation);
      if AML_Frame_Handles.Has_Authority (Admitted_Domain)
        and then Result.Status = AML_Execute.Fatal_Requested
      then A.Last_Fatal := (Present => True, Item => Result.Fatal); end if;
      -- Explicit acquisitions belong to the outer invocation, never a child
      -- method. No allocation or user callback occurs during this final drain.
      if AML_Frame_Handles.Has_Authority (Admitted_Domain) then
         declare
            Outcome : Mutex_Registry.Outcome;
            use type Mutex_Registry.Outcome;
         begin
            Mutex_Registry.End_Invocation (A.Mutexes, Mutex_Admission_For (A),
              Admitted_Domain, Outcome);
            if Outcome /= Mutex_Registry.Succeeded then
               raise Program_Error with "mutex invocation close invariant";
            end if;
         end;
      end if;
   end Run_Context_Session;

   generic
      with procedure Handoff (E : in out Arena; Result : AML_Execute.Execution_Result);
      with procedure Before_Allocation (E : in out Arena; Extra : Root_Values;
         Status : out Allocation_Hook_Status) is Skip_Collection;
   procedure Run_Owned_Session (A : in out Arena; Input : aliased AML_Table_Backing.State;
     Code : AML_Decode.Bytes; Source : aliased AML_Byte_Sources.Storage; Source_First : Natural; Width : AML_Decode.Integer_Width; Scope : Node_ID;
     Args : AML_Execute.Value_Arguments; Argument_Count : Natural;
     Budget : Natural; Current_Sync : AML_Execute.Sync_Level;
     Result : out AML_Execute.Execution_Result;
     Session_Request : AML_Declaration_Contexts.Request := AML_Declaration_Contexts.Method_Request;
     Implementation : AML_Execute.Method_Implementation := AML_Execute.AML_Implementation);
   procedure Run_Owned_Session (A : in out Arena; Input : aliased AML_Table_Backing.State;
     Code : AML_Decode.Bytes; Source : aliased AML_Byte_Sources.Storage; Source_First : Natural; Width : AML_Decode.Integer_Width; Scope : Node_ID;
     Args : AML_Execute.Value_Arguments; Argument_Count : Natural;
     Budget : Natural; Current_Sync : AML_Execute.Sync_Level;
     Result : out AML_Execute.Execution_Result;
     Session_Request : AML_Declaration_Contexts.Request := AML_Declaration_Contexts.Method_Request;
     Implementation : AML_Execute.Method_Implementation := AML_Execute.AML_Implementation) is
      IO : Null_Field_Context;
      procedure Run is new Run_Context_Session (Null_Field_Context, Reject_Field_Operation, Handoff, Before_Allocation);
   begin
      Run (A, IO, Input, Code, Source, Source_First, Width, Scope, Args, Argument_Count, Budget, Current_Sync, Result, Session_Request, Implementation);
   end Run_Owned_Session;

   generic
      type IO_Context is limited private;
      with procedure Field_Operation
        (E : in out Arena; IO : in out IO_Context; Ref : Reference;
         Width : AML_Decode.Integer_Width; For_Write : Boolean;
         Value : in out AML_Execute.Datum; Status : out AML_Execute.Execution_Status);
      with procedure Handoff (E : in out Arena; Result : AML_Execute.Execution_Result);
      with procedure Before_Allocation (E : in out Arena; Extra : Root_Values;
         Status : out Allocation_Hook_Status) is Skip_Collection;
   procedure Invoke_Context_With_Handoff (A : in out Arena; IO : in out IO_Context; Input : aliased AML_Table_Backing.State;
     Node : Node_ID; Args : AML_Execute.Value_Arguments; Argument_Count : Natural;
     Budget : Natural; Result : out AML_Execute.Execution_Result);
   procedure Invoke_Context_With_Handoff (A : in out Arena; IO : in out IO_Context; Input : aliased AML_Table_Backing.State;
     Node : Node_ID; Args : AML_Execute.Value_Arguments; Argument_Count : Natural;
     Budget : Natural; Result : out AML_Execute.Execution_Result) is
      procedure Run is new Run_Context_Session (IO_Context, Field_Operation, Handoff, Before_Allocation);
      Definition : constant AML_Execute.Method_Definition := Read_Method (A.Tree, Node);
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
      Run (A, IO, Input,
        (if Definition.Implementation = AML_Execute.AML_Implementation then Definition.Code.Bytes
         else AML_Decode.Bytes'(1 .. 0 => 0)), Definition.Code, 0, Definition.Width, Node, Args,
        Argument_Count, Budget, 0, Result,
        Implementation => Definition.Implementation);
   end Invoke_Context_With_Handoff;

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
      IO : Null_Field_Context;
      procedure Run is new Invoke_Context_With_Handoff (Null_Field_Context, Reject_Field_Operation, Handoff, Before_Allocation);
   begin
      Run (A, IO, Input, Node, Args, Argument_Count, Budget, Result);
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
      type IO_Context is limited private;
      with procedure Field_Operation
        (E : in out Arena; IO : in out IO_Context; Ref : Reference;
         Width : AML_Decode.Integer_Width; For_Write : Boolean;
         Value : in out AML_Execute.Datum; Status : out AML_Execute.Execution_Status);
      with procedure Handoff (E : in out Arena; Result : AML_Execute.Execution_Result);
      with procedure Before_Allocation (E : in out Arena; Extra : Root_Values;
         Status : out Allocation_Hook_Status) is Skip_Collection;
   procedure Run_Registered_Context_Session
     (A : in out Arena; IO : in out IO_Context; Loads : in out AML_Definition_Loads.State; Copies : in out AML_Copied_Tables.State;
      Input : aliased AML_Table_Backing.State; Code : AML_Decode.Bytes;
      Source : aliased AML_Byte_Sources.Storage; Source_First : Natural; Width : AML_Decode.Integer_Width;
      Scope : Node_ID; Args : AML_Execute.Value_Arguments; Argument_Count : Natural;
      Budget : Natural; Current_Sync : AML_Execute.Sync_Level; Result : out AML_Execute.Execution_Result;
      Session_Request : AML_Declaration_Contexts.Request := AML_Declaration_Contexts.Method_Request;
      Implementation : AML_Execute.Method_Implementation := AML_Execute.AML_Implementation;
      Deferred_Readiness : Boolean := False;
      Enclosing_Table : AML_Execute.Load_Table_Token := AML_Execute.No_Load_Table);
   procedure Run_Registered_Context_Session
     (A : in out Arena; IO : in out IO_Context; Loads : in out AML_Definition_Loads.State; Copies : in out AML_Copied_Tables.State;
      Input : aliased AML_Table_Backing.State; Code : AML_Decode.Bytes;
      Source : aliased AML_Byte_Sources.Storage; Source_First : Natural; Width : AML_Decode.Integer_Width;
      Scope : Node_ID; Args : AML_Execute.Value_Arguments; Argument_Count : Natural;
      Budget : Natural; Current_Sync : AML_Execute.Sync_Level; Result : out AML_Execute.Execution_Result;
      Session_Request : AML_Declaration_Contexts.Request := AML_Declaration_Contexts.Method_Request;
      Implementation : AML_Execute.Method_Implementation := AML_Execute.AML_Implementation;
      Deferred_Readiness : Boolean := False;
      Enclosing_Table : AML_Execute.Load_Table_Token := AML_Execute.No_Load_Table) is
      use AML_Execute;
      use type AML_Definition_Loads.Load_Phase;
      use type AML_Declaration_Contexts.Lease;
      type Pending_Load is record
         Live, Entered, Has_Parameter : Boolean := False;
         Caller : AML_Declaration_Contexts.Lease := AML_Declaration_Contexts.No_Lease;
         Parameter : Reference := AML_References.No_Reference;
         Prior : Definition_Identity := No_Definition;
      end record;
      type Pending_Array is array (Positive range <>) of Pending_Load;
      Pending : Pending_Array (1 .. Loads.Capacity);
      Parameter_Store_Charge : constant Positive := 1;
      Prefix_Limit : constant Positive := AML_Names.Segment_Array'Length;
      Max_Path_Bytes : constant Positive := Prefix_Limit +
        AML_Names.Segment_Array'Length * AML_Names.Segment'Length +
        (AML_Names.Segment_Array'Length - 1);
      subtype Text_Operand_Index is Load_Table_Operand_Index range
        Load_Table_Operand_Index'First .. Load_Table_Operand_Index'Last - 1;
      subtype Text_Length is Natural range 0 .. Max_Path_Bytes;
      Buffer_Hex_Byte_Width : constant Positive := 5;
      type Operand_Text is record
         Valid : Boolean := False;
         Length : Text_Length := 0;
         Data : String (1 .. Max_Path_Bytes) := [others => Character'Val (0)];
      end record;
      type Text_Operands is array (Text_Operand_Index) of Operand_Text;
      function Source_Text (E : Arena; Value : Datum;
         Width : AML_Decode.Integer_Width) return Operand_Text is
         Result : Operand_Text;
         ID : AML_Objects.Object_ID;
         Kind : AML_Objects.Object_Kind;
         Size : Natural;
         procedure Copy (Data : AML_Decode.Bytes) is
         begin
            Result.Length := Data'Length;
            for I in 1 .. Result.Length loop
               Result.Data (I) := Character'Val (Data (Data'First + I - 1));
            end loop;
            Result.Valid := True;
         end Copy;
      begin
         if Value.Value_Kind = Integer_Datum then
            Copy (AML_Coercions.Strings.From_Integer (Value.Number, Width));
            return Result;
         elsif Value.Value_Kind /= Object_Datum
           or else Value.Object.ID /= AML_References.Source (Value.Object.Source)
           or else not Has_Source (E, Value.Object.Source)
         then return Result; end if;
         ID := Value.Object.ID;
         Kind := AML_Objects.Kind (E.Tree.Values, ID);
         if Kind = AML_Objects.Integer_Object then
            Copy (AML_Coercions.Strings.From_Integer
              (AML_Objects.Integer_Data (E.Tree.Values, ID), Width));
         elsif Kind = AML_Objects.String_Object then
            Size := AML_Objects.Length (E.Tree.Values, ID);
            if Size > Max_Path_Bytes then return Result; end if;
            Result.Length := Size;
            for I in 1 .. Size loop
               Result.Data (I) := Character'Val (AML_Objects.Stored_Byte (E.Tree.Values, ID, I - 1));
            end loop;
            Result.Valid := True;
         elsif Kind = AML_Objects.Buffer_Object then
            Size := AML_Objects.Length (E.Tree.Values, ID);
            -- Reject expansion before constructing either bounded scratch view.
            if Size > (Max_Path_Bytes + 1) / Buffer_Hex_Byte_Width then return Result; end if;
            Copy (AML_Coercions.Strings.From_Buffer (AML_Objects.Byte_Data (E.Tree.Values, ID)));
         end if;
         return Result;
      end Source_Text;
      function Text (Value : Operand_Text) return String is
        (Value.Data (1 .. Value.Length));
      function Parse_Path (Text : String) return AML_Names.Name_Result is
         Bytes : Firmware_Tables.Bytes (1 .. Text'Length + 1) := [others => 0];
      begin
         for I in Text'Range loop Bytes (I - Text'First + 1) := Character'Pos (Text (I)); end loop;
         return ACPI_Resource_Names.Parse (Bytes);
      end Parse_Path;
      function Plan_Live (Plan : Load_Table_Plan) return Boolean is
        (Plan.Disposition = Load_Module_Prepared and then Plan.Token /= No_Load_Table
         and then Natural (Plan.Token) <= Pending'Last and then Pending (Positive (Plan.Token)).Live);
      package Table_Sources renames AML_Table_Sources;
      package Table_Readers renames AML_Table_Readers;
      use type Table_Readers.Read_Status;
      Scan_Window_Bytes : constant Positive := 256;
      function Source_Limits return Table_Sources.Quotas is
        (Boot => Input.Table_Capacity, Copied => Copies.Table_Capacity);
      function Source_Counts return Table_Sources.Table_Counts is
        (Boot => Input.Count, Copied => AML_Copied_Tables.Count (Copies));
      function Sources_Admitted return Boolean is
        (Table_Sources.Fits (Source_Limits) and then
         Table_Sources.Capacity (Source_Limits) <= Loads.Capacity and then
         Input.Count <= Input.Table_Capacity and then AML_Copied_Tables.Valid (Copies));
      function Source_At (Slot : Natural) return Table_Sources.Source is
        (Table_Sources.Source_For (Source_Limits, Source_Counts,
          Table_Sources.Registry_Slot (Slot)));
      function Equal_Bytes (Backing : aliased AML_Table_Backing.State;
         Left, Right : Table_Sources.Source; Extent : Natural) return Boolean is
         Offset : Natural := 0;
         Read_Left, Read_Right : Table_Readers.Read_Status;
      begin
         while Offset < Extent loop
            declare
               Count : constant Positive := Positive'Min (Scan_Window_Bytes, Extent - Offset);
               L, R : AML_Decode.Bytes (1 .. Count);
            begin
               Table_Readers.Read_Window (Backing, Copies, Left, Offset, L, Read_Left);
               Table_Readers.Read_Window (Backing, Copies, Right, Offset, R, Read_Right);
               if Read_Left /= Table_Readers.Readable or else Read_Right /= Table_Readers.Readable
                 or else L /= R then return False; end if;
               Offset := Offset + Count;
            end;
         end loop;
         return True;
      end Equal_Bytes;
      -- Same selected signature/declared extent/checksum condition as Read_Table,
      -- streamed through owned sources without allocating another whole table.
      function Valid_Table (Backing : aliased AML_Table_Backing.State;
         Item : Table_Sources.Source; Name : Firmware_Tables.Signature;
         Extent : Natural) return Boolean is
         Header : AML_Decode.Bytes (1 .. Firmware_Tables.Table_Header_Size);
         Read_Status : Table_Readers.Read_Status;
         Encoded_Length : Interfaces.Unsigned_64 := 0;
         Length_Offset : constant Natural := 4;
         Length_Octets : constant Positive := 4;
         Bits_Per_Octet : constant Positive := 8;
         Sum : AML_Decode.Byte := 0;
         Offset : Natural := 0;
      begin
         if Extent < Firmware_Tables.Table_Header_Size then return False; end if;
         Table_Readers.Read_Window (Backing, Copies, Item, 0, Header, Read_Status);
         if Read_Status /= Table_Readers.Readable then return False; end if;
         for I in Name'Range loop
            if Header (I) /= Character'Pos (Name (I)) then return False; end if;
         end loop;
         for I in 0 .. Length_Octets - 1 loop
            Encoded_Length := Encoded_Length or Interfaces.Shift_Left
              (Interfaces.Unsigned_64 (Header (Length_Offset + I + 1)), I * Bits_Per_Octet);
         end loop;
         if Encoded_Length /= Interfaces.Unsigned_64 (Extent) then return False; end if;
         while Offset < Extent loop
            declare
               Count : constant Positive := Positive'Min (Scan_Window_Bytes, Extent - Offset);
               Data : AML_Decode.Bytes (1 .. Count);
            begin
               Table_Readers.Read_Window (Backing, Copies, Item, Offset, Data, Read_Status);
               if Read_Status /= Table_Readers.Readable then return False; end if;
               for B of Data loop Sum := Sum + B; end loop;
               Offset := Offset + Count;
            end;
         end loop;
         return Sum = 0;
      end Valid_Table;
      procedure Prepare
        (E : in out Arena; Backing : aliased AML_Table_Backing.State;
         Lease : AML_Declaration_Contexts.Lease; Scope : Natural;
         Width : AML_Decode.Integer_Width; Operands : Load_Table_Operands;
         Budget : Natural; Charged : in out Natural; Succeeded : in out Boolean;
         Status : in out Execution_Status; Plan : in out Load_Table_Plan) is
         Allowed : Boolean;
         Arguments : Text_Operands;
         procedure Spend (Amount : Natural) is
         begin
            Allowed := Amount <= Budget - Charged;
            if Allowed then Charged := Charged + Amount; else Status := Budget_Exceeded; end if;
         end Spend;
      begin
         Charged := 0; Succeeded := False; Status := Unsupported_Value; Plan := (others => <>);
         if not Sources_Admitted or else Scope > E.Tree.Used then return; end if;
         for I in Text_Operand_Index loop
            Arguments (I) := Source_Text (E, Operands (I), Width);
            if not Arguments (I).Valid then return; end if;
            Spend (Arguments (I).Length); if not Allowed then return; end if;
         end loop;
         if Backing.Count + AML_Copied_Tables.Count (Copies) > (Budget - Charged) / Firmware_Tables.Table_Header_Size
         then Status := Budget_Exceeded; return; end if;
         Spend ((Backing.Count + AML_Copied_Tables.Count (Copies)) * Firmware_Tables.Table_Header_Size);
         if not Allowed then return; end if;
         declare
            Signature : constant String := Text (Arguments (0));
            OEM : constant String := Text (Arguments (1));
            OEM_Table : constant String := Text (Arguments (2));
            Requested : Firmware_Tables.Identifiers.Selection;
            Index : Natural;
            Selected : Table_Readers.Table_Info;
         begin
            if Signature'Length /= Requested.Name'Length or else OEM'Length > Requested.OEM'Length
              or else OEM_Table'Length > Requested.OEM_Table'Length then return; end if;
            Requested.Name := Signature;
            Requested.Match_OEM := OEM'Length /= 0; Requested.Match_OEM_Table := OEM_Table'Length /= 0;
            for I in OEM'Range loop Requested.OEM (I) := OEM (I); end loop;
            for I in OEM_Table'Range loop Requested.OEM_Table (I) := OEM_Table (I); end loop;
            declare
               Found : constant Table_Readers.Selection_Result :=
                 Table_Readers.Find_Table (Backing, Copies, Requested);
               Candidate : Table_Sources.Source;
            begin
               case Found.Status is
                  when Table_Readers.Invalid_Selection_Inventory => return;
                  when Table_Readers.No_Matching_Table =>
                     Plan.Disposition := Load_Handled; Status := Returned; return;
                  when Table_Readers.Table_Selected => null;
               end case;
               Candidate := Found.Item;
               Index := Natural (Table_Sources.Slot_For (Source_Limits, Source_Counts, Candidate));
               if Index = 0 then return; end if;
               if Candidate.Kind = Table_Sources.Boot_Source and then Requested.Name in "DSDT" | "SSDT"
               then return; end if;
               Selected := Table_Readers.Describe (Backing, Copies, Candidate);
               if Selected.Status /= Table_Readers.Readable then return; end if;
               for I in 1 .. Loads.Capacity loop
                  if AML_Definition_Loads.Phase (Loads, I) = AML_Definition_Loads.Loaded then
                     declare
                        Prior_Source : constant Table_Sources.Source := Source_At (I);
                        Prior : constant Table_Readers.Table_Info := Table_Readers.Describe (Backing, Copies, Prior_Source);
                     begin
                        if Prior.Status /= Table_Readers.Readable then return; end if;
                        if Prior.Extent = Selected.Extent then
                           Spend (Selected.Extent); if not Allowed then return; end if;
                           if Equal_Bytes (Backing, Candidate, Prior_Source, Selected.Extent) then
                              Plan.Disposition := Load_Handled; Status := Duplicate_Name; return;
                           end if;
                        end if;
                     end;
                  end if;
               end loop;
            end;
            declare
               Root_Text : constant String := Text (Arguments (3));
               Parameter_Text : constant String := Text (Arguments (4));
               Root_Name, Parameter_Name : AML_Names.Name_Result;
               Destination : Lookup_Result := (Found, Root);
               Target : Lookup_Result;
               Parameter : Reference := AML_References.No_Reference;
            begin
               -- Checksum admission scans the complete selected table before effects.
               Spend (Selected.Extent); if not Allowed then return; end if;
               if not Valid_Table (Backing, Source_At (Index), Requested.Name, Selected.Extent) then return; end if;
               if Root_Text'Length /= 0 then
                  Root_Name := Parse_Path (Root_Text); if Root_Name.Kind /= AML_Names.Accepted then return; end if;
                  Destination := Resolve (E.Tree, Node_ID (Scope), Root_Name);
                  if Destination.Status /= Found then return; end if;
                  Destination := Canonical_Node (E.Tree, Destination.Node);
                  if Destination.Status /= Found then return; end if;
               end if;
               if Destination.Node /= Root and then not Static_Scope (Kind (E.Tree, Destination.Node)) then return; end if;
               if Parameter_Text'Length /= 0 then
                  Parameter_Name := Parse_Path (Parameter_Text); if Parameter_Name.Kind /= AML_Names.Accepted then return; end if;
                  Target := Resolve (E.Tree, (if Parameter_Name.Rooted or else Parameter_Name.Parents > 0 then Node_ID (Scope)
                    else Destination.Node), Parameter_Name);
                  if Target.Status /= Found then return; end if;
                  Target := Canonical_Node (E.Tree, Target.Node); if Target.Status /= Found then return; end if;
                  Make_Named_Reference (E, Target.Node, Parameter, Allowed); if not Allowed then return; end if;
               end if;
               AML_Definition_Loads.Reserve (Loads, Index, Allowed); if not Allowed then return; end if;
               Pending (Index) := (Live => True, Entered => False, Has_Parameter => Parameter_Text'Length /= 0,
                 Caller => Lease, Parameter => Parameter, Prior => No_Definition);
               Plan := (Load_Module_Prepared, Load_Table_Token (Index), Natural (Destination.Node),
                 Selected.Extent - Firmware_Tables.Table_Header_Size);
               Status := Returned;
            end;
         end;
      end Prepare;
      procedure Capture_Source
        (E : in out Arena; Backing : aliased AML_Table_Backing.State;
         Lease : AML_Declaration_Contexts.Lease; Scope : Natural;
         Width : AML_Decode.Integer_Width; Source : Datum;
         Budget : Natural; Charged : in out Natural; Succeeded : in out Boolean;
         Status : in out Execution_Status; Plan : in out Load_Table_Plan) is
         pragma Unreferenced (Width);
         ID : AML_Objects.Object_ID := AML_Objects.No_Object;
         Header : AML_Decode.Bytes (1 .. Firmware_Tables.Table_Header_Size);
         Raw_Length : Interfaces.Unsigned_64 := 0;
         Length_Offset : constant Natural := 4;
         Length_Octets : constant Positive := 4;
         Bits_Per_Octet : constant Positive := 8;
         Source_Length, Source_First_Byte : Natural := 0;
         Source_First_Bit : Natural range 0 .. Bits_Per_Octet - 1 := 0;
         Source_Last_Bits : Positive range 1 .. Bits_Per_Octet := Bits_Per_Octet;
         Source_Read_Units : Positive range 1 .. 2 := 1;
         Source_Is_Table : Boolean := False;
         Source_Table : Table_Readers.Field_View;
         Source_Read_Status : Table_Readers.Read_Status := Table_Readers.Readable;
         Extent, Index : Natural;
         Stored : AML_Copied_Tables.Report;
         Candidate : Table_Sources.Source;
         Allowed : Boolean;
         use type AML_Copied_Tables.Outcome;
         procedure Spend (Amount : Natural) is
         begin
            Allowed := Amount <= Budget - Charged;
            if Allowed then Charged := Charged + Amount; else Status := Budget_Exceeded; end if;
         end Spend;
         procedure Spend_Source_Read (Amount : Natural) is
         begin
            if Amount > (Budget - Charged) / Source_Read_Units then
               Allowed := False; Status := Budget_Exceeded;
            else Spend (Amount * Source_Read_Units); end if;
         end Spend_Source_Read;
         function Source_Byte (Offset : Natural) return AML_Decode.Byte
           with Pre => Offset < Source_Length
         is
            Position : constant Natural := Source_First_Byte + Offset;
            Available : constant Positive :=
              (if Offset = Source_Length - 1 then Source_Last_Bits else Bits_Per_Octet);
            Value : AML_Decode.Byte;
         begin
            if Source_Is_Table then
               Table_Readers.Read_Field_Byte
                 (Backing, Copies, Source_Table, Offset, Value, Source_Read_Status);
               return Value;
            end if;
            Value := Interfaces.Shift_Right
              (AML_Objects.Stored_Byte (E.Tree.Values, ID, Position), Source_First_Bit);
            if Source_First_Bit /= 0 and then Available > Bits_Per_Octet - Source_First_Bit then
               Value := Value or Interfaces.Shift_Left
                 (AML_Objects.Stored_Byte (E.Tree.Values, ID, Position + 1), Bits_Per_Octet - Source_First_Bit);
            end if;
            if Available < Bits_Per_Octet then
               Value := Value and Interfaces.Shift_Right (AML_Decode.Byte'Last, Bits_Per_Octet - Available);
            end if;
            return Value;
         end Source_Byte;
      begin
         Charged := 0; Succeeded := False; Status := Unsupported_Value; Plan := (others => <>);
         if not Sources_Admitted or else Scope > E.Tree.Used then return; end if;
         if Source.Value_Kind = Reference_Datum and then Buffer_Field_Matches (E, Source.Ref) then
            declare
               Info : constant Buffer_Field_Descriptor :=
                 E.Tree.Items (Node_ID (AML_References.Named_Node (Source.Ref))).Buffer_Field_Info;
               Field_Bits_Per_Byte : constant AML_Decode.Integer_Value := AML_Decode.Integer_Value (Bits_Per_Octet);
               Count : AML_Decode.Integer_Value;
            begin
               ID := AML_References.Source (Info.Backing);
               if Info.Bits = 0 or else AML_Objects.Kind (E.Tree.Values, ID) /= AML_Objects.Buffer_Object
                 or else not AML_Buffer_Bits.Fits (AML_Objects.Length (E.Tree.Values, ID), Info.Offset, Info.Bits)
               then Status := Buffer_Field_Limit; return; end if;
               Count := (Info.Bits - 1) / Field_Bits_Per_Byte + 1;
               if Count > AML_Decode.Integer_Value (AML_Objects.Length (E.Tree.Values, ID))
                 or else Info.Offset / Field_Bits_Per_Byte > AML_Decode.Integer_Value (AML_Objects.Length (E.Tree.Values, ID))
               then Status := Buffer_Field_Limit; return; end if;
               Source_Length := Natural (Count);
               Source_First_Byte := Natural (Info.Offset / Field_Bits_Per_Byte);
               Source_First_Bit := Natural (Info.Offset mod Field_Bits_Per_Byte);
               Source_Last_Bits := Positive ((Info.Bits - 1) mod Field_Bits_Per_Byte + 1);
               if Source_First_Bit /= 0 then Source_Read_Units := 2; end if;
            end;
         elsif Source.Value_Kind = Reference_Datum and then Table_Field_Matches (E, Source.Ref) then
            declare
               Info : constant Table_Field :=
                 E.Tree.Items (Node_ID (AML_References.Named_Node (Source.Ref))).Table_Binding;
            begin
               Source_Table :=
                 (Item => Info.Region.Item,
                  Extent => Info.Region.Extent, Offset => Info.Offset, Bits => Info.Bits);
               if Info.Bits = 0 or else not Table_Readers.Valid_Field (Backing, Copies, Source_Table) then
                  Status := Buffer_Field_Limit; return;
               end if;
               Source_Length := Table_Readers.Field_Length (Source_Table);
               Source_Is_Table := True;
               -- Read_Byte touches at most two owned physical octets, including
               -- unaligned windows. No provider callback or hardware read occurs.
               Source_Read_Units := 2;
            end;
         else
            if Source.Value_Kind /= Object_Datum
              or else Source.Object.ID /= AML_References.Source (Source.Object.Source)
              or else not Has_Source (E, Source.Object.Source)
            then return; end if;
            ID := Source.Object.ID;
            if AML_Objects.Kind (E.Tree.Values, ID) /= AML_Objects.Buffer_Object then return; end if;
            Source_Length := AML_Objects.Length (E.Tree.Values, ID);
         end if;
         if Source_Length < Header'Length then
            Status := Invalid_Table_Length; return;
         end if;
         Spend_Source_Read (Header'Length); if not Allowed then return; end if;
         for I in Header'Range loop
            Header (I) := Source_Byte (I - 1);
            if Source_Read_Status /= Table_Readers.Readable then return; end if;
         end loop;
         for I in 0 .. Length_Octets - 1 loop
            Raw_Length := Raw_Length or Interfaces.Shift_Left
              (Interfaces.Unsigned_64 (Header (Length_Offset + I + 1)), I * Bits_Per_Octet);
         end loop;
         if Raw_Length < Interfaces.Unsigned_64 (Header'Length) then
            Status := Invalid_Table_Length; return;
         end if;
         if Raw_Length > Interfaces.Unsigned_64 (Source_Length) then
            Status := Buffer_Field_Limit; return;
         end if;
         -- The source length is Natural, so only narrow after that comparison.
         Extent := Natural (Raw_Length);
         Index := 0;
         Spend_Source_Read (Extent); if not Allowed then return; end if;
         declare
            Data : AML_Decode.Bytes (1 .. Extent);
            Name : Firmware_Tables.Signature;
            Metadata_Charge : constant Positive := 1;
            Same, Readable : Boolean;
            procedure Compare_Data (Item : Table_Sources.Source;
               Equal, Read_OK : out Boolean) is
               Offset : Natural := 0;
               Read_Status : Table_Readers.Read_Status;
            begin
               Equal := False; Read_OK := False;
               while Offset < Data'Length loop
                  declare
                     Count : constant Positive := Positive'Min (Scan_Window_Bytes, Data'Length - Offset);
                     Window : AML_Decode.Bytes (1 .. Count);
                  begin
                     Table_Readers.Read_Window (Backing, Copies, Item, Offset, Window, Read_Status);
                     if Read_Status /= Table_Readers.Readable then return; end if;
                     for I in Window'Range loop
                        if Window (I) /= Data (Data'First + Offset + I - 1) then Read_OK := True; return; end if;
                     end loop;
                     Offset := Offset + Count;
                  end;
               end loop;
               Equal := True; Read_OK := True;
            end Compare_Data;
            use type Firmware_Tables.Admission;
         begin
            -- No AML, collection or provider callback while observing source
            -- bytes. Validate before treating an owned boot span as equivalent.
            for I in Data'Range loop
               Data (I) := Source_Byte (I - 1);
               if Source_Read_Status /= Table_Readers.Readable then return; end if;
            end loop;
            for I in Name'Range loop Name (I) := Character'Val (Header (I)); end loop;
            if Name = "FACS" then return; end if;
            Spend (Extent); if not Allowed then return; end if;
            declare
               Checked : constant Firmware_Tables.Table_Result := Firmware_Tables.Read_Table (Data, Name);
            begin
               if Checked.Status /= Firmware_Tables.Accepted or else Checked.Extent /= Extent then return; end if;
            end;
            -- A consumed definition wins over an unused alias, regardless of
            -- source kind. Never rerun active, loaded or terminal failed code.
            for I in 1 .. Loads.Capacity loop
               Spend (Metadata_Charge); if not Allowed then return; end if;
               if Has_Definition (AML_Definition_Loads.Definition (Loads, I)) then
                  declare
                     Prior_Source : constant Table_Sources.Source := Source_At (I);
                     Prior : constant Table_Readers.Table_Info := Table_Readers.Describe (Backing, Copies, Prior_Source);
                  begin
                     if Prior.Status /= Table_Readers.Readable then return; end if;
                     if Prior.Extent = Extent then
                        Spend (Extent); if not Allowed then return; end if;
                        Compare_Data (Prior_Source, Same, Readable); if not Readable then return; end if;
                        if Same then
                           Plan.Disposition := Load_Handled;
                           if AML_Definition_Loads.Phase (Loads, I) in AML_Definition_Loads.Loaded | AML_Definition_Loads.Loading
                           then Status := Duplicate_Name; end if;
                           return;
                        end if;
                     end if;
                  end;
               end if;
            end loop;
            -- Reuse an identical owned boot table's own slot. The startup walk
            -- then observes Loaded and skips its body, with no permanent copy.
            for I in 1 .. Backing.Count loop
               Spend (Metadata_Charge); if not Allowed then return; end if;
               if not Has_Definition (AML_Definition_Loads.Definition (Loads, I)) then
                  declare
                     Item : constant Table_Sources.Source :=
                       (Table_Sources.Boot_Source, Table_Sources.Boot_Table_ID (I));
                     Info : constant Table_Readers.Table_Info := Table_Readers.Describe (Backing, Copies, Item);
                  begin
                     if Info.Status /= Table_Readers.Readable then return; end if;
                     if Info.Extent = Extent then
                        Spend (Extent); if not Allowed then return; end if;
                        Compare_Data (Item, Same, Readable); if not Readable then return; end if;
                        if Same then
                           if AML_Definition_Loads.Phase (Loads, I) /= AML_Definition_Loads.Retained then return; end if;
                           Candidate := Item; Index := I; exit;
                        end if;
                     end if;
                  end;
               end if;
            end loop;
            if Index = 0 then
               if Extent > Copies.Byte_Capacity then Status := Value_Limit; return; end if;
               AML_Copied_Tables.Retain (Copies, Data, Budget - Charged, Stored);
               Charged := Charged + Stored.Charged;
               case Stored.Status is
                  when AML_Copied_Tables.Added | AML_Copied_Tables.Already_Retained => null;
                  when AML_Copied_Tables.Budget_Exceeded => Status := Budget_Exceeded; return;
                  when AML_Copied_Tables.Table_Limit | AML_Copied_Tables.Byte_Limit => Status := Value_Limit; return;
                  when others => return;
               end case;
               Candidate := (Table_Sources.Copied_Source, Table_Sources.Copied_Table_ID (Stored.ID));
               Index := Natural (Table_Sources.Slot_For (Source_Limits, Source_Counts, Candidate));
               if Index = 0 then return; end if;
            end if;
         end;
         AML_Definition_Loads.Reserve (Loads, Index, Allowed); if not Allowed then return; end if;
         Pending (Index) := (Live => True, Entered => False, Has_Parameter => False,
           Caller => Lease, Parameter => AML_References.No_Reference, Prior => No_Definition);
         -- Modern Load always places definitions relative to namespace root.
         Plan := (Load_Module_Prepared, Load_Table_Token (Index), Natural (Root),
           Extent - Firmware_Tables.Table_Header_Size);
         Status := Returned;
      end Capture_Source;

      procedure Copy_Body (E : in out Arena; Backing : aliased AML_Table_Backing.State;
         Plan : Load_Table_Plan; Data : in out AML_Decode.Bytes; Status : in out Execution_Status) is
         pragma Unreferenced (E);
         Item : constant Table_Sources.Source := Source_At (Natural (Plan.Token));
         Info : constant Table_Readers.Table_Info := Table_Readers.Describe (Backing, Copies, Item);
         Read_Status : Table_Readers.Read_Status;
      begin
         Status := Unsupported_Value;
         if not Plan_Live (Plan) or else Info.Status /= Table_Readers.Readable
           or else Plan.Body_Length /= Info.Extent - Firmware_Tables.Table_Header_Size
           or else Data'Length /= Plan.Body_Length then return; end if;
         Table_Readers.Read_Window (Backing, Copies, Item, Firmware_Tables.Table_Header_Size, Data, Read_Status);
         if Read_Status = Table_Readers.Readable then Status := Returned; end if;
      end Copy_Body;
      procedure Begin_Body (E : in out Arena; Backing : aliased AML_Table_Backing.State;
         Plan : Load_Table_Plan; Status : in out Execution_Status) is
         pragma Unreferenced (Backing);
         Index : constant Natural := Natural (Plan.Token);
         ID, Prior : Definition_Identity;
         Allowed : Boolean;
      begin
         Status := Unsupported_Value;
         if not Plan_Live (Plan) or else Pending (Index).Entered or else Plan.Scope > E.Tree.Used then return; end if;
         if Plan.Scope /= Root and then (not Present (E.Tree, Node_ID (Plan.Scope))
           or else not Static_Scope (Kind (E.Tree, Node_ID (Plan.Scope)))) then return; end if;
         Enter_Definition (E.Tree, Prior, ID, Allowed); if not Allowed then Status := Value_Limit; return; end if;
         AML_Definition_Loads.Consume (Loads, Index, ID, Allowed);
         if not Allowed then E.Tree.Current_Definition := Prior; return; end if;
         Pending (Index).Prior := Prior; Pending (Index).Entered := True; Status := Returned;
      end Begin_Body;
      procedure Finish
        (E : in out Arena; Backing : aliased AML_Table_Backing.State;
         Lease : AML_Declaration_Contexts.Lease; Plan : Load_Table_Plan;
         Operands : Load_Table_Operands; Module_Result : Execution_Result;
         Budget : Natural; Charged : in out Natural; Succeeded : in out Boolean;
         Status : in out Execution_Status) is
         pragma Unreferenced (Backing);
         Index : constant Natural := Natural (Plan.Token);
         Allowed, Completed : Boolean := False;
      begin
         Charged := 0; Succeeded := False; Status := Unsupported_Value;
         if not Plan_Live (Plan) then return; end if;
         if Pending (Index).Entered then E.Tree.Current_Definition := Pending (Index).Prior; end if;
         Completed := Pending (Index).Entered and then Pending (Index).Caller = Lease
           and then Module_Result.Status = Module_Completed;
         if Completed and then Pending (Index).Has_Parameter then
            if Budget < Parameter_Store_Charge then Status := Budget_Exceeded; Completed := False;
            elsif not Admitted_Descriptor (E, Pending (Index).Parameter) then Completed := False;
            else
               Charged := Parameter_Store_Charge;
               Store_Reference_Value (E, Pending (Index).Parameter, Width, Operands (5), Status, Direct_Target);
               Completed := Status = Returned;
            end if;
         end if;
         -- Binding deferred members and providers is a separate readiness phase.
         -- Never claim success while unresolved namespace members remain.
         if Completed and then not Deferred_Readiness and then Pending_Members (E) /= 0 then Completed := False; Status := Uninitialized; end if;
         AML_Definition_Loads.Finish (Loads, Positive (Index), Completed, Allowed);
         if not Allowed then raise Program_Error with "active definition finalization invariant"; end if;
         Pending (Index) := (others => <>);
         if Completed then Succeeded := True; Status := Returned; end if;
      end Finish;
      function Expected_Reservations return Boolean is
         Index : constant Natural := Natural (Enclosing_Table);
      begin
         if Enclosing_Table /= No_Load_Table and then
           (Source_At (Index).Kind = Table_Sources.No_Source or else not AML_Declaration_Contexts.Is_Module (Session_Request))
         then return False; end if;
         return AML_Definition_Loads.Session_Admitted (Loads,
           AML_Definition_Loads.Slot_Selection (Enclosing_Table), A.Tree.Current_Definition);
      end Expected_Reservations;
      procedure Read_Registered_Table
        (Tree : in out State; Backing : aliased AML_Table_Backing.State; Scope : Natural;
         Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width;
         Purpose : AML_Execute.Binding_Purpose; Binding : out AML_Execute.Binding_Result) is
      begin
         Lookup_With_Owned_Tables (Tree, Backing, Copies, Scope, Path, Width, Purpose, Binding);
      end Read_Registered_Table;
      procedure Complete_Registered_Table
        (Tree : in out State; Backing : aliased AML_Table_Backing.State; Token : Natural;
         Width : AML_Decode.Integer_Width; Signature, OEM, Table_ID : AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status) is
      begin
         Complete_Owned_Region (Tree, Backing, Copies, Token, Width, Signature, OEM, Table_ID, Status);
      end Complete_Registered_Table;
      procedure Run is new Run_Context_Session
        (IO_Context, Field_Operation, Handoff, Before_Allocation,
         Nested_Loads_Enabled => True, Module_Recovery_Enabled => True,
         Restricted_Table_Loads_Enabled => AML_Declaration_Contexts.Is_Module (Session_Request),
         Prepare_Load => Prepare, Prepare_Source => Capture_Source, Copy_Load => Copy_Body,
         Begin_Load => Begin_Body, Finish_Load => Finish,
         Read_Table_Binding => Read_Registered_Table,
         Complete_Table_Binding => Complete_Registered_Table);
   begin
      if not Sources_Admitted or else Frame_Pins.Count (A.Frame_Roots) /= 0 or else not Expected_Reservations
        or else (not Deferred_Readiness and then Pending_Members (A.Tree) /= 0)
      then Result := (Unsupported_Value, 0); return; end if;
      Run (A, IO, Input, Code, Source, Source_First, Width, Scope, Args, Argument_Count,
        Budget, Current_Sync, Result, Session_Request, Implementation);
      if not Expected_Reservations then raise Program_Error with "active definition leaked reservation"; end if;
   end Run_Registered_Context_Session;
   generic
      type IO_Context is limited private;
      with procedure Field_Operation
        (E : in out Arena; IO : in out IO_Context; Ref : Reference;
         Width : AML_Decode.Integer_Width; For_Write : Boolean;
         Value : in out AML_Execute.Datum; Status : out AML_Execute.Execution_Status);
      with procedure Handoff (E : in out Arena; Result : AML_Execute.Execution_Result);
      with procedure Before_Allocation (E : in out Arena; Extra : Root_Values;
         Status : out Allocation_Hook_Status) is Skip_Collection;
   procedure Invoke_Registered_Context_With_Handoff
     (A : in out Arena; IO : in out IO_Context; Loads : in out AML_Definition_Loads.State; Copies : in out AML_Copied_Tables.State;
      Input : aliased AML_Table_Backing.State; Node : Node_ID;
      Args : AML_Execute.Value_Arguments; Argument_Count : Natural;
      Budget : Natural; Result : out AML_Execute.Execution_Result);
   procedure Invoke_Registered_Context_With_Handoff
     (A : in out Arena; IO : in out IO_Context; Loads : in out AML_Definition_Loads.State; Copies : in out AML_Copied_Tables.State;
      Input : aliased AML_Table_Backing.State; Node : Node_ID;
      Args : AML_Execute.Value_Arguments; Argument_Count : Natural;
      Budget : Natural; Result : out AML_Execute.Execution_Result) is
      Definition : constant AML_Execute.Method_Definition := Read_Method (A.Tree, Node);
      procedure Run is new Run_Registered_Context_Session
        (IO_Context, Field_Operation, Handoff, Before_Allocation);
   begin
      if not Definition.Exists then Result := (AML_Execute.Invalid_Method, 0); return; end if;
      if Natural (Definition.Flags and 7) /= Argument_Count then Result := (AML_Execute.Argument_Mismatch, 0); return; end if;
      Run (A, IO, Loads, Copies, Input,
        (if Definition.Implementation = AML_Execute.AML_Implementation then Definition.Code.Bytes else AML_Decode.Bytes'(1 .. 0 => 0)),
        Definition.Code, 0, Definition.Width, Node, Args, Argument_Count, Budget, 0, Result,
        Implementation => Definition.Implementation);
   end Invoke_Registered_Context_With_Handoff;

   generic
      type IO_Context is limited private;
      with procedure Field_Operation
        (E : in out Arena; IO : in out IO_Context; Ref : Reference;
         Width : AML_Decode.Integer_Width; For_Write : Boolean;
         Value : in out AML_Execute.Datum; Status : out AML_Execute.Execution_Status);
      with procedure Before_Allocation (E : in out Arena; Extra : Root_Values;
         Status : out Allocation_Hook_Status) is Skip_Collection;
   procedure Invoke_Registered_Context_Retained
     (A : in out Arena; IO : in out IO_Context; Loads : in out AML_Definition_Loads.State; Copies : in out AML_Copied_Tables.State; Input : aliased AML_Table_Backing.State;
      Node : Node_ID; Args : AML_Execute.Value_Arguments; Argument_Count : Natural;
      Budget : Natural; Result : out AML_Execute.Execution_Result;
      Result_Root : out Retained_Root; Retention : out Invocation_Retention_Status);
   procedure Invoke_Registered_Context_Retained
     (A : in out Arena; IO : in out IO_Context; Loads : in out AML_Definition_Loads.State; Copies : in out AML_Copied_Tables.State; Input : aliased AML_Table_Backing.State;
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
         elsif Returned_Result.Status = Metadata_Returned then
            if not Metadata_Matches (E, Returned_Result.Metadata) then return; end if;
            Value := (Metadata_Datum, Returned_Result.Metadata);
         elsif Returned_Result.Status = Reference_Returned then
            if not Admitted_Descriptor (E, Returned_Result.Ref) then return; end if;
            Value := (Reference_Datum, Returned_Result.Ref);
         else return;
         end if;
         Pins.Replace (E.Retention, Reserved_Root.Pin, Value, Pin_Status);
         if Pin_Status /= Pins.Ready then raise Program_Error with "retained result slot invariant"; end if;
         Captured := True;
      end Handoff;
      procedure Run is new Invoke_Registered_Context_With_Handoff (IO_Context, Field_Operation, Handoff, Before_Allocation);
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
      Run (A, IO, Loads, Copies, Input, Node, Args, Argument_Count, Budget, Result);
      if Captured and then Result.Status in Object_Returned | Reference_Returned | Metadata_Returned then
         Result_Root := Reserved_Root; Retention := Result_Retained;
      else
         Release (A, Reserved_Root, Removed);
         if Removed /= Released then raise Program_Error with "retained result release invariant"; end if;
         if Result.Status in Object_Returned | Reference_Returned | Metadata_Returned then
            Result := (Status => Unsupported_Value, Charged => Result.Charged);
            Retention := Invalid_Result;
         else Retention := No_Root_Required;
         end if;
      end if;
   end Invoke_Registered_Context_Retained;
   generic
      with procedure Before_Allocation (E : in out Arena; Extra : Root_Values;
         Status : out Allocation_Hook_Status) is Skip_Collection;
   procedure Invoke_Registered_Retained
     (A : in out Arena; Loads : in out AML_Definition_Loads.State; Copies : in out AML_Copied_Tables.State; Input : aliased AML_Table_Backing.State;
      Node : Node_ID; Args : AML_Execute.Value_Arguments; Argument_Count : Natural;
      Budget : Natural; Result : out AML_Execute.Execution_Result;
      Result_Root : out Retained_Root; Retention : out Invocation_Retention_Status);
   procedure Invoke_Registered_Retained
     (A : in out Arena; Loads : in out AML_Definition_Loads.State; Copies : in out AML_Copied_Tables.State; Input : aliased AML_Table_Backing.State;
      Node : Node_ID; Args : AML_Execute.Value_Arguments; Argument_Count : Natural;
      Budget : Natural; Result : out AML_Execute.Execution_Result;
      Result_Root : out Retained_Root; Retention : out Invocation_Retention_Status) is
      IO : Null_Field_Context;
      procedure Run is new Invoke_Registered_Context_Retained
        (Null_Field_Context, Reject_Field_Operation, Before_Allocation);
   begin Run (A, IO, Loads, Copies, Input, Node, Args, Argument_Count, Budget, Result, Result_Root, Retention);
   end Invoke_Registered_Retained;

   generic
      type IO_Context is limited private;
      with procedure Field_Operation
        (E : in out Arena; IO : in out IO_Context; Ref : Reference;
         Width : AML_Decode.Integer_Width; For_Write : Boolean;
         Value : in out AML_Execute.Datum; Status : out AML_Execute.Execution_Status);
      with procedure Before_Allocation (E : in out Arena; Extra : Root_Values;
         Status : out Allocation_Hook_Status) is Skip_Collection;
   procedure Invoke_Context_Retained
     (A : in out Arena; IO : in out IO_Context; Input : aliased AML_Table_Backing.State;
      Node : Node_ID; Args : AML_Execute.Value_Arguments; Argument_Count : Natural;
      Budget : Natural; Result : out AML_Execute.Execution_Result;
      Result_Root : out Retained_Root; Retention : out Invocation_Retention_Status);
   procedure Invoke_Context_Retained
     (A : in out Arena; IO : in out IO_Context; Input : aliased AML_Table_Backing.State;
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
         elsif Returned_Result.Status = Metadata_Returned then
            if not Metadata_Matches (E, Returned_Result.Metadata) then return; end if;
            Value := (Metadata_Datum, Returned_Result.Metadata);
         elsif Returned_Result.Status = Reference_Returned then
            if not Admitted_Descriptor (E, Returned_Result.Ref) then return; end if;
            Value := (Reference_Datum, Returned_Result.Ref);
         else return;
         end if;
         Pins.Replace (E.Retention, Reserved_Root.Pin, Value, Pin_Status);
         if Pin_Status /= Pins.Ready then raise Program_Error with "retained result slot invariant"; end if;
         Captured := True;
      end Handoff;
      procedure Run is new Invoke_Context_With_Handoff (IO_Context, Field_Operation, Handoff, Before_Allocation);
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
      Run (A, IO, Input, Node, Args, Argument_Count, Budget, Result);
      if Captured and then Result.Status in Object_Returned | Reference_Returned | Metadata_Returned then
         Result_Root := Reserved_Root; Retention := Result_Retained;
      else
         Release (A, Reserved_Root, Removed);
         if Removed /= Released then raise Program_Error with "retained result release invariant"; end if;
         if Result.Status in Object_Returned | Reference_Returned | Metadata_Returned then
            Result := (Status => Unsupported_Value, Charged => Result.Charged);
            Retention := Invalid_Result;
         else Retention := No_Root_Required;
         end if;
      end if;
   end Invoke_Context_Retained;

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
      Result_Root : out Retained_Root; Retention : out Invocation_Retention_Status) is
      IO : Null_Field_Context;
      procedure Run is new Invoke_Context_Retained (Null_Field_Context, Reject_Field_Operation, Before_Allocation);
   begin
      Run (A, IO, Input, Node, Args, Argument_Count, Budget, Result, Result_Root, Retention);
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
      function Module_Errors (A : Arena) return Module_Error_Observation is
        (Owned.Module_Errors (A.Inner));
      function Import_Model (A : Arena) return Import_State is (A.Import);
      function Module_Transient_Free (A : Arena) return Boolean is
        (not A.In_Progress and then Owned.Frame_Root_Count (A.Inner) = 0
         and then not AML_Frame_Handles.Present (AML_Module_Frames.Top (A.Inner.Tree.Module_Frames))
         and then (for all I in 1 .. A.Inner.Tree.Used => not A.Inner.Tree.Items (I).Initializing));
      function Invocation_Model (A : Arena) return AML_Frame_Handles.Invocation_Serial is
        (Owned.Invocation_Count (A.Inner));
      function Valid (A : Arena) return Boolean is
        (Owned.Valid (A.Inner) and then
           (if A.State = Uninitialized then not Owned.Initialized (A.Inner)
            else Owned.Initialized (A.Inner))
          and then (if A.Import = Module_Active then A.State = Loading and then A.In_Progress)
          and then (if A.Import = Module_Failed then A.State = Loading)
          and then (if A.State = Uninitialized then A.Import = Import_Undecided));
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
      function Pending_Notifications (A : Arena) return Notification_Count is
        (A.Inner.Notifications.Used);
      function Notification_Model (A : Arena) return Notification_State is
        (A.Inner.Notifications);
      procedure Configure_Notifications (A : in out Arena; Enabled : Boolean;
         Status : out Access_Status) is
      begin
         if A.In_Progress then Status := Busy; return;
         elsif A.State /= Ready then Status := Wrong_Phase; return; end if;
         A.Inner.Notifications.Enabled := Enabled;
         Status := Available;
      end Configure_Notifications;
      procedure Take_Notification (A : in out Arena; Item : out Notification_Observation;
         Present : out Boolean; Status : out Access_Status) is
      begin
         Item := (others => <>); Present := False;
         if A.In_Progress then Status := Busy; return;
         elsif A.State /= Ready then Status := Wrong_Phase; return; end if;
         Status := Available;
         if A.Inner.Notifications.Used = 0 then return; end if;
         Item := A.Inner.Notifications.Entries (1).Observation;
         Item.Current_Target := Named_Identity_Matches
           (A.Inner, A.Inner.Notifications.Entries (1).Target);
         Present := True;
         for I in 1 .. A.Inner.Notifications.Used - 1 loop
            A.Inner.Notifications.Entries (I) := A.Inner.Notifications.Entries (I + 1);
         end loop;
         A.Inner.Notifications.Entries (A.Inner.Notifications.Used) := (others => <>);
         A.Inner.Notifications.Used := A.Inner.Notifications.Used - 1;
      end Take_Notification;

      function Current (A : Arena) return Phase is (A.State);
      function Retained_Count (A : Arena) return Retained_Root_Count is
        (Owned.Retained_Count (A.Inner));
      function Node_Count (A : Arena) return Node_ID is (Owned.Node_Count (A.Inner));
      function Position (Item : Node_Descriptor) return Node_ID is (Item.Node);
      function Parent_Position (Item : Node_Descriptor) return Node_ID is (Item.Parent_Node);
      function Node_Name (Item : Node_Descriptor) return AML_Names.Segment is (Item.Part);
      function Node_Kind (Item : Node_Descriptor) return Object_Kind is (Item.Tag);
      procedure Describe_Node (A : Arena; Node : Node_ID;
         Item : out Node_Descriptor; Status : out Access_Status) is
      begin
         Item := No_Node;
         if A.In_Progress then Status := Busy; return;
         elsif A.State /= Ready then Status := Wrong_Phase; return;
         elsif Node > Owned.Node_Count (A.Inner) or else not Owned.Present (A.Inner, Node)
         then Status := Invalid_Value; return; end if;
         Item := (Identity => Metadata_Identity_At (A.Inner.Tree, A.Inner.Token, Node),
           Node => Node, Parent_Node => Parent (A.Inner.Tree, Node),
           Part => (if Node = Root then "____" else Name (A.Inner.Tree, Node)),
           Tag => Kind (A.Inner.Tree, Node));
         Status := Available;
      end Describe_Node;
      procedure Describe_Definition (A : Arena; Item : Node_Descriptor;
         Definition : out Definition_Identity; Status : out Access_Status) is
      begin
         Definition := No_Definition;
         if A.In_Progress then Status := Busy; return;
         elsif A.State /= Ready then Status := Wrong_Phase; return;
         elsif not Current_Binding (A, Item) then Status := Invalid_Value; return;
         end if;
         Definition := Definition_Of (A.Inner.Tree, Item.Node); Status := Available;
      end Describe_Definition;
      function Current_Binding (A : Arena; Item : Node_Descriptor) return Boolean is
        (not A.In_Progress and then A.State = Ready
         and then Item.Node <= Owned.Node_Count (A.Inner)
         and then Owned.Present (A.Inner, Item.Node)
         and then Named_Identity_Matches (A.Inner, Item.Identity)
         and then Item.Identity = Metadata_Identity_At (A.Inner.Tree, A.Inner.Token, Item.Node)
         and then Item.Parent_Node = Parent (A.Inner.Tree, Item.Node)
         and then Item.Part = (if Item.Node = Root then "____" else Name (A.Inner.Tree, Item.Node)));

      function Current_Node (A : Arena; Item : Node_Descriptor) return Boolean is
        (Current_Binding (A, Item) and then Item.Tag = Kind (A.Inner.Tree, Item.Node));
      function Field_Key_Matches (A : Arena; Key : Field_Key) return Boolean is
         F, R : Node_ID;
      begin
         if A.In_Progress or else A.State /= Ready
           or else AML_References.Kind (Key.Field) /= AML_References.Named_Cell
           or else AML_References.Kind (Key.Region) /= AML_References.Named_Cell
           or else not Named_Identity_Matches (A.Inner, Key.Field)
           or else not Named_Identity_Matches (A.Inner, Key.Region)
         then return False; end if;
         F := Node_ID (AML_References.Named_Node (Key.Field));
         R := Node_ID (AML_References.Named_Node (Key.Region));
         return A.Inner.Tree.Items (F).Object_Type = Region_Field_Object
           and then A.Inner.Tree.Items (R).Object_Type = Operation_Region_Object
           and then not A.Inner.Tree.Items (F).Initializing
           and then not A.Inner.Tree.Items (R).Initializing
           and then A.Inner.Tree.Items (F).Region_Field_Info.Region = R
           and then A.Inner.Tree.Items (F).Region_Field_Info.Incarnation =
             A.Inner.Tree.Items (R).Incarnation;
      end Field_Key_Matches;

      procedure Read_Region_Descriptor (A : Arena; Item : Node_Descriptor;
         Description : out Operation_Region_Attributes; Status : out Access_Status) is
      begin
         Description := (others => <>);
         if A.In_Progress then Status := Busy; return;
         elsif A.State /= Ready then Status := Wrong_Phase; return;
         elsif not Current_Node (A, Item) then Status := Invalid_Value; return;
         elsif Item.Tag /= Operation_Region_Object then Status := Wrong_Kind; return;
         end if;
         Description := A.Inner.Tree.Items (Item.Node).Operation_Info;
         Status := Available;
      end Read_Region_Descriptor;
      procedure Read_Field_Descriptor (A : Arena; Node : Node_ID;
         Description : out Field_Description; Key : out Field_Key;
         Status : out Access_Status) is
         Selected : Lookup_Result;
         Candidate : Field_Key := No_Field_Key;
         Success : Boolean;
         R : Node_ID;
      begin
         Description := No_Field_Description; Key := No_Field_Key;
         if A.In_Progress then Status := Busy; return;
         elsif A.State /= Ready then Status := Wrong_Phase; return;
         elsif Node > A.Inner.Tree.Used then Status := Invalid_Value; return;
         end if;
         Selected := Canonical_Node (A.Inner.Tree, Node);
         if Selected.Status /= Found then Status := Invalid_Value; return;
         elsif Selected.Node = Root or else
           A.Inner.Tree.Items (Selected.Node).Object_Type /= Region_Field_Object
         then Status := Wrong_Kind; return; end if;
         R := A.Inner.Tree.Items (Selected.Node).Region_Field_Info.Region;
         if R = Root or else R > A.Inner.Tree.Used then
            Status := Invalid_Value; return;
         end if;
         Make_Named_Identity (A.Inner, Natural (Selected.Node), Candidate.Field, Success);
         if not Success then Status := Invalid_Value; return; end if;
         Make_Named_Identity (A.Inner, Natural (R), Candidate.Region, Success);
         if not Success or else not Field_Key_Matches (A, Candidate) then
            Status := Invalid_Value; return;
         end if;
         Description := (Region => A.Inner.Tree.Items (R).Operation_Info,
           Field => A.Inner.Tree.Items (Selected.Node).Region_Field_Info);
         Key := Candidate; Status := Available;
      end Read_Field_Descriptor;

      function Present (A : Arena; Node : Node_ID) return Boolean is
        (not A.In_Progress and then A.State /= Uninitialized
         and then Node <= Owned.Node_Count (A.Inner) and then Owned.Present (A.Inner, Node));
      function Admission (A : Arena) return Access_Status is
        (if A.In_Progress then Busy elsif A.State /= Ready then Wrong_Phase else Available);
      function Audit (A : Arena) return Audit_Value is
        ((Tree => Owned.Snapshot (A.Inner), Import => A.Import, Preparation => A.Preparation));
      function Namespace_Equal (Current, Prior : Audit_Value) return Boolean is (Current.Tree = Prior.Tree);
      function Initialization_Frame (Current, Prior : Audit_Value) return Boolean is
        (Current.Import = Prior.Import and then Current.Preparation = Prior.Preparation
         and then AML_Namespace.Initialization_Frame (Current.Tree, Prior.Tree));
      function Table_Initialization_Frame
        (Current, Prior : Audit_Value; Input : AML_Table_Backing.State) return Boolean is
        (Current.Import = Prior.Import and then Current.Preparation = Prior.Preparation
         and then AML_Namespace.Table_Initialization_Frame (Current.Tree, Prior.Tree, Input));
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
         Node := Child (A.Inner.Tree, Parent, Part);
         Status := Available;
      end Find_Child;
      procedure Find_Object (A : Arena; Parent : Node_ID; Part : AML_Names.Segment;
         Result : out Lookup_Result; Status : out Access_Status) is
      begin
         Result := (Status => Not_Found);
         if A.In_Progress then Status := Busy; return;
         elsif A.State = Uninitialized then Status := Wrong_Phase; return;
         elsif not Present (A, Parent) then Status := Invalid_Value; return; end if;
         declare
            Canonical_Parent : constant Lookup_Result := Canonical_Node (A.Inner.Tree, Parent);
         begin
            if Canonical_Parent.Status /= Found then Status := Invalid_Value; return; end if;
            Result := Canonical_Child (A.Inner.Tree, Canonical_Parent.Node, Part);
         end;
         Status := Available;
      end Find_Object;
      procedure Resolve_Node (A : Arena; Scope : Node_Descriptor;
         Path : AML_Names.Name_Result; Item : out Node_Descriptor;
         Outcome : out Lookup_Status; Status : out Access_Status)
      is
         Located : Lookup_Result;
      begin
         Item := No_Node; Outcome := Not_Found;
         if A.In_Progress then Status := Busy; return;
         elsif A.State /= Ready then Status := Wrong_Phase; return;
         elsif not Current_Node (A, Scope) then Status := Invalid_Value; return;
         elsif Scope.Node /= Root and then not Declaration_Scope (Scope.Tag) then
            Status := Wrong_Kind; return;
         end if;
         Located := Resolve_Object (A.Inner.Tree, Scope.Node, Path);
         Outcome := Located.Status;
         Status := Available;
         if Located.Status = Found then
            Describe_Node (A, Located.Node, Item, Status);
         end if;
      end Resolve_Node;
      procedure Bind_Table_Region (A : in out Arena; Scope : Node_ID;
         Part : AML_Names.Segment; Region : Table_Region; Node : out Node_ID;
         Outcome : out Bind_Status; Status : out Access_Status; Owner : Node_ID := Root) is
      begin
         Node := Root; Outcome := Binding_Invalid;
         if A.In_Progress then Status := Busy; return;
         elsif A.State /= Loading or else A.Import not in Import_Undecided | Legacy_Import then Status := Wrong_Phase; return;
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
         elsif A.State /= Loading or else A.Import not in Import_Undecided | Legacy_Import then Status := Wrong_Phase; return;
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
      procedure Read_Processor_Metadata (A : Arena; Node : Node_ID;
         Metadata : out Processor_Attributes; Status : out Access_Status) is
      begin
         Metadata := (ID => 0, PBlock_Address => 0, PBlock_Length => 0);
         if A.In_Progress then Status := Busy; return;
         elsif A.State = Uninitialized then Status := Wrong_Phase; return;
         elsif Node = Root or else Node > Node_Count (A) or else not Present (A, Node) then
            Status := Invalid_Value; return;
         elsif Owned.Kind (A.Inner, Node) /= Processor_Object then
            Status := Wrong_Kind; return; end if;
         Metadata := Owned.Processor_Data (A.Inner, Node); Status := Available;
      end Read_Processor_Metadata;
      procedure Read_Power_Metadata (A : Arena; Node : Node_ID;
         Metadata : out Power_Attributes; Status : out Access_Status) is
      begin
         Metadata := (System_Level => 0, Order => 0);
         if A.In_Progress then Status := Busy; return;
         elsif A.State = Uninitialized then Status := Wrong_Phase; return;
         elsif Node = Root or else Node > Node_Count (A) or else not Present (A, Node) then
            Status := Invalid_Value; return;
         elsif Owned.Kind (A.Inner, Node) /= Power_Resource_Object then
            Status := Wrong_Kind; return; end if;
         Metadata := Owned.Power_Data (A.Inner, Node); Status := Available;
      end Read_Power_Metadata;
      procedure Reset (A : in out Arena; Status : out Access_Status) is
         Success : Boolean;
      begin
         if A.In_Progress or else Owned.Mutex_Registry.Active (A.Inner.Mutexes)
           or else Owned.Frame_Root_Count (A.Inner) /= 0
         then Status := Busy; return; end if;
         A.In_Progress := True;
         Owned.Reset (A.Inner, Success);
         if Success then A.State := Loading; A.Import := Import_Undecided;
            A.Preparation := Bare_Namespace; Status := Available;
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
         elsif A.State /= Loading or else A.Import not in Import_Undecided | Legacy_Import
           or else A.Preparation /= Bare_Namespace then Status := Wrong_Phase; return; end if;
         A.In_Progress := True;
         Owned.Load (A.Inner, Data, Width, Outcome);
         if Outcome = Loaded then A.Import := Legacy_Import; end if;
         A.In_Progress := False; Status := Available;
      end Load;
      function Current_Preparation (A : Arena) return Preparation_State is
        (A.Preparation);

      procedure Build_Firmware_Initial_State
        (Candidate : out State; Outcome : out Preparation_Outcome)
      is
         type Seed is record
            Part : AML_Names.Segment;
            Object_Type : Object_Kind;
         end record;
         Seeds : constant array (1 .. Firmware_Seed_Count) of Seed :=
           [("_PR_", Scope_Object), ("_GPE", Scope_Object),
            ("_SI_", Scope_Object), ("_SB_", Device_Object),
            ("_TZ_", Device_Object), ("_REV", Integer_Object), ("_OSI", Method_Object)];
         Firmware_Revision : constant AML_Decode.Integer_Value := 2;
         Node : Node_ID;
         Insert_Result : Insert_Status;
         Object_Ref : AML_Objects.Object_ID;
         Allocation : AML_Objects.Allocation_Status;
      begin
         Candidate := Empty;
         Outcome := Namespace_Capacity_Exceeded;
         for Item of Seeds loop
            Insert (Candidate, Root, Item.Part, Node, Insert_Result);
            if Insert_Result /= Inserted then return; end if;
            Candidate.Items (Node).Object_Type := Item.Object_Type;
            if Item.Object_Type = Method_Object then
               Candidate.Items (Node).Implementation := AML_Execute.Interface_Query;
               Candidate.Items (Node).Method_Flags := 1;
               Candidate.Items (Node).Method_Width := AML_Decode.Bits_64;
            elsif Item.Object_Type = Integer_Object then
               AML_Objects.New_Integer
                 (Candidate.Values, Firmware_Revision, Object_Ref, Allocation);
               if Allocation /= AML_Objects.Allocated then
                  Outcome := Value_Capacity_Exceeded; return;
               end if;
               Candidate.Items (Node).Object_Ref := Object_Ref;
            end if;
         end loop;
         Outcome := Firmware_Prepared;
      end Build_Firmware_Initial_State;

      function Firmware_Initial_State_Matches (Tree : State) return Boolean is
         Expected : State;
         Outcome : Preparation_Outcome;
      begin
         Build_Firmware_Initial_State (Expected, Outcome);
         return Outcome = Firmware_Prepared and then Tree = Expected;
      end Firmware_Initial_State_Matches;

      function Canonical_Firmware_Initial_State (A : Arena) return Boolean is
        (A.Preparation = Firmware_Initial_State
         and then Firmware_Initial_State_Matches (A.Inner.Tree));

      procedure Prepare_Firmware_Namespace (A : in out Arena;
         Outcome : out Preparation_Outcome; Status : out Access_Status)
      is
         Candidate : State;
      begin
         Outcome := Preparation_Rejected;
         if A.In_Progress then Status := Busy; return;
         elsif A.State /= Loading or else A.Import /= Import_Undecided
           or else A.Preparation /= Bare_Namespace then Status := Wrong_Phase; return;
         elsif Owned.Retained_Count (A.Inner) /= 0 or else Owned.Frame_Root_Count (A.Inner) /= 0
           or else A.Inner.Tree /= Empty then Status := Invalid_Value; return;
         end if;
         Build_Firmware_Initial_State (Candidate, Outcome);
         Status := Available;
         if Outcome = Firmware_Prepared then
            A.Inner.Tree := Candidate;
            A.Preparation := Firmware_Initial_State;
         end if;
      end Prepare_Firmware_Namespace;

      generic
         type Module_IO is limited private;
         with procedure Module_Field_Operation
           (E : in out Owned.Arena; IO : in out Module_IO; Ref : Reference;
            Width : AML_Decode.Integer_Width; For_Write : Boolean;
            Value : in out AML_Execute.Datum; Status : out AML_Execute.Execution_Status);
         with function Keep_Failed_Tree (IO : Module_IO) return Boolean;
      procedure Execute_Modules_With_Context (A : in out Arena; Loads : in out AML_Definition_Loads.State; Copies : in out AML_Copied_Tables.State;
         Registered : Boolean; IO : in out Module_IO;
         Input : aliased AML_Table_Backing.State; Budget : Natural;
         Report : out Module_Report; Status : out Access_Status);
      procedure Execute_Modules_With_Context (A : in out Arena; Loads : in out AML_Definition_Loads.State; Copies : in out AML_Copied_Tables.State;
         Registered : Boolean; IO : in out Module_IO;
         Input : aliased AML_Table_Backing.State; Budget : Natural;
         Report : out Module_Report; Status : out Access_Status) is
         use type Firmware_Tables.Admission;
         use type AML_Table_Code.View_Status;
         DSDT_Index : Natural := 0;
         Width : AML_Decode.Integer_Width := AML_Decode.Bits_64;
         Remaining : Natural := Budget;
         Local_Metrics : Collection_Statistics := A.Collections;
         Session_Active : Boolean := False;
         Executing_Table : AML_Execute.Load_Table_Token := AML_Execute.No_Load_Table;
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

         procedure Ignore_Module_Result (E : in out Owned.Arena; Result : AML_Execute.Execution_Result) is
            pragma Unreferenced (E, Result);
         begin null; end Ignore_Module_Result;
         procedure Run_Standalone is new Run_Context_Session
           (Module_IO, Module_Field_Operation, Ignore_Module_Result, Collect_Before);
         procedure Run_Registered is new Run_Registered_Context_Session
           (Module_IO, Module_Field_Operation, Ignore_Module_Result, Collect_Before);
         procedure Run (E : in out Owned.Arena; Backing : aliased AML_Table_Backing.State;
            Code : AML_Decode.Bytes; Source : aliased AML_Byte_Sources.Storage; Source_First : Natural;
            Width : AML_Decode.Integer_Width; Scope : Node_ID; Args : AML_Execute.Value_Arguments;
            Count, Fuel : Natural; Sync : AML_Execute.Sync_Level; Result : out AML_Execute.Execution_Result;
            Request : AML_Declaration_Contexts.Request) is
         begin
            if Registered then
               Run_Registered (E, IO, Loads, Copies, Backing, Code, Source, Source_First, Width,
                 Scope, Args, Count, Fuel, Sync, Result, Request, Deferred_Readiness => True,
                 Enclosing_Table => Executing_Table);
            else Run_Standalone (E, IO, Backing, Code, Source, Source_First, Width, Scope,
                 Args, Count, Fuel, Sync, Result, Request); end if;
         end Run;
         function Signature_At (Index : Positive) return Firmware_Tables.Signature is
            Offset : constant Natural := Input.Tables (Index).Offset;
         begin
            return [Character'Val (Input.Data.Bytes (Offset + 1)), Character'Val (Input.Data.Bytes (Offset + 2)),
              Character'Val (Input.Data.Bytes (Offset + 3)), Character'Val (Input.Data.Bytes (Offset + 4))];
         end Signature_At;
         procedure Execute_Selected (Index : Positive) is
            Prior : constant State := A.Inner.Tree;
            type Definition_Snapshot is array (Positive range <>) of Definition_Identity;
            Before : Definition_Snapshot (1 .. Loads.Capacity) := [others => No_Definition];
            procedure Close_Rolled_Back_Definitions is
               use type AML_Definition_Loads.Load_Phase;
               Accepted : Boolean;
            begin
               for I in Before'Range loop
                  if Before (I) /= AML_Definition_Loads.Definition (Loads, I) then
                     if Has_Definition (Before (I)) then raise Program_Error with "startup definition identity changed"; end if;
                     case AML_Definition_Loads.Phase (Loads, I) is
                        when AML_Definition_Loads.Loaded =>
                           AML_Definition_Loads.Require_Recovery (Loads, I, Accepted);
                           if not Accepted then raise Program_Error with "startup rollback record invariant"; end if;
                        when AML_Definition_Loads.Failed => null;
                        when others => raise Program_Error with "startup rollback pending definition invariant";
                     end case;
                  end if;
               end loop;
            end Close_Rolled_Back_Definitions;
            Prior_Definition, Definition : Definition_Identity;
            Definition_Opened : Boolean;
            Registry_Accepted : Boolean;
            use type AML_Definition_Loads.Load_Phase;
            Raw : AML_Execute.Execution_Result;
            Visited : AML_Table_Code.View_Status;
            procedure Consume (Backing : aliased AML_Table_Backing.State; Offset, Length : Natural) is
               Args : constant AML_Execute.Value_Arguments :=
                 [others => (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer)];
            begin
               if Length = 0 then
                  Run (A.Inner, Backing, [], Backing.Data, Offset, Width, Root, Args, 0, Remaining, 0, Raw,
                    AML_Declaration_Contexts.Module_Request);
               else
                  Run (A.Inner, Backing, Backing.Data.Bytes (Offset + 1 .. Offset + Length),
                    Backing.Data, Offset, Width, Root, Args, 0, Remaining, 0, Raw,
                    AML_Declaration_Contexts.Module_Request);
               end if;
            end Consume;
            procedure Visit is new AML_Table_Code.Visit_Body (Consume);
         begin
            if Registered then
               -- A table loaded earlier in the same startup owns its existing
               -- definition; the boot walk must not execute its body again.
               if AML_Definition_Loads.Phase (Loads, Index) = AML_Definition_Loads.Loaded then
                  Report.Completed := Report.Completed + 1; return;
               end if;
               for I in Before'Range loop Before (I) := AML_Definition_Loads.Definition (Loads, I); end loop;
               AML_Definition_Loads.Reserve (Loads, Index, Registry_Accepted);
               if not Registry_Accepted then
                  Report.Outcome := Module_Rejected; Report.Failed_Table := Index;
                  Report.Failure := AML_Execute.Duplicate_Name; A.Import := Module_Failed; return;
               end if;
            end if;
            Enter_Definition (A.Inner.Tree, Prior_Definition, Definition, Definition_Opened);
            if not Definition_Opened then
               if Registered then
                  AML_Definition_Loads.Finish (Loads, Index, False, Registry_Accepted);
                  if not Registry_Accepted then raise Program_Error with "boot unconsumed reservation invariant"; end if;
               end if;
               Report.Outcome := Module_Rejected; Report.Failed_Table := Index;
               Report.Failure := AML_Execute.Value_Limit; A.Import := Module_Failed; return;
            end if;
            pragma Assert (Has_Definition (Definition));
            if Registered then
               AML_Definition_Loads.Consume (Loads, Index, Definition, Registry_Accepted);
               if not Registry_Accepted then raise Program_Error with "boot definition registry invariant"; end if;
               Executing_Table := AML_Execute.Load_Table_Token (Index);
            end if;
            Visit (Input, Index, Visited);
            Executing_Table := AML_Execute.No_Load_Table;
            A.Inner.Tree.Current_Definition := Prior_Definition;
            if Visited /= AML_Table_Code.Body_Visited then
               Raw := (AML_Execute.Unsupported_Value, 0);
            end if;
            if Registered then
               AML_Definition_Loads.Finish (Loads, Index, Raw.Status = AML_Execute.Module_Completed, Registry_Accepted);
               if not Registry_Accepted then raise Program_Error with "boot definition finalization invariant"; end if;
            end if;
            Remaining := Remaining - Raw.Charged;
            Report.Charged := Budget - Remaining;
            -- Invocation serials and collector observations remain monotonic.
            -- Hardware-free attempts roll back Tree. Once a controlled provider
            -- may have observed an operation, retain the partial namespace and
            -- its identities for terminal recovery; effects are never replayed.
            if Raw.Status /= AML_Execute.Module_Completed then
               if Raw.Status = AML_Execute.Fatal_Requested then
                  Report.Fatal := (Present => True, Item => Raw.Fatal);
               end if;
               if Registered then Close_Rolled_Back_Definitions; end if;
               if not Keep_Failed_Tree (IO) then A.Inner.Tree := Prior; end if;
               Report.Outcome := Module_Rejected;
               Report.Failed_Table := Index;
               Report.Failure := (if Raw.Status in AML_Execute.Execution_Failure then Raw.Status else AML_Execute.Unsupported_Value);
               A.Import := Module_Failed;
            else Report.Completed := Report.Completed + 1; end if;
         end Execute_Selected;
      begin
         Report := (others => <>);
         if Registered and then (Input.Count > Loads.Capacity or else AML_Definition_Loads.Active (Loads)) then
            Status := Invalid_Value; return;
         end if;
         if A.In_Progress then Status := Busy; return;
         elsif A.State /= Loading or else A.Import /= Import_Undecided then Status := Wrong_Phase; return;
         elsif A.Inner.Notifications.Enabled or else A.Inner.Notifications.Used /= 0 then
            -- Module evaluation must never publish queue effects outside the
            -- namespace transaction. Interest can only be enabled after Ready.
            Status := Invalid_Value; return;
         elsif Owned.Retained_Count (A.Inner) /= 0 or else Owned.Frame_Root_Count (A.Inner) /= 0
           or else (if A.Preparation = Bare_Namespace then A.Inner.Tree /= Empty
             else not Firmware_Initial_State_Matches (A.Inner.Tree))
         then Status := Invalid_Value; return; end if;
         Status := Invalid_Value;
         if Input.Count = 0 or else Input.Count > Input.Table_Capacity then return; end if;
         for I in 1 .. Input.Count loop
            if not AML_Table_Backing.Valid_Span (Input, I) then return; end if;
            declare
               Span : constant AML_Table_Backing.Span := Input.Tables (I);
               Signature : constant Firmware_Tables.Signature := Signature_At (I);
               Header : constant Firmware_Tables.Table_Result := Firmware_Tables.Read_Table
                 (Input.Data.Bytes (Span.Offset + 1 .. Span.Offset + Span.Extent), Signature);
            begin
               if Header.Status /= Firmware_Tables.Accepted or else Header.Extent /= Span.Extent then return; end if;
               if Signature = "DSDT" then
                  if DSDT_Index /= 0 then return; end if;
                  DSDT_Index := I;
                  Width := (if Header.Revision < 2 then AML_Decode.Bits_32 else AML_Decode.Bits_64);
               end if;
            end;
         end loop;
         if DSDT_Index = 0 then return; end if;
         A.In_Progress := True; Session_Active := True; A.Import := Module_Active;
         Execute_Selected (DSDT_Index);
         for I in 1 .. Input.Count loop
            exit when A.Import = Module_Failed;
            if Signature_At (I) = "SSDT" then Execute_Selected (I); end if;
         end loop;
         if A.Import /= Module_Failed then
            A.Import := Module_Complete; Report.Outcome := Modules_Completed;
            Report.Failure := AML_Execute.No_Return;
         end if;
         A.Collections := Local_Metrics;
         A.In_Progress := False; Status := Available;
      end Execute_Modules_With_Context;
      function No_Module_Effects (IO : Null_Field_Context) return Boolean is
         pragma Unreferenced (IO);
      begin return False; end No_Module_Effects;
      procedure Execute_Modules_With_Loads is new Execute_Modules_With_Context
        (Null_Field_Context, Reject_Field_Operation, No_Module_Effects);
      procedure Execute_Modules (A : in out Arena; Input : aliased AML_Table_Backing.State;
         Budget : Natural; Report : out Module_Report; Status : out Access_Status) is
         No_Loads : AML_Definition_Loads.State (1);
         No_Copies : AML_Copied_Tables.State (0, 0);
         IO : Null_Field_Context;
      begin Execute_Modules_With_Loads (A, No_Loads, No_Copies, False, IO, Input, Budget, Report, Status); end Execute_Modules;
      procedure Execute_Registered_Modules (A : in out Arena; Loads : in out AML_Definition_Loads.State; Copies : in out AML_Copied_Tables.State;
         Input : aliased AML_Table_Backing.State; Budget : Natural; Report : out Module_Report; Status : out Access_Status) is
         IO : Null_Field_Context;
      begin Execute_Modules_With_Loads (A, Loads, Copies, True, IO, Input, Budget, Report, Status); end Execute_Registered_Modules;

      function Attempt_Definition (Attempt : Append_Attempt) return Definition_Identity is (Attempt.Definition);
      function Consumed (Attempt : Append_Attempt) return Boolean is (Attempt.Used);
      generic
         with function Stored_Definition (A : Arena; Loads : AML_Definition_Loads.State) return Definition_Identity;
         with procedure Consume_Definition (A : in out Arena; Loads : in out AML_Definition_Loads.State; ID : Definition_Identity);
      procedure Append_Module (A : in out Arena; Loads : in out AML_Definition_Loads.State; Copies : in out AML_Copied_Tables.State; Registered : Boolean;
         Input : aliased AML_Table_Backing.State; Index : Positive;
         Width : AML_Decode.Integer_Width; Budget : Natural;
         Report : out Append_Report; Status : out Access_Status;
         Root_At : Module_Root := Namespace_Module_Root);
      procedure Append_Module (A : in out Arena; Loads : in out AML_Definition_Loads.State; Copies : in out AML_Copied_Tables.State; Registered : Boolean;
         Input : aliased AML_Table_Backing.State; Index : Positive;
         Width : AML_Decode.Integer_Width; Budget : Natural;
         Report : out Append_Report; Status : out Access_Status;
         Root_At : Module_Root := Namespace_Module_Root) is
         use type Firmware_Tables.Admission;
         use type AML_Table_Code.View_Status;
         Local_Metrics : Collection_Statistics := A.Collections;
         Session_Active : Boolean := False;
         Roots : Root_Workspace;
         Reclaim_Scratch : AML_Objects.Reclamation.Workspace;
         Keep : Object_Root_Set;
         Raw : AML_Execute.Execution_Result;
         Visited : AML_Table_Code.View_Status;
         Tables, Members : Initialization_Report;
         Entry_Node : Node_ID := Root;
         Prior_Definition : Definition_Identity;
         Definition_Opened : Boolean;
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


         procedure Ignore_Result (E : in out Owned.Arena; Result : AML_Execute.Execution_Result) is
            pragma Unreferenced (E, Result);
         begin null; end Ignore_Result;
         procedure Run_Standalone is new Run_Owned_Session (Ignore_Result, Collect_Before);
         procedure Run_Registered is new Run_Registered_Context_Session
           (Null_Field_Context, Reject_Field_Operation, Ignore_Result, Collect_Before);
         IO : Null_Field_Context;
         procedure Run (E : in out Owned.Arena; Backing : aliased AML_Table_Backing.State;
            Code : AML_Decode.Bytes; Source : aliased AML_Byte_Sources.Storage; Source_First : Natural;
            Width : AML_Decode.Integer_Width; Scope : Node_ID; Args : AML_Execute.Value_Arguments;
            Count, Fuel : Natural; Sync : AML_Execute.Sync_Level; Result : out AML_Execute.Execution_Result;
            Request : AML_Declaration_Contexts.Request) is
         begin
            if Registered then
               Run_Registered (E, IO, Loads, Copies, Backing, Code, Source, Source_First, Width,
                 Scope, Args, Count, Fuel, Sync, Result, Request,
                 Enclosing_Table => AML_Execute.Load_Table_Token (Index));
            else Run_Standalone (E, Backing, Code, Source, Source_First, Width, Scope,
                 Args, Count, Fuel, Sync, Result, Request); end if;
         end Run;
         procedure Consume (Backing : aliased AML_Table_Backing.State; Offset, Length : Natural) is
            Args : constant AML_Execute.Value_Arguments :=
              [others => (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer)];
         begin
            if Length = 0 then
               Run (A.Inner, Backing, [], Backing.Data, Offset, Width, Entry_Node, Args, 0, Budget, 0, Raw,
                 AML_Declaration_Contexts.Module_Request);
            else
               Run (A.Inner, Backing, Backing.Data.Bytes (Offset + 1 .. Offset + Length),
                 Backing.Data, Offset, Width, Entry_Node, Args, 0, Budget, 0, Raw,
                 AML_Declaration_Contexts.Module_Request);
            end if;
         end Consume;
         procedure Visit is new AML_Table_Code.Visit_Body (Consume);
      begin
         Report := (others => <>); Status := Invalid_Value;
         if Has_Definition (Stored_Definition (A, Loads)) then
            Report.Outcome := Append_Already_Consumed; Report.Definition := Stored_Definition (A, Loads);
            Status := Wrong_Phase; return;
         end if;
         if A.In_Progress then Status := Busy; return;
         elsif A.State /= Ready or else A.Import /= Module_Complete then Status := Wrong_Phase; return;
         elsif A.Inner.Notifications.Enabled or else A.Inner.Notifications.Used /= 0
           or else Owned.Frame_Root_Count (A.Inner) /= 0 then return;
         end if;
         if Root_At.Kind = Selected_Root then
            if not Current_Node (A, Root_At.Scope) then return; end if;
            Entry_Node := Position (Root_At.Scope);
            if Entry_Node /= Root and then not Static_Scope (Kind (A.Inner.Tree, Entry_Node)) then return; end if;
         end if;
         if Input.Count = 0 or else Input.Count > Input.Table_Capacity or else Index > Input.Count
           or else not AML_Table_Backing.Valid_Span (Input, Index) then return; end if;
         declare
            Span : constant AML_Table_Backing.Span := Input.Tables (Index);
            Signature : constant Firmware_Tables.Signature :=
              [Character'Val (Input.Data.Bytes (Span.Offset + 1)), Character'Val (Input.Data.Bytes (Span.Offset + 2)),
               Character'Val (Input.Data.Bytes (Span.Offset + 3)), Character'Val (Input.Data.Bytes (Span.Offset + 4))];
            Header : constant Firmware_Tables.Table_Result := Firmware_Tables.Read_Table
              (Input.Data.Bytes (Span.Offset + 1 .. Span.Offset + Span.Extent), Signature);
         begin
            if Header.Status /= Firmware_Tables.Accepted or else Header.Extent /= Span.Extent
              or else Signature in "DSDT" | "SSDT" then return; end if;
         end;
         Status := Available;
         if Budget = 0 then Report.Outcome := Append_Budget_Refused; return; end if;
         Enter_Definition (A.Inner.Tree, Prior_Definition, Report.Definition, Definition_Opened);
         if not Definition_Opened then Status := Invalid_Value; Report.Failure := AML_Execute.Value_Limit; return; end if;
         Consume_Definition (A, Loads, Report.Definition); Report.Recovery_Needed := True;
         A.In_Progress := True; Session_Active := True;
         Visit (Input, Index, Visited);
         A.Inner.Tree.Current_Definition := Prior_Definition;
         if Visited /= AML_Table_Code.Body_Visited then Raw := (AML_Execute.Unsupported_Value, 0); end if;
         A.Collections := Local_Metrics;
         A.In_Progress := False;
         Report.Charged := Natural'Min (Raw.Charged, Budget);
         if Raw.Charged > Budget or else Raw.Status /= AML_Execute.Module_Completed then
            Report.Outcome := Append_Failed;
            Report.Failure := (if Raw.Status in AML_Execute.Execution_Failure then Raw.Status else AML_Execute.Unsupported_Value);
            if Raw.Status = AML_Execute.Fatal_Requested then Report.Fatal := (Present => True, Item => Raw.Fatal); end if;
            A.State := Loading; A.Import := Module_Failed; return;
         end if;
         Resolve_Static_Tables (A.Inner.Tree, Input, True, Tables);
         Owned.Initialize_Members (A.Inner, Members);
         Report.Initialization := (Bound => Tables.Bound + Members.Bound,
           Missing => Tables.Missing + Members.Missing, Unsupported => Tables.Unsupported + Members.Unsupported);
         if Report.Initialization.Missing /= 0 or else Report.Initialization.Unsupported /= 0 then
            Report.Outcome := Append_Initialization_Failed;
            A.State := Loading; A.Import := Module_Failed; return;
         end if;
         Report.Outcome := Append_Completed; Report.Recovery_Needed := False;
      end Append_Module;
      procedure Append_Ready_Module (A : in out Arena;
         Input : aliased AML_Table_Backing.State; Index : Positive;
         Width : AML_Decode.Integer_Width; Budget : Natural;
         Attempt : in out Append_Attempt; Report : out Append_Report; Status : out Access_Status;
         Root_At : Module_Root := Namespace_Module_Root) is
         function Prior (Unused : Arena; Registry : AML_Definition_Loads.State) return Definition_Identity is
            pragma Unreferenced (Unused, Registry);
         begin return Attempt.Definition; end Prior;
         procedure Consume (Unused : in out Arena; Registry : in out AML_Definition_Loads.State; ID : Definition_Identity) is
            pragma Unreferenced (Unused, Registry);
         begin Attempt.Definition := ID; Attempt.Used := True; end Consume;
         procedure Append is new Append_Module (Prior, Consume);
         No_Loads : AML_Definition_Loads.State (1);
         No_Copies : AML_Copied_Tables.State (0, 0);
      begin Append (A, No_Loads, No_Copies, False, Input, Index, Width, Budget, Report, Status, Root_At); end Append_Ready_Module;
      procedure Append_Registered_Module (A : in out Arena; Loads : in out AML_Definition_Loads.State; Copies : in out AML_Copied_Tables.State;
         Input : aliased AML_Table_Backing.State; Index : Positive;
         Width : AML_Decode.Integer_Width; Budget : Natural;
         Existing_Index : out Natural; Report : out Append_Report; Status : out Access_Status;
         Root_At : Module_Root := Namespace_Module_Root) is
         use type AML_Definition_Loads.Load_Phase;
         Accepted : Boolean;
         function Prior (Unused : Arena; Registry : AML_Definition_Loads.State) return Definition_Identity is
            pragma Unreferenced (Unused);
         begin return AML_Definition_Loads.Definition (Registry, Index); end Prior;
         procedure Consume (Unused : in out Arena; Registry : in out AML_Definition_Loads.State; ID : Definition_Identity) is
            pragma Unreferenced (Unused);
            Consumed_OK : Boolean;
         begin
            AML_Definition_Loads.Consume (Registry, Index, ID, Consumed_OK);
            if not Consumed_OK then raise Program_Error with "definition registry consumption invariant"; end if;
         end Consume;
         procedure Append is new Append_Module (Prior, Consume);
         function Equal_Bytes (Left, Right : Positive) return Boolean is
            L : constant AML_Table_Backing.Span := Input.Tables (Left);
            R : constant AML_Table_Backing.Span := Input.Tables (Right);
         begin
            if L.Extent /= R.Extent then return False; end if;
            for Offset in 1 .. L.Extent loop
               if Input.Data.Bytes (L.Offset + Offset) /= Input.Data.Bytes (R.Offset + Offset) then return False; end if;
            end loop;
            return True;
         end Equal_Bytes;
      begin
         Existing_Index := 0; Report := (others => <>); Status := Wrong_Phase;
         if A.In_Progress then Status := Busy; return;
         elsif A.State /= Ready or else A.Import /= Module_Complete then return; end if;
         if Input.Count > Loads.Capacity or else Index > Input.Count then Status := Invalid_Value; return; end if;
         for I in 1 .. Input.Count loop
            if not AML_Table_Backing.Valid_Span (Input, I) then Status := Invalid_Value; return; end if;
         end loop;
         declare
            Offset : constant Natural := Input.Tables (Index).Offset;
            Signature : constant Firmware_Tables.Signature :=
              [Character'Val (Input.Data.Bytes (Offset + 1)), Character'Val (Input.Data.Bytes (Offset + 2)),
               Character'Val (Input.Data.Bytes (Offset + 3)), Character'Val (Input.Data.Bytes (Offset + 4))];
         begin if Signature in "DSDT" | "SSDT" then Status := Wrong_Kind; return; end if; end;
         if AML_Definition_Loads.Active (Loads) then Status := Busy; return; end if;
         for I in 1 .. Input.Count loop
            if AML_Definition_Loads.Phase (Loads, I) = AML_Definition_Loads.Loaded and then Equal_Bytes (Index, I) then
               Existing_Index := I; Report.Definition := AML_Definition_Loads.Definition (Loads, I);
               Status := Available; return;
            end if;
         end loop;
         AML_Definition_Loads.Reserve (Loads, Index, Accepted);
         if not Accepted then Status := Wrong_Phase; return; end if;
         Append (A, Loads, Copies, True, Input, Index, Width, Budget, Report, Status, Root_At);
         AML_Definition_Loads.Finish (Loads, Index, Report.Outcome = Append_Completed, Accepted);
         if not Accepted then raise Program_Error with "definition registry finalization invariant"; end if;
      end Append_Registered_Module;
      procedure Require_Registered_Recovery (A : in out Arena; Loads : in out AML_Definition_Loads.State; Copies : in out AML_Copied_Tables.State;
         Index : Positive; Status : out Access_Status) is
         pragma Unreferenced (Copies);
         use type AML_Definition_Loads.Load_Phase;
         Accepted : Boolean;
      begin
         Status := Wrong_Phase;
         if Index > Loads.Capacity then Status := Invalid_Value; return; end if;
         if AML_Definition_Loads.Phase (Loads, Index) /= AML_Definition_Loads.Loaded then return; end if;
         Require_Module_Recovery (A, Status);
         if Status = Available then
            AML_Definition_Loads.Require_Recovery (Loads, Index, Accepted);
            if not Accepted then raise Program_Error with "definition registry recovery invariant"; end if;
         end if;
      end Require_Registered_Recovery;

      procedure Require_Module_Recovery (A : in out Arena; Status : out Access_Status) is
      begin
         if A.In_Progress then Status := Busy; return;
         elsif A.State /= Ready or else A.Import /= Module_Complete then Status := Wrong_Phase; return;
         end if;
         A.State := Loading; A.Import := Module_Failed; Status := Available;
      end Require_Module_Recovery;
      procedure Store_Parameter_Data (A : in out Arena; Target : Node_Descriptor;
         Item : Argument; Width : AML_Decode.Integer_Width; Budget : Natural;
         Charged : out Natural; Outcome : out AML_Execute.Execution_Status;
         Status : out Access_Status)
      is
         Value : AML_Execute.Datum := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Ref : Reference;
         Accepted : Boolean;
         Located : Lookup_Result;
         Read_Status : AML_Execute.Execution_Status;
      begin
         Charged := 0; Outcome := AML_Execute.Unsupported_Value;
         Status := Admission (A); if Status /= Available then return; end if;
         if A.Inner.Field_Uncertain.Present then Outcome := AML_Execute.Field_Indeterminate; return; end if;
         if not Current_Binding (A, Target) then Status := Invalid_Value; return; end if;
         Located := Canonical_Node (A.Inner.Tree, Target.Node);
         if Located.Status /= Found then Status := Invalid_Value; return; end if;
         if Kind (A.Inner.Tree, Located.Node) not in
           Integer_Object | String_Object | Buffer_Object | Package_Object |
           Reference_Object | Metadata_Value_Object | Uninitialized_Name_Object | Buffer_Field_Object
         then Status := Wrong_Kind; return; end if;
         if Budget < Parameter_Store_Charge then Outcome := AML_Execute.Budget_Exceeded; return; end if;
         case Item.Kind is
            when Immediate_Argument =>
               Value := (AML_Execute.Integer_Datum, Item.Number, AML_Decode.Ordinary_Integer);
            when Retained_Argument =>
               Owned.Read_Retained (A.Inner, Item.Handle.Root, Value, Read_Status);
               if Read_Status /= AML_Execute.Returned then Status := Invalid_Value; return; end if;
         end case;
         Owned.Make_Named_Reference (A.Inner, Located.Node, Ref, Accepted);
         if not Accepted then Status := Invalid_Value; return; end if;
         Charged := Parameter_Store_Charge;
         A.In_Progress := True;
         Owned.Store_Reference_Value (A.Inner, Ref, Width, Value, Outcome, AML_Execute.Direct_Target);
         A.In_Progress := False;
      end Store_Parameter_Data;
      procedure Seal_Common (A : in out Arena; Input : aliased AML_Table_Backing.State;
         Have_Input : Boolean; Report : out Initialization_Report; Status : out Access_Status) is
         Table_Report, Member_Report : Initialization_Report;
      begin
         Report := (others => 0);
         if A.In_Progress then Status := Busy; return;
         elsif A.State /= Loading or else A.Import not in Import_Undecided | Legacy_Import | Module_Complete then Status := Wrong_Phase; return; end if;
         A.In_Progress := True;
         Resolve_Static_Tables (A.Inner.Tree, Input, Have_Input, Table_Report);
         Owned.Initialize_Members (A.Inner, Member_Report);
         Report := (Bound => Table_Report.Bound + Member_Report.Bound,
           Missing => Table_Report.Missing + Member_Report.Missing,
           Unsupported => Table_Report.Unsupported + Member_Report.Unsupported);
         A.State := Ready; A.In_Progress := False; Status := Available;
      end Seal_Common;
      procedure Seal (A : in out Arena; Report : out Initialization_Report;
         Status : out Access_Status) is
         No_Tables : aliased AML_Table_Backing.State (1, 1);
      begin
         Seal_Common (A, No_Tables, False, Report, Status);
      end Seal;
      procedure Seal_With_Tables (A : in out Arena; Input : aliased AML_Table_Backing.State;
         Report : out Initialization_Report; Status : out Access_Status) is
      begin
         Seal_Common (A, Input, True, Report, Status);
      end Seal_With_Tables;
      procedure Invoke_With_Loads (A : in out Arena; Loads : in out AML_Definition_Loads.State; Copies : in out AML_Copied_Tables.State;
         Registered : Boolean; Input : aliased AML_Table_Backing.State;
         Node : Node_ID; Args : Arguments; Count : Argument_Count;
         Budget : Natural; Outcome : out Result; Status : out Access_Status) is
         use type AML_Definition_Loads.Load_Phase;
         Raw_Args : AML_Execute.Value_Arguments :=
           [others => (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer)];
         Raw : AML_Execute.Execution_Result;
         Root : Retained_Root;
         Retention : Invocation_Retention_Status;
         Read_Status : AML_Execute.Execution_Status;
         subtype Failure_Status is AML_Execute.Payload_Free_Failure;
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
         procedure Run_Registered is new Invoke_Registered_Retained (Collect_Before);
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
         if Registered then
            Run_Registered (A.Inner, Loads, Copies, Input, Node, Raw_Args, Count, Budget, Raw, Root, Retention);
         else
            Run (A.Inner, Input, Node, Raw_Args, Count, Budget, Raw, Root, Retention);
         end if;
         A.Collections := Local_Metrics;
         A.In_Progress := False;
         if Registered then
            for I in 1 .. Loads.Capacity loop
               if AML_Definition_Loads.Phase (Loads, I) = AML_Definition_Loads.Failed then
                  A.State := Loading; A.Import := Module_Failed;
               end if;
            end loop;
         end if;
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
            when AML_Execute.Metadata_Returned =>
               Outcome := (AML_Execute.Metadata_Returned, Raw.Charged, (Root => Root));
            when AML_Execute.Fatal_Requested => Outcome := (AML_Execute.Fatal_Requested, Raw.Charged, Raw.Fatal);
            when AML_Execute.Module_Completed => Outcome := (AML_Execute.Unsupported_Value, Raw.Charged);
            when others => Outcome := (Failure_Status (Raw.Status), Raw.Charged);
         end case;
      end Invoke_With_Loads;
      procedure Invoke (A : in out Arena; Input : aliased AML_Table_Backing.State;
         Node : Node_ID; Args : Arguments; Count : Argument_Count;
         Budget : Natural; Outcome : out Result; Status : out Access_Status) is
         No_Loads : AML_Definition_Loads.State (1);
         No_Copies : AML_Copied_Tables.State (0, 0);
      begin Invoke_With_Loads (A, No_Loads, No_Copies, False, Input, Node, Args, Count, Budget, Outcome, Status); end Invoke;
      procedure Invoke_Registered (A : in out Arena; Loads : in out AML_Definition_Loads.State; Copies : in out AML_Copied_Tables.State;
         Input : aliased AML_Table_Backing.State; Node : Node_ID; Args : Arguments; Count : Argument_Count;
         Budget : Natural; Outcome : out Result; Status : out Access_Status) is
      begin Invoke_With_Loads (A, Loads, Copies, True, Input, Node, Args, Count, Budget, Outcome, Status); end Invoke_Registered;

      generic
         type Evaluation_Context is limited private;
         Region_Read_Enabled : Boolean;
         with procedure Invoke_Method
           (A : in out Arena; IO : in out Evaluation_Context;
            Input : aliased AML_Table_Backing.State; Node : Node_ID;
            Budget : Natural; Outcome : out Result; Status : out Access_Status);
         with procedure Read_Region
           (E : in out Owned.Arena; IO : in out Evaluation_Context;
            Ref : Reference; Width : AML_Decode.Integer_Width;
            Value : in out AML_Execute.Datum; Status : out AML_Execute.Execution_Status);
         with procedure Read_Table_Binding
           (Tree : in out State; Input : aliased AML_Table_Backing.State; Scope : Natural;
            Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width;
            Purpose : AML_Execute.Binding_Purpose; Binding : out AML_Execute.Binding_Result)
           is Lookup_With_Tables;
      procedure Evaluate_With_Context
        (A : in out Arena; IO : in out Evaluation_Context;
         Input : aliased AML_Table_Backing.State; Item : Node_Descriptor;
         Width : AML_Decode.Integer_Width; Budget : Natural;
         Outcome : out Result; Status : out Access_Status);
      procedure Evaluate_With_Context (A : in out Arena; IO : in out Evaluation_Context;
         Input : aliased AML_Table_Backing.State;
         Item : Node_Descriptor; Width : AML_Decode.Integer_Width; Budget : Natural;
         Outcome : out Result; Status : out Access_Status)
      is
         use type AML_Execute.Binding_Status;
         use type Pins.Result_Status;
         Node : constant Node_ID := Item.Node;
         Path : constant AML_Names.Name_Result (AML_Names.Accepted) :=
           (Kind => AML_Names.Accepted, Rooted => False, Parents => 0,
            Count => 1, Parts => [1 => Item.Part, others => "____"], Consumed => 4);
         Located : Lookup_Result;
         Binding : AML_Execute.Binding_Result;
         Read_Status : AML_Execute.Execution_Status;
         Value : AML_Execute.Datum := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Reserved : Retained_Root := No_Retained_Root;
         Retention : Retain_Status;
         Released : Release_Status;
         Pin_Status : Pins.Result_Status;
         Handle : Value_Handle := No_Value;
         Keep_Result : Boolean := False;
         Session_Active : Boolean := False;
         Local_Metrics : Collection_Statistics := A.Collections;
         Region_Gate : Allocation_Hook_Status;
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
           (E : in out Owned.Arena; Extra : Root_Values; Gate : out Allocation_Hook_Status)
         is
            Trace : Root_Trace_Status;
            Reclaimed : AML_Objects.Reclamation.Reclaim_Status;
            Before, After : AML_Objects.Usage;
            use type AML_Objects.Reclamation.Reclaim_Status;
         begin
            Add (Local_Metrics.Attempted);
            Gate := Invalid_Census;
            -- This is an exclusive external resolution, with no executing
            -- method or synthetic frame. Existing retained and namespace
            -- roots plus authenticated field extras form its actual census.
            if not Session_Active or else not Owned.Valid (E)
              or else Frame_Root_Count (E) /= 0
              or else not Named_Identity_Matches (E, Item.Identity)
            then Add (Local_Metrics.Rejected); return; end if;
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
         package Readers is new Named_Readers (Collect_Before, Read_Table_Binding);
      begin
         Outcome := (Status => AML_Execute.No_Return, Charged => 0);
         Status := Admission (A); if Status /= Available then return; end if;
         if not Current_Binding (A, Item) or else Node = Root then
            Status := Invalid_Value; return;
         end if;
         if Owned.Kind (A.Inner, Node) = Method_Object then
            Invoke_Method (A, IO, Input, Node, Budget, Outcome, Status);
            return;
         end if;
         if Budget < Named_Evaluation_Charge then
            Outcome := (Status => AML_Execute.Budget_Exceeded, Charged => 0); return;
         end if;
         if Owned.Kind (A.Inner, Node) in Integer_Object | String_Object | Buffer_Object |
           Package_Object | Reference_Object | Metadata_Value_Object
         then
            Observe_Named_Value (A, Node, Outcome, Status);
            Outcome.Charged := Named_Evaluation_Charge;
            return;
         end if;
         Outcome := (Status => AML_Execute.Unsupported_Value, Charged => Named_Evaluation_Charge);
         if Owned.Kind (A.Inner, Node) not in Buffer_Field_Object | Table_Field_Object |
           Region_Field_Object | Uninitialized_Table_Field_Object
         then return; end if;
         Located := Resolve_Object (A.Inner.Tree, Item.Parent_Node, Path);
         if Located.Status /= Found or else Located.Node /= Node then
            Status := Invalid_Value; return;
         end if;
         if Frame_Root_Count (A.Inner) /= 0 then Status := Busy; return; end if;
         if Region_Read_Enabled and then Owned.Kind (A.Inner, Node) = Region_Field_Object then
            -- Reserve the external result root before an allocating hardware
            -- read. Scalar fields keep the existing nonallocating path.
            if AML_Decode.Integer_Value (A.Inner.Tree.Items (Node).Region_Field_Info.Bits) >
              AML_Decode.Integer_Value (Field_Integer_Bits (Width)) then
               Owned.Retain (A.Inner, Value, Reserved, Retention);
               case Retention is
                  when Owned.Root_Limit => Status := Root_Limit; return;
                  when Owned.Identity_Exhausted => Status := Identity_Exhausted; return;
                  when Owned.Invalid_Value => Status := Invalid_Value; return;
                  when Retained => null;
               end case;
            end if;
            A.In_Progress := True; Session_Active := True;
            Region_Gate := Allocation_Allowed;
            if Reserved /= No_Retained_Root then
               Collect_Before (A.Inner, [], Region_Gate);
            end if;
            if Region_Gate = Allocation_Allowed then
               Read_Region (A.Inner, IO, Item.Identity, Width, Value, Read_Status);
            else Read_Status := AML_Execute.Unsupported_Value; end if;
            A.In_Progress := False; Session_Active := False;
            A.Collections := Local_Metrics;
            if Read_Status = AML_Execute.Returned and then
              Value.Value_Kind = AML_Execute.Integer_Datum
            then
               Outcome := (AML_Execute.Returned, Named_Evaluation_Charge,
                  Value.Number, Value.Origin);
            elsif Read_Status = AML_Execute.Returned
              and then Value.Value_Kind = AML_Execute.Object_Datum
              and then Reserved /= No_Retained_Root
              and then Value.Object.ID = AML_References.Source (Value.Object.Source)
              and then Has_Source (A.Inner, Value.Object.Source)
            then
               Pins.Replace (A.Inner.Retention, Reserved.Pin, Value, Pin_Status);
               if Pin_Status = Pins.Ready then
                  Handle.Root := Reserved;
                  Outcome := (AML_Execute.Object_Returned, Named_Evaluation_Charge, Handle);
                  Keep_Result := True;
               else Status := Invalid_Value; end if;
            else
               declare
                  Failure : constant AML_Execute.Payload_Free_Failure :=
                    (if Read_Status in AML_Execute.Payload_Free_Failure then Read_Status
                     else AML_Execute.Unsupported_Value);
               begin
                  Outcome := (Failure, Named_Evaluation_Charge);
               end;
            end if;
            if Reserved /= No_Retained_Root and then not Keep_Result then
               Owned.Release (A.Inner, Reserved, Released);
               if Released /= Owned.Released then
                  raise Program_Error with "region evaluation reservation invariant";
               end if;
            end if;
            return;
         end if;
         Owned.Retain (A.Inner, Value, Reserved, Retention);
         case Retention is
            when Owned.Root_Limit => Status := Root_Limit; return;
            when Owned.Identity_Exhausted => Status := Identity_Exhausted; return;
            when Owned.Invalid_Value => Status := Invalid_Value; return;
            when Retained => null;
         end case;
         A.In_Progress := True; Session_Active := True;
         Readers.Lookup (A.Inner, Input, Item.Parent_Node, Path, Width,
           AML_Execute.Evaluate_Binding, Binding);
         case Binding.Status is
            when AML_Execute.Integer_Binding =>
               Outcome := (AML_Execute.Returned, Named_Evaluation_Charge, Binding.Value, Binding.Origin);
            when AML_Execute.Non_Integer_Binding =>
               Value := (AML_Execute.Object_Datum, Binding.Object);
               if Binding.Object.ID = AML_References.Source (Binding.Object.Source)
                 and then Has_Source (A.Inner, Binding.Object.Source)
               then
                  Pins.Replace (A.Inner.Retention, Reserved.Pin, Value, Pin_Status);
                  if Pin_Status = Pins.Ready then
                     Handle.Root := Reserved;
                     Outcome := (AML_Execute.Object_Returned, Named_Evaluation_Charge, Handle);
                     Keep_Result := True;
                  else Status := Invalid_Value; end if;
               else Status := Invalid_Value; end if;
            when AML_Execute.Failed_Binding =>
               declare
                  Failure : constant AML_Execute.Payload_Free_Failure :=
                    (if Binding.Failure in AML_Execute.Payload_Free_Failure then Binding.Failure
                     else AML_Execute.Unsupported_Value);
               begin
                  Outcome := (Failure, Named_Evaluation_Charge);
               end;
            when others => null;
         end case;
         if not Keep_Result then
            Owned.Release (A.Inner, Reserved, Released);
            if Released /= Owned.Released then
               raise Program_Error with "named evaluation reservation invariant";
            end if;
         end if;
         A.Collections := Local_Metrics;
         A.In_Progress := False;
      end Evaluate_With_Context;

      package body Field_IO is
         use type ACPI_FADT.Transactions.Access_Width;
         use type AML_Field_Plans.Datum_Count;
         use type AML_Field_Plans.Read_Form;
         use type AML_Field_Access.Target;
         use type AML_Field_Layout.Field_Geometry;
         function Key_Of (Item : Binding) return Field_Key is
           ((Field => Item.Field, Region => Item.Region));
         use type AML_Field_Layout.Register_Geometry;
         use type AML_Field_Layout.Region_Geometry;
         use type AML_Field_Access.Observation;
         function Key_Matches (E : Owned.Arena; Key : Field_Key) return Boolean is
            F, R : Node_ID;
         begin
            if AML_References.Kind (Key.Field) /= AML_References.Named_Cell
              or else AML_References.Kind (Key.Region) /= AML_References.Named_Cell
              or else not Named_Identity_Matches (E, Key.Field)
              or else not Named_Identity_Matches (E, Key.Region) then return False; end if;
            F := Node_ID (AML_References.Named_Node (Key.Field));
            R := Node_ID (AML_References.Named_Node (Key.Region));
            return E.Tree.Items (F).Object_Type = Region_Field_Object
              and then E.Tree.Items (R).Object_Type = Operation_Region_Object
              and then not E.Tree.Items (F).Initializing and then not E.Tree.Items (R).Initializing
              and then E.Tree.Items (F).Region_Field_Info.Region = R
              and then E.Tree.Items (F).Region_Field_Info.Incarnation = E.Tree.Items (R).Incarnation;
         end Key_Matches;
         function Geometry_Of (E : Owned.Arena; Key : Field_Key)
           return AML_Field_Layout.Field_Geometry is
            F : constant Region_Field_Attributes :=
              E.Tree.Items (Node_ID (AML_References.Named_Node (Key.Field))).Region_Field_Info;
            R : constant Operation_Region_Attributes :=
              E.Tree.Items (Node_ID (AML_References.Named_Node (Key.Region))).Operation_Info;
         begin
            return (Region => (Space => AML_Decode.Byte (R.Space), Base => R.Address, Length => R.Length),
              Bit_Offset => AML_Decode.Integer_Value (F.Offset),
              Bit_Length => AML_Decode.Integer_Value (F.Bits), Flags => F.Raw_Flags,
              Effective_Access => F.Access_Type, Attribute => F.Attribute,
              Access_Length => F.Access_Length, others => <>);
         end Geometry_Of;
         procedure Bind_Owned
           (E : Owned.Arena; IO : IO_Context; Key : Field_Key; Target : AML_Field_Access.Target;
            Item : out Binding; Status : out Access_Status) is
            Geometry : AML_Field_Layout.Register_Geometry;
            Allowed : Boolean;
         begin
            Item := No_Binding;
            Status := Invalid_Value;
            if E.Field_Uncertain.Present or else Pending (IO)
              or else not Key_Matches (E, Key) then return; end if;
            Describe_Target (IO, Target, Geometry, Allowed);
            if not Allowed or else not AML_Field_Layout.Admitted (Geometry_Of (E, Key), Geometry)
            then return; end if;
            Item := (Field => Key.Field, Region => Key.Region, Target => Target,
                     Geometry => Geometry, others => <>);
            if Geometry.Width = ACPI_FADT.Transactions.Qword_Access then
               -- Use the common datum path for 64-bit masking and the Buffer
               -- result required when this field exceeds the AML integer width.
               declare
                  Field : constant AML_Field_Layout.Field_Geometry := Geometry_Of (E, Key);
                  Plan : constant AML_Field_Plans.Plan := AML_Field_Plans.Prepare (Field);
                  First : constant AML_Field_Plans.Datum_Position := AML_Field_Plans.Datum_Position'First;
               begin
                  if not AML_Field_Plans.Valid (Plan) or else AML_Field_Plans.Count (Plan) /= 1
                    or else not AML_Field_Plans.Matches (Plan, First, Geometry)
                  then Item := No_Binding; return; end if;
                  Item.Read_Count := 1; Item.Writes_Enabled := True;
                  Item.Read_Field := Field;
                  Item.Read_Targets (First) := Target;
                  Item.Read_Geometries (First) := Geometry;
               end;
            end if;
            Status := Available;
         end Bind_Owned;
         procedure Bind
           (A : Arena; IO : IO_Context; Key : Field_Key; Target : AML_Field_Access.Target;
            Item : out Binding; Status : out Access_Status) is
         begin
            Item := No_Binding;
            Status := Admission (A); if Status /= Available then return; end if;
            if not Field_Key_Matches (A, Key) then Status := Invalid_Value; return; end if;
            Bind_Owned (A.Inner, IO, Key, Target, Item, Status);
         end Bind;
         procedure Clear (Items : out Binding_Set) is
         begin
            Items := (others => <>);
         end Clear;
         procedure Add_Region_Register
           (A : Arena; IO : IO_Context; Region : Node_Descriptor;
            Target : AML_Field_Access.Target; Allow_Read, Allow_Write : Boolean;
            Items : in out Binding_Set; Status : out Access_Status) is
            Geometry : AML_Field_Layout.Register_Geometry;
            Whole : AML_Field_Layout.Field_Geometry;
            R : Operation_Region_Attributes;
            Identity : Reference;
            Allowed : Boolean;
            Access_Code : AML_Decode.Byte;
         begin
            Status := Admission (A); if Status /= Available then return; end if;
            Status := Invalid_Value;
            if (not Allow_Read and then not Allow_Write)
              or else Items.Register_Count = Binding_Count'Last
              or else A.Inner.Field_Uncertain.Present or else Pending (IO)
              or else not Current_Node (A, Region)
              or else Region.Tag /= Operation_Region_Object
              or else A.Inner.Tree.Items (Region.Node).Initializing then return; end if;
            if (Items.Count /= 0 or else Items.Register_Count /= 0) and then
              (Items.Owner /= A.Inner.Token or else Items.Provider /= Target.Provider) then return; end if;
            Make_Named_Identity (A.Inner, Natural (Region.Node), Identity, Allowed);
            if not Allowed then return; end if;
            for I in Binding_Count range 1 .. Items.Register_Count loop
               if Items.Registers (I).Region = Identity and then Items.Registers (I).Target = Target then return; end if;
            end loop;
            Describe_Target (IO, Target, Geometry, Allowed);
            if not Allowed then return; end if;
            R := A.Inner.Tree.Items (Region.Node).Operation_Info;
            if Geometry.Region /= AML_Field_Layout.Region_Geometry'
              (Space => AML_Decode.Byte (R.Space), Base => R.Address, Length => R.Length)
              or else Geometry.Byte_Offset > AML_Decode.Integer_Value'Last / AML_Field_Layout.Bits_Per_Byte then return; end if;
            Access_Code := AML_Decode.Byte (ACPI_FADT.Transactions.Access_Width'Pos (Geometry.Width) + 1);
            Whole := (Region => Geometry.Region,
              Bit_Offset => Geometry.Byte_Offset * AML_Field_Layout.Bits_Per_Byte,
              Bit_Length => AML_Decode.Integer_Value (AML_Field_Layout.Bits_Per_Byte *
                ACPI_FADT.Transactions.Octets (Geometry.Width)),
              Flags => Access_Code, Effective_Access => Access_Code, others => <>);
            if not AML_Field_Layout.Admitted (Whole, Geometry) then return; end if;
            if Allow_Read then
               Authorize_Target (IO, Target, False, Allowed); if not Allowed then return; end if;
            end if;
            if Allow_Write then
               Authorize_Target (IO, Target, True, Allowed); if not Allowed then return; end if;
            end if;
            Items.Register_Count := Items.Register_Count + 1;
            Items.Registers (Items.Register_Count) := (Identity, Target, Geometry, Allow_Read, Allow_Write);
            Items.Owner := A.Inner.Token; Items.Provider := Target.Provider;
            Status := Available;
         end Add_Region_Register;
         procedure Add_Binding
           (A : Arena; IO : IO_Context; Key : Field_Key;
            Target : AML_Field_Access.Target; Items : in out Binding_Set;
            Status : out Access_Status) is
            Candidate : Binding;
         begin
            Status := Admission (A); if Status /= Available then return; end if;
            Status := Invalid_Value;
            if Items.Count = Binding_Count'Last then return; end if;
            if (Items.Count /= 0 or else Items.Register_Count /= 0) and then
              (Items.Owner /= A.Inner.Token or else Items.Provider /= Target.Provider)
            then return; end if;
            for I in Binding_Count range 1 .. Items.Count loop
               if Items.Entries (I).Field = Key.Field then return; end if;
            end loop;
            Bind (A, IO, Key, Target, Candidate, Status);
            if Status /= Available then return; end if;
            Items.Entries (Items.Count + 1) := Candidate;
            Items.Count := Items.Count + 1;
            Items.Owner := A.Inner.Token;
            Items.Provider := Target.Provider;
         end Add_Binding;
         procedure Add_Read_Binding
           (A : Arena; IO : IO_Context; Key : Field_Key;
            Targets : AML_Field_Targets.Vector; Count : AML_Field_Plans.Datum_Count;
            Items : in out Binding_Set; Status : out Access_Status) is
            Candidate : Binding := No_Binding;
            Field : AML_Field_Layout.Field_Geometry;
            Plan : AML_Field_Plans.Plan;
            Allowed : Boolean;
         begin
            Status := Admission (A); if Status /= Available then return; end if;
            Status := Invalid_Value;
            if Count = 0 or else Items.Count = Binding_Count'Last
              or else A.Inner.Field_Uncertain.Present or else Pending (IO)
              or else not Field_Key_Matches (A, Key)
            then return; end if;
            if (Items.Count /= 0 or else Items.Register_Count /= 0) and then
              (Items.Owner /= A.Inner.Token
               or else Items.Provider /= Targets (AML_Field_Plans.Datum_Position'First).Provider)
            then return; end if;
            for I in Binding_Count range 1 .. Items.Count loop
               if Items.Entries (I).Field = Key.Field then return; end if;
            end loop;
            Field := Geometry_Of (A.Inner, Key);
            Plan := AML_Field_Plans.Prepare (Field);
            if not AML_Field_Plans.Valid (Plan) or else AML_Field_Plans.Count (Plan) /= Count
            then return; end if;
            for Position in AML_Field_Plans.Datum_Position loop
               if Position > Count then
                  if Targets (Position) /= AML_Field_Access.No_Target then return; end if;
               else
                  if Targets (Position).Provider /=
                    Targets (AML_Field_Plans.Datum_Position'First).Provider then return; end if;
                  Describe_Target (IO, Targets (Position), Candidate.Read_Geometries (Position), Allowed);
                  if not Allowed or else not AML_Field_Plans.Matches
                    (Plan, Position, Candidate.Read_Geometries (Position)) then return; end if;
               end if;
            end loop;
            Candidate.Field := Key.Field; Candidate.Region := Key.Region;
            Candidate.Target := Targets (AML_Field_Plans.Datum_Position'First);
            Candidate.Geometry := Candidate.Read_Geometries (AML_Field_Plans.Datum_Position'First);
            Candidate.Read_Count := Count; Candidate.Read_Field := Field;
            Candidate.Read_Targets := Targets;
            Items.Entries (Items.Count + 1) := Candidate;
            Items.Count := Items.Count + 1;
            Items.Owner := A.Inner.Token; Items.Provider := Candidate.Target.Provider;
            Status := Available;
         end Add_Read_Binding;
         procedure Add_Write_Binding
           (A : Arena; IO : IO_Context; Key : Field_Key;
            Targets : AML_Field_Targets.Vector; Count : AML_Field_Plans.Datum_Count;
            Items : in out Binding_Set; Status : out Access_Status) is
            Candidate : Binding_Set := Items;
            Allowed : Boolean;
         begin
            Add_Read_Binding (A, IO, Key, Targets, Count, Candidate, Status);
            if Status /= Available then return; end if;
            Status := Invalid_Value;
            declare
               Plan : constant AML_Field_Plans.Plan := AML_Field_Plans.Prepare
                 (Candidate.Entries (Candidate.Count).Read_Field);
            begin
               for Position in AML_Field_Plans.Datum_Position range 1 .. Count loop
                  Authorize_Target (IO, Targets (Position), True, Allowed);
                  if not Allowed then return; end if;
                  declare
                     D : constant AML_Field_Datums.Datum := AML_Field_Plans.Datum_At (Plan, Position);
                  begin
                     if AML_Field_Datums.Needs_Read (D.Access_Width, D.Mask, AML_Field_Plans.Rule (Plan)) then
                        Authorize_Target (IO, Targets (Position), False, Allowed);
                        if not Allowed then return; end if;
                     end if;
                  end;
               end loop;
            end;
            Candidate.Entries (Candidate.Count).Writes_Enabled := True;
            Items := Candidate; Status := Available;
         end Add_Write_Binding;
         function Uncertain (A : Arena) return AML_Field_Access.Observation is
           (A.Inner.Field_Uncertain);
         function Original_Field (A : Arena) return Reference is
           (A.Inner.Uncertain_Field);
         function Original_Region (A : Arena) return Reference is
           (A.Inner.Uncertain_Region);
         procedure Reconcile
           (A : in out Arena; IO : IO_Context; Expected : AML_Field_Access.Observation;
            Status : out Access_Status) is
            Confirmed : Boolean;
         begin
            if A.In_Progress then Status := Busy; return; end if;
            Status := Invalid_Value;
            if not Expected.Present or else A.Inner.Field_Uncertain /= Expected then return; end if;
            Completion_Confirmed (IO, Expected, Confirmed);
            if not Confirmed then return; end if;
            -- Completion is independent of current namespace liveness: Reset
            -- invalidates associations but cannot undo external effects.
            A.Inner.Field_Uncertain := AML_Field_Access.No_Observation;
            A.Inner.Uncertain_Field := AML_References.No_Reference;
            A.Inner.Uncertain_Region := AML_References.No_Reference;
            Status := Available;
         end Reconcile;
         function Binding_Accessible (E : Owned.Arena; Context : IO_Context; Item : Binding; Writing : Boolean) return Boolean is
               Current : AML_Field_Layout.Register_Geometry;
               Allowed : Boolean;
               Field_Plan : constant AML_Field_Plans.Plan := AML_Field_Plans.Prepare (Item.Read_Field);
               procedure Check_Target (Target : AML_Field_Access.Target;
                 Expected : AML_Field_Layout.Register_Geometry; Write_Access : Boolean) is
               begin
                  Describe_Target (Context, Target, Current, Allowed);
                  if not Allowed or else Current /= Expected then Allowed := False; return; end if;
                  Authorize_Target (Context, Target, Write_Access, Allowed);
               end Check_Target;
            begin
               if E.Field_Uncertain.Present or else Pending (Context)
                 or else not Key_Matches (E, Key_Of (Item)) then return False; end if;
               if Item.Read_Count = 0 then
                  Check_Target (Item.Target, Item.Geometry, Writing);
                  return Allowed and then AML_Field_Layout.Admitted (Geometry_Of (E, Key_Of (Item)), Current);
               end if;
               if (Writing and then not Item.Writes_Enabled)
                 or else Geometry_Of (E, Key_Of (Item)) /= Item.Read_Field
                 or else not AML_Field_Plans.Valid (Field_Plan)
                 or else AML_Field_Plans.Count (Field_Plan) /= Item.Read_Count then return False; end if;
               for Position in AML_Field_Plans.Datum_Position range 1 .. Item.Read_Count loop
                  Check_Target (Item.Read_Targets (Position), Item.Read_Geometries (Position), Writing);
                  if not Allowed or else not AML_Field_Plans.Matches (Field_Plan, Position, Current) then return False; end if;
                  if Writing then
                     declare D : constant AML_Field_Datums.Datum := AML_Field_Plans.Datum_At (Field_Plan, Position); begin
                        if AML_Field_Datums.Needs_Read (D.Access_Width, D.Mask, AML_Field_Plans.Rule (Field_Plan)) then
                           Check_Target (Item.Read_Targets (Position), Item.Read_Geometries (Position), False);
                           if not Allowed then return False; end if;
                        end if;
                     end;
                  end if;
               end loop;
               return True;
            end Binding_Accessible;
         use type AML_Execution_Limits.Field_Request_Count;
         procedure Perform_Direct_Field
           (E : in out Owned.Arena; Context : in out IO_Context; Items : Binding_Set; Ref : Reference;
            Width : AML_Decode.Integer_Width; For_Write : Boolean;
            Value : in out AML_Execute.Datum; Status : out AML_Execute.Execution_Status;
            Remaining : in out AML_Execution_Limits.Field_Request_Count);
         procedure Perform_Direct_Field
           (E : in out Owned.Arena; Context : in out IO_Context; Items : Binding_Set; Ref : Reference;
            Width : AML_Decode.Integer_Width; For_Write : Boolean;
            Value : in out AML_Execute.Datum; Status : out AML_Execute.Execution_Status;
            Remaining : in out AML_Execution_Limits.Field_Request_Count) is
            Item : Binding := No_Binding;
            Found : Boolean := False;
            Current : AML_Field_Layout.Register_Geometry;
            Allowed : Boolean;
            Source_ID : AML_Objects.Object_ID := AML_Objects.No_Object;
            Reply : AML_Field_Access.Response;
            Request : AML_Field_Access.Request;
            Modulus : AML_Decode.Integer_Value;
            procedure Capture_Unknown is
            begin
               E.Field_Uncertain := (Present => True, Provider => Reply.Provider,
                  Item => Request, Operation => Reply.Operation);
               E.Uncertain_Field := Item.Field;
               E.Uncertain_Region := Item.Region;
               Status := AML_Execute.Field_Indeterminate;
            end Capture_Unknown;
         begin
            Status := AML_Execute.Unsupported_Value;
            if E.Field_Uncertain.Present or else Pending (Context) then
               Status := AML_Execute.Field_Indeterminate; return;
            end if;
            for I in Binding_Count range 1 .. Items.Count loop
               if Ref = Items.Entries (I).Field then
                  Item := Items.Entries (I); Found := True; exit;
               end if;
            end loop;
            if not Found or else not Key_Matches (E, Key_Of (Item)) then return; end if;
            if Item.Read_Count /= 0 then
               if (For_Write and then not Item.Writes_Enabled)
                 or else Geometry_Of (E, Key_Of (Item)) /= Item.Read_Field then return; end if;
               declare
                  Plan : constant AML_Field_Plans.Plan := AML_Field_Plans.Prepare (Item.Read_Field);
                  Accumulator : AML_Decode.Integer_Value := 0;
                  procedure Read_Datums is
                  begin
                     Status := AML_Execute.Unsupported_Value;
                     for Position in AML_Field_Plans.Datum_Position range 1 .. Item.Read_Count loop
                        if Pending (Context) then Status := AML_Execute.Field_Indeterminate; return; end if;
                        Describe_Target (Context, Item.Read_Targets (Position), Current, Allowed);
                        if not Allowed or else Current /= Item.Read_Geometries (Position)
                          or else not AML_Field_Plans.Matches (Plan, Position, Current) then return; end if;
                        Authorize_Target (Context, Item.Read_Targets (Position), False, Allowed);
                        if not Allowed then return; end if;
                        Request := (Destination => Item.Read_Targets (Position), For_Write => False, Value => 0);
                        if Remaining = 0 then Status := AML_Execute.Value_Limit; return; end if;
                        Remaining := Remaining - 1;
                        Dispatch (Context, Request, Reply);
                        case Reply.Status is
                           when AML_Field_Access.Indeterminate => Capture_Unknown; return;
                           when AML_Field_Access.Denied | AML_Field_Access.Failed_Completed => return;
                           when AML_Field_Access.Completed =>
                              if Reply.Value > AML_Field_Datums.Width_Mask (Current.Width)
                              then return; end if;
                              declare
                                 Datum : constant AML_Field_Datums.Datum :=
                                   AML_Field_Plans.Datum_At (Plan, Position);
                              begin
                                 Accumulator := Accumulator or Interfaces.Shift_Left
                                   (Interfaces.Shift_Right (Reply.Value and Datum.Mask, Datum.First_Bit),
                                    AML_Field_Datums.Local_Bit (Datum.Source_Bit));
                              end;
                        end case;
                     end loop;
                     Status := AML_Execute.Returned;
                  end Read_Datums;
                  procedure Write_Datums is
                     Input : AML_Decode.Integer_Value := 0;
                     Maximum_Source_Octets : constant Positive :=
                       Natural (AML_Field_Plans.Maximum_Bits) / AML_Field_Layout.Bits_Per_Byte;
                     procedure Check_Access
                       (Position : AML_Field_Plans.Datum_Position; Writing : Boolean;
                        OK : out Boolean) is
                     begin
                        OK := False;
                        if Pending (Context) then Status := AML_Execute.Field_Indeterminate; return; end if;
                        Describe_Target (Context, Item.Read_Targets (Position), Current, Allowed);
                        if not Allowed or else Current /= Item.Read_Geometries (Position)
                          or else not AML_Field_Plans.Matches (Plan, Position, Current) then return; end if;
                        Authorize_Target (Context, Item.Read_Targets (Position), Writing, OK);
                     end Check_Access;
                     OK : Boolean;
                  begin
                     case Value.Value_Kind is
                        when AML_Execute.Integer_Datum =>
                           Input := AML_Integers.Normalize (Value.Number, Width);
                        when AML_Execute.Object_Datum =>
                           if not Has_Source (E, Value.Object.Source)
                             or else Value.Object.ID /= AML_References.Source (Value.Object.Source) then return; end if;
                           Source_ID := Value.Object.ID;
                           if AML_Objects.Kind (E.Tree.Values, Source_ID) not in
                             AML_Objects.String_Object | AML_Objects.Buffer_Object then return; end if;
                           for I in 1 .. Natural'Min (Maximum_Source_Octets, AML_Objects.Length (E.Tree.Values, Source_ID)) loop
                              Input := Input or Interfaces.Shift_Left
                                (AML_Decode.Integer_Value (AML_Objects.Stored_Byte (E.Tree.Values, Source_ID, I - 1)),
                                 (I - 1) * AML_Field_Layout.Bits_Per_Byte);
                           end loop;
                        when others => return;
                     end case;
                     -- Preflight the complete operation before its first effect.
                     for Position in AML_Field_Plans.Datum_Position range 1 .. Item.Read_Count loop
                        declare
                           D : constant AML_Field_Datums.Datum := AML_Field_Plans.Datum_At (Plan, Position);
                        begin
                           Check_Access (Position, True, OK); if not OK then return; end if;
                           if AML_Field_Datums.Needs_Read (D.Access_Width, D.Mask, AML_Field_Plans.Rule (Plan)) then
                              Check_Access (Position, False, OK); if not OK then return; end if;
                           end if;
                        end;
                     end loop;
                     for Position in AML_Field_Plans.Datum_Position range 1 .. Item.Read_Count loop
                        declare
                           D : constant AML_Field_Datums.Datum := AML_Field_Plans.Datum_At (Plan, Position);
                           Old_Value : AML_Decode.Integer_Value := 0;
                           Selected : constant AML_Decode.Integer_Value := Interfaces.Shift_Left
                             (Interfaces.Shift_Right (Input, AML_Field_Datums.Local_Bit (D.Source_Bit)), D.First_Bit);
                        begin
                           Check_Access (Position, True, OK); if not OK then return; end if;
                           if AML_Field_Datums.Needs_Read (D.Access_Width, D.Mask, AML_Field_Plans.Rule (Plan)) then
                              Check_Access (Position, False, OK); if not OK then return; end if;
                              Request := (Item.Read_Targets (Position), False, 0);
                              if Remaining = 0 then Status := AML_Execute.Value_Limit; return; end if;
                              Remaining := Remaining - 1;
                              Dispatch (Context, Request, Reply);
                              case Reply.Status is
                                 when AML_Field_Access.Indeterminate => Capture_Unknown; return;
                                 when AML_Field_Access.Denied | AML_Field_Access.Failed_Completed => return;
                                 when AML_Field_Access.Completed =>
                                    if Reply.Value > AML_Field_Datums.Width_Mask (D.Access_Width) then return; end if;
                                    Old_Value := Reply.Value;
                              end case;
                           end if;
                           -- Recheck after the read; permissions/epoch may change.
                           Check_Access (Position, True, OK); if not OK then return; end if;
                           Request := (Item.Read_Targets (Position), True,
                             AML_Field_Datums.Merge (D.Access_Width, D.Mask, Old_Value, Selected, AML_Field_Plans.Rule (Plan)));
                           if Remaining = 0 then Status := AML_Execute.Value_Limit; return; end if;
                           Remaining := Remaining - 1;
                           Dispatch (Context, Request, Reply);
                           case Reply.Status is
                              when AML_Field_Access.Indeterminate => Capture_Unknown; return;
                              when AML_Field_Access.Denied | AML_Field_Access.Failed_Completed => return;
                              when AML_Field_Access.Completed => if Reply.Value /= 0 then return; end if;
                           end case;
                        end;
                     end loop;
                     Status := AML_Execute.Returned;
                  end Write_Datums;
               begin
                  if not AML_Field_Plans.Valid (Plan)
                    or else AML_Field_Plans.Count (Plan) /= Item.Read_Count
                  then return; end if;
                  -- Validate the complete association before the first read.
                  for Position in AML_Field_Plans.Datum_Position range 1 .. Item.Read_Count loop
                     Describe_Target (Context, Item.Read_Targets (Position), Current, Allowed);
                     if not Allowed or else Current /= Item.Read_Geometries (Position)
                       or else not AML_Field_Plans.Matches (Plan, Position, Current) then return; end if;
                  end loop;
                  if For_Write then Write_Datums; return; end if;
                  if AML_Field_Plans.Result_Form (Plan, Width) = AML_Field_Plans.Buffer_Result then
                     declare
                        Candidate : AML_Objects.State := E.Tree.Values;
                        Data : constant AML_Decode.Bytes (1 .. AML_Field_Plans.Buffer_Length (Plan)) := [others => 0];
                        ID : AML_Objects.Object_ID;
                        Allocated : AML_Objects.Allocation_Status;
                        Source : AML_References.Object_Handle;
                        OK : Boolean;
                     begin
                        -- Stage allocation before effects. The provider has no
                        -- arena alias, so this private candidate stays current.
                        AML_Objects.New_Bytes (Candidate, AML_Objects.Buffer_Object, Data, ID, Allocated);
                        if Allocated /= AML_Objects.Allocated then Status := AML_Execute.Value_Limit; return; end if;
                        Read_Datums;
                        if Status /= AML_Execute.Returned then return; end if;
                        for I in Data'Range loop
                           AML_Objects.Set_Stored_Byte (Candidate, ID, I - 1,
                             AML_Decode.Byte (Interfaces.Shift_Right (Accumulator,
                               (I - 1) * AML_Field_Layout.Bits_Per_Byte) and 16#FF#));
                        end loop;
                        E.Tree.Values := Candidate;
                        Owned.Make_Source (E, ID, Source, OK);
                        if not OK then raise Program_Error with "region buffer source invariant"; end if;
                        Owned.Read_Source (E, Source, Value, Status);
                     end;
                  else
                     Read_Datums;
                     if Status /= AML_Execute.Returned then return; end if;
                     Value := (AML_Execute.Integer_Datum, Accumulator, AML_Decode.Ordinary_Integer);
                  end if;
                  return;
               end;
            end if;
            Describe_Target (Context, Item.Target, Current, Allowed);
            if not Allowed or else Current /= Item.Geometry
              or else not AML_Field_Layout.Admitted (Geometry_Of (E, Key_Of (Item)), Current)
              -- Qword bindings must use the planned path above, including its
              -- allocation-before-effects and width-dependent result handling.
              or else Current.Width = ACPI_FADT.Transactions.Qword_Access
            then return; end if;
            Modulus := 2 ** (AML_Field_Layout.Bits_Per_Byte *
              ACPI_FADT.Transactions.Octets (Current.Width));
            Request := (Destination => Item.Target, For_Write => For_Write, Value => 0);
            if For_Write then
               case Value.Value_Kind is
                  when AML_Execute.Integer_Datum =>
                     Request.Value := AML_Integers.Normalize (Value.Number, Width) mod Modulus;
                  when AML_Execute.Object_Datum =>
                     if not Has_Source (E, Value.Object.Source)
                       or else Value.Object.ID /= AML_References.Source (Value.Object.Source)
                     then return; end if;
                     Source_ID := Value.Object.ID;
                     if AML_Objects.Kind (E.Tree.Values, Source_ID) not in
                       AML_Objects.String_Object | AML_Objects.Buffer_Object
                     then return; end if;
                     -- Stage primitive bytes before dispatch; short sources pad with zero.
                     declare
                        Bytes : constant Natural := Natural'Min
                          (ACPI_FADT.Transactions.Octets (Current.Width),
                           AML_Objects.Length (E.Tree.Values, Source_ID));
                     begin
                        for I in 1 .. Bytes loop
                           Request.Value := Request.Value or Interfaces.Shift_Left
                             (AML_Decode.Integer_Value
                                (AML_Objects.Stored_Byte (E.Tree.Values, Source_ID, I - 1)),
                              (I - 1) * AML_Field_Layout.Bits_Per_Byte);
                        end loop;
                     end;
                  when others => return;
               end case;
            end if;
            -- All resolution, layout and conversion work precedes dispatch.
            -- The interpreter has already charged this operand/target.
            Authorize_Target (Context, Item.Target, For_Write, Allowed);
            if not Allowed then return; end if;
            if Remaining = 0 then Status := AML_Execute.Value_Limit; return; end if;
            Remaining := Remaining - 1;
            Dispatch (Context, Request, Reply);
            case Reply.Status is
               when AML_Field_Access.Denied => Status := AML_Execute.Unsupported_Value;
               when AML_Field_Access.Failed_Completed =>
                  Status := AML_Execute.Unsupported_Value;
               when AML_Field_Access.Indeterminate => Capture_Unknown;
               when AML_Field_Access.Completed =>
                  if (For_Write and then Reply.Value /= 0)
                    or else (not For_Write and then Reply.Value >= Modulus)
                  then Status := AML_Execute.Unsupported_Value; return; end if;
                  if not For_Write then
                     Value := (AML_Execute.Integer_Datum, Reply.Value, AML_Decode.Ordinary_Integer);
                  end if;
                  Status := AML_Execute.Returned;
            end case;
         end Perform_Direct_Field;
         procedure Perform_Composed_Field
           (E : in out Owned.Arena; Context : in out IO_Context; Items : Binding_Set;
            Ref : Reference; Width : AML_Decode.Integer_Width; For_Write : Boolean;
            Value : in out AML_Execute.Datum; Status : out AML_Execute.Execution_Status;
            Remaining : in out AML_Execution_Limits.Field_Request_Count;
            Work_Allowance : AML_Execution_Limits.Field_Work_Count) is
            use type AML_Field_Datums.Datum_Index;
            use type AML_Field_Datums.Update_Rule;
            use type AML_Execution_Limits.Field_Work_Count;
            Scalar_Bits : constant Positive := AML_Integers.Bit_Width (AML_Decode.Bits_64);
            Scalar_Octets : constant Positive := Scalar_Bits / AML_Field_Layout.Bits_Per_Byte;
            Maximum_Field_Bits : constant Positive := AML_Objects.Max_Bytes * AML_Field_Layout.Bits_Per_Byte;
            Maximum_Composed_Datums : constant Positive := AML_Objects.Max_Bytes + 1;
            subtype Composed_Datum_Count is Natural range 0 .. Maximum_Composed_Datums;
            function Root_Bit_Length return Encoded_Field_Bits is
            begin
               if not Named_Identity_Matches (E, Ref) then return 0; end if;
               return E.Tree.Items (Node_ID (AML_References.Named_Node (Ref))).Region_Field_Info.Bits;
            end Root_Bit_Length;
            Bits : constant Encoded_Field_Bits := Root_Bit_Length;
            Output_Length : constant Natural :=
              (if not For_Write and then Bits in Scalar_Bits + 1 .. Maximum_Field_Bits
               then (Bits + AML_Field_Layout.Bits_Per_Byte - 1) / AML_Field_Layout.Bits_Per_Byte else 0);
            Wide_Output : AML_Decode.Bytes (1 .. Output_Length) := [others => 0];
            Input_Object : AML_Objects.Object_ID := AML_Objects.No_Object;
            type Frame_Count is new Natural range 0 .. Capacity;
            subtype Frame_Position is Frame_Count range 1 .. Frame_Count'Last;
            type Phase is (Start_Datum, After_Index_Read, After_Index_Write,
              After_Bank_Read, After_Bank_Write, After_Data_Read, After_Data_Write);
            type Frame is record
               Identity, Region, Index_Source, Data_Source, Bank_Source : Reference := AML_References.No_Reference;
               Node : Node_ID := Root;
               Info : Region_Field_Attributes;
               Writing : Boolean := False;
               Input, Accumulator, Pending_Value, Previous_Raw : AML_Decode.Integer_Value := 0;
               Position, Number : Composed_Datum_Count := 0;
               Next_Dependency : Natural range 0 .. 3 := 0;
               Indexed_Plan : AML_Field_Datums.Plan;
               Region_Geometry : AML_Field_Layout.Region_Geometry;
               Choice : Binding_Slot := Binding_Slot'First;
               Rule : AML_Field_Datums.Update_Rule := AML_Field_Datums.Preserve;
               Step : Phase := Start_Datum;
            end record;
            type Frame_Array is array (Frame_Position) of Frame;
            Stack : Frame_Array;
            Top : Frame_Count := 0;
            type Directions is array (Boolean) of Boolean;
            Work : AML_Execution_Limits.Field_Work_Count := Work_Allowance;
            Last_Scalar, Input : AML_Decode.Integer_Value := 0;
            NoLock_And_Reserved_Mask : constant AML_Decode.Byte := 16#90#;
            Update_Rule_Unit : constant AML_Decode.Byte := 32;
            Raw_Access_Mask : constant AML_Decode.Byte := 16#0F#;
            Last_Defined_Access : constant AML_Decode.Byte := 5;
            Logical_Bytes : constant AML_Decode.Integer_Value :=
              AML_Decode.Integer_Value (Field_Bit_Position'Last) / AML_Field_Layout.Bits_Per_Byte +
                AML_Decode.Integer_Value (ACPI_FADT.Transactions.Octets (ACPI_FADT.Transactions.Qword_Access));
            procedure Charge (Accepted : out Boolean)
              with Post => (if Accepted then Work'Old > 0 and then Work = Work'Old - 1
                else Work = 0 and then Status = AML_Execute.Value_Limit)
            is
            begin
               Accepted := Work > 0;
               Work := AML_Execution_Limits.Charged_Field_Work (Work);
               if not Accepted then Status := AML_Execute.Value_Limit; end if;
            end Charge;
            function Datum_At (F : Frame) return AML_Field_Datums.Datum is
            begin
               return AML_Field_Datums.At_Index (F.Indexed_Plan, AML_Field_Datums.Datum_Index (F.Position));
            end Datum_At;
            function Needs_Old (F : Frame) return Boolean is
            begin
               for P in 0 .. F.Number - 1 loop
                  declare
                     Copy : Frame := F;
                  begin
                     Copy.Position := P;
                     declare D : constant AML_Field_Datums.Datum := Datum_At (Copy); begin
                        if AML_Field_Datums.Needs_Read (D.Access_Width, D.Mask, F.Rule) then return True; end if;
                     end;
                  end;
               end loop;
               return False;
            end Needs_Old;
            procedure Source_Identity (N : Node_ID; Incarnation : AML_References.Node_Incarnation;
              Identity : out Reference; Good : out Boolean) is
            begin
               Identity := AML_References.No_Reference; Good := False;
               if N = Root or else not Present (E.Tree, N)
                 or else E.Tree.Items (N).Incarnation /= Incarnation
                 or else E.Tree.Items (N).Object_Type /= Region_Field_Object then return; end if;
               Make_Named_Identity (E, Natural (N), Identity, Good);
            end Source_Identity;
            function Data_Allowed (F : Frame; Choice : Binding_Slot; Writing : Boolean) return Boolean is
               Item : constant Region_Register := Items.Registers (Choice);
               D : constant AML_Field_Datums.Datum := Datum_At (F);
               Current : AML_Field_Layout.Register_Geometry;
               Allowed : Boolean;
            begin
               if E.Field_Uncertain.Present or else Pending (Context)
                 or else Choice > Items.Register_Count or else Item.Region /= F.Region
                 or else not Named_Identity_Matches (E, Item.Region)
                 or else (Writing and then not Item.Allow_Write)
                 or else (not Writing and then not Item.Allow_Read) then return False; end if;
               Describe_Target (Context, Item.Target, Current, Allowed);
               if not Allowed or else Current /= Item.Geometry
                 or else Current.Region /= F.Region_Geometry
                 or else Current.Byte_Offset /= AML_Decode.Integer_Value (D.Offset)
                 or else Current.Width /= D.Access_Width then return False; end if;
               Authorize_Target (Context, Item.Target, Writing, Allowed); return Allowed;
            end Data_Allowed;
            procedure Select_Region_Target (F : Frame; Choice : out Binding_Slot;
              Good : out Boolean) is
               D : constant AML_Field_Datums.Datum := Datum_At (F);
               Charged : Boolean;
            begin
               Status := AML_Execute.Unsupported_Value;
               Choice := Binding_Slot'First; Good := False;
               for Candidate in Binding_Slot range 1 .. Items.Register_Count loop
                  if F.Info.Bits > Scalar_Bits then
                     Charge (Charged); if not Charged then return; end if;
                  end if;
                  if Data_Allowed (F, Candidate, F.Writing) and then
                    (not F.Writing or else not AML_Field_Datums.Needs_Read (D.Access_Width, D.Mask, F.Rule)
                     or else Data_Allowed (F, Candidate, False)) then
                     Choice := Candidate; Good := True; return;
                  end if;
               end loop;
            end Select_Region_Target;
            procedure Build_Frame (Identity : Reference; Writing : Boolean; Number : AML_Decode.Integer_Value;
              F : out Frame; Good : out Boolean) is
               Allowed : Boolean;
               Region_Info : Operation_Region_Attributes;
               Geometry : AML_Field_Layout.Field_Geometry;
               Access_Width : ACPI_FADT.Transactions.Access_Width;
               Selected : Binding_Slot;
            begin
               F := (others => <>); Good := False;
               if Items.Owner /= E.Token or else E.Field_Uncertain.Present or else Pending (Context)
                 or else AML_References.Kind (Identity) /= AML_References.Named_Cell
                 or else not Named_Identity_Matches (E, Identity) then return; end if;
               F.Node := Node_ID (AML_References.Named_Node (Identity));
               if E.Tree.Items (F.Node).Object_Type /= Region_Field_Object then return; end if;
               F.Identity := Identity; F.Info := E.Tree.Items (F.Node).Region_Field_Info;
               F.Writing := Writing; F.Input := Number;
               if F.Info.Organization = Direct_Region_Field and then F.Info.Bits <= Scalar_Bits then
                  for I in Binding_Count range 1 .. Items.Count loop
                     if Items.Entries (I).Field = Identity then
                        Good := Binding_Accessible (E, Context, Items.Entries (I), Writing); return;
                     end if;
                  end loop;
                  return;
               end if;
               if F.Info.Bits = 0 or else F.Info.Bits > Maximum_Field_Bits
                 or else (F.Info.Bits > Scalar_Bits and then F.Identity /= Ref and then not Writing) then return; end if;
               if F.Info.Organization = Indexed_Register_Field then
                  if (F.Info.Raw_Flags and NoLock_And_Reserved_Mask) /= 0
                    or else (F.Info.Raw_Flags and Raw_Access_Mask) > Last_Defined_Access
                    or else F.Info.Raw_Flags / Update_Rule_Unit > AML_Decode.Byte
                      (AML_Field_Datums.Update_Rule'Pos (AML_Field_Datums.Update_Rule'Last))
                    or else F.Info.Access_Type > 4 or else F.Info.Attribute /= 0 or else F.Info.Access_Length /= 0 then return; end if;
                  Access_Width := (case F.Info.Access_Type is
                    when 0 | 1 => ACPI_FADT.Transactions.Byte_Access,
                    when 2 => ACPI_FADT.Transactions.Word_Access,
                    when 3 => ACPI_FADT.Transactions.Dword_Access,
                    when others => ACPI_FADT.Transactions.Qword_Access);
                  F.Rule := AML_Field_Datums.Update_Rule'Val (F.Info.Raw_Flags / Update_Rule_Unit);
                  F.Indexed_Plan := AML_Field_Datums.Prepare (0, Logical_Bytes,
                    AML_Field_Datums.Bit_Position (F.Info.Offset), AML_Field_Datums.Bit_Length (F.Info.Bits), Access_Width);
                  if not AML_Field_Datums.Valid (F.Indexed_Plan) or else
                    AML_Field_Datums.Count (F.Indexed_Plan) > AML_Field_Datums.Datum_Index (Maximum_Composed_Datums) then return; end if;
                  F.Number := Natural (AML_Field_Datums.Count (F.Indexed_Plan));
                  Source_Identity (F.Info.Index_Register, F.Info.Index_Incarnation, F.Index_Source, Allowed);
                  if not Allowed then return; end if;
                  Source_Identity (F.Info.Data_Register, F.Info.Data_Incarnation, F.Data_Source, Allowed);
                  if not Allowed then return; end if;
                  declare
                     Index_Bits : constant Encoded_Field_Bits := E.Tree.Items (F.Info.Index_Register).Region_Field_Info.Bits;
                     Data_Bits : constant Encoded_Field_Bits := E.Tree.Items (F.Info.Data_Register).Region_Field_Info.Bits;
                     Mask : AML_Decode.Integer_Value;
                  begin
                     if Index_Bits not in 1 .. Encoded_Field_Bits (Maximum_Field_Bits)
                       or else Data_Bits not in 1 .. Encoded_Field_Bits (Maximum_Field_Bits) then return; end if;
                     -- DataName stores insert an eight-byte scalar and pad any
                     -- wider field. Reads still require a scalar destination.
                     if Data_Bits > Scalar_Bits and then
                       (not Writing or else Needs_Old (F)) then return; end if;
                     Mask := (if Index_Bits >= Scalar_Bits then AML_Decode.Integer_Value'Last
                       else Interfaces.Shift_Right (AML_Decode.Integer_Value'Last, Scalar_Bits - Natural (Index_Bits)));
                     for P in 0 .. F.Number - 1 loop
                        F.Position := P;
                        if AML_Decode.Integer_Value (Datum_At (F).Offset) > Mask then return; end if;
                     end loop;
                  end;
                  F.Position := 0; Good := True; return;
               end if;
               if Items.Register_Count = 0 or else not Present (E.Tree, F.Info.Region)
                 or else E.Tree.Items (F.Info.Region).Incarnation /= F.Info.Incarnation
                 or else E.Tree.Items (F.Info.Region).Object_Type /= Operation_Region_Object then return; end if;
               Make_Named_Identity (E, Natural (F.Info.Region), F.Region, Allowed); if not Allowed then return; end if;
               if F.Info.Organization = Banked_Region_Field then
                  Source_Identity (F.Info.Bank_Register, F.Info.Bank_Incarnation, F.Bank_Source, Allowed);
                  if not Allowed then return; end if;
                  declare Bits : constant Encoded_Field_Bits := E.Tree.Items (F.Info.Bank_Register).Region_Field_Info.Bits; begin
                     if Bits not in 1 .. Encoded_Field_Bits (Maximum_Field_Bits)
                       or else (Bits < Scalar_Bits and then F.Info.Bank_Value >
                         Interfaces.Shift_Right (AML_Decode.Integer_Value'Last, Scalar_Bits - Natural (Bits))) then return; end if;
                  end;
               end if;
               Region_Info := E.Tree.Items (F.Info.Region).Operation_Info;
               Geometry := (Region => (Space => AML_Decode.Byte (Region_Info.Space),
                   Base => Region_Info.Address, Length => Region_Info.Length),
                 Bit_Offset => AML_Decode.Integer_Value (F.Info.Offset), Bit_Length => AML_Decode.Integer_Value (F.Info.Bits),
                 Flags => F.Info.Raw_Flags, Effective_Access => F.Info.Access_Type,
                 Attribute => F.Info.Attribute, Access_Length => F.Info.Access_Length, others => <>);
               declare
                  Probe : AML_Field_Layout.Field_Geometry := Geometry;
                  Plan : AML_Field_Plans.Plan;
               begin
                  Probe.Bit_Length := AML_Decode.Integer_Value'Min (Geometry.Bit_Length, AML_Field_Plans.Maximum_Bits);
                  Plan := AML_Field_Plans.Prepare (Probe);
                  if not AML_Field_Plans.Valid (Plan) then return; end if;
                  F.Rule := AML_Field_Plans.Rule (Plan);
                  Access_Width := AML_Field_Plans.Datum_At (Plan, AML_Field_Plans.Datum_Position'First).Access_Width;
               end;
               F.Region_Geometry := Geometry.Region;
               F.Indexed_Plan := AML_Field_Datums.Prepare (Geometry.Region.Base, Geometry.Region.Length,
                 AML_Field_Datums.Bit_Position (Geometry.Bit_Offset),
                 AML_Field_Datums.Bit_Length (Geometry.Bit_Length), Access_Width);
               if not AML_Field_Datums.Valid (F.Indexed_Plan) or else
                 AML_Field_Datums.Count (F.Indexed_Plan) > AML_Field_Datums.Datum_Index (Maximum_Composed_Datums) then return; end if;
               F.Number := Natural (AML_Field_Datums.Count (F.Indexed_Plan));
               for P in 0 .. F.Number - 1 loop
                  F.Position := P;
                  Select_Region_Target (F, Selected, Allowed);
                  if not Allowed then return; end if;
                  F.Choice := Selected;
               end loop;
               F.Position := 0;
               Good := True;
            end Build_Frame;
            procedure Push_Into (Frames : in out Frame_Array; Depth : in out Frame_Count; Identity : Reference; Writing : Boolean; Number : AML_Decode.Integer_Value;
              Good : out Boolean) is
               Candidate : Frame;
            begin
               Status := AML_Execute.Unsupported_Value; Good := False;
               for P in Frame_Position range 1 .. Depth loop
                  if Frames (P).Identity = Identity then Status := AML_Execute.Unsupported_Value; return; end if;
               end loop;
               if Depth = Frame_Count'Last then Status := AML_Execute.Value_Limit; return; end if;
               Build_Frame (Identity, Writing, Number, Candidate, Good);
               if Good then Depth := Depth + 1; Frames (Depth) := Candidate; end if;
            end Push_Into;
            procedure Push (Identity : Reference; Writing : Boolean; Number : AML_Decode.Integer_Value;
              Good : out Boolean) is
            begin
               Push_Into (Stack, Top, Identity, Writing, Number, Good);
            end Push;
            procedure Preflight (Identity : Reference; For_Writing : Boolean; Number : AML_Decode.Integer_Value; Good : out Boolean) is
               Frames : Frame_Array;
               Depth : Frame_Count := 0;
               Done : array (Node_ID) of Directions := [others => [others => False]];
               Child : Reference;
               Writing, Pushed, Charged : Boolean;
            begin
               Push_Into (Frames, Depth, Identity, For_Writing, Number, Good); if not Good then return; end if;
               while Depth > 0 loop
                  pragma Loop_Variant (Decreases => Work);
                  Charge (Charged); if not Charged then Good := False; return; end if;
                  Child := AML_References.No_Reference; Writing := False;
                  declare F : Frame renames Frames (Depth); begin
                     if F.Info.Organization = Banked_Region_Field and then F.Next_Dependency = 0 then
                        Child := F.Bank_Source; Writing := True; F.Next_Dependency := 1;
                     elsif F.Info.Organization = Indexed_Register_Field then
                        case F.Next_Dependency is
                           when 0 => Child := F.Index_Source; Writing := True;
                           when 1 => Child := F.Data_Source; Writing := F.Writing;
                           when 2 => if F.Writing and then Needs_Old (F) then Child := F.Data_Source; end if;
                           when others => null;
                        end case;
                        if F.Next_Dependency < 3 then F.Next_Dependency := F.Next_Dependency + 1; end if;
                     end if;
                  end;
                  if Child = AML_References.No_Reference then
                     Done (Frames (Depth).Node) (Frames (Depth).Writing) := True; Depth := Depth - 1;
                  elsif not Done (Node_ID (AML_References.Named_Node (Child))) (Writing) then
                     Push_Into (Frames, Depth, Child, Writing, 0, Pushed); if not Pushed then Good := False; return; end if;
                  end if;
               end loop;
               Good := True;
            end Preflight;
            function Selected_Input (F : Frame; D : AML_Field_Datums.Datum) return AML_Decode.Integer_Value is
               Selected : AML_Decode.Integer_Value := 0;
            begin
               if F.Info.Bits <= Scalar_Bits then
                  return Interfaces.Shift_Left (Interfaces.Shift_Right (F.Input,
                    AML_Field_Datums.Local_Bit (D.Source_Bit)), D.First_Bit) and D.Mask;
               end if;
               for I in 0 .. D.Bits - 1 loop
                  declare
                     Source_Bit : constant AML_Decode.Integer_Value := AML_Decode.Integer_Value (D.Source_Bit) +
                       AML_Decode.Integer_Value (I);
                     Bit_Value : AML_Decode.Integer_Value := 0;
                  begin
                     -- Selector writes use their own scalar, zero-extended to
                     -- the child field width. Only the root owns Input_Object.
                     if F.Identity = Ref and then Input_Object /= AML_Objects.No_Object then
                        if Source_Bit / AML_Field_Layout.Bits_Per_Byte <
                          AML_Decode.Integer_Value (AML_Objects.Length (E.Tree.Values, Input_Object)) then
                           Bit_Value := Interfaces.Shift_Right (AML_Decode.Integer_Value
                             (AML_Objects.Stored_Byte (E.Tree.Values, Input_Object,
                               Natural (Source_Bit / AML_Field_Layout.Bits_Per_Byte))),
                             Natural (Source_Bit mod AML_Field_Layout.Bits_Per_Byte)) and 1;
                        end if;
                     elsif Source_Bit < AML_Decode.Integer_Value (Scalar_Bits) then
                        Bit_Value := Interfaces.Shift_Right (F.Input, Natural (Source_Bit)) and 1;
                     end if;
                     Selected := Selected or Interfaces.Shift_Left (Bit_Value, D.First_Bit + I);
                  end;
               end loop;
               return Selected;
            end Selected_Input;
            procedure Merge_Write (F : in out Frame; Old_Value : AML_Decode.Integer_Value) is
               D : constant AML_Field_Datums.Datum := Datum_At (F);
            begin
               if F.Info.Organization = Indexed_Register_Field then
                  -- DataName is a field in its own right. Apply the update
                  -- rule to the full scalar before that field stores it.
                  F.Pending_Value := Selected_Input (F, D);
                  case F.Rule is
                     when AML_Field_Datums.Preserve =>
                        F.Pending_Value := F.Pending_Value or (Old_Value and not D.Mask);
                     when AML_Field_Datums.Write_As_Ones =>
                        F.Pending_Value := F.Pending_Value or not D.Mask;
                     when AML_Field_Datums.Write_As_Zeros => null;
                  end case;
               else
                  F.Pending_Value := AML_Field_Datums.Merge (D.Access_Width, D.Mask, Old_Value,
                    Selected_Input (F, D), F.Rule);
               end if;
            end Merge_Write;
            procedure Accumulate_Indexed_Read (F : in out Frame; Raw : AML_Decode.Integer_Value) is
               D : constant AML_Field_Datums.Datum := Datum_At (F);
               Access_Bits : constant Positive := AML_Field_Layout.Bits_Per_Byte *
                 ACPI_FADT.Transactions.Octets (D.Access_Width);
               Start_Bit : constant Natural := Natural (F.Info.Offset mod Field_Bit_Position (Access_Bits));
               Chunks : constant Positive := (Natural (F.Info.Bits) + Access_Bits - 1) / Access_Bits;
               Tail : constant Natural := Natural (F.Info.Bits) mod Access_Bits;
               procedure Store_Chunk (Number : AML_Decode.Integer_Value; Position : Natural) is
                  Mask : AML_Decode.Integer_Value := AML_Field_Datums.Width_Mask (D.Access_Width);
               begin
                  pragma Assert (Position < Chunks);
                  if Position = Chunks - 1 and then Tail /= 0 then
                     Mask := Interfaces.Shift_Right (AML_Decode.Integer_Value'Last, Scalar_Bits - Tail);
                  end if;
                  if F.Identity = Ref and then Bits > Scalar_Bits then
                     for I in 0 .. ACPI_FADT.Transactions.Octets (D.Access_Width) - 1 loop
                        declare
                           Output_Byte : constant Natural := Position * ACPI_FADT.Transactions.Octets (D.Access_Width) + I;
                        begin
                           if Output_Byte < Wide_Output'Length then
                              Wide_Output (Output_Byte + 1) := AML_Decode.Byte (Interfaces.Shift_Right
                                (Number and Mask, I * AML_Field_Layout.Bits_Per_Byte) and 16#FF#);
                           end if;
                        end;
                     end loop;
                  else
                     F.Accumulator := F.Accumulator or Interfaces.Shift_Left (Number and Mask, Position * Access_Bits);
                  end if;
               end Store_Chunk;
            begin
               if Start_Bit = 0 and then Natural (F.Info.Bits) = Access_Bits then
                  -- A whole-datum access returns the DataName value, whose
                  -- field width is independent of the index alignment width.
                  F.Accumulator := Raw;
               else
                  if F.Position > 0 then
                     declare Number : AML_Decode.Integer_Value := Interfaces.Shift_Right (F.Previous_Raw, Start_Bit); begin
                        if Access_Bits - Start_Bit < Scalar_Bits then
                           Number := Number or Interfaces.Shift_Left (Raw, Access_Bits - Start_Bit);
                        end if;
                        Store_Chunk (Number, F.Position - 1);
                     end;
                  end if;
                  if F.Position = F.Number - 1 and then F.Number = Chunks then
                     Store_Chunk (Interfaces.Shift_Right (Raw, Start_Bit), Chunks - 1);
                  end if;
               end if;
               F.Previous_Raw := Raw;
            end Accumulate_Indexed_Read;
            procedure Accumulate_Region_Read (F : in out Frame; Raw : AML_Decode.Integer_Value) is
               D : constant AML_Field_Datums.Datum := Datum_At (F);
            begin
               if F.Identity = Ref and then Bits > Scalar_Bits then
                  for I in 0 .. D.Bits - 1 loop
                     declare
                        Destination : constant Natural := Natural (D.Source_Bit) + I;
                        Byte : constant Positive := Destination / AML_Field_Layout.Bits_Per_Byte + 1;
                        Bit_Value : constant AML_Decode.Byte := AML_Decode.Byte
                          (Interfaces.Shift_Right (Raw, D.First_Bit + I) and 1);
                     begin
                        Wide_Output (Byte) := Wide_Output (Byte) or AML_Decode.Byte
                          (Interfaces.Shift_Left (AML_Decode.Integer_Value (Bit_Value),
                            Destination mod AML_Field_Layout.Bits_Per_Byte));
                     end;
                  end loop;
               else
                  F.Accumulator := F.Accumulator or Interfaces.Shift_Left
                    (Interfaces.Shift_Right (Raw and D.Mask, D.First_Bit),
                      AML_Field_Datums.Local_Bit (D.Source_Bit));
               end if;
            end Accumulate_Region_Read;
            procedure Bank_Transfer (F : Frame; Writing : Boolean; Number : out AML_Decode.Integer_Value;
              Good : out Boolean);
            procedure Start_Transfer (Writing : Boolean; Good : out Boolean) is
               Child : Reference;
               Number : AML_Decode.Integer_Value;
            begin
               declare F : Frame renames Stack (Top); begin
                  if F.Info.Organization = Direct_Region_Field then
                     Bank_Transfer (F, Writing, Last_Scalar, Good);
                     F.Step := (if Writing then After_Data_Write else After_Data_Read);
                     return;
                  end if;
                  if F.Info.Organization = Banked_Region_Field then
                     if not Data_Allowed (F, F.Choice, Writing) then
                        Status := AML_Execute.Unsupported_Value; Good := False; return;
                     end if;
                     Child := F.Bank_Source; Number := F.Info.Bank_Value;
                     F.Step := (if Writing then After_Bank_Write else After_Bank_Read);
                  else
                     Preflight (F.Data_Source, Writing, F.Pending_Value, Good); if not Good then return; end if;
                     Child := F.Index_Source; Number := AML_Decode.Integer_Value (Datum_At (F).Offset);
                     F.Step := (if Writing then After_Index_Write else After_Index_Read);
                  end if;
               end;
               Preflight (Child, True, Number, Good); if not Good then return; end if;
               Push (Child, True, Number, Good);
            end Start_Transfer;
            procedure Bank_Transfer (F : Frame; Writing : Boolean; Number : out AML_Decode.Integer_Value;
              Good : out Boolean) is
               Item : constant Region_Register := Items.Registers (F.Choice);
               Request : constant AML_Field_Access.Request :=
                 (Item.Target, Writing, (if Writing then F.Pending_Value else 0));
               Reply : AML_Field_Access.Response;
            begin
               Status := AML_Execute.Unsupported_Value; Good := False; Number := 0;
               if not Data_Allowed (F, F.Choice, Writing) then return; end if;
               if Remaining = 0 then Status := AML_Execute.Value_Limit; return; end if;
               Remaining := Remaining - 1; Dispatch (Context, Request, Reply);
               case Reply.Status is
                  when AML_Field_Access.Denied | AML_Field_Access.Failed_Completed => return;
                  when AML_Field_Access.Indeterminate =>
                     E.Field_Uncertain := (True, Reply.Provider, Request, Reply.Operation);
                     E.Uncertain_Field := F.Identity; E.Uncertain_Region := F.Region;
                     Status := AML_Execute.Field_Indeterminate; return;
                  when AML_Field_Access.Completed =>
                     if (Writing and then Reply.Value /= 0) or else
                       (not Writing and then Reply.Value > AML_Field_Datums.Width_Mask (Item.Geometry.Width)) then return; end if;
                     Number := Reply.Value; Good := True;
               end case;
            end Bank_Transfer;
            procedure Execute (Good : out Boolean) is
               Accepted, Charged : Boolean;
               Child : Reference;
               Number : AML_Decode.Integer_Value;
               Writing : Boolean;
               Selected : Binding_Slot;
            begin
               Top := 0; Push (Ref, For_Write, Input, Good); if not Good then return; end if;
               while Top > 0 loop
                  pragma Loop_Variant (Decreases => Work);
                  Charge (Charged); if not Charged then Good := False; return; end if;
                  case Stack (Top).Step is
                     when Start_Datum =>
                        if Stack (Top).Info.Organization = Direct_Region_Field
                          and then Stack (Top).Info.Bits <= Scalar_Bits then
                           declare
                              Datum : AML_Execute.Datum := (AML_Execute.Integer_Datum,
                                Stack (Top).Input, AML_Decode.Ordinary_Integer);
                           begin
                              Perform_Direct_Field (E, Context, Items, Stack (Top).Identity, AML_Decode.Bits_64,
                                Stack (Top).Writing, Datum, Status, Remaining);
                              if Status /= AML_Execute.Returned then Good := False; return; end if;
                              if not Stack (Top).Writing then
                                 if Datum.Value_Kind /= AML_Execute.Integer_Datum then
                                    Status := AML_Execute.Unsupported_Value; Good := False; return;
                                 end if;
                                 Last_Scalar := Datum.Number;
                              else Last_Scalar := 0; end if;
                              Top := Top - 1;
                           end;
                        elsif Stack (Top).Position = Stack (Top).Number then
                           Last_Scalar := Stack (Top).Accumulator; Top := Top - 1;
                        else
                           if Stack (Top).Info.Organization /= Indexed_Register_Field then
                              Select_Region_Target (Stack (Top), Selected, Accepted);
                              if not Accepted then Good := False; return; end if;
                              Stack (Top).Choice := Selected;
                           end if;
                           Writing := Stack (Top).Writing;
                           if Writing then
                              declare D : constant AML_Field_Datums.Datum := Datum_At (Stack (Top)); begin
                                 if AML_Field_Datums.Needs_Read (D.Access_Width, D.Mask, Stack (Top).Rule) then Writing := False;
                                 else Merge_Write (Stack (Top), 0); end if;
                              end;
                           end if;
                           Start_Transfer (Writing, Accepted); if not Accepted then Good := False; return; end if;
                        end if;
                     when After_Index_Read | After_Index_Write =>
                        Writing := Stack (Top).Step = After_Index_Write;
                        Child := Stack (Top).Data_Source; Number := Stack (Top).Pending_Value;
                        Stack (Top).Step := (if Writing then After_Data_Write else After_Data_Read);
                        Push (Child, Writing, Number, Accepted); if not Accepted then Good := False; return; end if;
                     when After_Bank_Read | After_Bank_Write =>
                        Writing := Stack (Top).Step = After_Bank_Write;
                        Bank_Transfer (Stack (Top), Writing, Last_Scalar, Accepted);
                        if not Accepted then Good := False; return; end if;
                        Stack (Top).Step := (if Writing then After_Data_Write else After_Data_Read);
                     when After_Data_Read =>
                        if Stack (Top).Writing then
                           Merge_Write (Stack (Top), Last_Scalar);
                           Start_Transfer (True, Accepted); if not Accepted then Good := False; return; end if;
                        else
                           if Stack (Top).Info.Organization = Indexed_Register_Field then
                              Accumulate_Indexed_Read (Stack (Top), Last_Scalar);
                           else
                              Accumulate_Region_Read (Stack (Top), Last_Scalar);
                           end if;
                           Stack (Top).Position := Stack (Top).Position + 1; Stack (Top).Step := Start_Datum;
                        end if;
                     when After_Data_Write =>
                        Stack (Top).Position := Stack (Top).Position + 1; Stack (Top).Step := Start_Datum;
                  end case;
               end loop;
               Status := AML_Execute.Returned; Good := True;
            end Execute;
            Good : Boolean;
         begin
            Status := AML_Execute.Unsupported_Value;
            if not Named_Identity_Matches (E, Ref) then return; end if;
            if Bits > Maximum_Field_Bits then Status := AML_Execute.Value_Limit; return; end if;
            if For_Write then
               case Value.Value_Kind is
                  when AML_Execute.Integer_Datum => Input := AML_Integers.Normalize (Value.Number, Width);
                  when AML_Execute.Object_Datum =>
                     if not Has_Source (E, Value.Object.Source) or else Value.Object.ID /= AML_References.Source (Value.Object.Source)
                       or else AML_Objects.Kind (E.Tree.Values, Value.Object.ID) not in AML_Objects.String_Object | AML_Objects.Buffer_Object then return; end if;
                     Input_Object := Value.Object.ID;
                     for I in 1 .. Natural'Min (Scalar_Octets, AML_Objects.Length (E.Tree.Values, Value.Object.ID)) loop
                        Input := Input or Interfaces.Shift_Left (AML_Decode.Integer_Value
                          (AML_Objects.Stored_Byte (E.Tree.Values, Value.Object.ID, I - 1)), (I - 1) * AML_Field_Layout.Bits_Per_Byte);
                     end loop;
                  when others => return;
               end case;
            end if;
            Preflight (Ref, For_Write, Input, Good); if not Good then return; end if;
            if not For_Write and then Natural (Bits) > AML_Integers.Bit_Width (Width) then
               declare
                  use type AML_Objects.Buffer_Update_Status;
                  Candidate : AML_Objects.State := E.Tree.Values;
                  Data : AML_Decode.Bytes (1 .. (Natural (Bits) + AML_Field_Layout.Bits_Per_Byte - 1) /
                    AML_Field_Layout.Bits_Per_Byte) := [others => 0];
                  ID : AML_Objects.Object_ID;
                  Allocated : AML_Objects.Allocation_Status;
                  Updated : AML_Objects.Buffer_Update_Status;
                  Source : AML_References.Object_Handle;
               begin
                  AML_Objects.New_Bytes (Candidate, AML_Objects.Buffer_Object, Data, ID, Allocated);
                  if Allocated /= AML_Objects.Allocated then Status := AML_Execute.Value_Limit; return; end if;
                  Execute (Good); if not Good then return; end if;
                  for I in Data'Range loop
                     Data (I) := (if Bits > Scalar_Bits then Wide_Output (I)
                       else AML_Decode.Byte (Interfaces.Shift_Right (Last_Scalar,
                         (I - 1) * AML_Field_Layout.Bits_Per_Byte) and 16#FF#));
                  end loop;
                  AML_Objects.Store_Buffer (Candidate, ID, Data, Updated);
                  if Updated /= AML_Objects.Buffer_Updated then
                     raise Program_Error with "preallocated composed field buffer invariant";
                  end if;
                  E.Tree.Values := Candidate;
                  Owned.Make_Source (E, ID, Source, Good);
                  if not Good then raise Program_Error with "composed field result source invariant"; end if;
                  Owned.Read_Source (E, Source, Value, Status);
               end;
            else
               Execute (Good);
               if Good and then not For_Write then
                  Value := (AML_Execute.Integer_Datum,
                    AML_Integers.Normalize (Last_Scalar, Width), AML_Decode.Ordinary_Integer);
               end if;
            end if;
         end Perform_Composed_Field;
         procedure Perform_Bound_Field
           (E : in out Owned.Arena; Context : in out IO_Context; Items : Binding_Set; Ref : Reference;
            Width : AML_Decode.Integer_Width; For_Write : Boolean;
            Value : in out AML_Execute.Datum; Status : out AML_Execute.Execution_Status;
            Work_Allowance : AML_Execution_Limits.Field_Work_Count := Maximum_Work) is
            use type AML_Execution_Limits.Field_Work_Count;
            Remaining : AML_Execution_Limits.Field_Request_Count := Maximum_Requests;
         begin
            if E.Field_Uncertain.Present or else Pending (Context) then
               Status := AML_Execute.Field_Indeterminate; return;
            end if;
            if Work_Allowance = 0 then Status := AML_Execute.Value_Limit; return; end if;
            if AML_References.Kind (Ref) = AML_References.Named_Cell
              and then Named_Identity_Matches (E, Ref)
            then
               declare N : constant Node_ID := Node_ID (AML_References.Named_Node (Ref)); begin
                  if E.Tree.Items (N).Object_Type = Region_Field_Object and then
                    (E.Tree.Items (N).Region_Field_Info.Organization /= Direct_Region_Field
                     or else E.Tree.Items (N).Region_Field_Info.Bits > AML_Integers.Bit_Width (AML_Decode.Bits_64)) then
                     Perform_Composed_Field (E, Context, Items, Ref, Width, For_Write, Value, Status, Remaining, Work_Allowance); return;
                  end if;
               end;
            end if;
            Perform_Direct_Field (E, Context, Items, Ref, Width, For_Write, Value, Status, Remaining);
         end Perform_Bound_Field;
      procedure Execute_Registered_Modules
        (A : in out Arena; IO : in out IO_Context;
         Targets : Module_Target_Set; Count : Module_Target_Count;
         Loads : in out AML_Definition_Loads.State; Copies : in out AML_Copied_Tables.State; Input : aliased AML_Table_Backing.State;
         Budget : Natural; Report : out Module_Report; Status : out Access_Status) is
         Provider_May_Have_Effects : Boolean := False;
         function Keep_Tree (Context : IO_Context) return Boolean is
            pragma Unreferenced (Context);
         begin return Provider_May_Have_Effects; end Keep_Tree;
         procedure Perform
           (E : in out Owned.Arena; Context : in out IO_Context; Ref : Reference;
            Width : AML_Decode.Integer_Width; For_Write : Boolean;
            Value : in out AML_Execute.Datum; Status : out AML_Execute.Execution_Status) is
            Prepared : Binding_Set;
            Good : Boolean := False;
            Root_Node : Node_ID;
            Work : AML_Execution_Limits.Field_Work_Count := Maximum_Work;
            use type AML_Execution_Limits.Field_Work_Count;
            use type AML_Field_Datums.Datum_Index;
            Scalar_Bits : constant Positive := AML_Integers.Bit_Width (AML_Decode.Bits_64);
            Needed : array (Node_ID) of Boolean := [others => False];
            procedure Charge (Allowed : out Boolean) is
            begin
               Allowed := Work > 0;
               Work := AML_Execution_Limits.Charged_Field_Work (Work);
               if not Allowed then Status := AML_Execute.Value_Limit; end if;
            end Charge;
            procedure Select_Provider (Target : AML_Field_Access.Target; Allowed : out Boolean) is
            begin
               if Prepared.Count = 0 and then Prepared.Register_Count = 0 then
                  Prepared.Owner := E.Token; Prepared.Provider := Target.Provider;
                  Allowed := True; return;
               end if;
               Allowed := Prepared.Owner = E.Token and then Prepared.Provider = Target.Provider;
            end Select_Provider;
            procedure Bind_Direct (Node : Node_ID; Allowed : out Boolean) is
               Key : Field_Key := No_Field_Key;
               Candidate, Selected : Binding;
               Bound : Access_Status;
               Found, Matched, Current : Boolean := False;
               Geometry : AML_Field_Layout.Register_Geometry;
               Field : AML_Field_Layout.Field_Geometry;
               Plan : AML_Field_Plans.Plan;
               Region : constant Node_ID := E.Tree.Items (Node).Region_Field_Info.Region;
            begin
               Allowed := False;
               if Region = Root or else Region > E.Tree.Used then return; end if;
               Make_Named_Identity (E, Natural (Node), Key.Field, Current);
               if not Current then return; end if;
               Make_Named_Identity (E, Natural (Region), Key.Region, Current);
               if not Current or else not Key_Matches (E, Key) then return; end if;
               for Position in Module_Target_Position loop
                  exit when Module_Target_Count (Position) > Count;
                  Charge (Current); if not Current then return; end if;
                  Bind_Owned (E, Context, Key, Targets (Position), Candidate, Bound);
                  if Bound = Available then
                     if Found then return; end if;
                     Selected := Candidate; Found := True;
                  end if;
               end loop;
               if not Found then
                  Field := Geometry_Of (E, Key); Plan := AML_Field_Plans.Prepare (Field);
                  if not AML_Field_Plans.Valid (Plan) then return; end if;
                  Selected := No_Binding;
                  Selected.Field := Key.Field; Selected.Region := Key.Region;
                  Selected.Read_Count := AML_Field_Plans.Count (Plan);
                  Selected.Read_Field := Field; Selected.Writes_Enabled := True;
                  for Datum in AML_Field_Plans.Datum_Position range 1 .. Selected.Read_Count loop
                     Matched := False;
                     for Position in Module_Target_Position loop
                        exit when Module_Target_Count (Position) > Count;
                        Charge (Current); if not Current then return; end if;
                        Describe_Target (Context, Targets (Position), Geometry, Current);
                        if Current and then AML_Field_Plans.Matches (Plan, Datum, Geometry) then
                           if Matched then return; end if;
                           Matched := True;
                           Selected.Read_Targets (Datum) := Targets (Position);
                           Selected.Read_Geometries (Datum) := Geometry;
                        end if;
                     end loop;
                     if not Matched or else Selected.Read_Targets (Datum).Provider /=
                       Selected.Read_Targets (AML_Field_Plans.Datum_Position'First).Provider then return; end if;
                  end loop;
                  Selected.Target := Selected.Read_Targets (AML_Field_Plans.Datum_Position'First);
                  Selected.Geometry := Selected.Read_Geometries (AML_Field_Plans.Datum_Position'First);
               end if;
               if Prepared.Count = Binding_Count'Last then Status := AML_Execute.Value_Limit; return; end if;
               Select_Provider (Selected.Target, Current); if not Current then return; end if;
               Prepared.Count := Prepared.Count + 1;
               Prepared.Entries (Prepared.Count) := Selected;
               Allowed := True;
            end Bind_Direct;
            procedure Bind_Region (Node : Node_ID; Allowed : out Boolean) is
               Info : constant Region_Field_Attributes := E.Tree.Items (Node).Region_Field_Info;
               Geometry : AML_Field_Layout.Field_Geometry;
               Probe : AML_Field_Layout.Field_Geometry;
               Register : AML_Field_Layout.Register_Geometry;
               Plan : AML_Field_Datums.Plan;
               Access_Plan : AML_Field_Plans.Plan;
               Region : Reference;
               Chosen : AML_Field_Access.Target;
               Chosen_Geometry : AML_Field_Layout.Register_Geometry;
               Found, Current, Readable, Writable, Duplicate : Boolean;
            begin
               Allowed := False;
               if Info.Region = Root or else Info.Region > E.Tree.Used
                 or else E.Tree.Items (Info.Region).Object_Type /= Operation_Region_Object
                 or else E.Tree.Items (Info.Region).Incarnation /= Info.Incarnation
                 or else E.Tree.Items (Info.Region).Initializing then return; end if;
               Make_Named_Identity (E, Natural (Info.Region), Region, Current);
               if not Current then return; end if;
               declare R : constant Operation_Region_Attributes := E.Tree.Items (Info.Region).Operation_Info; begin
                  Geometry := (Region => (Space => AML_Decode.Byte (R.Space), Base => R.Address, Length => R.Length),
                     Bit_Offset => AML_Decode.Integer_Value (Info.Offset), Bit_Length => AML_Decode.Integer_Value (Info.Bits),
                     Flags => Info.Raw_Flags, Effective_Access => Info.Access_Type,
                     Attribute => Info.Attribute, Access_Length => Info.Access_Length, others => <>);
               end;
               Probe := Geometry;
               Probe.Bit_Length := AML_Decode.Integer_Value'Min (Probe.Bit_Length, AML_Field_Plans.Maximum_Bits);
               Access_Plan := AML_Field_Plans.Prepare (Probe);
               if not AML_Field_Plans.Valid (Access_Plan) then return; end if;
               Plan := AML_Field_Datums.Prepare (Geometry.Region.Base, Geometry.Region.Length,
                  AML_Field_Datums.Bit_Position (Geometry.Bit_Offset), AML_Field_Datums.Bit_Length (Geometry.Bit_Length),
                  AML_Field_Plans.Datum_At (Access_Plan, AML_Field_Plans.Datum_Position'First).Access_Width);
               if not AML_Field_Datums.Valid (Plan) then return; end if;
               for Datum in AML_Field_Datums.Datum_Index range 0 .. AML_Field_Datums.Count (Plan) - 1 loop
                  declare D : constant AML_Field_Datums.Datum := AML_Field_Datums.At_Index (Plan, Datum); begin
                     Found := False; Chosen := AML_Field_Access.No_Target;
                     for Position in Module_Target_Position loop
                        exit when Module_Target_Count (Position) > Count;
                        Charge (Current); if not Current then return; end if;
                        Describe_Target (Context, Targets (Position), Register, Current);
                        if Current and then Register.Region = Geometry.Region
                          and then Register.Byte_Offset = AML_Decode.Integer_Value (D.Offset)
                          and then Register.Width = D.Access_Width then
                           if Found then return; end if;
                           Found := True; Chosen := Targets (Position); Chosen_Geometry := Register;
                        end if;
                     end loop;
                     if not Found then return; end if;
                     Select_Provider (Chosen, Current); if not Current then return; end if;
                     Duplicate := False;
                     for Position in Binding_Count range 1 .. Prepared.Register_Count loop
                        if Prepared.Registers (Position).Region = Region
                          and then Prepared.Registers (Position).Target = Chosen then
                           if Prepared.Registers (Position).Geometry /= Chosen_Geometry then return; end if;
                           Duplicate := True;
                        end if;
                     end loop;
                     if not Duplicate then
                        if Prepared.Register_Count = Binding_Count'Last then Status := AML_Execute.Value_Limit; return; end if;
                        Authorize_Target (Context, Chosen, False, Readable);
                        Authorize_Target (Context, Chosen, True, Writable);
                        Prepared.Register_Count := Prepared.Register_Count + 1;
                        Prepared.Registers (Prepared.Register_Count) :=
                           (Region, Chosen, Chosen_Geometry, Readable, Writable);
                     end if;
                  end;
               end loop;
               Allowed := True;
            end Bind_Region;
            procedure Require_Source (Parent, Child : Node_ID;
               Incarnation : AML_References.Node_Incarnation; Allowed : out Boolean) is
            begin
               Allowed := Child > Root and then Child < Parent
                 and then E.Tree.Items (Child).Object_Type = Region_Field_Object
                 and then E.Tree.Items (Child).Incarnation = Incarnation
                 and then not E.Tree.Items (Child).Initializing;
               if Allowed then Needed (Child) := True; end if;
            end Require_Source;
         begin
            Status := AML_Execute.Unsupported_Value;
            if E.Field_Uncertain.Present or else Pending (Context) then
               Status := AML_Execute.Field_Indeterminate; return;
            end if;
            if AML_References.Kind (Ref) /= AML_References.Named_Cell
              or else not Named_Identity_Matches (E, Ref) then return; end if;
            Root_Node := Node_ID (AML_References.Named_Node (Ref));
            if E.Tree.Items (Root_Node).Object_Type /= Region_Field_Object then return; end if;
            Needed (Root_Node) := True;
            -- Dependencies bind earlier field incarnations. A descending walk
            -- visits the complete reachable graph without recursion or AML.
            for Node in reverse Node_ID range Root + 1 .. Root_Node loop
               Charge (Good); if not Good then return; end if;
               if Needed (Node) then
                  if E.Tree.Items (Node).Initializing then return; end if;
                  declare Info : constant Region_Field_Attributes := E.Tree.Items (Node).Region_Field_Info; begin
                     if Info.Organization = Indexed_Register_Field then
                        Require_Source (Node, Info.Index_Register, Info.Index_Incarnation, Good);
                        if not Good then return; end if;
                        Require_Source (Node, Info.Data_Register, Info.Data_Incarnation, Good);
                        if not Good then return; end if;
                     else
                        if Info.Organization = Banked_Region_Field then
                           Require_Source (Node, Info.Bank_Register, Info.Bank_Incarnation, Good);
                           if not Good then return; end if;
                        end if;
                        if Info.Organization = Direct_Region_Field and then Info.Bits <= Scalar_Bits then
                           Bind_Direct (Node, Good);
                        else Bind_Region (Node, Good); end if;
                        if not Good then return; end if;
                     end if;
                  end;
               end if;
            end loop;
            -- The existing executor preflights all concrete directions before
            -- selector/data IO, then rechecks rights/geometry on each access.
            Provider_May_Have_Effects := True;
            Perform_Bound_Field (E, Context, Prepared, Ref, Width, For_Write, Value, Status, Work);
         end Perform;
         procedure Run is new Execute_Modules_With_Context (IO_Context, Perform, Keep_Tree);
      begin
         Report := (Outcome => Module_Rejected, Failure => AML_Execute.Field_Indeterminate, others => <>);
         Status := Invalid_Value;
         if A.Inner.Field_Uncertain.Present or else Pending (IO) then return; end if;
         for Position in Module_Target_Position loop
            if Module_Target_Count (Position) > Count and then Targets (Position) /= AML_Field_Access.No_Target then return; end if;
         end loop;
         Run (A, Loads, Copies, True, IO, Input, Budget, Report, Status);
      end Execute_Registered_Modules;
      procedure Invoke_With_Loads (A : in out Arena; IO : in out IO_Context; Items : Binding_Set;
         Loads : in out AML_Definition_Loads.State; Copies : in out AML_Copied_Tables.State; Registered : Boolean;
         Input : aliased AML_Table_Backing.State;
         Node : Node_ID; Args : Arguments; Count : Argument_Count;
         Budget : Natural; Outcome : out Result; Status : out Access_Status) is
         use type AML_Definition_Loads.Load_Phase;
         Raw_Args : AML_Execute.Value_Arguments :=
           [others => (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer)];
         Raw : AML_Execute.Execution_Result;
         Root : Retained_Root;
         Retention : Invocation_Retention_Status;
         Read_Status : AML_Execute.Execution_Status;
         subtype Failure_Status is AML_Execute.Payload_Free_Failure;
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
         procedure Perform
           (E : in out Owned.Arena; Context : in out IO_Context; Ref : Reference;
            Width : AML_Decode.Integer_Width; For_Write : Boolean;
            Value : in out AML_Execute.Datum; Status : out AML_Execute.Execution_Status) is
         begin
            Perform_Bound_Field (E, Context, Items, Ref, Width, For_Write, Value, Status);
         end Perform;
         procedure Run is new Invoke_Context_Retained (IO_Context, Perform, Collect_Before);
         procedure Run_Registered is new Invoke_Registered_Context_Retained (IO_Context, Perform, Collect_Before);
      begin
         Outcome := (Status => AML_Execute.No_Return, Charged => 0);
         Status := Admission (A); if Status /= Available then return; end if;
         if A.Inner.Field_Uncertain.Present or else Pending (IO) then
            Outcome := (AML_Execute.Field_Indeterminate, 0); return;
         end if;
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
         if Registered then
            Run_Registered (A.Inner, IO, Loads, Copies, Input, Node, Raw_Args, Count, Budget, Raw, Root, Retention);
         else
            Run (A.Inner, IO, Input, Node, Raw_Args, Count, Budget, Raw, Root, Retention);
         end if;
         A.Collections := Local_Metrics;
         A.In_Progress := False;
         if Registered then
            for I in 1 .. Loads.Capacity loop
               if AML_Definition_Loads.Phase (Loads, I) = AML_Definition_Loads.Failed then
                  A.State := Loading; A.Import := Module_Failed;
               end if;
            end loop;
         end if;
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
            when AML_Execute.Metadata_Returned =>
               Outcome := (AML_Execute.Metadata_Returned, Raw.Charged, (Root => Root));
            when AML_Execute.Fatal_Requested => Outcome := (AML_Execute.Fatal_Requested, Raw.Charged, Raw.Fatal);
            when AML_Execute.Module_Completed => Outcome := (AML_Execute.Unsupported_Value, Raw.Charged);
            when others => Outcome := (Failure_Status (Raw.Status), Raw.Charged);
         end case;
      end Invoke_With_Loads;
      procedure Invoke (A : in out Arena; IO : in out IO_Context; Items : Binding_Set;
         Input : aliased AML_Table_Backing.State; Node : Node_ID; Args : Arguments; Count : Argument_Count;
         Budget : Natural; Outcome : out Result; Status : out Access_Status) is
         No_Loads : AML_Definition_Loads.State (1);
         No_Copies : AML_Copied_Tables.State (0, 0);
      begin Invoke_With_Loads (A, IO, Items, No_Loads, No_Copies, False, Input, Node, Args, Count, Budget, Outcome, Status); end Invoke;
      procedure Invoke_Registered (A : in out Arena; Loads : in out AML_Definition_Loads.State; Copies : in out AML_Copied_Tables.State;
         IO : in out IO_Context; Items : Binding_Set; Input : aliased AML_Table_Backing.State;
         Node : Node_ID; Args : Arguments; Count : Argument_Count;
         Budget : Natural; Outcome : out Result; Status : out Access_Status) is
      begin Invoke_With_Loads (A, IO, Items, Loads, Copies, True, Input, Node, Args, Count, Budget, Outcome, Status); end Invoke_Registered;

      procedure Invoke (A : in out Arena; IO : in out IO_Context; Item : Binding;
         Input : aliased AML_Table_Backing.State; Node : Node_ID;
         Args : Arguments; Count : Argument_Count; Budget : Natural;
         Outcome : out Result; Status : out Access_Status) is
         Items : Binding_Set;
      begin
         -- Preserve singleton entry admission; effects use the common engine.
         Outcome := (Status => AML_Execute.No_Return, Charged => 0);
         Status := Admission (A); if Status /= Available then return; end if;
         if A.Inner.Field_Uncertain.Present or else Pending (IO) then
            Outcome := (AML_Execute.Field_Indeterminate, 0); return;
         end if;
         if not Field_Key_Matches (A, Key_Of (Item)) then
            Status := Invalid_Value; return;
         end if;
         Items.Count := 1; Items.Owner := A.Inner.Token;
         Items.Provider := Item.Target.Provider; Items.Entries (1) := Item;
         Invoke (A, IO, Items, Input, Node, Args, Count, Budget, Outcome, Status);
      end Invoke;
      procedure Invoke_Registered (A : in out Arena; Loads : in out AML_Definition_Loads.State; Copies : in out AML_Copied_Tables.State;
         IO : in out IO_Context; Item : Binding;
         Input : aliased AML_Table_Backing.State; Node : Node_ID;
         Args : Arguments; Count : Argument_Count; Budget : Natural;
         Outcome : out Result; Status : out Access_Status) is
         Items : Binding_Set;
      begin
         -- Preserve singleton entry admission; effects use the common engine.
         Outcome := (Status => AML_Execute.No_Return, Charged => 0);
         Status := Admission (A); if Status /= Available then return; end if;
         if A.Inner.Field_Uncertain.Present or else Pending (IO) then
            Outcome := (AML_Execute.Field_Indeterminate, 0); return;
         end if;
         if not Field_Key_Matches (A, Key_Of (Item)) then
            Status := Invalid_Value; return;
         end if;
         Items.Count := 1; Items.Owner := A.Inner.Token;
         Items.Provider := Item.Target.Provider; Items.Entries (1) := Item;
         Invoke_Registered (A, Loads, Copies, IO, Items, Input, Node, Args, Count, Budget, Outcome, Status);
      end Invoke_Registered;
      procedure Evaluate_Named
        (A : in out Arena; IO : in out IO_Context; Items : Binding_Set;
         Input : aliased AML_Table_Backing.State; Item : Node_Descriptor;
         Width : AML_Decode.Integer_Width; Budget : Natural;
         Outcome : out Result; Status : out Access_Status) is
         procedure Method_Value
           (E : in out Arena; Context : in out IO_Context;
            Backing : aliased AML_Table_Backing.State; Node : Node_ID;
            Fuel : Natural; Value : out Result; Access_Result : out Access_Status) is
         begin
            Invoke (E, Context, Items, Backing, Node,
              [others => (Immediate_Argument, 0)], 0, Fuel, Value, Access_Result);
         end Method_Value;
         procedure Region_Value
           (E : in out Owned.Arena; Context : in out IO_Context;
            Ref : Reference; Integer_Width : AML_Decode.Integer_Width;
            Value : in out AML_Execute.Datum; Read_Status : out AML_Execute.Execution_Status) is
         begin
            Perform_Bound_Field (E, Context, Items, Ref, Integer_Width, False, Value, Read_Status);
         end Region_Value;
         procedure Evaluate is new Evaluate_With_Context
           (IO_Context, True, Method_Value, Region_Value);
      begin
         Outcome := (Status => AML_Execute.No_Return, Charged => 0);
         Status := Admission (A); if Status /= Available then return; end if;
         if A.Inner.Field_Uncertain.Present or else Pending (IO) then
            Outcome := (AML_Execute.Field_Indeterminate, 0); return;
         end if;
         Evaluate (A, IO, Input, Item, Width, Budget, Outcome, Status);
      end Evaluate_Named;
      procedure Evaluate_Registered
        (A : in out Arena; Loads : in out AML_Definition_Loads.State; Copies : in out AML_Copied_Tables.State;
         IO : in out IO_Context; Items : Binding_Set;
         Input : aliased AML_Table_Backing.State; Item : Node_Descriptor;
         Width : AML_Decode.Integer_Width; Budget : Natural;
         Outcome : out Result; Status : out Access_Status) is
         procedure Method_Value
           (E : in out Arena; Context : in out IO_Context;
            Backing : aliased AML_Table_Backing.State; Node : Node_ID;
            Fuel : Natural; Value : out Result; Access_Result : out Access_Status) is
         begin
            Invoke_Registered (E, Loads, Copies, Context, Items, Backing, Node,
              [others => (Immediate_Argument, 0)], 0, Fuel, Value, Access_Result);
         end Method_Value;
         procedure Region_Value
           (E : in out Owned.Arena; Context : in out IO_Context;
            Ref : Reference; Integer_Width : AML_Decode.Integer_Width;
            Value : in out AML_Execute.Datum; Read_Status : out AML_Execute.Execution_Status) is
         begin
            Perform_Bound_Field (E, Context, Items, Ref, Integer_Width, False, Value, Read_Status);
         end Region_Value;
         procedure Read_Registered_Table
           (Tree : in out State; Backing : aliased AML_Table_Backing.State; Scope : Natural;
            Path : AML_Names.Name_Result; Integer_Width : AML_Decode.Integer_Width;
            Purpose : AML_Execute.Binding_Purpose; Binding : out AML_Execute.Binding_Result) is
         begin
            Lookup_With_Owned_Tables (Tree, Backing, Copies, Scope, Path, Integer_Width, Purpose, Binding);
         end Read_Registered_Table;
         procedure Evaluate is new Evaluate_With_Context
           (IO_Context, True, Method_Value, Region_Value, Read_Registered_Table);
      begin
         Outcome := (Status => AML_Execute.No_Return, Charged => 0);
         Status := Admission (A); if Status /= Available then return; end if;
         if A.Inner.Field_Uncertain.Present or else Pending (IO) then
            Outcome := (AML_Execute.Field_Indeterminate, 0); return;
         end if;
         Evaluate (A, IO, Input, Item, Width, Budget, Outcome, Status);
      end Evaluate_Registered;
      end Field_IO;


      procedure Release (A : in out Arena; Handle : in out Value_Handle;
         Status : out Access_Status) is
         Released : Release_Status;
      begin
         -- Pin deletion remains available after terminal module failure.
         -- Value access and AML entry keep their ordinary admission checks.
         if A.In_Progress then Status := Busy; return;
         elsif A.State /= Ready and then
           (A.State /= Loading or else A.Import /= Module_Failed)
         then Status := Wrong_Phase; return; end if;
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
            when AML_Execute.Metadata_Datum =>
               declare Metadata : AML_Execute.Reference_Metadata; begin
                  Owned.Describe_Metadata_Value (A.Inner, Value.Metadata, Metadata);
                  if Metadata.Kind = AML_Execute.Metadata_Only then
                     Description := (Metadata_Description, Metadata.Object_Type);
                  else Status := Invalid_Value; end if;
               end;
            when AML_Execute.Object_Datum =>
               case Value.Object.Type_Code is
                  when 2 => Description := (String_Description, Value.Object.Size);
                  when 3 => Description := (Buffer_Description, Value.Object.Size);
                  when 4 => Description := (Package_Description, Value.Object.Size);
                  when 7 => Description := (Kind => Event_Description);
                  when AML_Mutexes.Object_Type_Code => Description := (Metadata_Description, AML_Execute.Mutex_Metadata);
                  when others => Status := Wrong_Kind;
               end case;
         end case;
      end Describe;
      procedure Read_Device_Metadata (A : Arena; Handle : Value_Handle;
         Item : out Node_Descriptor; Status : out Access_Status) is
         Value : AML_Execute.Datum;
         Identity : AML_References.Reference;
         Node : Node_ID;
      begin
         Item := No_Node;
         Fetch (A, Handle, Value, Status); if Status /= Available then return; end if;
         if Value.Value_Kind /= AML_Execute.Metadata_Datum
           or else Value.Metadata.Mode /= AML_Execute.Live_Namespace then
            Status := Wrong_Kind; return;
         end if;
         Identity := Value.Metadata.Namespace_Identity;
         if not Named_Identity_Matches (A.Inner, Identity)
           or else AML_References.Kind (Identity) /= AML_References.Named_Cell then
            Status := Invalid_Value; return;
         end if;
         Node := Node_ID (AML_References.Named_Node (Identity));
         if Kind (A.Inner.Tree, Node) /= Device_Object then Status := Wrong_Kind; return; end if;
         Describe_Node (A, Node, Item, Status);
      end Read_Device_Metadata;
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
      type Constructed_Kind is (Buffer_Argument, Empty_Package_Argument);
      procedure Create_Object
        (A : in out Arena; Kind : Constructed_Kind; Data : AML_Decode.Bytes;
         Outcome : out Result; Status : out Access_Status) is
         use type Pins.Result_Status;
         Reserved : Retained_Root;
         Retention : Retain_Status;
         Removed : Release_Status;
         ID : AML_Objects.Object_ID;
         Allocation : AML_Objects.Allocation_Status;
         Source : AML_References.Object_Handle;
         Value : AML_Execute.Datum;
         Read_Status : AML_Execute.Execution_Status;
         Pin_Status : Pins.Result_Status;
         Has_Source : Boolean;
      begin
         Outcome := (Status => AML_Execute.Unsupported_Value, Charged => 0);
         Status := Admission (A); if Status /= Available then return; end if;
         -- Reserve a published placeholder before changing the object store.
         -- Root refusal therefore cannot allocate an unobservable object.
         Owned.Retain (A.Inner,
           (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer), Reserved, Retention);
         case Retention is
            when Owned.Root_Limit => Status := Root_Limit; return;
            when Owned.Identity_Exhausted => Status := Identity_Exhausted; return;
            when Owned.Invalid_Value => Status := Invalid_Value; return;
            when Retained => null;
         end case;
         case Kind is
            when Buffer_Argument =>
               AML_Objects.New_Buffer (A.Inner.Tree.Values,
                 AML_Decode.Integer_Value (Data'Length), Data, ID, Allocation);
            when Empty_Package_Argument =>
               AML_Objects.New_Package (A.Inner.Tree.Values, 0, ID, Allocation);
         end case;
         case Allocation is
            when AML_Objects.Allocated => null;
            when AML_Objects.Object_Limit | AML_Objects.Generation_Limit |
              AML_Objects.Byte_Limit | AML_Objects.Element_Limit =>
               Owned.Release (A.Inner, Reserved, Removed);
               if Removed /= Owned.Released then
                  raise Program_Error with "object construction reservation invariant";
               end if;
               Outcome := (Status => AML_Execute.Value_Limit, Charged => 0);
               if Allocation = AML_Objects.Generation_Limit then Status := Identity_Exhausted; end if;
               return;
            when others =>
               raise Program_Error with "object construction allocation invariant";
         end case;
         Owned.Make_Source (A.Inner, ID, Source, Has_Source);
         if not Has_Source then raise Program_Error with "constructed object identity invariant"; end if;
         Owned.Read_Source (A.Inner, Source, Value, Read_Status);
         if Read_Status /= AML_Execute.Returned or else Value.Value_Kind /= AML_Execute.Object_Datum then
            raise Program_Error with "constructed object read invariant";
         end if;
         -- No callback, collection or language execution intervenes before the
         -- reserved root is replaced by this exact newly owned object.
         Pins.Replace (A.Inner.Retention, Reserved.Pin, Value, Pin_Status);
         if Pin_Status /= Pins.Ready then raise Program_Error with "constructed result slot invariant"; end if;
         Outcome := (AML_Execute.Object_Returned, 0, (Root => Reserved));
      end Create_Object;
      procedure Create_Buffer
        (A : in out Arena; Data : AML_Decode.Bytes;
         Outcome : out Result; Status : out Access_Status) is
      begin
         Create_Object (A, Buffer_Argument, Data, Outcome, Status);
      end Create_Buffer;
      procedure Create_Empty_Package
        (A : in out Arena; Outcome : out Result; Status : out Access_Status) is
      begin
         Create_Object (A, Empty_Package_Argument, [], Outcome, Status);
      end Create_Empty_Package;
      type No_Evaluation_Context is null record;
      procedure Invoke_Default_Named
        (A : in out Arena; IO : in out No_Evaluation_Context;
         Input : aliased AML_Table_Backing.State; Node : Node_ID;
         Budget : Natural; Outcome : out Result; Status : out Access_Status) is
         pragma Unreferenced (IO);
      begin
         Invoke (A, Input, Node, [others => (Immediate_Argument, 0)], 0,
           Budget, Outcome, Status);
      end Invoke_Default_Named;
      procedure Unused_Region_Read
        (E : in out Owned.Arena; IO : in out No_Evaluation_Context;
         Ref : Reference; Width : AML_Decode.Integer_Width;
         Value : in out AML_Execute.Datum; Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (E, IO, Ref, Width, Value);
      begin
         Status := AML_Execute.Unsupported_Value;
      end Unused_Region_Read;
      procedure Evaluate_Default is new Evaluate_With_Context
        (No_Evaluation_Context, False, Invoke_Default_Named, Unused_Region_Read);
      procedure Evaluate_Named (A : in out Arena; Input : aliased AML_Table_Backing.State;
         Item : Node_Descriptor; Width : AML_Decode.Integer_Width; Budget : Natural;
         Outcome : out Result; Status : out Access_Status) is
         IO : No_Evaluation_Context;
      begin
         Evaluate_Default (A, IO, Input, Item, Width, Budget, Outcome, Status);
      end Evaluate_Named;
      procedure Evaluate_Registered (A : in out Arena; Loads : in out AML_Definition_Loads.State; Copies : in out AML_Copied_Tables.State;
         Input : aliased AML_Table_Backing.State; Item : Node_Descriptor;
         Width : AML_Decode.Integer_Width; Budget : Natural; Outcome : out Result; Status : out Access_Status) is
         IO : No_Evaluation_Context;
         procedure Method_Value (Owner : in out Arena; Context : in out No_Evaluation_Context;
            Backing : aliased AML_Table_Backing.State; Node : Node_ID;
            Fuel : Natural; Value : out Result; Access_Result : out Access_Status) is
            pragma Unreferenced (Context);
         begin Invoke_Registered (Owner, Loads, Copies, Backing, Node,
           [others => (Immediate_Argument, 0)], 0, Fuel, Value, Access_Result); end Method_Value;
         procedure Read_Registered_Table
           (Tree : in out State; Backing : aliased AML_Table_Backing.State; Scope : Natural;
            Path : AML_Names.Name_Result; Integer_Width : AML_Decode.Integer_Width;
            Purpose : AML_Execute.Binding_Purpose; Binding : out AML_Execute.Binding_Result) is
         begin
            Lookup_With_Owned_Tables (Tree, Backing, Copies, Scope, Path, Integer_Width, Purpose, Binding);
         end Read_Registered_Table;
         procedure Evaluate is new Evaluate_With_Context
           (No_Evaluation_Context, False, Method_Value, Unused_Region_Read, Read_Registered_Table);
      begin Evaluate (A, IO, Input, Item, Width, Budget, Outcome, Status); end Evaluate_Registered;
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
           Buffer_Object | Package_Object | Reference_Object | Metadata_Value_Object then Status := Wrong_Kind; return; end if;
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
               if Value.Value_Kind = AML_Execute.Metadata_Datum then
                  Outcome := (AML_Execute.Metadata_Returned, 0, Handle);
               elsif Value.Value_Kind = AML_Execute.Reference_Datum then
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
         else Owned.Resolve_Semantic_Reference (A.Inner, Saved.Ref, Target, Read_Status); end if;
         if Read_Status = AML_Execute.Uninitialized then Status := Uninitialized_Element;
         elsif Read_Status /= AML_Execute.Returned then Status := Invalid_Value;
         else Save (A, Target, Value, Status); end if;
      end Dereference;
   end Collecting;
end Owned;

end AML_Namespace;
