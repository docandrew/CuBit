pragma Ada_2022;
with AML_Fields;
with AML_Data;
with AML_Coercions;
with AML_Integers;
package body AML_Namespace with SPARK_Mode is
   use type AML_Decode.Bytes;
   use type AML_Objects.State;
   use type AML_Execute.Declaration_Status;
   use type AML_Execute.Execution_Status;
   subtype Code_Storage is AML_Decode.Bytes (1 .. AML_Execute.Max_Method_Bytes);
   procedure Append_Code
     (Store : in out Code_Storage; Used : in out AML_Execute.Method_Length;
      Data : AML_Decode.Bytes; Start : out AML_Execute.Method_Length)
     with Pre => Data'Length <= Store'Length - Used,
          Post => Start = Used'Old and then Used = Used'Old + Data'Length
            and then Store (1 .. Used'Old) = Store'Old (1 .. Used'Old)
            and then Store (Start + 1 .. Used) = Data
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
   function Present (Tree : State; Node : Node_ID) return Boolean is
     (Node = Root or else Tree.Items (Node).Alive);
   function Method_Usage (Tree : State) return AML_Execute.Method_Length is (Tree.Code_Used);
   function Method_Data (Tree : State; Node : Node_ID) return AML_Decode.Bytes is
     (Tree.Code (Tree.Items (Node).Method_Offset + 1 ..
                 Tree.Items (Node).Method_Offset + Tree.Items (Node).Method_Size));
   function Value_Usage (Tree : State) return AML_Objects.Usage is
     (AML_Objects.Usage_Of (Tree.Values));
   function Value_Store (Tree : State) return AML_Objects.State is (Tree.Values);
   function Data_Object (Tree : State; Node : Node_ID) return AML_Objects.Object_ID is
     (Tree.Items (Node).Object_Ref);
   function Integer_Updated
     (Tree, Prior : State; Node : Node_ID; Value : AML_Decode.Integer_Value)
      return Boolean is
     (Tree.Used = Prior.Used and then Tree.Items = Prior.Items
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
      elsif Tree.Used = Capacity then
         Result := Full;
      else
         Tree.Used := Tree.Used + 1;
         Tree.Items (Tree.Used) := (Up => Scope, Part => Part, others => <>);
         Node := Tree.Used;
         Result := Inserted;
      end if;
   end Insert;
   function Kind (Tree : State; Node : Node_ID) return Object_Kind is
     (if Node = Root then Scope_Object else Tree.Items (Node).Object_Type);
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
        Kind (Tree, Scope) not in Scope_Object | Device_Object | Method_Object or else
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
     (AML_Objects.Valid (Tree.Values) and then
       (for all I in 1 .. Tree.Used => Tree.Items (I).Up < I and then Tree.Items (I).Owner < I and then
          (if Tree.Items (I).Object_Type = Table_Field_Object then
             AML_Field_Data.Fits (Tree.Items (I).Table_Binding.Region.Extent,
               Tree.Items (I).Table_Binding.Offset, Tree.Items (I).Table_Binding.Bits)) and then
          Tree.Items (I).Method_Offset <= Tree.Code_Used and then
          Tree.Items (I).Method_Size <= Tree.Code_Used - Tree.Items (I).Method_Offset and then
          (if Tree.Items (I).Object_Type in Integer_Object | String_Object | Buffer_Object | Package_Object then
              Tree.Items (I).Object_Ref > 0 and then
              Tree.Items (I).Object_Ref <= AML_Objects.Count (Tree.Values) and then
              AML_Objects.Kind (Tree.Values, Tree.Items (I).Object_Ref) =
                (case Tree.Items (I).Object_Type is
                   when Integer_Object => AML_Objects.Integer_Object,
                   when String_Object => AML_Objects.String_Object,
                   when Buffer_Object => AML_Objects.Buffer_Object,
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
   procedure Load_Bound_Data is new AML_Data.Load_Bound (Count_Context, Read_Package_Count);

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
      elsif Kind (Tree, Located.Node) = Method_Object then
         return (Status => AML_Execute.Method_Binding,
                 Method_ID => Natural (Located.Node),
                 Parameters => Natural (Tree.Items (Located.Node).Method_Flags mod 8));
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
                 Object => (ID => (if Kind (Tree, Located.Node) in String_Object | Buffer_Object | Package_Object
                   then Tree.Items (Located.Node).Object_Ref else 0),
                 Conversion_32 => Conversion_32, Conversion_64 => Conversion_64,
                 Type_Code => (case Kind (Tree, Located.Node) is
                   when String_Object => 2, when Buffer_Object => 3,
                   when Package_Object => 4, when Device_Object => 6,
                   when Table_Region_Object => 10, when Table_Field_Object => 5,
                   when others => 0),
                 Size => (if Kind (Tree, Located.Node) in String_Object | Buffer_Object | Package_Object
                   then AML_Objects.Length (Tree.Values, Tree.Items (Located.Node).Object_Ref)
                   else 0)));
      end if;
      return (Status => AML_Execute.Integer_Binding,
              Value => Integer_Data (Tree, Located.Node));
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
      if not Has_Integer (Tree, Located.Node) or else Item.Is_Object then return; end if;
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

   procedure End_Method (Tree : in out State; Scope : Natural)
     with Pre => Valid_Context (Tree),
       Post => Valid_Context (Tree) and then Tree.Values = Tree'Old.Values
         and then Tree.Used <= Tree'Old.Used and then Tree.Code_Used <= Tree'Old.Code_Used
         and then (if Scope > 0 and then Scope <= Tree'Old.Used
           and then Tree'Old.Items (Scope).Active_Calls = 1 then
             (for all I in 1 .. Tree.Used =>
                not Tree.Items (I).Alive or else Tree.Items (I).Owner /= Scope))
   is
      Kept_Code : AML_Execute.Method_Length := 0;
   begin
      if Scope = 0 or else Scope > Tree.Used or else Tree.Items (Scope).Active_Calls = 0 then return; end if;
      Tree.Items (Scope).Active_Calls := Tree.Items (Scope).Active_Calls - 1;
      if Tree.Items (Scope).Active_Calls /= 0 then return; end if;
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
            Tree.Items (I).Alive := False;
         end if;
      end loop;
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
         Kept_Code := AML_Execute.Method_Length'Max
           (Kept_Code, Tree.Items (I).Method_Offset + Tree.Items (I).Method_Size);
      end loop;
      for I in Kept_Code + 1 .. Tree.Code_Used loop
         pragma Loop_Invariant (Tree.Used = Tree'Loop_Entry.Used);
         pragma Loop_Invariant (Tree.Items = Tree'Loop_Entry.Items);
         pragma Loop_Invariant (Tree.Values = Tree'Loop_Entry.Values);
         pragma Loop_Invariant (Tree.Code_Used = Tree'Loop_Entry.Code_Used);
         Tree.Code (I) := 0;
      end loop;
      Tree.Code_Used := Kept_Code;
   end End_Method;

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
      Start : AML_Execute.Method_Length;
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
      if Kind (Tree, Base) not in Scope_Object | Device_Object | Method_Object then
         Status := AML_Execute.Declaration_Missing; return;
      end if;
      if Code'Length > AML_Execute.Max_Method_Bytes - Tree.Code_Used then
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
      -- All parsing and failure paths precede the commit. Only the new field
      -- descriptors are staged; value storage and method bytecode are not copied.
      for I in 1 .. Pending_Count loop
         pragma Loop_Invariant (Valid_Context (Tree));
         pragma Loop_Invariant (Tree.Used = Initial_Count + I - 1);
         Tree.Used := Tree.Used + 1;
         Tree.Items (Tree.Used) :=
           (Up => Node_ID (Scope), Owner => Node_ID (Scope), Part => Pending (I).Name,
            Object_Type => Table_Field_Object,
            Table_Binding => (Region => Binding, Offset => Pending (I).Offset,
                              Bits => Pending (I).Bits), others => <>);
      end loop;
      Status := AML_Execute.Returned;
   end Define_Fields;

   procedure Materialize_Literal
     (Tree : in out State; Kind : AML_Execute.Literal_Kind; Data : AML_Decode.Bytes;
      Binding : out AML_Execute.Binding_Result)
     with Pre => Valid_Context (Tree) and then not Binding'Constrained,
          Post => Valid_Context (Tree)
   is
      use type AML_Execute.Literal_Kind;
      use type AML_Objects.Allocation_Status;
      ID : AML_Objects.Object_ID;
      Status : AML_Objects.Allocation_Status;
      C32, C64 : AML_Coercions.Result;
   begin
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
        Object => (ID => ID, Type_Code => (if Kind = AML_Execute.String_Literal then 2 else 3),
          Size => Data'Length, Conversion_32 => C32, Conversion_64 => C64));
   end Materialize_Literal;

   procedure Invoke_With_Tables
     (Tree : in out State; Input : aliased AML_Table_Backing.State; Node : Node_ID;
      Args : AML_Execute.Arguments; Argument_Count : Natural; Budget : Natural;
      Result : out AML_Execute.Execution_Result)
   is
      use type AML_Decode.Byte;
      use type AML_Decode.Status;
      use type AML_Execute.Binding_Purpose;
      use type AML_Decode.Integer_Width;
      use type AML_Coercions.Conversion_Status;
      use type AML_Objects.Allocation_Status;
      procedure Lookup
        (Tree : in out State; Input : aliased AML_Table_Backing.State; Scope : Natural;
         Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width;
         Purpose : AML_Execute.Binding_Purpose; Binding : out AML_Execute.Binding_Result)
        with Pre => Valid_Context (Tree) and then not Binding'Constrained,
             Post => Valid_Context (Tree)
      is
         Located : Lookup_Result;
         Data : AML_Field_Data.Read_Result;
         ID : AML_Objects.Object_ID;
         Status : AML_Objects.Allocation_Status;
         Converted : AML_Coercions.Result;
      begin
         Binding := Read_Binding (Tree, Scope, Path, Width);
         if Purpose = AML_Execute.Inspect_Binding or else Scope > Tree.Used then return; end if;
         Located := Resolve (Tree, Node_ID (Scope), Path);
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
                  Binding := (Status => AML_Execute.Integer_Binding, Value => 0);
               else
                  Converted := AML_Coercions.From_Buffer (Data.Content (1 .. Data.Length), Width);
                  if Converted.Status = AML_Coercions.Converted then
                     Binding := (Status => AML_Execute.Integer_Binding, Value => Converted.Value);
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
                 Object => (ID => ID, Type_Code => 3, Size => Data.Length,
                   Conversion_32 => AML_Coercions.From_Buffer (Data.Content (1 .. Data.Length), AML_Decode.Bits_32),
                   Conversion_64 => AML_Coercions.From_Buffer (Data.Content (1 .. Data.Length), AML_Decode.Bits_64)));
            end if;
         end;
      end Lookup;
      procedure Execute is new AML_Execute.Execute_With_Input
        (State, AML_Table_Backing.State, Valid_Context, Lookup, Read_Method, Write_Binding,
         Begin_Method, End_Method, Define_Method, Define_Fields, Materialize_Literal);
      Method : constant AML_Execute.Method_Definition := Read_Method (Tree, Node);
   begin
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
      Is_Scope, Is_Device, Is_Method : Boolean;
      Limit : Natural;
      Package_Info : AML_Decode.Package_Result;
      Located : Lookup_Result;
      Code_Start : AML_Execute.Method_Length;
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
            Is_Method := Op = 16#14#;
            if Op = 16#5B# and then Offset < Limit then
               Is_Device := Data (Data'First + Offset) = 16#82#;
               Offset := Offset + 1;
            end if;
            if not Is_Scope and then not Is_Device and then not Is_Method and then Op /= 16#08# then
               Result := Unsupported_Opcode; return;
            end if;
            if Is_Scope or Is_Device or Is_Method then
               if Offset = Limit then Result := Bad_Package; return; end if;
               Package_Info := AML_Decode.Read_Package
                 (Data (Data'First + Offset .. Data'First + (Limit - 1)));
               if Package_Info.Kind /= AML_Decode.Accepted then
                  Result := Bad_Package; return;
               end if;
               Limit := Offset + Package_Info.Extent;
               Offset := Offset + Package_Info.Encoding_Bytes;
            end if;
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
               if Kind (Candidate, Node) not in Scope_Object | Device_Object then
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
                  if Scope = Root or else Kind (Candidate, Scope) not in Scope_Object | Device_Object then
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
            end if;
            if Is_Method then
               if Offset = Limit then Result := Bad_Method; return; end if;
               Candidate.Items (Node).Method_Flags := Data (Data'First + Offset);
               Candidate.Items (Node).Method_Width := Width;
               Offset := Offset + 1;
               if Limit - Offset > AML_Execute.Max_Method_Bytes - Candidate.Code_Used then
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
            elsif Is_Scope or Is_Device then
               if Depth = 64 then Result := Nesting_Limit; return; end if;
               Depth := Depth + 1;
               Frames (Depth) := (Limit => Limit, Scope => Node);
            else
               if Offset = Limit then Result := Bad_Integer; return; end if;
               if Data (Data'First + Offset) in 16#12# | 16#13# then
                  declare
                     Used : Natural;
                     Parsed : AML_Decode.Status;
                     Environment : constant Count_Context := (Candidate, Frames (Depth).Scope);
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
                  if Buffer_Item.Kind /= AML_Decode.Accepted then
                     Result := (if Buffer_Item.Kind = AML_Decode.Limit_Exceeded then
                                  Value_Limit else Bad_Buffer);
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
                  AML_Objects.New_Integer (Candidate.Values, Value.Value, Object_Ref, Allocation);
                  if Allocation /= AML_Objects.Allocated then Result := Value_Limit; return; end if;
                  Candidate.Items (Node).Object_Ref := Object_Ref;
                  Candidate.Items (Node).Object_Type := Integer_Object;
               end if;
            end if;
         end if;
      end loop;
      Tree := Candidate;
   end Load_Names;
end AML_Namespace;
