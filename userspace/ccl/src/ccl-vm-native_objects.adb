with CCL.Objects.Values;
with CCL.Objects.Views;
with CCL.VM.Resource_Values;

package body CCL.VM.Native_Objects with SPARK_Mode is
   use type CCL.Imports.Import_Phase;
   use type CCL.Objects.Build_Result;
   use type Text_Regions.Operation_Result;
   use type List_Regions.Operation_Result;

   --  The most arena a value of the Persistable type Root can take: nodes,
   --  slots and strings. Persistable types refer only to earlier types and
   --  hold no lists, so one pass in declaration order computes it.
   type Arena_Need is record
      Nodes : Natural := 0;
      Slots : Natural := 0;
      Strings : Natural := 0;
      Elements : Natural := 0;
   end record;
   --  An image holds at most Maximum_Cells cells, and every node, slot,
   --  string and list element takes at least one: a type with a list needs
   --  at most that many of each.
   Image_Bound : constant Arena_Need :=
     (CCL.Objects.Maximum_Cells, CCL.Objects.Maximum_Cells, CCL.Objects.Maximum_Cells, CCL.Objects.Maximum_Cells);
   function Need_Of (Types : CCL.Types.Registry; Root : CCL.Types.Type_Reference) return Arena_Need is
      Needs : array (CCL.Types.Type_Reference) of Arena_Need := [others => (others => <>)];
      D : CCL.Types.Description;
      --  Saturating: an implausible need never wraps into a small one.
      function Plus (A, B : Natural) return Natural is
        (if A > Natural'Last - B then Natural'Last else A + B);
   begin
      Needs (CCL.Types.String_Type) := (Strings => 1, others => 0);
      for Ref in CCL.Types.Declared_Type'First .. Root loop
         D := CCL.Types.Describe (Types, Ref);
         if D.Form = CCL.Types.Sequence then
            Needs (Ref) := Image_Bound;
         elsif D.Form = CCL.Types.Product or else
           (D.Form = CCL.Types.Sum and then not CCL.Types.Is_Scalar_Sum (Types, Ref))
         then
            Needs (Ref) := (Nodes => 1, Slots => (if D.Form = CCL.Types.Product then D.Count else 1),
                            Strings => 0, Elements => 0);
            for P in 1 .. D.Count loop
               if D.Parts (P).Payload < Ref then
                  declare
                     Part : constant Arena_Need := Needs (D.Parts (P).Payload);
                  begin
                     if D.Form = CCL.Types.Product then
                        Needs (Ref) := (Plus (Needs (Ref).Nodes, Part.Nodes), Plus (Needs (Ref).Slots, Part.Slots),
                                        Plus (Needs (Ref).Strings, Part.Strings),
                                        Plus (Needs (Ref).Elements, Part.Elements));
                     else
                        Needs (Ref) := (Natural'Max (Needs (Ref).Nodes, Plus (1, Part.Nodes)),
                                        Natural'Max (Needs (Ref).Slots, Plus (1, Part.Slots)),
                                        Natural'Max (Needs (Ref).Strings, Part.Strings),
                                        Natural'Max (Needs (Ref).Elements, Part.Elements));
                     end if;
                  end;
               end if;
            end loop;
         end if;
      end loop;
      return Needs (Root);
   end Need_Of;

   --  Whether the arena and text region can hold any value of type Root.
   function Room_For (Types : CCL.Types.Registry; State : Machine_State; Root : CCL.Types.Type_Reference)
      return Boolean is
      Need : constant Arena_Need := Need_Of (Types, Root);
   begin
      return Need.Elements <= MAX_LIST_ELEMENTS - List_Regions.Used_Bytes (State.Lists) and then
        (Need.Elements = 0 or else List_Regions.Live_Values (State.Lists) < MAX_LIST_VALUES) and then
        Need.Nodes <= MAX_VALUE_NODES - State.Arena.Nodes_Used and then
        Need.Slots <= MAX_VALUE_SLOTS - State.Arena.Slots_Used and then
        Need.Strings <= MAX_TEXT_VALUES - Text_Regions.Live_Values (State.Text) and then
        (Need.Strings = 0 or else
         CCL.Objects.Maximum_Text_Bytes <= MAX_TEXT_BYTES - Text_Regions.Used_Bytes (State.Text));
   end Room_For;

   --  Copy-in: the value at Position of a captured image, as a value of the
   --  local type Ref in State's arena. Components are loaded (and their
   --  nodes allocated) before their owner's node, so nodes point backwards.
   procedure Load
     (Image : CCL.Objects.Views.Snapshot; Position : CCL.Objects.Views.Cursor;
      Types : CCL.Types.Registry; Ref : CCL.Types.Type_Reference;
      Arena : in out Value_Arena; Texts : in out Text_Regions.Stack; Lists : in out List_Regions.Stack;
      Result : out Value; Good : out Boolean)
     with Subprogram_Variant => (Decreases => Ref)
   is
      D : constant CCL.Types.Description := CCL.Types.Describe (Types, Ref);
      Cell : CCL.Objects.Cell;
      Choice : CCL.Types.Component_Count;
      Parts : Component_Values := [others => (others => <>)];
      Count : CCL.Types.Component_Count := 0;
   begin
      Result := (others => <>);
      Good := CCL.Objects.Views.Local_Type (Image, Position, Types) = Ref;
      if not Good then return; end if;
      case Kind_For_Type (Types, Ref) is
         when Integer_Value =>
            Result := Integer_Constant (CCL.Objects.Integer_Of (CCL.Objects.Views.Scalar (Image, Position)));
         when Boolean_Value =>
            Result := Boolean_Constant (CCL.Objects.Views.Scalar (Image, Position).First = 1);
         when Character_Value =>
            Cell := CCL.Objects.Views.Scalar (Image, Position);
            Good := Cell.First <= MAX_CHARACTER_CODE;
            if Good then Result := Character_Constant (Character'Val (Cell.First)); end if;
         when Text_Value =>
            declare
               Length : constant CCL.Objects.Views.Text_Size := CCL.Objects.Views.Text_Length (Image, Position);
               Buffer : String (1 .. CCL.Objects.Maximum_Text_Bytes) := [others => ' '];
               Stored : Text_Regions.Operation_Result;
            begin
               CCL.Objects.Views.Copy_Text (Image, Position, Buffer (1 .. Length), Good);
               if Good then
                  Result := (Kind => Text_Value, others => <>);
                  Text_Regions.Allocate_String (Texts, Buffer (1 .. Length), Result.Text, Stored);
                  Good := Stored = Text_Regions.Operation_Ok;
               end if;
            end;
         when Variant_Value =>
            Choice := CCL.Objects.Views.Alternative (Image, Position);
            Good := Choice in 1 .. D.Count;
            if Good then
               Result := (Kind => Variant_Value, Data_Type => Ref, Alternative => Choice, others => <>);
               Cell := CCL.Objects.Views.Scalar (Image, CCL.Objects.Views.Payload (Image, Position));
               case D.Parts (Choice).Payload is
                  when CCL.Types.Integer_Type => Result.Integer := CCL.Objects.Integer_Of (Cell);
                  when CCL.Types.Boolean_Type => Result.Boolean := Cell.First = 1;
                  when CCL.Types.Unit_Type => null;
                  when others => Good := False;
               end case;
            end if;
         when Object_Value =>
            if D.Form = CCL.Types.Product then
               Count := D.Count;
               Choice := 0;
               for P in 1 .. Count loop
                  exit when not Good;
                  if D.Parts (P).Payload < Ref then
                     Load (Image, CCL.Objects.Views.Field (Image, Position, P), Types, D.Parts (P).Payload,
                           Arena, Texts, Lists, Parts (P), Good);
                  else
                     Good := False;
                  end if;
               end loop;
            else
               Choice := CCL.Objects.Views.Alternative (Image, Position);
               Good := Choice in 1 .. D.Count;
               if Good and then D.Parts (Choice).Payload /= CCL.Types.Unit_Type then
                  Count := 1;
                  if D.Parts (Choice).Payload < Ref then
                     Load (Image, CCL.Objects.Views.Payload (Image, Position), Types, D.Parts (Choice).Payload,
                           Arena, Texts, Lists, Parts (1), Good);
                  else
                     Good := False;
                  end if;
               end if;
            end if;
            if Good then
               Allocate_Node (Arena, Types, Ref, Choice, Parts, Count, Result, Good);
            end if;
         when List_Value =>
            --  The elements in order (each loaded before the list exists),
            --  then one list value in the region.
            declare
               Count : constant CCL.Objects.Views.Element_Count := CCL.Objects.Views.Length (Image, Position);
               Items : List_Element_Array (1 .. CCL.Objects.Maximum_Cells) := [others => Null_List_Element];
               Element_Type : constant CCL.Types.Type_Reference := CCL.Types.Element_Of (Types, Ref);
               Item : Value;
               Stored : List_Regions.Operation_Result;
            begin
               --  Element types are earlier than their list's (Persistable).
               Good := Element_Type < Ref;
               if Element_Type < Ref then
                  for E in 1 .. Count loop
                     Load (Image, CCL.Objects.Views.Element (Image, Position, E), Types, Element_Type,
                           Arena, Texts, Lists, Item, Good);
                     exit when not Good;
                     Items (E) := To_Element (Item);
                  end loop;
               end if;
               if Good then
                  Result := (Kind => List_Value, Data_Type => Ref, others => <>);
                  List_Regions.Allocate (Lists, Items (1 .. Count), Result.Items, Stored);
                  Good := Stored = List_Regions.Operation_Ok;
               end if;
            end;
         when Resource_Value | Function_Value => Good := False;
      end case;
      if not Good then
         Result := (others => <>);
      end if;
   end Load;

   --  Copy-out: Item, a value of the local type Ref, appended to an image in
   --  the native layout: a product's field count or a variant's member, then
   --  its components depth first.
   procedure Store
     (State : Machine_State; Types : CCL.Types.Registry; Ref : CCL.Types.Type_Reference;
      Item : Value; Image : in out CCL.Objects.Image; Good : in out Boolean)
     with Subprogram_Variant => (Decreases => Ref)
   is
      D : constant CCL.Types.Description := CCL.Types.Describe (Types, Ref);
      Built : CCL.Objects.Build_Result := CCL.Objects.Added;
      Part : Value;
   begin
      if not Good then return; end if;
      case Kind_For_Type (Types, Ref) is
         when Integer_Value => CCL.Objects.Append (Image, CCL.Objects.Integer_Cell (Item.Integer), Built);
         when Boolean_Value => CCL.Objects.Append (Image, CCL.Objects.Boolean_Cell (Item.Boolean), Built);
         when Character_Value =>
            if Item.Integer in 0 .. MAX_CHARACTER_CODE then
               CCL.Objects.Append (Image, CCL.Objects.Character_Cell (Character'Val (Item.Integer)), Built);
            else
               Good := False;
            end if;
         when Text_Value =>
            declare
               Length : constant Natural := Text_Regions.Length (Item.Text);
               Buffer : String (1 .. CCL.Objects.Maximum_Text_Bytes) := [others => ' '];
               Copied : Text_Regions.Operation_Result;
            begin
               if Length > CCL.Objects.Maximum_Text_Bytes then
                  Good := False;
               else
                  Text_Regions.Copy_To (State.Text, Item.Text, Buffer (1 .. Length), Copied);
                  Good := Copied = Text_Regions.Operation_Ok;
                  if Good then CCL.Objects.Append_Text (Image, Buffer (1 .. Length), Built); end if;
               end if;
            end;
         when Variant_Value =>
            if Item.Alternative > D.Count then
               Good := False;
            else
               CCL.Objects.Append (Image, CCL.Objects.Variant_Cell (Item.Alternative), Built);
               if Built = CCL.Objects.Added then
                  case D.Parts (Item.Alternative).Payload is
                     when CCL.Types.Integer_Type => CCL.Objects.Append (Image, CCL.Objects.Integer_Cell (Item.Integer), Built);
                     when CCL.Types.Boolean_Type => CCL.Objects.Append (Image, CCL.Objects.Boolean_Cell (Item.Boolean), Built);
                     when CCL.Types.Unit_Type => CCL.Objects.Append (Image, CCL.Objects.Unit_Cell, Built);
                     when others => Good := False;
                  end case;
               end if;
            end if;
         when Object_Value =>
            if D.Form = CCL.Types.Product then
               CCL.Objects.Append (Image, CCL.Objects.Product_Cell (D.Count), Built);
               for P in 1 .. D.Count loop
                  exit when not Good or else Built /= CCL.Objects.Added;
                  Component (State.Arena, Types, Item, P, Part, Good);
                  if Good and then D.Parts (P).Payload < Ref then
                     Store (State, Types, D.Parts (P).Payload, Part, Image, Good);
                  else
                     Good := False;
                  end if;
               end loop;
            elsif Item.Alternative > D.Count then
               Good := False;
            else
               CCL.Objects.Append (Image, CCL.Objects.Variant_Cell (Item.Alternative), Built);
               if Built = CCL.Objects.Added then
                  if D.Parts (Item.Alternative).Payload = CCL.Types.Unit_Type then
                     CCL.Objects.Append (Image, CCL.Objects.Unit_Cell, Built);
                  else
                     Component (State.Arena, Types, Item, 1, Part, Good);
                     if Good and then D.Parts (Item.Alternative).Payload < Ref then
                        Store (State, Types, D.Parts (Item.Alternative).Payload, Part, Image, Good);
                     else
                        Good := False;
                     end if;
                  end if;
               end if;
            end if;
         when List_Value =>
            declare
               Element_Type : constant CCL.Types.Type_Reference := CCL.Types.Element_Of (Types, Ref);
               Count : constant Natural := List_Regions.Length (Item.Items);
               Element : List_Element;
               Read : List_Regions.Operation_Result;
            begin
               if Element_Type >= Ref or else Count > CCL.Objects.Maximum_Cells then
                  Good := False;
               else
                  CCL.Objects.Append (Image, CCL.Objects.Sequence_Cell (Count), Built);
                  for E in 1 .. Count loop
                     exit when not Good or else Built /= CCL.Objects.Added;
                     List_Regions.Read (State.Lists, Item.Items, List_Regions.Array_Index (E), Element, Read);
                     Good := Read = List_Regions.Operation_Ok;
                     if Good then
                        From_Element (State.Arena, Types, Ref, Element, Part, Good);
                     end if;
                     if Good then
                        Store (State, Types, Element_Type, Part, Image, Good);
                     end if;
                  end loop;
               end if;
            end;
         when Resource_Value | Function_Value => Good := False;
      end case;
      Good := Good and then Built = CCL.Objects.Added;
   end Store;

   procedure Initialize (Item : Validated_Program; Fuel : Natural; State : in out Machine) is
   begin
      CCL.VM.Initialize (Item, Fuel, State.Core);
      State.Initialized := True;
   end Initialize;

   function Snapshot (State : Machine) return Machine_Snapshot is
     (CCL.VM.Snapshot (State.Core));

   procedure Inspect
     (Item : Validated_Program; State : Machine;
      Result : out Inspection_Snapshot) is
   begin
      Result := (others => <>);
      Result.Machine := Snapshot (State);
      if State.Initialized and then Is_Well_Formed (Item, State.Core) then
         CCL.VM.Inspect (Item, State.Core, Result);
      end if;
   end Inspect;

   procedure Continue_Execution_For
     (Item : Validated_Program; State : in out Machine;
      Instructions : Natural; Result : out Execution_Result) is
   begin
      Result := (others => <>);
      if not State.Initialized or else not Is_Well_Formed (Item, State.Core) then
         Result.Status := Invalid_Bytecode; return;
      end if;
      CCL.VM.Continue_Execution_For (Item, State.Core, Instructions, Result);
      -- Admission before the host effect, not only when its reply arrives:
      -- the arena must hold any value of the import's result type.
      if Result.Status = Waiting_For_Host and then
        State.Core.Waiting_Result_Kind in Object_Value | List_Value and then
        not Room_For (Item.Content.Data_Types, State.Core,
                      Item.Content.Imports (State.Core.Waiting_Import).Result_Data_Type)
      then
         if State.Core.Waiting_Owned then
            -- No effect was submitted: return the offered local unchanged
            -- before making exhaustion terminal. Do not strand the lifecycle
            -- in Offered while clearing its Waiting flag.
            CCL.VM.Acknowledge_Host_Submission (Item, State.Core, False);
         end if;
         State.Core.Waiting := False;
         State.Core.Terminal := True;
         State.Core.Terminal_Status := Object_Storage_Exhausted;
         Result := (Status => Object_Storage_Exhausted,
           Fuel_Remaining => Result.Fuel_Remaining, Steps => Result.Steps, others => <>);
      end if;
   end Continue_Execution_For;

   function Pending_Call (Item : Validated_Program; State : Machine)
     return Execution_Result is
      Position : constant Machine_Snapshot := Snapshot (State.Core);
   begin
      if not State.Initialized or else not Is_Well_Formed (Item, State.Core) or else
        not State.Core.Waiting or else State.Core.Terminal
      then return (others => <>); end if;
      return (Status => Waiting_For_Host,
        Fuel_Remaining => Position.Fuel_Remaining, Steps => Position.Steps,
        Requested_Import => State.Core.Waiting_Import,
        Request_Argument => State.Core.Waiting_Argument,
        Request_Receiver => State.Core.Waiting_Receiver,
        Request_Owned => State.Core.Waiting_Owned,
        Requested_Authority => Item.Content.Imports (State.Core.Waiting_Import).Authority,
        Requested_Binding => Item.Content.Imports (State.Core.Waiting_Import).Binding,
        others => <>);
   end Pending_Call;

   function Ready_For_Completion (Item : Validated_Program; State : Machine) return Boolean is
     (State.Initialized and then Is_Well_Formed (Item, State.Core) and then
      State.Core.Waiting and then not State.Core.Terminal and then
      (not State.Core.Waiting_Owned or else
       CCL.Imports.Phase (State.Core.Import_Lifecycle) = CCL.Imports.Import_Accepted));

   function Accepts_Object_Result
     (Item : Validated_Program; State : Machine; Contract : CCL.Objects.Binding)
      return Boolean is
     (Pending_Call (Item, State).Status = Waiting_For_Host and then
      CCL.Objects.Matches_Type (Contract, Item.Content.Data_Types,
        (case State.Core.Waiting_Result_Kind is
          when Integer_Value => CCL.Types.Integer_Type,
          when Boolean_Value => CCL.Types.Boolean_Type,
          when Variant_Value | Object_Value | List_Value =>
            Item.Content.Imports (State.Core.Waiting_Import).Result_Data_Type,
          when Resource_Value | Text_Value | Character_Value | Function_Value =>
            CCL.Types.Invalid_Type)) and then
      (State.Core.Waiting_Result_Kind not in Object_Value | List_Value or else
       Room_For (Item.Content.Data_Types, State.Core,
                 Item.Content.Imports (State.Core.Waiting_Import).Result_Data_Type)));

   procedure Complete_Object
     (Item : Validated_Program; State : in out Machine;
      Contract : CCL.Objects.Binding; Response : CCL.Objects.Image; Accepted : Boolean)
   is
      Good : Boolean;
      Result : CCL.VM.Value;
   begin
      if not Ready_For_Completion (Item, State) then return; end if;
      Good := Accepted and then Accepts_Object_Result (Item, State, Contract);
      if Good and then State.Core.Waiting_Result_Kind in Object_Value | List_Value then
         declare
            Image : CCL.Objects.Views.Snapshot;
         begin
            CCL.Objects.Views.Capture (Image, Contract, Response, Good);
            if Good then
               Load (Image, CCL.Objects.Views.Root (Image), Item.Content.Data_Types,
                     Item.Content.Imports (State.Core.Waiting_Import).Result_Data_Type,
                     State.Core.Arena, State.Core.Text, State.Core.Lists, Result, Good);
            end if;
            CCL.Objects.Views.Clear (Image);
         end;
      elsif Good then
         CCL.Objects.Values.To_VM (Contract, Item.Content.Data_Types, Response, Result, Good);
      end if;
      Complete_Checked_Host_Call (Item, State.Core, Result, Good, True);
   end Complete_Object;

   procedure Complete_Text
     (Item : Validated_Program; State : in out Machine; Response : String; Accepted : Boolean)
   is
      Good : Boolean;
      Result : CCL.VM.Value := (Kind => Text_Value, others => <>);
      Status : Text_Regions.Operation_Result;
   begin
      if not Ready_For_Completion (Item, State) then return; end if;
      Good := Accepted and then State.Core.Waiting_Result_Kind = Text_Value and then
        Response'Length <= Item.Content.Imports (State.Core.Waiting_Import).Result_Text_Limit;
      if Good then
         Text_Regions.Allocate_String (State.Core.Text, Response, Result.Text, Status);
         if Status /= Text_Regions.Operation_Ok then
            State.Core.Waiting := False;
            State.Core.Terminal := True;
            State.Core.Terminal_Status := Text_Storage_Exhausted;
            return;
         end if;
      end if;
      Complete_Checked_Host_Call (Item, State.Core, Result, Good, False);
   end Complete_Text;

   procedure Complete_Stream_View
     (Item : Validated_Program; State : in out Machine; Reply : CCL.Streams.View_Reply)
   is
      Good : Boolean := False;
      Result : CCL.VM.Value := (others => <>);
   begin
      if not State.Initialized or else not Is_Well_Formed (Item, State.Core) or else
        not State.Core.Waiting_Stream
      then
         return;
      end if;
      case Reply.Status is
         when CCL.Streams.No_Such_Stream =>
            Complete_Stream_Call (Item, State.Core, Result, Stream_Unavailable); return;
         when CCL.Streams.Stream_Empty =>
            Complete_Stream_Call (Item, State.Core, Result, Stream_Empty); return;
         when CCL.Streams.View_Answered => null;
      end case;
      if not CCL.Streams.Returns_Elements (State.Core.Stream_Request.View) then
         Result := Integer_Constant (Reply.Total);
         Good := True;
      else
         declare
            Image : CCL.Objects.Views.Snapshot;
         begin
            CCL.Objects.Views.Capture_Local
              (Image, Item.Content.Data_Types, State.Core.Stream_Result_Type, Reply.Elements, Good);
            if Good then
               Load (Image, CCL.Objects.Views.Root (Image), Item.Content.Data_Types,
                     State.Core.Stream_Result_Type,
                     State.Core.Arena, State.Core.Text, State.Core.Lists, Result, Good);
            end if;
            CCL.Objects.Views.Clear (Image);
         end;
      end if;
      Complete_Stream_Call
        (Item, State.Core, Result, (if Good then Completed else Stream_Element_Mismatch));
   end Complete_Stream_View;

   procedure Complete_Scalar
     (Item : Validated_Program; State : in out Machine; Response : Value; Accepted : Boolean) is
   begin
      if State.Initialized and then Is_Well_Formed (Item, State.Core) then
         CCL.VM.Complete_Host_Call (Item, State.Core, Response, Accepted);
      end if;
   end Complete_Scalar;

   procedure Complete_Resource
     (Item : Validated_Program; State : in out Machine;
      Owner : CCL.Resources.Registry; Resource : CCL.Resources.Reference;
      Accepted : out Boolean) is
   begin
      Accepted := False;
      if State.Initialized and then Is_Well_Formed (Item, State.Core) then
         CCL.VM.Resource_Values.Complete (Item, State.Core, Owner, Resource, Accepted);
      end if;
   end Complete_Resource;

   procedure Acknowledge_Host_Submission
     (Item : Validated_Program; State : in out Machine; Accepted : Boolean) is
   begin
      if State.Initialized and then Is_Well_Formed (Item, State.Core) then
         CCL.VM.Acknowledge_Host_Submission (Item, State.Core, Accepted);
      end if;
   end Acknowledge_Host_Submission;

   procedure Export_Value
     (Item : Validated_Program; State : Machine; Source : CCL.VM.Value;
      Contract : CCL.Objects.Binding; Value : out CCL.Objects.Image; Accepted : out Boolean) is
   begin
      Value := CCL.Objects.Empty (Contract); Accepted := False;
      if not Source.Copyable or else Source.Type_Tag /= 0 then return; end if;
      if Source.Kind in Object_Value | Text_Value | Character_Value | List_Value then
         --  Arena values (a record, or a String or Character such as a
         --  projected field) are walked into the image.
         declare
            Ref : constant CCL.Types.Type_Reference :=
              (case Source.Kind is
                  when Text_Value => CCL.Types.String_Type,
                  when Character_Value => CCL.Types.Character_Type,
                  when others => Source.Data_Type);
         begin
            if not CCL.Objects.Matches_Type (Contract, Item.Content.Data_Types, Ref) then
               return;
            end if;
            Accepted := True;
            Store (State.Core, Item.Content.Data_Types, Ref, Source, Value, Accepted);
         end;
         Accepted := Accepted and then CCL.Objects.Validate (Value, Contract);
         if not Accepted then
            Value := CCL.Objects.Empty (Contract);
         end if;
      else
         CCL.Objects.Values.From_VM (Contract, Item.Content.Data_Types, Source, Value, Accepted);
      end if;
   end Export_Value;

   procedure Export_Argument
     (Item : Validated_Program; State : Machine; Contract : CCL.Objects.Binding;
      Value : out CCL.Objects.Image; Accepted : out Boolean) is
   begin
      Value := CCL.Objects.Empty (Contract); Accepted := False;
      if State.Initialized and then State.Core.Waiting and then not State.Core.Terminal and then
        (not State.Core.Waiting_Owned or else Has_Receiver (Item.Content.Imports (State.Core.Waiting_Import)))
      then Export_Value (Item, State, State.Core.Waiting_Argument, Contract, Value, Accepted); end if;
   end Export_Argument;

   procedure Export_Result
     (Item : Validated_Program; State : Machine; Contract : CCL.Objects.Binding;
      Value : out CCL.Objects.Image; Accepted : out Boolean) is
   begin
      Value := CCL.Objects.Empty (Contract); Accepted := False;
      if State.Initialized and then State.Core.Terminal and then
        State.Core.Terminal_Status = Completed and then State.Core.Has_Value
      then Export_Value (Item, State, State.Core.Result_Value, Contract, Value, Accepted); end if;
   end Export_Result;

   procedure Stop (State : in out Machine) is
   begin
      CCL.VM.Stop (State.Core);
      State.Initialized := False;
   end Stop;
end CCL.VM.Native_Objects;
