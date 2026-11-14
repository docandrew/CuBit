with CCL.Objects.Values;
with CCL.VM.Resource_Values;
with CCL.VM.Results;

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
   --  A page holds at most Maximum_Cells cells, and every node, slot,
   --  string and list element takes at least one: a type with a list needs
   --  at most that many of each in a machine that does not grow.
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
      --  However the type nests, one image holds at most Image_Bound of
      --  each: sums of nested lists overstate it.
      return (Nodes => Natural'Min (Needs (Root).Nodes, Image_Bound.Nodes),
              Slots => Natural'Min (Needs (Root).Slots, Image_Bound.Slots),
              Strings => Natural'Min (Needs (Root).Strings, Image_Bound.Strings),
              Elements => Natural'Min (Needs (Root).Elements, Image_Bound.Elements));
   end Need_Of;

   --  Whether the arena and text region can hold any value of type Root.
   function Room_For (Types : CCL.Types.Registry; State : Sized_Machine_State; Root : CCL.Types.Type_Reference)
      return Boolean is
      Need : constant Arena_Need := Need_Of (Types, Root);
   begin
      return Need.Elements <= Natural (State.Element_Room) - List_Regions.Used_Bytes (State.Lists) and then
        (Need.Elements = 0 or else List_Regions.Live_Values (State.Lists) < Natural (State.List_Room)) and then
        Need.Nodes <= Natural (State.Node_Room) - Natural (State.Arena.Nodes_Used) and then
        Need.Slots <= Natural (State.Slot_Room) - Natural (State.Arena.Slots_Used) and then
        Need.Strings <= Natural (State.String_Room) - Text_Regions.Live_Values (State.Text) and then
        (Need.Strings = 0 or else
         CCL.Objects.Maximum_Text_Bytes <= Natural (State.Text_Room) - Text_Regions.Used_Bytes (State.Text));
   end Room_For;

   --  Copy-in: the value at Position of a captured image, as a value of the
   --  local type Ref in State's arena. Components are loaded (and their
   --  nodes allocated) before their owner's node, so nodes point backwards.
   procedure Load
     (Image : CCL.Objects.Views.Sized_Snapshot; Position : CCL.Objects.Views.Cursor;
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
            --  A string longer than one the machine holds is refused
            --  (MAX_STRING_BYTES, CAPACITY).
            declare
               Stored : Text_Regions.Operation_Result;
            begin
               Good := CCL.Objects.Views.Text_Length (Image, Position) <= MAX_STRING_BYTES;
               if Good then
                  Result := (Kind => Text_Value, others => <>);
                  Text_Regions.Allocate_String (Texts, CCL.Objects.Views.Text (Image, Position), Result.Text, Stored);
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
            --  The list's storage in the region, then its elements in order,
            --  each loaded and written in place (nothing of its size on the
            --  stack).
            declare
               Count : constant CCL.Objects.Views.Element_Count := CCL.Objects.Views.Length (Image, Position);
               Element_Type : constant CCL.Types.Type_Reference := CCL.Types.Element_Of (Types, Ref);
               Item : Value;
               At_Element : CCL.Objects.Views.Cursor := CCL.Objects.Views.First_Element (Image, Position);
               Stored : List_Regions.Operation_Result;
            begin
               --  Element types are earlier than their list's (Persistable).
               Good := Element_Type < Ref;
               if Good then
                  Result := (Kind => List_Value, Data_Type => Ref, others => <>);
                  List_Regions.Reserve (Lists, Count, Result.Items, Stored);
                  Good := Stored = List_Regions.Operation_Ok;
               end if;
               if Good and then Element_Type < Ref then
                  for E in 1 .. Count loop
                     Load (Image, At_Element, Types, Element_Type, Arena, Texts, Lists, Item, Good);
                     exit when not Good;
                     List_Regions.Write (Lists, Result.Items, List_Regions.Array_Index (E), To_Element (Item), Stored);
                     Good := Stored = List_Regions.Operation_Ok;
                     exit when not Good;
                     At_Element := CCL.Objects.Views.Next_Element (Image, Position, At_Element);
                  end loop;
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
     (State : Sized_Machine_State; Types : CCL.Types.Registry; Ref : CCL.Types.Type_Reference;
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

   procedure Initialize
     (Item : Validated_Program; Fuel : Natural; State : in out Sized_Machine; Growable : Boolean := False) is
   begin
      CCL.VM.Initialize (Item, Fuel, State.Core);
      State.Initialized := True;
      State.Growable := Growable;
   end Initialize;

   function Snapshot (State : Sized_Machine) return Machine_Snapshot is
     (CCL.VM.Snapshot (State.Core));

   procedure Inspect
     (Item : Validated_Program; State : Sized_Machine;
      Result : out Inspection_Snapshot) is
   begin
      Result := (others => <>);
      Result.Machine := Snapshot (State);
      if State.Initialized and then Is_Well_Formed (Item, State.Core) then
         CCL.VM.Inspect (Item, State.Core, Result);
      end if;
   end Inspect;

   procedure Continue_Execution_For
     (Item : Validated_Program; State : in out Sized_Machine;
      Instructions : Natural; Result : out Execution_Result) is
   begin
      Result := (others => <>);
      if not State.Initialized or else not Is_Well_Formed (Item, State.Core) then
         Result.Status := Invalid_Bytecode; return;
      end if;
      CCL.VM.Continue_Execution_For (Item, State.Core, Instructions, Result);
      -- Admission before the host effect, not only when its reply arrives:
      -- the arena must hold any value of the import's result type.
      if Result.Status = Waiting_For_Host and then not State.Growable and then
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

   function Pending_Call (Item : Validated_Program; State : Sized_Machine)
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

   function Ready_For_Completion (Item : Validated_Program; State : Sized_Machine) return Boolean is
     (State.Initialized and then Is_Well_Formed (Item, State.Core) and then
      State.Core.Waiting and then not State.Core.Terminal and then
      (not State.Core.Waiting_Owned or else
       CCL.Imports.Phase (State.Core.Import_Lifecycle) = CCL.Imports.Import_Accepted));

   function Accepts_Object_Result
     (Item : Validated_Program; State : Sized_Machine; Contract : CCL.Objects.Binding)
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
      (State.Core.Waiting_Result_Kind not in Object_Value | List_Value or else State.Growable or else
       Room_For (Item.Content.Data_Types, State.Core,
                 Item.Content.Imports (State.Core.Waiting_Import).Result_Data_Type)));

   --  What admitting View's value takes in a machine: nodes, slots,
   --  strings and their bytes, list elements and lists (an upper bound:
   --  every record and payload variant a node, every list its elements).
   type Value_Need is record
      Nodes, Slots, Strings, Bytes, Elements, Lists : Natural := 0;
   end record;
   function Need_Of_Value (View : CCL.Objects.Views.Sized_Snapshot) return Value_Need is
      Need : Value_Need;
      At_Cell : CCL.Objects.Views.Cursor;
      D : CCL.Types.Description;
      function Plus (A, B : Natural) return Natural is
        (if A > Natural'Last - B then Natural'Last else A + B);
   begin
      for P in 1 .. CCL.Objects.Views.Cells_Used (View) loop
         At_Cell := CCL.Objects.Views.At_Cell (View, P);
         if CCL.Objects.Views.Type_Of (View, At_Cell) = CCL.Types.String_Type then
            Need.Strings := Plus (Need.Strings, 1);
            Need.Bytes := Plus (Need.Bytes, CCL.Objects.Views.Text_Length (View, At_Cell));
         elsif CCL.Objects.Views.Type_Of (View, At_Cell) in CCL.Types.Declared_Type then
            D := CCL.Objects.Views.Describe (View, At_Cell);
            case D.Form is
               when CCL.Types.Product =>
                  Need.Nodes := Plus (Need.Nodes, 1);
                  Need.Slots := Plus (Need.Slots, D.Count);
               when CCL.Types.Sum =>
                  Need.Nodes := Plus (Need.Nodes, 1);
                  Need.Slots := Plus (Need.Slots, 1);
               when CCL.Types.Sequence =>
                  Need.Lists := Plus (Need.Lists, 1);
                  Need.Elements := Plus (Need.Elements, CCL.Objects.Views.Length (View, At_Cell));
               when others => null;
            end case;
         end if;
      end loop;
      return Need;
   end Need_Of_Value;

   --  Whether State's rooms hold Need beside what they hold.
   function Has_Room (State : Sized_Machine_State; Need : Value_Need) return Boolean is
     (Need.Nodes <= Natural (State.Node_Room) - Natural (State.Arena.Nodes_Used) and then
      Need.Slots <= Natural (State.Slot_Room) - Natural (State.Arena.Slots_Used) and then
      Need.Strings <= Natural (State.String_Room) - Text_Regions.Live_Values (State.Text) and then
      Need.Bytes <= Natural (State.Text_Room) - Text_Regions.Used_Bytes (State.Text) and then
      Need.Elements <= Natural (State.Element_Room) - List_Regions.Used_Bytes (State.Lists) and then
      Need.Lists <= Natural (State.List_Room) - List_Regions.Live_Values (State.Lists));

   procedure Needed_Rooms
     (Item : Validated_Program; State : Sized_Machine; View : CCL.Objects.Views.Sized_Snapshot;
      Wanted : out Rooms; Possible : out Boolean)
   is
      pragma Unreferenced (Item);
      Need : constant Value_Need := Need_Of_Value (View);
      --  Used + Need, doubled for what the run builds next, at least the
      --  room it has, at most Ceiling; Fits False past the ceiling.
      procedure Room
        (Room_Now, Used, Wanted_More, Ceiling : Natural; Result : out Positive; Fits : in out Boolean)
      is
         Total : constant Natural := (if Wanted_More > Ceiling - Natural'Min (Used, Ceiling) then Ceiling + 1
                                      else Used + Wanted_More);
      begin
         if Total > Ceiling then
            Fits := False;
            Result := Ceiling;
         else
            Result := Natural'Max (1, Natural'Max (Room_Now, (if Total > Ceiling / 2 then Ceiling else 2 * Total)));
         end if;
      end Room;
      Nodes, Slots, Text, Strings, Elements, Lists : Positive;
   begin
      Possible := True;
      Room (Natural (State.Node_Room), Natural (State.Core.Arena.Nodes_Used), Need.Nodes, NODE_CEILING,
            Nodes, Possible);
      Room (Natural (State.Slot_Room), Natural (State.Core.Arena.Slots_Used), Need.Slots, SLOT_CEILING,
            Slots, Possible);
      Room (Natural (State.Text_Room), Text_Regions.Used_Bytes (State.Core.Text), Need.Bytes, TEXT_BYTE_CEILING,
            Text, Possible);
      Room (Natural (State.String_Room), Text_Regions.Live_Values (State.Core.Text), Need.Strings,
            TEXT_VALUE_CEILING, Strings, Possible);
      Room (Natural (State.Element_Room), List_Regions.Used_Bytes (State.Core.Lists), Need.Elements,
            LIST_ELEMENT_CEILING, Elements, Possible);
      Room (Natural (State.List_Room), List_Regions.Live_Values (State.Core.Lists), Need.Lists,
            LIST_VALUE_CEILING, Lists, Possible);
      Wanted := (Nodes => Node_Room_Count (Nodes), Slots => Slot_Room_Count (Slots), Text => Text,
                 Strings => Strings, Elements => Elements, Lists => Lists);
   end Needed_Rooms;

   procedure Stop_Exhausted (State : in out Sized_Machine) is
   begin
      if State.Initialized and then State.Core.Waiting and then not State.Core.Terminal and then
        not State.Core.Waiting_Owned
      then
         State.Core.Waiting := False;
         State.Core.Terminal := True;
         State.Core.Terminal_Status := Object_Storage_Exhausted;
      end if;
   end Stop_Exhausted;

   function Can_Hold (From, Into : Sized_Machine) return Boolean is (Holds_All (From.Core, Into.Core));

   procedure Grow (From : Sized_Machine; Into : in out Sized_Machine) is
   begin
      Copy_State (From.Core, Into.Core);
      Into.Initialized := From.Initialized;
      Into.Growable := From.Growable;
   end Grow;

   procedure Complete_Captured
     (Item : Validated_Program; State : in out Sized_Machine;
      Contract : CCL.Objects.Binding; View : CCL.Objects.Views.Sized_Snapshot; Accepted : Boolean;
      Fits : out Boolean)
   is
      Good : Boolean;
      Result : CCL.VM.Value;
   begin
      Fits := True;
      if not Ready_For_Completion (Item, State) then return; end if;
      pragma Assert (Is_Well_Formed (Item, State.Core));
      Good := Accepted and then Accepts_Object_Result (Item, State, Contract) and then
        CCL.Objects.Views.Cells_Used (View) > 0;
      if Good and then State.Core.Waiting_Result_Kind not in Object_Value | List_Value then
         --  A scalar or a scalar variant: a cell or two, unboxed.
         declare
            Page : CCL.Objects.Image;
         begin
            CCL.Objects.Views.Copy_Value (View, CCL.Objects.Views.Root (View), Contract, Page, Good);
            if Good then
               CCL.Objects.Values.To_VM (Contract, Item.Content.Data_Types, Page, Result, Good);
            end if;
         end;
      elsif Good and then not Has_Room (State.Core, Need_Of_Value (View)) then
         Fits := False;
         return;
      elsif Good then
         Load (View, CCL.Objects.Views.Root (View), Item.Content.Data_Types,
               Item.Content.Imports (State.Core.Waiting_Import).Result_Data_Type,
               State.Core.Arena, State.Core.Text, State.Core.Lists, Result, Good);
         --  Loading changed the regions only, not the call's lifecycle.
         pragma Assert (Is_Well_Formed (Item, State.Core));
      end if;
      Complete_Checked_Host_Call (Item, State.Core, Result, Good, True);
   end Complete_Captured;

   procedure Result_Literal
     (Item : Validated_Program; State : Sized_Machine; Output : out String; Printed : out Boolean) is
   begin
      Output := [others => ' '];
      Printed := State.Initialized and then State.Core.Terminal and then
        State.Core.Terminal_Status = Completed;
      if Printed then
         CCL.VM.Results.Print_Literal (Item, State.Core, Output, Printed);
      end if;
   end Result_Literal;

   procedure Complete_Object
     (Item : Validated_Program; State : in out Sized_Machine;
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
            Room : Boolean;
         begin
            CCL.Objects.Views.Capture (Image, Contract, Response, Good);
            if Good then
               Complete_Captured (Item, State, Contract, Image, True, Room);
               if not Room then
                  --  A machine's full rooms end the run, as Room_For would
                  --  have before the effect.
                  State.Core.Waiting := False;
                  State.Core.Terminal := True;
                  State.Core.Terminal_Status := Object_Storage_Exhausted;
               end if;
               CCL.Objects.Views.Clear (Image);
               return;
            end if;
            CCL.Objects.Views.Clear (Image);
         end;
      elsif Good then
         CCL.Objects.Values.To_VM (Contract, Item.Content.Data_Types, Response, Result, Good);
      end if;
      Complete_Checked_Host_Call (Item, State.Core, Result, Good, True);
   end Complete_Object;

   procedure Complete_Text
     (Item : Validated_Program; State : in out Sized_Machine; Response : String; Accepted : Boolean)
   is
      Good : Boolean;
      Result : CCL.VM.Value := (Kind => Text_Value, others => <>);
      Status : Text_Regions.Operation_Result;
   begin
      if not Ready_For_Completion (Item, State) then return; end if;
      Good := Accepted and then State.Core.Waiting_Result_Kind = Text_Value and then
        Within (Response'Length, Item.Content.Imports (State.Core.Waiting_Import).Result_Text_Limit);
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
     (Item : Validated_Program; State : in out Sized_Machine; Reply : CCL.Streams.View_Reply)
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
     (Item : Validated_Program; State : in out Sized_Machine; Response : Value; Accepted : Boolean) is
   begin
      if State.Initialized and then Is_Well_Formed (Item, State.Core) then
         CCL.VM.Complete_Host_Call (Item, State.Core, Response, Accepted);
      end if;
   end Complete_Scalar;

   procedure Complete_Resource
     (Item : Validated_Program; State : in out Sized_Machine;
      Owner : CCL.Resources.Registry; Resource : CCL.Resources.Reference;
      Accepted : out Boolean) is
   begin
      Accepted := False;
      if State.Initialized and then Is_Well_Formed (Item, State.Core) then
         CCL.VM.Resource_Values.Complete (Item, State.Core, Owner, Resource, Accepted);
      end if;
   end Complete_Resource;

   procedure Acknowledge_Host_Submission
     (Item : Validated_Program; State : in out Sized_Machine; Accepted : Boolean) is
   begin
      if State.Initialized and then Is_Well_Formed (Item, State.Core) then
         CCL.VM.Acknowledge_Host_Submission (Item, State.Core, Accepted);
      end if;
   end Acknowledge_Host_Submission;

   procedure Export_Value
     (Item : Validated_Program; State : Sized_Machine; Source : CCL.VM.Value;
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
     (Item : Validated_Program; State : Sized_Machine; Contract : CCL.Objects.Binding;
      Value : out CCL.Objects.Image; Accepted : out Boolean) is
   begin
      Value := CCL.Objects.Empty (Contract); Accepted := False;
      if State.Initialized and then State.Core.Waiting and then not State.Core.Terminal and then
        (not State.Core.Waiting_Owned or else Has_Receiver (Item.Content.Imports (State.Core.Waiting_Import)))
      then Export_Value (Item, State, State.Core.Waiting_Argument, Contract, Value, Accepted); end if;
   end Export_Argument;

   procedure Export_Result
     (Item : Validated_Program; State : Sized_Machine; Contract : CCL.Objects.Binding;
      Value : out CCL.Objects.Image; Accepted : out Boolean) is
   begin
      Value := CCL.Objects.Empty (Contract); Accepted := False;
      if State.Initialized and then State.Core.Terminal and then
        State.Core.Terminal_Status = Completed and then State.Core.Has_Value
      then Export_Value (Item, State, State.Core.Result_Value, Contract, Value, Accepted); end if;
   end Export_Result;

   procedure Stop (State : in out Sized_Machine) is
   begin
      CCL.VM.Stop (State.Core);
      State.Initialized := False;
   end Stop;
end CCL.VM.Native_Objects;
