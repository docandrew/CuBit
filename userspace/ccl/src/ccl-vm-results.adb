with CCL.Text_Operations;

package body CCL.VM.Results with SPARK_Mode => On is
   use type List_Regions.Operation_Result;
   use type Text_Regions.Operation_Result;

   --  Copy a list result out, as the interpreter does: the first elements
   --  as values, strings as consecutive slices of List_Text.
   procedure Export_List
     (Lists : List_Regions.Stack; Texts : Text_Regions.Stack; Arena : Value_Arena;
      Types : CCL.Types.Registry; Item : Value; Result : in out Execution_Result)
   is
      Total : constant List_Element_Count := Element_Count (Item.Items);
      Element : List_Element;
      Element_Value : Value;
      Read_Status : List_Regions.Operation_Result;
      Copied : Text_Regions.Operation_Result;
      Used : Result_Text_Length := 0;
      Size : Natural;
      Good : Boolean;
   begin
      Result.List_Total := Total;
      Result.List_Length := Carried (Total);
      for I in 1 .. Result.List_Length loop
         List_Regions.Read (Lists, Item.Items, List_Regions.Array_Index (I), Element, Read_Status);
         From_Element (Arena, Types, Item.Data_Type, Element, Element_Value, Good);
         if Read_Status /= List_Regions.Operation_Ok or else not Good then
            Result.List_Length := I - 1; exit;
         end if;
         if Element_Value.Kind = Text_Value then
            Size := Text_Regions.Length (Element.Text);
            if Size > MAX_RESULT_TEXT - Natural (Used) then
               --  Carry out the elements that fit.
               Result.List_Length := I - 1; exit;
            end if;
            Text_Regions.Copy_To
              (Texts, Element.Text, Result.List_Text.Data (Natural (Used) + 1 .. Natural (Used) + Size), Copied);
            if Copied /= Text_Regions.Operation_Ok then
               Result.List_Length := I - 1; exit;
            end if;
            Used := Used + Result_Text_Length (Size);
            Result.List_Text_Ends (I) := Used;
         elsif Element_Value.Kind = Character_Value then
            Result.List_Values (I) := Integer_Constant (Element.Integer);
         elsif Element_Value.Kind = Variant_Value then
            Result.List_Values (I) := Integer_Constant (Integer_64 (Element.Alternative));
         else
            Result.List_Values (I) := Element_Value;
         end if;
      end loop;
      Result.List_Text.Length := Used;
   end Export_List;

   --  A result's canonical CCL literal, as the interpreter prints it (it
   --  reads back as the same value): (Pair 42 "hi"), Shape.Empty,
   --  (Shape.Circle 3), [1 2], (list-of T). Characters have no literal.
   --  A node's components are older nodes, so Bound (the node being
   --  printed) decreases; a list's elements print at Level 0, which has no
   --  lists of its own.
   type Print_Bound is range 0 .. MAX_VALUE_NODES + 1;
   subtype Print_Level is Natural range 0 .. 1;
   --  Printing a node's components: they are older nodes, below it.
   function Below (Node : Node_Count) return Print_Bound is (Print_Bound (Node));
   procedure Print_Value
     (Arena : Value_Arena; Texts : Text_Regions.Stack; Lists : List_Regions.Stack;
      Types : CCL.Types.Registry; Item : Value; Bound : Print_Bound; Level : Print_Level;
      Output : in out Literal_Text; Good : in out Boolean)
     with Subprogram_Variant => (Decreases => Bound, Decreases => Level)
   is
      procedure Add (Text : String) is
      begin
         if Good and then Text'Length <= MAX_LITERAL_TEXT - Length_Of (Output) then
            Output.Data (Length_Of (Output) + 1 .. Length_Of (Output) + Text'Length) := Text;
            Output.Length := Output.Length + Literal_Length (Text'Length);
         else
            Good := False;
         end if;
      end Add;
      function Name_Of (Ref : CCL.Types.Type_Reference) return String is
        (CCL.Types.Image (CCL.Types.Describe (Types, Ref).Identifier));
      D : constant CCL.Types.Description := CCL.Types.Describe (Types, Item.Data_Type);
      Part : Value;
      C : Character;
      Read : Text_Regions.Operation_Result;
      Element : List_Element;
      Listed : List_Regions.Operation_Result;
   begin
      if not Good then return; end if;
      if Below (Item.Node) >= Bound then
         Good := False;
         return;
      end if;
      case Item.Kind is
         when Integer_Value => Add (CCL.Text_Operations.Decimal_Image (Item.Integer));
         when Boolean_Value => Add ((if Item.Boolean then "true" else "false"));
         when Text_Value =>
            Add ("""");
            for I in 1 .. Text_Regions.Length (Item.Text) loop
               exit when not Good;
               Text_Regions.Read (Texts, Item.Text, Text_Regions.String_Index (I), C, Read);
               Good := Read = Text_Regions.Operation_Ok;
               exit when not Good;
               case C is
                  when '"' => Add ("\""");
                  when '\' => Add ("\\");
                  when ASCII.LF => Add ("\n");
                  when ASCII.CR => Add ("\r");
                  when ASCII.HT => Add ("\t");
                  when ' ' .. '!' | '#' .. '[' | ']' .. '~' => Add ([1 => C]);
                  when others => Good := False;
               end case;
            end loop;
            Add ("""");
         when List_Value =>
            if Level = 0 then
               Good := False;
            elsif List_Regions.Length (Item.Items) = 0 then
               Add ("(list-of " & Name_Of (CCL.Types.Element_Of (Types, Item.Data_Type)) & ")");
            else
               Add ("[");
               for I in 1 .. List_Regions.Length (Item.Items) loop
                  exit when not Good;
                  List_Regions.Read (Lists, Item.Items, List_Regions.Array_Index (I), Element, Listed);
                  Good := Good and then Listed = List_Regions.Operation_Ok;
                  if Good then
                     From_Element (Arena, Types, Item.Data_Type, Element, Part, Good);
                  end if;
                  if I > 1 then Add (" "); end if;
                  if Good then
                     Print_Value (Arena, Texts, Lists, Types, Part, Bound, 0, Output, Good);
                  end if;
               end loop;
               Add ("]");
            end if;
         when Variant_Value | Object_Value =>
            if D.Form = CCL.Types.Product and then Item.Node /= 0 then
               Add ("(" & Name_Of (Item.Data_Type));
               for P in 1 .. D.Count loop
                  exit when not Good;
                  Component (Arena, Types, Item, P, Part, Good);
                  exit when not Good;
                  Add (" ");
                  Print_Value (Arena, Texts, Lists, Types, Part, Below (Item.Node), 1, Output, Good);
               end loop;
               Add (")");
            elsif D.Form = CCL.Types.Sum and then Item.Alternative <= D.Count then
               if D.Parts (Item.Alternative).Payload = CCL.Types.Unit_Type then
                  Add (Name_Of (Item.Data_Type) & "." & CCL.Types.Image (D.Parts (Item.Alternative).Identifier));
               else
                  Add ("(" & Name_Of (Item.Data_Type) & "." &
                       CCL.Types.Image (D.Parts (Item.Alternative).Identifier) & " ");
                  if Item.Node /= 0 then
                     Component (Arena, Types, Item, 1, Part, Good);
                     if Good then
                        Print_Value (Arena, Texts, Lists, Types, Part, Below (Item.Node), 1, Output, Good);
                     end if;
                  else
                     --  A scalar payload carried inline: no node to decrease.
                     case D.Parts (Item.Alternative).Payload is
                        when CCL.Types.Integer_Type => Add (CCL.Text_Operations.Decimal_Image (Item.Integer));
                        when CCL.Types.Boolean_Type => Add ((if Item.Boolean then "true" else "false"));
                        when others => Good := False;
                     end case;
                  end if;
                  Add (")");
               end if;
            else
               Good := False;
            end if;
         when Character_Value | Resource_Value | Function_Value => Good := False;
      end case;
   end Print_Value;

   --  Whether a list's elements are records or payload variants: such a
   --  list leaves as a literal, as the interpreter's does.
   function Compound_Elements (Types : CCL.Types.Registry; List_Type : CCL.Types.Type_Reference) return Boolean is
     (CCL.Types.Describe (Types, CCL.Types.Element_Of (Types, List_Type)).Form = CCL.Types.Product or else
      (CCL.Types.Describe (Types, CCL.Types.Element_Of (Types, List_Type)).Form = CCL.Types.Sum and then
       not CCL.Types.Is_Enumeration (Types, CCL.Types.Element_Of (Types, List_Type))));

   function Import_Text_Of (Region : Text_Regions.Stack; Item : Value) return Import_Text is
      Text : Import_Text;
      Status : Text_Regions.Operation_Result;
   begin
      if Item.Kind = Text_Value and then Text_Regions.Is_Valid (Region, Item.Text) then
         Text.Length := Import_Text_Length (Text_Regions.Length (Item.Text));
         Text_Regions.Copy_To (Region, Item.Text, Text.Data (1 .. Length_Of (Text)), Status);
         if Status /= Text_Regions.Operation_Ok then
            Text := (others => <>);
         end if;
      end if;
      return Text;
   end Import_Text_Of;

   function Result_Text_Of (Region : Text_Regions.Stack; Item : Value) return Result_Text is
      Text : Result_Text;
      Status : Text_Regions.Operation_Result;
   begin
      if Item.Kind = Text_Value and then Text_Regions.Is_Valid (Region, Item.Text) and then
        Text_Regions.Length (Item.Text) <= MAX_RESULT_TEXT
      then
         Text.Length := Result_Text_Length (Text_Regions.Length (Item.Text));
         Text_Regions.Copy_To (Region, Item.Text, Text.Data (1 .. Length_Of (Text)), Status);
         if Status /= Text_Regions.Operation_Ok then
            Text := (others => <>);
         end if;
      end if;
      return Text;
   end Result_Text_Of;

   procedure Report
     (Item : Validated_Program; State : Machine_State; Status : Execution_Status;
      Waiting, Waiting_Owned : Boolean; Result : out Execution_Result) is
   begin
      Result :=
        (Status         => Status,
         Has_Value      => State.Has_Value,
         Result_Value   => State.Result_Value,
         Fuel_Remaining =>
           CCL.Execution_Budgets.Remaining (State.Execution_Budget),
         Steps          => CCL.Execution_Budgets.Steps (State.Execution_Budget),
         Requested_Import => State.Waiting_Import,
         Request_Argument => State.Waiting_Argument,
         Has_Request_Text => Waiting and then State.Waiting_Argument.Kind = Text_Value and then
           Text_Regions.Is_Valid (State.Text, State.Waiting_Argument.Text),
         Request_Text => Import_Text_Of (State.Text, State.Waiting_Argument),
         Result_Text_Value => Result_Text_Of (State.Text, State.Result_Value),
         Has_Result_Text => State.Has_Value and then State.Result_Value.Kind = Text_Value and then
           Text_Regions.Is_Valid (State.Text, State.Result_Value.Text) and then
           Text_Regions.Length (State.Result_Value.Text) <= MAX_RESULT_TEXT,
         Request_Receiver => State.Waiting_Receiver,
         Request_Owned => Waiting_Owned,
         Requested_Authority =>
           (if Waiting then
               Item.Content.Imports (State.Waiting_Import).Authority
            else No_Authority),
         Requested_Binding =>
           (if Waiting then
               Item.Content.Imports (State.Waiting_Import).Binding
            else 0),
         Stream_Requested => State.Waiting_Stream and then not State.Terminal,
         Stream_Request => State.Stream_Request,
         others => <>);
      if State.Has_Value and then
        (State.Result_Value.Kind = Object_Value or else
         (State.Result_Value.Kind = List_Value and then
          Compound_Elements (Item.Content.Data_Types, State.Result_Value.Data_Type)))
      then
         declare
            Printed : Boolean := True;
         begin
            Print_Value (State.Arena, State.Text, State.Lists, Item.Content.Data_Types, State.Result_Value,
                         Print_Bound'Last, 1, Result.Literal, Printed);
            --  The literal is for display; a value without one (a Character
            --  field, or longer than a result carries) still completes, and a
            --  host takes it through Native_Objects.Export_Result.
            if Printed then
               Result.Has_Literal := True;
               Result.Literal_Shape := CCL.Types.Shapes.Shape_Of
                 (Item.Content.Data_Types, State.Result_Value.Data_Type);
            else
               Result.Literal := (others => <>);
            end if;
         end;
      elsif State.Has_Value and then State.Result_Value.Kind = List_Value then
         Export_List (State.Lists, State.Text, State.Arena, Item.Content.Data_Types, State.Result_Value, Result);
      end if;
   end Report;
end CCL.VM.Results;
