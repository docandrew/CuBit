with CCL.Compiler;
with CCL.Objects.Views;
with CCL.Handler_References;
with CCL.Imports;
with CCL.Ownership;
with CCL.Types;
with CCL.VM.Native_Objects;

package body CCL.Evaluation is
   package L renames CCL.Language;
   package N renames CCL.VM.Native_Objects;
   use type CCL.Catalog.Link_Result;
   use type CCL.Compiler.Compilation_Status;
   use type CCL.Host_Values.Value_Kind;
   use type CCL.Language.Analysis_Status;
   use type CCL.Language.Interpretation_Status;
   use type CCL.Streams.View_Kind;
   use type CCL.Streams.View_Status;
   use type CCL.Types.Type_Reference;
   use type CCL.VM.Execution_Status;
   use type CCL.VM.Validation_Error;
   use type CCL.VM.Value_Kind;
   use type CCL.Imports.Cancellation_Mode;
   use type CCL.Ownership.Disposition_Id;
   use type L.Node_Kind;

   function Status_Of (Item : CCL.VM.Execution_Status) return L.Interpretation_Status is
     (case Item is
         when CCL.VM.Completed => L.Succeeded,
         when CCL.VM.Fuel_Exhausted => L.Evaluation_Fuel_Exhausted,
         when CCL.VM.Arithmetic_Overflow => L.Evaluation_Overflow,
         when CCL.VM.Division_By_Zero => L.Evaluation_Division_By_Zero,
         when CCL.VM.Object_Storage_Exhausted => L.Evaluation_Object_Storage_Exhausted,
         when CCL.VM.Text_Storage_Exhausted => L.Evaluation_Text_Storage_Exhausted,
         when CCL.VM.Invalid_Number => L.Evaluation_Invalid_Number,
         when CCL.VM.Index_Out_Of_Range => L.Evaluation_Index_Error,
         when CCL.VM.List_Storage_Exhausted => L.Evaluation_List_Storage_Exhausted,
         when CCL.VM.Range_Error => L.Evaluation_Range_Error,
         when CCL.VM.Call_Depth_Exhausted => L.Evaluation_Depth_Exhausted,
         when CCL.VM.Stream_Unavailable => L.Stream_Unavailable,
         when CCL.VM.Stream_Empty => L.Stream_Empty,
         when CCL.VM.Stream_Window_Out_Of_Range => L.Stream_Window_Out_Of_Range,
         when CCL.VM.Stream_Element_Mismatch => L.Stream_Element_Mismatch,
         when CCL.VM.Host_Call_Failed => L.Host_Call_Failed,
         when CCL.VM.Host_Argument_Out_Of_Bounds => L.Host_Argument_Out_Of_Bounds,
         when CCL.VM.Invalid_Bytecode | CCL.VM.Paused | CCL.VM.Stopped |
              CCL.VM.Waiting_For_Host | CCL.VM.No_Result => L.Not_Compiled);

   procedure Name_Reason (Result : in out L.Interpretation_Result; Reason : String) is
   begin
      Result.Status := L.Not_Compiled;
      Result.Diagnostic_Subject := CCL.Types.Named (Reason);
   end Name_Reason;

   --  A type as source writes it: Integer, P, (List P).
   function Type_Source (Types : CCL.Types.Registry; Kind : CCL.Types.Type_Reference) return String
     with Subprogram_Variant => (Decreases => Kind)
   is
   begin
      if CCL.Types.Is_List (Types, Kind) and then CCL.Types.Element_Of (Types, Kind) < Kind then
         return "(List " & Type_Source (Types, CCL.Types.Element_Of (Types, Kind)) & ")";
      end if;
      return CCL.Types.Image (CCL.Types.Describe (Types, Kind).Identifier);
   end Type_Source;

   --  The checked, compiled, linked and verified form of Source, or why not.
   type Prepared is record
      Analysis : L.Analysis_Result;
      Compiled : CCL.Compiler.Compilation_Result;
      Program : CCL.VM.Validated_Program;
      Ready : Boolean := False;
   end record;

   --  Where import Import is called in the source: its operation's name and
   --  position, for a failure.
   procedure Locate_Call
     (Item : Prepared; Import : CCL.VM.Import_Index; Result : in out L.Interpretation_Result)
   is
      Wanted : constant CCL.Catalog.Resolved_Operation :=
        CCL.Catalog.Element (Item.Compiled.Linkage, Import);
   begin
      for Index in 0 .. L.Analysis_Node_Count (Item.Analysis) - 1 loop
         declare
            Node : constant L.Node := L.Analysis_Node (Item.Analysis, Index);
         begin
            if Node.Kind = L.Host_Import_Form
              and then CCL.Catalog.Same_Operation (Node.Host_Call, Wanted)
            then
               Result.Failed_Operation := Node.Identifier;
               Result.Diagnostic_Position := Node.Source_Position;
               return;
            end if;
         end;
      end loop;
   end Locate_Call;

   --  The first operation the program calls that Grants does not hold.
   procedure Name_Ungranted
     (Item : Prepared; Grants : CCL.Catalog.Granted_Bindings; Result : in out L.Interpretation_Result)
   is
      Binding : Interfaces.Unsigned_32;
      Found : Boolean;
   begin
      for Import in 0 .. Natural (CCL.Catalog.Length (Item.Compiled.Linkage)) - 1 loop
         CCL.Catalog.Find_Granted_Binding
           (Grants, CCL.Catalog.Element (Item.Compiled.Linkage, CCL.VM.Import_Index (Import)), Binding, Found);
         if not Found then
            Locate_Call (Item, CCL.VM.Import_Index (Import), Result);
            return;
         end if;
      end loop;
   end Name_Ungranted;

   --  Compile, link and verify Item.Analysis (already set).
   procedure Build
     (Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings; Item : in out Prepared;
      Result : in out L.Interpretation_Result; Scalar_Only : Boolean := False)
   is
      Linked : CCL.Catalog.Link_Result;
      Program : CCL.VM.Program;
      Error : CCL.VM.Validation_Error;
   begin
      Item.Ready := False;
      if L.Analysis_Status_Of (Item.Analysis) /= L.Analysis_Succeeded then
         Result.Status :=
           (if L.Analysis_Status_Of (Item.Analysis) = L.Analysis_Type_Check_Failed
            then L.Type_Check_Failed else L.Parse_Failed);
         Result.Diagnostic := L.Analysis_Diagnostic (Item.Analysis);
         Result.Diagnostic_Position := L.Analysis_Diagnostic_Position (Item.Analysis);
         Result.Diagnostic_Subject := L.Analysis_Diagnostic_Subject (Item.Analysis);
         Result.Diagnostic_Expected := L.Analysis_Diagnostic_Expected (Item.Analysis);
         Result.Diagnostic_Found := L.Analysis_Diagnostic_Found (Item.Analysis);
         return;
      end if;
      CCL.Compiler.Compile (Item.Analysis, Item.Compiled);
      if Item.Compiled.Status = CCL.Compiler.Ownership_Check_Failed then
         Result.Status := L.Type_Check_Failed;
         Result.Diagnostic := L.Resource_Ownership_Violation;
         return;
      elsif Item.Compiled.Status /= CCL.Compiler.Compilation_Succeeded then
         Name_Reason (Result, CCL.Compiler.Compilation_Status'Image (Item.Compiled.Status));
         Result.Diagnostic_Position := Item.Compiled.Source_Position;
         return;
      end if;
      Program := Item.Compiled.Program;
      --  This driver answers each call synchronously, by value: an import
      --  with a cancellation protocol, an owned operand, dispositions or
      --  resources (CCL-003) is refused before anything runs.
      for Import in 0 .. Natural (CCL.Catalog.Length (Item.Compiled.Linkage)) - 1 loop
         declare
            Op : constant CCL.Host_Values.Import_Declaration :=
              CCL.Catalog.Element (Item.Compiled.Linkage, CCL.VM.Import_Index (Import)).Import;
         begin
            if Op.Ownership_Argument or else CCL.Host_Values.Has_Resources (Op) or else
              Op.Cancellation /= CCL.Imports.Not_Cancellable or else
              Op.Success_Verb /= 0 or else Op.Failure_Verb /= 0 or else Op.Cancel_Verb /= 0 or else
              (Scalar_Only and then
               (Op.Argument not in CCL.Host_Values.Integer_Value | CCL.Host_Values.Boolean_Value or else
                Op.Result not in CCL.Host_Values.Integer_Value | CCL.Host_Values.Boolean_Value))
            then
               Result.Status := L.Host_Contract_Unsupported;
               Locate_Call (Item, CCL.VM.Import_Index (Import), Result);
               return;
            end if;
         end;
      end loop;
      CCL.Catalog.Link_Program (Grants, Item.Compiled.Linkage, Program, Linked, Visible_Interfaces);
      case Linked is
         when CCL.Catalog.Link_Valid => null;
         when CCL.Catalog.Authority_Not_Granted =>
            Result.Status := L.Host_Authority_Denied;
            Name_Ungranted (Item, Grants, Result);
            return;
         when CCL.Catalog.Linkage_Length_Mismatch | CCL.Catalog.Import_Contract_Mismatch =>
            Result.Status := L.Host_Contract_Unsupported; return;
      end case;
      CCL.VM.Verify (Program, Item.Program, Error);
      if Error /= CCL.VM.Valid then
         Name_Reason (Result, CCL.VM.Validation_Error'Image (Error));
         return;
      end if;
      Item.Ready := True;
   end Build;


   --  Fill Result from a completed run's value.
   procedure Report_Value
     (Item : Prepared; Step : CCL.VM.Execution_Result; Result : in out L.Interpretation_Result)
   is
      Types : constant CCL.Types.Registry := L.Analysis_Types (Item.Analysis);
      V : CCL.VM.Value renames Step.Result_Value;
   begin
      Result.Has_Value := Step.Has_Value;
      if not Step.Has_Value then return; end if;
      case V.Kind is
         when CCL.VM.Integer_Value | CCL.VM.Boolean_Value =>
            Result.Result_Value := V;
            if V.Data_Type /= CCL.Types.Invalid_Type and then CCL.Types.Is_Handle (Types, V.Data_Type) then
               declare
                  Element : constant String :=
                    Type_Source (Types, (if CCL.Types.Is_Task (Types, V.Data_Type)
                                         then CCL.Types.Task_Result (Types, V.Data_Type)
                                         else CCL.Types.Stream_Element (Types, V.Data_Type)));
               begin
                  if Element'Length > L.MAX_TYPE_TEXT then
                     Result.Status := L.Host_Contract_Unsupported; Result.Has_Value := False;
                     return;
                  end if;
                  Result.Has_Stream := True;
                  Result.Is_Task := CCL.Types.Is_Task (Types, V.Data_Type);
                  Result.Stream := CCL.Streams.Handle (V.Integer);
                  Result.Stream_Element.Length := Element'Length;
                  Result.Stream_Element.Data (1 .. Element'Length) := Element;
               end;
            end if;
         when CCL.VM.Text_Value =>
            if not Step.Has_Result_Text then
               Result.Status := L.Evaluation_Text_Storage_Exhausted; Result.Has_Value := False;
               return;
            end if;
            Result.Has_Text := True;
            Result.Result_Text.Length := Step.Result_Text_Value.Length;
            Result.Result_Text.Data (1 .. Step.Result_Text_Value.Length) :=
              Step.Result_Text_Value.Data (1 .. Step.Result_Text_Value.Length);
         when CCL.VM.Character_Value =>
            Result.Has_Character := True;
            Result.Result_Character := Character'Val (V.Integer);
         when CCL.VM.Variant_Value =>
            --  An enumeration member, or a variant with a scalar payload:
            --  the payload is the result's value (as the interpreter gave).
            Result.Result_Value :=
              (if CCL.Types.Describe (Types, V.Data_Type).Parts (V.Alternative).Payload = CCL.Types.Boolean_Type
               then CCL.VM.Boolean_Constant (V.Boolean) else CCL.VM.Integer_Constant (V.Integer));
            Result.Variant_Type := V.Data_Type;
            Result.Variant_Type_Name := CCL.Types.Describe (Types, V.Data_Type).Identifier;
            Result.Variant_Member_Name := CCL.Types.Describe (Types, V.Data_Type).Parts (V.Alternative).Identifier;
            Result.Variant_Payload_Type := CCL.Types.Describe (Types, V.Data_Type).Parts (V.Alternative).Payload;
         when CCL.VM.Function_Value =>
            Result.Has_Function := True;
            Result.Function_Name := L.Analysis_Function (Item.Analysis, L.Function_Index (V.Integer)).Identifier;
         when CCL.VM.List_Value | CCL.VM.Object_Value =>
            if Step.Has_Literal then
               Result.Has_Literal := True;
               Result.Literal_Type := V.Data_Type;
               Result.Literal_Type_Name := CCL.Types.Describe (Types, V.Data_Type).Identifier;
               Result.Literal.Length := Step.Literal.Length;
               Result.Literal.Data (1 .. Step.Literal.Length) := Step.Literal.Data (1 .. Step.Literal.Length);
               Result.Literal_Shape := Step.Literal_Shape;
            elsif V.Kind = CCL.VM.List_Value and then
              CCL.VM.Element_Kind (Types, V.Data_Type) /= CCL.VM.Object_Value
            then
               Result.Has_List := True;
               Result.List_Type := V.Data_Type;
               Result.List_Element_Type := CCL.Types.Element_Of (Types, V.Data_Type);
               Result.List_Length := Step.List_Length;
               Result.List_Total := Step.List_Total;
               Result.List_Values := Step.List_Values;
               Result.List_Text.Length := Natural'Min (Step.List_Text.Length, L.MAX_TEXT_BYTES);
               Result.List_Text.Data (1 .. Result.List_Text.Length) :=
                 Step.List_Text.Data (1 .. Result.List_Text.Length);
               Result.List_Text_Ends := Step.List_Text_Ends;
            else
               Result.Status := L.Host_Contract_Unsupported; Result.Has_Value := False;
            end if;
         when CCL.VM.Resource_Value =>
            --  Kept by the session (CCL-003); nothing else holds one yet.
            Result.Status := L.Host_Contract_Unsupported; Result.Has_Value := False;
      end case;
   end Report_Value;

   --  A result's value as CCL writes it, for the scalar, text, enum and
   --  literal results a task can carry ("" for anything else).
   function Literal_Of (Item : L.Interpretation_Result) return String is
      function Trimmed (Image : String) return String is
        (if Image'Length > 0 and then Image (Image'First) = ' '
         then Image (Image'First + 1 .. Image'Last) else Image);
   begin
      if Item.Has_Literal then
         return Item.Literal.Data (1 .. Item.Literal.Length);
      elsif Item.Has_Text then
         return '"' & Item.Result_Text.Data (1 .. Item.Result_Text.Length) & '"';
      elsif Item.Variant_Type /= CCL.Types.Invalid_Type then
         return CCL.Types.Image (Item.Variant_Type_Name) & "." & CCL.Types.Image (Item.Variant_Member_Name);
      elsif Item.Has_Value and then Item.Result_Value.Kind = CCL.VM.Integer_Value then
         return Trimmed (Interfaces.Integer_64'Image (Item.Result_Value.Integer));
      elsif Item.Has_Value and then Item.Result_Value.Kind = CCL.VM.Boolean_Value then
         return (if Item.Result_Value.Boolean then "true" else "false");
      end if;
      return "";
   end Literal_Of;

   generic
      type Host_Context is limited private;
      with procedure Invoke
        (Context : in out Host_Context; Binding : Interfaces.Unsigned_32;
         Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result);
      with procedure Read_Stream
        (Context : in out Host_Context; Request : CCL.Streams.View_Request;
         Reply : in out CCL.Streams.View_Reply);
   procedure Run
     (Analysis : L.Analysis_Result; Fuel : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context; Result : out L.Interpretation_Result;
      Expected : CCL.Objects.Binding; Want_Object : Boolean;
      Object : out CCL.Objects.Image; Object_Ready : out Boolean;
      Scalar_Only : Boolean := False);

   procedure Run
     (Analysis : L.Analysis_Result; Fuel : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context; Result : out L.Interpretation_Result;
      Expected : CCL.Objects.Binding; Want_Object : Boolean;
      Object : out CCL.Objects.Image; Object_Ready : out Boolean;
      Scalar_Only : Boolean := False)
   is
      Item : Prepared;
      Machine : N.Machine;
      Step : CCL.VM.Execution_Result;
      Mismatched : Boolean := False;

      --  The waiting call's argument, as the host takes it.
      procedure Argument_Of
        (Op : CCL.Host_Values.Import_Declaration; Argument : out CCL.Host_Values.Value;
         Good : out Boolean)
      is
         Contract : CCL.Objects.Binding;
         Image : CCL.Objects.Image;
      begin
         Argument := CCL.Host_Values.Integer_Constant (0);
         Good := True;
         case Step.Request_Argument.Kind is
            when CCL.VM.Integer_Value | CCL.VM.Boolean_Value =>
               Argument := CCL.Host_Values.From_Scalar (Step.Request_Argument);
            when CCL.VM.Text_Value =>
               Good := Step.Has_Request_Text;
               if Good and then Op.Argument = CCL.Host_Values.Object_Value then
                  --  A schema-bound String: the host takes it as an image.
                  declare
                     Built : CCL.Objects.Build_Result;
                  begin
                     CCL.Catalog.Resolve_Schema (Visible_Interfaces, Op.Argument_Schema, Contract);
                     Image := CCL.Objects.Empty (Contract);
                     CCL.Objects.Append_Text
                       (Image, Step.Request_Text.Data (1 .. Step.Request_Text.Length), Built);
                     Good := CCL.Objects."=" (Built, CCL.Objects.Added) and then
                       CCL.Objects.Validate (Image, Contract);
                     if Good then Argument := CCL.Host_Values.Object_Constant (Image); end if;
                  end;
               elsif Good then
                  declare
                     Text : CCL.Host_Values.Text;
                  begin
                     CCL.Host_Values.Copy_Text
                       (Step.Request_Text.Data (1 .. Step.Request_Text.Length), Text, Good);
                     if Good then Argument := CCL.Host_Values.Text_Constant (Text); end if;
                  end;
               end if;
            when CCL.VM.Function_Value =>
               --  A handler: the host registers it by the program's source
               --  and the function's name (CCL.Handler_References).
               declare
                  Name : constant L.Name :=
                    L.Analysis_Function (Item.Analysis, L.Function_Index (Step.Request_Argument.Integer)).Identifier;
                  Ref : CCL.Handler_References.Reference;
               begin
                  CCL.Handler_References.Create
                    (L.Analysis_Source (Item.Analysis), Name.Data (1 .. Name.Length), Ref, Good);
                  if Good then Argument := CCL.Host_Values.Handler_Constant (Ref); end if;
               end;
            when CCL.VM.Object_Value | CCL.VM.List_Value | CCL.VM.Variant_Value =>
               CCL.Catalog.Resolve_Schema (Visible_Interfaces, Op.Argument_Schema, Contract);
               N.Export_Argument (Item.Program, Machine, Contract, Image, Good);
               if Good then Argument := CCL.Host_Values.Object_Constant (Image); end if;
            when others =>
               Good := False;
         end case;
      end Argument_Of;

      --  Answer the waiting call with the host's reply.
      procedure Answer (Op : CCL.Host_Values.Import_Declaration; Reply : CCL.Host_Values.Call_Result) is
         Scalar : CCL.VM.Value;
         Good : Boolean;
         Contract : CCL.Objects.Binding;
      begin
         --  A reply of another kind than the contract's is the host's fault.
         if Reply.Success and then Reply.Value.Kind /=
           (if Op.Result_Stream or Op.Result_Task then CCL.Host_Values.Integer_Value else Op.Result)
         then
            Mismatched := True;
         end if;
         if Op.Result = CCL.Host_Values.Text_Value then
            if Reply.Success and then Reply.Value.Kind = CCL.Host_Values.Text_Value and then
              Reply.Value.Content.Length > Op.Result_Text_Limit
            then
               Mismatched := True;
            end if;
            if Reply.Success and then Reply.Value.Kind = CCL.Host_Values.Text_Value then
               N.Complete_Text (Item.Program, Machine,
                                Reply.Value.Content.Data (1 .. Reply.Value.Content.Length), True);
            else
               N.Complete_Text (Item.Program, Machine, "", False);
            end if;
         elsif Op.Result = CCL.Host_Values.Object_Value and then
           CCL.Catalog.Schema_Type (Visible_Interfaces, Op.Result_Schema) = CCL.Types.String_Type
         then
            --  A schema-bound String image: the run takes its text.
            CCL.Catalog.Resolve_Schema (Visible_Interfaces, Op.Result_Schema, Contract);
            if Reply.Success and then Reply.Value.Kind = CCL.Host_Values.Object_Value and then
              CCL.Objects.Validate (Reply.Value.Object, Contract)
            then
               declare
                  View : CCL.Objects.Views.Snapshot;
                  Captured : Boolean;
               begin
                  CCL.Objects.Views.Capture (View, Contract, Reply.Value.Object, Captured);
                  if Captured then
                     N.Complete_Text (Item.Program, Machine,
                                      CCL.Objects.Views.Text (View, CCL.Objects.Views.Root (View)), True);
                  else
                     N.Complete_Text (Item.Program, Machine, "", False);
                  end if;
               end;
            else
               if Reply.Success then Mismatched := True; end if;
               N.Complete_Text (Item.Program, Machine, "", False);
            end if;
         elsif Op.Result = CCL.Host_Values.Object_Value and then not (Op.Result_Stream or Op.Result_Task) then
            CCL.Catalog.Resolve_Schema (Visible_Interfaces, Op.Result_Schema, Contract);
            if Reply.Success and then Reply.Value.Kind = CCL.Host_Values.Object_Value and then
              not CCL.Objects.Validate (Reply.Value.Object, Contract)
            then
               Mismatched := True;
            end if;
            if Reply.Success and then Reply.Value.Kind = CCL.Host_Values.Object_Value then
               N.Complete_Object (Item.Program, Machine, Contract, Reply.Value.Object, True);
            else
               N.Complete_Object (Item.Program, Machine, Contract, CCL.Objects.Empty (Contract), False);
            end if;
         else
            CCL.Host_Values.To_Scalar (Reply.Value, Scalar, Good);
            N.Complete_Scalar (Item.Program, Machine, Scalar, Reply.Success and Good);
         end if;
      end Answer;
   begin
      Result := (Fuel_Remaining => Fuel, others => <>);
      Object := (if Want_Object then CCL.Objects.Empty (Expected) else (others => <>));
      Object_Ready := False;
      Item.Analysis := Analysis;
      Build (Visible_Interfaces, Grants, Item, Result, Scalar_Only);
      if not Item.Ready then return; end if;
      if Want_Object and then not CCL.Objects.Matches_Type
        (Expected, L.Analysis_Types (Item.Analysis),
         L.Analysis_Node (Item.Analysis, L.Analysis_Root (Item.Analysis)).Static_Kind)
      then
         --  The program's type is not the one asked for: refused before it runs.
         Result.Status := L.Type_Check_Failed;
         return;
      end if;
      N.Initialize (Item.Program, Fuel, Machine);
      loop
         N.Continue_Execution_For (Item.Program, Machine, Fuel, Step);
         exit when Step.Status /= CCL.VM.Waiting_For_Host;
         if Step.Stream_Requested then
            declare
               Reply : CCL.Streams.View_Reply;
            begin
               Reply.Status := CCL.Streams.No_Such_Stream;
               Read_Stream (Context, Step.Stream_Request, Reply);
               if Step.Stream_Request.View = CCL.Streams.Wait_View
                 and then Reply.Status = CCL.Streams.Stream_Empty
               then
                  --  Pending: the caller runs the entry again once it is done.
                  N.Stop (Machine);
                  Result.Status := L.Waiting_On_Task;
                  Result.Waited_On := Step.Stream_Request.Stream;
                  Result.Fuel_Remaining := Natural'Min (Natural (Step.Fuel_Remaining), Fuel);
                  return;
               end if;
               N.Complete_Stream_View (Item.Program, Machine, Reply);
            end;
         else
            declare
               Op : constant CCL.Host_Values.Import_Declaration :=
                 CCL.Catalog.Element (Item.Compiled.Linkage, Step.Requested_Import).Import;
               Argument : CCL.Host_Values.Value;
               Reply : CCL.Host_Values.Call_Result;
               Good : Boolean;
            begin
               Argument_Of (Op, Argument, Good);
               if Good then
                  Invoke (Context, Step.Requested_Binding, Argument, Reply);
               else
                  Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
               end if;
               if not Reply.Success then
                  Result.Failure := Reply.Why;
                  Locate_Call (Item, Step.Requested_Import, Result);
                  if not Good then
                     Name_Reason (Result, "ARGUMENT_NOT_EXPORTED");
                  end if;
               end if;
               Answer (Op, Reply);
            end;
         end if;
      end loop;
      Result.Fuel_Remaining := Natural'Min (Natural (Step.Fuel_Remaining), Fuel);
      if Result.Status = L.Not_Compiled then
         N.Stop (Machine);
         return;
      end if;
      Result.Status :=
        (if Mismatched then L.Host_Result_Type_Mismatch else Status_Of (Step.Status));
      if Result.Status = L.Succeeded then
         Result.Diagnostic_Position := 0;
         if Want_Object then
            --  Only the image matters: no display limits apply.
            Result.Has_Value := Step.Has_Value;
            N.Export_Result (Item.Program, Machine, Expected, Object, Object_Ready);
            if not Object_Ready then
               Result.Status := L.Host_Result_Type_Mismatch;
            end if;
         else
            Report_Value (Item, Step, Result);
         end if;
      elsif Result.Status /= L.Host_Call_Failed then
         Result.Diagnostic_Position := 0;
      end if;
      N.Stop (Machine);
   end Run;

   procedure Evaluate_Analysis_With_Values
     (Analysis : CCL.Language.Analysis_Result; Fuel : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context; Result : out L.Interpretation_Result)
   is
      procedure Execute is new Run (Host_Context, Invoke, Read_Stream);
      Unused_Expected : CCL.Objects.Binding;
      Unused_Object : CCL.Objects.Image;
      Unused_Ready : Boolean;
   begin
      Execute (Analysis, Fuel, Visible_Interfaces, Grants, Context, Result,
               Unused_Expected, False, Unused_Object, Unused_Ready);
   end Evaluate_Analysis_With_Values;

   procedure Evaluate_With_Values
     (Source : String; Fuel : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context; Result : out L.Interpretation_Result)
   is
      procedure Show_Task_State;
      procedure Execute is new Evaluate_Analysis_With_Values (Host_Context, Invoke, Read_Stream);
      Analysis : L.Analysis_Result;

      --  A task result shows its state, read without waiting: Running, or
      --  Done with its result. The result comes from running the program
      --  again with its root expression replaced by (wait (task T n)): the
      --  task is done, so that run makes no host call, and the program's
      --  type and function definitions stay in scope for T.
      procedure Show_Task_State is
         Root : constant L.Node := L.Analysis_Node (Analysis, L.Analysis_Root (Analysis));
         Reply : CCL.Streams.View_Reply;
         Handle_Image : constant String := CCL.Streams.Handle'Image (Result.Stream);
         Waiting : constant String :=
           "(wait (task " & Result.Stream_Element.Data (1 .. Result.Stream_Element.Length) &
           Handle_Image & "))";
         Inner : L.Interpretation_Result;
         Inner_Analysis : L.Analysis_Result;
      begin
         Reply.Status := CCL.Streams.No_Such_Stream;
         Read_Stream (Context, (Stream => Result.Stream, View => CCL.Streams.Wait_View, Count => 1), Reply);
         if Reply.Status /= CCL.Streams.View_Answered then return; end if;
         Result.Task_Done := True;
         --  Positions are one-based; the end is one past the root's text.
         if Root.Source_Position not in 1 .. Source'Length or else
           Root.Source_End_Position not in Root.Source_Position + 1 .. Source'Length + 1
         then
            return;
         end if;
         L.Analyze (Source (Source'First .. Source'First + Root.Source_Position - 2) & Waiting &
                    Source (Source'First + Root.Source_End_Position - 1 .. Source'Last),
                    Visible_Interfaces, Inner_Analysis);
         Execute (Inner_Analysis, Fuel, Visible_Interfaces, Grants, Context, Inner);
         if Inner.Status = L.Succeeded then
            declare
               Shown : constant String := Literal_Of (Inner);
            begin
               if Shown'Length <= L.MAX_LITERAL_BYTES then
                  Result.Task_Value.Length := Shown'Length;
                  Result.Task_Value.Data (1 .. Shown'Length) := Shown;
               end if;
            end;
         end if;
      end Show_Task_State;
   begin
      L.Analyze (Source, Visible_Interfaces, Analysis);
      Execute (Analysis, Fuel, Visible_Interfaces, Grants, Context, Result);
      if Result.Status = L.Succeeded and then Result.Is_Task then
         Show_Task_State;
      end if;
   end Evaluate_With_Values;

   procedure Evaluate_With_Host
     (Source : String; Fuel : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context; Result : out L.Interpretation_Result)
   is
      procedure Invoke_Values
        (Context : in out Host_Context; Binding : Interfaces.Unsigned_32;
         Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
      is
         Scalar, Answer : CCL.VM.Value;
         Good, Success : Boolean;
      begin
         Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
         CCL.Host_Values.To_Scalar (Argument, Scalar, Good);
         if Good then
            Invoke (Context, Binding, Scalar, Answer, Success);
            --  Only a plain value: no ownership tag, copyable, untyped.
            if Success and then Answer.Kind in CCL.VM.Scalar_Kind and then Answer.Type_Tag = 0 and then
              Answer.Copyable and then Answer.Data_Type = CCL.Types.Invalid_Type
            then
               Reply := (Value => CCL.Host_Values.From_Scalar (Answer), Success => True, Why => <>);
            end if;
         end if;
      end Invoke_Values;
      procedure No_Views
        (Context : in out Host_Context; Request : CCL.Streams.View_Request;
         Reply : in out CCL.Streams.View_Reply) is null;
      procedure Execute is new Run (Host_Context, Invoke_Values, No_Views);
      Analysis : L.Analysis_Result;
      Unused_Expected : CCL.Objects.Binding;
      Unused_Object : CCL.Objects.Image;
      Unused_Ready : Boolean;
   begin
      L.Analyze (Source, Visible_Interfaces, Analysis);
      Execute (Analysis, Fuel, Visible_Interfaces, Grants, Context, Result,
               Unused_Expected, False, Unused_Object, Unused_Ready, Scalar_Only => True);
   end Evaluate_With_Host;

   type No_Host is null record;
   procedure Deny
     (Context : in out No_Host; Binding : Interfaces.Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
   is
      pragma Unreferenced (Context, Binding, Argument);
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
   end Deny;
   procedure No_Streams
     (Context : in out No_Host; Request : CCL.Streams.View_Request;
      Reply : in out CCL.Streams.View_Reply) is null;

   procedure Evaluate
     (Source : String; Fuel : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Result : out L.Interpretation_Result)
   is
      procedure Execute is new Evaluate_With_Values (No_Host, Deny, No_Streams);
      Context : No_Host;
      Grants : CCL.Catalog.Granted_Bindings;
   begin
      CCL.Catalog.Initialize (Grants);
      Execute (Source, Fuel, Visible_Interfaces, Grants, Context, Result);
      --  Without a host nothing can be granted: an operation needs one.
      if Result.Status = L.Host_Authority_Denied then
         Result.Status := L.Host_Import_Required;
      end if;
   end Evaluate;

   procedure Evaluate (Source : String; Fuel : Natural; Result : out L.Interpretation_Result) is
      Catalog : CCL.Catalog.Interface_Catalog;
   begin
      CCL.Catalog.Initialize (Catalog);
      Evaluate (Source, Fuel, Catalog, Result);
   end Evaluate;

   procedure Evaluate_Object_With_Values
     (Source : String; Fuel : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context; Expected : CCL.Objects.Binding;
      Result : out L.Object_Interpretation_Result)
   is
      procedure No_Views
        (Context : in out Host_Context; Request : CCL.Streams.View_Request;
         Reply : in out CCL.Streams.View_Reply) is null;
      procedure Execute is new Run (Host_Context, Invoke, No_Views);
      Outcome : L.Interpretation_Result;
      Ready : Boolean;
      Analysis : L.Analysis_Result;
   begin
      Result := (Fuel_Remaining => Fuel, others => <>);
      L.Analyze (Source, Visible_Interfaces, Analysis);
      Execute (Analysis, Fuel, Visible_Interfaces, Grants, Context, Outcome,
               Expected, True, Result.Value, Ready);
      Result.Status := Outcome.Status;
      Result.Diagnostic := Outcome.Diagnostic;
      Result.Diagnostic_Position := Outcome.Diagnostic_Position;
      Result.Diagnostic_Subject := Outcome.Diagnostic_Subject;
      Result.Diagnostic_Expected := Outcome.Diagnostic_Expected;
      Result.Diagnostic_Found := Outcome.Diagnostic_Found;
      Result.Fuel_Remaining := Outcome.Fuel_Remaining;
      Result.Has_Value := Outcome.Status = L.Succeeded and Ready;
   end Evaluate_Object_With_Values;

   procedure Evaluate_Object
     (Source : String; Fuel : Natural; Expected : CCL.Objects.Binding;
      Result : out L.Object_Interpretation_Result)
   is
      procedure Execute is new Evaluate_Object_With_Values (No_Host, Deny);
      Context : No_Host;
      Catalog : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
   begin
      CCL.Catalog.Initialize (Catalog);
      CCL.Catalog.Initialize (Grants);
      Execute (Source, Fuel, Catalog, Grants, Context, Expected, Result);
   end Evaluate_Object;
end CCL.Evaluation;
