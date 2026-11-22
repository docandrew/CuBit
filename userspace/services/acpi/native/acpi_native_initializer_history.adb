pragma Ada_2022;
package body ACPI_Native_Initializer_History is
   package C renames ACPI_Service;
   package H renames Encoding;
   Trace : H.State;
   Last : Report;
   use type H.Outcome_Code, C.Values.Access_Status;
   pragma Compile_Time_Error
     (C.Values.Access_Status'Pos (C.Values.Access_Status'Last) > H.Outcome_Code'Last
      or else AML_Execute.Execution_Status'Pos (AML_Execute.Execution_Status'Last) > H.Outcome_Code'Last,
      "initializer history outcome encoding must retain every status");
   procedure Capture_Path (Service : C.State; Node : C.Namespace.Node_ID;
      Text : out String; Complete : out Boolean) is
      Cursor : C.Namespace.Node_ID := Node;
      Item : C.Node_Descriptor;
      Status : C.Values.Access_Status;
      At_Character : Natural := Text'Last;
   begin
      Text := [Text'Range => ' ']; Complete := False;
      for Depth in 1 .. Path_Segments loop
         if Cursor = C.Namespace.Root then
            Text(At_Character) := '\'; Complete := True; return;
         end if;
         C.Describe_Node(Service,Cursor,Item,Status);
         if Status /= C.Values.Available then return; end if;
         Text(At_Character - Segment_Characters + 1 .. At_Character) := C.Values.Node_Name(Item);
         At_Character := At_Character - Segment_Characters;
         if C.Values.Parent_Position(Item) = C.Namespace.Root then
            Text(At_Character) := '\'; Complete := True; return;
         end if;
         Text(At_Character) := '.';
         At_Character := At_Character - 1;
         Cursor := C.Values.Parent_Position(Item);
      end loop;
      -- A full buffer is explicitly truncated, never presented as a full path.
   end Capture_Path;
   procedure Copy_Path (Service : C.State; Node : C.Namespace.Node_ID;
      Text : out Path_Text; Complete : out Boolean) is
   begin Capture_Path (Service, Node, Text, Complete); end Copy_Path;
   procedure Note
     (Service : C.State; Scope : C.Namespace.Node_ID; Method : C.Node_Descriptor;
      Access_Result : C.Values.Access_Status; Execution : AML_Execute.Execution_Status;
      Reported_Charge : Natural) is
      Accepted : Boolean;
   begin
      Last.Seen := True; Last.Last_Scope := Scope; Last.Last_Method := C.Values.Position (Method);
      Last.Access_Result := Access_Result; Last.Execution := Execution; Last.Reported_Charge := Reported_Charge;
      Last.Last_Path := [others => ' ']; Last.Path_Complete := False;
      if not C.Current_Binding (Service, Method) then
         H.Invalidate (Trace); return;
      end if;
      Capture_Path (Service, C.Values.Position (Method), Last.Last_Path, Last.Path_Complete);
      if C.Values.Position (Method) = C.Namespace.Root then
         H.Invalidate (Trace); return;
      end if;
      H.Append (Trace,
        (Scope => H.Node_Position (Scope), Method => H.Method_Position (C.Values.Position (Method)),
         Access_Result => H.Outcome_Code (C.Values.Access_Status'Pos (Access_Result)),
         Execution => H.Outcome_Code (AML_Execute.Execution_Status'Pos (Execution)),
         Reported_Charge => H.Fuel_Charge (Reported_Charge)), Accepted);
   end Note;
   function Observe return Report is
      Result : Report := Last;
   begin
      Result.Invocations := Interfaces.Unsigned_32 (H.Count (Trace));
      Result.Ordered_Digest := H.Digest (Trace); Result.Complete := H.Complete (Trace);
      return Result;
   end Observe;
end ACPI_Native_Initializer_History;
