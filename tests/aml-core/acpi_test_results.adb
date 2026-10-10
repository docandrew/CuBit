with Ada.Text_IO;
package body ACPI_Test_Results is
   package V renames ACPI_Service.Values;
   use type V.Value_Handle;
   use type V.Access_Status;
   use type V.Value_Kind;
   use type AML_Execute.Execution_Status;
   procedure Drop (Service : in out ACPI_Service.State; Held : in out Holder) is
      Status : V.Access_Status;
   begin
      if Held.Root /= V.No_Value then
         ACPI_Service.Release_Result (Service, Held.Root, Status);
         if Status /= V.Available then raise Program_Error with "test result release failed"; end if;
      end if;
   end Drop;
   procedure Invoke
     (Service : aliased in out ACPI_Service.State; Held : in out Holder;
      Node : ACPI_Service.Namespace.Node_ID; Args : AML_Execute.Arguments;
      Count, Budget : Natural; Result : out V.Result) is
      Status : V.Access_Status;
   begin
      Drop (Service, Held);
      ACPI_Service.Invoke_Retained (Service, Node, Args, Count, Budget, Result, Status);
      if Status /= V.Available then raise Program_Error with "invoke access: " & Status'Image; end if;
      if Result.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned then
         Held.Root := Result.Handle;
      end if;
   end Invoke;
   procedure Seal (Service : in out ACPI_Service.State) is
      Report : ACPI_Service.Namespace.Initialization_Report;
      Status : V.Access_Status;
   begin
      ACPI_Service.Initialize_Members (Service, Report, Status);
      if Status /= V.Available then raise Program_Error with "seal: " & Status'Image; end if;
      if Report.Missing /= 0 or else Report.Unsupported /= 0 then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
            "acpi member initialization: bound=" & Report.Bound'Image &
            " missing=" & Report.Missing'Image & " unsupported=" & Report.Unsupported'Image);
      end if;
   end Seal;
   function Child (Service : ACPI_Service.State; Parent : ACPI_Service.Namespace.Node_ID;
      Part : AML_Names.Segment) return ACPI_Service.Namespace.Node_ID is
      Node : ACPI_Service.Namespace.Node_ID;
      Status : V.Access_Status;
   begin
      ACPI_Service.Find_Child (Service, Parent, Part, Node, Status);
      if Status /= V.Available then raise Program_Error with "child access: " & Status'Image; end if;
      return Node;
   end Child;
   function Integer_Data (Service : in out ACPI_Service.State;
      Node : ACPI_Service.Namespace.Node_ID) return AML_Decode.Integer_Value is
      Result : V.Result;
      Status : V.Access_Status;
      Handle : V.Value_Handle;
   begin
      ACPI_Service.Observe_Named_Value (Service, Node, Result, Status);
      if Status = V.Available and then Result.Status = AML_Execute.Returned then return Result.Number; end if;
      if Status = V.Available and then Result.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned then
         Handle := Result.Handle; ACPI_Service.Release_Result (Service, Handle, Status);
      end if;
      raise Program_Error with "named integer expected";
   end Integer_Data;
   function Describe (Service : ACPI_Service.State; Handle : V.Value_Handle) return V.Value_Description is
      D : V.Value_Description;
      Status : V.Access_Status;
   begin
      ACPI_Service.Describe_Result (Service, Handle, D, Status);
      if Status /= V.Available then raise Program_Error with "describe failed"; end if;
      return D;
   end Describe;
   function Bytes (Service : ACPI_Service.State; Handle : V.Value_Handle) return AML_Decode.Bytes is
      Description : V.Value_Description;
      Status : V.Access_Status;
   begin
      ACPI_Service.Describe_Result (Service, Handle, Description, Status);
      if Status /= V.Available or else Description.Kind not in V.String_Description | V.Buffer_Description then
         raise Program_Error with "byte result expected";
      end if;
      declare
         Data : AML_Decode.Bytes (1 .. Description.Length);
         Copied : Natural;
      begin
         ACPI_Service.Read_Result_Bytes (Service, Handle, 0, Data, Copied, Status);
         if Status /= V.Available or else Copied /= Data'Length then raise Program_Error with "byte result read"; end if;
         return Data;
      end;
   end Bytes;
   procedure Emit (Service : in out ACPI_Service.State; Handle : V.Value_Handle; Depth : Natural := 0) is
      Description : V.Value_Description;
      Status : V.Access_Status;
      Chunk_Size : constant := 256;
      Data : AML_Decode.Bytes (1 .. Chunk_Size);
      Offset, Copied : Natural := 0;
   begin
      if Depth > 64 then raise Program_Error with "object depth"; end if;
      ACPI_Service.Describe_Result (Service, Handle, Description, Status);
      if Status /= V.Available then raise Program_Error with "describe: " & Status'Image; end if;
      case Description.Kind is
         when V.Integer_Description => Ada.Text_IO.Put_Line ("INTEGER" & Description.Number'Image);
         when V.Reference_Description => Ada.Text_IO.Put_Line ("REFERENCE " & Description.Ref_Kind'Image);
         when V.String_Description | V.Buffer_Description =>
            Ada.Text_IO.Put ((if Description.Kind = V.String_Description then "STRING " else "BUFFER"));
            while Offset < Description.Length loop
               ACPI_Service.Read_Result_Bytes (Service, Handle, Offset, Data, Copied, Status);
               if Status /= V.Available or else Copied = 0 then raise Program_Error with "emit bytes"; end if;
               for I in 1 .. Copied loop
                  if Description.Kind = V.String_Description then Ada.Text_IO.Put (Character'Val (Data (I)));
                  else Ada.Text_IO.Put (Data (I)'Image); end if;
               end loop;
               Offset := Offset + Copied;
            end loop;
            Ada.Text_IO.New_Line;
         when V.Package_Description =>
            Ada.Text_IO.Put_Line ("PACKAGE" & Description.Length'Image);
            for I in 1 .. Description.Length loop
               declare
                  Element : V.Value_Handle := V.No_Value;
               begin
                  ACPI_Service.Read_Result_Element (Service, Handle, I - 1, Element, Status);
                  if Status = V.Uninitialized_Element then Ada.Text_IO.Put_Line ("NULL");
                  elsif Status /= V.Available then raise Program_Error with "emit element: " & Status'Image;
                  else
                     Emit (Service, Element, Depth + 1);
                     ACPI_Service.Release_Result (Service, Element, Status);
                     if Status /= V.Available then raise Program_Error with "emit release"; end if;
                  end if;
               exception
                  when others =>
                     if Element /= V.No_Value then ACPI_Service.Release_Result (Service, Element, Status); end if;
                     raise;
               end;
            end loop;
      end case;
   end Emit;
   procedure Emit_Named (Service : in out ACPI_Service.State; Node : ACPI_Service.Namespace.Node_ID) is
      Result : V.Result;
      Status : V.Access_Status;
      Handle : V.Value_Handle := V.No_Value;
   begin
      ACPI_Service.Observe_Named_Value (Service, Node, Result, Status);
      if Status = V.Uninitialized_Element then Ada.Text_IO.Put_Line ("NULL"); return; end if;
      if Status /= V.Available then raise Program_Error with "named read: " & Status'Image; end if;
      if Result.Status = AML_Execute.Returned then Ada.Text_IO.Put_Line ("INTEGER" & Result.Number'Image);
      else
         Handle := Result.Handle; Emit (Service, Handle);
         ACPI_Service.Release_Result (Service, Handle, Status);
         if Status /= V.Available then raise Program_Error with "named release"; end if;
      end if;
   exception
      when others =>
         if Handle /= V.No_Value then ACPI_Service.Release_Result (Service, Handle, Status); end if;
         raise;
   end Emit_Named;
end ACPI_Test_Results;
