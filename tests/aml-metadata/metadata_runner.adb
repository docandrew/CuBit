with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with ACPI_Service_Core;
with AML_Delays;
with AML_Execute;
with AML_Decode;
with Firmware_Tables;
procedure Metadata_Runner is
   procedure No_Delay (Item : AML_Delays.Request; Result : out AML_Delays.Outcome) is
      pragma Unreferenced (Item);
   begin
      Result := AML_Delays.Unavailable;
   end No_Delay;
   Table_Byte_Capacity : constant Positive := 4096;
   Execution_Budget : constant Positive := 100_000;
   package Core is new ACPI_Service_Core
     (Perform_Delay => No_Delay);
   use Core;
   use type Ada.Streams.Stream_Element_Offset;
   use type Ada.Streams.Stream_IO.Count;
   use type Values.Access_Status;
   use type AML_Execute.Execution_Status;
   package IO renames Ada.Streams.Stream_IO;
   type State_Access is access all State;
   Service : constant State_Access := new State (1, Table_Byte_Capacity, Table_Byte_Capacity, Load_Capacity => 1);
   File : IO.File_Type;
   Loaded : Install_Status;
   Access_Result : Values.Access_Status;
   Report : Namespace.Initialization_Report;
   Test_Node, Marker_Node, Post_Node : Namespace.Node_ID;
   Result, Post_Result : Values.Result;
   Marker : Values.Result;
   function Trim (Text : String) return String is
     (Ada.Strings.Fixed.Trim (Text, Ada.Strings.Both));
   procedure Require_Access is
   begin
      if Access_Result /= Values.Available then
         raise Program_Error with "service access: " & Access_Result'Image;
      end if;
   end Require_Access;
   procedure Release_Unexpected_Compound (Item : Values.Result) is
      Handle : Values.Value_Handle;
   begin
      -- Preserve the actual success status for protocol rejection; never fold
      -- compound success into the expected Unsupported_Value interpreter error.
      if Item.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
                        AML_Execute.Metadata_Returned then
         Handle := Item.Handle;
         Release_Result (Service.all, Handle, Access_Result); Require_Access;
      end if;
      if Retained_Results (Service.all) /= 0 then
         raise Program_Error with "phase leaked retained results";
      end if;
   end Release_Unexpected_Compound;
   procedure Emit (Phase : String; Status : AML_Execute.Execution_Status;
                   Value : AML_Decode.Integer_Value) is
   begin
      Ada.Text_IO.Put_Line (Phase & " " & Status'Image & " " &
        (if Status = AML_Execute.Returned then Trim (Value'Image) else "NONE"));
   end Emit;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      raise Program_Error with "usage: metadata_runner table.aml";
   end if;
   IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
   if IO.Size (File) = 0 or else IO.Size (File) > IO.Count (Table_Byte_Capacity) then
      raise Program_Error with "table byte bound";
   end if;
   declare
      Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
      Last : Ada.Streams.Stream_Element_Offset;
      Data : Firmware_Tables.Bytes (1 .. Raw'Length);
   begin
      IO.Read (File, Raw, Last); IO.Close (File);
      if Last /= Raw'Last then raise Program_Error with "short read"; end if;
      for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
      Install (Service.all, 1, DSDT, Data, Loaded);
   end;
   if Loaded /= Installed then raise Program_Error with "install: " & Loaded'Image; end if;
   Initialize_Members (Service.all, Report, Access_Result); Require_Access;
   if Report.Missing /= 0 or else Report.Unsupported /= 0 then
      raise Program_Error with "incomplete namespace initialization";
   end if;
   Find_Child (Service.all, Namespace.Root, "TEST", Test_Node, Access_Result); Require_Access;
   Find_Child (Service.all, Namespace.Root, "MARK", Marker_Node, Access_Result); Require_Access;
   Find_Child (Service.all, Namespace.Root, "POST", Post_Node, Access_Result); Require_Access;
   Invoke_Retained (Service.all, Test_Node, [others => 0], 0, Execution_Budget,
                    Result, Access_Result); Require_Access;
   Release_Unexpected_Compound (Result);
   Observe_Named_Value (Service.all, Marker_Node, Marker, Access_Result); Require_Access;
   Release_Unexpected_Compound (Marker);
   Invoke_Retained (Service.all, Post_Node, [others => 0], 0, Execution_Budget,
                    Post_Result, Access_Result); Require_Access;
   Release_Unexpected_Compound (Post_Result);
   Ada.Text_IO.Put_Line ("METADATA_REPLAY 1");
   Emit ("TEST", Result.Status,
         (if Result.Status = AML_Execute.Returned then Result.Number else 0));
   Emit ("MARK", Marker.Status,
         (if Marker.Status = AML_Execute.Returned then Marker.Number else 0));
   Emit ("POST", Post_Result.Status,
         (if Post_Result.Status = AML_Execute.Returned then Post_Result.Number else 0));
   Ada.Text_IO.Put_Line ("END");
end Metadata_Runner;
