with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with ACPI_Service_Core;
with AML_Delays;
with AML_Execute;
with Firmware_Tables;
procedure Notify_Replay is
   procedure No_Delay (Item : AML_Delays.Request; Result : out AML_Delays.Outcome) is
      pragma Unreferenced (Item);
   begin
      Result := AML_Delays.Unavailable;
   end No_Delay;
   Queue_Capacity : constant Positive := 32;
   Table_Byte_Capacity : constant Positive := 4096;
   Execution_Budget : constant Positive := 100_000;
   package Core is new ACPI_Service_Core
     (Perform_Delay => No_Delay, Notification_Capacity => Queue_Capacity);
   use Core;
   use type Ada.Streams.Stream_Element_Offset;
   use type Ada.Streams.Stream_IO.Count;
   use type Values.Access_Status;
   use type AML_Execute.Execution_Status;
   package IO renames Ada.Streams.Stream_IO;
   type State_Access is access all State;
   Service : constant State_Access := new State (1, Table_Byte_Capacity, Table_Byte_Capacity);
   File : IO.File_Type;
   Loaded : Install_Status;
   Access_Result : Values.Access_Status;
   Report : Namespace.Initialization_Report;
   Test_Node, Marker_Node : Namespace.Node_ID;
   Result : AML_Execute.Execution_Result;
   Marker : Values.Result;
   Item : Notification_Observation;
   Present : Boolean;
   Count : Notification_Count;
   function Trim (Text : String) return String is
     (Ada.Strings.Fixed.Trim (Text, Ada.Strings.Both));
   procedure Require_Access is
   begin
      if Access_Result /= Values.Available then
         raise Program_Error with "service access: " & Access_Result'Image;
      end if;
   end Require_Access;
   function Event_Path return String is
      Text : String (1 .. 1 + Natural (Item.Path_Length) * 5);
      Last : Natural := 1;
   begin
      Text (1) := '\';
      for I in 1 .. Item.Path_Length loop
         if I > 1 then Last := Last + 1; Text (Last) := '.'; end if;
         Text (Last + 1 .. Last + 4) := Item.Path (I);
         Last := Last + 4;
      end loop;
      return Text (1 .. Last);
   end Event_Path;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      raise Program_Error with "usage: notify_replay table.aml";
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
   Configure_Notifications (Service.all, True, Access_Result); Require_Access;
   Find_Child (Service.all, Namespace.Root, "TEST", Test_Node, Access_Result); Require_Access;
   Find_Child (Service.all, Namespace.Root, "MARK", Marker_Node, Access_Result); Require_Access;
   Invoke_Scalar (Service.all, Test_Node, [others => 0], 0, Execution_Budget, Result);
   Observe_Named_Value (Service.all, Marker_Node, Marker, Access_Result); Require_Access;
   if Marker.Status /= AML_Execute.Returned then raise Program_Error with "integer marker required"; end if;
   Count := Pending_Notifications (Service.all);
   Ada.Text_IO.Put_Line ("NOTIFY_REPLAY 1");
   Ada.Text_IO.Put_Line ("STATUS " & Result.Status'Image);
   if Result.Status = AML_Execute.Returned then
      Ada.Text_IO.Put_Line ("RESULT " & Trim (Result.Value'Image));
   else
      Ada.Text_IO.Put_Line ("RESULT NONE");
   end if;
   Ada.Text_IO.Put_Line ("MARKER " & Trim (Marker.Number'Image));
   Ada.Text_IO.Put_Line ("EVENTS " & Trim (Count'Image));
   for I in 1 .. Count loop
      Take_Notification (Service.all, Item, Present, Access_Result); Require_Access;
      if not Present then raise Program_Error with "short drain"; end if;
      Ada.Text_IO.Put_Line ("EVENT " &
        (case Item.Class is when Namespace.System_Notify => "SYSTEM", when Namespace.Device_Notify => "DEVICE") & " " &
        (case Item.Target_Kind is when Namespace.Device_Notification => "DEVICE",
          when Namespace.Processor_Notification => "PROCESSOR", when Namespace.Thermal_Notification => "THERMAL") & " " &
        Event_Path & " " & Trim (Item.Number'Image) & " " &
        (if Item.Current_Target then "CURRENT" else "STALE"));
   end loop;
   Take_Notification (Service.all, Item, Present, Access_Result); Require_Access;
   if Present or else Pending_Notifications (Service.all) /= 0 then raise Program_Error with "extra drain"; end if;
   Ada.Text_IO.Put_Line ("END");
end Notify_Replay;
