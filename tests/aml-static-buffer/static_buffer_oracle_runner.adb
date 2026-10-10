with AML_Delays;
with Ada.Command_Line;
with Ada.Real_Time;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with ACPI_Service_Core;
with AML_Decode;
with AML_Execute;
with Firmware_Tables;
procedure Static_Buffer_Oracle_Runner is
   use type Ada.Real_Time.Time;
   use type Ada.Real_Time.Time_Span;
   use type Ada.Streams.Stream_Element_Offset;
   use type AML_Execute.Execution_Status;
   package IO renames Ada.Streams.Stream_IO;
   use type IO.Count;
   Started : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
   Max_Hosted_Seconds : constant := 180;
   Execution_Budget : constant := 100_000;
   procedure Read_Host_Microseconds
     (Value : out AML_Decode.Integer_Value; Available : out Boolean) is
      Elapsed : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Clock - Started;
   begin
      Value := 0; Available := False;
      if Elapsed < Ada.Real_Time.Time_Span_Zero
        or else Elapsed > Ada.Real_Time.Seconds (Max_Hosted_Seconds)
      then return; end if;
      Value := AML_Decode.Integer_Value (Elapsed / Ada.Real_Time.Microseconds (1));
      Available := True;
   end Read_Host_Microseconds;
   package Core is new ACPI_Service_Core (AML_Delays.Unavailable_Provider, Read_Host_Microseconds);
   use Core;
   use type Values.Access_Status;
   function Read_Input return Firmware_Tables.Bytes is
      File : IO.File_Type;
   begin
      if Ada.Command_Line.Argument_Count /= 1 then
         raise Program_Error with "usage: static_buffer_oracle_runner table.aml";
      end if;
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
      declare
         Size : constant IO.Count := IO.Size (File);
      begin
         if Size < IO.Count (Firmware_Tables.Table_Header_Size)
           or else Size > IO.Count (Max_Total_Bytes)
         then IO.Close (File); raise Program_Error with "input outside hosted bound"; end if;
         declare
            Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Size));
            Last : Ada.Streams.Stream_Element_Offset;
            Data : Firmware_Tables.Bytes (1 .. Natural (Size));
         begin
            IO.Read (File, Raw, Last); IO.Close (File);
            if Last /= Raw'Last then raise Program_Error with "short read"; end if;
            for I in Data'Range loop
               Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I)));
            end loop;
            return Data;
         end;
      end;
   end Read_Input;
   Table_Data : constant Firmware_Tables.Bytes := Read_Input;
   Service : aliased State (Max_Tables, Max_Total_Bytes, Table_Data'Length);
   Installed_Status : Install_Status;
   Access_Result : Values.Access_Status;
   Report : Namespace.Initialization_Report;
   Node : Namespace.Node_ID;
   Result : AML_Execute.Execution_Result;
begin
   Install (Service, 1, DSDT, Table_Data, Installed_Status);
   if Installed_Status /= Installed then
      declare
         Code : constant Natural := Observe (Service).Last_Load_Code;
      begin
         if Code > 0 and then Code <= Namespace.Load_Status'Pos (Namespace.Load_Status'Last) + 1 then
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
              "LOAD_STATUS " & Namespace.Load_Status'Val (Code - 1)'Image);
         end if;
      end;
      raise Program_Error with "install: " & Installed_Status'Image;
   end if;
   Initialize_Members (Service, Report, Access_Result);
   if Access_Result /= Values.Available then
      raise Program_Error with "seal: " & Access_Result'Image;
   end if;
   Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
     "INITIALIZATION BOUND" & Report.Bound'Image & " MISSING" & Report.Missing'Image &
     " UNSUPPORTED" & Report.Unsupported'Image);
   Find_Child (Service, Namespace.Root, "TEST", Node, Access_Result);
   if Access_Result /= Values.Available or else Node = Namespace.Root then
      raise Program_Error with "missing TEST";
   end if;
   Invoke_Scalar (Service, Node, [others => 0], 0, Execution_Budget, Result);
   Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
     "EXECUTION_STATUS " & Result.Status'Image & " CHARGED" & Result.Charged'Image);
   if Result.Status /= AML_Execute.Returned then
      raise Program_Error with "execute: " & Result.Status'Image;
   end if;
   Ada.Text_IO.Put_Line ("RESULT" & Result.Value'Image);
end Static_Buffer_Oracle_Runner;
