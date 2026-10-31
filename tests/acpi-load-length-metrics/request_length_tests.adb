pragma Ada_2022;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Requests; use ACPI_Requests;
with ACPI_Service;
with Firmware_Tables;
procedure Request_Length_Tests is
   subtype Bytes is Firmware_Tables.Bytes;
   use type Ada.Streams.Stream_Element_Offset;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Why; end if;
   end Check;
   function Read (Argument : Positive) return Bytes is
      package IO renames Ada.Streams.Stream_IO;
      File : IO.File_Type;
   begin
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (Argument));
      declare
         Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
         Last : Ada.Streams.Stream_Element_Offset;
         Data : Bytes (1 .. Raw'Length);
      begin
         IO.Read (File, Raw, Last); IO.Close (File);
         Check (Last = Raw'Last, "whole fixture");
         for I in Data'Range loop Data (I) := Unsigned_8 (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Data;
      end;
   end Read;
   Boot : constant Bytes := Read (1);
   Wrapper : constant Bytes := Read (2);
   Expected_Error : constant Revision_Number := Revision_Number'Value (Ada.Command_Line.Argument (3));
   S : State (2, Boot'Length + Wrapper'Length, Positive'Max (Boot'Length, Wrapper'Length), 2, 0);
   Reply, Fuel : Response;
   Prior : Revision_Number;
   procedure Query (Page : Unsigned_64; Expected : Response_Words) is
      Saved_Revision : constant Revision_Number := Revision (S);
      Before : constant State_Model := Model (S) with Ghost;
   begin
      Handle (S, Observer, (Label => Read_Metrics, Data => [Page,0,0,0], others => <>), Reply);
      Check (Reply.Status = OK and then Reply.Data = Expected,
        "metrics page" & Page'Image & " status=" & Reply.Status'Image &
        " data=" & Reply.Data (0)'Image & "," & Reply.Data (1)'Image &
        "," & Reply.Data (2)'Image & "," & Reply.Data (3)'Image);
      Check (Revision (S) = Saved_Revision, "observer revision unchanged");
      pragma Assert (Model (S) = Before, "read-only metric frames owner model");
   end Query;
begin
   Handle (S, Snapshot_Provider, (Label => Start_Snapshot, Data => [0,2,0,0], others => <>), Reply);
   Check (Reply.Status = OK, "start snapshot");
   Import_Block (S, Snapshot_Provider, Revision (S), 11, ACPI_Service.DSDT, Boot, Reply);
   Check (Reply.Status = OK, "import boot");
   Import_Block (S, Snapshot_Provider, Revision (S), 12, ACPI_Service.Description, Wrapper, Reply);
   Check (Reply.Status = OK, "import data table");
   Prior := Revision (S);
   declare Before : constant State_Model := Model (S) with Ghost; begin
      Handle (S, Observer, (Label => Finish_Snapshot, Data => [Prior,0,0,0], others => <>), Reply);
      Check (Reply.Status = Denied and then Revision (S) = Prior, "observer cannot start AML");
      pragma Assert (Model (S) = Before);
   end;
   Handle (S, Snapshot_Provider, (Label => Finish_Snapshot, Data => [Prior,0,0,0], others => <>), Reply);
   Check (Reply.Status = OK and then Revision (S) = Prior + 1 and then Current (S) = Complete,
     "startup completes with a report " & Reply.Status'Image & " " & Current (S)'Image);
   Query (Metrics_Device_Error_Page, [Revision (S), Expected_Error, 0, 0]);
   Query (Metrics_Device_State_Page,
     [Revision (S),
      Unsigned_64 (Device_Phase_Code'Enum_Rep
        ((if Expected_Error = 4 then Device_Completed_Code else Device_Completed_With_Errors_Code))),
      Unsigned_64 (Region_Status_Code'Enum_Rep (No_Regions_Required_Code)),
      (if Expected_Error = 4 then 0 else 1)]);
   Query (Metrics_Device_Calls_Page, [Revision (S),0,2,0]);
   -- Completed module import leaves its failure field at No_Return (wire 4).
   Query (Metrics_Module_Error_Page, [Revision (S),4,0,0]);
   Handle (S, Observer, (Label => Read_Metrics, Data => [Metrics_Startup_Fuel_Page,0,0,0], others => <>), Fuel);
   Check (Fuel.Status = OK and then Fuel.Data (1) = Fuel.Data (2) + Fuel.Data (3)
     and then Fuel.Data (2) > 0 and then Fuel.Data (3) > 0, "startup work is reported");
   Prior := Revision (S);
   declare Before : constant State_Model := Model (S) with Ghost; begin
      Handle (S, No_Authority, (Label => Read_Metrics, Data => [Metrics_Device_Error_Page,0,0,0], others => <>), Reply);
      Check (Reply.Status = Denied and then Reply.Data = [0,0,0,0] and then Revision (S) = Prior,
        "unprivileged metrics denied and zeroed");
      pragma Assert (Model (S) = Before);
   end;
   Handle (S, Snapshot_Provider, (Label => Finish_Snapshot, Data => [Prior,0,0,0], others => <>), Reply);
   Check (Reply.Status = Wrong_Order and then Revision (S) = Prior, "startup cannot replay");
   Query (Metrics_Device_Error_Page, [Revision (S),Expected_Error,0,0]);
   Query (Metrics_Startup_Fuel_Page, Fuel.Data);
   Check (Valid (S), "valid final owner");
   Ada.Text_IO.Put_Line ("Request length checks" & Checks'Image);
end Request_Length_Tests;
