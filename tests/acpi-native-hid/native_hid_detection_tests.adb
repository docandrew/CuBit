pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_Service;
with ACPI_Requests;
with ACPI_Native_HID_Detection;
with Native_HID_Test_Log;
with Firmware_Tables;
with Interfaces;
with HID_I2C_Descriptors;
procedure Native_HID_Detection_Tests is
   package C renames ACPI_Service;
   package Q renames ACPI_Requests;
   package D renames ACPI_Native_HID_Detection;
   package IO renames Ada.Streams.Stream_IO;
   use type Ada.Streams.Stream_Element_Offset, Q.Outcome, Q.Phase, Q.Revision_Number;
   use type D.Outcome, D.Report, D.Channels.Outcome;
   use type C.Node_Descriptor;
   use type HID_I2C_Descriptors.Register_Number;
   use type D.Channels.I.Address_Value, D.Channels.I.Hertz;
   use type D.Channels.G.Pin_Number, D.Channels.G.Trigger_Mode, D.Channels.G.Polarity;
   Checks : Natural := 0;
   Mode : constant String := Ada.Command_Line.Argument (2);
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   function Read_Table return Firmware_Tables.Bytes is
      File : IO.File_Type;
   begin
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
      Check (IO.Size (File) in 36 .. 4096, "bounded fixture");
      declare
         Bytes : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
         Last : Ada.Streams.Stream_Element_Offset;
         Data : Firmware_Tables.Bytes (1 .. Bytes'Length);
      begin
         IO.Read (File, Bytes, Last); IO.Close (File); Check (Last = Bytes'Last, "whole fixture");
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Bytes (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Data;
      end;
   end Read_Table;
   Table : constant Firmware_Tables.Bytes := Read_Table;
   Server, Foreign_Server : Q.State (1, Table'Length, Table'Length, 1, 0);
   Reply : Q.Response;
   Limits : D.Policy;
   Result, Again : D.Report;
   Expected : D.Outcome := D.Finished;
   Raised : Boolean := False;
   procedure Send (Label : Interfaces.Unsigned_32; Value : Q.Revision_Number := 0) is
   begin
      Q.Handle (Server, Q.Snapshot_Provider,
        (Label => Label, Data => [Q.Revision (Server), Value, 0, 0], others => <>), Reply);
      Check (Reply.Status = Q.OK, "snapshot request");
   end Send;
begin
   if Mode /= "unstarted" then
      Send (Q.Start_Snapshot, 1);
      Q.Import_Block (Server, Q.Snapshot_Provider, Q.Revision (Server), 1, C.DSDT, Table, Reply);
      Check (Reply.Status = Q.OK, "real AML admitted"); Send (Q.Finish_Snapshot);
      Check (Q.Current (Server) = Q.Complete, "complete snapshot");
   end if;
   if Mode in "channel-role" | "partial-channel" then Expected := D.Channel_Refused;
   elsif Mode = "log-error" then Expected := D.Owner_Error;
   elsif Mode in "fatal-cid" | "fatal-sta" | "fatal-crs" | "fatal-dsm" then Expected := D.Recovery_Required;
   elsif Mode in "hardware" | "unstarted" then Expected := D.Not_Ready;
   elsif Mode = "empty" then Expected := D.No_Devices;
   elsif Mode = "missing" then Expected := D.Sources_Failed;
   elsif Mode = "small" then Limits.Resource_Bytes := 1; Expected := D.Capacity_Refused;
   elsif Mode = "zero" then Limits.Fuel := 0; Expected := D.Budget_Exhausted;
   elsif Mode = "fuel" then Limits.Fuel := 1; Expected := D.Budget_Exhausted;
   elsif Mode = "limit" then Limits.Candidates := 1; Expected := D.Candidate_Limit;
   elsif Mode = "full" then
      Limits.Candidates := D.Maximum_Candidates; Limits.Fuel := D.Maximum_AML_Fuel;
      Expected := D.Candidate_Limit;
   end if;
   declare Before : constant Q.Revision_Number := Q.Revision (Server); begin
      begin
         D.Run (Server, Result, Limits);
      exception
         when Program_Error =>
            if Mode /= "log-error" then raise; end if;
            Raised := True;
      end;
      Check (Raised = (Mode = "log-error"), "only modeled logger exception escapes");
      if Raised then D.Run (Server, Result, Limits); end if;
      Check (Result.Status = Expected, "typed native discovery outcome " & Result.Status'Image);
      Check (Q.Valid (Server) and then Q.Revision (Server) = Before, "service invariant and observation revision");
   end;
   Check (Result.Charged <= Limits.Fuel, "one total fuel budget");
   Check (Result.Recovery_Needed = (Mode in "fatal-cid" | "fatal-sta" | "fatal-crs" | "fatal-dsm"),
     "fatal recovery required independently of retained values");
   Check (not Result.Cleanup_Required, "retained CRS released on supported success/refusal paths");
   if Mode in "normal" | "budget" then
      Check (Result.Candidates = 2 and then Result.Observed = 2, "both HID devices discovered past controllers");
   elsif Mode = "limit" then
      Check (Result.Candidates = 1 and then Result.Observed = 1, "explicit candidate budget stops enumeration");
   elsif Mode = "full" then
      Check (Result.Candidates = D.Maximum_Candidates and then Result.Observed = D.Maximum_Candidates,
        "last bounded storage row is complete and enumeration stops without overflow");
   elsif Mode = "partial-channel" then
      Check (Result.Candidates = 2 and then Result.Observed = 1,
        "later channel refusal retains only earlier complete observation");
   else Check (Result.Observed = 0, "no partial resource success"); end if;
   if Mode in "channel-role" | "partial-channel" then
      Check (Result.Channel_Attempted and then
        Result.Last_Channel_Status = D.Channels.Unsupported_I2C_Role,
        "native channel refusal retains typed cause");
   end if;
   for I in 1 .. Result.Observed loop
      declare Row : constant D.Observation := Result.Items (I); begin
         Check (Row.Channel_Data.Status = D.Channels.Prepared, "only complete channel rows exposed");
         Check (Row.Device /= C.Values.No_Node and then
           C.Values.Node_Name (Row.I2C_Controller) = "I2C0" and then
           C.Values.Node_Name (Row.GPIO_Controller) = "GPIO",
           "original resolved controller identities retained");
         Check (Row.Channel_Data.I2C.Address = 16#15# and then
           Row.Channel_Data.I2C.Maximum_Hz = 400_000 and then
           Row.Channel_Data.GPIO.Pin = 16#2A# and then
           Row.Channel_Data.GPIO.Trigger = D.Channels.G.Level_Triggered and then
           Row.Channel_Data.GPIO.Active = D.Channels.G.Active_Low,
           "native copied row preserves exact channel parameters");
         Check (Row.Channel_Data.Facts.Descriptor_Register = 16#20#,
           "native row preserves observed descriptor register");
      end;
   end loop;
   for I in Natural (Result.Observed) + 1 .. D.Maximum_Candidates loop
      Check (Result.Items (I).Device = C.Values.No_Node and then
        Result.Items (I).I2C_Controller = C.Values.No_Node and then
        Result.Items (I).GPIO_Controller = C.Values.No_Node and then
        Result.Items (I).Channel_Data.Status /= D.Channels.Prepared,
        "unused and refused rows expose no partial plan");
   end loop;
   Check (Native_HID_Test_Log.Facts = Result.Observed, "one fact line per current observation");
   Check (Native_HID_Test_Log.Summaries = Boolean'Pos (Mode /= "log-error"), "one summary unless logger failed");
   Check (Native_HID_Test_Log.Warnings = Boolean'Pos (Mode /= "log-error" and then Expected not in D.Finished | D.No_Devices), "summary severity follows outcome");
   declare Logs : constant Natural := Native_HID_Test_Log.Total; begin
      D.Run (Foreign_Server, Again, (Fuel => 0, Resource_Bytes => 0, Candidates => 1));
      Check (Again = Result and then Native_HID_Test_Log.Total = Logs,
        "terminal process latch preserves outcome without AML/log replay on another owner");
      Check (Q.Current (Foreign_Server) = Q.Idle and then Q.Revision (Foreign_Server) = 0,
        "repeat cannot touch foreign Core");
   end;
   Ada.Text_IO.Put_Line ("Native HID checks" & Checks'Image);
end Native_HID_Detection_Tests;
