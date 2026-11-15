pragma Ada_2022;
with CuBit.Messages;with Ada.Strings.Fixed;with ACPI_Native_Delays;with ACPI_Module_Diagnostics;
with Ada.Command_Line;with Ada.Text_IO;with Ada.Streams.Stream_IO;
with Startup_Test_Firmware;with ACPI_FADT;with Interfaces;
with AML_Decode;with ACPI_Hosted_Heap;with ACPI_Native_Instance;
with ACPI_One_Service;with ACPI_Snapshot_Startup;with Owner_Test_Control;
with ACPI_Native_Power_Entry;with ACPI_Native_Power_Buttons;with ACPI_Native_Button_Discovery;
pragma Elaborate_All(ACPI_Hosted_Heap);
procedure Startup_Firmware_Tests is
   package T renames Owner_Test_Control;
   package One renames ACPI_One_Service;
   package L renames ACPI_Snapshot_Startup;
   Checks:Natural:=0;
   EC_Lines,Device_Lines,Stop_Lines,Serve_Failures,Retire_Lines:Natural:=0;
   use type T.Level,ACPI_Module_Diagnostics.Count,L.Count,L.Outcome,L.R.Tick,T.Scenario,One.Runtime.Status.Phase;
   use type One.Root.Evaluation.Startup.Reason;
   use type Interfaces.Unsigned_32;
   procedure Dump(Prefix:String) is
      package IO renames Ada.Streams.Stream_IO;
   begin
      for ID in 1..Startup_Test_Firmware.Table_Count loop
         declare
            Raw:constant Startup_Test_Firmware.Bytes:=Startup_Test_Firmware.Image(ID);
            Data:Ada.Streams.Stream_Element_Array(1..Ada.Streams.Stream_Element_Offset(Raw'Length));
            File:IO.File_Type;
         begin
            for I in Raw'Range loop Data(Ada.Streams.Stream_Element_Offset(I-Raw'First+1)):=Ada.Streams.Stream_Element(Raw(I));end loop;
            IO.Create(File,IO.Out_File,Prefix&(if ID=Startup_Test_Firmware.DSDT_ID then ".dsdt" else ".fadt"));
            IO.Write(File,Data);IO.Close(File);
         end;
      end loop;
   end Dump;
   procedure Logs is
   begin
      for I in 1..T.Log_Count loop Ada.Text_IO.Put_Line(T.Log(I).Text(1..T.Log(I).Length));end loop;
   end Logs;
   procedure Check(Good:Boolean;Label:String) is
   begin Checks:=Checks+1;if not Good then Logs;raise Program_Error with Label;end if;end Check;
begin
   Check(Ada.Command_Line.Argument_Count in 3..4,"scenario width logger arguments");
   T.Selected:=T.Scenario'Value(Ada.Command_Line.Argument(1));
   if Ada.Command_Line.Argument(2)="32" then T.Width:=AML_Decode.Bits_32;
   elsif Ada.Command_Line.Argument(2)="64" then T.Width:=AML_Decode.Bits_64;
   else raise Program_Error with "unknown AML width";end if;
   T.Broken_Critical:=Boolean'Value(Ada.Command_Line.Argument(3));
   if Ada.Command_Line.Argument_Count=4 then Dump(Ada.Command_Line.Argument(4));end if;
   declare FADT:constant ACPI_FADT.Result:=ACPI_FADT.Decode(Startup_Test_Firmware.Image(Startup_Test_Firmware.FADT_ID));begin
      Check(FADT.Valid,"fixture FADT structurally admitted by actual parser");
      Check(ACPI_FADT.Hardware_Reduced(FADT.Value),"fixture explicitly hardware reduced");
      Check(FADT.Value.FACS=0 and FADT.Value.Sleep_Control.Present and FADT.Value.Sleep_Control.Value.Address=0,
        "fixture supplies no FACS or usable sleep-control register");
   end;
   begin ACPI_Native_Instance.Start;exception when others=>Logs;raise;end;
   Check(T.Loads=1,"one snapshot startup");
   Check(T.Last_Load.Status=L.Loaded,"actual root admits complete startup");
   Check(T.Passed_Deadline=60_000_010,"finite original microsecond deadline");
   Check(T.Last_Load.Catalog.Total_Bytes=L.Count(Startup_Test_Firmware.Image(Startup_Test_Firmware.DSDT_ID)'Length+Startup_Test_Firmware.Image(Startup_Test_Firmware.FADT_ID)'Length)
     and T.Last_Load.Catalog.Largest_Table=L.Count(Startup_Test_Firmware.Image(Startup_Test_Firmware.FADT_ID)'Length),"complete catalog totals and largest extent");
   Check(T.Last_Load.Catalog.Tables=2,"one DSDT and one FADT");
   Check(One.Root.Evaluation.Observe.FADT_Index=2,"real root consumed copied FADT");
   Check(One.Root.Evaluation.Observe.Status=One.Root.Evaluation.Startup.Modules_Completed,
     "real module startup completed");
   Check(T.Port_Inspections>0,"real hardware owner inspected exact modeled port capability");
   Check(T.Hardware_Clock_Reads>0 and T.IPC_Clock_Reads>0,"independent native and IPC clock sources sampled");
   Check(T.Detections=1 and T.Power_Detections=1,"HID and power discovery follow admitted startup");
   Check(T.Detected_At>T.Load_Completed_At and T.Power_Detected_At>T.Detected_At,"discovery order");
   Check(T.Registered_Role=CuBit.Messages.DRIVER_ACPI and T.Registration_Result=(if T.Selected=T.Register_Refused then Interfaces.Unsigned_64'Last else CuBit.Messages.To_Word(CuBit.Messages.Own_Process)),"named ACPI role and full opaque registration result");
   Check(T.Registrations=1,"one registration after discovery");
   Check(T.Serves=Boolean'Pos(T.Selected/=T.Register_Refused),"only registered owner enters serve");
   Check(One.Runtime.Current=One.Runtime.Status.Stopped,"real runtime closes admission");
   Check(One.Root.Shutdown.May_Retire,"real root confirms retirement");
   Check(ACPI_Native_Power_Entry.Quiet and ACPI_Native_Power_Buttons.Quiet and ACPI_Native_Button_Discovery.Quiet,
     "power owners quiet on stop");
   Check(T.Null_Inspections=Boolean'Pos(T.Selected/=T.Register_Refused),"absent completion endpoint queried only after successful registration");
   Check(T.Timing_Reports=1,"timing finalized once");
   declare Deadline:Interfaces.Unsigned_64;Accepted:Boolean;begin
      ACPI_Native_Delays.Inspect_Window(Deadline,Accepted);
      Check(not Accepted and Deadline=0,"real delay domain closed after typed return");
   end;
   for I in 1..T.Log_Count loop
      declare Text:constant String:=T.Log(I).Text(1..T.Log(I).Length);begin
         if Ada.Strings.Fixed.Index(Text,"acpi: stage ec-boot ")=1 then
            EC_Lines:=EC_Lines+1;
            Check(T.Log(I).Severity=T.Information and Device_Lines=0 and Ada.Strings.Fixed.Index(Text,"MISSING_ECDT")>0,"bounded EC observation precedes devices");
         elsif Ada.Strings.Fixed.Index(Text,"acpi: stage devices ")=1 then
            Device_Lines:=Device_Lines+1;
            Check(T.Log(I).Severity=T.Information and Text'Length<=256 and Ada.Strings.Fixed.Index(Text,"DEVICES_COMPLETED")>0,"successful device summary is bounded and informational");
         elsif Text="acpi: stage serve ok STOP_REQUESTED" then
            Stop_Lines:=Stop_Lines+1;Check(T.Log(I).Severity=T.Information,"Stop remains informational");
         elsif Ada.Strings.Fixed.Index(Text,"acpi: stage serve fail ")=1 then
            Serve_Failures:=Serve_Failures+1;Check(T.Log(I).Severity=T.Warning and Text="acpi: stage serve fail WAIT_UNAVAILABLE","abnormal modeled server exit is reported once as Warning");
         elsif Text="acpi: stage retire ok QUIESCENT" then
            Retire_Lines:=Retire_Lines+1;Check(T.Log(I).Severity=T.Information,"confirmed retirement is informational");
         end if;
      end;
   end loop;
   Check(EC_Lines=1 and Device_Lines=1,"exactly one EC and device outcome");
   Check(Stop_Lines=Boolean'Pos(T.Selected=T.Stop_Request) and Serve_Failures=Boolean'Pos(T.Selected=T.Success),"exact typed server disposition");
   Check(Retire_Lines=1,"exactly one confirmed retirement record");
   Check(T.Critical_Attempts=0 and not T.Last_Load.Fatal.Present and T.Recovery_Attempts=0 and T.Last_Load.Module_Errors.Count=0,"no invented fatal or recovery report");
   Check(T.Registered_At>T.Power_Detected_At and (if T.Selected=T.Register_Refused then T.Served_At=0 else T.Served_At>T.Registered_At),"registration and serve ordering preserved");
   declare N:constant Natural:=T.Log_Count;Clocks:constant Natural:=T.Clock_Reads;begin
      ACPI_Native_Instance.Start;
      Check(T.Log_Count=N and T.Clock_Reads=Clocks and T.Loads=1 and T.Registrations=1,
        "second Start cannot replay startup");
   end;
   Ada.Text_IO.Put_Line("Actual root firmware checks"&Checks'Image);
end Startup_Firmware_Tests;
