pragma Ada_2022;
with Ada.Command_Line;with Ada.Exceptions;with Ada.Text_IO;with Ada.Strings.Fixed;
with Interfaces;with AML_Decode;with ACPI_Hosted_Heap;with ACPI_Native_Instance;
with ACPI_One_Service;with ACPI_Native_Delays;with ACPI_Snapshot_Startup;
with Owner_Test_Control;with ACPI_Native_Power_Entry;with ACPI_Native_Power_Buttons;
with ACPI_Native_Button_Discovery;
pragma Elaborate_All(ACPI_Hosted_Heap);
procedure Startup_Failure_Tests is
   package T renames Owner_Test_Control;package One renames ACPI_One_Service;
   package L renames ACPI_Snapshot_Startup;
   use type T.Scenario,T.Level,L.Outcome,L.R.Tick,One.Runtime.Status.Phase;
   use type Interfaces.Unsigned_64;
   Checks:Natural:=0;
   Fault_Seen:Boolean:=False;
   Snapshot_Lines,Retire_Lines,Device_Lines:Natural:=0;
   procedure Logs is
   begin for I in 1..T.Log_Count loop Ada.Text_IO.Put_Line(T.Log(I).Text(1..T.Log(I).Length));end loop;end Logs;
   procedure Check(Good:Boolean;Label:String) is
   begin Checks:=Checks+1;if not Good then Logs;raise Program_Error with Label;end if;end Check;
begin
   Check(Ada.Command_Line.Argument_Count=3,"scenario width logger arguments");
   T.Selected:=T.Scenario'Value(Ada.Command_Line.Argument(1));
   if T.Selected not in T.Snapshot_Refused|T.Copy_Refused|T.Load_Exception then
      raise Program_Error with "unsupported refusal scenario";
   end if;
   if Ada.Command_Line.Argument(2)="32" then T.Width:=AML_Decode.Bits_32;
   elsif Ada.Command_Line.Argument(2)="64" then T.Width:=AML_Decode.Bits_64;
   else raise Program_Error with "unknown AML width";end if;
   T.Broken_Critical:=Boolean'Value(Ada.Command_Line.Argument(3));
   begin ACPI_Native_Instance.Start;
   exception when E:Program_Error=>
      Fault_Seen:=True;
      Check(T.Selected=T.Load_Exception and Ada.Exceptions.Exception_Message(E)="modeled loader exception",
        "only the exact modeled trusted-boundary exception propagates");
   end;
   Check(Fault_Seen=(T.Selected=T.Load_Exception),"typed refusal versus terminal hosted contract exception");
   Check(T.Loads=1,"one controlled load attempt");
   Check(T.Passed_Deadline=60_000_010,"original finite startup deadline retained");
   Check(T.Copies=Boolean'Pos(T.Selected=T.Copy_Refused),"catalog refusal and exception do not copy bytes");
   Check(T.Detections=0 and T.Power_Detections=0,"failed snapshot cannot reach discovery");
   Check(T.Registrations=0 and T.Serves=0,"failed snapshot cannot register or serve");
   Check(T.Null_Inspections=0,"no consumer completion after snapshot failure");
   Check(T.Critical_Attempts=0 and T.Recovery_Attempts=0,"copy/source errors do not invent AML Fatal or recovery");
   Check(One.Root.Hardware.Observe.Registers=0 and One.Root.Hardware.Observe.NVS_Records=0,
     "no hardware or NVS bindings precede admitted tables");
   Check(T.Port_Inspections=0,"hardware setup cannot inspect port grant before table admission");
   if T.Selected/=T.Load_Exception then
      Check(T.Last_Load.Status=(if T.Selected=T.Snapshot_Refused then L.Source_Refused else L.Copy_Refused),
        "exact typed snapshot failure survives cleanup");
      Check(not T.Last_Load.Fatal.Present and not T.Last_Load.Owner_Retained,"failed copy publishes no retained AML owner");
      Check(One.Runtime.Current=One.Runtime.Status.Stopped and One.Root.Shutdown.May_Retire,
        "typed source/copy refusal closes and retires real root");
      Check(ACPI_Native_Power_Entry.Quiet and ACPI_Native_Power_Buttons.Quiet and ACPI_Native_Button_Discovery.Quiet,
        "power owners quiet on typed startup refusal");
      Check(T.Timing_Reports=1,"typed refusal finalizes timing once");
   else
      Check(One.Runtime.Current=One.Runtime.Status.Fresh and not One.Root.Shutdown.May_Retire,
        "hosted propagation does not invent native retirement");
      Check(T.Timing_Reports=0,"terminal boundary exception does not run normal finalization");
   end if;
   for I in 1..T.Log_Count loop
      declare Text:constant String:=T.Log(I).Text(1..T.Log(I).Length);begin
         if Ada.Strings.Fixed.Index(Text,"acpi: stage snapshot fail ")=1 then
            Snapshot_Lines:=Snapshot_Lines+1;Check(T.Log(I).Severity=T.Warning,"typed snapshot refusal has Warning severity");
         elsif Text="acpi: stage retire ok QUIESCENT" then Retire_Lines:=Retire_Lines+1;
         elsif Ada.Strings.Fixed.Index(Text,"acpi: stage devices ")=1 then Device_Lines:=Device_Lines+1;end if;
      end;
   end loop;
   Check(Snapshot_Lines=Boolean'Pos(T.Selected/=T.Load_Exception) and Retire_Lines=Boolean'Pos(T.Selected/=T.Load_Exception)
     and Device_Lines=0,"one typed outcome and retirement only after normal return");
   declare Until_Time:Interfaces.Unsigned_64;Accepted:Boolean;begin
      ACPI_Native_Delays.Inspect_Window(Until_Time,Accepted);
      Check(Accepted=(T.Selected=T.Load_Exception) and Until_Time=(if Accepted then 60_000_010 else 0),
        "actual delay domain closes only on normal native return");
   end;
   declare
      Before_Clocks:constant Natural:=T.Clock_Reads;
      Before_Logs:constant Natural:=T.Log_Count;
      Before_Copies:constant Natural:=T.Copies;
   begin
      ACPI_Native_Instance.Start;
      Check(T.Loads=1 and T.Clock_Reads=Before_Clocks and T.Log_Count=Before_Logs and T.Copies=Before_Copies
        and T.Registrations=0 and T.Serves=0,"second Start cannot replay refused or terminal startup");
   end;
   Ada.Text_IO.Put_Line("Actual root refusal checks"&Checks'Image);
end Startup_Failure_Tests;
