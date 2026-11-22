pragma Ada_2022;
with Ada.Command_Line;with Ada.Text_IO;
with AML_Decode;with ACPI_Hosted_Heap;with ACPI_Native_Instance;
with ACPI_One_Service;with ACPI_Native_Power_Buttons;with ACPI_Native_Power_Entry;
with ACPI_Native_Button_Discovery;with Owner_Test_Control;
pragma Elaborate_All(ACPI_Hosted_Heap);
procedure Startup_Clock_Tests is
   package T renames Owner_Test_Control;
   package One renames ACPI_One_Service;
   Checks:Natural:=0;
   Failures,Retirements:Natural:=0;
   procedure Check(Good:Boolean;Label:String) is
   begin Checks:=Checks+1;if not Good then raise Program_Error with Label;end if;end Check;
   use type One.Runtime.Status.Phase;
begin
   Check(Ada.Command_Line.Argument_Count=3,"scenario width logger arguments");
   if Ada.Command_Line.Argument(1)="clock-unavailable" then T.Selected:=T.Clock_Unavailable;
   elsif Ada.Command_Line.Argument(1)="clock-overflow" then T.Selected:=T.Clock_Overflow;
   else raise Program_Error with "unknown clock scenario";end if;
   if Ada.Command_Line.Argument(2)="32" then T.Width:=AML_Decode.Bits_32;
   elsif Ada.Command_Line.Argument(2)="64" then T.Width:=AML_Decode.Bits_64;
   else raise Program_Error with "unknown AML width";end if;
   T.Broken_Critical:=Boolean'Value(Ada.Command_Line.Argument(3));
   ACPI_Native_Instance.Start;
   Check(T.Clock_Reads=1,"one initial startup clock sample");
   Check(T.Loads=0 and T.Copies=0,"unusable clock prevents table load and copy");
   Check(T.Null_Inspections=0 and T.Port_Inspections=0 and T.Hardware_Clock_Reads=0 and T.IPC_Clock_Reads=0 and T.Firmware_Infos=0,
     "early clock failure admits no capability or firmware query");
   Check(T.Native_Reads=0,"early failure requires no native clock or snapshot access");
   Check(T.Detections=0 and T.Power_Detections=0,"early failure prevents discovery");
   Check(T.Registrations=0 and T.Serves=0,"early failure cannot register or serve");
   Check(T.Critical_Attempts=0,"clock failure is a typed outcome, not a fatal AML report");
   Check(One.Runtime.Current=One.Runtime.Status.Stopped,"real runtime closes admission");
   Check(ACPI_Native_Power_Entry.Blocks_Runtime and ACPI_Native_Power_Entry.Quiet,
     "stopped entry remains closed to AML while physically quiet");
   Check(T.Timing_Reports=1,"quiet stopped owner emits final timing exactly once");
   Check(One.Root.Shutdown.May_Retire,"real root confirms retirement");
   Check(One.Root.Hardware.Observe.Registers=0 and One.Root.Hardware.Observe.NVS_Records=0,
     "no hardware or NVS bindings were created");
   Check(One.Root.Fields.Control_Idle and One.Root.Fields.EC_Commands.Quiescent,
     "real Fields and EC owners are idle");
   Check(not One.Root.Fields.Uncertain.Present,"no uncertain physical operation");
   Check(ACPI_Native_Power_Buttons.Quiet and ACPI_Native_Power_Entry.Quiet and
     ACPI_Native_Button_Discovery.Quiet,"all new power owners stop quietly");
   for I in 1..T.Log_Count loop
      declare Text:constant String:=T.Log(I).Text(1..T.Log(I).Length);begin
         if Text="acpi: stage snapshot fail CLOCK_UNAVAILABLE" then Failures:=Failures+1;end if;
         if Text="acpi: stage retire ok QUIESCENT" then Retirements:=Retirements+1;end if;
      end;
   end loop;
   Check(Failures=1 and Retirements=1,"one typed failure and confirmed retirement diagnostic");
   declare
      Before_Log:constant Natural:=T.Log_Count;
      Before_Clock:constant Natural:=T.Clock_Reads;
      Before_Timing:constant Natural:=T.Timing_Reports;
   begin
      ACPI_Native_Instance.Start;
      Check(T.Log_Count=Before_Log and T.Clock_Reads=Before_Clock and T.Timing_Reports=Before_Timing,"second Start is inert after terminal early failure");
      Check(T.Loads=0 and T.Registrations=0 and T.Serves=0,"second Start cannot reopen admission");
   end;
   Ada.Text_IO.Put_Line("Actual root clock checks"&Checks'Image);
end Startup_Clock_Tests;
