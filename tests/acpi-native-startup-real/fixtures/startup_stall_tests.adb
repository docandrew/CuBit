pragma Ada_2022;
with Ada.Command_Line;with Ada.Text_IO;
with AML_Decode;with ACPI_Hosted_Heap;with ACPI_Native_Instance;
with ACPI_One_Service;with ACPI_Snapshot_Startup;with Owner_Test_Control;
pragma Elaborate_All(ACPI_Hosted_Heap);
procedure Startup_Stall_Tests is
   package T renames Owner_Test_Control;package One renames ACPI_One_Service;
   Checks:Natural:=0;
   use type ACPI_Snapshot_Startup.Outcome,One.Runtime.Status.Phase;
   procedure Check(Good:Boolean;Label:String) is
   begin
      Checks:=Checks+1;
      if not Good then
         for I in 1..T.Log_Count loop Ada.Text_IO.Put_Line(T.Log(I).Text(1..T.Log(I).Length));end loop;
         raise Program_Error with Label;
      end if;
   end Check;
begin
   Check(Ada.Command_Line.Argument_Count=1,"width argument");
   T.Selected:=T.Stalled_Initializer;
   if Ada.Command_Line.Argument(1)="32" then T.Width:=AML_Decode.Bits_32;
   elsif Ada.Command_Line.Argument(1)="64" then T.Width:=AML_Decode.Bits_64;
   else raise Program_Error with "unknown AML width";end if;
   ACPI_Native_Instance.Start;
   Check(T.Last_Load.Status=ACPI_Snapshot_Startup.Loaded,"catalog retained after bounded initializer wait failure");
   Check(T.Stall_Entries=1 and T.Sleep_Entries=0,"real native Stall callback executes once");
   Check(T.Clock_Reads>=65_536 and T.Clock_Reads<66_000,"frozen clock consumes finite observation budget");
   Check(T.Critical_Attempts=0 and T.Recovery_Attempts=0 and not T.Last_Load.Fatal.Present,"wait failure remains typed");
   Check(T.Registrations=0 and T.Serves=0,"closed delay domain cannot transfer to runtime");
   Check(One.Runtime.Current=One.Runtime.Status.Stopped and One.Root.Shutdown.May_Retire,"failed wait retires real physical owner");
   Check(T.Timing_Reports=1,"typed wait failure finalizes timing once");
   declare N:constant Natural:=T.Clock_Reads;begin
      ACPI_Native_Instance.Start;Check(T.Clock_Reads=N and T.Stall_Entries=1 and T.Loads=1,"second Start cannot replay stalled initializer");
   end;
   Ada.Text_IO.Put_Line("Native frozen-clock Stall checks"&Checks'Image);
end Startup_Stall_Tests;
