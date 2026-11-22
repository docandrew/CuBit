-- Hosted regression main; physical/kernel boundaries are modeled.
with Ada.Text_IO; with Interfaces; with ACPI_Service; with ACPI_Bootstrap; with ACPI_Requests; with ACPI_Requests.With_Owner_Cleanup; with ACPI_Delay_Domains;
procedure Cleanup_Tests is
 package B renames ACPI_Bootstrap; package R renames ACPI_Requests; package D renames ACPI_Delay_Domains;
 use type D.Phase,D.Tick,Interfaces.Unsigned_64,B.Phase,B.Device_Phase;
 Calls,Checks : Natural:=0;
 procedure Check(OK : Boolean) is begin Checks:=Checks+1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
 procedure Owned_Only(Service : aliased in out ACPI_Service.State; Completed : out Boolean) is
 begin Calls:=Calls+1; Service.Own:=0; Completed:=True; end Owned_Only;
 package Cleanup is new R.With_Owner_Cleanup(Owned_Only);
begin
 for Boot in B.Phase loop for Devices in B.Device_Phase loop
 for Own in 0 .. 1 loop for Other in 0 .. 2 loop for Notifications in 0 .. 3 loop
 declare Server : R.State; Done : Boolean; Before : constant Natural:=Calls; begin
 Server.Boot.Stage:=Boot; Server.Boot.Devices:=Devices;
 Server.Boot.Core:=(Own=>Own,Other=>Other,Notifications=>Notifications);
 Cleanup.Advance(Server,Done);
 Check(Done and Calls=Before+1 and Server.Boot.Core.Own=0);
 Check(Server.Boot.Core.Other=Other and Server.Boot.Core.Notifications=Notifications);
 Check(Server.Boot.Stage=Boot and Server.Boot.Devices=Devices and R.Revision(Server)=5);
 end;
 end loop; end loop; end loop; end loop; end loop;
 for Scenario in 1 .. 6 loop declare S : D.State; Accepted : Boolean; Until_Time : D.Tick; begin
 D.Start(S,10,20,True,Accepted); Check(Accepted);
 if Scenario=6 then
 D.Begin_Wait(S,Until_Time,Accepted); Check(Accepted and D.Working(S));
 D.Inspect_Paused_Startup(S,11,True,Until_Time,Accepted); Check(not Accepted and D.Working(S) and D.Current(S)=D.Stopped);
 D.End_Wait(S,10,11,True,Accepted); Check(not Accepted and not D.Working(S));
 else
 D.Pause_Startup(S,11,True,Accepted); Check(Accepted);
 D.Inspect_Paused_Startup(S,(if Scenario=1 then 11 elsif Scenario=2 then 19 elsif Scenario=3 then 9 else 20),Scenario/=5,Until_Time,Accepted);
 if Scenario in 1 | 2 then
 Check(Accepted and Until_Time=20 and D.Current(S)=D.Startup_Paused);
 D.Inspect_Window(S,19,True,Until_Time,Accepted); Check(not Accepted and Until_Time=0);
 D.Begin_Wait(S,Until_Time,Accepted); Check(not Accepted and not D.Working(S));
 D.Resume_Startup(S,19,True,Accepted); Check(Accepted and D.Deadline(S)=20);
 else Check(not Accepted and Until_Time=0 and D.Current(S)=D.Stopped); end if;
 end if;
 end; end loop;
 Ada.Text_IO.Put_Line("Owner cleanup checks" & Checks'Image);
end Cleanup_Tests;