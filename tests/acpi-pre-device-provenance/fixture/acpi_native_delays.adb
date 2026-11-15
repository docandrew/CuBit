-- Hosted test model only; never a native runtime/provider implementation.
with ACPI_Delay_Domains; with Prep_Model; package body ACPI_Native_Delays is
 package D renames ACPI_Delay_Domains; Domain : D.State;
 procedure Test_Start is Accepted : Boolean; begin D.Start(Domain,0,10000,True,Accepted); if not Accepted then raise Program_Error; end if; end Test_Start;
 procedure Assert_Closed is Until_Time : D.Tick; Accepted : Boolean;
 begin D.Begin_Wait(Domain,Until_Time,Accepted); if Accepted then raise Program_Error with "wait admitted during peer call"; end if;
 D.Inspect_Window(Domain,D.Tick(Prep_Model.Micro_Now),True,Until_Time,Accepted); if Accepted then raise Program_Error with "IO admitted during peer call"; end if;
 end Assert_Closed;
 procedure Pause_Startup(Accepted : out Boolean) is begin Prep_Model.Pauses:=Prep_Model.Pauses+1; D.Pause_Startup(Domain,D.Tick(Prep_Model.Micro_Now),Prep_Model.Mode/=12,Accepted); end Pause_Startup;
 procedure Resume_Startup(Accepted : out Boolean) is begin Prep_Model.Resumes:=Prep_Model.Resumes+1; D.Resume_Startup(Domain,D.Tick(Prep_Model.Micro_Now),True,Accepted); end Resume_Startup;
 procedure Inspect_Paused_Startup(Deadline : out Interfaces.Unsigned_64; Accepted : out Boolean) is T : D.Tick;
 begin D.Inspect_Paused_Startup(Domain,D.Tick(Prep_Model.Micro_Now),True,T,Accepted); Deadline:=Interfaces.Unsigned_64(T); end Inspect_Paused_Startup;
end ACPI_Native_Delays;