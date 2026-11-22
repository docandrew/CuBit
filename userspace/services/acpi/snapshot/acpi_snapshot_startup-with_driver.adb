pragma Ada_2022;
with ACPI_Requests.With_Driver;
package body ACPI_Snapshot_Startup.With_Driver is
   use type ACPI_Requests.Outcome;
   package Controller is new ACPI_Requests.With_Driver (Device_Driver, Execute_Module_Startup, Initialize_Member_Startup);
   function Consumed (Context : Session) return Boolean is (Context.Attempt.Started);
   procedure Load (Context : in out Session; Run : in out Device_Driver.Session;
      Deadline : R.Tick; Owner : out State_Access; Result : out Report) is
      procedure Complete
        (Server : in out ACPI_Requests.State; Reply : out ACPI_Requests.Response) is
      begin
         -- Request capture through the SAME Bootstrap/session before Finish:
         -- it enables the actual Core before handler/device _INI execution.
         -- Direct Core flag changes or a late serving-only enable would lose
         -- startup Notify or disagree with the broker's readiness policy.
         ACPI_Requests.Handle(Server,ACPI_Requests.Snapshot_Provider,
           (Label=>ACPI_Requests.Configure_Notifications,
            Data=>[ACPI_Requests.Revision(Server),1,0,0],others=><>),Reply);
         if Reply.Status/=ACPI_Requests.OK then return;end if;
         Controller.Handle (Server, Run, ACPI_Requests.Snapshot_Provider,
           (Label => ACPI_Requests.Finish_Snapshot,
            Data => [ACPI_Requests.Revision (Server), 0, 0, 0], others => <>), Reply);
      end Complete;
      procedure Import_And_Complete is new Load_Using
        (Info, Describe, Chunk_Max, Read_Part, Clock, Complete, True);
   begin
      if Context.Attempt.Started then
         Owner := Context.Attempt.Owner; Result := Context.Attempt.Last;
         Result.Status := Startup_Already_Consumed;
         Result.Owner_Retained := Owner /= null;
         return;
      end if;
      Context.Attempt.Started := True;
      if Device_Driver.Startup_Claimed (Run) then
         Context.Attempt.Last.Status := Startup_Already_Consumed;
      else
         Import_And_Complete (Deadline, Context.Attempt.Owner, Context.Attempt.Last);
      end if;
      Owner := Context.Attempt.Owner;
      Result := Context.Attempt.Last;
   end Load;
end ACPI_Snapshot_Startup.With_Driver;
