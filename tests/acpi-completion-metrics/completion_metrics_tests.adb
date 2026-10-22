pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Requests; use ACPI_Requests;
with ACPI_Service;
with ACPI_Endpoint;
with Migration_Tables;
with Firmware_Tables;
procedure Completion_Metrics_Tests is
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Query (S : in out State; Page : Unsigned_64; A, B, C : Unsigned_64) is
      Prior : constant Revision_Number := Revision (S);
      Before : constant State_Model := Model (S) with Ghost;
      Reply : Response;
      Wire : Packet;
      Config : constant ACPI_Endpoint.Configuration := (Observer_Tag => 10, Provider_Tag => 11);
   begin
      for Origin in Observer .. Snapshot_Provider loop
         Handle (S, Origin, (Label => Read_Metrics, Data => [Page,0,0,0], others => <>), Reply);
         Check (Reply.Status = OK and then Reply.Data = [Prior,A,B,C]);
      end loop;
      for Stamp in Unsigned_64 range 10 .. 11 loop
         ACPI_Endpoint.Dispatch (S, Config, Stamp,
           (Label => Read_Metrics, Data => [Page,0,0,0], others => <>), Wire);
         Check (Wire.Label = ACPI_Endpoint.Reply_OK and then Wire.Length = 4
           and then Wire.Data = [Prior,A,B,C]);
      end loop;
      -- A claimed tag in request data cannot replace the authenticated stamp.
      ACPI_Endpoint.Dispatch (S, Config, 12,
        (Label => Read_Metrics, Data => [Page,10,11,0], others => <>), Wire);
      Check (Wire.Label = ACPI_Endpoint.Reply_Error and then
        Wire.Data = [Unsigned_64 (Outcome'Pos (Denied)),0,0,0]);
      Handle (S, No_Authority, (Label => Read_Metrics, Data => [Page,0,0,0], others => <>), Reply);
      Check (Reply.Status = Denied and then Reply.Data = [0,0,0,0]);
      for Word in 1 .. 3 loop
         declare
            Request : Packet := (Label => Read_Metrics, Data => [Page,0,0,0], others => <>);
         begin
            Request.Data (Word) := 1;
            Handle (S, Observer, Request, Reply);
            Check (Reply.Status = Malformed);
         end;
      end loop;
      Check (Revision (S) = Prior);
      pragma Assert (Model (S) = Before);
   end Query;
   procedure Initial (S : in out State) is
   begin
      -- Not attempted qualifies Wrong_Phase/default Catalog_Rejected/No_Return.
      Query (S, Metrics_Completion_State_Page, 0,1,0);
      Query (S, Metrics_Completion_Access_Page, 0,8,8);
      Query (S, Metrics_Module_State_Page, 0,0,0);
      Query (S, Metrics_Module_Error_Page, 4,0,0);
   end Initial;
   type Scenario is (Successful, Unsupported_Module, Exhausted_Module);
   First : constant Firmware_Tables.Bytes := Migration_Tables.Make ([16#A3#]);
begin
   for Case_ID in Scenario loop
      declare
         S : State (2,512,256,0);
         Reply : Response;
         Prior : Revision_Number;
         -- A real module-level While(One){Noop} consumes the service's unchanged
         -- 100000 fuel budget. No test hook lowers the public startup budget.
         Second : constant Firmware_Tables.Bytes := Migration_Tables.Make
           ((case Case_ID is
              when Successful => [16#A3#],
              when Unsupported_Module => [16#FE#],
              when Exhausted_Module => [16#A2#,3,1,16#A3#]), Secondary => True);
         procedure Finished is
         begin
            Query (S, Metrics_Completion_State_Page,
              (if Case_ID = Successful then 4 else 2),0,2);
            Query (S, Metrics_Completion_Access_Page,2,0,
              (if Case_ID = Successful then 0 else 8));
            Query (S, Metrics_Module_State_Page,
              (if Case_ID = Successful then 1 else 2),
              (if Case_ID = Successful then 2 else 1),
              (if Case_ID = Successful then 0 else 907));
            Query (S, Metrics_Module_Error_Page,
              (case Case_ID is when Successful => 4,
               when Unsupported_Module => 6, when Exhausted_Module => 9),0,0);
         end Finished;
      begin
         Initial (S);
         Handle (S, Snapshot_Provider,
           (Label => Start_Snapshot, Data => [0,2,0,0], others => <>), Reply);
         Check (Reply.Status = OK);
         Initial (S);
         Import_Block (S, Snapshot_Provider, Revision (S), 41, ACPI_Service.DSDT, First, Reply);
         Check (Reply.Status = OK);
         Import_Block (S, Snapshot_Provider, Revision (S), 907, ACPI_Service.SSDT, Second, Reply);
         Check (Reply.Status = OK);
         Initial (S);
         Handle (S, Snapshot_Provider,
           (Label => Finish_Snapshot, Data => [Revision (S),0,0,0], others => <>), Reply);
         Check (Reply.Status = (if Case_ID = Successful then OK else Incomplete));
         Check (Current (S) = (if Case_ID = Successful then Complete else Failed));
         Finished;
         Handle (S, Observer,
           (Label => Read_Metrics, Data => [Metrics_Device_State_Page,0,0,0], others => <>), Reply);
         Check (Reply.Status = OK and then Reply.Data (1) = (if Case_ID = Successful then 6 else 0));
         Handle (S, Observer,
           (Label => Read_Metrics, Data => [Metrics_Startup_Fuel_Page,0,0,0], others => <>), Reply);
         Check (Reply.Status = OK and then Reply.Data (3) > 0);
         if Case_ID = Exhausted_Module then Check (Reply.Data (3) = 100_000); end if;
         Prior := Revision (S);
         Handle (S, Snapshot_Provider,
           (Label => Finish_Snapshot, Data => [Prior,0,0,0], others => <>), Reply);
         Check (Reply.Status = Wrong_Order and then Revision (S) = Prior);
         Finished;
         Handle (S, Observer,
           (Label => Read_Metrics, Data => [Metrics_Last_Page+1,0,0,0], others => <>), Reply);
         Check (Reply.Status = Malformed and then Revision (S) = Prior);
      end;
   end loop;
   declare
      S : State (2,512,256,0);
      Reply : Response;
      Prior : Revision_Number;
   begin
      Handle (S, Snapshot_Provider,
        (Label => Start_Snapshot, Data => [0,2,0,0], others => <>), Reply);
      Check (Reply.Status = OK);
      Import_Block (S, Snapshot_Provider, Revision (S), 41, ACPI_Service.DSDT, First, Reply);
      Check (Reply.Status = OK);
      Handle (S, Snapshot_Provider,
        (Label => Finish_Snapshot, Data => [Revision (S),0,0,0], others => <>), Reply);
      Check (Reply.Status = Incomplete and then Current (S) = Failed);
      -- The session failed before Finish entered its freeze/execution stages.
      -- Default completion fields therefore require page0's overall phase.
      Query (S, 0,3,2,1);
      Initial (S);
      Prior := Revision (S);
      Handle (S, Snapshot_Provider,
        (Label => Finish_Snapshot, Data => [Prior,0,0,0], others => <>), Reply);
      Check (Reply.Status = Wrong_Order and then Revision (S) = Prior);
      Query (S, 0,3,2,1);
      Initial (S);
   end;
   Ada.Text_IO.Put_Line ("Completion metrics checks" & Checks'Image);
end Completion_Metrics_Tests;
