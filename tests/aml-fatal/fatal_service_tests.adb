with Ada.Text_IO;
with Interfaces; use Interfaces;
with AML_Decode;
with AML_Execute;
with AML_Fatal;
with ACPI_Bootstrap;
with ACPI_Fatal_Diagnostics;
with ACPI_Endpoint;
with ACPI_Requests; use ACPI_Requests;
with ACPI_Service;
with Firmware_Tables;
with Root_Test_Support; use Root_Test_Support;
procedure Fatal_Service_Tests is
   use type Firmware_Tables.Bytes;
   use type AML_Execute.Execution_Status;
   use type ACPI_Service.Install_Status;
   use type ACPI_Bootstrap.Phase;
   use type ACPI_Bootstrap.Device_Phase;
   use type ACPI_Bootstrap.Finish_Stage;
   use type ACPI_Fatal_Diagnostics.Origin;
   use type ACPI_Fatal_Diagnostics.Catalog_Identifier;
   use type ACPI_Fatal_Diagnostics.Namespace_Position;
   use type ACPI_Fatal_Diagnostics.Observation;
   use type ACPI_Bootstrap.State_Model;
   use type AML_Decode.Integer_Width;
   subtype Bytes is Firmware_Tables.Bytes;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Label : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Label; end if; end Check;
   Fatal : constant Bytes := [16#5B#,16#32#,16#A7#,16#78#,16#56#,16#34#,16#F2#,16#0A#,16#55#];
   Config : constant ACPI_Endpoint.Configuration := (Observer_Tag => 10, Provider_Tag => 11);
   function Table (Code : Bytes; Width : AML_Decode.Integer_Width) return Bytes is
      Data : Bytes (1 .. 36 + Code'Length) := [others => 0];
      Sum : Unsigned_8 := 0;
   begin
      Data (1 .. 4) := Enc ("DSDT");
      for I in 1 .. 4 loop Data (4 + I) := Unsigned_8 ((Data'Length / 256 ** (I - 1)) mod 256); end loop;
      Data (9) := (if Width = AML_Decode.Bits_32 then 1 else 2);
      Data (11 .. 16) := Enc ("CUBIT "); Data (17 .. 24) := Enc ("FATALSVC");
      Data (37 .. Data'Last) := Code;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum; return Data;
   end Table;
   procedure Payload (Seen : AML_Fatal.Observation; Width : AML_Decode.Integer_Width) is
   begin
      Check (Seen.Present, "retained typed fatal present");
      if Seen.Present then
         Check (Seen.Item.Literal.Kind = 16#A7# and then Seen.Item.Literal.Code = 16#F2345678#,
           "literal fatal fields survive service reports");
         Check (Seen.Item.Evaluated_Argument = 16#55# and then Seen.Item.Width = Width,
           "raw argument and width retained");
      end if;
   end Payload;
   procedure Diagnostic (Seen : ACPI_Fatal_Diagnostics.Observation;
      In_Ini : Boolean; Width : AML_Decode.Integer_Width) is
   begin
      Check (Seen.Present, "typed observer present");
      if Seen.Present then
         Payload ((Present => True, Item => Seen.Item), Width);
         Check (Seen.Source.Kind = (if In_Ini then ACPI_Fatal_Diagnostics.Device_Initializer
           else ACPI_Fatal_Diagnostics.Definition_Block), "typed diagnostic origin");
         case Seen.Source.Kind is
            when ACPI_Fatal_Diagnostics.Definition_Block =>
               Check (Seen.Source.Table_ID = 41, "observer catalog ID");
            when ACPI_Fatal_Diagnostics.Device_Initializer =>
               Check (Seen.Source.Initializer_Scope = 0, "observer root initializer context");
         end case;
         Check (ACPI_Fatal_Diagnostics.Format (Seen)'Length <= ACPI_Fatal_Diagnostics.Maximum_Text_Length,
           "bounded existing-logger-compatible formatting");
      end if;
   end Diagnostic;
begin
   Check (Startup_Error_Code'Enum_Rep (Error_Notify_Limit) = 38, "old wire38");
   Check (Startup_Error_Code'Enum_Rep (Error_Module_Completed) = 39, "old wire39");
   Check (Startup_Error_Code'Enum_Rep (Error_Mutex_Not_Acquired) = 40, "old wire40");
   Check (Startup_Error_Code'Enum_Rep (Error_Fatal_Requested) = 41, "appended wire41");
   for Width in AML_Decode.Integer_Width loop
      for In_Ini in Boolean loop
         declare
            Code : constant Bytes := (if In_Ini then Method ("_INI", Fatal) else Fatal);
            Raw : constant Bytes := Table (Code, Width);
            Boot : ACPI_Bootstrap.State (1,256,256);
            Server : State (1,256,256,0);
            Accepted : Boolean;
            Installed : ACPI_Service.Install_Status;
            Wire : Packet;
            Expected_Page : constant Unsigned_64 :=
              (if In_Ini then Metrics_Device_Error_Page else Metrics_Module_Error_Page);
         begin
            Check (not ACPI_Bootstrap.Fatal_Diagnostic (Boot).Present, "fresh bootstrap observer absent");
            Check (not Fatal_Diagnostic (Server).Present, "fresh request observer absent");
            -- Actual Bootstrap/Core path exposes the typed copied diagnostics.
            ACPI_Bootstrap.Start (Boot,1,Accepted); Check (Accepted, "bootstrap start");
            ACPI_Bootstrap.Import_Table (Boot,41,ACPI_Service.DSDT,Raw,Installed);
            Check (Installed = ACPI_Service.Installed, "bootstrap import");
            Check (not ACPI_Bootstrap.Fatal_Diagnostic (Boot).Present, "import does not fabricate fatal");
            ACPI_Bootstrap.Finish (Boot);
            declare Before : constant ACPI_Bootstrap.State_Model := ACPI_Bootstrap.Model (Boot) with Ghost; begin
               Diagnostic (ACPI_Bootstrap.Fatal_Diagnostic (Boot), In_Ini, Width);
               pragma Assert (ACPI_Bootstrap.Model (Boot) = Before);
            end;
            if In_Ini then
               -- Catalog completion is distinct from device initialization.
               Check (ACPI_Bootstrap.Current (Boot) = ACPI_Bootstrap.Complete, "catalog completed");
               Check (ACPI_Bootstrap.Device_Completion (Boot).Stage = ACPI_Bootstrap.Devices_Failed,
                 "fatal stops device initialization");
               Check (ACPI_Bootstrap.Device_Completion (Boot).First_Error = AML_Execute.Fatal_Requested,
                 "device fatal status");
               Payload (ACPI_Bootstrap.Device_Completion (Boot).Fatal,Width);
            else
               Check (ACPI_Bootstrap.Current (Boot) = ACPI_Bootstrap.Failed, "module fails startup");
               Check (ACPI_Bootstrap.Completion (Boot).Stage = ACPI_Bootstrap.Execution_Failed,
                 "execution failure stage");
               Check (ACPI_Bootstrap.Completion (Boot).Execution.Failed_Table_ID = 41,
                 "module failure retains catalog ID rather than ordinal");
               Payload (ACPI_Bootstrap.Completion (Boot).Execution.Modules.Fatal,Width);
            end if;
            ACPI_Endpoint.Dispatch (Server,Config,11,
              (Label => Start_Snapshot,Data => [0,1,0,0],others => <>),Wire);
            Check (Wire.Label = ACPI_Endpoint.Reply_OK, "authenticated start");
            ACPI_Endpoint.Dispatch_Block (Server,Config,11,Revision (Server),41,ACPI_Service.DSDT,Raw,Wire);
            Check (Wire.Label = ACPI_Endpoint.Reply_OK, "authenticated import");
            Check (Revision (Server) /= 0 and then not Fatal_Diagnostic (Server).Present,
              "new revisions before execution stay absent");
            ACPI_Endpoint.Dispatch (Server,Config,11,
              (Label => Finish_Snapshot,Data => [Revision (Server),0,0,0],others => <>),Wire);
            Check (Wire.Label = (if In_Ini then ACPI_Endpoint.Reply_OK else ACPI_Endpoint.Reply_Error),
              "actual finish envelope");
            if not In_Ini then
               Check (Completion (Server).Execution.Failed_Table_ID = 41,
                 "endpoint module failure retains catalog ID");
               Payload (Completion (Server).Execution.Modules.Fatal,Width);
            end if;
            declare
               Prior : constant Revision_Number := Revision (Server);
               Before : constant State_Model := Model (Server) with Ghost;
               Seen : constant ACPI_Fatal_Diagnostics.Observation := Fatal_Diagnostic (Server);
               Fresh : State (1,256,256,97);
            begin
               Diagnostic (Seen, In_Ini, Width);
               Check (Revision (Fresh) = 97 and then not Fatal_Diagnostic (Fresh).Present,
                 "fresh request lifetime with new initial revision has no stale fatal");
               for Stamp in Unsigned_64 range 10 .. 11 loop
                  ACPI_Endpoint.Dispatch (Server,Config,Stamp,
                    (Label => Read_Metrics,Data => [Expected_Page,0,0,0],others => <>),Wire);
                  Check (Wire.Label = ACPI_Endpoint.Reply_OK and then Wire.Data = [Prior,41,0,0],
                    "authorized exact fatal metric");
               end loop;
               ACPI_Endpoint.Dispatch (Server,Config,12,
                 (Label => Read_Metrics,Data => [Expected_Page,10,11,0],others => <>),Wire);
               Check (Wire.Label = ACPI_Endpoint.Reply_Error and then
                 Wire.Data = [Unsigned_64 (Outcome'Pos (Denied)),0,0,0], "payload tags grant no authority");
               Check (Revision (Server) = Prior, "diagnostic reads do not mutate revision");
               Check (Fatal_Diagnostic (Server) = Seen, "read-only observation preserves exact retained payload");
               pragma Assert (Model (Server) = Before);
            end;
         end;
      end loop;
   end loop;
   -- Constructed AML regression, not an ACPICA oracle: the root ordinary
   -- error must remain First_Error while _SB_._INI Fatal has its own scope.
   -- This initializer runs before the real service region-unavailable boundary.
   for Width in AML_Decode.Integer_Width loop
      declare
         Divide_By_Zero : constant Bytes := [16#78#, 1, 0, 0, 0];
         Code : constant Bytes := Method ("_INI", Divide_By_Zero) &
           Block ([16#10#], Enc ("_SB_") & Method ("_INI", Fatal));
         Raw : constant Bytes := Table (Code, Width);
         Boot : ACPI_Bootstrap.State (1,256,256);
         Server : State (1,256,256,0);
         Accepted : Boolean;
         Installed : ACPI_Service.Install_Status;
         Wire : Packet;
      begin
         ACPI_Bootstrap.Start (Boot,1,Accepted);
         Check (Accepted, "context bootstrap start");
         ACPI_Bootstrap.Import_Table (Boot,41,ACPI_Service.DSDT,Raw,Installed);
         Check (Installed = ACPI_Service.Installed, "context bootstrap import");
         ACPI_Bootstrap.Finish (Boot);
         declare
            Report : constant ACPI_Bootstrap.Device_Report := ACPI_Bootstrap.Device_Completion (Boot);
            Seen : constant ACPI_Fatal_Diagnostics.Observation := ACPI_Bootstrap.Fatal_Diagnostic (Boot);
         begin
            Ada.Text_IO.Put_Line ("CONTEXT width=" & Width'Image &
              " stage=" & Report.Stage'Image & " errors=" & Report.Errors'Image &
              " first=" & Report.First_Error'Image & " node=" & Report.First_Error_Node'Image &
              " fatal=" & Report.Fatal.Present'Image & " scope=" & Report.Fatal_Initializer'Image);
            Check (Report.Stage = ACPI_Bootstrap.Devices_Failed and then Report.Errors = 2,
              "ordinary error followed by fatal stops initialization");
            Check (Report.First_Error = AML_Execute.Division_By_Zero and then Report.First_Error_Node = 0,
              "first ordinary root error remains intact");
            Check (Report.Fatal.Present and then Report.Fatal_Initializer /= 0,
              "fatal retains separate non-root initializer scope");
            Payload (Report.Fatal, Width);
            Check (Seen.Present, "non-root diagnostic present");
            if Seen.Present then
               Check (Seen.Source.Kind = ACPI_Fatal_Diagnostics.Device_Initializer,
                 "non-root diagnostic origin");
               if Seen.Source.Kind = ACPI_Fatal_Diagnostics.Device_Initializer then
                  Check (Seen.Source.Initializer_Scope =
                    ACPI_Fatal_Diagnostics.Namespace_Position (Report.Fatal_Initializer),
                    "diagnostic uses fatal scope rather than first-error node");
               end if;
            end if;
            ACPI_Endpoint.Dispatch (Server,Config,11,
              (Label => Start_Snapshot,Data => [0,1,0,0],others => <>),Wire);
            Check (Wire.Label = ACPI_Endpoint.Reply_OK, "context endpoint start");
            ACPI_Endpoint.Dispatch_Block (Server,Config,11,Revision (Server),41,ACPI_Service.DSDT,Raw,Wire);
            Check (Wire.Label = ACPI_Endpoint.Reply_OK, "context endpoint import");
            ACPI_Endpoint.Dispatch (Server,Config,11,
              (Label => Finish_Snapshot,Data => [Revision (Server),0,0,0],others => <>),Wire);
            Check (Wire.Label = ACPI_Endpoint.Reply_OK, "context catalog finish");
            Check (Fatal_Diagnostic (Server) = Seen,
              "Requests preserves separate non-root context and exact payload");
         end;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Fatal service checks" & Checks'Image);
end Fatal_Service_Tests;
