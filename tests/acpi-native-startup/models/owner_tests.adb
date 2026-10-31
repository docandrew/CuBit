pragma Ada_2022;
with Delay_Owner_Model;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with AML_Decode;
with ACPI_Native_Instance;
with ACPI_Snapshot_Startup;
with ACPI_Fatal_Diagnostics;
with ACPI_Module_Diagnostics;
with Ada.Strings.Fixed;
with CuBit.Messages;
with Owner_Test_Control;
procedure Owner_Tests is
   package T renames Owner_Test_Control;
   package L renames ACPI_Snapshot_Startup;
   use type T.Scenario;
   use type L.Outcome;
   use type L.R.Tick;
   use type T.Level;
   use type ACPI_Module_Diagnostics.Count;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Label; end if;
   end Check;
   Device_Lines, EC_Lines : Natural := 0;
   EC_Prefix : constant String := "acpi: stage ec-boot ";
   Device_Prefix : constant String := "acpi: stage devices ";
   Recovery_Prefix : constant String := "acpi: stage aml-recovery ok ";
   Serve_Failure_Prefix : constant String := "acpi: stage serve fail ";
   Critical_Seen, Stop_Seen, Serve_Failure_Seen, Recovery_Seen : Natural := 0;
   Load_Expected, Completed, Should_Register, Should_Serve, Wanted_Fatal, Power_Expected : Boolean;
   Fault_Expected, Fault_Seen : Boolean := False;
   function Expected_Fault return String is
     (case T.Selected is
        when T.Load_Exception => "modeled loader exception",
        when T.Detection_Exception => "modeled unexpected detection exception",
        when T.Power_Exception => "modeled power exception",
        when T.Recovery_Log_Failure => "modeled recovery logger failure",
        when others => "modeled critical logger failure");
   procedure Check_No_Replay is
      Loads : constant Natural := T.Loads;
      Detections : constant Natural := T.Detections;
      Powers : constant Natural := T.Power_Detections;
      Registers : constant Natural := T.Registrations;
      Serves : constant Natural := T.Serves;
      Copies : constant Natural := T.Copies;
      Clocks : constant Natural := T.Clock_Reads;
      Logs : constant Natural := T.Log_Count;
      Arms : constant Natural := Delay_Owner_Model.Arms;
      Closes : constant Natural := Delay_Owner_Model.Closes;
   begin
      ACPI_Native_Instance.Start;
      Check (T.Power_Detections = Powers and T.Detections = Detections and T.Loads = Loads and T.Registrations = Registers and T.Serves = Serves and
        T.Copies = Copies and T.Clock_Reads = Clocks and T.Log_Count = Logs and
        Delay_Owner_Model.Arms = Arms and Delay_Owner_Model.Closes = Closes,
        "second Start never replays any effect after a normal result or host-only propagated fault");
   end Check_No_Replay;
begin
   if Ada.Command_Line.Argument_Count /= 3 then raise Program_Error with "three model parameters required"; end if;
   T.Selected := T.Scenario'Value (Ada.Command_Line.Argument (1));
   T.Width := AML_Decode.Integer_Width'Value (Ada.Command_Line.Argument (2));
   T.Broken_Critical := Boolean'Value (Ada.Command_Line.Argument (3));
   Fault_Expected := T.Selected in T.Load_Exception | T.Detection_Exception |
     T.Power_Exception | T.Recovery_Log_Failure or else
       (T.Broken_Critical and then T.Selected in T.Module_Fatal .. T.Device_Fatal);
   -- Only this hosted test boundary catches the modeled contract violation.
   -- Production runs under No_Exception_Handlers: these failures are terminal.
   begin
      ACPI_Native_Instance.Start;
   exception
      when E : Program_Error =>
         Fault_Seen := True;
         Check (Fault_Expected and then Ada.Exceptions.Exception_Message (E) = Expected_Fault,
           "exact modeled trusted-boundary failure propagates");
   end;
   Check (Fault_Seen = Fault_Expected, "only modeled contract violations propagate");
   Load_Expected := T.Selected not in T.Clock_Unavailable | T.Clock_Overflow;
   Check (T.Clock_Reads = 1, "one initial monotonic sample");
   Check (T.Loads = Boolean'Pos (Load_Expected), "one load only when clock representable");
   Check (T.Native_Reads = 0, "no real native adapter executed by owner model");
   if Load_Expected then
      Check (T.Passed_Deadline = 10_000_010, "finite microsecond deadline");
   else
      Check (T.Copies = 0, "no copy on unavailable clock");
   end if;
   if Fault_Expected then
      Check (T.Registrations = 0 and then T.Serves = 0,
        "a contract violation never registers or serves a partial owner");
      Check (T.Detections = Boolean'Pos (T.Selected in T.Detection_Exception | T.Power_Exception),
        "fatal logger and loader faults stop before detection");
      Check (T.Power_Detections = Boolean'Pos (T.Selected = T.Power_Exception),
        "later power work is not attempted after an earlier terminal fault");
      Check (Delay_Owner_Model.Active and then Delay_Owner_Model.Closes = 0,
        "host propagation does not invent unavailable native recovery cleanup");
      Check_No_Replay;
      Ada.Text_IO.Put_Line ("Native owner checks" & Checks'Image);
      return;
   end if;
   Completed := Load_Expected and then T.Selected /= T.Load_Exception and then T.Last_Load.Status = L.Loaded;
   Power_Expected := Completed and then T.Selected not in T.Detection_Cleanup | T.Detection_Exception | T.Detection_Recovery;
   Should_Register := Power_Expected and then T.Selected not in T.Power_Cleanup | T.Power_Exception;
   Should_Serve := Should_Register and then T.Selected /= T.Register_Refused;
   Wanted_Fatal := Load_Expected and then T.Selected in T.Module_Fatal .. T.Device_Fatal;
   if T.Selected in T.Detection_Cleanup | T.Detection_Recovery then
      Check (T.Log (T.Log_Count).Severity = T.Warning and then
        T.Log (T.Log_Count).Text (1 .. T.Log (T.Log_Count).Length) =
          "acpi: bring-up reached hid-discovery (3/4)", "cleanup refusal retains final bring-up summary");
   end if;
   if T.Selected = T.Power_Cleanup then
      Check (T.Log (T.Log_Count).Severity = T.Warning and then
        T.Log (T.Log_Count).Text (1 .. T.Log (T.Log_Count).Length) =
          "acpi: bring-up reached power-discovery (3/4)", "power recovery preserves final summary");
   end if;
   Check (T.Power_Detections = Boolean'Pos (Power_Expected), "power only after completed HID pass");
   if Power_Expected then Check (T.Power_Detected_At > T.Detected_At, "power follows HID cleanup"); end if;
   Check (T.Detections = Boolean'Pos (Completed), "one detection only after completed snapshot");
   if Completed then Check (T.Detected_At > T.Load_Completed_At, "detection follows snapshot completion"); end if;
   Check (T.Registrations = Boolean'Pos (Should_Register), "no registration before complete owner");
   Check (T.Serves = Boolean'Pos (Should_Serve), "only registered complete owner served");
   if Should_Register then
      Check (T.Registered_Role = CuBit.Messages.DRIVER_ACPI, "named catalog role");
      Check (T.Registered_At > T.Power_Detected_At, "registration follows complete load");
   end if;
   if Should_Serve then Check (T.Served_At > T.Registered_At, "serve follows registration"); end if;
   Check (T.Critical_Attempts = Boolean'Pos (Wanted_Fatal), "exactly one fatal log attempt");
   for I in 1 .. T.Log_Count loop
      if T.Log (I).Length >= EC_Prefix'Length and then T.Log (I).Text (1 .. EC_Prefix'Length) = EC_Prefix then
         EC_Lines := EC_Lines + 1;
         Check (Device_Lines = 0, "EC metadata precedes device summary");
         Check (T.Log (I).Severity = T.Information and then
           Ada.Strings.Fixed.Index (T.Log (I).Text (1 .. T.Log (I).Length), "MISSING_ECDT") > 0,
           "absent boot EC metadata is informational");
      end if;
      if T.Log (I).Length >= Device_Prefix'Length and then
        T.Log (I).Text (1 .. Device_Prefix'Length) = Device_Prefix then
         Device_Lines := Device_Lines + 1;
         declare
            Text : constant String := T.Log (I).Text (1 .. T.Log (I).Length);
            Warn : constant Boolean := T.Selected in T.Unconfigured_Region | T.Ordinary_Error | T.Root_Fatal | T.Bus_Fatal | T.Device_Fatal;
         begin
            Check ((T.Log (I).Severity = T.Warning) = Warn, "device outcome severity distinguishes incomplete initialization");
            Check (Text'Length <= 256, "device outcome fits bounded photo-readable line");
            if T.Selected = T.Unconfigured_Region then
               Check (Ada.Strings.Fixed.Index (Text, "regions=REGIONS_UNAVAILABLE") > 0,
                 "unconfigured hardware barrier is visible");
            end if;
         end;
      end if;
      if T.Log (I).Text (1 .. T.Log (I).Length) = "acpi: stage serve ok STOP_REQUESTED" then
         Stop_Seen := Stop_Seen + 1;
         Check (T.Log (I).Severity = T.Information, "normal Stop has Info severity");
      end if;
      if T.Log (I).Length >= Serve_Failure_Prefix'Length and then
        T.Log (I).Text (1 .. Serve_Failure_Prefix'Length) = Serve_Failure_Prefix then
         Serve_Failure_Seen := Serve_Failure_Seen + 1;
         Check (T.Log (I).Severity = T.Warning, "abnormal server exit has Warning severity");
      end if;
      if T.Log (I).Length >= Recovery_Prefix'Length and then T.Log (I).Text (1 .. Recovery_Prefix'Length) = Recovery_Prefix then
         Recovery_Seen := Recovery_Seen + 1;
         Check (T.Log (I).Severity = T.Warning, "recovered firmware error retains Warning severity");
         Check (T.Log (I).Text (1 .. T.Log (I).Length) = ACPI_Module_Diagnostics.Format (T.Last_Load.Module_Errors),
           "copied recovery diagnostic survives partial owner release");
         Check (T.Log (I).Length <= ACPI_Module_Diagnostics.Maximum_Text_Length, "bounded recovery line");
      end if;
      if T.Log (I).Severity = T.Critical then
         Critical_Seen := Critical_Seen + 1;
         Check (T.Log (I).Text (1 .. T.Log (I).Length) =
                ACPI_Fatal_Diagnostics.Format (T.Last_Load.Fatal), "copied exact Fatal diagnostic");
      end if;
   end loop;
   Check (EC_Lines = Boolean'Pos (Completed), "one EC catalog observation only after loaded snapshot");
   Check (Device_Lines = Boolean'Pos (Completed), "one device outcome only after loaded snapshot");
   Check (T.Recovery_Attempts = Boolean'Pos (T.Selected in T.Recovered | T.Recovered_Then_Failed | T.Recovery_Log_Failure),
     "one recovery warning attempt only, even if logging raises");
   Check (Recovery_Seen = T.Recovery_Attempts, "exactly one copied recovery record");
   if T.Selected in T.Recovered | T.Recovered_Then_Failed | T.Recovery_Log_Failure then
      Check (T.Last_Load.Module_Errors.Count = 1, "snapshot copied real interpreter recovery count");
   end if;
   Check (Critical_Seen = T.Critical_Attempts, "Critical severity retained even if logger raises");
   Check (Stop_Seen = Boolean'Pos (Should_Serve and then T.Selected = T.Stop_Request),
     "normal Stop reported exactly once only after serving");
   Check (Serve_Failure_Seen = Boolean'Pos (Should_Serve and then T.Selected /= T.Stop_Request),
     "normal Stop is not a serve failure; abnormal return is reported once");
   Check_No_Replay;
   Check (not Delay_Owner_Model.Active and then Delay_Owner_Model.Closes > 0,
     "startup delay domain closed on every normal typed return");
   Ada.Text_IO.Put_Line ("Native owner checks" & Checks'Image);
end Owner_Tests;
