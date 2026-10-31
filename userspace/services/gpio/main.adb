pragma Ada_2022;
with Interfaces;
with CuBit.Messages;
with CuBit.Process_IDs;
with CuBit.Handles;
with CuBit.Handles.Startup;
with CuBit.Handles.IPC;
with CuBit.Driver_Readiness;
with Parameters;
with Startup;
with Intel_ADLN_GPIO;
with Intel_GPIO_IRQ;
with GPIO_Community_Park;
with GPIO_Native_Communities;
with GPIO_Native_Clock;
with GPIO_Native_Resources;
with GPIO_Native_Driver;
with GPIO_Native_Pump;
with GPIO_Native_Log;
with GPIO_Native_Wait;
with GPIO_Controller_Resources;
with GPIO_Mapping_Policy;
with GPIO_Loop_Time;
with GPIO_Polling_Loop;
with GPIO_Startup_Readiness;
procedure Main is
   package M renames CuBit.Messages;
   package PID renames CuBit.Process_IDs;
   package I renames Intel_GPIO_IRQ;
   package G renames Intel_ADLN_GPIO;
   package Time renames GPIO_Loop_Time;
   package Resources renames GPIO_Native_Resources;
   package Mapping renames GPIO_Mapping_Policy;
   Raw : constant Parameters.GPIO_Parameters_Value := Parameters.Value;
   use type Interfaces.Unsigned_64, M.MessageTag, M.Activity_Result;
   use type I.Tick, Resources.Outcome, Mapping.Address;
   use type GPIO_Native_Communities.Resource_Count;
   package Parking is new GPIO_Community_Park
     (GPIO_Native_Communities.Ready, GPIO_Native_Communities.Admit_Pin,
      GPIO_Native_Clock.Read, GPIO_Native_Communities.Read, GPIO_Native_Communities.Write);
   Parking_State : Parking.State;
   use type Parking.Phase;
   function Startup_Parked return Boolean is (Parking.Observe (Parking_State).At_Phase = Parking.Ready);
   package Driver is new GPIO_Native_Driver
     (GPIO_Controller_Resources.Pin_Limit (Raw.capacity), I.Limit (Raw.operation_budget),
      Startup.Launcher, Startup_Parked);
   package Log is new GPIO_Native_Log (Startup.Logstore);
   package Native is new GPIO_Native_Pump
     (Driver, Positive (Raw.work.events), Positive (Raw.work.requests), Positive (Raw.work.steps),
      I.Tick (Raw.timing.admission), I.Tick (Raw.timing.cleanup), Log.Disable);
   package Scheduler is new GPIO_Polling_Loop
     (Native.Pump, GPIO_Native_Clock.Read, Time.Duration (Raw.timing.poll_interval),
      Time.Duration (Raw.timing.poll_window), Time.Duration (Raw.timing.progress));
   use type Driver.Admin.Phase;
   State : Driver.Admin.State;
   Frame : Scheduler.Turn_Report;
   Resource_Report : Resources.Report;
   GPIO_Role : constant Interfaces.Unsigned_64 := 34;
   Registration_Refused : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;
   -- Dedicated process-local MMIO arena, separate from the image/heap convention.
   -- Kernel Map rejects existing mappings; each attempted span stays reserved
   -- locally even on failure. No physical address is selected here.
   Register_VA_First : constant Mapping.Address := 16#6000_0000_0000#;
   Resource_Count : constant Mapping.Address := Mapping.Address (G.Memory_Resource_Ordinal'Last) + 1;
   Register_VA_Bytes : constant Mapping.Address :=
      Resource_Count * Mapping.Maximum_Map_Pages * Mapping.Page_Bytes;
   Register_VA_Limit : constant Mapping.Address := Register_VA_First + Register_VA_Bytes;
   package Readiness renames GPIO_Startup_Readiness;
   Readiness_State : Readiness.State;
   use type Readiness.Choice, CuBit.Handles.ABI.Kind;
   Stop_Latched, Wait_Failure_Noted : Boolean := False;
   Hold_Resources : Boolean := False;
   Now, Startup_Deadline : I.Tick;
   Clock_OK, Accepted, Resources_OK, Ready_OK, Found : Boolean;
   Wait_Result : M.Activity_Result;
   function Authority (Item : CuBit.Handles.Startup.Optional_Device_Memory) return CuBit.Handles.Device_Memory is
     (if Item.Present then Item.Value else CuBit.Handles.No_Device_Memory);
   Caps : constant Resources.Authorities :=
     [Authority (Startup.Community0), Authority (Startup.Community1),
      Authority (Startup.Community4), Authority (Startup.Community5)];
   procedure Wait (Plan : Time.Wait_Plan) is
      Ignored : Interfaces.Unsigned_64;
   begin
      GPIO_Native_Wait.Await (Plan, Time.Duration (Raw.timing.progress), Wait_Result);
      if Wait_Result = M.Unavailable then
         if not Wait_Failure_Noted then
            Wait_Failure_Noted := True;
            Log.Stage (Log.Readiness, False, "activity-wait-unavailable");
         end if;
         -- Yield still gives other ready threads a turn if both timed waits
         -- failed; it does not complete or retry a device operation.
         Ignored := M.syscall (M.SYSCALL_YIELD);
      end if;
   end Wait;
   procedure Request_Stop is
      Sample : I.Tick;
      Available : Boolean;
      Limit : I.Tick := 0;
   begin
      if Stop_Latched then return; end if;
      Stop_Latched := True;
      GPIO_Native_Clock.Read (Sample, Available);
      if Available then Limit := Time.After (Sample, I.Tick (Raw.timing.cleanup)); end if;
      Driver.Registry.Request_Stop (Limit); Driver.Admin.Request_Stop (State);
   end Request_Stop;
   procedure Report_Once (Ready : Boolean; Acknowledged : out Boolean) is
      Message : M.Message := M.NULL_MESSAGE;
      Reply : M.MessageTag;
      Sample : I.Tick;
      Available, Queued : Boolean;
      Selected : Readiness.Choice;
   begin
      Acknowledged := False; if Readiness.Attempted (Readiness_State) then return; end if;
      GPIO_Native_Clock.Read (Sample, Available);
      Readiness.Decide (Readiness_State,
        (Community_Parked => Startup_Parked,
         Registry_Operational => Driver.Registry.Observe.Operational,
         Role_Registered => Ready,
         Admission_Clean => Driver.Admin.Quiescent (State) and then not Driver.Admin.Observe (State).Stopping,
         Not_Stopping => not Driver.Registry.Observe.Stopping and then not Stop_Latched,
         Clock_Available => Available), Selected);
      if Selected = Readiness.No_Report then return; end if;
      if not Available then
         Message.tag.label := CuBit.Driver_Readiness.Not_Present_Label;
         Queued := M.capSubmit (Startup.Ready, Message, M.NO_COMPLETION_TOKEN);
         Log.Stage (Log.Readiness, False, (if Queued then "decline-queued-no-clock" else "decline-refused-no-clock"));
         return;
      end if;
      Message.tag.label := (if Selected = Readiness.Report_Ready then CuBit.Driver_Readiness.Ready_Label else CuBit.Driver_Readiness.Not_Present_Label);
      Reply := M.capSend (Startup.Ready, Message,
         Interfaces.Unsigned_64 (Time.After (Sample, I.Tick (Raw.timing.readiness))));
      Acknowledged := Selected = Readiness.Report_Ready and then Reply = M.NULL_TAG;
      Log.Stage (Log.Readiness, Acknowledged,
         (if Reply = M.NULL_TAG then (if Selected = Readiness.Report_Ready then "acknowledged-polled-no-parent-irq" else "declined") else "report-refused"));
   end Report_Once;
   procedure Startup_Queues is
   begin
      for N in 1 .. Positive (Raw.work.events) loop
         Native.Poll_Event (State, Found); exit when not Found;
      end loop;
      for N in 1 .. Positive (Raw.work.requests) loop
         Native.Poll_Request (State, Found); exit when not Found;
      end loop;
      if Driver.Registry.Observe.Stopping or else Driver.Admin.Observe (State).At_Phase = Driver.Admin.Quarantined then
         Parking.Request_Stop (Parking_State);
      end if;
   end Startup_Queues;
   procedure Decline (Keep_Resources : Boolean) is
      Acknowledged : Boolean;
   begin
      Report_Once (False, Acknowledged); Log.Summary;
      Hold_Resources := Keep_Resources; Request_Stop;
   end Decline;
begin
   declare Launcher : constant CuBit.Handles.IPC.Description := CuBit.Handles.IPC.Describe (Startup.Launcher); begin
      if not Launcher.Current or else Launcher.Kind /= CuBit.Handles.ABI.Process or else
         not PID.Is_Process (PID.From_Word (Launcher.Object)) then
         Log.Stage (Log.Identify, False, "launcher-lifetime-refused"); Decline (False); return;
      end if;
   end;
   Log.Stage (Log.Identify, True, G.Target_HID, " mode=polled parent-irq=unbound");
   GPIO_Native_Clock.Read (Now, Clock_OK);
   if not Clock_OK then
      Log.Stage (Log.Resources, False, "clock-unavailable"); Decline (False); return;
   end if;
   Startup_Deadline := Time.After (Now, I.Tick (Raw.timing.startup));
   Resources.Setup (Caps, Register_VA_First, Register_VA_Limit, Mapping.Address (Raw.pages_per_resource),
      Startup_Deadline, Resource_Report);
   Resources_OK := Resource_Report.Ready_Count > 0 and then not Resource_Report.Repeated;
   for Ordinal in G.Memory_Resource_Ordinal loop
      Log.Community (Ordinal, Resource_Report.Resources (Ordinal));
      if CuBit.Handles.Valid (Caps (Ordinal)) and then Resource_Report.Resources (Ordinal).Status /= Resources.Ready then
         Resources_OK := False;
      end if;
   end loop;
   Log.Stage (Log.Resources, Resources_OK, (if Resources_OK then "mapped-and-probed" else "mapping-or-probe-refused"));
   if not Resources_OK then Decline (False); return; end if;
   Log.Stage (Log.Geometry, True, "frozen-profile", " gpio-addressable=251 nomap-untouched=6");
   Parking.Start (Parking_State, Startup_Deadline, Parking.Work_Limit (Raw.startup_budget), Accepted);
   if not Accepted then Log.Stage (Log.Pin_Setup, False, "parking-not-started"); Decline (True);
   else
      loop
         Startup_Queues;
         for N in 1 .. Positive (Raw.work.steps) loop
            Parking.Step (Parking_State);
            exit when Parking.Observe (Parking_State).At_Phase in Parking.Ready | Parking.Failed | Parking.Cancelled;
         end loop;
         exit when Parking.Observe (Parking_State).At_Phase in Parking.Ready | Parking.Failed | Parking.Cancelled;
         Wait ((Time.Relative_Delay, Time.Duration (Raw.timing.progress)));
         if Wait_Failure_Noted then Parking.Request_Stop (Parking_State); end if;
      end loop;
      declare Result : constant Parking.Report := Parking.Observe (Parking_State); begin
         Log.Stage (Log.Pin_Setup, Result.At_Phase = Parking.Ready, Parking.Failure'Image (Result.Reason),
            Log.Hex ("pin", Interfaces.Unsigned_64 (Result.Last_Pin)) &
            Log.Hex ("val", Interfaces.Unsigned_64 (Result.Last_Value)) & " at=" & Parking.Phase'Image (Result.Last_Access_Phase));
         Log.Stage (Log.Parked, Result.At_Phase = Parking.Ready, (if Result.At_Phase = Parking.Ready then "host-owned-only" else "partial-retained"),
            Log.Hex ("visited", Interfaces.Unsigned_64 (Result.Visited)) &
            Log.Hex ("parked", Interfaces.Unsigned_64 (Result.Parked)) &
            Log.Hex ("firmware", Interfaces.Unsigned_64 (Result.Firmware_Owned)) &
            Log.Hex ("absent", Interfaces.Unsigned_64 (Result.Unmapped)) & " nomap-untouched=6");
      end;
      Startup_Queues;
      if not Startup_Parked or else Driver.Registry.Observe.Stopping or else
         Driver.Admin.Observe (State).At_Phase = Driver.Admin.Quarantined then Decline (True);
      else
         Driver.Registry.Seal (Accepted);
         if not Accepted then Log.Stage (Log.Registration, False, "geometry-seal-refused"); Decline (True);
         else
            declare Registered : constant Interfaces.Unsigned_64 := M.registerDriver (GPIO_Role, Startup.Registration); begin
               Ready_OK := PID.Is_Process (M.Own_Process) and then Registered /= Registration_Refused and then
                  Registered = PID.To_Word (M.Own_Process);
            end;
            Log.Stage (Log.Registration, Ready_OK, (if Ready_OK then "primary" else "role-refused"));
            Report_Once (Ready_OK, Accepted); Log.Summary;
            if not Ready_OK or else not Accepted then Request_Stop; end if;
         end if;
      end if;
   end if;
   loop
      Scheduler.Turn (State, Frame);
      if Frame.Supervision_Required or else Wait_Failure_Noted then
         Hold_Resources := True; Request_Stop;
      end if;
      exit when Frame.Work.Terminal and then Frame.Work.Quiescent and then not Hold_Resources;
      Wait ((if Hold_Resources then (Time.Relative_Delay, Time.Duration (Raw.timing.progress)) else Frame.Wait_For));
   end loop;
end Main;
