pragma Ada_2022;
with Interfaces;
with CuBit.Messages;
with CuBit.Process_IDs;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Driver_Readiness;
with GPIO_Native_Policy;
with GPIO_Native_Launch;
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
with GPIO_IRQ_Turn;
with GPIO_IRQ_Sweep;
with CuBit.Handles.Lines;
with Interrupt_Line_ABI;
with Launch_Configuration;
with GPIO_Startup_Readiness;
procedure Main is
   package M renames CuBit.Messages;
   package PID renames CuBit.Process_IDs;
   package I renames Intel_GPIO_IRQ;
   package G renames Intel_ADLN_GPIO;
   package Time renames GPIO_Loop_Time;
   package Resources renames GPIO_Native_Resources;
   package Mapping renames GPIO_Mapping_Policy;
   Raw : constant GPIO_Native_Policy.Settings := GPIO_Native_Policy.Value;
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
      Startup.Ready, Startup_Parked);
   package Log is new GPIO_Native_Log (Startup.Logstore);
   package Native is new GPIO_Native_Pump
     (Driver, Positive (Raw.work.events), Positive (Raw.work.requests), Positive (Raw.work.steps),
      I.Tick (Raw.timing.admission), I.Tick (Raw.timing.cleanup), Log.Disable);
   package LA renames Interrupt_Line_ABI;
   use type Driver.Admin.Phase;
   State : Driver.Admin.State;
   use type Driver.Registry.Channel_ID;
   Frame : Native.Pump.Turn_Report;
   Resource_Report : Resources.Report;
   Launch_Report : GPIO_Native_Launch.Report;
   use type GPIO_Native_Launch.Outcome;
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
   Stop_Latched, Wait_Failure_Noted, IRQ_Failure_Noted : Boolean := False;
   Hold_Resources : Boolean := False;
   Now, Startup_Deadline : I.Tick;
   Clock_OK, Accepted, Resources_OK, Ready_OK, Found : Boolean;
   Wait_Result : M.Activity_Result;
   Parent_Present : Boolean := False;
   Parent_Vector : LA.Line_Vector := LA.First_Vector;
   package Sweep is new GPIO_IRQ_Sweep (Driver.Registry, Native.Hold_Requests, Native.Requests_Held);
   Sweep_State : Sweep.State;
   use type Sweep.Phase;
   procedure IRQ_Clock (Sample : out Interfaces.Unsigned_64; Available : out Boolean) is
      Value : I.Tick;
   begin GPIO_Native_Clock.Read (Value, Available); Sample := Interfaces.Unsigned_64 (Value); end IRQ_Clock;
   procedure IRQ_Start (Deadline : Interfaces.Unsigned_64; Started : out Boolean) is
   begin Sweep.Start (Sweep_State, I.Tick (Deadline), Started); end IRQ_Start;
   procedure IRQ_Advance is
   begin Native.Pump.Turn (State, Frame); Sweep.Advance (Sweep_State); end IRQ_Advance;
   procedure IRQ_Inspect (Clear, Uncertain : out Boolean) is
   begin Clear := Sweep.Current (Sweep_State) = Sweep.Source_Clear;
      Uncertain := Sweep.Current (Sweep_State) = Sweep.Uncertain; end IRQ_Inspect;
   procedure IRQ_ACK (Acknowledged : out Boolean) is
   begin
      Acknowledged := False;
      if Parent_Present and then Startup.Parent.Present then
         Acknowledged := CuBit.Handles.Lines.Acknowledge (Startup.Parent.Value, Parent_Vector).Accepted;
      end if;
      Sweep.Finish (Sweep_State, Acknowledged);
   end IRQ_ACK;
   package IRQ is new GPIO_IRQ_Turn (IRQ_Clock, IRQ_Start, IRQ_Advance, IRQ_Inspect, IRQ_ACK,
      Time.Duration (Raw.timing.sweep), Time.Duration (Raw.timing.progress));
   IRQ_State : IRQ.State;
   use type IRQ.Phase;
   Caps : constant Resources.Authorities :=
     [Startup.Community0, Startup.Community1, Startup.Community4, Startup.Community5];
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
         (if Reply = M.NULL_TAG then (if Selected = Readiness.Report_Ready then (if Parent_Present then "acknowledged-parent-irq" else "acknowledged-events-unavailable") else "declined") else "report-refused"));
   end Report_Once;
   procedure Configure_Parent (Ready : out Boolean) is
      Configured : Boolean;
      Reason : String (1 .. 32) := [others => ' '];
      Last : Natural := 0;
      procedure Why (Text : String) is
      begin Reason (1 .. Text'Length) := Text; Last := Text'Length; end Why;
   begin
      Ready := True;
      if not Launch_Configuration.Valid_Parent_Line (Launch_Report.Parent) then
         Ready := False; Why ("invalid-description");
      elsif not Launch_Report.Parent.Present then
         if Startup.Parent.Present then Ready := False; Why ("unexpected-authority");
         else Why ("not-routed"); end if;
      elsif not Startup.Parent.Present then Ready := False; Why ("missing-authority");
      else
         declare D : constant CuBit.Handles.IPC.Description := CuBit.Handles.IPC.Describe (Startup.Parent.Value); begin
            if D.Current and then D.Kind = CuBit.Handles.ABI.Interrupt and then D.Held.Read and then
               D.Object = Interfaces.Unsigned_64 (Launch_Report.Parent.Vector) and then D.Parameter = 1
            then
               Parent_Vector := LA.Line_Vector (Launch_Report.Parent.Vector);
               Parent_Present := True; Why ("validated");
            else Ready := False; Why ("authority-mismatch"); end if;
         end;
      end if;
      Native.Configure_Parent (Parent_Present, Configured);
      if not Configured then Ready := False; Why ("configuration-repeated"); end if;
      if Parent_Present and Configured then
         -- Release any parent delivery that preceded registration. Startup
         -- parking and the sealed registry establish the initial quiet source.
         IRQ.Note (IRQ_State, True);
         for N in 1 .. Positive (Raw.work.steps) loop
            IRQ.Turn (IRQ_State);
            exit when IRQ.Observe (IRQ_State).At_Phase in IRQ.Idle | IRQ.Faulted;
         end loop;
         Ready := IRQ.Observe (IRQ_State).At_Phase = IRQ.Idle;
         if not Ready then Why ("initial-ack-unconfirmed"); end if;
      end if;
      Log.Parent_Line (Parent_Present and Ready, Natural (Launch_Report.Parent.Vector), Reason (1 .. Last));
   end Configure_Parent;
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
   declare Launcher : constant CuBit.Handles.IPC.Description := CuBit.Handles.IPC.Describe (Startup.Ready); begin
      if not Launcher.Current or else Launcher.Kind /= CuBit.Handles.ABI.Endpoint or else
         not Launcher.Held.Read or else not PID.Is_Process (PID.From_Word (Launcher.Object)) then
         Log.Stage (Log.Identify, False, "launcher-lifetime-refused"); Decline (False); return;
      end if;
   end;
   GPIO_Native_Launch.Validate (Caps, Launch_Report);
   if Launch_Report.Status /= GPIO_Native_Launch.Validated then
      Log.Stage (Log.Identify, False, "launch-" & Launch_Report.Status'Image,
         " resource=" & Launch_Report.At_Resource'Image);
      Decline (False); return;
   end if;
   Log.Stage (Log.Identify, True, G.Target_HID, " mode=irq-or-events-unavailable");
   GPIO_Native_Clock.Read (Now, Clock_OK);
   if not Clock_OK then
      Log.Stage (Log.Resources, False, "clock-unavailable"); Decline (False); return;
   end if;
   Startup_Deadline := Time.After (Now, I.Tick (Raw.timing.startup));
   Resources.Setup (Caps, Register_VA_First, Register_VA_Limit, Mapping.Address (Raw.pages_per_resource),
      Startup_Deadline, Resource_Report);
   Resources_OK := Resource_Report.Ready_Count = GPIO_Native_Communities.Resource_Count
     (Integer (G.Memory_Resource_Ordinal'Last) + 1) and then not Resource_Report.Repeated;
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
            Configure_Parent (Accepted);
            if not Accepted then Hold_Resources := True; Request_Stop; end if;
            declare Registered : constant Interfaces.Unsigned_64 := M.registerDriver (GPIO_Role, Startup.Registration); begin
               Ready_OK := not Stop_Latched and then PID.Is_Process (M.Own_Process) and then Registered /= Registration_Refused and then
                  Registered = PID.To_Word (M.Own_Process);
            end;
            Log.Stage (Log.Registration, Ready_OK, (if Ready_OK then "primary" else "role-refused"));
            Report_Once (Ready_OK, Accepted); Log.Summary;
            if not Ready_OK or else not Accepted then Request_Stop; end if;
         end if;
      end if;
   end if;
   loop
      for N in 1 .. Positive (Raw.work.steps) loop
         declare Notice : Boolean; begin
            Native.Take_Parent_Notice (Notice); IRQ.Note (IRQ_State, Notice);
            if IRQ.Observe (IRQ_State).Needs_Progress then IRQ.Turn (IRQ_State);
            else Native.Pump.Turn (State, Frame); end if;
            Native.Take_Parent_Notice (Notice); IRQ.Note (IRQ_State, Notice);
         end;
         exit when not IRQ.Observe (IRQ_State).Needs_Progress and then not Driver.Needs_Step (State);
      end loop;
      if IRQ.Observe (IRQ_State).At_Phase = IRQ.Faulted or else
         Driver.Controller.Observe.Faulted or else Driver.Registry.Observe.Layout_Changed or else
         Driver.Registry.Observe.Unresolved /= 0 or else Driver.Admin.Observe (State).At_Phase = Driver.Admin.Quarantined or else
         Wait_Failure_Noted
      then
         if not IRQ_Failure_Noted then
            IRQ_Failure_Noted := True;
            Log.Stage (Log.Readiness, False, "runtime-supervision-required",
               " irq=" & IRQ.Observe (IRQ_State).Reason'Image);
         end if;
         Hold_Resources := True; Request_Stop;
      end if;
      if Driver.Registry.Observe.Stopping then Request_Stop; end if;
      exit when Driver.Registry.Terminal and then Driver.Registry.Quiescent and then Driver.Admin.Quiescent (State)
         and then not IRQ.Observe (IRQ_State).Parent_Held and then not Native.Requests_Held and then not Hold_Resources;
      if Hold_Resources then Wait ((Time.Relative_Delay, Time.Duration (Raw.timing.progress)));
      elsif IRQ.Observe (IRQ_State).Needs_Progress or else Driver.Needs_Step (State) or else Native.Requests_Held then
         GPIO_Native_Clock.Read (Now, Clock_OK);
         if not Clock_OK or else Now >= Time.Finite_Time'Last then Hold_Resources := True; Request_Stop;
         else Wait ((Time.Until_Time, Time.After (Now, I.Tick (Raw.timing.progress)))); end if;
      else Wait ((Kind => Time.Activity)); end if;
   end loop;
end Main;
