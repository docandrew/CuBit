pragma Ada_2022;
with ACPI_Native_Discovery_Budget;
with ACPI_Service;
with ACPI_Demand_Refresh;
with ACPI_Source_Path_Reader;
with ACPI_Requests.With_Retained_Notification;
with ACPI_Native_Power_Inventory;
with ACPI_One_Service;
with ACPI_Clock_Source;
with ACPI_Lid;with ACPI_Power_Source;
with ACPI_Requests.Lids;with ACPI_Requests.Lids.With_Local_Events;
with ACPI_Requests.Power_Sources;with ACPI_Requests.Power_Sources.With_Local_Events;
with ACPI_Battery;with ACPI_Battery_Information;with ACPI_Battery_Information_Reader;
with ACPI_Battery_Legacy_Information;with ACPI_Battery_Legacy_Information_Reader;
with ACPI_Battery_Information_Selection;with ACPI_Battery_Observation;with ACPI_Battery_Inventory;
with ACPI_Requests.Battery_Observations;with ACPI_Requests.Battery_Observations.With_Local_Events;
with ACPI_HID_I2C_Candidate;with ACPI_HID_I2C_Discovery;with ACPI_HID_I2C_Resolved;with ACPI_HID_Inventory;
with ACPI_Requests.HID_Devices;with ACPI_Requests.HID_Devices.With_Local_Events;
with ACPI_Local_Sample_Status;with AML_Execute;
package body ACPI_Native_Consumers is
   package C renames ACPI_Service;package Q renames ACPI_Requests;package One renames ACPI_One_Service;
   package Status renames ACPI_Local_Sample_Status;
   use type ACPI_Source_Paths.Status,ACPI_Cached_Power_Values.Generation;
   use type C.Values.Access_Status, AML_Execute.Execution_Status, C.Node_Descriptor;
   use type C.Namespace.Notification_Target_Kind;
   use type Interfaces.Unsigned_64;
   Evaluation_Failed : Boolean := False;
   Evaluation_Method : String (1 .. 4) := "NONE";
   Evaluation_Access : C.Values.Access_Status := C.Values.Wrong_Phase;
   Evaluation_Result : AML_Execute.Execution_Status := AML_Execute.Unsupported_Value;
   Evaluation_Charge : Natural := 0;
   procedure Note_Evaluation (Method : String; Result : C.Values.Result; Access_Result : C.Values.Access_Status) is
   begin
      if Evaluation_Failed then return; end if;
      Evaluation_Method := Method; Evaluation_Access := Access_Result;
      Evaluation_Result := Result.Status; Evaluation_Charge := Result.Charged;
      Evaluation_Failed := Access_Result /= C.Values.Available or else Result.Status not in
        AML_Execute.Returned | AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
        AML_Execute.Metadata_Returned | AML_Execute.No_Return;
   end Note_Evaluation;
   procedure Evaluate_Sample (Service : aliased in out C.State; Item : C.Node_Descriptor; Budget : Natural;
      Result : out C.Values.Result; Access_Result : out C.Values.Access_Status) is
      Method : constant String := C.Values.Node_Name (Item);
   begin
      One.Runtime.Evaluate_Sample (Service, Item, Budget, Result, Access_Result);
      Note_Evaluation (Method, Result, Access_Result);
   end Evaluate_Sample;
   procedure Invoke_Sample (Service : aliased in out C.State; Node : C.Namespace.Node_ID;
      Args : C.Values.Arguments; Argument_Count : C.Values.Argument_Count; Budget : Natural;
      Result : out C.Values.Result; Access_Result : out C.Values.Access_Status) is
   begin
      One.Runtime.Invoke_Sample (Service, Node, Args, Argument_Count, Budget, Result, Access_Result);
      Note_Evaluation ("ARGS", Result, Access_Result);
   end Invoke_Sample;
   package LD is new ACPI_Lid(C,Evaluate_Sample);
   package PD is new ACPI_Power_Source(C,Evaluate_Sample);
   procedure Remember_Source (Device : C.Node_Descriptor);
   package Lids is new Q.Lids(LD);
   package Sources is new Q.Power_Sources(PD,True,Remember_Source);
   package Format is new ACPI_Battery_Information;
   package Dynamic is new ACPI_Battery(C,Evaluate_Sample);
   package Extended_Info is new ACPI_Battery_Information_Reader(C,Format,Evaluate_Sample);
   package Legacy_Format is new ACPI_Battery_Legacy_Information;
   package Legacy_Info is new ACPI_Battery_Legacy_Information_Reader(C,Legacy_Format,Evaluate_Sample);
   package Static_Info is new ACPI_Battery_Information_Selection(C,Format,Legacy_Format,Extended_Info,Legacy_Info);
   package Collector is new ACPI_Battery_Observation(C,Format,Dynamic,Static_Info);
   package Batteries is new Q.Battery_Observations(Dynamic,Collector);
   Maximum_Batteries : constant Positive := Maximum_Battery_Facts;
   package BI is new ACPI_Battery_Inventory(Collector,Maximum_Batteries);
   Maximum_Devices : constant Positive := Maximum_HID_Facts;
   Maximum_Source_Bytes : constant Positive := 256;
   Maximum_Resource_Bytes : constant Positive := 65_536;
   package Candidates is new ACPI_HID_I2C_Candidate(Maximum_Source_Bytes);
   package Discovery is new ACPI_HID_I2C_Discovery(C,Invoke_Sample,Evaluate_Sample);
   package Resolver is new ACPI_HID_I2C_Resolved(C,Candidates);
   package Hids is new Q.HID_Devices(Discovery,Resolver);
   package HI is new ACPI_HID_Inventory(Resolver,Maximum_Devices);
   use type One.Runtime.Status.Phase,Status.Outcome,Status.Notice;
   use type Lids.Samples.Availability,Sources.Samples.Availability,BI.Samples.Availability,HI.Samples.Availability;
   use type BI.Availability,BI.Observation;
   use type Sources.Cached.Sample;
   use type Collector.Outcome, Static_Info.Source_Kind;
   Fuel : constant Positive := 100_000;
   Window : constant Status.Window_Allowance := ACPI_Native_Discovery_Budget.Window_Microseconds;
   package Demands is new ACPI_Demand_Refresh;
   Battery_Demand:Demands.State;
   Last : Report;
   Attempted,Working : Boolean := False;
   Pending,Runnable : array(Family) of Boolean := [others=>False];
   Suspended : array (Family) of Boolean := [others => False];
   Next_Family : Family := Lid;
   Lid_Value:Lids.Samples.View;Source_Value:Sources.Samples.View;
   Battery_Value:BI.Samples.View;HID_Value:HI.Samples.View;
   Kinds:array(Family) of Status.Notice:=[others=>Status.No_Device];
   Battery_Cursor:BI.Slot:=BI.Slot'First;HID_Cursor:HI.Slot:=HI.Slot'First;
   function Observe return Report is (Last);
   function Facts_Pending return Boolean is (for some F in Family=>Pending(F));
   function Healthy return Boolean is
     (Last.Why=None and then Sources_Healthy);
   function Owner_Quiet(Server:Q.State) return Boolean is
     (not Q.Notifications_Pending(Server) and then not Q.Retained_Results_Pending(Server)
      and then One.Runtime.Current not in One.Runtime.Status.Fresh|One.Runtime.Status.Active
      and then One.Root.Fields.Control_Idle);
   procedure Capture(F:Family;Kind:Status.Notice;Accepted:out Boolean) is
   begin
      Accepted:=False;
      if Pending(F) then Last.Why:=Publication_Overlap;Last.At_Family:=F;return;end if;
      Pending(F):=True;Kinds(F):=Kind;Accepted:=True;
   end Capture;
   procedure Read_Lid_Clock(Now:out Lids.Samples.Tick;Available:out Boolean) is
      Value:Interfaces.Unsigned_64;
   begin ACPI_Clock_Source.Read(Value,Available);Now:=Lids.Samples.Tick(Value);end Read_Lid_Clock;
   procedure Read_Source_Clock(Now:out Sources.Samples.Tick;Available:out Boolean) is
      Value:Interfaces.Unsigned_64;
   begin ACPI_Clock_Source.Read(Value,Available);Now:=Sources.Samples.Tick(Value);end Read_Source_Clock;
   procedure Read_Battery_Clock(Now:out BI.Samples.Tick;Available:out Boolean) is
      Value:Interfaces.Unsigned_64;
   begin ACPI_Clock_Source.Read(Value,Available);Now:=BI.Samples.Tick(Value);end Read_Battery_Clock;
   procedure Read_HID_Clock(Now:out HI.Samples.Tick;Available:out Boolean) is
      Value:Interfaces.Unsigned_64;
   begin ACPI_Clock_Source.Read(Value,Available);Now:=HI.Samples.Tick(Value);end Read_HID_Clock;
   -- Fast copied-fact sinks only. No IPC, rendering, Core mutation or callback.
   -- Descriptive discovery roster. Never invokes a remembered device. Every
   -- match below revalidates its binding against the same retained Core.
   package Paths renames ACPI_Source_Paths;
   package Identity_Wire renames ACPI_Power_Identity_Protocol;
   package Path_Reader is new ACPI_Source_Path_Reader(C);
   type Identity_Entry is record Device:C.Node_Descriptor:=C.Values.No_Node;Value:Paths.Path:=Paths.Empty;end record;
   type Identity_List is array(Natural range 0..Maximum_Batteries)of Identity_Entry;
   Identities:Identity_List;
   Source_Identity_Device:C.Node_Descriptor:=C.Values.No_Node;
   Source_Identity_Revision,Battery_Identity_Revision:ACPI_Cached_Power_Values.Generation:=0;
   Identity_Battery_Count:Natural range 0..Maximum_Batteries:=0;
   type Power_Devices is array (C.Namespace.Node_ID) of C.Node_Descriptor;
   Known_Power : Power_Devices := [others=>C.Values.No_Node];
   Source_Device : array (C.Namespace.Node_ID) of Boolean := [others=>False];
   Source_Batch,Battery_Batch : Boolean := False;
   procedure Clear_Devices (Sources : Boolean) is
   begin
      for I in Known_Power'Range loop
         if Source_Device(I)=Sources then Known_Power(I):=C.Values.No_Node;end if;
      end loop;
   end Clear_Devices;
   procedure Remember_Battery (Device : C.Node_Descriptor) is
      I : constant C.Namespace.Node_ID:=C.Values.Position(Device);
   begin
      if not Battery_Batch then Clear_Devices(False);Battery_Batch:=True;end if;
      Known_Power(I):=Device;Source_Device(I):=False;
   end Remember_Battery;
   procedure Remember_Source (Device : C.Node_Descriptor) is
      I : constant C.Namespace.Node_ID:=C.Values.Position(Device);
   begin
      if not Source_Batch then Clear_Devices(True);Source_Batch:=True;Source_Identity_Device:=Device;end if;
      Known_Power(I):=Device;Source_Device(I):=True;
   end Remember_Source;
   Power_Dirty : array (Family range AC_Source..Battery) of Boolean := [others=>False];
   Power_Published : array (Family range AC_Source..Battery) of Boolean := [others=>False];
   procedure Report_Lid(Server:Q.State;Kind:Status.Notice;Value:Lids.Samples.View;Flags:Interfaces.Unsigned_64) is
      pragma Unreferenced(Server,Flags);Accepted:Boolean;
   begin Capture(Lid,Kind,Accepted);if Accepted then Lid_Value:=Value;end if;end Report_Lid;
   procedure Report_Source(Server:Q.State;Kind:Status.Notice;Value:Sources.Samples.View;Flags:Interfaces.Unsigned_64) is
      pragma Unreferenced(Server,Flags);Accepted:Boolean;
   begin
      if Kind=Status.No_Device then Clear_Devices(True);end if;
      Capture(AC_Source,Kind,Accepted);
      if Accepted then
         if not Power_Published(AC_Source) or else Power_Dirty(AC_Source)
           or else Value.Status/=Source_Value.Status or else Value.Value/=Source_Value.Value then
            ACPI_Native_Power_Inventory.Changed;
         end if;
         Power_Published(AC_Source):=True;Power_Dirty(AC_Source):=False;Source_Value:=Value;
      end if;
   end Report_Source;
   procedure Report_Battery(Server:Q.State;Kind:Status.Notice;Value:BI.Samples.View;Flags:Interfaces.Unsigned_64) is
      pragma Unreferenced(Server,Flags);Accepted:Boolean;
   begin
      if Kind=Status.No_Device or else Value.Value.Count=0 then Clear_Devices(False);end if;
      Capture(Battery,Kind,Accepted);
      if Accepted then
         Demands.Complete(Battery_Demand);
         if not Power_Published(Battery) or else Power_Dirty(Battery)
           or else Value.Status/=Battery_Value.Status or else Value.Value/=Battery_Value.Value then
            ACPI_Native_Power_Inventory.Changed;
         end if;
         Power_Published(Battery):=True;Power_Dirty(Battery):=False;
         Battery_Value:=Value;Battery_Cursor:=BI.Slot'First;
      end if;
   end Report_Battery;
   procedure Report_HID(Server:Q.State;Kind:Status.Notice;Value:HI.Samples.View;Flags:Interfaces.Unsigned_64) is
      pragma Unreferenced(Server,Flags);Accepted:Boolean;
   begin Capture(HID_I2C,Kind,Accepted);if Accepted then HID_Value:=Value;HID_Cursor:=HI.Slot'First;end if;end Report_HID;
   package LE is new Lids.With_Local_Events(One.Runtime.Current,One.Runtime_Access.Begin_Event,One.Runtime_Access.End_Event,
      Read_Lid_Clock,Report_Lid,Healthy,Fuel,Window);
   package PE is new Sources.With_Local_Events(One.Runtime.Current,One.Runtime_Access.Begin_Event,One.Runtime_Access.End_Event,
      Read_Source_Clock,Report_Source,Healthy,Fuel,Window);
   package BE is new Batteries.With_Local_Events(BI,One.Runtime.Current,One.Runtime_Access.Begin_Event,One.Runtime_Access.End_Event,
      Read_Battery_Clock,Report_Battery,Healthy,Fuel,Window,Remember_Battery);
   package HE is new Hids.With_Local_Events(HI,One.Runtime.Current,One.Runtime_Access.Begin_Event,One.Runtime_Access.End_Event,
      Read_HID_Clock,Report_HID,Healthy,Maximum_Resource_Bytes,Fuel,Window);
   procedure Capture_Refusal (F : Family; Isolated : Boolean) is
      procedure Copy (Index : Positive; Text : String) is
         N : constant Natural := Natural'Min (Text'Length, Last.Diagnostics (Index).Text'Length);
      begin
         Last.Diagnostics (Index).Last := N;
         Last.Diagnostics (Index).Text (1 .. N) := Text (Text'First .. Text'First + N - 1);
      end Copy;
   begin
      if Last.Refusal_Count < Natural'Last then Last.Refusal_Count := Last.Refusal_Count + 1; end if;
      Last.Diagnostics := [others => <>];
      Copy (1, "acpi: sample refusal family=" & F'Image & " isolated=" & Isolated'Image &
         " runtime=" & One.Runtime.Current'Image & " owner-guard=" & One.Runtime.Observation'Image);
      Copy (2, "acpi: sample evaluation method=" & Evaluation_Method & " failed=" & Evaluation_Failed'Image &
         " access=" & Evaluation_Access'Image & " execution=" & Evaluation_Result'Image & " charged=" & Evaluation_Charge'Image);
      declare
         Command:constant One.Root.Fields.EC_Commands.Observation:=One.Root.Fields.EC_Commands.Observe;
         Engine:constant One.Root.Fields.EC_Commands.T.Transaction_Observation:=One.Root.Fields.EC_Commands.Engine_Observation;
      begin
         Copy(5,"acpi: sample ec owner=" & Command.Stage'Image & " why=" & Command.Why'Image &
           " recovery=" & Command.Recovery'Image & " effects=" & Command.Effects_Possible'Image & " engine=" & Engine.Status'Image);
         Copy(6,"acpi: sample ec clock deadline-us=" & Engine.Deadline'Image & " sample-us=" & Engine.Last_Sample'Image &
           " status=" & Engine.Status_Result'Image & " known=" & Engine.Status_Known'Image & " byte=" & Engine.Last_Status'Image);
      end;
      if F = Battery then
         declare D : constant BE.Collection_Diagnostic := BE.Collection_Report; begin
            Copy (3, "acpi: battery guard=" & D.Outcome'Image & " count=" & D.Count'Image & " charged=" & D.Charged'Image &
               " search=" & D.Search.Outcome'Image & " identity=" & D.Search.Last_Identity.Outcome'Image);
            Copy (4, "acpi: battery probe=" & D.Probe.Status'Image & " bst=" & D.Probe.Dynamic_Data.Execution'Image &
               " bix=" & D.Probe.Static_Data.Extended_Data.Execution'Image & " bif=" & D.Probe.Static_Data.Legacy_Data.Execution'Image);
         end;
      end if;
   end Capture_Refusal;
   procedure Read_Power (Server : Q.State; Maximum_Age : ACPI_Cached_Power_Values.Microseconds;
      Value : out ACPI_Cached_Power_Values.Snapshot) is
      package V renames ACPI_Cached_Power_Values;
      Now : Interfaces.Unsigned_64; Clock_Available : Boolean;
      LV : Lids.Samples.View; PV : Sources.Samples.View;
      function Convert (Item : Lids.Samples.Availability) return V.Availability is
        (case Item is
           when Lids.Samples.Not_Ready => V.Not_Ready, when Lids.Samples.Available => V.Available,
           when Lids.Samples.Missing => V.Missing, when Lids.Samples.Failure => V.Failure,
           when Lids.Samples.Too_Old => V.Too_Old, when Lids.Samples.Clock_Unknown => V.Clock_Unknown,
           when Lids.Samples.Clock_Regressed => V.Clock_Regressed,
           when Lids.Samples.Counter_Exhausted => V.Counter_Exhausted);
      function Convert (Item : Sources.Samples.Availability) return V.Availability is
        (case Item is
           when Sources.Samples.Not_Ready => V.Not_Ready, when Sources.Samples.Available => V.Available,
           when Sources.Samples.Missing => V.Missing, when Sources.Samples.Failure => V.Failure,
           when Sources.Samples.Too_Old => V.Too_Old, when Sources.Samples.Clock_Unknown => V.Clock_Unknown,
           when Sources.Samples.Clock_Regressed => V.Clock_Regressed,
           when Sources.Samples.Counter_Exhausted => V.Counter_Exhausted);
   begin
      Value := (others => <>);
      if Working or else not Last.Started or else Last.Stopping or else Last.Closed
        or else not Healthy or else not Owner_Quiet (Server)
        or else One.Runtime.Current /= One.Runtime.Status.Idle then return; end if;
      ACPI_Clock_Source.Read (Now, Clock_Available);
      LV := LE.Read (Server, (Clock_Available, Lids.Samples.Tick (Now)), Lids.Samples.Tick (Maximum_Age));
      PV := PE.Read (Server, (Clock_Available, Sources.Samples.Tick (Now)), Sources.Samples.Tick (Maximum_Age));
      Value.Lid := (V.Generation (LV.Revision), Convert (LV.Status),
        (LV.Time.Available, V.Microseconds (LV.Time.Value)), LV.Value);
      Value.Source := (V.Generation (PV.Revision), Convert (PV.Status),
        (PV.Time.Available, V.Microseconds (PV.Time.Value)), PV.Value);
   end Read_Power;
   procedure Read_Batteries (Server : Q.State; Maximum_Age : ACPI_Cached_Power_Values.Microseconds;
      Value : out ACPI_Cached_Battery_Values.View) is
      package V renames ACPI_Cached_Power_Values;
      package B renames ACPI_Cached_Battery_Values;
      Now : Interfaces.Unsigned_64;
      Clock_Available : Boolean;
      Cached : BI.Samples.View;
      function Convert (Item : BI.Samples.Availability) return V.Availability is
        (case Item is
           when BI.Samples.Not_Ready => V.Not_Ready, when BI.Samples.Available => V.Available,
           when BI.Samples.Missing => V.Missing, when BI.Samples.Failure => V.Failure,
           when BI.Samples.Too_Old => V.Too_Old, when BI.Samples.Clock_Unknown => V.Clock_Unknown,
           when BI.Samples.Clock_Regressed => V.Clock_Regressed,
           when BI.Samples.Counter_Exhausted => V.Counter_Exhausted);
      function Convert (Item : Collector.Outcome) return B.Acquisition_Reason is
        (case Item is
           when Collector.Sampled => B.No_Failure, when Collector.Not_Battery => B.Not_Battery,
           when Collector.Absent => B.Absent, when Collector.Not_Ready => B.Not_Ready,
           when Collector.Identification_Failed => B.Identification_Failed,
           when Collector.Presence_Failed => B.Presence_Failed,
           when Collector.Information_Failed => B.Information_Failed,
           when Collector.Status_Failed => B.Status_Failed,
           when Collector.Presence_Changed => B.Presence_Changed,
           when Collector.Budget_Exhausted => B.Budget_Exhausted,
           when Collector.Cleanup_Failed => B.Cleanup_Failed,
           when Collector.Provider_Contract_Error => B.Provider_Contract_Error);
   begin
      Value := (others => <>);
      if Working or else not Last.Started or else Last.Stopping or else Last.Closed
        or else not Healthy or else not Owner_Quiet (Server)
        or else One.Runtime.Current /= One.Runtime.Status.Idle then return; end if;
      ACPI_Clock_Source.Read (Now, Clock_Available);
      Cached := BE.Read (Server, (Clock_Available, BI.Samples.Tick (Now)), BI.Samples.Tick (Maximum_Age));
      Value.Revision := V.Generation (Cached.Revision);
      Value.Status := Convert (Cached.Status);
      Value.Time := (Cached.Time.Available, V.Microseconds (Cached.Time.Value));
      if Cached.Status /= BI.Samples.Available then return; end if;
      Value.Count := B.Battery_Count (Cached.Value.Count);
      for Index in 1 .. Cached.Value.Count loop
         declare
            Item : BI.Entry_Value renames Cached.Value.Items (Index);
            Target : B.Entry_Value renames Value.Items (B.Slot (Index));
            Common : constant Static_Info.Common_Information := Static_Info.Common (Item.Value.Information);
         begin
            Target.Reason := Convert (Item.Reason);
            case Item.Status is
               when BI.Known_Absent =>
                  if Item.Reason = Collector.Absent then Target.Status := B.Known_Absent;
                  else Target.Reason := B.Provider_Contract_Error; end if;
               when BI.Acquisition_Failed => Target.Status := B.Acquisition_Failed;
               when BI.Sampled =>
                  if Item.Reason /= Collector.Sampled or else Common.Source = Static_Info.None_Reported then
                     Target.Reason := B.Provider_Contract_Error;
                  else
                     Target.Status := B.Sampled;
                     Target.Value.Source := (case Common.Source is
                        when Static_Info.None_Reported => B.None_Reported,
                        when Static_Info.Extended_Reported => B.Extended_Reported,
                        when Static_Info.Legacy_Reported => B.Legacy_Reported);
                     Target.Value.Units := (case Common.Units is
                        when Static_Info.Unspecified_Unit => B.Unspecified_Unit,
                        when Static_Info.Energy_And_Power => B.Energy_And_Power,
                        when Static_Info.Charge_And_Current => B.Charge_And_Current);
                     Target.Value.Design_Capacity :=
                       (Common.Design.Known, (if Common.Design.Known then Common.Design.Value else 0));
                     Target.Value.Last_Full_Capacity :=
                       (Common.Last_Full.Known, (if Common.Last_Full.Known then Common.Last_Full.Value else 0));
                     Target.Value.Design_Voltage :=
                       (Common.Voltage.Known, (if Common.Voltage.Known then Common.Voltage.Value else 0));
                     Target.Value.Dynamic := Item.Value.Status;
                  end if;
            end case;
         end;
      end loop;
   end Read_Batteries;
   function Demand_Allowed(Server:Q.State) return Boolean is
     (Last.Started and then not Working and then not Last.Stopping and then not Last.Closed
      and then Last.Why=None and then not Suspended(Battery) and then Sources_Healthy
      and then Owner_Quiet(Server) and then One.Runtime.Current=One.Runtime.Status.Idle
      and then BE.Owns_Context(Server));
   procedure Preview_Battery_Refresh(Server:Q.State;Now:out Interfaces.Unsigned_64;Allowed:out Boolean) is
      Known:Boolean;
   begin
      Now:=0;Allowed:=False;if not Demand_Allowed(Server) then return;end if;
      ACPI_Clock_Source.Read(Now,Known);Allowed:=Demands.Can_Request(Battery_Demand,Now,Known);
   end Preview_Battery_Refresh;
   procedure Commit_Battery_Refresh(Server:Q.State;Now:Interfaces.Unsigned_64;
      Ticket:out Interfaces.Unsigned_64;Accepted:out Boolean) is
      Scheduled,New_Attempt:Boolean;
   begin
      Ticket:=0;Accepted:=False;
      if not Demand_Allowed(Server) or else not Demands.Can_Request(Battery_Demand,Now,True) then return;end if;
      if not Demands.Pending(Battery_Demand) then
         BE.Request_Refresh(Server,(True,BI.Samples.Tick(Now)),Scheduled);if not Scheduled then return;end if;
      end if;
      Demands.Request(Battery_Demand,Now,True,Ticket,New_Attempt,Accepted);
      if Accepted then Runnable(Battery):=True;end if;
   end Commit_Battery_Refresh;
   function Battery_Refresh_Ready(Ticket:Interfaces.Unsigned_64) return Boolean is
     (Demands.Ready(Battery_Demand,Ticket));
   procedure Capture_Identities(Server:Q.State;F:Family) is
      Good:Boolean;BV:BI.Samples.View;PV:Sources.Samples.View;
      procedure Copy(Service:C.State;Completed:out Boolean) is
      begin
         Completed:=True;
         if F=AC_Source then
            Identities(0):=(Source_Identity_Device,Paths.Empty);
            Path_Reader.Read(Service,Source_Identity_Device,Identities(0).Value);
            Source_Identity_Revision:=ACPI_Cached_Power_Values.Generation(PV.Revision);
         elsif F=Battery then
            Identity_Battery_Count:=Natural(BV.Value.Count);
            for I in 1..Maximum_Batteries loop
               Identities(I):=(others=><>);
               if I<=Identity_Battery_Count then
                  Identities(I).Device:=BV.Value.Items(I).Device;
                  Path_Reader.Read(Service,Identities(I).Device,Identities(I).Value);
               end if;
            end loop;
            Battery_Identity_Revision:=ACPI_Cached_Power_Values.Generation(BV.Revision);
         end if;
      end Copy;
      package Metadata is new Q.With_Metadata_Continuation(Copy);
   begin
      if F=AC_Source then Source_Identity_Revision:=0;Identities(0):=(others=><>);
      elsif F=Battery then Battery_Identity_Revision:=0;Identity_Battery_Count:=0;
      else return;end if;
      if not Owner_Quiet(Server) or else One.Runtime.Current/=One.Runtime.Status.Idle then return;end if;
      if F=AC_Source then
         PV:=PE.Metadata_View(Server);
         if PV.Status not in Sources.Samples.Available|Sources.Samples.Missing|Sources.Samples.Failure then return;end if;
      else
         BV:=BE.Metadata_View(Server);
         if BV.Status/=BI.Samples.Available then return;end if;
      end if;
      Metadata.Advance(Server,Good);
      if not Good then
         if F=AC_Source then Source_Identity_Revision:=0;else Battery_Identity_Revision:=0;Identity_Battery_Count:=0;end if;
      end if;
   end Capture_Identities;
   procedure Read_Identity(Server:Q.State;Index:Natural;Expected:ACPI_Cached_Power_Values.Generation;
      Current:out ACPI_Cached_Power_Values.Generation;Code:out Identity_Wire.Result_Code;Value:out Paths.Path) is
      Good:Boolean;Fresh:Paths.Path;
      procedure Check(Service:C.State;Completed:out Boolean) is
      begin Path_Reader.Read(Service,Identities(Index).Device,Fresh);Completed:=True;end Check;
      package Metadata is new Q.With_Metadata_Continuation(Check);
   begin
      Current:=0;Code:=Identity_Wire.Not_Ready;Value:=Paths.Empty;
      if Index>Maximum_Batteries or else Working or else Last.Stopping or else Last.Closed or else not Last.Started
        or else Last.Why/=None or else not Sources_Healthy or else not Owner_Quiet(Server)
        or else One.Runtime.Current/=One.Runtime.Status.Idle then return;end if;
      if Index=0 then
         Current:=ACPI_Cached_Power_Values.Generation(PE.Metadata_Revision(Server));
      else Current:=ACPI_Cached_Power_Values.Generation(BE.Metadata_Revision(Server));end if;
      if Current=0 then return;end if;
      if Expected/=Current then Code:=Identity_Wire.Stale;return;end if;
      if Index=0 then
         if Current/=Source_Identity_Revision then return;end if;
      else
         if Current/=Battery_Identity_Revision then return;end if;
         if Index>Identity_Battery_Count then Code:=Identity_Wire.Index_Out_Of_Range;return;end if;
      end if;
      Metadata.Advance(Server,Good);if not Good then return;end if;
      if Identities(Index).Value.State=Paths.Too_Long and Fresh.State=Paths.Too_Long then Code:=Identity_Wire.Too_Long;return;end if;
      if not Paths.Same(Identities(Index).Value,Fresh) then return;end if;
      Value:=Identities(Index).Value;Code:=Identity_Wire.OK;
   end Read_Identity;
   procedure Start(Server:Q.State;Accepted:out Boolean) is
      L,P,B,H:Boolean;
   begin
      Accepted:=False;if Attempted or Working or Last.Stopping then return;end if;Attempted:=True;
      if Q.Retained_Results_Pending(Server) or else not One.Root.Fields.Control_Idle
        or else One.Runtime.Current/=One.Runtime.Status.Idle or else not Sources_Healthy then Last.Why:=Not_Ready;return;end if;
      Working:=True;LE.Start(Server,L);PE.Start(Server,P);BE.Start(Server,B);HE.Start(Server,H);
      Last.Started:=L and P and B and H;Accepted:=Last.Started;
      if not Accepted then Last.Why:=Start_Refused;else Runnable:=[others=>True];end if;
      Working:=False;
   end Start;
   Last_Power_Receipt : Q.Receipt_Number := 0;
   type Power_Route_Token is limited null record;
   Power_Token : Power_Route_Token;
   procedure Observe_Power_Notice (Token : in out Power_Route_Token; Service : C.State;
      ID : Q.Receipt_Number; Event : C.Notification_Observation; Accepted : out Boolean) is
      pragma Unreferenced(Token);
   begin
      Accepted:=False;
      if ID=0 or ID<Last_Power_Receipt then return;end if;
      if ID=Last_Power_Receipt then Accepted:=True;return;end if;
      if Event.Current_Target and then Event.Target_Kind=C.Namespace.Device_Notification
        and then Event.Number in 16#80#|16#81# then
         declare I:constant C.Namespace.Node_ID:=Event.Node;begin
            if Known_Power(I)/=C.Values.No_Node and then C.Current_Binding(Service,Known_Power(I)) then
               ACPI_Native_Power_Inventory.Changed;
               Power_Dirty((if Source_Device(I) then AC_Source else Battery)):=True;
            end if;
         end;
      end if;
      Last_Power_Receipt:=ID;Accepted:=True;
   end Observe_Power_Notice;
   package Power_Route is new Q.With_Retained_Notification(Power_Route_Token,Observe_Power_Notice);
   procedure Route(Server:Q.State;Routed:out Boolean) is
      L,P,B,H,W:Boolean;
   begin
      Routed:=False;if Working or not Last.Started or Last.Closed then return;end if;
      -- Every route is attempted, including after an earlier refusal. ACK is
      -- authorized only by their conjunction in the caller's private broker.
      Power_Route.Route(Power_Token,Server,W);
      LE.Route(Server,L);PE.Route(Server,P);BE.Route(Server,B);HE.Route(Server,H);
      Routed:=L and P and B and H and W;
      if Routed and not Last.Stopping then
         for F in Family loop Runnable (F) := not Suspended (F); end loop;
      end if;
   end Route;
   procedure Advance(Server:in out Q.State;Result:out ACPI_Native_Server.Owner_Outcome) is
      Outcome:Status.Outcome;F:Family:=Next_Family;Found:Boolean:=False;
   begin
      Result:=ACPI_Native_Server.Owner_Refused;
      if Working or not Last.Started or Last.Why/=None then return;end if;
      if Last.Stopping then Result:=ACPI_Native_Server.Owner_Idle;return;end if;
      if not Sources_Healthy then Last.Why:=Source_Unhealthy;return;end if;
      if Q.Notifications_Pending(Server) or Facts_Pending then Result:=ACPI_Native_Server.Owner_Pending;return;end if;
      if not Owner_Quiet(Server) or One.Runtime.Current/=One.Runtime.Status.Idle then Last.Why:=Not_Ready;return;end if;
      for Step in Family loop
         if Runnable(F) then Found:=True;exit;end if;
         F:=(if F=Family'Last then Family'First else Family'Succ(F));
      end loop;
      if not Found then Result:=ACPI_Native_Server.Owner_Idle;return;end if;
      Working:=True;Last.At_Family:=F;Evaluation_Failed:=False;Evaluation_Method:="NONE";
      Evaluation_Access:=C.Values.Wrong_Phase;Evaluation_Result:=AML_Execute.Unsupported_Value;Evaluation_Charge:=0;
      case F is
         when Lid=>LE.Advance(Server,Outcome);
         when AC_Source=>Source_Batch:=False;PE.Advance(Server,Outcome);
         when Battery=>Battery_Batch:=False;BE.Advance(Server,Outcome);
         when HID_I2C=>HE.Advance(Server,Outcome);
      end case;
      Working:=False;
      if F in AC_Source|Battery and then Pending(F) then Capture_Identities(Server,F);end if;
      Next_Family:=(if F=Family'Last then Family'First else Family'Succ(F));
      if Last.Why/=None then return;end if;
      if Outcome=Status.Isolated then
         -- The family closed its unchanged owner window before suspending.
         -- It still routes every receipt, but is never resampled implicitly.
         if One.Runtime.Current/=One.Runtime.Status.Idle or else not Owner_Quiet(Server) or else not Sources_Healthy then
            Capture_Refusal(F,False);Last.Why:=Family_Refused;return;
         end if;
         Suspended(F):=True;Capture_Refusal(F,True);
         if F in AC_Source|Battery then ACPI_Native_Power_Inventory.Changed;end if;
         if F=Battery then Demands.Complete(Battery_Demand);end if;
      elsif Outcome=Status.Refused then Capture_Refusal(F,False);Last.Why:=Family_Refused;return;end if;
      Runnable(F):=Outcome=Status.Pending;
      Result:=(if Facts_Pending or else (for some Kind in Family=>Runnable(Kind))
         then ACPI_Native_Server.Owner_Pending else ACPI_Native_Server.Owner_Idle);
   end Advance;
   function Image(N:Interfaces.Unsigned_64) return String is
      Text:constant String:=N'Image;
   begin return Text(Text'First+1..Text'Last);end Image;
   procedure Take_Fact(Server:Q.State;Line:out Fact_Line;Last:out Fact_Length;Present:out Boolean) is
      More:Boolean:=False;
      procedure Emit(Text:String) is
      begin
         if Text'Length>Line'Length then ACPI_Native_Consumers.Last.Why:=Fact_Too_Long;return;end if;
         Last:=Text'Length;Line(1..Last):=Text;Present:=True;
      end Emit;
   begin
      Line:=[others=>' '];Last:=0;Present:=False;
      if Working or not Owner_Quiet(Server) then return;end if;
      for F in Family loop
         if Pending(F) then
            case F is
               when Lid=>
                  if Kinds(F)=Status.Sample_Available and Lid_Value.Status=Lids.Samples.Available then Emit("acpi: lid "&Lid_Value.Value.State'Image);
                  else Emit("acpi: lid "&Kinds(F)'Image);end if;
               when AC_Source=>
                  if Kinds(F)=Status.Sample_Available and Source_Value.Status=Sources.Samples.Available then Emit("acpi: AC "&Source_Value.Value.State'Image);
                  else Emit("acpi: AC "&Kinds(F)'Image);end if;
               when Battery=>
                  if Kinds(F)=Status.Sample_Available and Battery_Value.Status=BI.Samples.Available and Battery_Value.Value.Count>0 then
                     declare Item:BI.Entry_Value renames Battery_Value.Value.Items(Battery_Cursor);begin
                        if Item.Status=BI.Sampled then
                           declare Info:constant Static_Info.Common_Information:=Static_Info.Common(Item.Value.Information);begin
                              Emit("acpi: battery "&Image(Interfaces.Unsigned_64(Battery_Cursor))&" "&Item.Value.Status.Flow'Image&
                                 " remaining="&(if Item.Value.Status.Remaining_Capacity.Known then Image(Interfaces.Unsigned_64(Item.Value.Status.Remaining_Capacity.Value)) else "UNKNOWN")&
                                 " full="&(if Info.Last_Full.Known then Image(Interfaces.Unsigned_64(Info.Last_Full.Value)) else "UNKNOWN")&" units="&Info.Units'Image);
                           end;
                        else Emit("acpi: battery "&Image(Interfaces.Unsigned_64(Battery_Cursor))&" "&Item.Status'Image&" reason="&Item.Reason'Image);end if;
                     end;
                     More:=Natural(Battery_Cursor)<Natural(Battery_Value.Value.Count);
                     if Present and More then Battery_Cursor:=Battery_Cursor+1;end if;
                  else Emit("acpi: battery "&Kinds(F)'Image&" count="&Image(Interfaces.Unsigned_64(Battery_Value.Value.Count)));end if;
               when HID_I2C=>
                  if Kinds(F)=Status.Sample_Available and HID_Value.Status=HI.Samples.Available and HID_Value.Value.Count>0 then
                     declare Item:constant Candidates.Candidate:=Resolver.Facts(HID_Value.Value.Items(HID_Cursor));begin
                        Emit("acpi: HID-I2C candidate "&Image(Interfaces.Unsigned_64(HID_Cursor))&" addr="&Image(Interfaces.Unsigned_64(Item.I2C.Address))&
                           " descriptor="&Image(Interfaces.Unsigned_64(Item.Descriptor_Register))&" gpio="&Image(Interfaces.Unsigned_64(Item.Pin)));
                     end;
                     More:=Natural(HID_Cursor)<Natural(HID_Value.Value.Count);
                     if Present and More then HID_Cursor:=HID_Cursor+1;end if;
                  else Emit("acpi: HID-I2C "&Kinds(F)'Image&" count="&Image(Interfaces.Unsigned_64(HID_Value.Value.Count)));end if;
            end case;
            if Present and not More then Pending(F):=False;end if;return;
         end if;
      end loop;
   end Take_Fact;
   procedure Begin_Stop is begin Demands.Begin_Stop(Battery_Demand);Last.Stopping:=True;end Begin_Stop;
   function Quiet(Server:Q.State) return Boolean is
     (Last.Closed and then not Working and then not Facts_Pending and then Owner_Quiet(Server));
   procedure Close(Server:Q.State;Completed:out Boolean) is
   begin
      Completed:=False;Begin_Stop;
      if Working or Facts_Pending or not Owner_Quiet(Server) then return;end if;
      LE.Stop;PE.Stop;BE.Stop;HE.Stop;Last.Closed:=True;Completed:=Quiet(Server);
   end Close;
end ACPI_Native_Consumers;
