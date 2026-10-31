pragma Ada_2022;
with Firmware_Tables.DMAR;
with Firmware_Tables.MCFG;
with Firmware_Tables.MADT;
with Firmware_Tables.SRAT;
with Firmware_Tables.SLIT;
with ACPI_Service;
with Firmware_Tables;
with ACPI_Device_Initialization;
with ACPI_Device_Policy;
with AML_Execute;
with AML_Fatal;
with ACPI_Fatal_Diagnostics;
package ACPI_Bootstrap with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   use type ACPI_Service.State_Model;
   use type ACPI_Service.Namespace.Initialization_Report;
   use type ACPI_Service.Catalog_State;
   use type ACPI_Service.Values.Module_Outcome;
   use type ACPI_Service.Values.Access_Status;
   use type ACPI_Service.Execution_Phase_Result;
   use type ACPI_Service.Values.Preparation_Outcome;
   Default_Startup_Budget : constant Natural := 100_000;
   subtype Startup_Budget is Natural range 0 .. Default_Startup_Budget;
   type Finish_Stage is
     (Not_Attempted, Freeze_Failed, Execution_Failed, Members_Failed, Finished,
      Driver_Reused);
   type Finish_Report is record
      Stage : Finish_Stage := Not_Attempted;
      Freeze : ACPI_Service.Freeze_Status := ACPI_Service.Freeze_Wrong_Phase;
      Execution : ACPI_Service.Execution_Report;
      Execution_Status : ACPI_Service.Values.Access_Status := ACPI_Service.Values.Wrong_Phase;
      Member_Status : ACPI_Service.Values.Access_Status := ACPI_Service.Values.Wrong_Phase;
   end record;
   type Device_Phase is (Devices_Not_Started, Devices_Preparing, Devices_Awaiting_Regions,
      Devices_Walking, Devices_Completed, Devices_Completed_With_Errors,
      Devices_Incomplete, Devices_Failed);
   type Device_Report is record
      Stage : Device_Phase := Devices_Not_Started;
      Charged, Status_Methods, Initialization_Methods, Errors : Natural := 0;
      Regions_Attempted : Boolean := False;
      Region_Status : ACPI_Device_Policy.Region_Outcome := ACPI_Device_Policy.Regions_Unavailable;
      First_Error_Node : ACPI_Service.Namespace.Node_ID := ACPI_Service.Namespace.Root;
      First_Error : AML_Execute.Execution_Status := AML_Execute.No_Return;
      Fatal : AML_Fatal.Observation;
      Fatal_Initializer : ACPI_Service.Namespace.Node_ID := ACPI_Service.Namespace.Root;
   end record;
   type Phase is (Receiving, Complete, Failed);
   type State (Table_Capacity, Byte_Capacity, Table_Byte_Limit, Load_Capacity : Positive) is limited private
     with Default_Initial_Condition => Valid (State) and then not Started (State)
       and then Current (State) = Failed and then Expected (State) = 0
       and then Installed (State) = 0;
   function Valid (Session : State) return Boolean;
   function Started (Session : State) return Boolean;
   type State_Model (Table_Capacity, Byte_Capacity, Load_Capacity : Positive) is private with Ghost;
   function Model (Session : State) return State_Model with Ghost,
     Post => Model'Result.Table_Capacity = Session.Table_Capacity
       and then Model'Result.Byte_Capacity = Session.Byte_Capacity
       and then Model'Result.Load_Capacity = Session.Load_Capacity;
   function Core_Model (Session : State) return ACPI_Service.State_Model with Ghost,
     Post => Core_Model'Result.Table_Capacity = Session.Table_Capacity
       and then Core_Model'Result.Byte_Capacity = Session.Byte_Capacity
       and then Core_Model'Result.Load_Capacity = Session.Load_Capacity;
   function Completion (Session : State) return Finish_Report;
   function Module_Errors (Session : State) return ACPI_Service.Module_Error_Observation
     with Global => null;
   -- Read-only copied request and authoritative evaluation context; no delivery
   -- or namespace authority is granted. A fresh session starts absent.
   function Fatal_Diagnostic (Session : State) return ACPI_Fatal_Diagnostics.Observation
     with Global => null;
   -- Snapshot completion and device readiness are distinct. The current service
   -- has no registered hardware provider: actual hardware declarations leave
   -- its region phase unavailable, while an empty requirement can complete.
   function Device_Completion (Session : State) return Device_Report;
   function Startup_Charged (Session : State) return Natural
     with Pre => Valid (Session), Post => Startup_Charged'Result <= Default_Startup_Budget;
   -- Owner interest is latched before startup and applied only after Ready,
   -- before device initializers. Module execution remains disabled.
   function Notifications_Requested (Session : State) return Boolean;
   function Notifications_Enabled (Session : State) return Boolean;
   function Notification_Status (Session : State) return ACPI_Service.Values.Access_Status;
   function Pending_Notifications (Session : State) return ACPI_Service.Notification_Count;
   procedure Set_Notification_Interest (Session : in out State; Enabled : Boolean;
      Status : out ACPI_Service.Values.Access_Status)
     with Pre => Valid (Session), Post => Valid (Session)
       and then (if Status /= ACPI_Service.Values.Available then
         Model (Session) = Model (Session)'Old);
   procedure Take_Notification (Session : in out State;
      Item : out ACPI_Service.Notification_Observation; Present : out Boolean;
      Status : out ACPI_Service.Values.Access_Status)
     with Pre => Valid (Session), Post => Valid (Session);
   function Catalog_Model (Session : State) return ACPI_Service.Catalog_State with Ghost,
     Post => Catalog_Model'Result.Table_Capacity = Session.Table_Capacity
       and then Catalog_Model'Result.Byte_Capacity = Session.Byte_Capacity;
   function Members_Initialized (Session : State) return Boolean;
   function Member_Initialization (Session : State) return ACPI_Service.Namespace.Initialization_Report;
   function Pending_Members (Session : State) return Natural;
   function Current (Session : State) return Phase;
   function Expected (Session : State) return Natural;
   function Installed (Session : State) return Natural;
   function Observe (Session : State) return ACPI_Service.Metrics with
     Post => Observe'Result.Tables <= Session.Table_Capacity
       and then Observe'Result.Bytes <= Session.Byte_Capacity;
   function Audit (Session : State) return ACPI_Service.Values.Audit_Value;
   function Table_Info (Session : State; Index : Positive) return ACPI_Service.Table_Metadata
     with Pre => Index <= Installed (Session),
       Post => Table_Info'Result.Extent <= Session.Table_Byte_Limit;
   -- Read-only descriptions from retained snapshots; no hardware authority.
   function MADT_Info (Session : State; Index : Positive)
     return Firmware_Tables.MADT.Table_Metadata with Pre => Index <= Installed (Session);
   function MADT_Record (Session : State; Index : Positive; Record_Index : Natural)
     return Firmware_Tables.MADT.Record_Result with Pre => Index <= Installed (Session);
   function DMAR_Info (Session : State; Index : Positive) return Firmware_Tables.DMAR.Table_Metadata
     with Pre => Index <= Installed (Session);
   function DMAR_Record (Session : State; Index : Positive ; Record_Index : Natural) return Firmware_Tables.DMAR.Record_Result
     with Pre => Index <= Installed (Session);
   function DMAR_Scope (Session : State; Index : Positive ; Record_Index, Scope_Index : Natural) return Firmware_Tables.DMAR.Scope_Result
     with Pre => Index <= Installed (Session);
   function DMAR_Path (Session : State; Index : Positive ; Record_Index, Scope_Index, Path_Index : Natural) return Firmware_Tables.DMAR.Path_Result
     with Pre => Index <= Installed (Session);
   function SRAT_Info (Session : State; Index : Positive)
     return Firmware_Tables.SRAT.Table_Metadata with Pre => Index <= Installed (Session);
   function SRAT_Record (Session : State; Index : Positive; Record_Index : Natural)
     return Firmware_Tables.SRAT.Record_Result with Pre => Index <= Installed (Session);
   function MCFG_Info (Session : State; Index : Positive) return Firmware_Tables.MCFG.Table_Metadata
     with Pre => Index <= Installed (Session);
   function MCFG_Allocation (Session : State; Index : Positive; Allocation_Index : Natural) return Firmware_Tables.MCFG.Allocation_Result
     with Pre => Index <= Installed (Session);
   function SLIT_Info (Session : State; Index : Positive) return Firmware_Tables.SLIT.Table_Metadata
     with Pre => Index <= Installed (Session);
   function SLIT_Distance (Session : State; Index : Positive; From_Locality, To_Locality : Natural) return Firmware_Tables.SLIT.Distance_Result
     with Pre => Index <= Installed (Session);
   function Table_Byte (Session : State; Index : Positive; Offset : Natural) return Firmware_Tables.Byte
     with Pre => Index <= Installed (Session) and then Offset < Table_Info (Session, Index).Extent;
   -- The trusted provider advertises one DSDT first, followed by ordered SSDTs
   -- and other immutable description tables. Completion
   -- includes ordered definition-block execution and member initialization.
   -- It does not establish device presence or activate hardware.
   procedure Start (Session : in out State; Table_Count : Natural; Accepted : out Boolean)
     with Pre => Valid (Session),
       Post => Valid (Session) and then Started (Session)
         and then Core_Model (Session) = Core_Model (Session)'Old
         and then Accepted = (not Started (Session)'Old
           and then Table_Count in 1 .. Session.Table_Capacity)
         and then (if Started (Session)'Old then Model (Session) = Model (Session)'Old
           else Current (Session) =
             (if Table_Count in 1 .. Session.Table_Capacity then Receiving else Failed)
             and then Expected (Session) =
               (if Table_Count in 1 .. Session.Table_Capacity then Table_Count else 0));
   procedure Import_Table
     (Session : in out State; ID : Positive; Kind : ACPI_Service.Table_Kind;
      Data : Firmware_Tables.Bytes; Result : out ACPI_Service.Install_Status)
     with Pre => Valid (Session), Post => Valid (Session)
       and then Started (Session) = Started (Session)'Old
       and then Expected (Session) = Expected (Session)'Old and then
       (if Current (Session)'Old /= Receiving then Model (Session) = Model (Session)'Old)
       and then (if Current (Session) = Complete then Current (Session)'Old = Complete);
   -- A failed table or premature finish is sticky for this session.
   procedure Finish
     (Session : in out State; Budget : Startup_Budget := Default_Startup_Budget) with
     Pre => Valid (Session), Post => Valid (Session)
       and then Started (Session) = Started (Session)'Old
       and then Expected (Session) = Expected (Session)'Old
       and then Installed (Session) = Installed (Session)'Old
       and then Catalog_Model (Session) = Catalog_Model (Session)'Old
       and then (if Current (Session)'Old = Receiving and then
           Installed (Session)'Old = Expected (Session)'Old then
             Current (Session) in Complete | Failed
             and then Completion (Session).Stage /= Not_Attempted
             and then Completion (Session).Execution.Modules.Charged <= Budget
             and then Startup_Charged (Session) <= Budget
             and then (if Current (Session) = Complete then
               Completion (Session).Stage = Finished
               and then Completion (Session).Execution_Status = ACPI_Service.Values.Available
               and then Completion (Session).Execution.Phase = ACPI_Service.Module_Execution_Reported
               and then Completion (Session).Execution.Preparation = ACPI_Service.Values.Firmware_Prepared
               and then Completion (Session).Execution.Modules.Outcome = ACPI_Service.Values.Modules_Completed
               and then Members_Initialized (Session) and then Pending_Members (Session) = 0)
         elsif Current (Session)'Old = Receiving then
             Current (Session) = Failed and then Core_Model (Session) = Core_Model (Session)'Old
             and then Members_Initialized (Session) = Members_Initialized (Session)'Old
             and then Member_Initialization (Session) = Member_Initialization (Session)'Old
             and then Completion (Session) = Completion (Session)'Old
         else Model (Session) = Model (Session)'Old);

   -- Trusted controller selects an actual device driver, including its region
   -- barrier and provisioned evaluator. Run is process-lifetime state: it is
   -- never reset/replayed. A consumed Run cannot report readiness for a new
   -- snapshot. Table completion and device readiness remain separate outcomes.
   generic
      with package Device_Driver is new ACPI_Device_Initialization
        (Core => ACPI_Service, others => <>);
      -- Trusted module actual must honor Execute_Frozen's owner/catalog and
      -- fuel contract and preserve lower provider obligations on every exit.
      -- Provisioning, region connection and failure recovery are independent.
      with procedure Execute_Module_Startup
        (Service : in out ACPI_Service.State; Budget : Natural;
         Report : out ACPI_Service.Execution_Report;
         Status : out ACPI_Service.Values.Access_Status) is ACPI_Service.Execute_Frozen;
   package With_Driver is
      pragma Unevaluated_Use_Of_Old (Allow);
   procedure Finish
     (Session : in out State; Run : in out Device_Driver.Session;
      Budget : Startup_Budget := Default_Startup_Budget)
     with Pre => Valid (Session), Post => Valid (Session)
       and then Started (Session) = Started (Session)'Old
       and then Expected (Session) = Expected (Session)'Old
       and then Installed (Session) = Installed (Session)'Old
       and then Catalog_Model (Session) = Catalog_Model (Session)'Old
       and then (if Current (Session)'Old = Receiving and then
           Installed (Session)'Old = Expected (Session)'Old then
             Current (Session) in Complete | Failed
             and then Completion (Session).Stage /= Not_Attempted
             and then Completion (Session).Execution.Modules.Charged <= Budget
             and then Startup_Charged (Session) <= Budget
             and then (if Current (Session) = Complete then
               Completion (Session).Stage = Finished
               and then Completion (Session).Execution_Status = ACPI_Service.Values.Available
               and then Completion (Session).Execution.Phase = ACPI_Service.Module_Execution_Reported
               and then Completion (Session).Execution.Preparation = ACPI_Service.Values.Firmware_Prepared
               and then Completion (Session).Execution.Modules.Outcome = ACPI_Service.Values.Modules_Completed
               and then Members_Initialized (Session) and then Pending_Members (Session) = 0)
         elsif Current (Session)'Old = Receiving then
             Current (Session) = Failed and then Core_Model (Session) = Core_Model (Session)'Old
             and then Members_Initialized (Session) = Members_Initialized (Session)'Old
             and then Member_Initialization (Session) = Member_Initialization (Session)'Old
             and then Completion (Session) = Completion (Session)'Old
         else Model (Session) = Model (Session)'Old);
   end With_Driver;

private
   procedure Unconfigured_Regions
     (Service : aliased in out ACPI_Service.State; Budget : Natural;
      Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome);
   package Devices is new ACPI_Device_Initialization
     (Core => ACPI_Service, Initialize_Regions => Unconfigured_Regions);
   type State_Model (Table_Capacity, Byte_Capacity, Load_Capacity : Positive) is record
      Core : ACPI_Service.State_Model (Table_Capacity, Byte_Capacity, Load_Capacity);
      Stage : Phase;
      Required : Natural;
      Begun : Boolean;
      Initialized : Boolean;
      Initialization : ACPI_Service.Namespace.Initialization_Report;
      Completion_Data : Finish_Report;
      Device_Data : Device_Report;
      Startup_Limit : Startup_Budget := 0;
      Notify_Requested, Notify_Enabled : Boolean := False;
      Notify_Status : ACPI_Service.Values.Access_Status := ACPI_Service.Values.Wrong_Phase;
   end record;
   type State (Table_Capacity, Byte_Capacity, Table_Byte_Limit, Load_Capacity : Positive) is limited record
      Core : aliased ACPI_Service.State (Table_Capacity, Byte_Capacity, Table_Byte_Limit, Load_Capacity);
      Device_Run : Devices.Session;
      Begun : Boolean := False;
      Initialized : Boolean := False;
      Completion_Data : Finish_Report;
      Device_Data : Device_Report;
      Startup_Limit : Startup_Budget := 0;
      Notify_Requested, Notify_Enabled : Boolean := False;
      Notify_Status : ACPI_Service.Values.Access_Status := ACPI_Service.Values.Wrong_Phase;
      Initialization : ACPI_Service.Namespace.Initialization_Report := (others => 0);
      Stage : Phase := Failed;
      Required : Natural := 0;
   end record with Type_Invariant =>
     (if not State.Begun then State.Stage = Failed and then State.Required = 0
       and then ACPI_Service.Observe (State.Core).Tables = 0)
     and then State.Required <= State.Table_Capacity
     and then State.Completion_Data.Execution.Modules.Charged <= State.Startup_Limit
     and then State.Device_Data.Charged <=
       State.Startup_Limit - State.Completion_Data.Execution.Modules.Charged
     and then (if State.Stage in Receiving | Complete then
        State.Required > 0 and then ACPI_Service.Observe (State.Core).Tables <= State.Required)
     and then (if State.Stage = Complete then
        ACPI_Service.Observe (State.Core).Tables = State.Required
        and then State.Initialized and then ACPI_Service.Pending_Members (State.Core) = 0
        and then State.Completion_Data.Stage = Finished
        and then State.Completion_Data.Execution_Status = ACPI_Service.Values.Available
        and then State.Completion_Data.Member_Status = ACPI_Service.Values.Available
        and then State.Completion_Data.Execution.Phase = ACPI_Service.Module_Execution_Reported
        and then State.Completion_Data.Execution.Preparation = ACPI_Service.Values.Firmware_Prepared
        and then State.Completion_Data.Execution.Modules.Outcome = ACPI_Service.Values.Modules_Completed)
     and then (if not State.Initialized then State.Initialization = (0,0,0));
   function Started (Session : State) return Boolean is (Session.Begun);
end ACPI_Bootstrap;
