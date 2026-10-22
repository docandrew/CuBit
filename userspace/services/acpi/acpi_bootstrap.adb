pragma Ada_2022;
package body ACPI_Bootstrap with SPARK_Mode is
   use type Devices.Phase;
   procedure Unavailable_Regions
     (Service : aliased in out ACPI_Service.State; Budget : Natural;
      Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome) is
      pragma Unreferenced (Service, Budget);
   begin
      Charged := 0;
      Outcome := ACPI_Device_Policy.Regions_Unavailable;
   end Unavailable_Regions;
   function Device_Completion (Session : State) return Device_Report is (Session.Device_Data);
   function Fatal_Diagnostic (Session : State) return ACPI_Fatal_Diagnostics.Observation is
   begin
      if Session.Completion_Data.Execution.Modules.Fatal.Present then
         return (Present => True, Item => Session.Completion_Data.Execution.Modules.Fatal.Item,
           Source => (Kind => ACPI_Fatal_Diagnostics.Definition_Block,
             Table_ID => ACPI_Fatal_Diagnostics.Catalog_Identifier
               (Session.Completion_Data.Execution.Failed_Table_ID)));
      elsif Session.Device_Data.Fatal.Present then
         return (Present => True, Item => Session.Device_Data.Fatal.Item,
           Source => (Kind => ACPI_Fatal_Diagnostics.Device_Initializer,
             Initializer_Scope => ACPI_Fatal_Diagnostics.Namespace_Position
               (Session.Device_Data.Fatal_Initializer)));
      else return (Present => False); end if;
   end Fatal_Diagnostic;
   function Startup_Charged (Session : State) return Natural is
     (Session.Completion_Data.Execution.Modules.Charged + Session.Device_Data.Charged);
   function Public_Report (Item : Devices.Report) return Device_Report is
     (Stage => (case Item.Stage is
        when Devices.Not_Started => Devices_Not_Started,
        when Devices.Preparing => Devices_Preparing,
        when Devices.Awaiting_Regions => Devices_Awaiting_Regions,
        when Devices.Walking => Devices_Walking,
        when Devices.Completed => Devices_Completed,
        when Devices.Completed_With_Errors => Devices_Completed_With_Errors,
        when Devices.Incomplete => Devices_Incomplete,
        when Devices.Failed => Devices_Failed),
      Charged => Item.Charged, Status_Methods => Item.Status_Methods,
      Initialization_Methods => Item.Initialization_Methods, Errors => Item.Errors,
      Region_Status => Item.Region_Status, First_Error_Node => Item.First_Error_Node,
      First_Error => Item.First_Error, Fatal => Item.Fatal, Fatal_Initializer => Item.Fatal_Initializer);
   pragma Unevaluated_Use_Of_Old (Allow);
   function Valid (Session : State) return Boolean is (ACPI_Service.Valid (Session.Core));
   function Core_Model (Session : State) return ACPI_Service.State_Model is
     (ACPI_Service.Model (Session.Core));
   function Model (Session : State) return State_Model is
     (Table_Capacity => Session.Table_Capacity, Byte_Capacity => Session.Byte_Capacity,
      Core => Core_Model (Session), Stage => Session.Stage, Required => Session.Required,
      Begun => Session.Begun, Initialized => Session.Initialized,
      Initialization => Session.Initialization, Completion_Data => Session.Completion_Data,
      Device_Data => Session.Device_Data, Startup_Limit => Session.Startup_Limit,
      Notify_Requested => Session.Notify_Requested, Notify_Enabled => Session.Notify_Enabled,
      Notify_Status => Session.Notify_Status);
   use type ACPI_Service.Install_Status;
   use type ACPI_Service.Freeze_Status;
   function Completion (Session : State) return Finish_Report is (Session.Completion_Data);
   function Notifications_Requested (Session : State) return Boolean is (Session.Notify_Requested);
   function Notifications_Enabled (Session : State) return Boolean is (Session.Notify_Enabled);
   function Notification_Status (Session : State) return ACPI_Service.Values.Access_Status is (Session.Notify_Status);
   function Pending_Notifications (Session : State) return ACPI_Service.Notification_Count is
     (ACPI_Service.Pending_Notifications (Session.Core));
   procedure Set_Notification_Interest (Session : in out State; Enabled : Boolean;
      Status : out ACPI_Service.Values.Access_Status) is
   begin
      if Session.Initialized then
         ACPI_Service.Configure_Notifications (Session.Core, Enabled, Status);
         if Status /= ACPI_Service.Values.Available then return; end if;
         Session.Notify_Enabled := Enabled;
      elsif not Session.Begun or else Session.Stage = Receiving then
         Status := ACPI_Service.Values.Available;
      else Status := ACPI_Service.Values.Wrong_Phase; return;
      end if;
      Session.Notify_Requested := Enabled;
      Session.Notify_Status := Status;
   end Set_Notification_Interest;
   procedure Take_Notification (Session : in out State;
      Item : out ACPI_Service.Notification_Observation; Present : out Boolean;
      Status : out ACPI_Service.Values.Access_Status) is
   begin
      ACPI_Service.Take_Notification (Session.Core, Item, Present, Status);
   end Take_Notification;
   function Catalog_Model (Session : State) return ACPI_Service.Catalog_State is
     (ACPI_Service.Catalog_Snapshot (Session.Core));
   function Members_Initialized (Session : State) return Boolean is (Session.Initialized);
   function Member_Initialization (Session : State) return ACPI_Service.Namespace.Initialization_Report is
     (Session.Initialization);
   function Pending_Members (Session : State) return Natural is
     (ACPI_Service.Pending_Members (Session.Core));
   function Current (Session : State) return Phase is (Session.Stage);
   function Expected (Session : State) return Natural is (Session.Required);
   function Installed (Session : State) return Natural is (ACPI_Service.Observe (Session.Core).Tables);
   function Observe (Session : State) return ACPI_Service.Metrics is (ACPI_Service.Observe (Session.Core));
   function Audit (Session : State) return ACPI_Service.Values.Audit_Value is (ACPI_Service.Audit (Session.Core));
   function Table_Info (Session : State; Index : Positive) return ACPI_Service.Table_Metadata is
     (ACPI_Service.Table_Info (Session.Core, Index));
   function MADT_Info (Session : State; Index : Positive)
     return Firmware_Tables.MADT.Table_Metadata is (ACPI_Service.MADT_Info (Session.Core, Index));
   function MADT_Record (Session : State; Index : Positive; Record_Index : Natural)
     return Firmware_Tables.MADT.Record_Result is (ACPI_Service.MADT_Record (Session.Core, Index, Record_Index));
   function DMAR_Info (Session : State; Index : Positive) return Firmware_Tables.DMAR.Table_Metadata is
     (ACPI_Service.DMAR_Info (Session.Core, Index));
   function DMAR_Record (Session : State; Index : Positive ; Record_Index : Natural) return Firmware_Tables.DMAR.Record_Result is
     (ACPI_Service.DMAR_Record (Session.Core, Index, Record_Index));
   function DMAR_Scope (Session : State; Index : Positive ; Record_Index, Scope_Index : Natural) return Firmware_Tables.DMAR.Scope_Result is
     (ACPI_Service.DMAR_Scope (Session.Core, Index, Record_Index, Scope_Index));
   function DMAR_Path (Session : State; Index : Positive ; Record_Index, Scope_Index, Path_Index : Natural) return Firmware_Tables.DMAR.Path_Result is
     (ACPI_Service.DMAR_Path (Session.Core, Index, Record_Index, Scope_Index, Path_Index));
   function SRAT_Info (Session : State; Index : Positive)
     return Firmware_Tables.SRAT.Table_Metadata is (ACPI_Service.SRAT_Info (Session.Core, Index));
   function SRAT_Record (Session : State; Index : Positive; Record_Index : Natural)
     return Firmware_Tables.SRAT.Record_Result is (ACPI_Service.SRAT_Record (Session.Core, Index, Record_Index));
   function MCFG_Info (Session : State; Index : Positive) return Firmware_Tables.MCFG.Table_Metadata is
     (ACPI_Service.MCFG_Info (Session.Core, Index));
   function MCFG_Allocation (Session : State; Index : Positive; Allocation_Index : Natural) return Firmware_Tables.MCFG.Allocation_Result is
     (ACPI_Service.MCFG_Allocation (Session.Core, Index, Allocation_Index));
   function SLIT_Info (Session : State; Index : Positive) return Firmware_Tables.SLIT.Table_Metadata is
     (ACPI_Service.SLIT_Info (Session.Core, Index));
   function SLIT_Distance (Session : State; Index : Positive; From_Locality, To_Locality : Natural) return Firmware_Tables.SLIT.Distance_Result is
     (ACPI_Service.SLIT_Distance (Session.Core, Index, From_Locality, To_Locality));
   function Table_Byte (Session : State; Index : Positive; Offset : Natural) return Firmware_Tables.Byte is
     (ACPI_Service.Table_Byte (Session.Core, Index, Offset));
   procedure Start (Session : in out State; Table_Count : Natural; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Session.Begun then return; end if;
      Session.Begun := True;
      if Table_Count in 1 .. Session.Table_Capacity then
         Session.Required := Table_Count;
         Session.Stage := Receiving;
         Accepted := True;
      end if;
   end Start;
   procedure Import_Table
     (Session : in out State; ID : Positive; Kind : ACPI_Service.Table_Kind;
      Data : Firmware_Tables.Bytes; Result : out ACPI_Service.Install_Status) is
   begin
      if Session.Stage /= Receiving then Result := ACPI_Service.Wrong_Order; return; end if;
      if Installed (Session) = Session.Required then
         Session.Stage := Failed; Result := ACPI_Service.Table_Limit; return;
      end if;
      declare
         Staged : ACPI_Service.Stage_Status;
      begin
         ACPI_Service.Stage_Table (Session.Core, ID, Kind, Data, Staged);
         Result := (case Staged is
           when ACPI_Service.Staged => ACPI_Service.Installed,
           when ACPI_Service.Stage_Wrong_Order => ACPI_Service.Wrong_Order,
           when ACPI_Service.Stage_Duplicate_ID => ACPI_Service.Duplicate_ID,
           when ACPI_Service.Stage_Table_Limit => ACPI_Service.Table_Limit,
           when ACPI_Service.Stage_Byte_Limit => ACPI_Service.Byte_Limit,
           when ACPI_Service.Stage_Invalid_Table => ACPI_Service.Invalid_Table);
      end;
      if Result /= ACPI_Service.Installed then Session.Stage := Failed; end if;
      -- The core admits at most one table per call. Verify the advertised
      -- count at this boundary before exposing Receiving/Complete again.
      if ACPI_Service.Observe (Session.Core).Tables > Session.Required then Session.Stage := Failed; end if;
   end Import_Table;
   procedure Finish
     (Session : in out State; Budget : Startup_Budget := Default_Startup_Budget) is
   begin
      if Session.Stage /= Receiving then return; end if;
      -- Consume this one-shot session before any operation that can fail.
      Session.Stage := Failed;
      if Installed (Session) /= Session.Required then return; end if;
      Session.Startup_Limit := Budget;
      Session.Completion_Data.Stage := Freeze_Failed;
      ACPI_Service.Freeze_Catalog (Session.Core, Session.Completion_Data.Freeze);
      if Session.Completion_Data.Freeze /= ACPI_Service.Catalog_Frozen_OK then return; end if;
      Session.Completion_Data.Stage := Execution_Failed;
      ACPI_Service.Execute_Frozen
        (Session.Core, Budget, Session.Completion_Data.Execution,
         Session.Completion_Data.Execution_Status);
      if Session.Completion_Data.Execution_Status /= ACPI_Service.Values.Available
        or else Session.Completion_Data.Execution.Phase /= ACPI_Service.Module_Execution_Reported
        or else Session.Completion_Data.Execution.Preparation /= ACPI_Service.Values.Firmware_Prepared
        or else Session.Completion_Data.Execution.Modules.Outcome /= ACPI_Service.Values.Modules_Completed
      then return; end if;
      Session.Completion_Data.Stage := Members_Failed;
      declare
         Prior : constant ACPI_Service.State_Model := Core_Model (Session) with Ghost;
         Pending : constant Natural := Pending_Members (Session);
         Report : ACPI_Service.Namespace.Initialization_Report;
      begin
         ACPI_Service.Initialize_Members
           (Session.Core, Report, Session.Completion_Data.Member_Status);
         if Session.Completion_Data.Member_Status /= ACPI_Service.Values.Available then return; end if;
         pragma Assert (ACPI_Service.Initialization_Frame (Core_Model (Session), Prior));
         pragma Assert (Report.Bound + Report.Missing + Report.Unsupported = Pending);
         Session.Initialization := Report;
      end;
      Session.Initialized := True;
      ACPI_Service.Configure_Notifications
        (Session.Core, Session.Notify_Requested, Session.Notify_Status);
      if Session.Notify_Status /= ACPI_Service.Values.Available then return; end if;
      Session.Notify_Enabled := Session.Notify_Requested;
      declare
         Accepted : Boolean;
      begin
         Devices.Begin_Initialization (Session.Device_Run, Session.Core,
           Budget - Session.Completion_Data.Execution.Modules.Charged, Accepted);
         if Accepted and then Devices.Observe (Session.Device_Run).Stage = Devices.Awaiting_Regions then
            Devices.Complete_Initialization (Session.Device_Run, Session.Core, Accepted);
         end if;
         Session.Device_Data := Public_Report (Devices.Observe (Session.Device_Run));
      end;
      -- Method declarations and provisional members must be cleaned at return.
      -- Do not label the retained snapshot complete if that invariant is lost.
      if ACPI_Service.Pending_Members (Session.Core) /= 0 then return; end if;
      Session.Completion_Data.Stage := Finished;
      Session.Stage := Complete;
   end Finish;
end ACPI_Bootstrap;
