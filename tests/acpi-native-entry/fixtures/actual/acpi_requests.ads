pragma Ada_2022;
with AML_Execute;
with Interfaces;
with ACPI_Service;
with ACPI_Bootstrap;
with ACPI_Fatal_Diagnostics;
with ACPI_Notification_Receipts;
with Firmware_Tables;
-- Transport-independent request core. No endpoint/authority tag is allocated
-- here. Origin must come from a trusted native capability adapter, never wire
-- words, a PID claim, or the unauthenticated message reserved field.
package ACPI_Requests with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   use Interfaces;
   use type ACPI_Bootstrap.Phase;
   type Authority is (No_Authority, Observer, Snapshot_Provider);
   -- CCL integers are signed 64-bit. All observation words, including the
   -- revision token, must remain representable without reinterpretation.
   Max_Revision : constant Unsigned_64 := Unsigned_64 (Integer_64'Last);
   subtype Revision_Number is Unsigned_64 range 0 .. Max_Revision;
   type Phase is (Idle, Receiving, Complete, Failed);
   for Phase use (Idle => 0, Receiving => 1, Complete => 2, Failed => 3);
   type Words is array (Natural range 0 .. 3) of Unsigned_64;
   type Packet is record
      Label : Unsigned_32 := 0;
      Length : Unsigned_8 := 4;
      Flags : Unsigned_8 := 0;
      Reserved : Unsigned_16 := 0;
      Data : Words := [others => 0];
   end record;
   -- Labels are scoped to this draft service protocol, not kernel service IDs.
   Read_Metrics : constant Unsigned_32 := 0;
   Metrics_Members_State_Page : constant Unsigned_64 := 8;
   Metrics_Device_State_Page : constant Unsigned_64 := 9;
   Metrics_Startup_Fuel_Page : constant Unsigned_64 := 10;
   Metrics_Device_Calls_Page : constant Unsigned_64 := 11;
   Metrics_Device_Error_Page : constant Unsigned_64 := 12;
   -- Completion phase qualifies default report fields; No_Return is not success.
   Metrics_Completion_State_Page : constant Unsigned_64 := 13;
   Metrics_Completion_Access_Page : constant Unsigned_64 := 14;
   Metrics_Module_State_Page : constant Unsigned_64 := 15;
   Metrics_Module_Error_Page : constant Unsigned_64 := 16;
   -- [region barrier attempted, Region_Status_Code, zero]. The status is only
   -- an attempted outcome when word1 is1; neither word grants authority.
   Metrics_Region_Attempt_Page : constant Unsigned_64 := 17;
   Metrics_Last_Page : constant Unsigned_64 := Metrics_Region_Attempt_Page;
   type Finish_Stage_Code is
     (Finish_Not_Attempted_Code,
      Finish_Freeze_Failed_Code,
      Finish_Execution_Failed_Code,
      Finish_Members_Failed_Code,
      Finish_Finished_Code);
   for Finish_Stage_Code use
     (Finish_Not_Attempted_Code => 0,
      Finish_Freeze_Failed_Code => 1,
      Finish_Execution_Failed_Code => 2,
      Finish_Members_Failed_Code => 3,
      Finish_Finished_Code => 4);
   type Freeze_Outcome_Code is
     (Freeze_Catalog_Frozen_OK_Code,
      Freeze_Freeze_Wrong_Phase_Code,
      Freeze_Freeze_Empty_Code);
   for Freeze_Outcome_Code use
     (Freeze_Catalog_Frozen_OK_Code => 0,
      Freeze_Freeze_Wrong_Phase_Code => 1,
      Freeze_Freeze_Empty_Code => 2);
   type Execution_Phase_Code is
     (Execution_Execution_Not_Started_Code,
      Execution_Preparation_Failed_Code,
      Execution_Module_Execution_Reported_Code);
   for Execution_Phase_Code use
     (Execution_Execution_Not_Started_Code => 0,
      Execution_Preparation_Failed_Code => 1,
      Execution_Module_Execution_Reported_Code => 2);
   type Preparation_Outcome_Code is
     (Preparation_Preparation_Not_Attempted_Code,
      Preparation_Preparation_Rejected_Code,
      Preparation_Firmware_Prepared_Code,
      Preparation_Namespace_Capacity_Exceeded_Code,
      Preparation_Value_Capacity_Exceeded_Code);
   for Preparation_Outcome_Code use
     (Preparation_Preparation_Not_Attempted_Code => 0,
      Preparation_Preparation_Rejected_Code => 1,
      Preparation_Firmware_Prepared_Code => 2,
      Preparation_Namespace_Capacity_Exceeded_Code => 3,
      Preparation_Value_Capacity_Exceeded_Code => 4);
   type Access_Outcome_Code is
     (Access_Available_Code,
      Access_Invalid_Value_Code,
      Access_Uninitialized_Element_Code,
      Access_Wrong_Kind_Code,
      Access_Out_Of_Bounds_Code,
      Access_Root_Limit_Code,
      Access_Identity_Exhausted_Code,
      Access_Busy_Code,
      Access_Wrong_Phase_Code);
   for Access_Outcome_Code use
     (Access_Available_Code => 0,
      Access_Invalid_Value_Code => 1,
      Access_Uninitialized_Element_Code => 2,
      Access_Wrong_Kind_Code => 3,
      Access_Out_Of_Bounds_Code => 4,
      Access_Root_Limit_Code => 5,
      Access_Identity_Exhausted_Code => 6,
      Access_Busy_Code => 7,
      Access_Wrong_Phase_Code => 8);
   type Module_Outcome_Code is
     (Module_Catalog_Rejected_Code,
      Module_Modules_Completed_Code,
      Module_Module_Rejected_Code);
   for Module_Outcome_Code use
     (Module_Catalog_Rejected_Code => 0,
      Module_Modules_Completed_Code => 1,
      Module_Module_Rejected_Code => 2);
   type Device_Phase_Code is (Device_Not_Started_Code, Device_Preparing_Code,
      Device_Awaiting_Regions_Code, Device_Walking_Code, Device_Completed_Code,
      Device_Completed_With_Errors_Code, Device_Incomplete_Code, Device_Failed_Code);
   for Device_Phase_Code use (Device_Not_Started_Code => 0, Device_Preparing_Code => 1,
      Device_Awaiting_Regions_Code => 2, Device_Walking_Code => 3, Device_Completed_Code => 4,
      Device_Completed_With_Errors_Code => 5, Device_Incomplete_Code => 6, Device_Failed_Code => 7);
   type Region_Status_Code is (Regions_Connected_Code, No_Regions_Required_Code,
      Regions_Unavailable_Code, Region_Failed_Code);
   for Region_Status_Code use (Regions_Connected_Code => 0, No_Regions_Required_Code => 1,
      Regions_Unavailable_Code => 2, Region_Failed_Code => 3);
   -- Explicit wire mapping, independent of internal execution enum order.
   type Startup_Error_Code is
     (Error_Returned,
      Error_Object_Returned,
      Error_Reference_Returned,
      Error_Metadata_Returned,
      Error_No_Return,
      Error_Truncated,
      Error_Unsupported,
      Error_Uninitialized,
      Error_Missing_Argument,
      Error_Budget_Exceeded,
      Error_Invalid_Method,
      Error_Argument_Mismatch,
      Error_Mutex_Order,
      Error_Duplicate_Name,
      Error_Namespace_Limit,
      Error_Expression_Limit,
      Error_Block_Limit,
      Error_Bad_Package,
      Error_Invalid_Control,
      Error_Unknown_Name,
      Error_Unsupported_Value,
      Error_Bad_Name,
      Error_Call_Limit,
      Error_Division_By_Zero,
      Error_Empty_Buffer,
      Error_Missing_Result,
      Error_Value_Limit,
      Error_Numeric_Overflow,
      Error_Invalid_Delay,
      Error_Delay_Failed,
      Error_Package_Limit,
      Error_Invalid_Match_Operation,
      Error_No_Resource_End_Tag,
      Error_Invalid_Resource_Type,
      Error_Bad_Resource_Length,
      Error_Resource_Buffer_Length,
      Error_Invalid_Field_Length,
      Error_Buffer_Field_Limit,
      Error_Notify_Limit,
      Error_Module_Completed,
      Error_Mutex_Not_Acquired, Error_Fatal_Requested, Error_Field_Indeterminate, Error_Invalid_Table_Length);
   for Startup_Error_Code use
     (Error_Returned => 0,
      Error_Object_Returned => 1,
      Error_Reference_Returned => 2,
      Error_Metadata_Returned => 3,
      Error_No_Return => 4,
      Error_Truncated => 5,
      Error_Unsupported => 6,
      Error_Uninitialized => 7,
      Error_Missing_Argument => 8,
      Error_Budget_Exceeded => 9,
      Error_Invalid_Method => 10,
      Error_Argument_Mismatch => 11,
      Error_Mutex_Order => 12,
      Error_Duplicate_Name => 13,
      Error_Namespace_Limit => 14,
      Error_Expression_Limit => 15,
      Error_Block_Limit => 16,
      Error_Bad_Package => 17,
      Error_Invalid_Control => 18,
      Error_Unknown_Name => 19,
      Error_Unsupported_Value => 20,
      Error_Bad_Name => 21,
      Error_Call_Limit => 22,
      Error_Division_By_Zero => 23,
      Error_Empty_Buffer => 24,
      Error_Missing_Result => 25,
      Error_Value_Limit => 26,
      Error_Numeric_Overflow => 27,
      Error_Invalid_Delay => 28,
      Error_Delay_Failed => 29,
      Error_Package_Limit => 30,
      Error_Invalid_Match_Operation => 31,
      Error_No_Resource_End_Tag => 32,
      Error_Invalid_Resource_Type => 33,
      Error_Bad_Resource_Length => 34,
      Error_Resource_Buffer_Length => 35,
      Error_Invalid_Field_Length => 36,
      Error_Buffer_Field_Limit => 37,
      Error_Notify_Limit => 38,
      Error_Module_Completed => 39,
      Error_Mutex_Not_Acquired => 40, Error_Fatal_Requested => 41,
      Error_Field_Indeterminate => 42, Error_Invalid_Table_Length => 43);
   Start_Snapshot : constant Unsigned_32 := 1;
   Begin_Table : constant Unsigned_32 := 2;
   Write_Chunk : constant Unsigned_32 := 3;
   Commit_Table : constant Unsigned_32 := 4;
   Finish_Snapshot : constant Unsigned_32 := 5;
   Read_Table_Info : constant Unsigned_32 := 6;
   Read_Table_Chunk : constant Unsigned_32 := 7;
   -- Queries use [revision, table index, record/row, page/column].
   -- MCFG info page (word2) 0 returns [revision,count,0]; page 1
   -- returns [reserved low32,reserved high32,0]. Word3 must be zero.
   -- Allocation page 0 returns [base low32, base high32, segment];
   -- page 1 returns [first bus, last bus, reserved32].
   -- SLIT info returns [revision, locality count, 0]; distance [value,0,0].
   -- Label 8 is reserved by the native grant-import adapter.
   Read_MCFG_Info : constant Unsigned_32 := 9;
   Read_MCFG_Allocation : constant Unsigned_32 := 10;
   Read_SLIT_Info : constant Unsigned_32 := 11;
   Read_SLIT_Distance : constant Unsigned_32 := 12;
   Read_MADT_Info : constant Unsigned_32 := 13;
   Read_MADT_Record : constant Unsigned_32 := 14;
   Read_MADT_Fields : constant Unsigned_32 := 15;
   Read_SRAT_Info : constant Unsigned_32 := 16;
   Read_SRAT_Record : constant Unsigned_32 := 17;
   Read_SRAT_Fields : constant Unsigned_32 := 18;
   Read_DMAR_Info : constant Unsigned_32 := 19;
   Read_DMAR_Record : constant Unsigned_32 := 20;
   Read_DMAR_Fields : constant Unsigned_32 := 21;
   Read_DMAR_Scope : constant Unsigned_32 := 22;
   Read_DMAR_Path : constant Unsigned_32 := 23;
   Configure_Notifications : constant Unsigned_32 := 24;
   Claim_Notification : constant Unsigned_32 := 25;
   Read_Notification : constant Unsigned_32 := 26;
   Acknowledge_Notification : constant Unsigned_32 := 27;
   Read_Notification_State : constant Unsigned_32 := 28;
   subtype Notification_Page is Unsigned_64 range 0 .. 2;
   Notification_Data_Page : constant Notification_Page := 0;
   Notification_Identity_Page : constant Notification_Page := 1;
   Notification_Path_Page : constant Notification_Page := 2;
   subtype Notification_State_Page is Unsigned_64 range 0 .. 1;
   Notification_State_Summary : constant Notification_State_Page := 0;
   Notification_State_Policy : constant Notification_State_Page := 1;
   subtype Receipt_Number is Revision_Number;
   type Notification_Kind_Code is (Notify_Device, Notify_Processor, Notify_Thermal);
   for Notification_Kind_Code use (Notify_Device => 0, Notify_Processor => 1, Notify_Thermal => 2);
   type Notification_Class_Code is (Notify_System_Class, Notify_Device_Class);
   for Notification_Class_Code use (Notify_System_Class => 0, Notify_Device_Class => 1);
   subtype DMAR_Header_Page is Unsigned_64 range 0 .. 1;
   subtype DMAR_Record_Page is Unsigned_64 range 0 .. 1;
   subtype DMAR_Field_Page is Unsigned_64 range 0 .. 2;
   subtype DMAR_Scope_Page is Unsigned_64 range 0 .. 2;
   DMAR_Scope_Page_Radix : constant Unsigned_64 := 4;
   DMAR_Path_Index_Radix : constant Unsigned_64 := 128;
   -- Scope/path selectors encode bounded indices, never addresses. Unknown
   -- record scope counts count decoded scopes only; raw bytes remain readable.
   SRAT_Header_Page : constant Unsigned_64 := 0;
   SRAT_Reserved_Page : constant Unsigned_64 := 1;
   SRAT_Common_Page : constant Unsigned_64 := 0;
   SRAT_Detail_Page : constant Unsigned_64 := 1;
   SRAT_Base_Page : constant Unsigned_64 := 2;
   SRAT_Length_Page : constant Unsigned_64 := 3;
   SRAT_Handle_First_Page : constant Unsigned_64 := 4;
   SRAT_Handle_Last_Page : constant Unsigned_64 := 9;
   SRAT_Handle_Bytes_Per_Page : constant := 3;
   MADT_Header_Page : constant Unsigned_64 := 0;
   MADT_Address_Page : constant Unsigned_64 := 1;
   MADT_Fields_Page : constant Unsigned_64 := 0;
   MADT_Override_Flags_Page : constant Unsigned_64 := 1;
   MCFG_Header_Page : constant Unsigned_64 := 0;
   MCFG_Reserved_Page : constant Unsigned_64 := 1;
   MCFG_Address_Page : constant Unsigned_64 := 0;
   MCFG_Bus_Page : constant Unsigned_64 := 1;
   type Outcome is
     (OK, Denied, Malformed, Stale, Wrong_Order, Resource_Limit,
      Table_Rejected, Incomplete, Not_Found, Wrong_Table_Kind, Index_Out_Of_Range, Unsupported_Record_Kind);
   -- Observation results must be representable as CCL integers. Upload packets
   -- retain unrestricted words because their payload contains arbitrary bytes.
   type Response_Words is array (Natural range 0 .. 3) of Revision_Number;
   type Response is record
      Status : Outcome := Malformed;
      Data : Response_Words := [others => 0];
   end record;
   type State (Table_Capacity, Byte_Capacity, Table_Byte_Limit, Load_Capacity : Positive; Initial_Revision : Revision_Number) is limited private
     with Default_Initial_Condition => Valid (State)
       and then Revision (State) = State.Initial_Revision
       and then Current (State) = Idle and then not Table_Open (State)
       and then Received (State) = 0;
   function Valid (Server : State) return Boolean;
   type State_Model (Table_Capacity, Byte_Capacity, Table_Byte_Limit, Load_Capacity : Positive) is private with Ghost;
   function Model (Server : State) return State_Model with Ghost,
     Post => Model'Result.Table_Capacity = Server.Table_Capacity
       and then Model'Result.Byte_Capacity = Server.Byte_Capacity
       and then Model'Result.Table_Byte_Limit = Server.Table_Byte_Limit;
   function Revision (Server : State) return Revision_Number;
   function Current (Server : State) return Phase;
   function Table_Open (Server : State) return Boolean;
   function Received (Server : State) return Natural;
   function Completion (Server : State) return ACPI_Bootstrap.Finish_Report;
   -- Copied initialization outcome; no AML, state change or new wire authority.
   function Device_Completion (Server : State) return ACPI_Bootstrap.Device_Report
     with Global => null;
   -- Copied observation only; does not add a wire operation or authority.
   function Module_Errors (Server : State) return ACPI_Service.Module_Error_Observation
     with Global => null;
   -- In-process read-only observation, not a new wire operation or authority.
   -- Caller correlates this copied record with Revision(Server); a revision
   -- change does not erase an already retained fatal from this session.
   function Fatal_Diagnostic (Server : State) return ACPI_Fatal_Diagnostics.Observation
     with Global => null;
   function Observe (Server : State) return ACPI_Service.Metrics;
   -- All requests contain exactly four words and zero flags/reserved.
   -- Mutations use word0 as an optimistic revision token. Every state change
   -- consumes one token; it never wraps. Authorized replies report revision in
   -- word0. Unclassified callers receive Denied with zero data.
   procedure Handle (Server : in out State; Origin : Authority;
                     Request : Packet; Reply : out Response) with
     Pre => Valid (Server), Post => Valid (Server) and then (if Origin = No_Authority then Reply.Status = Denied and then Reply.Data = [0, 0, 0, 0]
              else Reply.Data (0) = Revision (Server))
       and then Revision (Server) >= Revision (Server)'Old
       and then (if Origin = Snapshot_Provider
         and then Request.Label in Start_Snapshot .. Finish_Snapshot
         and then Reply.Status in OK | Table_Rejected | Incomplete
         then Revision (Server)'Old < Max_Revision
           and then Revision (Server) = Revision (Server)'Old + 1)
       and then (for all Word of Reply.Data => Word <= Max_Revision)
       and then (if Origin /= Snapshot_Provider or else Request.Label in Read_Metrics | Read_Table_Info | Read_Table_Chunk | Read_MCFG_Info .. Read_DMAR_Path | Read_Notification | Read_Notification_State
         or else Reply.Status in Denied | Malformed | Stale | Wrong_Order | Resource_Limit
         then Model (Server) = Model (Server)'Old);
   -- Native adapter entry point for one complete table from a bulk grant.
   -- The adapter authenticates Origin, validates the mapped extent and keeps
   -- Data stable and mapped for this entire call. Receiver read-only permission
   -- alone does not freeze the provider's alias. No address or handle supplied
   -- in a packet is dereferenced here; Data contains exactly the table bytes,
   -- excluding page padding. Accepted imports retain their own byte copy.
   procedure Import_Block
     (Server : in out State; Origin : Authority; Token : Unsigned_64;
      ID : Positive; Kind : ACPI_Service.Table_Kind;
      Data : Firmware_Tables.Bytes; Reply : out Response) with
     Pre => Valid (Server), Post => Valid (Server) and then
       (if Origin = No_Authority then
          Reply.Status = Denied and then Reply.Data = [0, 0, 0, 0]
        else Reply.Data (0) = Revision (Server))
       and then Revision (Server) >= Revision (Server)'Old
       and then (if Reply.Status in Denied | Malformed | Stale | Wrong_Order | Resource_Limit
         then Model (Server) = Model (Server)'Old)
       and then (if Origin /= Snapshot_Provider then
         Reply.Status = Denied and then Model (Server) = Model (Server)'Old)
       and then (if Reply.Status in OK | Table_Rejected then
         Revision (Server)'Old < Max_Revision
         and then Revision (Server) = Revision (Server)'Old + 1
         and then not Table_Open (Server) and then Received (Server) = 0);
private
   -- Private wire codec; deliberately independent of internal enum positions.
   function Error_Code (Item : AML_Execute.Execution_Status) return Startup_Error_Code;
   package Receipts is new ACPI_Notification_Receipts (Max_Revision);
   type State_Model (Table_Capacity, Byte_Capacity, Table_Byte_Limit, Load_Capacity : Positive) is record
      Boot : ACPI_Bootstrap.State_Model (Table_Capacity, Byte_Capacity, Load_Capacity);
      Initial_Revision, Version : Revision_Number;
      Receipt : Receipts.State;
      Staged_Notification : ACPI_Service.Notification_Observation;
      Open : Boolean;
      ID : Positive;
      Kind : ACPI_Service.Table_Kind;
      Extent, Used : Natural;
      Buffer_Data : Firmware_Tables.Bytes (1 .. Table_Byte_Limit);
   end record;
   function Consistent (Server : State) return Boolean;
   type State (Table_Capacity, Byte_Capacity, Table_Byte_Limit, Load_Capacity : Positive; Initial_Revision : Revision_Number) is limited record
      Boot : ACPI_Bootstrap.State
        (Table_Capacity, Byte_Capacity, Table_Byte_Limit, Load_Capacity);
      Version : Revision_Number := Initial_Revision;
      Receipt : Receipts.State;
      Staged_Notification : ACPI_Service.Notification_Observation;
      Open : Boolean := False;
      ID : Positive := 1;
      Kind : ACPI_Service.Table_Kind := ACPI_Service.DSDT;
      Extent : Natural := 0;
      Used : Natural := 0;
      Buffer_Data : Firmware_Tables.Bytes (1 .. Table_Byte_Limit) := [others => 0];
   end record with Type_Invariant => Consistent (State);
   function Consistent (Server : State) return Boolean is
     (Receipts.Valid (Server.Receipt) and then Server.Used <= Server.Extent and then Server.Extent <= Server.Table_Byte_Limit and then
     (if Server.Open then ACPI_Bootstrap.Started (Server.Boot) and then
        ACPI_Bootstrap.Current (Server.Boot) = ACPI_Bootstrap.Receiving
        and then Server.Extent >= Firmware_Tables.Table_Header_Size
      else Server.Used = 0 and then Server.Extent = 0));
   function Current (Server : State) return Phase is
     (if not ACPI_Bootstrap.Started (Server.Boot) then Idle else
       (case ACPI_Bootstrap.Current (Server.Boot) is
          when ACPI_Bootstrap.Receiving => Receiving,
          when ACPI_Bootstrap.Complete => Complete,
          when ACPI_Bootstrap.Failed => Failed));
end ACPI_Requests;
