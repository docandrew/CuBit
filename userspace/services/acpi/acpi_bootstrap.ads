pragma Ada_2022;
with Firmware_Tables.DMAR;
with Firmware_Tables.MCFG;
with Firmware_Tables.MADT;
with Firmware_Tables.SRAT;
with Firmware_Tables.SLIT;
with ACPI_Service;
with Firmware_Tables;
package ACPI_Bootstrap with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   use type ACPI_Service.State_Model;
   use type ACPI_Service.Namespace.Initialization_Report;
   type Phase is (Receiving, Complete, Failed);
   type State (Table_Capacity, Byte_Capacity, Table_Byte_Limit : Positive) is limited private
     with Default_Initial_Condition => Valid (State) and then not Started (State)
       and then Current (State) = Failed and then Expected (State) = 0
       and then Installed (State) = 0;
   function Valid (Session : State) return Boolean;
   function Started (Session : State) return Boolean;
   type State_Model (Table_Capacity, Byte_Capacity : Positive) is private with Ghost;
   function Model (Session : State) return State_Model with Ghost,
     Post => Model'Result.Table_Capacity = Session.Table_Capacity
       and then Model'Result.Byte_Capacity = Session.Byte_Capacity;
   function Core_Model (Session : State) return ACPI_Service.State_Model with Ghost,
     Post => Core_Model'Result.Table_Capacity = Session.Table_Capacity
       and then Core_Model'Result.Byte_Capacity = Session.Byte_Capacity;
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
   -- includes static package member initialization, not full AML activation or device presence.
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
   procedure Finish (Session : in out State) with
     Pre => Valid (Session), Post => Valid (Session)
       and then Started (Session) = Started (Session)'Old
       and then Expected (Session) = Expected (Session)'Old
       and then Installed (Session) = Installed (Session)'Old
       and then (if Current (Session)'Old = Receiving and then
           Installed (Session)'Old = Expected (Session)'Old then
             Current (Session) = Complete and then Members_Initialized (Session)
             and then Pending_Members (Session) = 0
             and then ACPI_Service.Initialization_Frame (Core_Model (Session), Core_Model (Session)'Old)
             and then Member_Initialization (Session).Bound + Member_Initialization (Session).Missing
               + Member_Initialization (Session).Unsupported = Pending_Members (Session)'Old
         elsif Current (Session)'Old = Receiving then
             Current (Session) = Failed and then Core_Model (Session) = Core_Model (Session)'Old
             and then Members_Initialized (Session) = Members_Initialized (Session)'Old
             and then Member_Initialization (Session) = Member_Initialization (Session)'Old
         else Model (Session) = Model (Session)'Old);
private
   type State_Model (Table_Capacity, Byte_Capacity : Positive) is record
      Core : ACPI_Service.State_Model (Table_Capacity, Byte_Capacity);
      Stage : Phase;
      Required : Natural;
      Begun : Boolean;
      Initialized : Boolean;
      Initialization : ACPI_Service.Namespace.Initialization_Report;
   end record;
   type State (Table_Capacity, Byte_Capacity, Table_Byte_Limit : Positive) is limited record
      Core : ACPI_Service.State (Table_Capacity, Byte_Capacity, Table_Byte_Limit);
      Begun : Boolean := False;
      Initialized : Boolean := False;
      Initialization : ACPI_Service.Namespace.Initialization_Report := (others => 0);
      Stage : Phase := Failed;
      Required : Natural := 0;
   end record with Type_Invariant =>
     (if not State.Begun then State.Stage = Failed and then State.Required = 0
       and then ACPI_Service.Observe (State.Core).Tables = 0)
     and then State.Required <= State.Table_Capacity
     and then (if State.Stage in Receiving | Complete then
        State.Required > 0 and then ACPI_Service.Observe (State.Core).Tables <= State.Required)
     and then (if State.Stage = Complete then
        ACPI_Service.Observe (State.Core).Tables = State.Required
        and then State.Initialized and then ACPI_Service.Pending_Members (State.Core) = 0)
     and then (if not State.Initialized then State.Initialization = (0,0,0));
   function Started (Session : State) return Boolean is (Session.Begun);
end ACPI_Bootstrap;
