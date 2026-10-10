pragma Ada_2022;
package body ACPI_Bootstrap with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   function Valid (Session : State) return Boolean is (ACPI_Service.Valid (Session.Core));
   function Core_Model (Session : State) return ACPI_Service.State_Model is
     (ACPI_Service.Model (Session.Core));
   function Model (Session : State) return State_Model is
     (Table_Capacity => Session.Table_Capacity, Byte_Capacity => Session.Byte_Capacity,
      Core => Core_Model (Session), Stage => Session.Stage, Required => Session.Required,
      Begun => Session.Begun, Initialized => Session.Initialized,
      Initialization => Session.Initialization);
   use type ACPI_Service.Install_Status;
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
      ACPI_Service.Install (Session.Core, ID, Kind, Data, Result);
      if Result /= ACPI_Service.Installed then Session.Stage := Failed; end if;
      -- The core admits at most one table per call. Verify the advertised
      -- count at this boundary before exposing Receiving/Complete again.
      if ACPI_Service.Observe (Session.Core).Tables > Session.Required then Session.Stage := Failed; end if;
   end Import_Table;
   procedure Finish (Session : in out State) is
      Status : ACPI_Service.Values.Access_Status;
      use type ACPI_Service.Values.Access_Status;
   begin
      if Session.Stage = Receiving then
         if Installed (Session) = Session.Required then
            ACPI_Service.Initialize_Members (Session.Core, Session.Initialization, Status);
            if Status = ACPI_Service.Values.Available then
               Session.Initialized := True; Session.Stage := Complete;
            else Session.Stage := Failed; end if;
         else
            Session.Stage := Failed;
         end if;
      end if;
   end Finish;
end ACPI_Bootstrap;
