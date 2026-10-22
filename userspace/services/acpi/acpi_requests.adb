pragma Ada_2022;
with ACPI_Device_Policy;
with Firmware_Tables.DMAR;
with Firmware_Tables.MCFG;
with Firmware_Tables.MADT;
with Firmware_Tables.SRAT;
with Firmware_Tables.SLIT;
package body ACPI_Requests with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   function Valid (Server : State) return Boolean is
     (ACPI_Bootstrap.Valid (Server.Boot) and then Receipts.Valid (Server.Receipt));
   function Model (Server : State) return State_Model is
     (Table_Capacity => Server.Table_Capacity, Byte_Capacity => Server.Byte_Capacity,
      Table_Byte_Limit => Server.Table_Byte_Limit, Boot => ACPI_Bootstrap.Model (Server.Boot),
      Initial_Revision => Server.Initial_Revision,
      Version => Server.Version, Receipt => Server.Receipt,
      Staged_Notification => Server.Staged_Notification, Open => Server.Open, ID => Server.ID,
      Kind => Server.Kind, Extent => Server.Extent, Used => Server.Used,
      Buffer_Data => Server.Buffer_Data);
   use type ACPI_Service.Install_Status;
   use type ACPI_Service.State_Model;
   function Revision (Server : State) return Revision_Number is (Server.Version);
   function Table_Open (Server : State) return Boolean is (Server.Open);
   function Received (Server : State) return Natural is (Server.Used);
   function Completion (Server : State) return ACPI_Bootstrap.Finish_Report is
     (ACPI_Bootstrap.Completion (Server.Boot));
   function Fatal_Diagnostic (Server : State) return ACPI_Fatal_Diagnostics.Observation is
     (ACPI_Bootstrap.Fatal_Diagnostic (Server.Boot));
   function Observe (Server : State) return ACPI_Service.Metrics is (ACPI_Bootstrap.Observe (Server.Boot));
   procedure Import_Block
     (Server : in out State; Origin : Authority; Token : Unsigned_64;
      ID : Positive; Kind : ACPI_Service.Table_Kind;
      Data : Firmware_Tables.Bytes; Reply : out Response) is
      Result : ACPI_Service.Install_Status;
   begin
      Reply := (Status => Denied, Data => [others => 0]);
      if Origin = No_Authority then return; end if;
      Reply.Data (0) := Server.Version;
      if Origin /= Snapshot_Provider then return; end if;
      Reply.Status := Stale;
      if Token /= Server.Version then return; end if;
      Reply.Status := Resource_Limit;
      if Server.Version = Max_Revision then return; end if;
      Reply.Status := Malformed;
      if Data'Length < Firmware_Tables.Table_Header_Size
        or else Data'Length > Server.Table_Byte_Limit
      then return; end if;
      Reply.Status := Wrong_Order;
      if Current (Server) /= Receiving or else Server.Open or else
        ACPI_Bootstrap.Installed (Server.Boot) = ACPI_Bootstrap.Expected (Server.Boot)
      then return; end if;
      ACPI_Bootstrap.Import_Table (Server.Boot, ID, Kind, Data, Result);
      Server.Version := Server.Version + 1;
      Reply.Data (0) := Server.Version;
      Reply.Status := (if Result = ACPI_Service.Installed then OK else Table_Rejected);
      if Reply.Status = Table_Rejected then
         Reply.Data (1) := Unsigned_64 (ACPI_Service.Install_Status'Pos (Result));
      end if;
   end Import_Block;
   function Finish_Code (Item : ACPI_Bootstrap.Finish_Stage) return Finish_Stage_Code is
     (case Item is
        when ACPI_Bootstrap.Not_Attempted => Finish_Not_Attempted_Code,
        when ACPI_Bootstrap.Freeze_Failed => Finish_Freeze_Failed_Code,
        when ACPI_Bootstrap.Execution_Failed => Finish_Execution_Failed_Code,
        when ACPI_Bootstrap.Members_Failed => Finish_Members_Failed_Code,
        when ACPI_Bootstrap.Finished => Finish_Finished_Code);
   function Freeze_Code (Item : ACPI_Service.Freeze_Status) return Freeze_Outcome_Code is
     (case Item is
        when ACPI_Service.Catalog_Frozen_OK => Freeze_Catalog_Frozen_OK_Code,
        when ACPI_Service.Freeze_Wrong_Phase => Freeze_Freeze_Wrong_Phase_Code,
        when ACPI_Service.Freeze_Empty => Freeze_Freeze_Empty_Code);
   function Execution_Code (Item : ACPI_Service.Execution_Phase_Result) return Execution_Phase_Code is
     (case Item is
        when ACPI_Service.Execution_Not_Started => Execution_Execution_Not_Started_Code,
        when ACPI_Service.Preparation_Failed => Execution_Preparation_Failed_Code,
        when ACPI_Service.Module_Execution_Reported => Execution_Module_Execution_Reported_Code);
   function Preparation_Code (Item : ACPI_Service.Values.Preparation_Outcome) return Preparation_Outcome_Code is
     (case Item is
        when ACPI_Service.Values.Preparation_Not_Attempted => Preparation_Preparation_Not_Attempted_Code,
        when ACPI_Service.Values.Preparation_Rejected => Preparation_Preparation_Rejected_Code,
        when ACPI_Service.Values.Firmware_Prepared => Preparation_Firmware_Prepared_Code,
        when ACPI_Service.Values.Namespace_Capacity_Exceeded => Preparation_Namespace_Capacity_Exceeded_Code,
        when ACPI_Service.Values.Value_Capacity_Exceeded => Preparation_Value_Capacity_Exceeded_Code);
   function Access_Code (Item : ACPI_Service.Values.Access_Status) return Access_Outcome_Code is
     (case Item is
        when ACPI_Service.Values.Available => Access_Available_Code,
        when ACPI_Service.Values.Invalid_Value => Access_Invalid_Value_Code,
        when ACPI_Service.Values.Uninitialized_Element => Access_Uninitialized_Element_Code,
        when ACPI_Service.Values.Wrong_Kind => Access_Wrong_Kind_Code,
        when ACPI_Service.Values.Out_Of_Bounds => Access_Out_Of_Bounds_Code,
        when ACPI_Service.Values.Root_Limit => Access_Root_Limit_Code,
        when ACPI_Service.Values.Identity_Exhausted => Access_Identity_Exhausted_Code,
        when ACPI_Service.Values.Busy => Access_Busy_Code,
        when ACPI_Service.Values.Wrong_Phase => Access_Wrong_Phase_Code);
   function Module_Code (Item : ACPI_Service.Values.Module_Outcome) return Module_Outcome_Code is
     (case Item is
        when ACPI_Service.Values.Catalog_Rejected => Module_Catalog_Rejected_Code,
        when ACPI_Service.Values.Modules_Completed => Module_Modules_Completed_Code,
        when ACPI_Service.Values.Module_Rejected => Module_Module_Rejected_Code);
   function Device_Code (Item : ACPI_Bootstrap.Device_Phase) return Device_Phase_Code is
     (case Item is
        when ACPI_Bootstrap.Devices_Not_Started => Device_Not_Started_Code,
        when ACPI_Bootstrap.Devices_Preparing => Device_Preparing_Code,
        when ACPI_Bootstrap.Devices_Awaiting_Regions => Device_Awaiting_Regions_Code,
        when ACPI_Bootstrap.Devices_Walking => Device_Walking_Code,
        when ACPI_Bootstrap.Devices_Completed => Device_Completed_Code,
        when ACPI_Bootstrap.Devices_Completed_With_Errors => Device_Completed_With_Errors_Code,
        when ACPI_Bootstrap.Devices_Incomplete => Device_Incomplete_Code,
        when ACPI_Bootstrap.Devices_Failed => Device_Failed_Code);
   function Region_Code (Item : ACPI_Device_Policy.Region_Outcome) return Region_Status_Code is
     (case Item is
        when ACPI_Device_Policy.Regions_Connected => Regions_Connected_Code,
        when ACPI_Device_Policy.No_Regions_Required => No_Regions_Required_Code,
        when ACPI_Device_Policy.Regions_Unavailable => Regions_Unavailable_Code,
        when ACPI_Device_Policy.Region_Initialization_Failed => Region_Failed_Code);
   function Error_Code (Item : AML_Execute.Execution_Status) return Startup_Error_Code is
     (case Item is
        when AML_Execute.Returned => Error_Returned,
        when AML_Execute.Object_Returned => Error_Object_Returned,
        when AML_Execute.Reference_Returned => Error_Reference_Returned,
        when AML_Execute.Metadata_Returned => Error_Metadata_Returned,
        when AML_Execute.No_Return => Error_No_Return,
        when AML_Execute.Truncated => Error_Truncated,
        when AML_Execute.Unsupported => Error_Unsupported,
        when AML_Execute.Uninitialized => Error_Uninitialized,
        when AML_Execute.Missing_Argument => Error_Missing_Argument,
        when AML_Execute.Budget_Exceeded => Error_Budget_Exceeded,
        when AML_Execute.Invalid_Method => Error_Invalid_Method,
        when AML_Execute.Argument_Mismatch => Error_Argument_Mismatch,
        when AML_Execute.Mutex_Order => Error_Mutex_Order,
        when AML_Execute.Duplicate_Name => Error_Duplicate_Name,
        when AML_Execute.Namespace_Limit => Error_Namespace_Limit,
        when AML_Execute.Expression_Limit => Error_Expression_Limit,
        when AML_Execute.Block_Limit => Error_Block_Limit,
        when AML_Execute.Bad_Package => Error_Bad_Package,
        when AML_Execute.Invalid_Control => Error_Invalid_Control,
        when AML_Execute.Unknown_Name => Error_Unknown_Name,
        when AML_Execute.Unsupported_Value => Error_Unsupported_Value,
        when AML_Execute.Bad_Name => Error_Bad_Name,
        when AML_Execute.Call_Limit => Error_Call_Limit,
        when AML_Execute.Division_By_Zero => Error_Division_By_Zero,
        when AML_Execute.Empty_Buffer => Error_Empty_Buffer,
        when AML_Execute.Missing_Result => Error_Missing_Result,
        when AML_Execute.Value_Limit => Error_Value_Limit,
        when AML_Execute.Numeric_Overflow => Error_Numeric_Overflow,
        when AML_Execute.Invalid_Delay => Error_Invalid_Delay,
        when AML_Execute.Delay_Failed => Error_Delay_Failed,
        when AML_Execute.Package_Limit => Error_Package_Limit,
        when AML_Execute.Invalid_Match_Operation => Error_Invalid_Match_Operation,
        when AML_Execute.No_Resource_End_Tag => Error_No_Resource_End_Tag,
        when AML_Execute.Invalid_Resource_Type => Error_Invalid_Resource_Type,
        when AML_Execute.Bad_Resource_Length => Error_Bad_Resource_Length,
        when AML_Execute.Resource_Buffer_Length => Error_Resource_Buffer_Length,
        when AML_Execute.Invalid_Field_Length => Error_Invalid_Field_Length,
        when AML_Execute.Buffer_Field_Limit => Error_Buffer_Field_Limit,
        when AML_Execute.Notify_Limit => Error_Notify_Limit,
        when AML_Execute.Module_Completed => Error_Module_Completed,
        when AML_Execute.Mutex_Not_Acquired => Error_Mutex_Not_Acquired,
        when AML_Execute.Fatal_Requested => Error_Fatal_Requested,
        when AML_Execute.Field_Indeterminate => Error_Field_Indeterminate);
   -- Read requests cannot modify the snapshot or its upload state.
   procedure Read_Request (Server : State; Request : Packet; Reply : out Response)
     with Pre => Consistent (Server) and then Valid (Server) and then
       Request.Label in Read_Metrics | Read_Table_Info | Read_Table_Chunk | Read_MCFG_Info .. Read_DMAR_Path,
       Post => Reply.Data (0) = Revision (Server)
         and then (for all Word of Reply.Data => Word <= Max_Revision)
   is
      Amount : Natural range 0 .. 16;
   begin
      Reply := (Status => Malformed, Data => [0 => Server.Version, others => 0]);
      if Request.Label = Read_Metrics then
         if Request.Data (0) > Metrics_Last_Page or else Request.Data (1 .. 3) /= [0, 0, 0] then return; end if;
         declare
            Finished : constant ACPI_Bootstrap.Finish_Report := Completion (Server);
            Stats : constant ACPI_Service.Metrics := Observe (Server);
            Devices : constant ACPI_Bootstrap.Device_Report := ACPI_Bootstrap.Device_Completion (Server.Boot);
         begin
            Reply.Status := OK;
            case Request.Data (0) is
               when 0 => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (Phase'Enum_Rep (Current (Server))),
                  Unsigned_64 (ACPI_Bootstrap.Expected (Server.Boot)), Unsigned_64 (Stats.Tables)];
               when 1 => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (Stats.Bytes), Unsigned_64 (Stats.Objects), Unsigned_64 (Stats.Value_Objects)];
               when 2 => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (Stats.Value_Bytes), Unsigned_64 (Stats.Package_Elements), Unsigned_64 (Stats.Rejections)];
               when 3 => Reply.Data (1 .. 3) :=
                 [Boolean'Pos (Stats.Counter_Saturated), Boolean'Pos (Server.Open), Unsigned_64 (Server.Used)];
               when 4 => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (Server.Table_Byte_Limit), Unsigned_64 (Server.Byte_Capacity),
                  Unsigned_64 (Server.Table_Capacity)];
               when 5 => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (Stats.Method_Bytes),
                  Unsigned_64 (ACPI_Service.Method_Storage_Capacity),
                  Unsigned_64 (ACPI_Service.Method_Storage_Capacity - Stats.Method_Bytes)];
               when 6 => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (Stats.Last_Load_Code), Unsigned_64 (ACPI_Service.Max_Namespace_Nodes),
                  Unsigned_64 (ACPI_Service.Max_Namespace_Nodes - Stats.Objects)];
               when 7 =>
                  declare
                     Report : constant ACPI_Service.Namespace.Initialization_Report :=
                       ACPI_Bootstrap.Member_Initialization (Server.Boot);
                  begin
                     Reply.Data (1 .. 3) := [Unsigned_64 (Report.Bound),
                       Unsigned_64 (Report.Missing), Unsigned_64 (Report.Unsupported)];
                  end;
               when Metrics_Members_State_Page => Reply.Data (1 .. 3) :=
                 [Boolean'Pos (ACPI_Bootstrap.Members_Initialized (Server.Boot)),
                  Unsigned_64 (ACPI_Bootstrap.Pending_Members (Server.Boot)), 0];
               when Metrics_Device_State_Page => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (Device_Phase_Code'Enum_Rep (Device_Code (Devices.Stage))),
                  Unsigned_64 (Region_Status_Code'Enum_Rep (Region_Code (Devices.Region_Status))),
                  Unsigned_64 (Devices.Errors)];
               when Metrics_Startup_Fuel_Page => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (ACPI_Bootstrap.Startup_Charged (Server.Boot)),
                  Unsigned_64 (Devices.Charged),
                  Unsigned_64 (ACPI_Bootstrap.Completion (Server.Boot).Execution.Modules.Charged)];
               when Metrics_Device_Calls_Page => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (Devices.Status_Methods), Unsigned_64 (Devices.Initialization_Methods),
                  Unsigned_64 (Devices.First_Error_Node)];
               when Metrics_Device_Error_Page => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (Startup_Error_Code'Enum_Rep (Error_Code (Devices.First_Error))), 0, 0];
               when Metrics_Completion_State_Page => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (Finish_Stage_Code'Enum_Rep (Finish_Code (Finished.Stage))),
                  Unsigned_64 (Freeze_Outcome_Code'Enum_Rep (Freeze_Code (Finished.Freeze))),
                  Unsigned_64 (Execution_Phase_Code'Enum_Rep (Execution_Code (Finished.Execution.Phase)))];
               when Metrics_Completion_Access_Page => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (Preparation_Outcome_Code'Enum_Rep (Preparation_Code (Finished.Execution.Preparation))),
                  Unsigned_64 (Access_Outcome_Code'Enum_Rep (Access_Code (Finished.Execution_Status))),
                  Unsigned_64 (Access_Outcome_Code'Enum_Rep (Access_Code (Finished.Member_Status)))];
               when Metrics_Module_State_Page => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (Module_Outcome_Code'Enum_Rep (Module_Code (Finished.Execution.Modules.Outcome))),
                  Unsigned_64 (Finished.Execution.Modules.Completed),
                  Unsigned_64 (Finished.Execution.Failed_Table_ID)];
               when Metrics_Module_Error_Page => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (Startup_Error_Code'Enum_Rep (Error_Code (Finished.Execution.Modules.Failure))),
                  0,
                  0];
               when others => null; -- Rejected by the upper-bound guard.

            end case;
         end;
         return;
      end if;
      if Request.Label in Read_MADT_Info .. Read_MADT_Fields then
         if Request.Label = Read_MADT_Info then
            if Request.Data (2) > MADT_Address_Page or else Request.Data (3) /= 0 then return; end if;
         elsif Request.Label = Read_MADT_Record then
            if Request.Data (3) /= 0 then return; end if;
         elsif Request.Data (3) > MADT_Override_Flags_Page then return;
         end if;
         if Request.Data (0) /= Server.Version then Reply.Status := Stale; return; end if;
         if Current (Server) /= Complete then Reply.Status := Wrong_Order; return; end if;
         Reply.Status := Not_Found;
         if Request.Data (1) = 0 or else
           Request.Data (1) > Unsigned_64 (ACPI_Bootstrap.Installed (Server.Boot))
         then return; end if;
         declare
            Index : constant Positive := Positive (Request.Data (1));
            Low_Mask : constant Unsigned_64 := 16#FFFF_FFFF#;
            Info : Firmware_Tables.MADT.Table_Metadata;
         begin
            Reply.Status := Wrong_Table_Kind;
            if ACPI_Bootstrap.Table_Info (Server.Boot, Index).Signature /= "APIC" then return; end if;
            Reply.Status := Table_Rejected;
            Info := ACPI_Bootstrap.MADT_Info (Server.Boot, Index);
            if not Info.Valid then return; end if;
            if Request.Label = Read_MADT_Info then
               if Request.Data (2) = MADT_Header_Page then
                  Reply.Data (1 .. 3) := [Unsigned_64 (Info.Revision), Unsigned_64 (Info.Count), Unsigned_64 (Info.Flags)];
               else
                  Reply.Data (1 .. 3) := [Unsigned_64 (Info.Local_Address) and Low_Mask,
                    Shift_Right (Unsigned_64 (Info.Local_Address), 32), 0];
               end if;
            else
               Reply.Status := Index_Out_Of_Range;
               if Request.Data (2) > Unsigned_64 (Natural'Last) then return; end if;
               declare
                  Item : constant Firmware_Tables.MADT.Record_Result :=
                    ACPI_Bootstrap.MADT_Record (Server.Boot, Index, Natural (Request.Data (2)));
                  use Firmware_Tables.MADT;
               begin
                  if not Item.Valid then return; end if;
                  if Request.Label = Read_MADT_Record then
                     Reply.Data (1 .. 3) := [Unsigned_64 (Item.Value.Wire_Type),
                       Unsigned_64 (Item.Value.Offset), Unsigned_64 (Item.Value.Length)];
                  else
                     Reply.Status := Unsupported_Record_Kind;
                     if Item.Value.Kind = Unknown then return; end if;
                     Reply.Status := Malformed;
                     if Request.Data (3) = MADT_Override_Flags_Page then
                        if Item.Value.Kind /= Source_Override then return; end if;
                        Reply.Data (1) := Unsigned_64 (Item.Value.Override_Flags);
                     else
                        case Item.Value.Kind is
                           when Local_APIC | Local_X2APIC =>
                              Reply.Data (1 .. 3) := [Unsigned_64 (Item.Value.UID),
                                Unsigned_64 (Item.Value.Controller), Unsigned_64 (Item.Value.CPU_Flags)];
                           when IO_APIC =>
                              Reply.Data (1 .. 3) := [Unsigned_64 (Item.Value.IO_ID),
                                Unsigned_64 (Item.Value.IO_Address), Unsigned_64 (Item.Value.Interrupt_Base)];
                           when Source_Override =>
                              Reply.Data (1 .. 3) := [Unsigned_64 (Item.Value.Bus),
                                Unsigned_64 (Item.Value.Source), Unsigned_64 (Item.Value.Global_Interrupt)];
                           when NMI_Source =>
                              Reply.Data (1 .. 3) := [Unsigned_64 (Item.Value.NMI_Interrupt), Unsigned_64 (Item.Value.NMI_Flags), 0];
                           when Local_NMI | X2APIC_NMI =>
                              Reply.Data (1 .. 3) := [Unsigned_64 (Item.Value.NMI_UID),
                                Unsigned_64 (Item.Value.LINT), Unsigned_64 (Item.Value.Local_Flags)];
                           when Address_Override =>
                              Reply.Data (1 .. 3) := [Unsigned_64 (Item.Value.Address) and Low_Mask,
                                Shift_Right (Unsigned_64 (Item.Value.Address), 32), 0];
                           when Unknown => return;
                        end case;
                     end if;
                  end if;
               end;
            end if;
            Reply.Status := OK;
         end;
         return;
      end if;
      if Request.Label in Read_DMAR_Info .. Read_DMAR_Path then
         declare
            use Firmware_Tables.DMAR;
            Max_Scope : constant Unsigned_64 := Unsigned_64 (Scope_Count'Last);
            Selector : constant Unsigned_64 := Request.Data (3);
            Scope_Index : Scope_Count := 0;
            Path_Index : Path_Count := 0;
            Page : Unsigned_64 := Selector;
            Low_Mask : constant Unsigned_64 := 16#FFFF_FFFF#;
         begin
            case Request.Label is
               when Read_DMAR_Info =>
                  if Request.Data (2) not in DMAR_Header_Page or else Selector /= 0 then return; end if;
               when Read_DMAR_Record =>
                  if Selector not in DMAR_Record_Page then return; end if;
               when Read_DMAR_Fields =>
                  if Selector not in DMAR_Field_Page then return; end if;
               when Read_DMAR_Scope =>
                  if Selector > Max_Scope * DMAR_Scope_Page_Radix + DMAR_Scope_Page'Last then return; end if;
                  Page := Selector mod DMAR_Scope_Page_Radix;
                  if Page not in DMAR_Scope_Page or else Selector / DMAR_Scope_Page_Radix = 0 then return; end if;
                  Scope_Index := Scope_Count (Selector / DMAR_Scope_Page_Radix);
               when Read_DMAR_Path =>
                  if Selector > Max_Scope * DMAR_Path_Index_Radix + Unsigned_64 (Path_Count'Last) then return; end if;
                  if Selector / DMAR_Path_Index_Radix = 0 or else
                    Selector mod DMAR_Path_Index_Radix not in 1 .. Unsigned_64 (Path_Count'Last)
                  then return; end if;
                  Scope_Index := Scope_Count (Selector / DMAR_Path_Index_Radix);
                  Path_Index := Path_Count (Selector mod DMAR_Path_Index_Radix);
               when others => return;
            end case;
            if Request.Data (0) /= Server.Version then Reply.Status := Stale; return; end if;
            if Current (Server) /= Complete then Reply.Status := Wrong_Order; return; end if;
            Reply.Status := Not_Found;
            if Request.Data (1) = 0 or else Request.Data (1) > Unsigned_64 (ACPI_Bootstrap.Installed (Server.Boot)) then return; end if;
            declare
               Index : constant Positive := Positive (Request.Data (1));
               Info : Table_Metadata;
            begin
               Reply.Status := Wrong_Table_Kind;
               if ACPI_Bootstrap.Table_Info (Server.Boot, Index).Signature /= "DMAR" then return; end if;
               Reply.Status := Table_Rejected;
               Info := ACPI_Bootstrap.DMAR_Info (Server.Boot, Index);
               if not Info.Valid then return; end if;
               if Request.Label = Read_DMAR_Info then
                  if Request.Data (2) = 0 then
                     Reply.Data (1 .. 3) := [Unsigned_64 (Info.Revision), Unsigned_64 (Info.Host_Width), Unsigned_64 (Info.Flags)];
                  else Reply.Data (1) := Unsigned_64 (Info.Count);
                  end if;
               else
                  Reply.Status := Index_Out_Of_Range;
                  if Request.Data (2) = 0 or else Request.Data (2) > Unsigned_64 (Natural'Last) then return; end if;
                  declare
                     Rec : constant Natural := Natural (Request.Data (2));
                     Item : constant Record_Result := ACPI_Bootstrap.DMAR_Record (Server.Boot, Index, Rec);
                  begin
                     if not Item.Valid then return; end if;
                     if Request.Label = Read_DMAR_Record then
                        if Page = 0 then
                           Reply.Data (1 .. 3) := [Unsigned_64 (Item.Value.Wire_Type), Unsigned_64 (Item.Value.Offset), Unsigned_64 (Item.Value.Length)];
                        else Reply.Data (1) := Unsigned_64 (Item.Value.Scopes);
                        end if;
                     elsif Request.Label = Read_DMAR_Fields then
                        Reply.Status := Unsupported_Record_Kind;
                        if Item.Value.Kind = Unknown then return; end if;
                        Reply.Status := Malformed;
                        declare
                           V : Record_Data renames Item.Value;
                           Address : Firmware_Tables.Address_Value := 0;
                        begin
                           if Page = 0 then
                              case V.Kind is
                                 when Hardware_Unit => Reply.Data (1 .. 3) := [Unsigned_64 (V.Unit_Flags), Unsigned_64 (V.Register_Size), Unsigned_64 (V.Unit_Segment)];
                                 when Reserved_Memory => Reply.Data (1) := Unsigned_64 (V.Memory_Segment);
                                 when ATS_Root | SATC => Reply.Data (1 .. 2) := [Unsigned_64 (V.Cache_Flags), Unsigned_64 (V.Cache_Segment)];
                                 when Hardware_Affinity => Reply.Data (1) := Unsigned_64 (V.Domain);
                                 when Namespace_Device => Reply.Data (1 .. 3) := [Unsigned_64 (V.Device_Number), Unsigned_64 (V.Name_Offset), Unsigned_64 (V.Name_Length)];
                                 when SIDP => Reply.Data (1) := Unsigned_64 (V.Device_Segment);
                                 when Unknown => return;
                              end case;
                           else
                              case V.Kind is
                                 when Hardware_Unit => if Page /= 1 then return; end if; Address := V.Register_Base;
                                 when Reserved_Memory => Address := (if Page = 1 then V.Base_Address else V.Inclusive_Limit);
                                 when Hardware_Affinity => if Page /= 1 then return; end if; Address := V.Affinity_Base;
                                 when others => return;
                              end case;
                              Reply.Data (1 .. 2) := [Address and Low_Mask, Shift_Right (Address, 32)];
                           end if;
                        end;
                     else
                        if Item.Value.Kind = Unknown then Reply.Status := Unsupported_Record_Kind; return; end if;
                        declare
                           Scope : constant Scope_Result := ACPI_Bootstrap.DMAR_Scope (Server.Boot, Index, Rec, Scope_Index);
                        begin
                           if not Scope.Valid then return; end if;
                           if Request.Label = Read_DMAR_Scope then
                              case DMAR_Scope_Page (Page) is
                                 when 0 => Reply.Data (1 .. 3) := [Unsigned_64 (Scope.Wire_Type), Unsigned_64 (Scope.Offset), Unsigned_64 (Scope.Length)];
                                 when 1 => Reply.Data (1 .. 3) := [Unsigned_64 (Scope.Flags), Unsigned_64 (Scope.Reserved), Unsigned_64 (Scope.Enumeration_ID)];
                                 when 2 => Reply.Data (1 .. 3) := [Unsigned_64 (Scope.Start_Bus), Unsigned_64 (Scope.Paths), Boolean'Pos (Scope.Known_Path)];
                              end case;
                           else
                              if not Scope.Known_Path then Reply.Status := Unsupported_Record_Kind; return; end if;
                              declare
                                 Path : constant Path_Result := ACPI_Bootstrap.DMAR_Path (Server.Boot, Index, Rec, Scope_Index, Path_Index);
                              begin
                                 if not Path.Valid then return; end if;
                                 Reply.Data (1 .. 2) := [Unsigned_64 (Path.Device), Unsigned_64 (Path.Func)];
                              end;
                           end if;
                        end;
                     end if;
                  end;
               end if;
               Reply.Status := OK;
            end;
         end;
         return;
      end if;
      if Request.Label in Read_SRAT_Info .. Read_SRAT_Fields then
         if Request.Label = Read_SRAT_Info then
            if Request.Data (2) > SRAT_Reserved_Page or else Request.Data (3) /= 0 then return; end if;
         elsif Request.Label = Read_SRAT_Record then
            if Request.Data (3) /= 0 then return; end if;
         elsif Request.Data (3) > SRAT_Handle_Last_Page then return;
         end if;
         if Request.Data (0) /= Server.Version then Reply.Status := Stale; return; end if;
         if Current (Server) /= Complete then Reply.Status := Wrong_Order; return; end if;
         Reply.Status := Not_Found;
         if Request.Data (1) = 0 or else
           Request.Data (1) > Unsigned_64 (ACPI_Bootstrap.Installed (Server.Boot))
         then return; end if;
         declare
            Index : constant Positive := Positive (Request.Data (1));
            Low_Mask : constant Unsigned_64 := 16#FFFF_FFFF#;
            Info : Firmware_Tables.SRAT.Table_Metadata;
         begin
            Reply.Status := Wrong_Table_Kind;
            if ACPI_Bootstrap.Table_Info (Server.Boot, Index).Signature /= "SRAT" then return; end if;
            Reply.Status := Table_Rejected;
            Info := ACPI_Bootstrap.SRAT_Info (Server.Boot, Index);
            if not Info.Valid then return; end if;
            if Request.Label = Read_SRAT_Info then
               if Request.Data (2) = SRAT_Header_Page then
                  Reply.Data (1 .. 3) := [Unsigned_64 (Info.Revision), Unsigned_64 (Info.Count), Unsigned_64 (Info.Table_Revision)];
               else
                  Reply.Data (1 .. 3) := [Info.Reserved and Low_Mask,
                    Shift_Right (Info.Reserved, 32), 0];
               end if;
            else
               Reply.Status := Index_Out_Of_Range;
               if Request.Data (2) > Unsigned_64 (Natural'Last) then return; end if;
               declare
                  Item : constant Firmware_Tables.SRAT.Record_Result :=
                    ACPI_Bootstrap.SRAT_Record (Server.Boot, Index, Natural (Request.Data (2)));
                  use Firmware_Tables.SRAT;
               begin
                  if not Item.Valid then return; end if;
                  if Request.Label = Read_SRAT_Record then
                     Reply.Data (1 .. 3) := [Unsigned_64 (Item.Value.Wire_Type),
                       Unsigned_64 (Item.Value.Offset), Unsigned_64 (Item.Value.Length)];
                  else
                     Reply.Status := Unsupported_Record_Kind;
                     if Item.Value.Kind = Unknown then return; end if;
                     Reply.Status := Malformed;
                     declare
                        Page : constant Unsigned_64 := Request.Data (3);
                        V : Record_Data renames Item.Value;
                     begin
                        case V.Kind is
                           when Local_APIC =>
                              if Page = SRAT_Common_Page then
                                 Reply.Data (1 .. 3) := [Unsigned_64 (V.Domain_Low), Unsigned_64 (V.Domain_High), Unsigned_64 (V.APIC_ID)];
                              elsif Page = SRAT_Detail_Page then
                                 Reply.Data (1 .. 3) := [Unsigned_64 (V.APIC_Flags), Unsigned_64 (V.SAPIC_EID), Unsigned_64 (V.APIC_Clock)];
                              else return; end if;
                           when Memory_Affinity =>
                              if Page = SRAT_Common_Page then
                                 Reply.Data (1 .. 3) := [Unsigned_64 (V.Memory_Domain), Unsigned_64 (V.Memory_Flags), 0];
                              elsif Page = SRAT_Base_Page then
                                 Reply.Data (1 .. 3) := [V.Base_Address and Low_Mask, Shift_Right (V.Base_Address, 32), 0];
                              elsif Page = SRAT_Length_Page then
                                 Reply.Data (1 .. 3) := [V.Address_Length and Low_Mask, Shift_Right (V.Address_Length, 32), 0];
                              else return; end if;
                           when X2APIC =>
                              if Page = SRAT_Common_Page then
                                 Reply.Data (1 .. 3) := [Unsigned_64 (V.X2_Domain), Unsigned_64 (V.X2_ID), Unsigned_64 (V.X2_Flags)];
                              elsif Page = SRAT_Detail_Page then Reply.Data (1) := Unsigned_64 (V.X2_Clock);
                              else return; end if;
                           when GICC =>
                              if Page = SRAT_Common_Page then
                                 Reply.Data (1 .. 3) := [Unsigned_64 (V.GICC_Domain), Unsigned_64 (V.GICC_UID), Unsigned_64 (V.GICC_Flags)];
                              elsif Page = SRAT_Detail_Page then Reply.Data (1) := Unsigned_64 (V.GICC_Clock);
                              else return; end if;
                           when GIC_ITS =>
                              if Page /= SRAT_Common_Page then return; end if;
                              Reply.Data (1 .. 3) := [Unsigned_64 (V.ITS_Domain), Unsigned_64 (V.ITS_ID), 0];
                           when Generic_Initiator | Generic_Port =>
                              if Page = SRAT_Common_Page then
                                 Reply.Data (1 .. 3) := [Unsigned_64 (V.Generic_Domain), Unsigned_64 (V.Handle_Type), Unsigned_64 (V.Generic_Flags)];
                              elsif Page in SRAT_Handle_First_Page .. SRAT_Handle_Last_Page then
                                 declare First : constant Natural := Natural (Page - SRAT_Handle_First_Page) * SRAT_Handle_Bytes_Per_Page; begin
                                    for I in 0 .. SRAT_Handle_Bytes_Per_Page - 1 loop
                                       if First + I < Handle_Size then
                                          Reply.Data (1 + I) := Unsigned_64 (V.Handle (First + I));
                                       end if;
                                    end loop;
                                 end;
                              else return; end if;
                           when RINTC =>
                              if Page = SRAT_Common_Page then
                                 Reply.Data (1 .. 3) := [Unsigned_64 (V.RINTC_Domain), Unsigned_64 (V.RINTC_UID), Unsigned_64 (V.RINTC_Flags)];
                              elsif Page = SRAT_Detail_Page then Reply.Data (1) := Unsigned_64 (V.RINTC_Clock);
                              else return; end if;
                           when Unknown => return;
                        end case;
                     end;
                  end if;
               end;
            end if;
            Reply.Status := OK;
         end;
         return;
      end if;
      if Request.Label in Read_MCFG_Info .. Read_SLIT_Distance then
         if Request.Label = Read_SLIT_Info then
            if Request.Data (2 .. 3) /= [0, 0] then return; end if;
         elsif Request.Label = Read_MCFG_Info then
            if Request.Data (2) > MCFG_Reserved_Page or else Request.Data (3) /= 0 then return; end if;
         elsif Request.Label = Read_MCFG_Allocation then
            if Request.Data (3) > MCFG_Bus_Page then return; end if;
         end if;
         if Request.Data (0) /= Server.Version then Reply.Status := Stale; return; end if;
         if Current (Server) /= Complete then Reply.Status := Wrong_Order; return; end if;
         Reply.Status := Not_Found;
         if Request.Data (1) = 0 or else
           Request.Data (1) > Unsigned_64 (ACPI_Bootstrap.Installed (Server.Boot))
         then return; end if;
         declare
            Index : constant Positive := Positive (Request.Data (1));
            Low_Mask : constant Unsigned_64 := 16#FFFF_FFFF#;
         begin
            Reply.Status := Wrong_Table_Kind;
            if ACPI_Bootstrap.Table_Info (Server.Boot, Index).Signature /=
              (if Request.Label in Read_MCFG_Info | Read_MCFG_Allocation then "MCFG" else "SLIT")
            then return; end if;
            Reply.Status := Table_Rejected;
            case Request.Label is
               when Read_MCFG_Info =>
                  declare
                     Info : constant Firmware_Tables.MCFG.Table_Metadata :=
                       ACPI_Bootstrap.MCFG_Info (Server.Boot, Index);
                  begin
                     if not Info.Valid then return; end if;
                     if Request.Data (2) = MCFG_Header_Page then
                        Reply.Data (1 .. 3) := [Unsigned_64 (Info.Revision), Unsigned_64 (Info.Count), 0];
                     else
                        Reply.Data (1 .. 3) := [Info.Reserved and Low_Mask, Shift_Right (Info.Reserved, 32), 0];
                     end if;
                  end;
               when Read_MCFG_Allocation =>
                  if not ACPI_Bootstrap.MCFG_Info (Server.Boot, Index).Valid then return; end if;
                  Reply.Status := Index_Out_Of_Range;
                  if Request.Data (2) > Unsigned_64 (Natural'Last) then return; end if;
                  declare
                     Item : constant Firmware_Tables.MCFG.Allocation_Result :=
                       ACPI_Bootstrap.MCFG_Allocation (Server.Boot, Index, Natural (Request.Data (2)));
                  begin
                     if not Item.Valid then return; end if;
                     if Request.Data (3) = MCFG_Address_Page then
                        Reply.Data (1 .. 3) := [Unsigned_64 (Item.Value.Base) and Low_Mask,
                          Shift_Right (Unsigned_64 (Item.Value.Base), 32), Unsigned_64 (Item.Value.Segment)];
                     else
                        Reply.Data (1 .. 3) := [Unsigned_64 (Item.Value.First_Bus),
                          Unsigned_64 (Item.Value.Last_Bus), Unsigned_64 (Item.Value.Reserved)];
                     end if;
                  end;
               when Read_SLIT_Info =>
                  declare
                     Info : constant Firmware_Tables.SLIT.Table_Metadata :=
                       ACPI_Bootstrap.SLIT_Info (Server.Boot, Index);
                  begin
                     if not Info.Valid then return; end if;
                     Reply.Data (1 .. 3) := [Unsigned_64 (Info.Revision), Unsigned_64 (Info.Count), 0];
                  end;
               when Read_SLIT_Distance =>
                  if not ACPI_Bootstrap.SLIT_Info (Server.Boot, Index).Valid then return; end if;
                  Reply.Status := Index_Out_Of_Range;
                  if Request.Data (2) > Unsigned_64 (Natural'Last) or else
                    Request.Data (3) > Unsigned_64 (Natural'Last) then return; end if;
                  declare
                     Item : constant Firmware_Tables.SLIT.Distance_Result :=
                       ACPI_Bootstrap.SLIT_Distance (Server.Boot, Index,
                         Natural (Request.Data (2)), Natural (Request.Data (3)));
                  begin
                     if not Item.Valid then return; end if;
                     Reply.Data (1) := Unsigned_64 (Item.Value);
                  end;
               when others => return;
            end case;
            Reply.Status := OK;
         end;
         return;
      end if;
      if Request.Label in Read_Table_Info | Read_Table_Chunk then
         if Request.Data (3) /= 0 or else
           (Request.Label = Read_Table_Info and then Request.Data (2) /= 0)
         then return; end if;
         if Request.Data (0) /= Server.Version then Reply.Status := Stale; return; end if;
         if Current (Server) /= Complete then Reply.Status := Wrong_Order; return; end if;
         if Request.Data (1) = 0 or else
           Request.Data (1) > Unsigned_64 (Server.Table_Capacity)
         then Reply.Status := Not_Found; return; end if;
         declare
            Index : constant Positive := Positive (Request.Data (1));
            Info : ACPI_Service.Table_Metadata;
            Low, High : Unsigned_32 := 0;
            Offset : Natural;
         begin
            if Index > ACPI_Bootstrap.Installed (Server.Boot) then
               Reply.Status := Not_Found; return;
            end if;
            Info := ACPI_Bootstrap.Table_Info (Server.Boot, Index);
            if Request.Label = Read_Table_Info then
               for I in Info.Signature'Range loop
                  Low := Low or Shift_Left (Unsigned_32 (Character'Pos (Info.Signature (I))), (I - 1) * 8);
               end loop;
               Reply.Data (1 .. 3) := [Unsigned_64 (Info.ID), Unsigned_64 (Info.Extent), Unsigned_64 (Low)];
            else
               if Request.Data (2) > Unsigned_64 (Server.Table_Byte_Limit) then return; end if;
               Offset := Natural (Request.Data (2));
               if Offset > Info.Extent then return; end if;
               Amount := Natural'Min (8, Info.Extent - Offset);
               for I in 0 .. Amount - 1 loop
                  declare
                     B : constant Unsigned_32 := Unsigned_32 (ACPI_Bootstrap.Table_Byte (Server.Boot, Index, Offset + I));
                  begin
                     if I < 4 then Low := Low or Shift_Left (B, I * 8);
                     else High := High or Shift_Left (B, (I - 4) * 8); end if;
                  end;
               end loop;
               Reply.Data (1 .. 3) := [Unsigned_64 (Amount), Unsigned_64 (Low), Unsigned_64 (High)];
            end if;
            Reply.Status := OK;
         end;
         return;
      end if;
   end Read_Request;
   function Notify_Kind (Kind : ACPI_Service.Namespace.Notification_Target_Kind)
     return Notification_Kind_Code is
     (case Kind is
        when ACPI_Service.Namespace.Device_Notification => Notify_Device,
        when ACPI_Service.Namespace.Processor_Notification => Notify_Processor,
        when ACPI_Service.Namespace.Thermal_Notification => Notify_Thermal);
   function Notify_Class (Class : ACPI_Service.Namespace.Notification_Class)
     return Notification_Class_Code is
     (case Class is
        when ACPI_Service.Namespace.System_Notify => Notify_System_Class,
        when ACPI_Service.Namespace.Device_Notify => Notify_Device_Class);
   procedure Observe_Notification (Server : State; Request : Packet; Reply : out Response) is
      ID : constant Receipt_Number := Receipts.Current (Server.Receipt);
      Item : ACPI_Service.Notification_Observation renames Server.Staged_Notification;
      Packed : Unsigned_64 := 0;
   begin
      Reply := (Status => Malformed, Data => [0 => Server.Version, others => 0]);
      if Request.Label = Read_Notification_State then
         if Request.Data (0) not in Notification_State_Page
           or else Request.Data (1 .. 3) /= [0,0,0] then return; end if;
         if Request.Data (0) = Notification_State_Summary then
            Reply.Data (1 .. 3) :=
              [Boolean'Pos (ACPI_Bootstrap.Notifications_Requested (Server.Boot)) +
               2 * Boolean'Pos (ACPI_Bootstrap.Notifications_Enabled (Server.Boot)),
               Unsigned_64 (ACPI_Bootstrap.Pending_Notifications (Server.Boot)), ID];
         else
            Reply.Data (1 .. 3) :=
              [Unsigned_64 (Access_Outcome_Code'Enum_Rep
                 (Access_Code (ACPI_Bootstrap.Notification_Status (Server.Boot)))),
               Unsigned_64 (ACPI_Service.Notification_Count'Last), 1];
         end if;
         Reply.Status := OK;
         return;
      end if;
      if Request.Data (1) not in Notification_Page or else Request.Data (3) /= 0
        or else (Request.Data (1) /= Notification_Path_Page and then Request.Data (2) /= 0)
      then return; end if;
      if ID = 0 or else Request.Data (0) /= ID then Reply.Status := Stale; return; end if;
      case Notification_Page (Request.Data (1)) is
         when Notification_Data_Page =>
            Reply.Data (1 .. 3) :=
              [Unsigned_64 (Notification_Kind_Code'Enum_Rep (Notify_Kind (Item.Target_Kind))),
               Unsigned_64 (Notification_Class_Code'Enum_Rep (Notify_Class (Item.Class))), Item.Number];
         when Notification_Identity_Page =>
            Reply.Data (1 .. 3) :=
              [Unsigned_64 (Item.Node), Unsigned_64 (Item.Incarnation), Boolean'Pos (Item.Current_Target)];
         when Notification_Path_Page =>
            if Request.Data (2) >= Unsigned_64 (Item.Path_Length) then
               Reply.Status := Index_Out_Of_Range; return;
            end if;
            declare
               Segment : constant String := Item.Path (Natural (Request.Data (2)) + 1);
            begin
               for I in Segment'Range loop
                  Packed := Packed or Shift_Left
                    (Unsigned_64 (Character'Pos (Segment (I))), (I - Segment'First) * 8);
               end loop;
            end;
            Reply.Data (1 .. 3) := [Packed, Unsigned_64 (Item.Path_Length), 0];
      end case;
      Reply.Status := OK;
   end Observe_Notification;
   procedure Mutate_Notification (Server : in out State; Request : Packet; Reply : out Response) is
      Status : ACPI_Service.Values.Access_Status;
      Item : ACPI_Service.Notification_Observation;
      Present : Boolean;
      Acknowledged : Receipts.Ack_Result;
      use type Receipts.Ack_Result;
      use type ACPI_Service.Values.Access_Status;
      procedure Claim_Reply is
      begin
         Reply := (Status => OK, Data =>
           [Server.Version, Receipts.Current (Server.Receipt),
            Unsigned_64 (ACPI_Bootstrap.Pending_Notifications (Server.Boot)),
            (if Receipts.Current (Server.Receipt) = 0 then 0
             else Unsigned_64 (Server.Staged_Notification.Path_Length))]);
      end Claim_Reply;
   begin
      Reply := (Status => Malformed, Data => [0 => Server.Version, others => 0]);
      if Request.Data (2 .. 3) /= [0,0] then return; end if;
      if Request.Label = Configure_Notifications then
         if Request.Data (1) > 1 then return; end if;
      elsif Request.Label = Claim_Notification then
         if Request.Data (1) /= 0 then return; end if;
      elsif Request.Data (1) = 0 or else Request.Data (1) > Max_Revision then return;
      end if;
      -- An old ACK cannot clear a newer active receipt. Zero never authorizes.
      if Request.Label = Acknowledge_Notification and then
        Request.Data (1) = Receipts.Last_Acknowledged (Server.Receipt)
      then
         Reply := (Status => OK, Data => [Server.Version, Request.Data (1),
           Unsigned_64 (ACPI_Bootstrap.Pending_Notifications (Server.Boot)), 0]);
         return;
      end if;
      if Request.Data (0) /= Server.Version then Reply.Status := Stale; return; end if;
      if Request.Label = Claim_Notification then
         if Receipts.Current (Server.Receipt) /= 0 then Claim_Reply; return; end if;
         if not ACPI_Bootstrap.Members_Initialized (Server.Boot) then
            Reply.Status := Wrong_Order; return;
         end if;
         if ACPI_Bootstrap.Pending_Notifications (Server.Boot) = 0 then Claim_Reply; return; end if;
         if Server.Version = Max_Revision or else not Receipts.Can_Issue (Server.Receipt) then
            Reply.Status := Resource_Limit; return;
         end if;
         ACPI_Bootstrap.Take_Notification (Server.Boot, Item, Present, Status);
         if Status /= ACPI_Service.Values.Available then Reply.Status := Wrong_Order; return; end if;
         if not Present then Claim_Reply; return; end if;
         -- All admission checks preceded Take. Fixed record assignment and
         -- preflighted monotonic counters cannot fail after the FIFO transfer.
         Server.Staged_Notification := Item;
         Receipts.Issue (Server.Receipt);
         Server.Version := Server.Version + 1;
         Claim_Reply;
      elsif Request.Label = Acknowledge_Notification then
         if Request.Data (1) /= Receipts.Current (Server.Receipt) then Reply.Status := Stale; return; end if;
         if Server.Version = Max_Revision then Reply.Status := Resource_Limit; return; end if;
         Receipts.Acknowledge (Server.Receipt, Request.Data (1), Acknowledged);
         pragma Assert (Acknowledged = Receipts.Accepted);
         Server.Staged_Notification := (others => <>);
         Server.Version := Server.Version + 1;
         Reply := (Status => OK, Data => [Server.Version, Request.Data (1),
           Unsigned_64 (ACPI_Bootstrap.Pending_Notifications (Server.Boot)), 0]);
      else
         -- Reject failed pre-Ready sessions even if requested interest matches.
         if Current (Server) = Failed and then not ACPI_Bootstrap.Members_Initialized (Server.Boot) then
            Reply.Status := Wrong_Order; return;
         end if;
         if (Request.Data (1) = 1) /= ACPI_Bootstrap.Notifications_Requested (Server.Boot)
           or else ACPI_Bootstrap.Notification_Status (Server.Boot) /= ACPI_Service.Values.Available
         then
            if Server.Version = Max_Revision then Reply.Status := Resource_Limit; return; end if;
            ACPI_Bootstrap.Set_Notification_Interest (Server.Boot, Request.Data (1) = 1, Status);
            if Status /= ACPI_Service.Values.Available then Reply.Status := Wrong_Order; return; end if;
            Server.Version := Server.Version + 1;
         end if;
         Reply := (Status => OK, Data => [Server.Version,
           Boolean'Pos (ACPI_Bootstrap.Notifications_Requested (Server.Boot)),
           Boolean'Pos (ACPI_Bootstrap.Notifications_Enabled (Server.Boot)),
           Unsigned_64 (Access_Outcome_Code'Enum_Rep
             (Access_Code (ACPI_Bootstrap.Notification_Status (Server.Boot))))]);
      end if;
   end Mutate_Notification;
   -- The packet handler has already rejected repeated startup and bad counts.
   procedure Start_Validated (Session : in out ACPI_Bootstrap.State; Count : Positive)
     with Pre => ACPI_Bootstrap.Valid (Session)
       and then not ACPI_Bootstrap.Started (Session)
       and then Count <= Session.Table_Capacity,
       Post => ACPI_Bootstrap.Valid (Session)
         and then ACPI_Bootstrap.Started (Session)
         and then ACPI_Bootstrap.Current (Session) = ACPI_Bootstrap.Receiving
         and then ACPI_Bootstrap.Expected (Session) = Count
         and then ACPI_Bootstrap.Core_Model (Session) =
           ACPI_Bootstrap.Core_Model (Session)'Old
   is
      Accepted : Boolean;
   begin
      ACPI_Bootstrap.Start (Session, Count, Accepted);
      pragma Assert (Accepted);
   end Start_Validated;
   procedure Handle (Server : in out State; Origin : Authority;
                     Request : Packet; Reply : out Response) is
      Amount : Natural range 0 .. 16;
      Initial_Version : constant Revision_Number := Server.Version;
      Result : ACPI_Service.Install_Status;
      function Octet (Index : Natural) return Firmware_Tables.Byte is
        (Firmware_Tables.Byte (Shift_Right (Request.Data (2 + Index / 8), (Index mod 8) * 8) and 255))
        with Pre => Index < 16;
   begin
      Reply := (Status => Malformed, Data => [0 => Server.Version, others => 0]);
      if Origin = No_Authority then
         Reply := (Status => Denied, Data => [others => 0]);
         return;
      end if;
      if Request.Length /= 4 or Request.Flags /= 0 or Request.Reserved /= 0 then return; end if;
      if Request.Label in Read_Metrics | Read_Table_Info | Read_Table_Chunk | Read_MCFG_Info .. Read_DMAR_Path then
         Read_Request (Server, Request, Reply);
         return;
      end if;
      if Request.Label in Read_Notification | Read_Notification_State then
         Observe_Notification (Server, Request, Reply); return;
      end if;
      if Origin /= Snapshot_Provider then Reply.Status := Denied; return; end if;
      if Request.Label in Configure_Notifications | Claim_Notification | Acknowledge_Notification then
         Mutate_Notification (Server, Request, Reply); return;
      end if;
      if Request.Label not in Start_Snapshot .. Finish_Snapshot then return; end if;
      if Request.Data (0) /= Server.Version then Reply.Status := Stale; return; end if;
      if Initial_Version = Max_Revision then Reply.Status := Resource_Limit; return; end if;
      case Request.Label is
         when Start_Snapshot =>
            if Request.Data (1) not in 1 .. Unsigned_64 (Positive'Last)
              or else Request.Data (2 .. 3) /= [0, 0] then return; end if;
            declare
               Count : constant Positive := Positive (Request.Data (1));
            begin
               if Count > Server.Boot.Table_Capacity then return; end if;
               if ACPI_Bootstrap.Started (Server.Boot) then Reply.Status := Wrong_Order; return; end if;
               Start_Validated (Server.Boot, Count);
            end;
            pragma Assert (Consistent (Server));
         when Begin_Table =>
            if Request.Data (1) not in 1 .. Unsigned_64 (Positive'Last)
              or else Request.Data (2) > 2
              or else Request.Data (3) < Unsigned_64 (Firmware_Tables.Table_Header_Size)
              or else Request.Data (3) > Unsigned_64 (Natural'Last)
            then return; end if;
            -- Check representation before conversion, then compare the quota
            -- in the same integer domain as the retained buffer extent.
            if Natural (Request.Data (3)) > Server.Table_Byte_Limit then return; end if;
            if Current (Server) /= Receiving or else Server.Open or else
              ACPI_Bootstrap.Installed (Server.Boot) = ACPI_Bootstrap.Expected (Server.Boot)
            then Reply.Status := Wrong_Order; return; end if;
            Server.ID := Positive (Request.Data (1));
            Server.Kind := ACPI_Service.Table_Kind'Val (Request.Data (2));
            Server.Extent := Natural (Request.Data (3));
            pragma Assert (ACPI_Bootstrap.Current (Server.Boot) = ACPI_Bootstrap.Receiving);
            pragma Assert (Server.Extent >= Firmware_Tables.Table_Header_Size);
            Server.Open := True;
            pragma Assert (Consistent (Server));
         when Write_Chunk =>
            if not Server.Open or else Server.Used = Server.Extent then Reply.Status := Wrong_Order; return; end if;
            if Request.Data (1) /= Unsigned_64 (Server.Used) then Reply.Status := Wrong_Order; return; end if;
            Amount := Natural'Min (16, Server.Extent - Server.Used);
            -- Reject noncanonical padding before changing any buffered byte.
            for I in Amount .. 15 loop
               if Octet (I) /= 0 then return; end if;
            end loop;
            declare
               Chunk_Length : constant Natural := Amount;
               Chunk : Firmware_Tables.Bytes (1 .. Chunk_Length);
            begin
               for I in Chunk'Range loop Chunk (I) := Octet (I - 1); end loop;
               Server.Buffer_Data (Server.Used + 1 .. Server.Used + Amount) := Chunk;
            end;
            Server.Used := Server.Used + Amount;
            pragma Assert (ACPI_Bootstrap.Current (Server.Boot) = ACPI_Bootstrap.Receiving);
            pragma Assert (Server.Extent >= Firmware_Tables.Table_Header_Size);

            pragma Assert (Consistent (Server));
         when Commit_Table =>
            if Request.Data (1 .. 3) /= [0, 0, 0] then return; end if;
            if not Server.Open or else Server.Used /= Server.Extent then Reply.Status := Wrong_Order; return; end if;
            declare
               Count : constant Natural := Server.Extent;
            begin
               Server.Open := False;
               Server.Used := 0;
               Server.Extent := 0;
               ACPI_Bootstrap.Import_Table (Server.Boot, Server.ID, Server.Kind, Server.Buffer_Data (1 .. Count), Result);
            end;
            if Result /= ACPI_Service.Installed then
               Reply.Status := Table_Rejected;
               Reply.Data (1) := Unsigned_64 (ACPI_Service.Install_Status'Pos (Result));
            end if;
            pragma Assert (Consistent (Server));
         when Finish_Snapshot =>
            if Request.Data (1 .. 3) /= [0, 0, 0] then return; end if;
            if Current (Server) /= Receiving or else Server.Open then Reply.Status := Wrong_Order; return; end if;
            ACPI_Bootstrap.Finish (Server.Boot, ACPI_Bootstrap.Default_Startup_Budget);
            if Current (Server) /= Complete then Reply.Status := Incomplete; end if;
            pragma Assert (Consistent (Server));
         when others => return;
      end case;
      Server.Version := Initial_Version + 1;
      Reply.Data (0) := Server.Version;
      if Reply.Status = Malformed then Reply.Status := OK; end if;
   end Handle;
end ACPI_Requests;
