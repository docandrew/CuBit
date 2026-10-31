pragma Ada_2022;
with AML_Table_Readers;
with AML_Table_Sources;
package body ACPI_Service_Core with SPARK_Mode is
   use type Values.Append_Outcome;
   pragma Unevaluated_Use_Of_Old (Allow);
   use type Firmware_Tables.Admission;
   use type AML_Table_Backing.State;
   use type Namespace.Load_Status;

   function Method_Storage_Capacity return Positive is (Aggregate_Method_Capacity);
   function Valid (Service : State) return Boolean is
     (Values.Valid (Service.Tree));
   function Current_Import_Phase (Service : State) return Import_Phase is (Service.Import_State);
   function Namespace_Load_Phase (Service : State) return Values.Phase is (Values.Current (Service.Tree));
   function Namespace_Generation (Service : State) return AML_Identity.Identity is (Values.Generation (Service.Tree));
   function Audit (Service : State) return Values.Audit_Value is (Values.Audit (Service.Tree));
   function Observe (Service : State) return Metrics is
      Usage : constant Values.Usage_Description := Values.Observe_Usage (Service.Tree);
   begin
      return (Service.Stats with delta Objects => Usage.Nodes,
         Method_Bytes => Usage.Methods, Value_Objects => Usage.Values.Objects,
         Value_Bytes => Usage.Values.Bytes, Package_Elements => Usage.Values.Elements);
   end Observe;
   function Namespace_Node_Count (Service : State) return Namespace.Node_ID is
     (Values.Node_Count (Service.Tree));
   procedure Describe_Node (Service : State; Node : Namespace.Node_ID;
      Item : out Node_Descriptor; Status : out Values.Access_Status) is
   begin Values.Describe_Node (Service.Tree, Node, Item, Status); end Describe_Node;
   function Current_Binding (Service : State; Item : Node_Descriptor) return Boolean is
     (Values.Current_Binding (Service.Tree, Item));
   function Current_Node (Service : State; Item : Node_Descriptor) return Boolean is
     (Values.Current_Node (Service.Tree, Item));
   procedure Find_Child (Service : State; Parent : Namespace.Node_ID; Part : AML_Names.Segment;
      Node : out Namespace.Node_ID; Status : out Values.Access_Status) is
   begin Values.Find_Child (Service.Tree, Parent, Part, Node, Status); end Find_Child;
   procedure Find_Object (Service : State; Parent : Namespace.Node_ID; Part : AML_Names.Segment;
      Result : out Namespace.Lookup_Result; Status : out Values.Access_Status) is
   begin Values.Find_Object (Service.Tree, Parent, Part, Result, Status); end Find_Object;
   procedure Resolve_Node (Service : State; Scope : Node_Descriptor;
      Path : AML_Names.Name_Result; Item : out Node_Descriptor;
      Outcome : out Namespace.Lookup_Status; Status : out Values.Access_Status) is
   begin Values.Resolve_Node (Service.Tree, Scope, Path, Item, Outcome, Status); end Resolve_Node;
   procedure Store_Parameter_Data (Service : in out State; Target : Node_Descriptor;
      Item : Values.Argument; Budget : Natural; Charged : out Natural;
      Outcome : out AML_Execute.Execution_Status; Status : out Values.Access_Status) is
   begin
      Values.Store_Parameter_Data (Service.Tree, Target, Item, Service.Width,
        Budget, Charged, Outcome, Status);
   end Store_Parameter_Data;
   procedure Complete_Owned_Invocation (Service : in out State) is
   begin
      Service.Stats.Objects := Values.Node_Count (Service.Tree);
      for I in 1 .. Service.Load_Capacity loop
         if AML_Definition_Loads.Phase (Service.Loads, I) = AML_Definition_Loads.Failed then
            Service.Import_State := Catalog_Failed;
         end if;
      end loop;
   end Complete_Owned_Invocation;
   procedure Stage_Copied_Source (Service : in out State; Data : Firmware_Tables.Bytes;
      Fuel : Natural; Report : out AML_Copied_Tables.Report; Status : out Values.Access_Status) is
   begin
      Report := (others => <>); Status := Values.Wrong_Phase;
      if Service.Import_State /= Catalog_Staging or else AML_Definition_Loads.Active (Service.Loads)
        or else Values.Node_Count (Service.Tree) /= Namespace.Root then return; end if;
      AML_Copied_Tables.Retain (Service.Copies, Data, Fuel, Report); Status := Values.Available;
   end Stage_Copied_Source;
   procedure Evaluate_Named (Service : aliased in out State; Item : Node_Descriptor;
      Budget : Natural; Result : out Values.Result; Status : out Values.Access_Status) is
   begin
      Values.Evaluate_Registered (Service.Tree, Service.Loads, Service.Copies, Service.Backing, Item, Service.Width,
        Budget, Result, Status);
      Complete_Owned_Invocation (Service);
   end Evaluate_Named;
   procedure Read_Region_Descriptor
     (Service : State; Item : Node_Descriptor;
      Description : out Namespace.Operation_Region_Attributes;
      Status : out Values.Access_Status) is
   begin
      Values.Read_Region_Descriptor (Service.Tree, Item, Description, Status);
   end Read_Region_Descriptor;
   procedure Read_Field_Descriptor
     (Service : State; Node : Namespace.Node_ID;
      Description : out Values.Field_Description; Key : out Values.Field_Key;
      Status : out Values.Access_Status) is
   begin
      Values.Read_Field_Descriptor (Service.Tree, Node, Description, Key, Status);
   end Read_Field_Descriptor;
   function Field_Key_Matches (Service : State; Key : Values.Field_Key) return Boolean is
     (Values.Field_Key_Matches (Service.Tree, Key));
   procedure Observe_Named_Value (Service : in out State; Node : Namespace.Node_ID;
      Result : out Values.Result; Status : out Values.Access_Status) is
   begin Values.Observe_Named_Value (Service.Tree, Node, Result, Status); end Observe_Named_Value;
   function Table_Info (Service : State; Index : Positive) return Table_Metadata is
     (Service.Catalog (Index).Metadata);
   function MADT_Info (Service : State; Index : Positive)
     return Firmware_Tables.MADT.Table_Metadata is (Firmware_Tables.MADT.Decode (Service.Backing.Data.Bytes (Service.Catalog (Index).Offset + 1 ..
       Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent)));
   function MADT_Record (Service : State; Index : Positive; Record_Index : Natural)
     return Firmware_Tables.MADT.Record_Result is (Firmware_Tables.MADT.Read_Record (Service.Backing.Data.Bytes (Service.Catalog (Index).Offset + 1 ..
       Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent), Record_Index));
   function DMAR_Info (Service : State; Index : Positive) return Firmware_Tables.DMAR.Table_Metadata is
     (Firmware_Tables.DMAR.Decode (Service.Backing.Data.Bytes (Service.Catalog (Index).Offset + 1 .. Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent)));
   function DMAR_Record (Service : State; Index : Positive ; Record_Index : Natural) return Firmware_Tables.DMAR.Record_Result is
     (Firmware_Tables.DMAR.Read_Record (Service.Backing.Data.Bytes (Service.Catalog (Index).Offset + 1 .. Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent), Record_Index));
   function DMAR_Scope (Service : State; Index : Positive ; Record_Index, Scope_Index : Natural) return Firmware_Tables.DMAR.Scope_Result is
     (Firmware_Tables.DMAR.Read_Scope (Service.Backing.Data.Bytes (Service.Catalog (Index).Offset + 1 .. Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent), Record_Index, Scope_Index));
   function DMAR_Path (Service : State; Index : Positive ; Record_Index, Scope_Index, Path_Index : Natural) return Firmware_Tables.DMAR.Path_Result is
     (Firmware_Tables.DMAR.Read_Path (Service.Backing.Data.Bytes (Service.Catalog (Index).Offset + 1 .. Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent), Record_Index, Scope_Index, Path_Index));
   function SRAT_Info (Service : State; Index : Positive)
     return Firmware_Tables.SRAT.Table_Metadata is (Firmware_Tables.SRAT.Decode (Service.Backing.Data.Bytes (Service.Catalog (Index).Offset + 1 ..
       Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent)));
   function SRAT_Record (Service : State; Index : Positive; Record_Index : Natural)
     return Firmware_Tables.SRAT.Record_Result is (Firmware_Tables.SRAT.Read_Record (Service.Backing.Data.Bytes (Service.Catalog (Index).Offset + 1 ..
       Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent), Record_Index));
   function MCFG_Info (Service : State; Index : Positive) return Firmware_Tables.MCFG.Table_Metadata is
     (Firmware_Tables.MCFG.Decode (Service.Backing.Data.Bytes (Service.Catalog (Index).Offset + 1 ..
       Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent)));
   function MCFG_Allocation (Service : State; Index : Positive; Allocation_Index : Natural) return Firmware_Tables.MCFG.Allocation_Result is
     (Firmware_Tables.MCFG.Read_Allocation (Service.Backing.Data.Bytes (Service.Catalog (Index).Offset + 1 ..
       Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent), Allocation_Index));
   function SLIT_Info (Service : State; Index : Positive) return Firmware_Tables.SLIT.Table_Metadata is
     (Firmware_Tables.SLIT.Decode (Service.Backing.Data.Bytes (Service.Catalog (Index).Offset + 1 ..
       Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent)));
   function SLIT_Distance (Service : State; Index : Positive; From_Locality, To_Locality : Natural) return Firmware_Tables.SLIT.Distance_Result is
     (Firmware_Tables.SLIT.Read_Distance (Service.Backing.Data.Bytes (Service.Catalog (Index).Offset + 1 ..
       Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent), From_Locality, To_Locality));
   function Table_Byte (Service : State; Index : Positive; Offset : Natural)
      return Firmware_Tables.Byte is
     (Service.Backing.Data.Bytes (Service.Catalog (Index).Offset + Offset + 1));
   function Fixed_Description (Service : State; Index : Positive) return ACPI_FADT.Result is
     (ACPI_FADT.Decode
        (Service.Backing.Data.Bytes (Service.Catalog (Index).Offset + 1 ..
           Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent)));
   function Table_Field
     (Service : State; Index : Positive; Bit_Offset, Bit_Count : Natural)
      return AML_Field_Data.Read_Result is
     (AML_Field_Data.Read_Bits
        (AML_Decode.Bytes (Service.Backing.Data.Bytes (Service.Catalog (Index).Offset + 1 ..
           Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent)),
         Bit_Offset, Bit_Count));
   function Table_Field_Window
     (Service : State; Index : Positive; Bit_Offset, Bit_Count : Natural;
      First_Byte, Count : Natural) return AML_Table_Backing.Window_Result is
     (AML_Table_Backing.Read_Window
        (Service.Backing, Index, Service.Catalog (Index).Metadata.Extent,
         Bit_Offset, Bit_Count, First_Byte, Count));
   function Table_Identity (Service : State; Index : Positive)
      return Firmware_Tables.Identifiers.Identity is
     (Firmware_Tables.Identifiers.Read_Identity
       (Service.Backing.Data.Bytes (Service.Catalog (Index).Offset + 1 ..
          Service.Catalog (Index).Offset + Firmware_Tables.Table_Header_Size)));
   function Find_Table
     (Service : State; Requested : Firmware_Tables.Identifiers.Selection)
      return Natural is
   begin
      for I in 1 .. Service.Stats.Tables loop
         if Firmware_Tables.Identifiers.Matches (Table_Identity (Service, I), Requested) then
            return I;
         end if;
         pragma Loop_Invariant (for all J in 1 .. I =>
           not Firmware_Tables.Identifiers.Matches (Table_Identity (Service, J), Requested));
      end loop;
      return 0;
   end Find_Table;
   function Catalog_Snapshot (Service : State) return Catalog_State is
     (Table_Capacity => Service.Table_Capacity, Byte_Capacity => Service.Byte_Capacity,
      Catalog => Service.Catalog, Backing => Service.Backing,
      Tables => Service.Stats.Tables, Bytes => Service.Stats.Bytes);
   function Load_Model (Service : State) return Table_Load_Model with Ghost is
      Result : Table_Load_Model (1 .. Service.Load_Capacity);
   begin
      for I in Result'Range loop
         Result (I) := ((case AML_Definition_Loads.Phase (Service.Loads, I) is
           when AML_Definition_Loads.Retained => Retained,
           when AML_Definition_Loads.Loading => Loading_Definition,
           when AML_Definition_Loads.Loaded => Loaded_Definition,
           when AML_Definition_Loads.Failed => Failed_Definition),
           Namespace.Has_Definition (AML_Definition_Loads.Definition (Service.Loads, I)),
           AML_Definition_Loads.Definition (Service.Loads, I));
      end loop;
      return Result;
   end Load_Model;
   function Module_Errors (Service : State) return Module_Error_Observation is
     (Values.Module_Errors (Service.Tree));
   function Model (Service : State) return State_Model is
     (Table_Capacity => Service.Table_Capacity, Byte_Capacity => Service.Byte_Capacity,
      Load_Capacity => Service.Load_Capacity,
      Catalog => Catalog_Snapshot (Service), Loads => Load_Model (Service),
      Copies => AML_Copied_Tables.Model (Service.Copies), Tree => Audit (Service),
      Stats => Service.Stats, Width => Service.Width, Table_Limit => Service.Table_Byte_Limit,
      Import_State => Service.Import_State,
      Generation => Values.Generation (Service.Tree),
      Notifications => Values.Notification_Model (Service.Tree), Errors => Module_Errors (Service));
   function Same_Catalog (Service, Prior : State) return Boolean is
     (Service.Catalog = Prior.Catalog and then Service.Backing = Prior.Backing
      and then Service.Stats.Tables = Prior.Stats.Tables and then Service.Stats.Bytes = Prior.Stats.Bytes);


   function Pending_Members (Service : State) return Natural is
     (Values.Observe_Usage (Service.Tree).Pending);
   function Initialization_Frame (Current, Prior : State_Model) return Boolean is
     (Current = (Prior with delta Tree => Current.Tree)
      and then Values.Table_Initialization_Frame (Current.Tree, Prior.Tree, Prior.Catalog.Backing));
   procedure Initialize_Members
     (Service : in out State; Report : out Namespace.Initialization_Report;
      Status : out Values.Access_Status) is
   begin
      if Service.Import_State not in Legacy_Import | Catalog_Executed then
         Report := (others => 0); Status := Values.Wrong_Phase; return;
      end if;
      Values.Seal_With_Tables (Service.Tree, Service.Backing, Report, Status);
   end Initialize_Members;
   function Retained_Results (Service : State) return Namespace.Owned.Retained_Root_Count is
     (Values.Retained_Count (Service.Tree));
   procedure Configure_Notifications
     (Service : in out State; Enabled : Boolean; Status : out Values.Access_Status) is
   begin
      Values.Configure_Notifications (Service.Tree, Enabled, Status);
   end Configure_Notifications;
   function Pending_Notifications (Service : State) return Notification_Count is
     (Values.Pending_Notifications (Service.Tree));
   procedure Take_Notification
     (Service : in out State; Item : out Notification_Observation;
      Present : out Boolean; Status : out Values.Access_Status) is
   begin
      Values.Take_Notification (Service.Tree, Item, Present, Status);
   end Take_Notification;
   procedure Create_Buffer_Result
     (Service : in out State; Data : AML_Decode.Bytes;
      Result : out Values.Result; Status : out Values.Access_Status) is
   begin
      Values.Create_Buffer (Service.Tree, Data, Result, Status);
   end Create_Buffer_Result;
   procedure Create_Empty_Package_Result
     (Service : in out State; Result : out Values.Result; Status : out Values.Access_Status) is
   begin
      Values.Create_Empty_Package (Service.Tree, Result, Status);
   end Create_Empty_Package_Result;
   procedure Invoke_With_Arguments
     (Service : aliased in out State; Node : Namespace.Node_ID; Args : Values.Arguments;
      Argument_Count : Values.Argument_Count; Budget : Natural; Result : out Values.Result;
      Status : out Values.Access_Status) is
   begin
      Values.Invoke_Registered (Service.Tree, Service.Loads, Service.Copies, Service.Backing, Node, Args, Argument_Count, Budget, Result, Status);
      Complete_Owned_Invocation (Service);
   end Invoke_With_Arguments;
   procedure Invoke_Retained
     (Service : aliased in out State; Node : Namespace.Node_ID; Args : AML_Execute.Arguments;
      Argument_Count : Natural; Budget : Natural; Result : out Values.Result;
      Status : out Values.Access_Status) is
      Arguments : Values.Arguments := [others => (Values.Immediate_Argument, 0)];
   begin
      for I in 1 .. Argument_Count loop Arguments (I - 1).Number := Args (I - 1); end loop;
      Invoke_With_Arguments (Service, Node, Arguments, Argument_Count, Budget, Result, Status);
   end Invoke_Retained;
   generic
      with procedure Run_Modules
        (Service : in out State; Budget : Natural;
         Report : out Values.Module_Report; Status : out Values.Access_Status);
   procedure Execute_Frozen_Using
     (Service : in out State; Budget : Natural; Report : out Execution_Report;
      Status : out Values.Access_Status);
   package body Field_IO is
      procedure Execute_Frozen
        (Service : in out State; IO : in out IO_Context;
         Targets : Module_Target_Set; Count : Module_Target_Count;
         Budget : Natural; Report : out Execution_Report; Status : out Values.Access_Status) is
         procedure Run_Modules
           (Service : in out State; Budget : Natural;
            Report : out Values.Module_Report; Status : out Values.Access_Status) is
         begin
            Adapter.Execute_Registered_Modules (Service.Tree, IO,
              Adapter.Module_Target_Set (Targets), Adapter.Module_Target_Count (Count),
              Service.Loads, Service.Copies, Service.Backing, Budget, Report, Status);
         end Run_Modules;
         procedure Execute is new Execute_Frozen_Using (Run_Modules);
      begin
         Execute (Service, Budget, Report, Status);
      end Execute_Frozen;
      procedure Bind
        (Service : State; IO : IO_Context; Key : Values.Field_Key;
         Target : AML_Field_Access.Target; Item : out Binding;
         Status : out Values.Access_Status) is
      begin
         Adapter.Bind (Service.Tree, IO, Key, Target, Item.Inner, Status);
      end Bind;
      procedure Clear (Items : out Binding_Set) is
      begin
         Adapter.Clear (Items.Inner);
      end Clear;
      procedure Add_Region_Register
        (Service : State; IO : IO_Context; Region : Node_Descriptor;
         Target : AML_Field_Access.Target; Allow_Read, Allow_Write : Boolean;
         Items : in out Binding_Set; Status : out Values.Access_Status) is
      begin
         Adapter.Add_Region_Register (Service.Tree, IO, Region, Target,
           Allow_Read, Allow_Write, Items.Inner, Status);
      end Add_Region_Register;
      procedure Add_Binding
        (Service : State; IO : IO_Context; Key : Values.Field_Key;
         Target : AML_Field_Access.Target; Items : in out Binding_Set;
         Status : out Values.Access_Status) is
      begin
         Adapter.Add_Binding (Service.Tree, IO, Key, Target, Items.Inner, Status);
      end Add_Binding;
      procedure Add_Read_Binding
        (Service : State; IO : IO_Context; Key : Values.Field_Key;
         Targets : AML_Field_Targets.Vector; Count : AML_Field_Plans.Datum_Count;
         Items : in out Binding_Set; Status : out Values.Access_Status) is
      begin
         Adapter.Add_Read_Binding (Service.Tree, IO, Key, Targets, Count, Items.Inner, Status);
      end Add_Read_Binding;
      procedure Add_Write_Binding
        (Service : State; IO : IO_Context; Key : Values.Field_Key;
         Targets : AML_Field_Targets.Vector; Count : AML_Field_Plans.Datum_Count;
         Items : in out Binding_Set; Status : out Values.Access_Status) is
      begin
         Adapter.Add_Write_Binding (Service.Tree, IO, Key, Targets, Count, Items.Inner, Status);
      end Add_Write_Binding;
      function Uncertain (Service : State) return AML_Field_Access.Observation is
        (Adapter.Uncertain (Service.Tree));
      function Original_Field (Service : State) return AML_References.Reference is
        (Adapter.Original_Field (Service.Tree));
      function Original_Region (Service : State) return AML_References.Reference is
        (Adapter.Original_Region (Service.Tree));
      procedure Reconcile
        (Service : in out State; IO : IO_Context; Expected : AML_Field_Access.Observation;
         Status : out Values.Access_Status) is
      begin
         Adapter.Reconcile (Service.Tree, IO, Expected, Status);
      end Reconcile;
      procedure Invoke_With_Arguments
        (Service : aliased in out State; IO : in out IO_Context; Item : Binding;
         Node : Namespace.Node_ID; Args : Values.Arguments;
         Argument_Count : Values.Argument_Count; Budget : Natural; Result : out Values.Result;
         Status : out Values.Access_Status) is
      begin
         Adapter.Invoke_Registered (Service.Tree, Service.Loads, Service.Copies, IO, Item.Inner, Service.Backing, Node,
           Args, Argument_Count, Budget, Result, Status);
         Complete_Owned_Invocation (Service);
      end Invoke_With_Arguments;
      procedure Invoke_With_Arguments
        (Service : aliased in out State; IO : in out IO_Context; Items : Binding_Set;
         Node : Namespace.Node_ID; Args : Values.Arguments;
         Argument_Count : Values.Argument_Count; Budget : Natural; Result : out Values.Result;
         Status : out Values.Access_Status) is
      begin
         Adapter.Invoke_Registered (Service.Tree, Service.Loads, Service.Copies, IO, Items.Inner, Service.Backing, Node,
           Args, Argument_Count, Budget, Result, Status);
         Complete_Owned_Invocation (Service);
      end Invoke_With_Arguments;
      procedure Invoke_Retained
        (Service : aliased in out State; IO : in out IO_Context; Item : Binding;
         Node : Namespace.Node_ID; Args : AML_Execute.Arguments;
         Argument_Count : Natural; Budget : Natural; Result : out Values.Result;
         Status : out Values.Access_Status) is
         Arguments : Values.Arguments := [others => (Values.Immediate_Argument, 0)];
      begin
         for I in 1 .. Argument_Count loop Arguments (I - 1).Number := Args (I - 1); end loop;
         Invoke_With_Arguments (Service, IO, Item, Node, Arguments,
           Argument_Count, Budget, Result, Status);
      end Invoke_Retained;
      procedure Invoke_Retained
        (Service : aliased in out State; IO : in out IO_Context; Items : Binding_Set;
         Node : Namespace.Node_ID; Args : AML_Execute.Arguments;
         Argument_Count : Natural; Budget : Natural; Result : out Values.Result;
         Status : out Values.Access_Status) is
         Arguments : Values.Arguments := [others => (Values.Immediate_Argument, 0)];
      begin
         for I in 1 .. Argument_Count loop Arguments (I - 1).Number := Args (I - 1); end loop;
         Invoke_With_Arguments (Service, IO, Items, Node, Arguments,
           Argument_Count, Budget, Result, Status);
      end Invoke_Retained;
      procedure Evaluate_Named
        (Service : aliased in out State; IO : in out IO_Context;
         Items : Binding_Set; Item : Node_Descriptor; Budget : Natural;
         Result : out Values.Result; Status : out Values.Access_Status) is
      begin
         Adapter.Evaluate_Registered (Service.Tree, Service.Loads, Service.Copies, IO, Items.Inner, Service.Backing,
           Item, Service.Width, Budget, Result, Status);
         Complete_Owned_Invocation (Service);
      end Evaluate_Named;
      procedure Finish_Scalar
        (Service : in out State; Outcome : Values.Result;
         Access_Result : Values.Access_Status; Result : out AML_Execute.Execution_Result) is
         Status : Values.Access_Status := Access_Result;
         Handle : Values.Value_Handle;
         subtype Failure_Status is AML_Execute.Payload_Free_Failure;
      begin
         -- An effectful failure must survive even an inconsistent access code.
         if Outcome.Status = AML_Execute.Field_Indeterminate then
            Result := (AML_Execute.Field_Indeterminate, Outcome.Charged);
         elsif Status /= Values.Available then
            Result := (AML_Execute.Unsupported_Value, Outcome.Charged);
         elsif Outcome.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned then
            Handle := Outcome.Handle; Release_Result (Service, Handle, Status);
            if Status /= Values.Available then raise Program_Error with "field scalar release failed"; end if;
            Result := (AML_Execute.Unsupported_Value, Outcome.Charged);
         elsif Outcome.Status = AML_Execute.Returned then
            Result := (AML_Execute.Returned, Outcome.Charged, Outcome.Number, Outcome.Origin);
         elsif Outcome.Status = AML_Execute.Fatal_Requested then
            Result := (Status => AML_Execute.Fatal_Requested, Charged => Outcome.Charged, Fatal => Outcome.Fatal);
         else Result := (Failure_Status (Outcome.Status), Outcome.Charged); end if;
      end Finish_Scalar;
      procedure Invoke_Scalar
        (Service : aliased in out State; IO : in out IO_Context; Item : Binding;
         Node : Namespace.Node_ID; Args : AML_Execute.Arguments;
         Argument_Count : Natural; Budget : Natural; Result : out AML_Execute.Execution_Result) is
         Outcome : Values.Result;
         Status : Values.Access_Status;
      begin
         Invoke_Retained (Service, IO, Item, Node, Args, Argument_Count, Budget, Outcome, Status);
         Finish_Scalar (Service, Outcome, Status, Result);
      end Invoke_Scalar;
      procedure Invoke_Scalar
        (Service : aliased in out State; IO : in out IO_Context; Items : Binding_Set;
         Node : Namespace.Node_ID; Args : AML_Execute.Arguments;
         Argument_Count : Natural; Budget : Natural; Result : out AML_Execute.Execution_Result) is
         Outcome : Values.Result;
         Status : Values.Access_Status;
      begin
         Invoke_Retained (Service, IO, Items, Node, Args, Argument_Count, Budget, Outcome, Status);
         Finish_Scalar (Service, Outcome, Status, Result);
      end Invoke_Scalar;

   end Field_IO;
   procedure Describe_Result (Service : State; Handle : Values.Value_Handle;
      Description : out Values.Value_Description; Status : out Values.Access_Status) is
   begin Values.Describe (Service.Tree, Handle, Description, Status); end Describe_Result;
   procedure Read_Result_Device (Service : State; Handle : Values.Value_Handle;
      Item : out Node_Descriptor; Status : out Values.Access_Status) is
   begin Values.Read_Device_Metadata (Service.Tree, Handle, Item, Status); end Read_Result_Device;
   procedure Read_Result_Bytes (Service : State; Handle : Values.Value_Handle; Offset : Natural;
      Data : out AML_Decode.Bytes; Copied : out Natural; Status : out Values.Access_Status) is
   begin Values.Read_Bytes (Service.Tree, Handle, Offset, Data, Copied, Status); end Read_Result_Bytes;
   procedure Read_Result_Element (Service : in out State; Handle : Values.Value_Handle;
      Index : Natural; Element : out Values.Value_Handle; Status : out Values.Access_Status) is
   begin Values.Read_Element (Service.Tree, Handle, Index, Element, Status); end Read_Result_Element;
   procedure Dereference_Result (Service : in out State; Handle : Values.Value_Handle;
      Value : out Values.Value_Handle; Status : out Values.Access_Status) is
   begin Values.Dereference (Service.Tree, Handle, Value, Status); end Dereference_Result;
   procedure Release_Result (Service : in out State; Handle : in out Values.Value_Handle;
      Status : out Values.Access_Status) is
   begin Values.Release (Service.Tree, Handle, Status); end Release_Result;
   procedure Invoke_Scalar
     (Service : aliased in out State; Node : Namespace.Node_ID; Args : AML_Execute.Arguments;
      Argument_Count : Natural; Budget : Natural; Result : out AML_Execute.Execution_Result) is
      Outcome : Values.Result;
      Status : Values.Access_Status;
      Handle : Values.Value_Handle;
      subtype Failure_Status is AML_Execute.Payload_Free_Failure;
   begin
      Invoke_Retained (Service, Node, Args, Argument_Count, Budget, Outcome, Status);
      if Status /= Values.Available then
         Result := (AML_Execute.Unsupported_Value, Outcome.Charged);
      elsif Outcome.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned then
         Handle := Outcome.Handle; Release_Result (Service, Handle, Status);
         if Status /= Values.Available then raise Program_Error with "scalar release failed"; end if;
         Result := (AML_Execute.Unsupported_Value, Outcome.Charged);
      elsif Outcome.Status = AML_Execute.Returned then
         Result := (AML_Execute.Returned, Outcome.Charged, Outcome.Number, Outcome.Origin);
      elsif Outcome.Status = AML_Execute.Fatal_Requested then
         Result := (Status => AML_Execute.Fatal_Requested, Charged => Outcome.Charged, Fatal => Outcome.Fatal);
      else Result := (Failure_Status (Outcome.Status), Outcome.Charged); end if;
   end Invoke_Scalar;

   procedure Declare_Table_Region
     (Service : in out State; Scope : Namespace.Node_ID; Part : AML_Names.Segment;
      Requested : Firmware_Tables.Identifiers.Selection;
      Node : out Namespace.Node_ID; Result : out Namespace.Bind_Status;
      Owner : Namespace.Node_ID := Namespace.Root)
   is
      Index : constant Natural := Find_Table (Service, Requested);
   begin
      Node := Namespace.Root;
      Result := Namespace.Binding_Invalid;
      if Service.Import_State /= Legacy_Import or else Index = 0 then return; end if;
      declare
         Status : Values.Access_Status;
      begin
         Values.Bind_Table_Region (Service.Tree, Scope, Part,
            (Item => (AML_Table_Sources.Boot_Source, AML_Table_Sources.Boot_Table_ID (Index)),
             Extent => Table_Info (Service, Index).Extent), Node, Result, Status, Owner);
      end;
      Service.Stats.Objects := Values.Node_Count (Service.Tree);
   end Declare_Table_Region;
   procedure Declare_Table_Field
     (Service : in out State; Scope : Namespace.Node_ID; Part : AML_Names.Segment;
      Region_Node : Namespace.Node_ID; Bit_Offset, Bit_Count : Natural;
      Node : out Namespace.Node_ID; Result : out Namespace.Bind_Status;
      Owner : Namespace.Node_ID := Namespace.Root)
   is
   begin
      Node := Namespace.Root;
      Result := Namespace.Binding_Invalid;
      if Service.Import_State /= Legacy_Import or else Service.Stats.Tables = 0 then return; end if;
      declare
         Status : Values.Access_Status;
      begin
         Values.Bind_Table_Field (Service.Tree, Scope, Part, Region_Node,
            Bit_Offset, Bit_Count, Node, Result, Status, Owner);
      end;
      Service.Stats.Objects := Values.Node_Count (Service.Tree);
   end Declare_Table_Field;
   function Read_Namespace_Field (Service : State; Node : Namespace.Node_ID)
      return AML_Field_Data.Read_Result is
      Field : Namespace.Table_Field;
      Status : Values.Access_Status;
      Read_Status : AML_Table_Readers.Read_Status;
      use type AML_Table_Readers.Read_Status;
      View : AML_Table_Readers.Field_View;
      Content : AML_Decode.Buffer_Storage := [others => 0];
      Length : Natural;
   begin
      Values.Read_Table_Field (Service.Tree, Node, Field, Status);
      if Status /= Values.Available then return (Status => AML_Decode.Malformed); end if;
      View := (Field.Region.Item, Field.Region.Extent, Field.Offset, Field.Bits);
      if not AML_Table_Readers.Valid_Field (Service.Backing, Service.Copies, View) then
         return (Status => AML_Decode.Malformed);
      elsif Field.Bits > AML_Field_Data.Max_Bits then
         return (Status => AML_Decode.Limit_Exceeded);
      end if;
      Length := AML_Table_Readers.Field_Length (View);
      for I in 1 .. Length loop
         AML_Table_Readers.Read_Field_Byte
           (Service.Backing, Service.Copies, View, I - 1, Content (I), Read_Status);
         if Read_Status /= AML_Table_Readers.Readable then return (Status => AML_Decode.Malformed); end if;
      end loop;
      return (Status => AML_Decode.Accepted, Length => Length, Content => Content);
   end Read_Namespace_Field;
   procedure Count_Rejection (Service : in out State) is
   begin
      if Service.Stats.Rejections = Natural'Last then
         Service.Stats.Counter_Saturated := True;
      else Service.Stats.Rejections := Service.Stats.Rejections + 1; end if;
   end Count_Rejection;

   -- One shared validation path. No namespace or catalog changes occur here.
   procedure Check_Table_Admission
     (Service : State; ID : Positive; Kind : Table_Kind; Data : Firmware_Tables.Bytes;
      Header : out Firmware_Tables.Table_Result; Signature : out Firmware_Tables.Signature;
      Result : out Install_Status)
     with Pre => Valid (Service) and then not Header'Constrained,
       Post => (if Result = Installed then Header.Status = Firmware_Tables.Accepted
         and then Header.Extent = Data'Length
         and then Service.Stats.Tables < Service.Table_Capacity
         and then Data'Length <= Service.Table_Byte_Limit
         and then Data'Length <= Service.Byte_Capacity - Service.Stats.Bytes)
   is
      procedure Reject_Admission (Why : Install_Status) is
      begin Result := Why; end Reject_Admission;
   begin
      Header := (Status => Firmware_Tables.Truncated);
      Signature := "____";
      Result := Invalid_Table;
      if (Kind = DSDT) /= (Service.Stats.Tables = 0) then
         Reject_Admission (Wrong_Order); return;
      end if;
      for I in 1 .. Service.Stats.Tables loop
         pragma Loop_Invariant (Service.Stats.Tables = Service.Stats.Tables'Loop_Entry);
         if Service.Catalog (I).Metadata.ID = ID then Reject_Admission (Duplicate_ID); return; end if;
      end loop;
      if Service.Stats.Tables = Service.Table_Capacity then Reject_Admission (Table_Limit); return; end if;
      if Data'Length > Service.Table_Byte_Limit or else
        Data'Length > Service.Byte_Capacity - Service.Stats.Bytes
      then
         Reject_Admission (Byte_Limit); return;
      end if;
      if Data'Length < Firmware_Tables.Table_Header_Size then Reject_Admission (Invalid_Table); return; end if;
      for I in Signature'Range loop
         Signature (I) := Character'Val (Data (Data'First + (I - 1)));
      end loop;
      if (Kind = DSDT and then Signature /= "DSDT") or else
         (Kind = SSDT and then Signature /= "SSDT") or else
         (Kind = Description and then Signature in "DSDT" | "SSDT" | "FACS")
      then Reject_Admission (Invalid_Table); return; end if;
      Header := Firmware_Tables.Read_Table (Data, Signature);
      if Header.Status /= Firmware_Tables.Accepted then
         Reject_Admission (Invalid_Table); return;
      end if;
      if Header.Extent /= Data'Length then Reject_Admission (Invalid_Table); return; end if;
      Result := Installed;
   end Check_Table_Admission;

   procedure Commit_Table
     (Service : in out State; ID : Positive; Data : Firmware_Tables.Bytes;
      Header : Firmware_Tables.Table_Result; Signature : Firmware_Tables.Signature;
      New_Phase : Import_Phase)
     with Pre => Valid (Service) and then Header.Status = Firmware_Tables.Accepted
       and then Header.Extent = Data'Length
       and then Service.Stats.Tables < Service.Table_Capacity
       and then Data'Length <= Service.Table_Byte_Limit
       and then Data'Length <= Service.Byte_Capacity - Service.Stats.Bytes
       and then New_Phase in Legacy_Import | Catalog_Staging
   is
      Old_Bytes : constant Natural := Service.Stats.Bytes;
   begin
      Service.Backing.Data.Bytes (Old_Bytes + 1 .. Old_Bytes + Data'Length) := Data;
      Service.Backing.Tables (Service.Stats.Tables + 1) :=
        (Offset => Old_Bytes, Extent => Data'Length);
      Service.Backing.Count := Service.Stats.Tables + 1;
      Service.Catalog (Service.Stats.Tables + 1) :=
        (Metadata => (ID => ID, Signature => Signature, Extent => Data'Length,
                      Revision => Header.Revision), Offset => Old_Bytes);
      Service.Stats.Tables := Service.Stats.Tables + 1;
      Service.Stats.Bytes := Service.Stats.Bytes + Header.Extent;
      Service.Import_State := New_Phase;
   end Commit_Table;

   procedure Install
     (Service : in out State; ID : Positive; Kind : Table_Kind;
      Data : Firmware_Tables.Bytes; Result : out Install_Status)
   is
      Header : Firmware_Tables.Table_Result;
      Width : AML_Decode.Integer_Width := Service.Width;
      Loaded : Namespace.Load_Status;
      Access_Result : Values.Access_Status;
      Signature : Firmware_Tables.Signature;
      procedure Reject (Why : Install_Status) is
      begin Count_Rejection (Service); Result := Why; end Reject;
   begin
      if Service.Import_State not in Undecided | Legacy_Import
        or else Values.Current (Service.Tree) = Values.Ready
      then Reject (Wrong_Order); return; end if;
      Check_Table_Admission (Service, ID, Kind, Data, Header, Signature, Result);
      if Result /= Installed then Count_Rejection (Service); return; end if;
      if Kind = DSDT then
         Width := (if Header.Revision < 2 then AML_Decode.Bits_32 else AML_Decode.Bits_64);
      end if;
      if Service.Stats.Tables = 0 then
         Values.Reset (Service.Tree, Access_Result);
         if Access_Result /= Values.Available then Reject (Invalid_AML); return; end if;
      end if;
      if Kind /= Description then
      if Header.Extent = Firmware_Tables.Table_Header_Size then
         Values.Load (Service.Tree, [1 .. 0 => 0], Width, Loaded, Access_Result);
      else
         Values.Load
           (Service.Tree, AML_Decode.Bytes
              (Data (Data'First + Firmware_Tables.Table_Header_Size .. Data'Last)),
            Width, Loaded, Access_Result);
      end if;
      Service.Stats.Last_Load_Code := Namespace.Load_Status'Pos (Loaded) + 1;
      if Access_Result /= Values.Available or else Loaded /= Namespace.Loaded then Reject (Invalid_AML); return; end if;
      end if;
      Commit_Table (Service, ID, Data, Header, Signature, Legacy_Import);
      Service.Stats.Objects := Values.Node_Count (Service.Tree);
      Service.Width := Width;
      Result := Installed;
   end Install;

   procedure Stage_Table
     (Service : in out State; ID : Positive; Kind : Table_Kind;
      Data : Firmware_Tables.Bytes; Result : out Stage_Status)
   is
      Header : Firmware_Tables.Table_Result;
      Signature : Firmware_Tables.Signature;
      Admission : Install_Status;
   begin
      if Service.Import_State not in Undecided | Catalog_Staging
        or else Values.Current (Service.Tree) not in Values.Uninitialized | Values.Loading
        or else Values.Node_Count (Service.Tree) /= Namespace.Root
        or else Values.Observe_Usage (Service.Tree).Pending /= 0
        or else Values.Observe_Usage (Service.Tree).Methods /= 0
        or else Values.Observe_Usage (Service.Tree).Values.Objects /= 0
        or else Values.Observe_Usage (Service.Tree).Values.Bytes /= 0
        or else Values.Observe_Usage (Service.Tree).Values.Elements /= 0
        or else Values.Retained_Count (Service.Tree) /= 0
      then Count_Rejection (Service); Result := Stage_Wrong_Order; return; end if;
      Check_Table_Admission (Service, ID, Kind, Data, Header, Signature, Admission);
      case Admission is
         when Installed => Result := Staged;
         when Wrong_Order => Result := Stage_Wrong_Order;
         when Duplicate_ID => Result := Stage_Duplicate_ID;
         when Table_Limit => Result := Stage_Table_Limit;
         when Byte_Limit => Result := Stage_Byte_Limit;
         when Invalid_Table | Invalid_AML => Result := Stage_Invalid_Table;
      end case;
      if Admission /= Installed then Count_Rejection (Service); return; end if;
      Commit_Table (Service, ID, Data, Header, Signature, Catalog_Staging);
      if Kind = DSDT then
         Service.Width := (if Header.Revision < 2 then AML_Decode.Bits_32 else AML_Decode.Bits_64);
      end if;
   end Stage_Table;

   procedure Describe_Definition (Service : State; Item : Node_Descriptor;
      Definition : out Namespace.Definition_Identity; Status : out Values.Access_Status) is
   begin Values.Describe_Definition (Service.Tree, Item, Definition, Status); end Describe_Definition;
   function Retained_Definition (Service : State; Index : Positive)
      return Namespace.Definition_Identity is
   begin
      if Index > Service.Stats.Tables then return Namespace.No_Definition; end if;
      return AML_Definition_Loads.Definition (Service.Loads, Index);
   end Retained_Definition;
   function Retained_Load_State (Service : State; Index : Positive)
      return Retained_Table_Phase is
   begin
      if Index > Service.Stats.Tables then return Not_Retained; end if;
      if Service.Catalog (Index).Metadata.Signature in "DSDT" | "SSDT" then return Boot_Managed; end if;
      return (case AML_Definition_Loads.Phase (Service.Loads, Index) is
         when AML_Definition_Loads.Retained => Retained,
         when AML_Definition_Loads.Loading => Loading_Definition,
         when AML_Definition_Loads.Loaded => Loaded_Definition,
         when AML_Definition_Loads.Failed => Failed_Definition);
   end Retained_Load_State;
   procedure Append_Retained_Module (Service : in out State; Index : Positive;
      Budget : Natural; Report : out Retained_Load_Report;
      Status : out Values.Access_Status;
      Root_At : Values.Module_Root := Values.Namespace_Module_Root) is
      Definition : Namespace.Definition_Identity;
   begin
      Report := (others => <>); Status := Values.Wrong_Phase;
      if Service.Import_State /= Catalog_Executed then return; end if;
      if Index > Service.Stats.Tables then Status := Values.Invalid_Value; return; end if;
      Values.Append_Registered_Module (Service.Tree, Service.Loads, Service.Copies, Service.Backing, Index, Service.Width,
        Budget, Report.Existing_Index, Report.Append, Status, Root_At);
      Definition := Retained_Definition (Service, Index);
      if Report.Existing_Index /= 0 then
         Report.Outcome := Retained_Load_Already_Present;
         Report.Definition := AML_Definition_Loads.Definition (Service.Loads, Report.Existing_Index);
         -- An idempotent service result did not invoke the append stage.
         Report.Append := (others => <>);
      elsif Namespace.Has_Definition (Definition) then
         Report.Definition := Definition;
         if Report.Append.Outcome = Values.Append_Completed then Report.Outcome := Retained_Load_Completed;
         elsif Retained_Load_State (Service, Index) = Failed_Definition then
            Service.Import_State := Catalog_Failed; Report.Outcome := Retained_Load_Failed;
         end if;
         Service.Stats.Objects := Values.Node_Count (Service.Tree);
      end if;
   end Append_Retained_Module;
   procedure Require_Load_Recovery (Service : in out State; Index : Positive;
      Status : out Values.Access_Status) is
   begin
      Status := Values.Wrong_Phase;
      if Service.Import_State /= Catalog_Executed then return; end if;
      if Index > Service.Stats.Tables then Status := Values.Invalid_Value; return; end if;
      Values.Require_Registered_Recovery (Service.Tree, Service.Loads, Service.Copies, Index, Status);
      if Status = Values.Available then Service.Import_State := Catalog_Failed; end if;
   end Require_Load_Recovery;
   procedure Execute_Frozen_Using
     (Service : in out State; Budget : Natural; Report : out Execution_Report;
      Status : out Values.Access_Status) is
   begin
      Report := (others => <>);
      Status := Values.Wrong_Phase;
      if Service.Import_State /= Catalog_Frozen then return; end if;
      -- Consume the service admission before any operation that can fail.
      Service.Import_State := Catalog_Failed;
      if Values.Current (Service.Tree) = Values.Uninitialized then
         Values.Reset (Service.Tree, Status);
         if Status /= Values.Available then return; end if;
      end if;
      Values.Prepare_Firmware_Namespace (Service.Tree, Report.Preparation, Status);
      if Status /= Values.Available or else Report.Preparation /= Values.Firmware_Prepared then
         Report.Phase := Preparation_Failed;
         return;
      end if;
      Run_Modules (Service, Budget, Report.Modules, Status);
      Report.Phase := Module_Execution_Reported;
      if Report.Modules.Failed_Table in 1 .. Service.Stats.Tables then
         Report.Failed_Table_ID := Service.Catalog (Report.Modules.Failed_Table).Metadata.ID;
      end if;
      if Report.Phase = Module_Execution_Reported
        and then Report.Preparation = Values.Firmware_Prepared
        and then Status = Values.Available and then Report.Modules.Outcome = Values.Modules_Completed then
         Service.Import_State := Catalog_Executed;
      end if;
   end Execute_Frozen_Using;
   procedure Default_Modules
     (Service : in out State; Budget : Natural;
      Report : out Values.Module_Report; Status : out Values.Access_Status) is
   begin
      Values.Execute_Registered_Modules (Service.Tree, Service.Loads, Service.Copies, Service.Backing, Budget, Report, Status);
   end Default_Modules;
   procedure Execute_Frozen
     (Service : in out State; Budget : Natural; Report : out Execution_Report;
      Status : out Values.Access_Status) is
      procedure Execute is new Execute_Frozen_Using (Default_Modules);
   begin
      Execute (Service, Budget, Report, Status);
   end Execute_Frozen;

   procedure Freeze_Catalog (Service : in out State; Result : out Freeze_Status) is
   begin
      if Service.Import_State = Undecided then Result := Freeze_Empty;
      elsif Service.Import_State /= Catalog_Staging then Result := Freeze_Wrong_Phase;
      else Service.Import_State := Catalog_Frozen; Result := Catalog_Frozen_OK; end if;
   end Freeze_Catalog;
end ACPI_Service_Core;
