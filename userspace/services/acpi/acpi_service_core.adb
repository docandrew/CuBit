pragma Ada_2022;
package body ACPI_Service_Core with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   use type Firmware_Tables.Admission;
   use type AML_Table_Backing.State;
   use type Namespace.Load_Status;
   use type AML_Execute.Execution_Status;

   function Method_Storage_Capacity return Positive is (Aggregate_Method_Capacity);
   function Valid (Service : State) return Boolean is
     (Values.Valid (Service.Tree));
   function Audit (Service : State) return Values.Audit_Value is (Values.Audit (Service.Tree));
   function Observe (Service : State) return Metrics is
      Usage : constant Values.Usage_Description := Values.Observe_Usage (Service.Tree);
   begin
      return (Service.Stats with delta Objects => Usage.Nodes,
         Method_Bytes => Usage.Methods, Value_Objects => Usage.Values.Objects,
         Value_Bytes => Usage.Values.Bytes, Package_Elements => Usage.Values.Elements);
   end Observe;
   procedure Find_Child (Service : State; Parent : Namespace.Node_ID; Part : AML_Names.Segment;
      Node : out Namespace.Node_ID; Status : out Values.Access_Status) is
   begin Values.Find_Child (Service.Tree, Parent, Part, Node, Status); end Find_Child;
   procedure Observe_Named_Value (Service : in out State; Node : Namespace.Node_ID;
      Result : out Values.Result; Status : out Values.Access_Status) is
   begin Values.Observe_Named_Value (Service.Tree, Node, Result, Status); end Observe_Named_Value;
   function Table_Info (Service : State; Index : Positive) return Table_Metadata is
     (Service.Catalog (Index).Metadata);
   function MADT_Info (Service : State; Index : Positive)
     return Firmware_Tables.MADT.Table_Metadata is (Firmware_Tables.MADT.Decode (Service.Backing.Data (Service.Catalog (Index).Offset + 1 ..
       Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent)));
   function MADT_Record (Service : State; Index : Positive; Record_Index : Natural)
     return Firmware_Tables.MADT.Record_Result is (Firmware_Tables.MADT.Read_Record (Service.Backing.Data (Service.Catalog (Index).Offset + 1 ..
       Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent), Record_Index));
   function DMAR_Info (Service : State; Index : Positive) return Firmware_Tables.DMAR.Table_Metadata is
     (Firmware_Tables.DMAR.Decode (Service.Backing.Data (Service.Catalog (Index).Offset + 1 .. Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent)));
   function DMAR_Record (Service : State; Index : Positive ; Record_Index : Natural) return Firmware_Tables.DMAR.Record_Result is
     (Firmware_Tables.DMAR.Read_Record (Service.Backing.Data (Service.Catalog (Index).Offset + 1 .. Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent), Record_Index));
   function DMAR_Scope (Service : State; Index : Positive ; Record_Index, Scope_Index : Natural) return Firmware_Tables.DMAR.Scope_Result is
     (Firmware_Tables.DMAR.Read_Scope (Service.Backing.Data (Service.Catalog (Index).Offset + 1 .. Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent), Record_Index, Scope_Index));
   function DMAR_Path (Service : State; Index : Positive ; Record_Index, Scope_Index, Path_Index : Natural) return Firmware_Tables.DMAR.Path_Result is
     (Firmware_Tables.DMAR.Read_Path (Service.Backing.Data (Service.Catalog (Index).Offset + 1 .. Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent), Record_Index, Scope_Index, Path_Index));
   function SRAT_Info (Service : State; Index : Positive)
     return Firmware_Tables.SRAT.Table_Metadata is (Firmware_Tables.SRAT.Decode (Service.Backing.Data (Service.Catalog (Index).Offset + 1 ..
       Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent)));
   function SRAT_Record (Service : State; Index : Positive; Record_Index : Natural)
     return Firmware_Tables.SRAT.Record_Result is (Firmware_Tables.SRAT.Read_Record (Service.Backing.Data (Service.Catalog (Index).Offset + 1 ..
       Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent), Record_Index));
   function MCFG_Info (Service : State; Index : Positive) return Firmware_Tables.MCFG.Table_Metadata is
     (Firmware_Tables.MCFG.Decode (Service.Backing.Data (Service.Catalog (Index).Offset + 1 ..
       Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent)));
   function MCFG_Allocation (Service : State; Index : Positive; Allocation_Index : Natural) return Firmware_Tables.MCFG.Allocation_Result is
     (Firmware_Tables.MCFG.Read_Allocation (Service.Backing.Data (Service.Catalog (Index).Offset + 1 ..
       Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent), Allocation_Index));
   function SLIT_Info (Service : State; Index : Positive) return Firmware_Tables.SLIT.Table_Metadata is
     (Firmware_Tables.SLIT.Decode (Service.Backing.Data (Service.Catalog (Index).Offset + 1 ..
       Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent)));
   function SLIT_Distance (Service : State; Index : Positive; From_Locality, To_Locality : Natural) return Firmware_Tables.SLIT.Distance_Result is
     (Firmware_Tables.SLIT.Read_Distance (Service.Backing.Data (Service.Catalog (Index).Offset + 1 ..
       Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent), From_Locality, To_Locality));
   function Table_Byte (Service : State; Index : Positive; Offset : Natural)
      return Firmware_Tables.Byte is
     (Service.Backing.Data (Service.Catalog (Index).Offset + Offset + 1));
   function Fixed_Description (Service : State; Index : Positive) return ACPI_FADT.Result is
     (ACPI_FADT.Decode
        (Service.Backing.Data (Service.Catalog (Index).Offset + 1 ..
           Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent)));
   function Table_Field
     (Service : State; Index : Positive; Bit_Offset, Bit_Count : Natural)
      return AML_Field_Data.Read_Result is
     (AML_Field_Data.Read_Bits
        (AML_Decode.Bytes (Service.Backing.Data (Service.Catalog (Index).Offset + 1 ..
           Service.Catalog (Index).Offset + Service.Catalog (Index).Metadata.Extent)),
         Bit_Offset, Bit_Count));
   function Table_Identity (Service : State; Index : Positive)
      return Firmware_Tables.Identifiers.Identity is
     (Firmware_Tables.Identifiers.Read_Identity
       (Service.Backing.Data (Service.Catalog (Index).Offset + 1 ..
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
   function Model (Service : State) return State_Model is
     (Table_Capacity => Service.Table_Capacity, Byte_Capacity => Service.Byte_Capacity,
      Catalog => Catalog_Snapshot (Service), Tree => Audit (Service),
      Stats => Service.Stats, Width => Service.Width, Table_Limit => Service.Table_Byte_Limit,
      Generation => Values.Generation (Service.Tree));
   function Same_Catalog (Service, Prior : State) return Boolean is
     (Service.Catalog = Prior.Catalog and then Service.Backing = Prior.Backing
      and then Service.Stats.Tables = Prior.Stats.Tables and then Service.Stats.Bytes = Prior.Stats.Bytes);


   function Pending_Members (Service : State) return Natural is
     (Values.Observe_Usage (Service.Tree).Pending);
   function Initialization_Frame (Current, Prior : State_Model) return Boolean is
     (Current = (Prior with delta Tree => Current.Tree)
      and then Values.Initialization_Frame (Current.Tree, Prior.Tree));
   procedure Initialize_Members
     (Service : in out State; Report : out Namespace.Initialization_Report;
      Status : out Values.Access_Status) is
   begin Values.Seal (Service.Tree, Report, Status); end Initialize_Members;
   function Retained_Results (Service : State) return Namespace.Owned.Retained_Root_Count is
     (Values.Retained_Count (Service.Tree));
   procedure Invoke_Retained
     (Service : aliased in out State; Node : Namespace.Node_ID; Args : AML_Execute.Arguments;
      Argument_Count : Natural; Budget : Natural; Result : out Values.Result;
      Status : out Values.Access_Status) is
      Arguments : Values.Arguments := [others => (Values.Immediate_Argument, 0)];
   begin
      for I in 1 .. Argument_Count loop Arguments (I - 1).Number := Args (I - 1); end loop;
      Values.Invoke (Service.Tree, Service.Backing, Node, Arguments, Argument_Count, Budget, Result, Status);
      Service.Stats.Objects := Values.Node_Count (Service.Tree);
   end Invoke_Retained;
   procedure Describe_Result (Service : State; Handle : Values.Value_Handle;
      Description : out Values.Value_Description; Status : out Values.Access_Status) is
   begin Values.Describe (Service.Tree, Handle, Description, Status); end Describe_Result;
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
      subtype Failure_Status is AML_Execute.Execution_Status range AML_Execute.No_Return .. AML_Execute.Execution_Status'Last;
   begin
      Invoke_Retained (Service, Node, Args, Argument_Count, Budget, Outcome, Status);
      if Status /= Values.Available then
         Result := (AML_Execute.Unsupported_Value, Outcome.Charged);
      elsif Outcome.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned then
         Handle := Outcome.Handle; Release_Result (Service, Handle, Status);
         if Status /= Values.Available then raise Program_Error with "scalar release failed"; end if;
         Result := (AML_Execute.Unsupported_Value, Outcome.Charged);
      elsif Outcome.Status = AML_Execute.Returned then
         Result := (AML_Execute.Returned, Outcome.Charged, Outcome.Number, Outcome.Origin);
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
      if Index = 0 then return; end if;
      declare
         Status : Values.Access_Status;
      begin
         Values.Bind_Table_Region (Service.Tree, Scope, Part,
            (Table => Index, Extent => Table_Info (Service, Index).Extent), Node, Result, Status, Owner);
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
      if Service.Stats.Tables = 0 then return; end if;
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
   begin
      Values.Read_Table_Field (Service.Tree, Node, Field, Status);
      if Status /= Values.Available then return (Status => AML_Decode.Malformed); end if;
      if Field.Region.Table > Service.Stats.Tables or else
        Field.Region.Extent /= Table_Info (Service, Field.Region.Table).Extent
      then return (Status => AML_Decode.Malformed); end if;
      return Table_Field (Service, Field.Region.Table, Field.Offset, Field.Bits);
   end Read_Namespace_Field;
   procedure Install
     (Service : in out State; ID : Positive; Kind : Table_Kind;
      Data : Firmware_Tables.Bytes; Result : out Install_Status)
   is
      Header : Firmware_Tables.Table_Result;
      Width : AML_Decode.Integer_Width := Service.Width;
      Loaded : Namespace.Load_Status;
      Access_Result : Values.Access_Status;
      Signature : Firmware_Tables.Signature;
      Old_Bytes : constant Natural := Service.Stats.Bytes;
      procedure Reject (Why : Install_Status) with
        Post => Valid (Service) = Valid (Service)'Old
          and then Values.Audit (Service.Tree) = Values.Audit (Service.Tree)'Old
          and then Service.Stats.Tables = Service.Stats.Tables'Old
          and then Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old
          and then Result = Why
      is
      begin
         Result := Why;
         if Service.Stats.Rejections = Natural'Last then
            Service.Stats.Counter_Saturated := True;
         else
            Service.Stats.Rejections := Service.Stats.Rejections + 1;
         end if;
      end Reject;
   begin
      if Values.Current (Service.Tree) = Values.Ready then Reject (Wrong_Order); return; end if;
      if (Kind = DSDT) /= (Service.Stats.Tables = 0) then
         Reject (Wrong_Order); return;
      end if;
      for I in 1 .. Service.Stats.Tables loop
         pragma Loop_Invariant (Service.Stats.Tables = Service.Stats.Tables'Loop_Entry);
         if Service.Catalog (I).Metadata.ID = ID then Reject (Duplicate_ID); return; end if;
      end loop;
      if Service.Stats.Tables = Service.Table_Capacity then Reject (Table_Limit); return; end if;
      if Data'Length > Service.Table_Byte_Limit or else
        Data'Length > Service.Byte_Capacity - Service.Stats.Bytes
      then
         Reject (Byte_Limit); return;
      end if;
      if Data'Length < Firmware_Tables.Table_Header_Size then Reject (Invalid_Table); return; end if;
      for I in Signature'Range loop
         Signature (I) := Character'Val (Data (Data'First + (I - 1)));
      end loop;
      if (Kind = DSDT and then Signature /= "DSDT") or else
         (Kind = SSDT and then Signature /= "SSDT") or else
         (Kind = Description and then Signature in "DSDT" | "SSDT" | "FACS")
      then Reject (Invalid_Table); return; end if;
      Header := Firmware_Tables.Read_Table (Data, Signature);
      if Header.Status /= Firmware_Tables.Accepted then
         Reject (Invalid_Table); return;
      end if;
      if Header.Extent /= Data'Length then Reject (Invalid_Table); return; end if;
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
      -- Commit retained bytes only after every admission/AML check succeeded.
      Service.Backing.Data (Old_Bytes + 1 .. Old_Bytes + Data'Length) := Data;
      Service.Backing.Tables (Service.Stats.Tables + 1) :=
        (Offset => Old_Bytes, Extent => Data'Length);
      Service.Backing.Count := Service.Stats.Tables + 1;
      Service.Catalog (Service.Stats.Tables + 1) :=
        (Metadata => (ID => ID, Signature => Signature, Extent => Data'Length,
                      Revision => Header.Revision), Offset => Old_Bytes);
      Service.Stats.Tables := Service.Stats.Tables + 1;
      Service.Stats.Bytes := Service.Stats.Bytes + Header.Extent;
      Service.Stats.Objects := Values.Node_Count (Service.Tree);
      Service.Width := Width;
      Result := Installed;
   end Install;
end ACPI_Service_Core;
