pragma Ada_2022;
package body ACPI_Service_Core with SPARK_Mode is
   use type Firmware_Tables.Admission;
   use type AML_Table_Backing.State;
   use type Namespace.Load_Status;
   use type Namespace.Object_Kind;
   function Fresh
     (Table_Capacity : Positive := Max_Tables;
      Byte_Capacity : Positive := Max_Total_Bytes;
      Table_Byte_Limit : Positive := Max_Table_Bytes) return State is
     ((Table_Capacity => Table_Capacity, Byte_Capacity => Byte_Capacity,
       Table_Byte_Limit => Table_Byte_Limit, Tree => Namespace.Empty, others => <>));
   function Observe (Service : State) return Metrics is
     (Service.Stats with delta
        Method_Bytes => Namespace.Method_Usage (Service.Tree),
        Value_Objects => Namespace.Value_Usage (Service.Tree).Objects,
        Value_Bytes => Namespace.Value_Usage (Service.Tree).Bytes,
        Package_Elements => Namespace.Value_Usage (Service.Tree).Elements);
   function Snapshot (Service : State) return Namespace.State is (Service.Tree);
   function Table_Info (Service : State; Index : Positive) return Table_Metadata is
     (Service.Catalog (Index).Metadata);
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
   function Same_Catalog (Service, Prior : State) return Boolean is
     (Service.Catalog = Prior.Catalog and then Service.Backing = Prior.Backing
      and then Service.Stats.Tables = Prior.Stats.Tables and then Service.Stats.Bytes = Prior.Stats.Bytes);


   procedure Invoke
     (Service : aliased in out State; Node : Namespace.Node_ID; Args : AML_Execute.Arguments;
      Argument_Count : Natural; Budget : Natural; Result : out AML_Execute.Execution_Result)
   is
   begin
      if Node > Namespace.Count (Service.Tree) then
         Result := (Status => AML_Execute.Invalid_Method, Charged => 0); return;
      end if;
      Namespace.Invoke_With_Tables
        (Service.Tree, Service.Backing, Node, Args, Argument_Count, Budget, Result);
      Service.Stats.Objects := Namespace.Count (Service.Tree);
   end Invoke;

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
      if Index = 0 or else Scope > Namespace.Count (Service.Tree) or else
        Owner > Namespace.Count (Service.Tree)
      then return; end if;
      Namespace.Bind_Table_Region (Service.Tree, Scope, Part,
        (Table => Index, Extent => Table_Info (Service, Index).Extent), Node, Result, Owner);
      Service.Stats.Objects := Namespace.Count (Service.Tree);
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
      if Scope > Namespace.Count (Service.Tree) or else Owner > Namespace.Count (Service.Tree)
        or else Region_Node = Namespace.Root or else Region_Node > Namespace.Count (Service.Tree)
        or else not Namespace.Present (Service.Tree, Region_Node)
        or else Namespace.Kind (Service.Tree, Region_Node) /= Namespace.Table_Region_Object
      then return; end if;
      Namespace.Bind_Table_Field (Service.Tree, Scope, Part,
        (Region => Namespace.Region_Data (Service.Tree, Region_Node),
         Offset => Bit_Offset, Bits => Bit_Count), Node, Result, Owner);
      Service.Stats.Objects := Namespace.Count (Service.Tree);
   end Declare_Table_Field;
   function Read_Namespace_Field (Service : State; Node : Namespace.Node_ID)
      return AML_Field_Data.Read_Result is
   begin
      if Node = Namespace.Root or else Node > Namespace.Count (Service.Tree)
        or else not Namespace.Present (Service.Tree, Node)
        or else Namespace.Kind (Service.Tree, Node) /= Namespace.Table_Field_Object
      then return (Status => AML_Decode.Malformed); end if;
      declare
         Field : constant Namespace.Table_Field := Namespace.Field_Data (Service.Tree, Node);
      begin
         if Field.Region.Table > Service.Stats.Tables or else
           Field.Region.Extent /= Table_Info (Service, Field.Region.Table).Extent
         then return (Status => AML_Decode.Malformed); end if;
         return Table_Field (Service, Field.Region.Table, Field.Offset, Field.Bits);
      end;
   end Read_Namespace_Field;
   procedure Install
     (Service : in out State; ID : Positive; Kind : Table_Kind;
      Data : Firmware_Tables.Bytes; Result : out Install_Status)
   is
      Header : Firmware_Tables.Table_Result;
      Width : AML_Decode.Integer_Width := Service.Width;
      Loaded : Namespace.Load_Status;
      Signature : Firmware_Tables.Signature := "____";
      Old_Bytes : constant Natural := Service.Stats.Bytes;
      procedure Reject (Why : Install_Status) with
        Post => Service.Tree = Service.Tree'Old
          and then Service.Stats.Tables = Service.Stats.Tables'Old
          and then Same_Catalog (Service, Service'Old)
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
      Result := Invalid_Table;
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
      if Kind /= Description then
      if Header.Extent = Firmware_Tables.Table_Header_Size then
         Namespace.Load_Names (Service.Tree, [1 .. 0 => 0], Width, Loaded);
      else
         Namespace.Load_Names
           (Service.Tree, AML_Decode.Bytes
              (Data (Data'First + Firmware_Tables.Table_Header_Size .. Data'Last)),
            Width, Loaded);
      end if;
      Service.Stats.Last_Load_Code := Namespace.Load_Status'Pos (Loaded) + 1;
      if Loaded /= Namespace.Loaded then Reject (Invalid_AML); return; end if;
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
      Service.Stats.Objects := Namespace.Count (Service.Tree);
      Service.Width := Width;
      Result := Installed;
   end Install;
end ACPI_Service_Core;
