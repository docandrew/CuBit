with Ada.Text_IO;
with AML_Identity;
with AML_Identity.Issuer;
with AML_Metadata_Kinds;
with AML_Object_Identifiers;
with AML_References;
with AML_Objects;
with AML_Objects.Copies;
with AML_Objects.Reachability;
with AML_Objects.Reclamation;
procedure Metadata_Snapshot_Tests is
   package O renames AML_Objects;
   package K renames AML_Metadata_Kinds;
   package R renames AML_References;
   package Trace renames AML_Objects.Reachability;
   package GC renames AML_Objects.Reclamation;
   use type O.State;
   use type O.Object_Kind;
   use type O.Allocation_Status;
   use type K.Snapshot_Description;
   use type R.Reference;
   use type Trace.Trace_Status;
   use type GC.Reclaim_Status;
   Store, Prior : O.State := O.Empty;
   Device, Thermal, Live, Ref, Package_ID, Copy_ID, Garbage, Fresh : O.Object_ID;
   Status : O.Allocation_Status;
   Witness : O.Copies.Copy_Witness;
   Owner, Other : AML_Identity.Identity;
   Success : Boolean;
   Named : R.Reference;
   Saved_Handle : R.Object_Handle;
   Device_Value : constant K.Snapshot_Description := (Object_Type => K.Device_Metadata);
   Thermal_Value : constant K.Snapshot_Description := (Object_Type => K.Thermal_Metadata);
   Seeds, Keep : GC.Keep_Set := [others => False];
   Targets : Trace.Reference_Targets := [others => AML_Object_Identifiers.No_Address];
   Trace_Work : Trace.Workspace;
   GC_Work : GC.Workspace;
   Trace_Status : Trace.Trace_Status;
   GC_Status : GC.Reclaim_Status;
   Checks : Natural := 0;
   Mixed_Size : constant := 4;
   procedure Check (Good : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Message; end if;
   end Check;
   procedure Reject_Unsupported is
   begin
      for Tag in K.Kind loop
         if Tag not in K.Snapshot_Kind then
            Prior := Store;
            O.New_Metadata_Snapshot (Store, (Object_Type => Tag), Fresh, Status);
            Check (Status = O.Invalid_Metadata and then Fresh = O.No_Object
              and then Store = Prior, "unsupported captured class rejected before quota");
         end if;
      end loop;
   end Reject_Unsupported;
begin
   AML_Identity.Issuer.Issue (Owner, Success); Check (Success, "issued owner");
   AML_Identity.Issuer.Issue (Other, Success); Check (Success, "issued other owner");
   Reject_Unsupported;
   O.New_Metadata_Snapshot (Store, Device_Value, Device, Status);
   Check (Status = O.Allocated and then O.Kind (Store, Device) = O.Metadata_Snapshot_Object
     and then O.Metadata_Snapshot_Data (Store, Device) = Device_Value
     and then O.Length (Store, Device) = 0, "captured device descriptor");
   O.New_Metadata_Snapshot (Store, Thermal_Value, Thermal, Status);
   Check (Status = O.Allocated and then O.Metadata_Snapshot_Data (Store, Thermal) = Thermal_Value
     and then O.Byte_Count (Store) = 0 and then O.Element_Count (Store) = 0,
     "captured thermal descriptor uses no backing storage");
   O.Copies.Clone (Store, Thermal, Copy_ID, Status, Witness);
   Check (Status = O.Allocated and then Copy_ID /= Thermal
     and then O.Kind (Store, Copy_ID) = O.Metadata_Snapshot_Object
     and then O.Metadata_Snapshot_Data (Store, Copy_ID) = Thermal_Value,
     "standalone clone preserves thermal snapshot payload");
   Named := R.Bind_Named (Owner, 1, 1);
   O.New_Metadata (Store, Named, Live, Status); Check (Status = O.Allocated, "live metadata leaf");
   O.New_Reference (Store, Named, Ref, Status); Check (Status = O.Allocated, "explicit reference leaf");
   O.New_Package (Store, Mixed_Size, Package_ID, Status); Check (Status = O.Allocated, "mixed package");
   O.Set_Element (Store, Package_ID, 0, Device);
   O.Set_Element (Store, Package_ID, 1, Device);
   O.Set_Element (Store, Package_ID, 2, Live);
   O.Set_Element (Store, Package_ID, 3, Ref);
   O.Copies.Clone (Store, Package_ID, Copy_ID, Status, Witness);
   Check (Status = O.Allocated and then Copy_ID /= Package_ID, "mixed package copied");
   declare
      Left : constant O.Object_ID := O.Element (Store, Copy_ID, 0);
      Right : constant O.Object_ID := O.Element (Store, Copy_ID, 1);
      Live_Copy : constant O.Object_ID := O.Element (Store, Copy_ID, 2);
      Ref_Copy : constant O.Object_ID := O.Element (Store, Copy_ID, 3);
   begin
      Check (Left /= Right and then Left /= Device and then Right /= Device
        and then O.Kind (Store, Left) = O.Metadata_Snapshot_Object
        and then O.Kind (Store, Right) = O.Metadata_Snapshot_Object
        and then O.Metadata_Snapshot_Data (Store, Left) = Device_Value
        and then O.Metadata_Snapshot_Data (Store, Right) = Device_Value,
        "repeated captured members have independent wrappers and same descriptor");
      Check (O.Kind (Store, Live_Copy) = O.Metadata_Object
        and then O.Metadata_Data (Store, Live_Copy) = Named
        and then O.Kind (Store, Ref_Copy) = O.Reference_Object
        and then O.Reference_Data (Store, Ref_Copy) = Named,
        "package copy preserves live versus captured versus explicit reference kinds");
      O.Set_Element (Store, Copy_ID, 0, Thermal);
      Check (O.Element (Store, Package_ID, 0) = Device,
        "copy mutation does not replace source captured member");
   end;
   O.New_Integer (Store, 42, Garbage, Status); Check (Status = O.Allocated, "unrelated object");
   -- Keep the original package only. Descriptor nodes must be retained through
   -- package edges; supplied targets cannot give captured leaves extra edges.
   Seeds (Package_ID) := True;
   Targets (Device) := O.Address_Of (Store, Garbage);
   Trace.Trace (Store, Seeds, Targets, Trace_Work, Keep, Trace_Status);
   Check (Trace_Status = Trace.Traced and then Keep (Package_ID) and then Keep (Device)
     and then Keep (Live) and then Keep (Ref) and then not Keep (Garbage)
     and then not Keep (Thermal) and then not Keep (Copy_ID), "package-only captured leaf tracing");
   Saved_Handle := R.Bind_Object (Owner, O.Address_Of (Store, Device));
   Check (R.Belongs_To (Saved_Handle, Owner) and then not R.Belongs_To (Saved_Handle, Other)
     and then O.Matches_Address (Store, R.Address (Saved_Handle)), "owner/address handle primitives");
   GC.Reclaim (Store, Owner, Keep, GC_Work, GC_Status);
   Check (GC_Status = GC.Reclaimed and then O.Live_Count (Store) = 4
     and then O.Metadata_Snapshot_Data (Store, Device) = Device_Value
     and then O.Matches_Address (Store, R.Address (Saved_Handle)), "captured value survives package-only GC");
   Keep := [others => False]; GC.Reclaim (Store, Owner, Keep, GC_Work, GC_Status);
   Check (GC_Status = GC.Reclaimed and then not O.Matches_Address (Store, R.Address (Saved_Handle)),
     "removed snapshot address invalidated");
   O.New_Metadata_Snapshot (Store, Thermal_Value, Fresh, Status);
   Check (Status = O.Allocated and then Fresh = Device
     and then not O.Matches_Address (Store, R.Address (Saved_Handle)),
     "slot reuse does not revive old snapshot handle");
   Store := O.Empty (Max_Incarnation => AML_Object_Identifiers.First_Incarnation);
   O.New_Metadata_Snapshot (Store, Thermal_Value, Thermal, Status); Check (Status = O.Allocated, "quota snapshot");
   O.New_Package (Store, 2, Package_ID, Status); Check (Status = O.Allocated, "quota package");
   O.Set_Element (Store, Package_ID, 0, Thermal); O.Set_Element (Store, Package_ID, 1, Thermal);
   while O.Live_Count (Store) < O.Max_Objects - 2 loop
      O.New_Integer (Store, 0, Fresh, Status); Check (Status = O.Allocated, "fill quota");
   end loop;
   Prior := Store; O.Copies.Clone (Store, Package_ID, Copy_ID, Status, Witness);
   Check (Status = O.Object_Limit and then Copy_ID = O.No_Object and then Store = Prior,
     "partial captured member allocation rolls back complete clone");
   O.New_Metadata_Snapshot (Store, Device_Value, Fresh, Status); Check (Status = O.Allocated, "penultimate slot");
   O.New_Metadata_Snapshot (Store, Device_Value, Fresh, Status); Check (Status = O.Allocated, "last slot");
   Reject_Unsupported;
   Prior := Store; O.New_Metadata_Snapshot (Store, Device_Value, Fresh, Status);
   Check (Status = O.Object_Limit and then Fresh = O.No_Object and then Store = Prior,
     "full snapshot allocation atomic");
   Keep := [others => False]; GC.Reclaim (Store, Owner, Keep, GC_Work, GC_Status);
   Check (GC_Status = GC.Reclaimed, "retire snapshot pool");
   Reject_Unsupported;
   Prior := Store; O.New_Metadata_Snapshot (Store, Thermal_Value, Fresh, Status);
   Check (Status = O.Generation_Limit and then Fresh = O.No_Object and then Store = Prior,
     "captured allocation honors retired incarnations");
   Ada.Text_IO.Put_Line ("Metadata snapshot checks" & Checks'Image);
end Metadata_Snapshot_Tests;
