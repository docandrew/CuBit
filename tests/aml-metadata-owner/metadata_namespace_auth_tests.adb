with Ada.Text_IO;
with AML_Delays;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Objects;
with AML_References;
with Metadata_Owner_Support; use Metadata_Owner_Support;
procedure Metadata_Namespace_Auth_Tests is
   package N is new AML_Namespace (32, AML_Delays.Unavailable_Provider);
   package O renames N.Owned;
   use type N.State;
   use type N.Load_Status;
   use type AML_Execute.Named_Metadata_Type;
   use type AML_Objects.Allocation_Status;
   use type AML_References.Object_Handle;
   A, B : O.Arena;
   Live, Captured, Fresh : Datum;
   Identity : AML_References.Reference;
   Buffer_Handle : AML_References.Object_Handle;
   Buffer_ID : AML_Objects.Object_ID;
   Allocation : AML_Objects.Allocation_Status;
   Status : Execution_Status;
   Loaded : N.Load_Status;
   Good : Boolean;
   Description : Reference_Metadata;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with Message; end if;
   end Check;
   procedure Setup (Target : in out O.Arena; Width : Integer_Width) is
      Report : N.Initialization_Report;
   begin
      O.Reset (Target, Good); Check (Good, "actual arena reset");
      O.Load (Target, Block ([16#5B#,16#82#], Enc ("DEV0")), Width, Loaded);
      Check (Loaded = N.Loaded, "load device");
      O.Initialize_Members (Target, Report);
      Check (Report.Missing = 0 and then Report.Unsupported = 0, "complete initialization");
   end Setup;
   procedure Reject (Target : in out O.Arena; Item : Datum; Width : Integer_Width) is
      Before : constant N.State := O.Snapshot (Target);
      Value : Datum := Item;
      Copy : Datum;
   begin
      O.Describe_Metadata_Value (Target, Item.Metadata, Description);
      Check (Description.Kind = Invalid_Reference, "unauthenticated metadata description rejected");
      O.Refresh_Value (Target, Value, Status);
      Check (Status = Unsupported_Value and then Value = Item
        and then O.Snapshot (Target) = Before, "refresh rejection preserves input and arena");
      O.Copy_Value (Target, Width, Item, Copy, Status);
      Check (Status = Unsupported_Value and then O.Snapshot (Target) = Before,
        "copy rejection publishes no allocation");
   end Reject;
begin
   for Width in Integer_Width loop
      Setup (A, Width); Setup (B, Width);
      O.Make_Named_Identity (A, 1, Identity, Good); Check (Good, "actual live metadata identity");
      Live := (Metadata_Datum, (Live_Namespace, Identity));
      O.Describe_Metadata_Value (A, Live.Metadata, Description);
      Check (Description.Kind = Metadata_Only and then Description.Object_Type = Device_Metadata,
        "same-owner live positive control");
      O.Copy_Value (A, Width, Live, Captured, Status);
      Check (Status = Returned and then Captured.Value_Kind = Metadata_Datum
        and then Captured.Metadata.Mode = Captured_Namespace, "capture minted by owner operation");
      O.Describe_Metadata_Value (A, Captured.Metadata, Description);
      Check (Description.Kind = Metadata_Only and then Description.Object_Type = Device_Metadata,
        "same-owner captured positive control");
      Reject (B, Captured, Width); Reject (B, Live, Width);
      Reject (A, (Metadata_Datum, (Captured_Namespace, AML_References.No_Object_Handle)), Width);
      O.Append (A, [16#42#], Buffer_ID, Allocation);
      Check (Allocation = AML_Objects.Allocated, "genuine wrong-kind object");
      O.Make_Source (A, Buffer_ID, Buffer_Handle, Good); Check (Good, "genuine same-owner buffer handle");
      Reject (A, (Metadata_Datum, (Captured_Namespace, Buffer_Handle)), Width);
      Setup (A, Width);
      Reject (A, Captured, Width); Reject (A, Live, Width);
      O.Make_Named_Identity (A, 1, Identity, Good); Check (Good, "fresh lifetime identity");
      O.Copy_Value (A, Width, (Metadata_Datum, (Live_Namespace, Identity)), Fresh, Status);
      Check (Status = Returned and then Fresh.Value_Kind = Metadata_Datum
        and then Fresh.Metadata.Mode = Captured_Namespace, "fresh lifetime capture succeeds");
      Check (AML_References.Source (Fresh.Metadata.Snapshot_Source) =
             AML_References.Source (Captured.Metadata.Snapshot_Source)
        and then Fresh.Metadata.Snapshot_Source /= Captured.Metadata.Snapshot_Source,
        "numeric slot reuse cannot replace owner lifetime");
      Reject (A, Captured, Width);
   end loop;
   Ada.Text_IO.Put_Line ("Metadata namespace auth checks" & Checks'Image);
end Metadata_Namespace_Auth_Tests;
