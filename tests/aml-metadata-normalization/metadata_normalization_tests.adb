with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Names;
with AML_Namespace;
with AML_Objects;
with AML_References;
procedure Metadata_Normalization_Tests is
   package E renames AML_Execute;
   package R renames AML_References;
   package O renames AML_Objects;
   package N is new AML_Namespace (Capacity => 16, Perform_Delay => AML_Delays.Unavailable_Provider);
   use type AML_Decode.Integer_Value;
   use type E.Execution_Status;
   use type E.Datum_Kind;
   use type E.Metadata_Mode;
   use type E.Reference_Metadata_Kind;
   use type E.Named_Metadata_Type;
   use type N.State;
   use type N.Object_Kind;
   use type N.Load_Status;
   use type N.Owned.Retain_Status;
   use type N.Owned.Release_Status;
   use type N.Owned.Retained_Root;
   use type O.Allocation_Status;
   use type O.Usage;
   use type R.Reference;
   A, B : N.Owned.Arena;
   Root_A, Foreign_Root, Variable_Identity : R.Reference;
   Live_Root, Captured, Wrapped, Copy, Live_Variable, Normalized, Read_Back, Buffer_Value : E.Datum;
   Source : R.Object_Handle;
   Object : O.Object_ID;
   Allocation : O.Allocation_Status;
   Loaded : N.Load_Status;
   Success : Boolean;
   Status : E.Execution_Status;
   Retain_Status : N.Owned.Retain_Status;
   Release_Status : N.Owned.Release_Status;
   Live_Pin, Captured_Pin, Rejected_Pin : N.Owned.Retained_Root;
   Before : N.State;
   Usage_Before : O.Usage;
   Pins_Before : N.Owned.Retained_Root_Count;
   Description : E.Reference_Metadata;
   Checks : Natural := 0;
   function Path (Name : String) return AML_Names.Name_Result is
      Data : Bytes (1 .. Name'Length);
   begin
      for I in Data'Range loop Data (I) := Character'Pos (Name (Name'First + I - 1)); end loop;
      return AML_Names.Read_Name (Data);
   end Path;
   -- Name(DST0,9), Name(VAR0,Zero); all identities are then issued by real arenas.
   Code : constant Bytes := [16#08#, 68,83,84,48,16#0A#,9, 16#08#,86,65,82,48,0];
   procedure Check (Good : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Message; end if;
   end Check;
   procedure Save_Frame is
   begin
      Before := N.Owned.Snapshot (A);
      Usage_Before := N.Owned.Values_Used (A);
      Pins_Before := N.Owned.Retained_Count (A);
   end Save_Frame;
   procedure Check_Frame (Message : String) is
   begin
      Check (N.Owned.Snapshot (A) = Before and then N.Owned.Values_Used (A) = Usage_Before
        and then N.Owned.Retained_Count (A) = Pins_Before, Message);
   end Check_Frame;
begin
   N.Owned.Reset (A, Success); Check (Success, "initialize actual owner A");
   N.Owned.Reset (B, Success); Check (Success, "initialize actual owner B");
   N.Owned.Load (A, Code, Bits_64, Loaded); Check (Loaded = N.Loaded, "load mutable names");
   N.Owned.Make_Named_Identity (A, N.Root, Root_A, Success); Check (Success, "A root identity");
   N.Owned.Make_Named_Identity (B, N.Root, Foreign_Root, Success);
   Check (Success and then R.Well_Formed (Foreign_Root)
     and then not N.Owned.Named_Identity_Matches (A, Foreign_Root), "genuine well-formed foreign identity");
   N.Owned.Resolve_Semantic_Reference (A, Root_A, Live_Root, Status);
   Check (Status = E.Returned and then Live_Root.Value_Kind = E.Metadata_Datum
     and then Live_Root.Metadata.Mode = E.Live_Namespace, "real live root value");
   N.Owned.Copy_Value (A, Bits_64, Live_Root, Captured, Status);
   Check (Status = E.Returned and then Captured.Value_Kind = E.Metadata_Datum
     and then Captured.Metadata.Mode = E.Captured_Namespace, "real captured positive control");
   Source := Captured.Metadata.Snapshot_Source;
   N.Owned.Read_Source (A, Source, Read_Back, Status);
   Check (Status = E.Returned and then Read_Back.Value_Kind = E.Metadata_Datum,
     "genuine source normalizes to metadata");
   -- Repackage only a genuine handle with its matching ID. The object-shaped
   -- cache is untrusted; it must not survive canonicalization as Object_Datum.
   Wrapped := (E.Object_Datum, (Source => Source, ID => R.Source (Source),
     Type_Code => 3, Size => 0, others => <>));
   Save_Frame;
   N.Owned.Replace_Value (A, N.Root, Path ("DST0"), Bits_64, Wrapped, Status);
   Check (Status = E.Unsupported_Value, "Replace rejects metadata discovered through object normalization");
   Check_Frame ("normalized metadata replacement is atomic and raises no exception");
   N.Owned.Append (A, [16#A5#], Object, Allocation); Check (Allocation = O.Allocated, "ordinary buffer positive control");
   N.Owned.Make_Source (A, Object, Source, Success); Check (Success, "actual buffer source");
   N.Owned.Read_Source (A, Source, Buffer_Value, Status); Check (Status = E.Returned, "read ordinary buffer");
   N.Owned.Replace_Value (A, N.Root, Path ("DST0"), Bits_64, Buffer_Value, Status);
   Check (Status = E.Returned, "Replace accepts supported normalized data");
   N.Owned.Copy_And_Attach (A, (E.Named_Destination, N.Root, Path ("VAR0")),
     Bits_64, Live_Root, Copy, Status);
   Check (Status = E.Returned and then N.Owned.Kind (A, N.Child (N.Owned.Snapshot (A), N.Root, "VAR0")) = N.Metadata_Value_Object,
     "named variable stores genuine captured metadata");
   N.Owned.Make_Named_Identity (A, N.Child (N.Owned.Snapshot (A), N.Root, "VAR0"), Variable_Identity, Success);
   Check (Success, "actual metadata variable identity");
   N.Owned.Resolve_Semantic_Reference (A, Variable_Identity, Live_Variable, Status);
   Check (Status = E.Returned and then Live_Variable.Value_Kind = E.Metadata_Datum
     and then Live_Variable.Metadata.Mode = E.Live_Namespace,
     "variable expression is live namespace metadata");
   N.Owned.Retain (A, Live_Variable, Live_Pin, Retain_Status);
   Check (Retain_Status = N.Owned.Retained and then Live_Pin /= N.Owned.No_Retained_Root,
     "retain live variable before replacement");
   N.Owned.Read_Retained (A, Live_Pin, Read_Back, Status);
   Check (Status = E.Returned and then Read_Back.Value_Kind = E.Metadata_Datum
     and then Read_Back.Metadata.Mode = E.Live_Namespace, "retained live descriptor positive read");
   N.Owned.Retain (A, Captured, Captured_Pin, Retain_Status);
   Check (Retain_Status = N.Owned.Retained, "retain independent captured positive control");
   -- Structural reference storage is permitted by this existing low-level API.
   -- It does not authorize retaining/using the resulting foreign descriptor.
   N.Owned.Replace_Value (A, N.Root, Path ("VAR0"), Bits_64, (E.Reference_Datum, Foreign_Root), Status);
   Check (Status = E.Returned, "store structurally valid foreign reference through supported API");
   Normalized := Live_Variable;
   N.Owned.Refresh_Value (A, Normalized, Status);
   Check (Status = E.Returned and then Normalized.Value_Kind = E.Reference_Datum
     and then Normalized.Ref = Foreign_Root, "live metadata normalization reaches foreign reference");
   Save_Frame;
   N.Owned.Retain (A, Live_Variable, Rejected_Pin, Retain_Status);
   Check (Retain_Status = N.Owned.Invalid_Value and then Rejected_Pin = N.Owned.No_Retained_Root,
     "Retain validates reference after metadata normalization");
   Check_Frame ("rejected normalized retention publishes no pin or data");
   N.Owned.Read_Retained (A, Live_Pin, Read_Back, Status);
   Check (Status = E.Unsupported_Value and then Read_Back.Value_Kind = E.Integer_Datum
     and then Read_Back.Number = 0, "Read_Retained rejects newly foreign normalized reference with safe output");
   Check_Frame ("invalid retained read preserves arena and existing pins");
   N.Owned.Read_Retained (A, Captured_Pin, Read_Back, Status);
   Check (Status = E.Returned and then Read_Back.Value_Kind = E.Metadata_Datum
     and then Read_Back.Metadata.Mode = E.Captured_Namespace, "independent capture remains usable");
   N.Owned.Describe_Metadata_Value (A, Read_Back.Metadata, Description);
   Check (Description.Kind = E.Metadata_Only and then Description.Object_Type = E.Device_Metadata,
     "captured positive control still describes Device");
   N.Owned.Release (A, Live_Pin, Release_Status); Check (Release_Status = N.Owned.Released, "release invalid-content live pin");
   N.Owned.Release (A, Captured_Pin, Release_Status);
   Check (Release_Status = N.Owned.Released and then N.Owned.Retained_Count (A) = 0, "all pins released");
   Ada.Text_IO.Put_Line ("Metadata normalization checks" & Checks'Image);
end Metadata_Normalization_Tests;
