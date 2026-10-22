with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Namespace;
with AML_Objects;
with AML_References;
with Root_Test_Support; use Root_Test_Support;
procedure Metadata_Root_Owner_Tests is
   use type AML_Decode.Bytes;
   package E renames AML_Execute;
   package O renames AML_Objects;
   package N is new AML_Namespace (Capacity => 32, Perform_Delay => AML_Delays.Unavailable_Provider);
   use type AML_Decode.Integer_Value;
   use type E.Execution_Status;
   use type E.Datum_Kind;
   use type O.Allocation_Status;
   use type O.Usage;
   use type N.State;
   use type N.Object_Kind;
   use type N.Load_Status;
   A, B : N.Owned.Arena;
   Root_A, Root_B, Package_Ref, Device_Ref : AML_References.Reference;
   Source : AML_References.Object_Handle;
   Success : Boolean;
   Load_Status : N.Load_Status;
   Status : E.Execution_Status;
   Allocation : O.Allocation_Status;
   Object : O.Object_ID;
   Live_Root, Captured, Same_Owner_Metadata, Buffer_Value, Package_Value, Copy, Current : E.Datum;
   Old_B_Capture, Fresh_B_Capture : E.Datum;
   Before : N.State;
   Usage_Before : O.Usage;
   Pins_Before : N.Owned.Retained_Root_Count;
   Child : N.Node_ID;
   Checks : Natural := 0;
   Code : constant Bytes := Bytes'[16#08#] & Enc ("CHLD") & [16#0A#, 7]
     & [16#08#] & Enc ("PKG0") & Block ([16#12#], [2, 16#0A#, 11, 16#0A#, 22])
     & Block ([16#5B#, 16#82#], Enc ("DEV0"));
   procedure Check (Good : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Message; end if;
   end Check;
   procedure Attach (Arena : in out N.Owned.Arena; Root_Ref : AML_References.Reference; Item : E.Datum) is
   begin
      N.Owned.Copy_And_Attach (Arena, (E.Referenced_Destination, Root_Ref), Bits_64, Item, Copy, Status);
   end Attach;
   procedure Check_Failure_Frame (Message : String) is
   begin
      Check (N.Owned.Snapshot (B) = Before and then N.Owned.Values_Used (B) = Usage_Before
        and then N.Owned.Retained_Count (B) = Pins_Before, Message);
   end Check_Failure_Frame;
begin
   -- Local owner-policy fixtures: direct attachment API is not an ACPICA AML
   -- conformance claim. All references originate from actual initialized arenas.
   N.Owned.Reset (A, Success); Check (Success, "initialize A");
   N.Owned.Reset (B, Success); Check (Success, "initialize B");
   N.Owned.Load (B, Code, Bits_64, Load_Status); Check (Load_Status = N.Loaded, "load children and package");
   Child := N.Child (N.Owned.Snapshot (B), N.Root, "CHLD");
   Check (Child /= N.Root and then N.Owned.Kind (B, N.Root) = N.Scope_Object, "physical root scope and child");
   N.Owned.Make_Named_Identity (A, N.Root, Root_A, Success); Check (Success, "authentic root A identity");
   N.Owned.Make_Named_Identity (B, N.Root, Root_B, Success); Check (Success, "authentic root B identity");
   N.Owned.Resolve_Semantic_Reference (A, Root_A, Live_Root, Status);
   Check (Status = E.Returned and then Live_Root.Value_Kind = E.Metadata_Datum, "initial root semantic metadata");
   N.Owned.Copy_Value (A, Bits_64, Live_Root, Captured, Status);
   Check (Status = E.Returned and then Captured.Value_Kind = E.Metadata_Datum, "genuine captured positive control");
   N.Owned.Resolve_Semantic_Reference (B, Root_B, Current, Status);
   Check (Status = E.Returned and then Current.Value_Kind = E.Metadata_Datum,
     "same-owner initial root metadata for stale-generation witness");
   N.Owned.Copy_Value (B, Bits_64, Current, Old_B_Capture, Status);
   Check (Status = E.Returned and then Old_B_Capture.Value_Kind = E.Metadata_Datum,
     "same-owner capture minted before reset");
   Before := N.Owned.Snapshot (B); Usage_Before := N.Owned.Values_Used (B); Pins_Before := N.Owned.Retained_Count (B);
   Attach (B, Root_B, Captured);
   Check (Status = E.Unsupported_Value, "foreign captured value rejected at root attachment");
   Check_Failure_Frame ("foreign capture cannot mutate semantic root or publish allocation");
   N.Owned.Copy_Value (B, Bits_64, (E.Integer_Datum, 42, Ordinary_Integer), Copy, Status);
   Check (Status = E.Returned, "B ordinary copy positive control");
   Attach (B, Root_B, (E.Integer_Datum, 42, Ordinary_Integer));
   Check (Status = E.Returned, "explicit root integer attachment positive control");
   N.Owned.Resolve_Semantic_Reference (B, Root_B, Current, Status);
   Check (Status = E.Returned and then Current.Value_Kind = E.Integer_Datum and then Current.Number = 42
     and then N.Owned.Kind (B, N.Root) = N.Scope_Object
     and then N.Child (N.Owned.Snapshot (B), N.Root, "CHLD") = Child,
     "root integer semantics remain distinct from physical scope");
   Before := N.Owned.Snapshot (B);
   N.Owned.Store_Integer (B, Root_B, 99, Status);
   Check (Status = E.Unsupported_Value and then N.Owned.Snapshot (B) = Before,
     "explicit copy does not grant ordinary root Store authority");
   N.Owned.Append (B, [16#A5#, 16#5A#], Object, Allocation); Check (Allocation = O.Allocated, "buffer source");
   N.Owned.Make_Source (B, Object, Source, Success); Check (Success, "actual buffer source handle");
   N.Owned.Read_Source (B, Source, Buffer_Value, Status); Check (Status = E.Returned, "read buffer source");
   Attach (B, Root_B, Buffer_Value); Check (Status = E.Returned, "attach root buffer");
   Check (not N.Owned.Matches (B, Root_B), "semantic root does not widen ordinary data-reference authority");
   Check (N.Owned.Kind (B, N.Root) = N.Scope_Object
     and then N.Child (N.Owned.Snapshot (B), N.Root, "CHLD") = Child,
     "semantic payload preserves physical root and child resolution");
   N.Owned.Resolve_Semantic_Reference (B, Root_B, Current, Status);
   Check (Status = E.Returned and then Current.Value_Kind = E.Object_Datum, "root resolves attached buffer");
   N.Owned.Make_Named_Reference (B, N.Child (N.Owned.Snapshot (B), N.Root, "PKG0"), Package_Ref, Success);
   Check (Success, "actual package reference");
   N.Owned.Resolve_Value (B, Package_Ref, Package_Value, Status); Check (Status = E.Returned, "package source");
   N.Owned.Make_Named_Identity (B, N.Child (N.Owned.Snapshot (B), N.Root, "DEV0"), Device_Ref, Success);
   Check (Success, "actual same-owner device identity");
   N.Owned.Resolve_Semantic_Reference (B, Device_Ref, Same_Owner_Metadata, Status);
   Check (Status = E.Returned and then Same_Owner_Metadata.Value_Kind = E.Metadata_Datum,
     "same-owner live metadata source before quota");
   while N.Owned.Values_Used (B).Objects < O.Max_Objects - 1 loop
      N.Owned.Append (B, Bytes'(1 .. 0 => 0), Object, Allocation);
      Check (Allocation = O.Allocated, "fill all but one object slot");
   end loop;
   Before := N.Owned.Snapshot (B); Usage_Before := N.Owned.Values_Used (B); Pins_Before := N.Owned.Retained_Count (B);
   Attach (B, Root_B, Package_Value);
   Check (Status = E.Value_Limit, "package root copy fails after candidate container allocation");
   Check_Failure_Frame ("partial package copy preserves root payload and complete state");
   N.Owned.Append (B, Bytes'(1 .. 0 => 0), Object, Allocation); Check (Allocation = O.Allocated, "fill last slot");
   Before := N.Owned.Snapshot (B); Usage_Before := N.Owned.Values_Used (B); Pins_Before := N.Owned.Retained_Count (B);
   Attach (B, Root_B, Same_Owner_Metadata);
   Check (Status = E.Value_Limit, "same-owner capture allocation exhausted at root attachment");
   Check_Failure_Frame ("failed captured allocation preserves prior root buffer");
   N.Owned.Reset (A, Success); Check (Success, "reset source owner");
   Before := N.Owned.Snapshot (B); Usage_Before := N.Owned.Values_Used (B); Pins_Before := N.Owned.Retained_Count (B);
   Attach (B, Root_B, Captured);
   Check (Status = E.Unsupported_Value, "stale foreign capture still rejects before quota");
   Check_Failure_Frame ("stale capture preserves full root arena");
   N.Owned.Reset (B, Success); Check (Success, "reset target owner");
   Check (N.Owned.Values_Used (B) = O.Usage'(others => 0)
     and then N.Owned.Kind (B, N.Root) = N.Scope_Object
     and then not N.Owned.Named_Identity_Matches (B, Root_B), "reset drops payload and invalidates old root identity");
   N.Owned.Make_Named_Identity (B, N.Root, Root_B, Success); Check (Success, "fresh root identity");
   N.Owned.Resolve_Semantic_Reference (B, Root_B, Current, Status);
   Check (Status = E.Returned and then Current.Value_Kind = E.Metadata_Datum, "reset restores semantic metadata root");
   N.Owned.Copy_Value (B, Bits_64, Current, Fresh_B_Capture, Status);
   Check (Status = E.Returned and then Fresh_B_Capture.Value_Kind = E.Metadata_Datum,
     "fresh same-owner capture minted after reset");
   Attach (B, Root_B, Fresh_B_Capture);
   Check (Status = E.Returned, "fresh same-owner capture attaches to fresh root");
   Before := N.Owned.Snapshot (B); Usage_Before := N.Owned.Values_Used (B); Pins_Before := N.Owned.Retained_Count (B);
   Attach (B, Root_B, Old_B_Capture);
   Check (Status = E.Unsupported_Value, "old same-arena capture rejected after generation reset");
   Check_Failure_Frame ("stale same-arena capture cannot alter fresh root binding");
   N.Owned.Resolve_Semantic_Reference (B, Root_B, Current, Status);
   Check (Status = E.Returned and then Current.Value_Kind = E.Metadata_Datum,
     "fresh root capture remains readable after stale rejection");
   Ada.Text_IO.Put_Line ("Metadata root owner checks" & Checks'Image);
end Metadata_Root_Owner_Tests;
