with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Namespace;
with AML_Objects;
with AML_References;
with AML_Execute;
with Alias_Test_Support; use Alias_Test_Support;
procedure Alias_Namespace_Tests is
   use type AML_Decode.Bytes;
   package N is new AML_Namespace (Capacity => 32, Perform_Delay => AML_Delays.Unavailable_Provider);
   package Tiny is new AML_Namespace (Capacity => 2, Perform_Delay => AML_Delays.Unavailable_Provider);
   use N;
   use type AML_Objects.Usage;
   use type AML_Objects.Object_Kind;
   use type AML_References.Reference_Kind;
   use type AML_Execute.Reference_Metadata_Kind;
   use type AML_Execute.Named_Metadata_Type;
   use type Integer_Value;
   use type Tiny.State;
   use type Tiny.Load_Status;
   Tree : State := Empty;
   Prior : State;
   Loaded : Load_Status;
   Checks : Natural := 0;
   Original, Alias_Node, Chain_Node, Device_Node, Device_Alias : Node_ID;
   Located : Lookup_Result;
   Base : constant Bytes := [16#08#] & Enc ("NUM0") & [16#0A#, 7];
   procedure Check (Good : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Message & " check" & Checks'Image & " " & Loaded'Image; end if;
   end Check;
   function Canonical_Is (Node, Expected : Node_ID) return Boolean is
      Value : constant Lookup_Result := Canonical_Node (Tree, Node);
   begin return Value.Status = Found and then Value.Node = Expected; end Canonical_Is;
   procedure Reject_Atomically (Code : Bytes; Message : String) is
      Before : constant State := Tree;
   begin
      Load_Names (Tree, Code, Bits_64, Loaded);
      Check (Loaded /= N.Loaded and then Tree = Before, Message);
   end Reject_Atomically;
begin
   Load_Names (Tree, Base, Bits_64, Loaded); Check (Loaded = N.Loaded, "base");
   Original := Child (Tree, Root, "NUM0"); Prior := Tree;
   Load_Names (Tree, Alias_Of ("NUM0", "ALS0") & Alias_Of ("ALS0", "ALS1"), Bits_64, Loaded);
   Check (Loaded = N.Loaded, "alias chain load");
   Alias_Node := Child (Tree, Root, "ALS0"); Chain_Node := Child (Tree, Root, "ALS1");
   Check (Alias_Node /= Original and then Chain_Node /= Alias_Node, "distinct physical bindings");
   Check (Name (Tree, Alias_Node) = "ALS0" and then Parent (Tree, Alias_Node) = Root, "physical metadata");
   Check (Kind (Tree, Alias_Node) = Alias_Object and then Kind (Tree, Original) = Integer_Object, "physical kinds");
   Check (Canonical_Is (Alias_Node, Original) and then Canonical_Is (Chain_Node, Original), "canonical chain");
   Located := Resolve (Tree, Root, Path ("ALS1"));
   Check (Located.Status = Found and then Located.Node = Chain_Node, "resolve physical final name");
   Located := Resolve_Object (Tree, Root, Path ("ALS1"));
   Check (Located.Status = Found and then Located.Node = Original, "resolve object identity");
   Check (Value_Usage (Tree) = Value_Usage (Prior) and then Method_Usage (Tree) = Method_Usage (Prior), "aliases allocate no copied values");
   Set_Integer (Tree, Original, 41);
   Located := Resolve_Object (Tree, Root, Path ("ALS0"));
   Check (Located.Status = Found and then Integer_Data (Tree, Located.Node) = 41, "mutation visible through alias");
   Load_Names (Tree, Alias_Of (Bytes'[16#5C#, 0], Enc ("RALS")), Bits_64, Loaded);
   Check (Loaded = N.Loaded, "root alias load");
   Alias_Node := Child (Tree, Root, "RALS");
   Check (Alias_Node /= Root and then Kind (Tree, Alias_Node) = Alias_Object,
     "root alias has physical binding");
   Check (Canonical_Is (Alias_Node, Root), "root alias canonical found root");
   Located := Resolve_Object (Tree, Root, Path ("RALS"));
   Check (Located.Status = Found and then Located.Node = Root, "root object result distinct from missing");
   Located := Resolve_Object (Tree, Root, Path ("MISS"));
   Check (Located.Status = Not_Found, "missing object remains not found");
   Check (Kind (Tree, Root) = Scope_Object, "root physical kind remains scope");
   Located := Resolve_Object (Tree, Root, Path (Bytes'[0]));
   Check (Located.Status = Found and then Located.Node = Root, "legitimate root nullpath");
   Load_Names (Tree,
     Block ([16#5B#, 16#82#], Enc ("DEV0") & [16#08#] & Enc ("CHLD") & [1]
       & Alias_Of (Bytes'[16#5E#] & Enc ("NUM0"), Enc ("UPAL")))
       & Alias_Of ("DEV0", "ADV0")
       & Block ([16#10#], Enc ("ADV0") & [16#08#] & Enc ("NEW0") & [16#0A#, 4]), Bits_64, Loaded);
   Check (Loaded = N.Loaded, "aliased scope load");
   Device_Node := Child (Tree, Root, "DEV0"); Device_Alias := Child (Tree, Root, "ADV0");
   Check (Child (Tree, Device_Node, "NEW0") /= Root and then Child (Tree, Device_Alias, "NEW0") = Root, "declaration belongs to canonical scope");
   Located := Resolve (Tree, Root, Path (Bytes'[16#2E#] & Enc ("ADV0CHLD")));
   Check (Located.Status = Found and then Parent (Tree, Located.Node) = Device_Node, "intermediate alias traversal");
   Located := Resolve_Object (Tree, Device_Alias, Path ("CHLD"));
   Check (Located.Status = Found and then Parent (Tree, Located.Node) = Device_Node, "alias starting scope");
   Located := Resolve_Object (Tree, Device_Node, Path ("UPAL"));
   Check (Located.Status = Found and then Located.Node = Original, "parent-prefixed source");
   Located := Resolve_Object (Tree, Device_Alias, Path (Bytes'[16#5E#] & Enc ("ALS0")));
   Check (Located.Status = Found and then Located.Node = Original, "canonical parent prefix");
   Located := Resolve_Object (Tree, Device_Node, Path (Bytes'[16#5C#] & Enc ("ALS0")));
   Check (Located.Status = Found and then Located.Node = Original, "absolute alias lookup");
   Reject_Atomically (Alias_Of ("NUM0", "TMP0") & Alias_Of ("NUM0", "ALS0"),
     "later duplicate rolls back earlier candidate alias");
   Reject_Atomically (Alias_Of (Bytes'[16#FF#], Enc ("BAD0")), "malformed source name");
   Reject_Atomically (Alias_Of (Bytes'[16#5E#, 16#5E#] & Enc ("NUM0"), Enc ("BAD0")),
     "above root source prefix rejected atomically");
   Reject_Atomically (Alias_Of ("NUM0", "ALS0"), "duplicate alias spelling");
   Reject_Atomically ([16#08#] & Enc ("ALS0") & [1], "ordinary declaration duplicate alias");
   Reject_Atomically (Alias_Of ("NUM0", "NUM0"), "duplicate source spelling");
   Reject_Atomically (Alias_Of ("MISS", "BAD0"), "missing source atomic");
   Reject_Atomically (Alias_Of (Enc ("NUM0"), Bytes'[16#2E#] & Enc ("MISSBAD0")), "missing destination scope");
   for Cut in 1 .. 8 loop
      declare
         Full : constant Bytes := Alias_Of ("NUM0", "CUT0");
      begin
         Reject_Atomically ([16#08#] & Enc ("SIDE") & [1] & Full (1 .. Cut), "truncated alias rolls back prior candidate declaration");
      end;
   end loop;
   Reject_Atomically ([16#06#] & Enc ("NUM0") & [16#FF#], "malformed destination");
   Reject_Atomically (Alias_Of (Enc ("NUM0"), Bytes'[0]), "destination bare NullName rejected");
   Reject_Atomically (Alias_Of (Enc ("NUM0"), Bytes'[16#5C#, 0]), "destination rooted NullName rejected");
   declare
      Small : Tiny.State := Tiny.Empty;
      Saved : Tiny.State;
      Outcome : Tiny.Load_Status;
   begin
      Tiny.Load_Names (Small, Base & Alias_Of ("NUM0", "ALS0"), Bits_64, Outcome);
      Check (Outcome = Tiny.Loaded, "tiny fills exactly"); Saved := Small;
      Tiny.Load_Names (Small, Alias_Of ("ALS0", "ALS1"), Bits_64, Outcome);
      Check (Outcome /= Tiny.Loaded and then Small = Saved, "capacity failure atomic");
   end;
   declare
      Package_Tree : State := Empty;
      Members : Initialization_Report;
      Package_Node : Node_ID;
   begin
      Load_Names (Package_Tree, Alias_Of (Bytes'[16#5C#, 0], Enc ("RALS"))
        & [16#08#] & Enc ("RPKG") & Block ([16#12#], [1] & Enc ("RALS")), Bits_64, Loaded);
      Check (Loaded = N.Loaded and then Pending_Members (Package_Tree) = 1,
        "root alias package member initially deferred");
      Initialize_Members (Package_Tree, Members);
      Check (Members.Unsupported = 1 and then Members.Bound = 0 and then Members.Missing = 0
        and then Pending_Members (Package_Tree) = 0, "ownerless state cannot mint root metadata authority");
      Package_Node := Child (Package_Tree, Root, "RPKG");
      Check (Kind (Package_Tree, Package_Node) = Package_Object
        and then AML_Objects.Element (Value_Store (Package_Tree), Data_Object (Package_Tree, Package_Node), 0) = 0,
        "ownerless root package member remains uninitialized");
   end;
   -- Ordinary static root-package semantics require an owning arena. The
   -- pure State rejection above remains a distinct no-owner safety check.
   for Width in Integer_Width loop
      declare
         A, Foreign : N.Owned.Arena;
         Success : Boolean;
         Members : Initialization_Report;
         Metadata : AML_Execute.Reference_Metadata;
         Root_Identity : AML_References.Reference;
         Package_Code : constant Bytes := Alias_Of (Bytes'[16#5C#, 0], Enc ("RALS"))
           & [16#08#] & Enc ("RPKG") & Block ([16#12#], [1] & Enc ("RALS"));
      begin
         N.Owned.Reset (A, Success); Check (Success, "owned package arena initialized");
         N.Owned.Reset (Foreign, Success); Check (Success, "foreign ownership control");
         N.Owned.Load (A, Package_Code, Width, Loaded);
         Check (Loaded = N.Loaded, "owned static root package loaded");
         N.Owned.Initialize_Members (A, Members);
         Check (Members.Bound = 1 and then Members.Missing = 0 and then Members.Unsupported = 0,
           "owned root package member bound");
         declare
            View : constant State := N.Owned.Snapshot (A);
            Node : constant Node_ID := Child (View, Root, "RPKG");
            Leaf : constant AML_Objects.Object_ID := AML_Objects.Element
              (Value_Store (View), Data_Object (View, Node), 0);
         begin
            Check (Kind (View, Node) = Package_Object and then Leaf /= AML_Objects.No_Object
              and then AML_Objects.Kind (Value_Store (View), Leaf) = AML_Objects.Metadata_Object,
              "static package owns live metadata leaf rather than empty slot");
            Root_Identity := AML_Objects.Metadata_Data (Value_Store (View), Leaf);
            Check (AML_References.Kind (Root_Identity) = AML_References.Root_Namespace
              and then N.Owned.Named_Identity_Matches (A, Root_Identity)
              and then not N.Owned.Named_Identity_Matches (Foreign, Root_Identity),
              "package root metadata authenticates only in actual owner");
            N.Owned.Describe_Metadata_Value (A, (AML_Execute.Live_Namespace, Root_Identity), Metadata);
            Check (Metadata.Kind = AML_Execute.Metadata_Only
              and then Metadata.Object_Type = AML_Execute.Device_Metadata,
              "static root package metadata has Device type");
            Check (Kind (View, Root) = Scope_Object and then not N.Owned.Matches (A, Root_Identity),
              "metadata support does not grant generic root data authority");
         end;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Alias namespace checks" & Checks'Image);
end Alias_Namespace_Tests;
