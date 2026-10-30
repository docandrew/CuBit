with AML_Identity;
with AML_Object_Identifiers;
with AML_Objects.Reclamation;
package AML_Objects.Root_Snapshots with SPARK_Mode, Pure is
   type Snapshot_Phase is (Vacant, Building, Published, Rejected);
   type Snapshot is private;
   type Result_Status is (Ready, Invalid_Owner, Owner_Mismatch, Wrong_Phase,
      Invalid_Address, Generation_Conflict, Incomplete_Snapshot, Invalid_Snapshot, Stale_Root);
   function Phase (Roots : Snapshot) return Snapshot_Phase;
   function Empty return Snapshot;
   -- Store-local metadata, not object authority. The producer must authenticate
   -- every source handle/descriptor before inclusion and must not allocate,
   -- collect or reenter while constructing a snapshot. Owner binding prevents
   -- resolving a snapshot against a different arena with colliding slot IDs.
   procedure Clear (Roots : out Snapshot) with Post => Roots = Empty;
   function Begun (Roots : Snapshot; Owner : AML_Identity.Identity) return Boolean with Ghost;
   procedure Begin_Build (Roots : out Snapshot; Owner : AML_Identity.Identity;
      Status : out Result_Status) with Post => Begun (Roots, Owner)
        and then Status = (if Phase (Roots) = Building then Ready else Invalid_Owner);
   function Included (After, Before : Snapshot; Owner : AML_Identity.Identity;
      Address : AML_Object_Identifiers.Object_Address) return Boolean with Ghost;
   function Invalidated (After, Before : Snapshot) return Boolean with Ghost;
   procedure Include (Roots : in out Snapshot; Owner : AML_Identity.Identity;
      Address : AML_Object_Identifiers.Object_Address; Status : out Result_Status)
      with Post => (if Status = Ready then Included (Roots, Roots'Old, Owner, Address)
        else Invalidated (Roots, Roots'Old));
   procedure Reject (Roots : in out Snapshot) with Post => Invalidated (Roots, Roots'Old);
   function Finished (After, Before : Snapshot; Owner : AML_Identity.Identity) return Boolean with Ghost;
   procedure Finish (Roots : in out Snapshot; Owner : AML_Identity.Identity;
      Status : out Result_Status) with Post =>
        (if Status = Ready then Finished (Roots, Roots'Old, Owner) else Invalidated (Roots, Roots'Old));
   function Validation (Roots : Snapshot; Owner : AML_Identity.Identity; Store : State)
      return Result_Status with Pre => Valid (Store);
   function Resolved (Roots : Snapshot; Owner : AML_Identity.Identity; Store : State;
      Keep : Reclamation.Keep_Set) return Boolean with Ghost, Pre => Valid (Store);
   -- This produces only seeds, NOT transitive closure or permission to reclaim.
   -- Every failure returns an empty mask; neither input is changed.
   procedure Resolve (Roots : Snapshot; Owner : AML_Identity.Identity; Store : State;
      Keep : out Reclamation.Keep_Set; Status : out Result_Status) with Pre => Valid (Store),
      Post => Status = Validation (Roots, Owner, Store) and then
        (if Status = Ready then Resolved (Roots, Owner, Store, Keep)
         else (for all ID in Keep'Range => not Keep (ID)));
private
   use type AML_Identity.Identity;
   type Stamp_Array is array (Object_ID range 1 .. Max_Objects)
      of AML_Object_Identifiers.Slot_Incarnation;
   type Snapshot is record
      Stage : Snapshot_Phase := Vacant;
      Arena : AML_Identity.Identity := AML_Identity.No_Identity;
      Stamps : Stamp_Array := [others => AML_Object_Identifiers.No_Incarnation];
   end record;
   function Phase (Roots : Snapshot) return Snapshot_Phase is (Roots.Stage);
end AML_Objects.Root_Snapshots;
