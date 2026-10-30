package body AML_Objects.Root_Snapshots with SPARK_Mode is
   package IDs renames AML_Object_Identifiers;
   function Empty return Snapshot is (others => <>);
   procedure Clear (Roots : out Snapshot) is
   begin Roots := Empty; end Clear;
   function Begun (Roots : Snapshot; Owner : AML_Identity.Identity) return Boolean is
     (Roots = Snapshot'(Stage => (if Owner = AML_Identity.No_Identity then Rejected else Building),
       Arena => Owner, others => <>));
   procedure Begin_Build (Roots : out Snapshot; Owner : AML_Identity.Identity;
      Status : out Result_Status) is
   begin
      Roots := (Stage => (if Owner = AML_Identity.No_Identity then Rejected else Building),
        Arena => Owner, others => <>);
      Status := (if Owner = AML_Identity.No_Identity then Invalid_Owner else Ready);
   end Begin_Build;
   function Invalidated (After, Before : Snapshot) return Boolean is
     (After = (Before with delta Stage => Rejected));
   procedure Reject (Roots : in out Snapshot) is
   begin Roots.Stage := Rejected; end Reject;
   function Included (After, Before : Snapshot; Owner : AML_Identity.Identity;
      Address : IDs.Object_Address) return Boolean is
     (Owner /= AML_Identity.No_Identity and then Before.Arena = Owner and then Before.Stage = Building
      and then IDs.Present (Address)
      and then (Before.Stamps (IDs.Slot_Of (Address)) = IDs.No_Incarnation
        or else Before.Stamps (IDs.Slot_Of (Address)) = IDs.Incarnation_Of (Address))
      and then After = (Before with delta Stamps =>
        (Before.Stamps with delta IDs.Slot_Of (Address) => IDs.Incarnation_Of (Address))));
   procedure Include (Roots : in out Snapshot; Owner : AML_Identity.Identity;
      Address : IDs.Object_Address; Status : out Result_Status) is
   begin
      if Owner = AML_Identity.No_Identity then Status := Invalid_Owner;
      elsif Roots.Arena /= Owner then Status := Owner_Mismatch;
      elsif Roots.Stage /= Building then Status := Wrong_Phase;
      elsif not IDs.Present (Address) then Status := Invalid_Address;
      elsif Roots.Stamps (IDs.Slot_Of (Address)) /= IDs.No_Incarnation
        and then Roots.Stamps (IDs.Slot_Of (Address)) /= IDs.Incarnation_Of (Address)
      then Status := Generation_Conflict;
      else
         Roots.Stamps (IDs.Slot_Of (Address)) := IDs.Incarnation_Of (Address);
         Status := Ready; return;
      end if;
      Reject (Roots);
   end Include;
   function Finished (After, Before : Snapshot; Owner : AML_Identity.Identity) return Boolean is
     (Owner /= AML_Identity.No_Identity and then Before.Arena = Owner and then Before.Stage = Building
      and then After = (Before with delta Stage => Published));
   procedure Finish (Roots : in out Snapshot; Owner : AML_Identity.Identity;
      Status : out Result_Status) is
   begin
      if Owner = AML_Identity.No_Identity then Status := Invalid_Owner;
      elsif Roots.Arena /= Owner then Status := Owner_Mismatch;
      elsif Roots.Stage /= Building then Status := Wrong_Phase;
      else Roots.Stage := Published; Status := Ready; return;
      end if;
      Reject (Roots);
   end Finish;
   function Validation (Roots : Snapshot; Owner : AML_Identity.Identity; Store : State)
      return Result_Status is
   begin
      if Owner = AML_Identity.No_Identity then return Invalid_Owner;
      elsif Roots.Stage = Vacant then return (if Roots = Empty then Ready else Invalid_Snapshot);
      elsif Roots.Arena /= Owner then return Owner_Mismatch;
      elsif Roots.Stage = Building then return Incomplete_Snapshot;
      elsif Roots.Stage = Rejected then return Invalid_Snapshot;
      end if;
      for ID in Roots.Stamps'Range loop
         if Roots.Stamps (ID) /= IDs.No_Incarnation and then
           (not Is_Live (Store, ID) or else Last_Incarnation (Store, ID) /= Roots.Stamps (ID))
         then return Stale_Root; end if;
      end loop;
      return Ready;
   end Validation;
   function Resolved (Roots : Snapshot; Owner : AML_Identity.Identity; Store : State;
      Keep : Reclamation.Keep_Set) return Boolean is
     (Validation (Roots, Owner, Store) = Ready and then
       (for all ID in Keep'Range => Keep (ID) = (Roots.Stamps (ID) /= IDs.No_Incarnation)
         and then (if Keep (ID) then Is_Live (Store, ID)
           and then Last_Incarnation (Store, ID) = Roots.Stamps (ID))));
   procedure Resolve (Roots : Snapshot; Owner : AML_Identity.Identity; Store : State;
      Keep : out Reclamation.Keep_Set; Status : out Result_Status) is
   begin
      Keep := [others => False];
      Status := Validation (Roots, Owner, Store);
      if Status /= Ready then return; end if;
      for ID in Keep'Range loop Keep (ID) := Roots.Stamps (ID) /= IDs.No_Incarnation; end loop;
   end Resolve;
end AML_Objects.Root_Snapshots;
