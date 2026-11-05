package body AML_Objects.Root_Snapshots with SPARK_Mode is
   package IDs renames AML_Object_Identifiers;
   function Empty (Object_Capacity : Storage_Capacity := Max_Objects) return Snapshot is
     (Object_Capacity => Object_Capacity, others => <>);
   function Empty_Header return Header is (others => <>);
   function Header_Of (Roots : Snapshot) return Header is
     (Stage => Roots.Stage, Arena => Roots.Arena);
   function Stamp_At (Roots : Snapshot; Index : Positive)
      return IDs.Slot_Incarnation is (Roots.Stamps (Index));
   function From_Parts (Metadata : Header; Stamps : Stamp_Vector) return Snapshot is
     (Object_Capacity => Stamps'Length, Stage => Metadata.Stage,
      Arena => Metadata.Arena, Stamps => Stamps);
   procedure Clear (Roots : out Snapshot) is
   begin Roots := Empty (Roots.Object_Capacity); end Clear;
   function Begun (Roots : Snapshot; Owner : AML_Identity.Identity) return Boolean is
     (Roots = Snapshot'(Object_Capacity => Roots.Object_Capacity, Stage => (if Owner = AML_Identity.No_Identity then Rejected else Building),
       Arena => Owner, others => <>));
   procedure Begin_Build (Roots : out Snapshot; Owner : AML_Identity.Identity;
      Status : out Result_Status) is
   begin
      Roots := (Object_Capacity => Roots.Object_Capacity, Stage => (if Owner = AML_Identity.No_Identity then Rejected else Building),
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
      and then IDs.Present (Address) and then IDs.Slot_Of (Address) in Before.Stamps'Range
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
      elsif not IDs.Present (Address) or else IDs.Slot_Of (Address) not in Roots.Stamps'Range
      then Status := Invalid_Address;
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
      elsif Roots.Object_Capacity /= Store.Object_Capacity then return Invalid_Shape;
      elsif Roots.Stage = Vacant then return (if Roots = Empty (Roots.Object_Capacity) then Ready else Invalid_Snapshot);
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
     (Keep'First = 1 and then Keep'Last = Store.Object_Capacity
      and then Validation (Roots, Owner, Store) = Ready and then
       (for all ID in Keep'Range => Keep (ID) = (Roots.Stamps (ID) /= IDs.No_Incarnation)
         and then (if Keep (ID) then Is_Live (Store, ID)
           and then Last_Incarnation (Store, ID) = Roots.Stamps (ID))));
   procedure Resolve (Roots : Snapshot; Owner : AML_Identity.Identity; Store : State;
      Keep : out Reclamation.Keep_Set; Status : out Result_Status) is
   begin
      Keep := [others => False];
      if Keep'First /= 1 or else Keep'Last /= Store.Object_Capacity then
         Status := Invalid_Shape; return;
      end if;
      Status := Validation (Roots, Owner, Store);
      if Status /= Ready then return; end if;
      for ID in Keep'Range loop Keep (ID) := Roots.Stamps (ID) /= IDs.No_Incarnation; end loop;
   end Resolve;
end AML_Objects.Root_Snapshots;
