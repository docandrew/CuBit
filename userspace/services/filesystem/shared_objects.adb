package body Shared_Objects with SPARK_Mode is
   pragma Assertion_Policy (Pre => Ignore, Post => Ignore, Ghost => Ignore,
                            Loop_Invariant => Ignore, Assert => Ignore);

   function Object_Of (S : State; Identity : Object_Key) return Link is
      First : constant Object_Index := Home (Identity);
   begin
      --  A live object of this key: in the window from First, every slot
      --  before it probed (Placed).
      pragma Assert
        (for all Ob in Object_Index =>
           (if S.Counts (Ob) > 0 and then S.Identities (Ob) = Identity then
              Ob >= First and then Ob - First < Probe_Window and then
              S.Probed (Ob) and then
              (for all Before in First .. Ob - 1 => S.Probed (Before))));
      for Distance in 0 .. Probe_Window - 1 loop
         if not S.Probed (First + Distance) then
            pragma Assert
              (for all Ob in Object_Index =>
                 (if S.Counts (Ob) > 0 then S.Identities (Ob) /= Identity));
            return 0;
         end if;
         if S.Counts (First + Distance) > 0 and then
           S.Identities (First + Distance) = Identity
         then
            return First + Distance;
         end if;
         pragma Loop_Invariant
           (for all Earlier in First .. First + Distance =>
              S.Probed (Earlier) and then
              not (S.Counts (Earlier) > 0 and then S.Identities (Earlier) = Identity));
      end loop;
      pragma Assert
        (for all Ob in Object_Index =>
           (if S.Counts (Ob) > 0 then S.Identities (Ob) /= Identity));
      return 0;
   end Object_Of;

   --  Valid implies isolation: a denying owner's object is live, has only
   --  it as holder, and no other live object has its key.
   procedure Lemma_Isolated (S : State)
     with Ghost, Pre => Valid (S), Post => Exclusive_Owners_Isolated (S)
   is
   begin
      pragma Assert
        (for all Owner in Owner_Index =>
           (if Exclusive (S, Owner) then
              S.Counts (S.Owners (Owner)) = 1 and then S.Places_Of (Owner) = 0 and then
              S.Holders (S.Owners (Owner)) (0) = Owner));
      pragma Assert
        (for all A in Owner_Index =>
           (for all B in Owner_Index =>
              (if Attached (S, A) and then Attached (S, B) and then Key (S, A) = Key (S, B)
               then S.Owners (A) = S.Owners (B))));
   end Lemma_Isolated;

   function Holder (S : State; Object : Object_Index; Index : Holder_Index)
      return Owner_Index is (S.Holders (Object) (Index));

   function Exclusively_Held (S : State; Identity : Object_Key) return Boolean is
      Object : constant Link := Object_Of (S, Identity);
   begin
      if Object = 0 then
         return False;
      end if;
      pragma Assert (S.Counts (Object) > 0);
      return S.Sharing (S.Holders (Object) (0)) = Deny_Sharing;
   end Exclusively_Held;

   procedure Attach
     (S : in out State; Owner : Owner_Index; Identity : Object_Key;
      Initial : Object_Value; Result : out Attach_Result;
      Mode : Sharing_Mode := Allow_Sharing)
   is
      Object : Link;
   begin
      if Attached (S, Owner) then
         Result := Owner_Busy;
         return;
      end if;
      Object := Object_Of (S, Identity);
      if Object /= 0 then
         if Mode = Deny_Sharing or else
           S.Sharing (S.Holders (Object) (0)) = Deny_Sharing
         then
            Result := Sharing_Conflict;
            return;
         elsif S.Counts (Object) = Max_Holders then
            Result := Full;
            return;
         end if;
         S.Holders (Object) (S.Counts (Object)) := Owner;
         S.Places_Of (Owner) := S.Counts (Object);
         S.Counts (Object) := S.Counts (Object) + 1;
         S.Owners (Owner) := Object;
         S.Sharing (Owner) := Mode;
         pragma Assert (Linked (S));
         pragma Assert (Placed (S));
         pragma Assert (Denials_Alone (S));
         Lemma_Isolated (S);
         Result := Shared;
         return;
      end if;
      --  A new object: the first slot of the key's window not live.
      declare
         First : constant Object_Index := Home (Identity);
      begin
         for Distance in 0 .. Probe_Window - 1 loop
            if S.Counts (First + Distance) = 0 then
               Object := First + Distance;
               exit;
            end if;
            pragma Loop_Invariant
              (for all Earlier in First .. First + Distance =>
                 S.Counts (Earlier) > 0);
         end loop;
      end;
      if Object = 0 then
         Result := Full;
         return;
      end if;
      S.Identities (Object) := Identity;
      S.Metadata (Object) := Initial;
      S.Holders (Object) (0) := Owner;
      S.Counts (Object) := 1;
      S.Probed (Object) := True;
      S.Places_Of (Owner) := 0;
      S.Owners (Owner) := Object;
      S.Sharing (Owner) := Mode;
      pragma Assert (Linked (S));
      pragma Assert (Placed (S));
      pragma Assert (Denials_Alone (S));
      Lemma_Isolated (S);
      Result := Created;
   end Attach;

   procedure Replace
     (S : in out State; Owner : Owner_Index; Item : Object_Value) is
   begin
      S.Metadata (S.Owners (Owner)) := Item;
   end Replace;

   procedure Detach (S : in out State; Owner : Owner_Index) is
      Object : constant Link := S.Owners (Owner);
   begin
      if Object = 0 then
         return;
      end if;
      declare
         Place : constant Holder_Index := S.Places_Of (Owner);
         Last : constant Holder_Index := S.Counts (Object) - 1;
         Moved : constant Owner_Index := S.Holders (Object) (Last);
      begin
         --  The last holder takes this one's place.
         S.Holders (Object) (Place) := Moved;
         S.Places_Of (Moved) := Place;
         S.Counts (Object) := Last;
         S.Owners (Owner) := 0;
         S.Sharing (Owner) := Allow_Sharing;
      end;
   end Detach;
end Shared_Objects;
