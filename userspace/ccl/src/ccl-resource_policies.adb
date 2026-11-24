package body CCL.Resource_Policies with SPARK_Mode is
   package T renames CCL.Types;
   package O renames CCL.Ownership;
   use type T.Shape;
   use type T.Name;
   use type O.Ownership_Mode;
   use type O.Disposition_Effect;
   use type O.Disposition_Id;

   function Ref_Of (Item : Policy_Entry) return T.Type_Reference is (Item.Ref);
   function Same_Ref (Left, Right : T.Type_Reference) return Boolean is (Left = Right);
   function Search_Ref is new Entry_Tables.Search
     (Key => T.Type_Reference, Key_Of => Ref_Of, Same_Key => Same_Ref);

   function Policy (Table : Policy_Table; Ref : T.Type_Reference) return Description is
      Found : constant Entry_Tables.Count := Search_Ref (Table.Entries, Ref);
   begin
      return (if Found = 0 then No_Policy else Entry_Tables.Get (Table.Entries, Found).Value);
   end Policy;

   procedure Set_Policy
     (Table : in out Policy_Table; Ref : T.Type_Reference; Value : Description;
      Done : out Boolean)
   is
      Found : constant Entry_Tables.Count := Search_Ref (Table.Entries, Ref);
   begin
      if Found = 0 then
         Entry_Tables.Append (Table.Entries, (Ref, Value), Done);
      else
         Entry_Tables.Replace (Table.Entries, Found, (Ref, Value), Done);
      end if;
   end Set_Policy;

   function Valid (Types : T.Registry; Root : T.Type_Reference; Policy : Description) return Boolean is
      Empty : constant Disposition := (others => <>);
      Target : T.Type_Reference;
   begin
      if not T.Known (Types, Root) or else T.Describe (Types, Root).Form /= T.Resource or else
        Policy.Mode = O.Unrestricted then return False; end if;
      for I in Policy.Dispositions'Range loop
         if I >= Policy.Count then
            if Policy.Dispositions (I) /= Empty then return False; end if;
         else
            if Policy.Dispositions (I).Verb = 0 then return False; end if;
            for J in 0 .. I - 1 loop
               if Policy.Dispositions (I).Verb = Policy.Dispositions (J).Verb then return False; end if;
            end loop;
            if Policy.Dispositions (I).Effect = O.Transition then
               if not T.Valid_Name (Policy.Dispositions (I).Next_Type) or else
                 Policy.Dispositions (I).Next_Type /= T.Named (T.Image (Policy.Dispositions (I).Next_Type))
               then return False; end if;
               Target := T.Find (Types, Policy.Dispositions (I).Next_Type);
               if not T.Known (Types, Target) or else T.Describe (Types, Target).Form /= T.Resource then return False; end if;
            elsif Policy.Dispositions (I).Next_Type /= Empty.Next_Type then return False;
            end if;
         end if;
      end loop;
      return True;
   end Valid;

   procedure Layout
     (Types : T.Registry; Policies : Policy_Table; Roots : Selection;
      Bindings : out Binding_Map; Definitions : out O.Type_Table;
      Count : out Layout_Count; Result : out Layout_Result)
   is
      subtype Reference is T.Type_Reference range 0 .. T.Last (Types);
      Needed : Selection (Reference) := [others => False];
      Visited : Selection (Reference) := [others => False];
      Next_Bindings : Binding_Map (Reference) := [others => 0];
      Next_Definitions : O.Type_Table := [others => (others => <>)];
      Next_Count : Layout_Count := 1;
      Target : T.Type_Reference;
      Tag : O.Type_Id;
      Found : Boolean;
      Current : Description;
   begin
      Bindings := [others => 0]; Definitions := [others => (others => <>)]; Count := 0;
      Result := Missing_Policy;
      for Ref in Roots'Range loop
         if Roots (Ref) then
            if Ref not in Reference then return; end if;
            Needed (Ref) := True;
         end if;
      end loop;
      Result := Invalid_Policy;
      --  Each pass that finds work visits at least one type not visited
      --  before, so one pass per type and a last one that finds nothing
      --  cover reverse chains and cycles.
      for Pass in Reference loop
         Found := False;
         for Ref in Reference loop
            if Needed (Ref) and then not Visited (Ref) then
               Found := True;
               Current := Policy (Policies, Ref);
               if Current.Mode = O.Unrestricted then Result := Missing_Policy; return; end if;
               if not Valid (Types, Ref, Current) then return; end if;
               Visited (Ref) := True;
               for I in 0 .. Current.Count - 1 loop
                  if Current.Dispositions (I).Effect = O.Transition then
                     --  Valid: the target is a known type of Types.
                     Target := T.Find (Types, Current.Dispositions (I).Next_Type);
                     if Target in Reference then Needed (Target) := True; end if;
                  end if;
               end loop;
            end if;
         end loop;
         exit when not Found;
      end loop;
      for Ref in Reference loop
         if Needed (Ref) then
            if Next_Count = O.MAX_TYPES then Result := Too_Many_Types; return; end if;
            Next_Bindings (Ref) := O.Type_Id (Next_Count);
            Next_Count := Next_Count + 1;
         end if;
      end loop;
      for Ref in Reference loop
         if Needed (Ref) then
            Tag := Next_Bindings (Ref);
            Current := Policy (Policies, Ref);
            Next_Definitions (Tag).Mode := Current.Mode;
            Next_Definitions (Tag).Dispositions_Length := Current.Count;
            for I in 0 .. Current.Count - 1 loop
               Next_Definitions (Tag).Dispositions (I) :=
                 (Verb => Current.Dispositions (I).Verb,
                  Effect => Current.Dispositions (I).Effect, Next_Type => 0);
               if Current.Dispositions (I).Effect = O.Transition then
                  Target := T.Find (Types, Current.Dispositions (I).Next_Type);
                  if Target in Reference then
                     Next_Definitions (Tag).Dispositions (I).Next_Type := Next_Bindings (Target);
                  end if;
               end if;
            end loop;
         end if;
      end loop;
      for Ref in Bindings'Range loop
         if Ref in Reference then Bindings (Ref) := Next_Bindings (Ref); end if;
      end loop;
      Definitions := Next_Definitions;
      Count := Next_Count; Result := Ready;
   end Layout;
end CCL.Resource_Policies;
