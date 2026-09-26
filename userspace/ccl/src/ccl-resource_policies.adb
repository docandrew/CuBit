package body CCL.Resource_Policies with SPARK_Mode is
   package T renames CCL.Types;
   package O renames CCL.Ownership;
   use type T.Shape;
   use type T.Name;
   use type O.Ownership_Mode;
   use type O.Disposition_Effect;
   use type O.Disposition_Id;

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
      Needed : Selection := Roots;
      Visited : Selection := [others => False];
      Next_Bindings : Binding_Map := [others => 0];
      Next_Definitions : O.Type_Table := [others => (others => <>)];
      Next_Count : Layout_Count := 1;
      Target : T.Type_Reference;
      Tag : O.Type_Id;
   begin
      Bindings := [others => 0]; Definitions := [others => (others => <>)]; Count := 0;
      Result := Invalid_Policy;
      -- Each productive pass visits at least one previously unvisited type.
      -- A fixed registry-sized pass bound covers reverse chains and cycles.
      for Pass in T.Type_Reference loop
         for Ref in T.Type_Reference loop
            if Needed (Ref) and then not Visited (Ref) then
               if Policies (Ref).Mode = O.Unrestricted then Result := Missing_Policy; return; end if;
               if not Valid (Types, Ref, Policies (Ref)) then return; end if;
               Visited (Ref) := True;
               for I in 0 .. Policies (Ref).Count - 1 loop
                  if Policies (Ref).Dispositions (I).Effect = O.Transition then
                     Target := T.Find (Types, Policies (Ref).Dispositions (I).Next_Type);
                     Needed (Target) := True;
                  end if;
               end loop;
            end if;
         end loop;
      end loop;
      for Ref in T.Type_Reference loop
         if Needed (Ref) then
            if Next_Count = O.MAX_TYPES then Result := Too_Many_Types; return; end if;
            Next_Bindings (Ref) := O.Type_Id (Next_Count);
            Next_Count := Next_Count + 1;
         end if;
      end loop;
      for Ref in T.Type_Reference loop
         if Needed (Ref) then
            Tag := Next_Bindings (Ref);
            Next_Definitions (Tag).Mode := Policies (Ref).Mode;
            Next_Definitions (Tag).Dispositions_Length := Policies (Ref).Count;
            for I in 0 .. Policies (Ref).Count - 1 loop
               Next_Definitions (Tag).Dispositions (I) :=
                 (Verb => Policies (Ref).Dispositions (I).Verb,
                  Effect => Policies (Ref).Dispositions (I).Effect, Next_Type => 0);
               if Policies (Ref).Dispositions (I).Effect = O.Transition then
                  Target := T.Find (Types, Policies (Ref).Dispositions (I).Next_Type);
                  Next_Definitions (Tag).Dispositions (I).Next_Type := Next_Bindings (Target);
               end if;
            end loop;
         end if;
      end loop;
      Bindings := Next_Bindings; Definitions := Next_Definitions;
      Count := Next_Count; Result := Ready;
   end Layout;
end CCL.Resource_Policies;
