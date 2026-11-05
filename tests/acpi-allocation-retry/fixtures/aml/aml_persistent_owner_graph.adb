package body AML_Persistent_Owner_Graph with SPARK_Mode is
   function Classify (Keep, Invalid, Ambiguous : Object_Set;
      Source : Source_Index; Incomplete : Boolean) return Result
   is
      Missing : Boolean := Incomplete;
   begin
      for ID in Keep'Range loop
         if Keep (ID) then
            if Invalid (ID) then return Invalid_Reachable_Edge; end if;
            Missing := Missing or Ambiguous (ID);
         end if;
         pragma Loop_Invariant
           (for all Seen in Keep'First .. ID =>
              not (Keep (Seen) and then Invalid (Seen)));
         pragma Loop_Invariant
           (Missing = (Incomplete or else
              (for some Seen in Keep'First .. ID =>
                 Keep (Seen) and then Ambiguous (Seen))));
      end loop;
      if Missing then return Incomplete_Census;
      elsif Keep (Source) then return Owner_Found;
      else return Owner_Not_Found;
      end if;
   end Classify;
end AML_Persistent_Owner_Graph;
