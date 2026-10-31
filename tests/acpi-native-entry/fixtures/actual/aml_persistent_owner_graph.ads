with AML_Objects.Reachability;
package AML_Persistent_Owner_Graph with SPARK_Mode, Pure is
   subtype Object_Set is AML_Objects.Reachability.Keep_Set;
   subtype Source_Index is AML_Objects.Object_ID range 1 .. AML_Objects.Max_Objects;
   type Result is (Invalid_Reachable_Edge, Incomplete_Census, Owner_Found, Owner_Not_Found);
   -- Closure masks only: caller authenticates source, roots and reference edges.
   -- A successful boolean closure is not itself a complete semantic census.
   function Classify (Keep, Invalid, Ambiguous : Object_Set;
      Source : Source_Index; Incomplete : Boolean) return Result
     with Post => Classify'Result =
       (if (for some ID in Keep'Range => Keep (ID) and then Invalid (ID))
        then Invalid_Reachable_Edge
        elsif Incomplete or else
          (for some ID in Keep'Range => Keep (ID) and then Ambiguous (ID))
        then Incomplete_Census
        elsif Keep (Source) then Owner_Found else Owner_Not_Found);
end AML_Persistent_Owner_Graph;
