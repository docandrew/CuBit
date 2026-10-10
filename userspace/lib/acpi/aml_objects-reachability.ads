with AML_Object_Identifiers;
with AML_Objects.Reclamation;
package AML_Objects.Reachability with SPARK_Mode, Pure is
   subtype Keep_Set is Reclamation.Keep_Set;
   -- Store-local edges, NOT authority. The owner must authenticate each
   -- descriptor against this same store snapshot before calling Trace; do not
   -- reuse this map after any store mutation. No_Address means
   -- no object edge. Entries for non-reference objects are ignored. A stale
   -- generation never keeps a replacement object alive.
   type Reference_Targets is array (Object_ID range 1 .. Max_Objects)
      of AML_Object_Identifiers.Object_Address;
   type Workspace is limited private;
   type Trace_Status is (Traced, Invalid_Seed);
   -- A discovery witness proves both closure and absence of unrelated objects:
   -- each non-seed has an edge from an earlier discovered object.
   function Exact_Closure
     (Store : State; Seeds : Keep_Set; Targets : Reference_Targets;
      Keep : Keep_Set; Scratch : Workspace) return Boolean
     with Ghost, Pre => Valid (Store);
   procedure Trace
     (Store : State; Seeds : Keep_Set; Targets : Reference_Targets;
      Scratch : in out Workspace; Keep : out Keep_Set; Status : out Trace_Status)
     with Pre => Valid (Store),
       Post => (if Status = Traced then Exact_Closure (Store, Seeds, Targets, Keep, Scratch)
         else (for all ID in Keep'Range => not Keep (ID))
           and then (for some ID in Seeds'Range => Seeds (ID) and then not Is_Live (Store, ID)));
private
   type Object_List is array (Object_ID range 1 .. Max_Objects) of Object_ID;
   type Workspace is limited record
      Queue : Object_List := [others => No_Object];
      Rank : Object_List := [others => No_Object];
      Parent : Object_List := [others => No_Object];
      Used : Object_ID := 0;
   end record;
end AML_Objects.Reachability;
