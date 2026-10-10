with AML_Identity;
package AML_Objects.Reclamation with SPARK_Mode, Pure is
   type Keep_Set is array (Object_ID range 1 .. Max_Objects) of Boolean;
   type Workspace is limited private;
   type Reclaim_Status is (Reclaimed, Invalid_Owner, Invalid_Keep_Set,
     Unclosed_Package, Unclosed_Container_Reference, Storage_Limit);
   -- Caller supplies the authenticated complete root closure. This unit can
   -- validate package/container edges, not namespace or frame referents.
   function Reclaimed_State (Store, Prior : State; Keep : Keep_Set) return Boolean
     with Ghost, Pre => Valid (Prior);
   procedure Reclaim
     (Store : in out State; Owner : AML_Identity.Identity; Keep : Keep_Set;
      Scratch : in out Workspace; Status : out Reclaim_Status)
     with Pre => Valid (Store),
       Post => Valid (Store) and then
         (if Status = Reclaimed then Reclaimed_State (Store, Store'Old, Keep)
          else Store = Store'Old);
private
   type Workspace is limited record
      Objects : Object_Array;
      Bytes : AML_Decode.Bytes (1 .. Max_Bytes) := [others => 0];
      Elements : Element_Array := [others => 0];
   end record;
end AML_Objects.Reclamation;
