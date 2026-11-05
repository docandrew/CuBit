with AML_Identity;
package AML_Objects.Reclamation with SPARK_Mode, Pure is
   type Keep_Set is array (Object_ID range <>) of Boolean;
   type Workspace (Object_Capacity : Storage_Capacity := Max_Objects;
      Byte_Capacity : Storage_Capacity := Max_Bytes;
      Element_Capacity : Storage_Capacity := Max_Elements) is limited private;
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
   type Workspace (Object_Capacity : Storage_Capacity := Max_Objects;
      Byte_Capacity : Storage_Capacity := Max_Bytes;
      Element_Capacity : Storage_Capacity := Max_Elements) is limited record
      Objects : Object_Array (1 .. Object_Capacity);
      Bytes : AML_Decode.Bytes (1 .. Byte_Capacity) := [others => 0];
      Elements : Element_Array (1 .. Element_Capacity) := [others => 0];
   end record;
end AML_Objects.Reclamation;
