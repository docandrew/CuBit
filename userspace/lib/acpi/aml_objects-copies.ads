pragma Ada_2022;
package AML_Objects.Copies with SPARK_Mode, Pure is
   type Copy_Witness is private;
   function Valid_Witness
     (Store, Prior : State; Source, Target : Object_ID; Witness : Copy_Witness)
      return Boolean with Ghost,
      Pre => Valid (Store) and then Valid (Prior)
        and then Is_Live (Prior, Source);
   -- Checks the complete value tree and disjoint per-occurrence ownership.
   -- Fresh objects may reuse holes; the witness records actual slot identities.
   -- Each nonzero package occurrence owns a distinct fresh object.
   function Is_Independent_Copy
     (Store, Prior : State; Source, Target : Object_ID; Witness : Copy_Witness) return Boolean
   with Ghost,
     Pre => Valid (Store) and then Valid (Prior)
       and then Is_Live (Prior, Source),
     Post => (if Valid_Witness (Store, Prior, Source, Target, Witness) then Is_Independent_Copy'Result);
   -- Each package occurrence owns a separate copy, including repeated links.
   -- Reference, live metadata, and captured metadata are distinct leaves: fresh wrappers retain
   -- their original kind and exactly the same typed
   -- referent identity or captured descriptor. No target is read, copied, authenticated or dereferenced.
   -- A cyclic graph cannot fit a finite tree: allocation limits reject it.
   -- Mutex copying is unsupported, including Mutex leaves inside packages.
   -- Failure is transactional, including all backing storage and counters.
   procedure Clone
     (Store : in out State; Source : Object_ID;
      Target : out Object_ID; Status : out Allocation_Status; Witness : out Copy_Witness)
   with Pre => Valid (Store) and then Is_Live (Store, Source),
     Post => Valid (Store) and then Extends (Store, Store'Old)
       and then (if Status = Allocated then
         Valid_Witness (Store, Store'Old, Source, Target, Witness)
         and then Is_Independent_Copy (Store, Store'Old, Source, Target, Witness)
         and then Is_Live (Store, Target) and then not Is_Live (Store'Old, Target)
         and then Kind (Store, Target) = Kind (Store'Old, Source)
         and then Length (Store, Target) = Length (Store'Old, Source)
         and then (if Kind (Store'Old, Source) = Integer_Object then
           Integer_Data (Store, Target) = Integer_Data (Store'Old, Source)
           and then Origin_Of (Store, Target) = Origin_Of (Store'Old, Source))
         and then (if Kind (Store'Old, Source) = Event_Object then Event_Units (Store, Target) = 0)
         and then (if Kind (Store'Old, Source) = Reference_Object then
           Reference_Data (Store, Target) = Reference_Data (Store'Old, Source))
         and then (if Kind (Store'Old, Source) = Metadata_Object then
           Metadata_Data (Store, Target) = Metadata_Data (Store'Old, Source))
         and then (if Kind (Store'Old, Source) = Metadata_Snapshot_Object then
           Metadata_Snapshot_Data (Store, Target) = Metadata_Snapshot_Data (Store'Old, Source))
         and then (if Kind (Store'Old, Source) in Byte_Kind then
           Byte_Data (Store, Target) = Byte_Data (Store'Old, Source))
       else Store = Store'Old and then Target = No_Object);
private
   type Copy_Link is record
      Original, Copy, Parent : Object_ID := 0;
      Slot : Natural range 0 .. Max_Elements - 1 := 0;
      Bytes_After : Natural range 0 .. Max_Bytes := 0;
      Elements_After : Natural range 0 .. Max_Elements := 0;
   end record;
   type Link_Array is array (Positive range 1 .. Max_Objects) of Copy_Link;
   type Copy_Witness is record
      Used : Object_ID := 0;
      Links : Link_Array := [others => <>];
   end record;
   function Header_Matches (Store, Prior : State; Original, Copy : Object_ID) return Boolean is
     (Is_Live (Prior, Original)
      and then Kind (Prior, Original) /= Mutex_Object
      and then Is_Live (Store, Copy) and then not Is_Live (Prior, Copy)
      and then Kind (Store, Copy) = Kind (Prior, Original)
      and then Length (Store, Copy) = Length (Prior, Original)
      and then (if Kind (Prior, Original) = Integer_Object then
        Integer_Data (Store, Copy) = Integer_Data (Prior, Original)
        and then Origin_Of (Store, Copy) = Origin_Of (Prior, Original))
      and then (if Kind (Prior, Original) = Event_Object then Event_Units (Store, Copy) = 0)
      and then (if Kind (Prior, Original) = Reference_Object then
        Reference_Data (Store, Copy) = Reference_Data (Prior, Original))
      and then (if Kind (Prior, Original) = Metadata_Object then
        Metadata_Data (Store, Copy) = Metadata_Data (Prior, Original))
      and then (if Kind (Prior, Original) = Metadata_Snapshot_Object then
        Metadata_Snapshot_Data (Store, Copy) = Metadata_Snapshot_Data (Prior, Original))
      and then (if Kind (Prior, Original) in Byte_Kind then
        Byte_Data (Store, Copy) = Byte_Data (Prior, Original)))
     with Ghost, Pre => Valid (Store) and then Valid (Prior);
   -- Exact arena frame shared by the two independent tree validators: old
   -- records/backing are retained, only fresh incarnations become live, and
   -- unused slots and backing tails remain exact.
   function Copy_Frame (Store, Prior : State) return Boolean with Ghost,
     Pre => Valid (Store) and then Valid (Prior);
end AML_Objects.Copies;
