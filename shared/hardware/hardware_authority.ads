with Interfaces; use Interfaces;
-- Pure rules for one named resource, shared by hardware classes. A value of
-- this type is NOT a kernel capability: only trusted catalog/cspace owners may
-- install it. Endpoint identity, incarnation, epoch, membership and revocation
-- checks remain mandatory outside this unit. No addresses or widths occur here.
package Hardware_Authority with Pure, SPARK_Mode is
   type Permission is record
      Resource_ID : Unsigned_64 := 0;
      Readable, Writable, Delegable : Boolean := False;
      Write_Mask : Unsigned_64 := 0;
   end record;
   function Valid (Item : Permission) return Boolean is
     (Item.Resource_ID /= 0 and then (Item.Readable or Item.Writable)
      and then (Item.Writable or Item.Write_Mask = 0));
   function Permits
     (Item : Permission; Resource_ID : Unsigned_64;
      For_Write : Boolean; Value : Unsigned_64) return Boolean is
     (Valid (Item) and then Resource_ID = Item.Resource_ID
      and then (if For_Write then Item.Writable and then
        (Value and not Item.Write_Mask) = 0
        else Item.Readable and then Value = 0));
   -- Exact requested rights must fit. Never silently broaden, retarget, or
   -- turn an ordinary endpoint tag-changing mint into hardware delegation.
   function Is_Subset (Parent, Child : Permission) return Boolean is
     (Valid (Parent) and then Valid (Child)
      and then Child.Resource_ID = Parent.Resource_ID
      and then (not Child.Readable or Parent.Readable)
      and then (not Child.Writable or Parent.Writable)
      and then (not Child.Delegable or Parent.Delegable)
      and then (Child.Write_Mask and not Parent.Write_Mask) = 0);
   -- Using previously installed authority does not require RIGHT_GRANT.
   -- Creating a child does: narrowing and delegation are distinct checks.
   function Can_Derive (Parent, Child : Permission) return Boolean is
     (Parent.Delegable and then Is_Subset (Parent, Child));
   function Subset_Preserves_Access
     (Parent, Child : Permission; Resource_ID : Unsigned_64;
      For_Write : Boolean; Value : Unsigned_64) return Boolean is
     (not Is_Subset (Parent, Child) or else
      not Permits (Child, Resource_ID, For_Write, Value) or else
      Permits (Parent, Resource_ID, For_Write, Value))
     with Ghost, Post => Subset_Preserves_Access'Result;
   function Derivation_Preserves_Access
     (Parent, Child : Permission; Resource_ID : Unsigned_64;
      For_Write : Boolean; Value : Unsigned_64) return Boolean is
     (not Can_Derive (Parent, Child) or else
      not Permits (Child, Resource_ID, For_Write, Value) or else
      Permits (Parent, Resource_ID, For_Write, Value))
     with Ghost, Post => Derivation_Preserves_Access'Result;
end Hardware_Authority;
