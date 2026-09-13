-- Architecture-neutral, constant-time link writes. The caller supplies
-- distinct link fields, not distinct neighboring nodes: in a singleton list
-- the previous and next nodes are the SAME sentinel, but its two fields differ.
generic
   type Reference is private;
package Intrusive_List_Splices with SPARK_Mode, Pure is
   procedure Insert_Front
     (Head_Next : in out Reference;
      First_Previous, New_Previous, New_Next : out Reference;
      Head, Item : Reference) with Inline_Always,
     Pre => Item /= Head and then Item /= Head_Next,
     Post => Head_Next = Item and then First_Previous = Item
       and then New_Previous = Head and then New_Next = Head_Next'Old;

   procedure Remove
     (Previous_Next, Next_Previous : out Reference;
      Previous, Following : Reference) with Inline_Always,
     Post => Previous_Next = Following and then Next_Previous = Previous;

   -- Keep count expressions separate from by-copy link parameters, allowing
   -- an in-place machine increment/decrement at the caller.
   function Added (Count : Natural) return Natural is (Count + 1)
     with Inline_Always, Pre => Count < Natural'Last,
       Post => Added'Result = Count + 1;
   function Removed (Count : Natural) return Natural is (Count - 1)
     with Inline_Always, Pre => Count > 0, Post => Removed'Result = Count - 1;

   -- These are algebraic composition checks of the actual operations, not
   -- a substitute implementation. Mapping fields to real nodes is a separate
   -- caller obligation, as are membership and exclusion of live allocations.
   procedure Prove_Insert_Remove
     (Head, Item, First : Reference; Count : Natural) with Ghost,
     Pre => Count < Natural'Last and then Item /= Head and then Item /= First;
   procedure Prove_Singleton_Removal (Head, Item : Reference) with Ghost;
end Intrusive_List_Splices;
