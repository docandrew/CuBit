-- Authoritative block-head state, stored outside allocatable payload memory.
-- Physical placement, locking and agreement with intrusive links are adapter
-- obligations; these are the actual state transitions used by that adapter.
package Buddy_Blocks with SPARK_Mode, Pure is
   Descriptor_Bits : constant := 16;
   type Order is range 0 .. 39;
   type Block_Kind is
     (Reserved, Interior, Detached, Listed, Allocated, Retiring);
   for Block_Kind use
     (Reserved => 0, Interior => 1, Detached => 2, Listed => 3,
      Allocated => 4, Retiring => 5);
   type Descriptor is private;
   function Kind (Item : Descriptor) return Block_Kind;
   function Block_Order (Item : Descriptor) return Order;
   function Matches (Item : Descriptor; Expected : Block_Kind; Size : Order)
     return Boolean is (Kind (Item) = Expected and Block_Order (Item) = Size);

   -- Boot admission is deliberately not a runtime free operation.
   procedure Admit (Item : in out Descriptor; Size : Order; Head : Boolean;
                    Success : out Boolean) with
     Post => Success = (Kind (Item'Old) = Reserved) and then
       (if Success then Matches (Item, (if Head then Detached else Interior),
                                 (if Head then Size else 0))
        else Item = Item'Old);

   type Transition is (Publish, Remove, Commit, Release_Block, Defer, Reclaim);
   function Source (Action : Transition) return Block_Kind is
     (case Action is
        when Publish | Commit => Detached,
        when Remove => Listed,
        when Release_Block | Defer => Allocated,
        when Reclaim => Retiring);
   function Target (Action : Transition) return Block_Kind is
     (case Action is
        when Publish => Listed,
        when Commit => Allocated,
        when Defer => Retiring,
        when Remove | Release_Block | Reclaim => Detached);
   procedure Move (Item : in out Descriptor; Size : Order;
                   Action : Transition; Success : out Boolean) with
     Post => Success = Matches (Item'Old, Source (Action), Size) and then
       (if Success then Matches (Item, Target (Action), Size)
        else Item = Item'Old);

   procedure Split (Left, Right : in out Descriptor; Size : Order;
                    Success : out Boolean) with
     Post => Success = (Size > 0 and then
               Matches (Left'Old, Detached, Size) and then
               Kind (Right'Old) = Interior) and then
       (if Success then Matches (Left, Detached, Size - 1) and then
                        Matches (Right, Detached, Size - 1)
        else Left = Left'Old and Right = Right'Old);
   procedure Merge (Left, Right : in out Descriptor; Size : Order;
                    Success : out Boolean) with
     Post => Success = (Size < Order'Last and then
               Matches (Left'Old, Detached, Size) and then
               Matches (Right'Old, Detached, Size)) and then
       (if Success then Matches (Left, Detached, Size + 1) and then
                        Matches (Right, Interior, 0)
        else Left = Left'Old and Right = Right'Old);
private
   type Descriptor is record
      State : Block_Kind := Reserved;
      Size : Order := 0;
   end record with Size => Descriptor_Bits;
   for Descriptor use record
      State at 0 range 0 .. 7;
      Size at 1 range 0 .. 7;
   end record;
   function Kind (Item : Descriptor) return Block_Kind is (Item.State);
   function Block_Order (Item : Descriptor) return Order is (Item.Size);
end Buddy_Blocks;
