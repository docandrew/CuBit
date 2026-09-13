package body Buddy_Blocks with SPARK_Mode is
   procedure Admit (Item : in out Descriptor; Size : Order; Head : Boolean;
                    Success : out Boolean) is
   begin
      Success := Item.State = Reserved;
      if Success then
         Item := (State => (if Head then Detached else Interior),
                  Size => (if Head then Size else 0));
      end if;
   end Admit;

   procedure Move (Item : in out Descriptor; Size : Order;
                   Action : Transition; Success : out Boolean) is
   begin
      Success := Matches (Item, Source (Action), Size);
      if Success then
         Item.State := Target (Action);
      end if;
   end Move;

   procedure Split (Left, Right : in out Descriptor; Size : Order;
                    Success : out Boolean) is
   begin
      Success := Size > 0 and then Matches (Left, Detached, Size)
        and then Right.State = Interior;
      if Success then
         Left.Size := Size - 1;
         Right := Left;
      end if;
   end Split;

   procedure Merge (Left, Right : in out Descriptor; Size : Order;
                    Success : out Boolean) is
   begin
      Success := Size < Order'Last and then Matches (Left, Detached, Size)
        and then Matches (Right, Detached, Size);
      if Success then
         Left.Size := Size + 1;
         Right := (State => Interior, Size => 0);
      end if;
   end Merge;
end Buddy_Blocks;
