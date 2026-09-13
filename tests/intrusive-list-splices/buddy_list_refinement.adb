package body Buddy_List_Refinement with SPARK_Mode is
   procedure Initialize_Empty
     (Next, Previous : in out References; Sequence, Rank : out References;
      Ledger : Descriptors; Count : out Natural; Size : Buddy_Blocks.Order) is
   begin
      Next (0) := 0;
      Previous (0) := 0;
      Sequence := [others => 0];
      Rank := [others => 0];
      Count := 0;
   end Initialize_Empty;

   procedure Insert_Front
     (Next, Previous, Sequence, Rank : in out References;
      Ledger : in out Descriptors; Count : in out Natural;
      Size : Buddy_Blocks.Order; Item : ID)
   is
      Old_Sequence : constant References := Sequence;
      Old_Rank : constant References := Rank;
      Old_Count : constant Natural := Count;
      First : constant ID := Next (0);
      Accepted : Boolean;
      Head_Next : ID := First;
      First_Previous, Item_Previous, Item_Next : ID;
   begin
      -- These are the production operations, in the physical adapter's order.
      Buddy_Blocks.Move (Ledger (Item), Size, Buddy_Blocks.Publish, Accepted);
      pragma Assert (Accepted);
      pragma Assert (First /= Item);
      Splices.Insert_Front (Head_Next, First_Previous, Item_Previous,
                            Item_Next, 0, Item);
      Next (0) := Head_Next;
      Previous (First) := First_Previous;
      Previous (Item) := Item_Previous;
      Next (Item) := Item_Next;
      Count := Splices.Added (Count);

      -- Only the Ghost witness is linear-time. No such arrays/loops enter
      -- the kernel; the actual representation remains an intrusive list.
      Sequence := [for P in Sequence'Range =>
        (if P = 0 then 0 elsif P = 1 then Item
         elsif P <= Old_Count + 1 then Old_Sequence (P - 1) else 0)];
      Rank := [for I in Rank'Range =>
        (if I = Item then 1 elsif Old_Rank (I) = 0 then 0
         else Old_Rank (I) + 1)];
   end Insert_Front;

   procedure Remove_At
     (Next, Previous, Sequence, Rank : in out References;
      Ledger : in out Descriptors; Count : in out Natural;
      Size : Buddy_Blocks.Order; Position : Positive)
   is
      Old_Sequence : constant References := Sequence;
      Old_Rank : constant References := Rank;
      Item : constant ID := Sequence (Position);
      Before, After : ID;
      Before_Next, After_Previous : ID;
      Accepted : Boolean;
   begin
      Buddy_Blocks.Move (Ledger (Item), Size, Buddy_Blocks.Remove, Accepted);
      pragma Assert (Accepted);
      Before := Previous (Item);
      After := Next (Item);
      Splices.Remove (Before_Next, After_Previous, Before, After);
      Next (Before) := Before_Next;
      Previous (After) := After_Previous;
      Count := Splices.Removed (Count);
      Sequence := [for P in Sequence'Range =>
        (if P = 0 or else P > Count then 0
         elsif P < Position then Old_Sequence (P) else Old_Sequence (P + 1))];
      Rank := [for I in Rank'Range =>
        (if I = Item then 0 elsif Old_Rank (I) > Position then Old_Rank (I) - 1
         else Old_Rank (I))];
   end Remove_At;

   procedure Prove_Membership
     (Next, Previous, Sequence, Rank : References;
      Ledger : Descriptors; Count : Natural; Size : Buddy_Blocks.Order;
      Item : ID)
   is
   begin
      if Rank (Item) > 0 then
         pragma Assert (Sequence (Rank (Item)) = Item);
      end if;
   end Prove_Membership;

   procedure Prove_Unique
     (Next, Previous, Sequence, Rank : References;
      Ledger : Descriptors; Count : Natural; Size : Buddy_Blocks.Order;
      P, Q : Positive) is
   begin
      null;
   end Prove_Unique;

   procedure Prove_Traversal
     (Next, Previous, Sequence, Rank : References;
      Ledger : Descriptors; Count : Natural; Size : Buddy_Blocks.Order;
      Traversed : out Natural; End_Node : out ID) is
   begin
      Traversed := 0;
      End_Node := Next (0);
      for P in 1 .. Count loop
         pragma Loop_Invariant (Traversed = P - 1);
         pragma Loop_Invariant (End_Node = Sequence (P));
         pragma Assert (End_Node /= 0);
         End_Node := Next (End_Node);
         Traversed := Traversed + 1;
      end loop;
   end Prove_Traversal;
end Buddy_List_Refinement;
