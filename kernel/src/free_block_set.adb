package body Free_Block_Set with SPARK_Mode is
   procedure Prove_Population_Change
     (Before, After : Storage; Item, Prefix : Index) with Ghost,
     Pre => Shape (Before) and then Shape (After)
       and then Before'Last = After'Last and then Item < Leaves (Before)
       and then Prefix <= Leaves (Before)
       and then (for all I in 0 .. Leaves (Before) - 1 =>
         (if I /= Item then Contains (Before, I) = Contains (After, I))),
     Post => Signed_Count (Population (After, Prefix)) =
       Signed_Count (Population (Before, Prefix)) +
         (if Item < Prefix then
            (if Contains (After, Item) then 1 else 0) -
            (if Contains (Before, Item) then 1 else 0) else 0),
     Subprogram_Variant => (Decreases => Prefix)
   is
   begin
      if Prefix > 0 then
         Prove_Population_Change (Before, After, Item, Prefix - 1);
      end if;
   end Prove_Population_Change;

   procedure Prove_Zero (Bits : Storage; Prefix : Index) with Ghost,
     Pre => Shape (Bits) and then Prefix <= Leaves (Bits) and then
       (for all I in 0 .. Prefix - 1 => not Contains (Bits, I)),
     Post => Population (Bits, Prefix) = 0,
     Subprogram_Variant => (Decreases => Prefix)
   is
   begin
      if Prefix > 0 then
         Prove_Zero (Bits, Prefix - 1);
      end if;
   end Prove_Zero;

   procedure Initialize (Bits : out Storage) is
   begin
      Bits := [others => False];
      Prove_Zero (Bits, Leaves (Bits));
   end Initialize;

   procedure Set_Membership
     (Bits : in out Storage; Item : Index; Present : Boolean;
      Changed : out Boolean)
   is
      Leaf : constant Node_Index := Leaves (Bits) + Item;
      Cursor : Node_Index := Leaf;
      Before : constant Storage := Bits with Ghost;
   begin
      Changed := Bits (Leaf) /= Present;
      if not Changed then
         return;
      end if;
      Bits (Leaf) := Present;
      while Cursor > 1 loop
         pragma Loop_Invariant (Cursor <= Leaf);
         pragma Loop_Invariant (Contains (Bits, Item) = Present);
         pragma Loop_Invariant
           (for all I in 0 .. Leaves (Bits) - 1 =>
              (if I /= Item then Contains (Bits, I) = Contains (Bits'Loop_Entry, I)));
         pragma Loop_Invariant
           (for all N in 1 .. Leaves (Bits) - 1 =>
              (if N /= Cursor / 2 then Consistent_Node (Bits, N)));
         pragma Loop_Variant (Decreases => Cursor);
         -- Once the parent's summary is unchanged, no ancestor can change.
         exit when Bits (Cursor / 2) =
           (Bits (2 * (Cursor / 2)) or Bits (2 * (Cursor / 2) + 1));
         Cursor := Cursor / 2;
         Bits (Cursor) := Bits (2 * Cursor) or Bits (2 * Cursor + 1);
      end loop;
      Prove_Population_Change (Before, Bits, Item, Leaves (Bits));
   end Set_Membership;

   procedure Update
     (Bits : in out Storage; Count : in out Index; Item : Index;
      Present : Boolean; Changed : out Boolean)
   is
   begin
      Set_Membership (Bits, Item, Present, Changed);
      if Changed then
         if Present then
            Count := Count + 1;
         else
            Count := Count - 1;
         end if;
      end if;
   end Update;

   procedure Prove_Node_Visible (Bits : Storage; Node : Node_Index) with Ghost,
     Pre => Shape (Bits) and then Valid (Bits) and then Node <= Bits'Last
       and then Bits (Node),
     Post => not Empty (Bits)
   is
      Cursor : Node_Index := Node;
   begin
      while Cursor > 1 loop
         pragma Loop_Invariant (Cursor <= Bits'Last);
         pragma Loop_Invariant (Bits (Cursor));
         pragma Loop_Variant (Decreases => Cursor);
         Cursor := Cursor / 2;
      end loop;
   end Prove_Node_Visible;

   procedure Prove_Visibility (Bits : Storage; Item : Index) is
   begin
      Prove_Node_Visible (Bits, Leaves (Bits) + Item);
   end Prove_Visibility;

   procedure Prove_Empty (Bits : Storage) with Ghost,
     Pre => Shape (Bits) and then Valid (Bits) and then Empty (Bits),
     Post => (for all I in 0 .. Leaves (Bits) - 1 => not Contains (Bits, I))
   is
   begin
      for I in 0 .. Leaves (Bits) - 1 loop
         if Contains (Bits, I) then
            Prove_Visibility (Bits, I);
         end if;
         pragma Loop_Invariant
           (for all J in 0 .. I => not Contains (Bits, J));
      end loop;
   end Prove_Empty;

   procedure Find (Bits : Storage; Item : out Index; Found : out Boolean) is
      Cursor : Node_Index := 1;
   begin
      Item := 0;
      Found := Bits (1);
      if not Found then
         Prove_Empty (Bits);
         return;
      end if;
      while Cursor < Leaves (Bits) loop
         pragma Loop_Invariant (Cursor <= Bits'Last);
         pragma Loop_Invariant (Bits (Cursor));
         pragma Loop_Variant (Increases => Cursor);
         if Bits (2 * Cursor) then
            Cursor := 2 * Cursor;
         else
            Cursor := 2 * Cursor + 1;
         end if;
      end loop;
      Item := Cursor - Leaves (Bits);
   end Find;

   procedure Find_Near
     (Bits : Storage; Near : Index; Item : out Index; Found : out Boolean)
   is
      Cursor : Node_Index := Leaves (Bits) + Near;
   begin
      while Cursor > 1 and then not Bits (Cursor) loop
         pragma Loop_Invariant (Cursor <= Bits'Last);
         pragma Loop_Variant (Decreases => Cursor);
         Cursor := Cursor / 2;
      end loop;
      Item := 0;
      Found := Bits (Cursor);
      if not Found then
         Prove_Empty (Bits);
         return;
      end if;
      Prove_Node_Visible (Bits, Cursor);
      while Cursor < Leaves (Bits) loop
         pragma Loop_Invariant (Cursor <= Bits'Last);
         pragma Loop_Invariant (Bits (Cursor));
         pragma Loop_Variant (Increases => Cursor);
         if Bits (2 * Cursor) then
            Cursor := 2 * Cursor;
         else
            Cursor := 2 * Cursor + 1;
         end if;
      end loop;
      Item := Cursor - Leaves (Bits);
   end Find_Near;
end Free_Block_Set;
