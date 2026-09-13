-- Packed membership tree. Leaves identify blocks; internal bits summarize
-- whether either child is nonempty. No addresses or payload data are stored.
package Free_Block_Set with SPARK_Mode, Pure is
   type Index is range 0 .. 2 ** 42 - 1;
   subtype Node_Index is Index range 1 .. Index'Last;
   subtype Capacity is Index range 1 .. 2 ** 40;
   type Signed_Count is range -(2 ** 40) .. 2 ** 40;
   type Storage is array (Node_Index range <>) of Boolean with Pack;
   function Shape (Bits : Storage) return Boolean is
     (Bits'Length > 0 and then Bits'First = 1 and then Bits'Last mod 2 = 1 and then
      Bits'Last <= 2 * Capacity'Last - 1);
   function Leaves (Bits : Storage) return Capacity is (Bits'Last / 2 + 1)
     with Pre => Shape (Bits);
   function Valid (Bits : Storage) return Boolean with Ghost,
     Pre => Shape (Bits);
   function Contains (Bits : Storage; Item : Index) return Boolean is
     (Bits (Leaves (Bits) + Item))
     with Pre => Shape (Bits) and then Item < Leaves (Bits);
   function Empty (Bits : Storage) return Boolean is (not Bits (1))
     with Pre => Shape (Bits);

   function Population (Bits : Storage; Prefix : Index) return Index with Ghost,
     Pre => Shape (Bits) and then Prefix <= Leaves (Bits),
     Post => Population'Result <= Prefix,
     Subprogram_Variant => (Decreases => Prefix);
   function Cardinality (Bits : Storage) return Index is
     (Population (Bits, Leaves (Bits))) with Ghost, Pre => Shape (Bits);

   procedure Initialize (Bits : out Storage) with
     Pre => Bits'Length > 0 and then Bits'First = 1 and then Bits'Last mod 2 = 1 and then
       Bits'Last <= 2 * Capacity'Last - 1,
     Post => Valid (Bits) and then Empty (Bits) and then
       (for all I in 0 .. Leaves (Bits) - 1 => not Contains (Bits, I))
       and then Cardinality (Bits) = 0;

   procedure Set_Membership
     (Bits : in out Storage; Item : Index; Present : Boolean;
      Changed : out Boolean) with
     Pre => Shape (Bits) and then Valid (Bits) and then Item < Leaves (Bits),
     Post => Valid (Bits) and then Contains (Bits, Item) = Present
       and then Changed = (Contains (Bits'Old, Item) /= Present)
       and then (for all I in 0 .. Leaves (Bits) - 1 =>
         (if I /= Item then Contains (Bits, I) = Contains (Bits'Old, I)))
       and then Signed_Count (Cardinality (Bits)) =
         Signed_Count (Cardinality (Bits'Old)) +
           (if Changed then (if Present then 1 else -1) else 0);

   procedure Update
     (Bits : in out Storage; Count : in out Index; Item : Index;
      Present : Boolean; Changed : out Boolean) with
     Pre => Shape (Bits) and then Valid (Bits) and then Item < Leaves (Bits)
       and then Count = Cardinality (Bits),
     Post => Valid (Bits) and then Count = Cardinality (Bits)
       and then Contains (Bits, Item) = Present
       and then Changed = (Contains (Bits'Old, Item) /= Present)
       and then (for all I in 0 .. Leaves (Bits) - 1 =>
         (if I /= Item then Contains (Bits, I) = Contains (Bits'Old, I)));

   procedure Prove_Visibility (Bits : Storage; Item : Index) with Ghost,
     Pre => Shape (Bits) and then Valid (Bits) and then Item < Leaves (Bits)
       and then Contains (Bits, Item),
     Post => not Empty (Bits);

   -- Returns an existing member without modifying the tree. Ordering is not
   -- promised (non-power-of-two capacities also work).
   procedure Find (Bits : Storage; Item : out Index; Found : out Boolean) with
     Pre => Shape (Bits) and then Valid (Bits),
     Post => Found = not Empty (Bits) and then
       (if Found then Item < Leaves (Bits) and then Contains (Bits, Item)
        else Item = 0 and then
          (for all I in 0 .. Leaves (Bits) - 1 => not Contains (Bits, I)));

   -- A locality hint is not authority: it may point at an occupied block.
   -- Ascend to the nearest nonempty subtree, then descend to a real member.
   procedure Find_Near
     (Bits : Storage; Near : Index; Item : out Index; Found : out Boolean) with
     Pre => Shape (Bits) and then Valid (Bits) and then Near < Leaves (Bits),
     Post => Found = not Empty (Bits) and then
       (if Found then Item < Leaves (Bits) and then Contains (Bits, Item)
        else Item = 0 and then
          (for all I in 0 .. Leaves (Bits) - 1 => not Contains (Bits, I)));
private
   function Population (Bits : Storage; Prefix : Index) return Index is
     (if Prefix = 0 then 0 else Population (Bits, Prefix - 1) +
       (if Contains (Bits, Prefix - 1) then 1 else 0));
   function Consistent_Node (Bits : Storage; N : Node_Index) return Boolean is
     (Bits (N) = (Bits (2 * N) or Bits (2 * N + 1)))
     with Ghost, Pre => Shape (Bits) and then N < Leaves (Bits);
   function Valid (Bits : Storage) return Boolean is
     (for all N in 1 .. Leaves (Bits) - 1 => Consistent_Node (Bits, N));
end Free_Block_Set;
