with Interfaces; use Interfaces;

--  Scratch ownership inventory for one inode's block tree. Its capacity is
--  a complete 4 KiB direct/single/double tree: at most 4,202,552 bytes,
--  transient on the validation call's stack; normally much smaller. Larger
--  allocations (only reachable through triple-indirect mappings) are rejected
--  by resize before any mutation. No per-hole entries, heap or BSS arena.
package Block_Inventory with Pure, SPARK_Mode is
   Maximum_Blocks : constant := 12 + 1024 + 1 + 1024 * 1024 + 1024 + 1;
   subtype Block_Count is Natural range 0 .. Maximum_Blocks;
   type Block_Array is array (Positive range <>) of Unsigned_32;

   procedure Sort_And_Check (Blocks : in out Block_Array; Unique : out Boolean)
     with Pre => Blocks'First = 1 and Blocks'Length <= Maximum_Blocks,
          Post => Unique =
            (for all I in 2 .. Blocks'Last => Blocks (I - 1) < Blocks (I));
   --  The proof covers memory safety and the final strict-order predicate.
   --  Permutation preservation of heapsort is regression-tested, not proved.
end Block_Inventory;
