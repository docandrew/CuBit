-- Failure-atomic multi-page growth. Hardware callbacks are trusted boundaries;
-- failure-injection tests exercise this same sequence with a resource model.
package Heap_Growth with SPARK_Mode is
   generic
      with procedure Add (Index : Natural; Success : out Boolean);
      -- Add(False) must leave no leaf/data-frame ownership for that index.
      with procedure Unmap (Index : Natural);
      with procedure Synchronize;
      with procedure Release_Latest;
   procedure Apply (Count : Natural; Success : out Boolean);
   -- On failure unmap the entire successfully added prefix, synchronize TLBs
   -- once, then release its frames in LIFO order. Existing pages are untouched.
   -- Intermediate page tables may remain owned for reuse / process teardown.
end Heap_Growth;
