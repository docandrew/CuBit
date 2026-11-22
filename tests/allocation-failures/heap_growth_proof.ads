package Heap_Growth_Proof with SPARK_Mode is
   -- Proves arithmetic/control-flow safety of the instantiated algorithm;
   -- callback effects and release ordering are checked by the resource model.
   procedure Exercise (Count, Fail_At : Natural; Success : out Boolean);
end Heap_Growth_Proof;
