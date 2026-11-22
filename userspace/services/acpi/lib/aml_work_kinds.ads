pragma Ada_2022;
-- Passive normally-returning observation only. No authority or work allowance.
package AML_Work_Kinds with Pure, SPARK_Mode is
   type Kind is (Namespace_Lookup, Namespace_Store, Value_Clone,
      Allocation_Preparation, Namespace_Snapshot);
end AML_Work_Kinds;
