pragma Ada_2022;
with AML_Names;

--  Capacity-aware bucket selection shared by namespace owners of any size.
package AML_Child_Hash with SPARK_Mode, Pure is
   function Bucket
     (Scope : Natural; Part : AML_Names.Segment; Capacity : Positive)
      return Positive
     with Post => Bucket'Result <= Capacity;
end AML_Child_Hash;
