pragma Ada_2022;
with AML_Child_Indices;
package Bounds_Proof with SPARK_Mode is
   package Small_Links is new AML_Child_Indices (512);
   package Small is new Small_Links.Code_Bounds (65_536);
   package Large_Links is new AML_Child_Indices (16_384);
   package Large is new Large_Links.Code_Bounds (65_536);
   procedure Run_Small;
   procedure Run_Large;
end Bounds_Proof;
