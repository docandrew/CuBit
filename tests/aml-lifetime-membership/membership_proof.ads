pragma Ada_2022;
with AML_Child_Indices;
package Membership_Proof with SPARK_Mode is
   package Small_Links is new AML_Child_Indices (512);
   package Small renames Small_Links.Memberships;
   package Large_Links is new AML_Child_Indices (16_384);
   package Large renames Large_Links.Memberships;
   procedure Run_Small;
   procedure Run_Large;
end Membership_Proof;
