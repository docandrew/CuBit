pragma Ada_2022;
package AML_Objects.Construction_Test_Fixture with Pure is
   procedure Dirty_Unused (Store : in out State; Existing : Object_ID);
   procedure Retire_Integer (Store : in out State; ID : Object_ID);
end AML_Objects.Construction_Test_Fixture;
