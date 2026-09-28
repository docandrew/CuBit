with CCL.Objects;
package Nested_Fixture is
   Name : constant String := "org.cubit.publication.preferences";
   procedure Define (Shifted : Boolean; Contract : out CCL.Objects.Binding; Success : out Boolean);
   procedure Values
     (Contract : CCL.Objects.Binding; First, Second : out CCL.Objects.Image;
      Success : out Boolean);
end Nested_Fixture;
