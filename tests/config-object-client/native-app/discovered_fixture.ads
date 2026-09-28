with CCL.Objects;
with CCL.Types;
with CCL.VM;

package Discovered_Fixture is
   Name : constant String := "org.cubit.publication.readings";
   procedure Build
     (Shifted : Boolean; Contract : out CCL.Objects.Binding;
      Local_Types : out CCL.Types.Registry; First, Second : out CCL.VM.Value;
      Good : out Boolean);
end Discovered_Fixture;
