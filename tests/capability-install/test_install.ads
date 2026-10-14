-- Instances under test and proof: Capability_Install.Placement over a
-- small capability table (4 leaves, generations retire at 3) and over the
-- kernel's geometry (512 leaves, 32-bit generations).
with Interfaces;
with Capabilities;
with Capability_Tables;
with Capability_Install.Placement;
with Fake_Pages;

package Test_Install with SPARK_Mode is
   Small_Generation_Limit : constant := 3;
   Small_Leaves           : constant := 4;

   package Small_Tables is new Capability_Tables
     (Element              => Capabilities.Capability,
      Empty_Element        => Capabilities.NULL_CAPABILITY,
      Generation_Limit     => Small_Generation_Limit,
      Leaves_Per_Directory => Small_Leaves,
      Allocate_Page        => Fake_Pages.Allocate_Page,
      Release_Page         => Fake_Pages.Release_Page);
   package Small is new Capability_Install.Placement (Small_Tables);

   Kernel_Leaves : constant := 512;

   package Kernel_Tables is new Capability_Tables
     (Element              => Capabilities.Capability,
      Empty_Element        => Capabilities.NULL_CAPABILITY,
      Generation_Limit     => Interfaces.Unsigned_32'Last,
      Leaves_Per_Directory => Kernel_Leaves,
      Allocate_Page        => Fake_Pages.Allocate_Page,
      Release_Page         => Fake_Pages.Release_Page);
   package Kernel is new Capability_Install.Placement (Kernel_Tables);
end Test_Install;
