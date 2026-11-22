-- Instances under test and proof:
--   Small:  64 x 4 = 256 slots, generations retire at 3 (wrap tests).
--   Kernel: the kernel's geometry and element (Capabilities.Capability,
--           512 leaves = 32768 slots, full 32-bit generations).
with Interfaces;
with Capabilities;
with Capability_Tables;
with Fake_Pages;

package Test_Tables with SPARK_Mode is
   type Test_Item is record
      Id : Interfaces.Unsigned_64 := 0;
   end record;
   Empty_Item : constant Test_Item := (Id => 0);

   Small_Generation_Limit : constant := 3;
   Small_Leaves           : constant := 4;

   package Small is new Capability_Tables
     (Element              => Test_Item,
      Empty_Element        => Empty_Item,
      Generation_Limit     => Small_Generation_Limit,
      Leaves_Per_Directory => Small_Leaves,
      Allocate_Page        => Fake_Pages.Allocate_Page,
      Release_Page         => Fake_Pages.Release_Page);

   Kernel_Leaves : constant := 512;

   package Kernel is new Capability_Tables
     (Element              => Capabilities.Capability,
      Empty_Element        => Capabilities.NULL_CAPABILITY,
      Generation_Limit     => Interfaces.Unsigned_32'Last,
      Leaves_Per_Directory => Kernel_Leaves,
      Allocate_Page        => Fake_Pages.Allocate_Page,
      Release_Page         => Fake_Pages.Release_Page);
end Test_Tables;
