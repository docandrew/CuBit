with System;
with Desktop_Composition;
--  Records opaque, unscaled client pixels using the existing proved planner.
--  Recorded is NOT execution, visibility, completion, or permission to retire.
--  Borrowed is an exclusively held native cubit_vulkan_copy context. Its images,
--  device, memory, layouts and synchronization are trusted platform obligations.
package Vulkan_Copy_Binding with SPARK_Mode is
   type Outcome is (Empty, Recorded, Rejected);
   procedure Draw_Client
     (Borrowed : System.Address;
      Target_Width, Target_Height, Source_Width, Source_Height : Natural;
      Destination : Desktop_Composition.Rectangle;
      Clipped : Boolean; Clip : Desktop_Composition.Rectangle;
      Result : out Outcome)
     with Global => null;
end Vulkan_Copy_Binding;
