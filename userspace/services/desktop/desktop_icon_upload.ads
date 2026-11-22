with Desktop_Icon_Pixels;
with Desktop_Vulkan_Startup;
with Vulkan_Owned_Targets;
-- Upload one bounded row chunk. The backing owner must poll completion and
-- initialize all rows before importing a source for scene capture.
package Desktop_Icon_Upload with SPARK_Mode is
   package D renames Desktop_Vulkan_Startup;
   procedure Start (Index : D.Backing_Slot; Lease : Vulkan_Owned_Targets.A.Ticket;
      Item : Desktop_Icon_Pixels.Asset; Result : out D.Source_Result)
     with Global => (In_Out => D.Engine), Pre => D.Valid, Post => D.Valid;
   procedure Start_Atlas (Index : D.Backing_Slot; Lease : Vulkan_Owned_Targets.A.Ticket;
      Kind : Desktop_Icon_Pixels.Family; Result : out D.Source_Result)
     with Global => (In_Out => D.Engine), Pre => D.Valid, Post => D.Valid;
end Desktop_Icon_Upload;
