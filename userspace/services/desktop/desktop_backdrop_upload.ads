with CuBit.Appearance;
with Desktop_Vulkan_Startup;
with Vulkan_Owned_Targets;
-- One bounded initialization chunk per call. The owner polls its transfer
-- before requesting another chunk and imports only complete image coverage.
package Desktop_Backdrop_Upload with SPARK_Mode is
   package D renames Desktop_Vulkan_Startup;
   procedure Start
     (Index : D.Backing_Slot; Lease : Vulkan_Owned_Targets.A.Ticket;
      Asset : CuBit.Appearance.Background; Result : out D.Source_Result)
     with Global => (In_Out => D.Engine), Pre => D.Valid, Post => D.Valid;
end Desktop_Backdrop_Upload;
