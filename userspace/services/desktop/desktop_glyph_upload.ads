with Desktop_Vulkan_Startup;
with Vulkan_Glyph_Sources;
with Vulkan_Owned_Targets;
-- Synchronous rasterization into the exclusive coherent staging mapping,
-- followed by one nonblocking transfer submission. No retained CPU raster or
-- repacking copy. Caller owns an R8 backing sized to Plan(Key.Scale), and must
-- poll upload completion before Import_Backing or glyph-source association.
package Desktop_Glyph_Upload with SPARK_Mode is
   package D renames Desktop_Vulkan_Startup;
   use type D.Source_Result;
   procedure Start (Index : D.Backing_Slot; Lease : Vulkan_Owned_Targets.A.Ticket;
      Key : Vulkan_Glyph_Sources.Key; Advance : out Natural;
      Result : out D.Source_Result)
     with Global => (In_Out => D.Engine), Pre => D.Valid,
       Post => D.Valid and
         (if Result = D.Source_Accepted then Advance > 0);
end Desktop_Glyph_Upload;
