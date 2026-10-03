with Intel_GPU_Extent_Directory;
with Intel_GPU_Physical_Extents;
package Extent_Directory_Fixture is
   procedure Initialize
     (Object : in out Intel_GPU_Extent_Directory.Directory;
      Bases : Intel_GPU_Physical_Extents.Addresses);
end Extent_Directory_Fixture;
