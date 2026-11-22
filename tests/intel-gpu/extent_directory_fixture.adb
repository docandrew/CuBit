with Interfaces; use Interfaces;
package body Extent_Directory_Fixture is
   procedure Initialize
     (Object : in out Intel_GPU_Extent_Directory.Directory;
      Bases : Intel_GPU_Physical_Extents.Addresses) is
      OK : Boolean;
   begin
      Intel_GPU_Extent_Directory.Initialize
        (Object, Intel_GPU_Physical_Extents.Capacity, 2 ** 32, OK);
      pragma Assert (OK);
      for Base of Bases loop
         Intel_GPU_Extent_Directory.Append (Object, Base, OK);
         pragma Assert (OK);
      end loop;
   end Initialize;
end Extent_Directory_Fixture;
