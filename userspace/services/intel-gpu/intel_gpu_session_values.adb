with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
package body Intel_GPU_Session_Values is
   Element_Bytes : constant Unsigned_64 :=
     Unsigned_64 ((Element_Array'Component_Size + System.Storage_Unit - 1) /
                  System.Storage_Unit);
   procedure Grow (To : Slot'Base; OK : out Boolean) is
   begin
      OK := To <= Count;
      if OK then return; end if;
      Storage.Commit_Through
        (Region, Unsigned_64 (To - (Slot'First - 1)) * Element_Bytes, OK);
      if not OK then return; end if;
      for Index in Count + 1 .. To loop
         Items (Index) := Default;
      end loop;
      Count := To;
   end Grow;
   procedure Reset (Index : Slot) is
   begin
      Items (Index) := Default;
   end Reset;
end Intel_GPU_Session_Values;
