with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
package body Intel_GPU_Session_Store is
   Element_Bytes : constant Storage_Count :=
     Storage_Count ((Element_Array'Component_Size + System.Storage_Unit - 1) /
                    System.Storage_Unit);
   -- Zero the element at Where, then apply Element's default values there
   -- (placement elaboration, not Import). Elements are never copied.
   procedure Initialize_At (Where : System.Address) is
      Memory : Storage_Array (1 .. Element_Bytes) with Import, Address => Where;
   begin
      Memory := [others => 0];
      declare
         Fresh : Element with Address => Where;
         pragma Warnings (Off, Fresh);
      begin
         null;
      end;
   end Initialize_At;
   procedure Initialize (Index : Slot) is
   begin
      Initialize_At (Items (Index)'Address);
   end Initialize;
   procedure Grow (To : Slot'Base; OK : out Boolean) is
      Elements : constant Unsigned_64 := Unsigned_64 (To - (Slot'First - 1));
   begin
      OK := To <= Count;
      if OK then return; end if;
      Storage.Commit_Through (Region, Elements * Unsigned_64 (Element_Bytes), OK);
      if not OK then return; end if;
      for Index in Count + 1 .. To loop
         Initialize (Index);
      end loop;
      Count := To;
   end Grow;
   procedure Reset (Index : Slot) is
   begin
      Initialize (Index);
   end Reset;
end Intel_GPU_Session_Store;
