pragma Ada_2022;
package body Recording_Pools is

   use type System.Address;

   overriding procedure Allocate
     (Pool                     : in out Recording_Pool;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : Storage_Count;
      Alignment                : Storage_Count)
   is
   begin
      CuBit.CuAlloc_Pool.CuAlloc_Storage_Pool (Pool).Allocate
        (Storage_Address, Size_In_Storage_Elements, Alignment);
      Pool.Allocations := Pool.Allocations + 1;
      Pool.Last_Size := Size_In_Storage_Elements;
      for B of Pool.Live loop
         if B.Address = System.Null_Address then
            B := (Storage_Address, Size_In_Storage_Elements, Alignment);
            exit;
         end if;
      end loop;
   end Allocate;

   overriding procedure Deallocate
     (Pool                     : in out Recording_Pool;
      Storage_Address          : System.Address;
      Size_In_Storage_Elements : Storage_Count;
      Alignment                : Storage_Count)
   is
   begin
      Pool.Frees := Pool.Frees + 1;
      for B of Pool.Live loop
         if B.Address = Storage_Address then
            if B.Size = Size_In_Storage_Elements and then B.Alignment = Alignment then
               Pool.Exact_Frees := Pool.Exact_Frees + 1;
            end if;
            B := (others => <>);
            exit;
         end if;
      end loop;
      CuBit.CuAlloc_Pool.CuAlloc_Storage_Pool (Pool).Deallocate
        (Storage_Address, Size_In_Storage_Elements, Alignment);
   end Deallocate;

end Recording_Pools;
