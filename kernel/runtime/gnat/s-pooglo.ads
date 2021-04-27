
 -- Hack to work-around issue with GNAT's Simple_Storage_Pools and arrays.
 -- When declaring an unbounded array ptr with 'Simple_Storage_Pool attribute,
 -- GNAT tries to ensure that this file is available when doing a
 -- myarrayptr := new MyArray(0..length) call.
 -- Examination of the actual allocation disassembly shows that the allocation
 -- will be performed with the StoragePools.Allocate function though.

package System.Pool_Global is

   type Pool is limited null record;

   Global_Pool_Object : aliased Pool;

end System.Pool_Global;
