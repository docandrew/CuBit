with System.Storage_Elements; use System.Storage_Elements;

package body Test_Table is
   use type System.Address;

   function C_Aligned_Alloc (Alignment, Size : Storage_Count) return System.Address
     with Import, Convention => C, External_Name => "aligned_alloc";
   procedure C_Free (Addr : System.Address)
     with Import, Convention => C, External_Name => "free";

   Held : Boolean := False with Atomic;
   function Test_And_Set (Ptr : System.Address; Value : Unsigned_8) return Unsigned_8
     with Import, Convention => Intrinsic,
          External_Name => "__sync_lock_test_and_set_1";

   procedure Reset (E : in out Record_Type) is
   begin
      E := (others => <>);
   end Reset;

   procedure Alloc_Page (Page_Bytes : Natural; Addr : out System.Address) is
      Size : constant Storage_Count := Storage_Count ((Page_Bytes + 63) / 64 * 64);
   begin
      Addr := C_Aligned_Alloc (64, Size);
      if Addr /= System.Null_Address then
         Pages_Allocated := Pages_Allocated + 1;
      end if;
   end Alloc_Page;

   procedure Free_Page (Page_Bytes : Natural; Addr : System.Address) is
      Bytes : Storage_Array (1 .. Storage_Offset (Page_Bytes))
        with Import, Address => Addr;
   begin
      --  Poison: a reader still holding a pointer into this page would see
      --  a tag that is neither 0 nor its ID's value.
      Bytes := [others => 16#DE#];
      C_Free (Addr);
      Pages_Freed := Pages_Freed + 1;
   end Free_Page;

   Lock_Byte : aliased Unsigned_8 := 0 with Volatile;

   procedure Lock is
   begin
      while Test_And_Set (Lock_Byte'Address, 1) /= 0 loop
         null;
      end loop;
      pragma Assert (not Held);
      Held := True;
   end Lock;

   procedure Unlock is
   begin
      Held := False;
      Lock_Byte := 0;
   end Unlock;
end Test_Table;
