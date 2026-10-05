with Interfaces; use Interfaces;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Messages;
with System.Storage_Elements; use System.Storage_Elements;

package body System.Memory is

   --  CuAlloc's entry points (CuAlloc_Native, in libgnat-user.a, libc.a and
   --  libcubit_allocator.a alike).
   function Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64
     with Import, Convention => C, External_Name => "cualloc_allocate";
   procedure Release (Item : Unsigned_64)
     with Import, Convention => C, External_Name => "cualloc_free";
   function Reallocate (Item, Bytes : Unsigned_64) return Unsigned_64
     with Import, Convention => C, External_Name => "cualloc_reallocate";

   --  The largest alignment an Ada object needs here.
   ALIGNMENT : constant := 16;

   function To_Address (Value : Unsigned_64) return System.Address;

   function To_Address (Value : Unsigned_64) return System.Address is
     (System.Storage_Elements.To_Address (Integer_Address (Value)));

   procedure Out_Of_Memory (Size : size_t)
     with No_Return;

   procedure Out_Of_Memory (Size : size_t) is
      Unused : Unsigned_64;
   begin
      CuBit.Messages.debugPrint
        ("Ada heap: cannot allocate" & size_t'Image (Size)
         & " bytes; stopping" & ASCII.LF);
      loop
         Unused := CuBit.Kernel_Calls.Call (CuBit.Kernel_ABI.Exit_Process, 1);
      end loop;
   end Out_Of_Memory;

   function Alloc (Size : size_t) return System.Address is
      Item : constant Unsigned_64 := Allocate (Unsigned_64 (Size), ALIGNMENT);
   begin
      if Item = 0 then
         Out_Of_Memory (Size);
      end if;
      return To_Address (Item);
   end Alloc;

   procedure Free (Ptr : System.Address) is
   begin
      Release (Unsigned_64 (To_Integer (Ptr)));
   end Free;

   function Realloc
     (Ptr : System.Address; Size : size_t) return System.Address
   is
      Item : constant Unsigned_64 :=
        Reallocate (Unsigned_64 (To_Integer (Ptr)), Unsigned_64 (Size));
   begin
      if Item = 0 then
         Out_Of_Memory (Size);
      end if;
      return To_Address (Item);
   end Realloc;

end System.Memory;
