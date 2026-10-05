pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuAlloc_Native;
with CuBit.Libc_ABI;
with CuBit.Libc_Imports;

package body CuBit.Libc_Memory is
   use type Interfaces.C.size_t;
   use type System.Address;

   --  malloc's alignment: max_align_t.
   FUNDAMENTAL_ALIGNMENT : constant := 16;

   function To_Address (Value : Unsigned_64) return System.Address is
     (System.Storage_Elements.To_Address (Integer_Address (Value)));
   function To_Value (Item : System.Address) return Unsigned_64 is
     (Unsigned_64 (To_Integer (Item)));

   --  The pointer, or null with errno = ENOMEM.
   function Result (Item : Unsigned_64) return System.Address is
   begin
      if Item = 0 then
         CuBit.Libc_Imports.Errno_Location.all := CuBit.Libc_ABI.ENOMEM;
         return System.Null_Address;
      end if;
      return To_Address (Item);
   end Result;

   function Power_Of_Two (Value : size_t) return Boolean is
     (Value /= 0 and then (Value and (Value - 1)) = 0);

   function malloc (Bytes : size_t) return System.Address is
     (Result (CuAlloc_Native.Allocate (Unsigned_64 (Bytes), FUNDAMENTAL_ALIGNMENT)));

   procedure free (Item : System.Address) is
   begin
      if Item /= System.Null_Address then
         CuAlloc_Native.Free (To_Value (Item));
      end if;
   end free;

   function calloc (Count, Bytes : size_t) return System.Address is
   begin
      if Bytes /= 0 and then Count > size_t'Last / Bytes then
         return Result (0);
      end if;
      return Result (CuAlloc_Native.Allocate_Zeroed (Unsigned_64 (Count * Bytes), FUNDAMENTAL_ALIGNMENT));
   end calloc;

   function realloc (Item : System.Address; Bytes : size_t) return System.Address is
     (Result (CuAlloc_Native.Reallocate (To_Value (Item), Unsigned_64 (Bytes))));

   function reallocarray (Item : System.Address; Count, Bytes : size_t) return System.Address is
   begin
      if Bytes /= 0 and then Count > size_t'Last / Bytes then
         return Result (0);
      end if;
      return realloc (Item, Count * Bytes);
   end reallocarray;

   function aligned_alloc (Alignment, Bytes : size_t) return System.Address is
   begin
      if not Power_Of_Two (Alignment) then
         CuBit.Libc_Imports.Errno_Location.all := CuBit.Libc_ABI.EINVAL;
         return System.Null_Address;
      end if;
      return Result (CuAlloc_Native.Allocate
        (Unsigned_64 (Bytes), Unsigned_64'Max (Unsigned_64 (Alignment), FUNDAMENTAL_ALIGNMENT)));
   end aligned_alloc;

   function memalign (Alignment, Bytes : size_t) return System.Address is
     (aligned_alloc (Alignment, Bytes));

   function posix_memalign (Result : System.Address; Alignment, Bytes : size_t) return int is
      Target : System.Address with Import, Address => Result;
      Item : Unsigned_64;
   begin
      if not Power_Of_Two (Alignment) or else Alignment mod (System.Address'Size / 8) /= 0 then
         return CuBit.Libc_ABI.EINVAL;
      end if;
      Item := CuAlloc_Native.Allocate
        (Unsigned_64 (Bytes), Unsigned_64'Max (Unsigned_64 (Alignment), FUNDAMENTAL_ALIGNMENT));
      if Item = 0 then
         return CuBit.Libc_ABI.ENOMEM;
      end if;
      Target := To_Address (Item);
      return 0;
   end posix_memalign;

   function malloc_usable_size (Item : System.Address) return size_t is
     (if Item = System.Null_Address then 0
      else size_t (CuAlloc_Native.Usable_Size (To_Value (Item))));

   function libc_malloc (Bytes : size_t) return System.Address is (malloc (Bytes));
   function libc_malloc_impl (Bytes : size_t) return System.Address is (malloc (Bytes));
   procedure libc_free (Item : System.Address) is
   begin
      free (Item);
   end libc_free;
   function libc_calloc (Count, Bytes : size_t) return System.Address is (calloc (Count, Bytes));
   function libc_realloc (Item : System.Address; Bytes : size_t) return System.Address is
     (realloc (Item, Bytes));
   procedure malloc_atfork (Who : int) is null;
   procedure malloc_donate (First, Last : System.Address) is null;
end CuBit.Libc_Memory;
