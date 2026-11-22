pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuAlloc_Native;
with CuAlloc_Requests;
with CuBit.Libc_ABI;
with CuBit.Libc_Imports;

package body CuBit.Libc_Memory is
   use type Interfaces.C.size_t;
   use type System.Address;

   package Requests renames CuAlloc_Requests;
   FUNDAMENTAL_ALIGNMENT : constant := Requests.Fundamental_Alignment;
   pragma Compile_Time_Error (size_t'Size /= Unsigned_64'Size, "size_t converts to Unsigned_64 exactly");

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

   --  Each entry point is CuAlloc's fast path (CuAlloc_Native's Quick
   --  calls, inline, with no call of their own), else one tail call to
   --  its out-of-line rest, which is where errno is set: the fast path
   --  sets up no frame and makes no call.
   function Allocate_Rest (Bytes, Alignment : Unsigned_64) return System.Address with No_Inline;
   function Allocate_Rest (Bytes, Alignment : Unsigned_64) return System.Address is
     (Result (CuAlloc_Native.Allocate_Rest (Bytes, Alignment)));
   function Allocate_Zeroed_Rest (Bytes : Unsigned_64) return System.Address with No_Inline;
   function Allocate_Zeroed_Rest (Bytes : Unsigned_64) return System.Address is
     (Result (CuAlloc_Native.Allocate_Zeroed_Rest (Bytes, FUNDAMENTAL_ALIGNMENT)));

   --  A tail call from calloc (it returns its target).
   function Memset (Target : System.Address; Value : int; Bytes : size_t) return System.Address
     with Import, Convention => C, External_Name => "memset";

   function malloc (Bytes : size_t) return System.Address is
      Item : constant Unsigned_64 := CuAlloc_Native.Allocate_Quick (Unsigned_64 (Bytes), FUNDAMENTAL_ALIGNMENT);
   begin
      if Item /= 0 then
         return To_Address (Item);
      end if;
      return Allocate_Rest (Unsigned_64 (Bytes), FUNDAMENTAL_ALIGNMENT);
   end malloc;

   --  CuAlloc ignores a null pointer (no test here).
   procedure free (Item : System.Address) is
   begin
      CuAlloc_Native.Free (To_Value (Item));
   end free;

   --  The arithmetic is CuAlloc_Requests' (proved): the product only when
   --  it fits, else ENOMEM.
   function calloc (Count, Bytes : size_t) return System.Address is
      Total : Unsigned_64;
      Item : Unsigned_64;
      Untouched : Boolean;
   begin
      if not Requests.Product_Fits (Unsigned_64 (Count), Unsigned_64 (Bytes)) then
         return Result (0);
      end if;
      Total := Requests.Product (Unsigned_64 (Count), Unsigned_64 (Bytes));
      Item := CuAlloc_Native.Allocate_Quick (Total, FUNDAMENTAL_ALIGNMENT, Untouched);
      if Item = 0 then
         return Allocate_Zeroed_Rest (Total);
      elsif Untouched then
         return To_Address (Item);
      end if;
      return Memset (To_Address (Item), 0, size_t (Total));
   end calloc;

   function Reallocate_Rest (Item : System.Address; Bytes : size_t) return System.Address with No_Inline;
   function Reallocate_Rest (Item : System.Address; Bytes : size_t) return System.Address is
     (Result (CuAlloc_Native.Reallocate (To_Value (Item), Unsigned_64 (Bytes))));

   function realloc (Item : System.Address; Bytes : size_t) return System.Address is
      Kept : constant Unsigned_64 := CuAlloc_Native.Reallocate_Quick (To_Value (Item), Unsigned_64 (Bytes));
   begin
      if Kept /= 0 then
         return To_Address (Kept);
      end if;
      return Reallocate_Rest (Item, Bytes);
   end realloc;

   function reallocarray (Item : System.Address; Count, Bytes : size_t) return System.Address is
   begin
      if not Requests.Product_Fits (Unsigned_64 (Count), Unsigned_64 (Bytes)) then
         return Result (0);
      end if;
      return realloc (Item, size_t (Requests.Product (Unsigned_64 (Count), Unsigned_64 (Bytes))));
   end reallocarray;

   function aligned_alloc (Alignment, Bytes : size_t) return System.Address is
      Align : Unsigned_64;
      Item : Unsigned_64;
   begin
      if not Requests.Power_Of_Two (Unsigned_64 (Alignment)) then
         CuBit.Libc_Imports.Errno_Location.all := CuBit.Libc_ABI.EINVAL;
         return System.Null_Address;
      end if;
      Align := Requests.Request_Alignment (Unsigned_64 (Alignment));
      Item := CuAlloc_Native.Allocate_Quick (Unsigned_64 (Bytes), Align);
      if Item /= 0 then
         return To_Address (Item);
      end if;
      return Allocate_Rest (Unsigned_64 (Bytes), Align);
   end aligned_alloc;

   function memalign (Alignment, Bytes : size_t) return System.Address is
     (aligned_alloc (Alignment, Bytes));

   function Posix_Rest (Result : System.Address; Alignment, Bytes : Unsigned_64) return int with No_Inline;
   function Posix_Rest (Result : System.Address; Alignment, Bytes : Unsigned_64) return int is
      Target : System.Address with Import, Address => Result;
      Item : constant Unsigned_64 := CuAlloc_Native.Allocate_Rest (Bytes, Alignment);
   begin
      if Item = 0 then
         return CuBit.Libc_ABI.ENOMEM;
      end if;
      Target := To_Address (Item);
      return 0;
   end Posix_Rest;

   function posix_memalign (Result : System.Address; Alignment, Bytes : size_t) return int is
      Target : System.Address with Import, Address => Result;
      Align : Unsigned_64;
      Item : Unsigned_64;
   begin
      if not Requests.Posix_Alignment (Unsigned_64 (Alignment)) then
         return CuBit.Libc_ABI.EINVAL;
      end if;
      Align := Requests.Request_Alignment (Unsigned_64 (Alignment));
      Item := CuAlloc_Native.Allocate_Quick (Unsigned_64 (Bytes), Align);
      if Item /= 0 then
         Target := To_Address (Item);
         return 0;
      end if;
      return Posix_Rest (Result, Align, Unsigned_64 (Bytes));
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
