pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuAlloc_Native;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Messages;

package body CuAlloc_Gnat is
   --  The largest alignment an Ada object needs here.
   ALIGNMENT : constant := 16;

   function To_Address (Value : Unsigned_64) return System.Address is
     (System.Storage_Elements.To_Address (Integer_Address (Value)));
   function To_Value (Item : System.Address) return Unsigned_64 is
     (Unsigned_64 (To_Integer (Item)));

   procedure Out_Of_Memory (Size : Unsigned_64) with No_Return, No_Inline;
   procedure Out_Of_Memory (Size : Unsigned_64) is
      Text : String (1 .. 20);
      First : Positive := Text'Last + 1;
      Rest : Unsigned_64 := Size;
      Unused : Unsigned_64;
   begin
      loop
         First := First - 1;
         Text (First) := Character'Val (Character'Pos ('0') + Natural (Rest mod 10));
         Rest := Rest / 10;
         exit when Rest = 0;
      end loop;
      CuBit.Messages.debugPrint ("Ada heap: cannot allocate ");
      CuBit.Messages.debugPrint (Text (First .. Text'Last));
      CuBit.Messages.debugPrint (" bytes; stopping" & ASCII.LF);
      loop
         Unused := CuBit.Kernel_Calls.Call (CuBit.Kernel_ABI.Exit_Process, 1);
      end loop;
   end Out_Of_Memory;

   --  The pointer, or the end of the process.
   function Checked (Item, Size : Unsigned_64) return System.Address is
   begin
      if Item = 0 then
         Out_Of_Memory (Size);
      end if;
      return To_Address (Item);
   end Checked;

   function Alloc_Rest (Size : Unsigned_64) return System.Address with No_Inline;
   function Alloc_Rest (Size : Unsigned_64) return System.Address is
     (Checked (CuAlloc_Native.Allocate_Rest (Size, ALIGNMENT), Size));

   function Alloc (Size : size_t) return System.Address is
      Item : constant Unsigned_64 := CuAlloc_Native.Allocate_Quick (Unsigned_64 (Size), ALIGNMENT);
   begin
      if Item /= 0 then
         return To_Address (Item);
      end if;
      return Alloc_Rest (Unsigned_64 (Size));
   end Alloc;

   procedure Free (Ptr : System.Address) is
   begin
      CuAlloc_Native.Free (To_Value (Ptr));
   end Free;

   function Realloc_Rest (Ptr : System.Address; Size : Unsigned_64) return System.Address with No_Inline;
   function Realloc_Rest (Ptr : System.Address; Size : Unsigned_64) return System.Address is
     (Checked (CuAlloc_Native.Reallocate (To_Value (Ptr), Size), Size));

   function Realloc (Ptr : System.Address; Size : size_t) return System.Address is
      Kept : constant Unsigned_64 := CuAlloc_Native.Reallocate_Quick (To_Value (Ptr), Unsigned_64 (Size));
   begin
      if Kept /= 0 then
         return To_Address (Kept);
      end if;
      return Realloc_Rest (Ptr, Unsigned_64 (Size));
   end Realloc;
end CuAlloc_Gnat;
