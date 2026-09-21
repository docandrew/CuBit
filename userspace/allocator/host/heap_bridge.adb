with Heap_Slab_Instance;
with Heap_Classes;
with System.Storage_Elements; use System.Storage_Elements;
package body Heap_Bridge is
   use type Interfaces.C.size_t;
   use type System.Address;
   package H renames Heap_Slab_Instance;
   Heap : H.State;
   Arena_Bytes : constant := H.Arena_Bytes;
   -- Slabs share this backing region. The OS page-provider boundary is next.
   type Byte_Array is array (0 .. Arena_Bytes - 1) of Interfaces.C.unsigned_char;
   Arena : aliased Byte_Array with Alignment => 4_096;

   procedure Initialize is
   begin
      H.Initialize (Heap);
   end Initialize;

   function Allocate (Size : Interfaces.C.size_t) return System.Address is
      Item : H.Allocation;
   begin
      if Size = 0 or else Size > 4_096 then return System.Null_Address; end if;
      H.Allocate (Heap, Heap_Classes.Request_Size (Size), Item);
      if not Item.Value.Success then return System.Null_Address; end if;
      return Arena'Address + Storage_Offset (Item.Value.Position);
   end Allocate;

   procedure Release (Pointer : System.Address) is
      Position, Base : Integer_Address;
      Status : H.Release_Status;
   begin
      if Pointer = System.Null_Address then return; end if;
      Position := To_Integer (Pointer);
      Base := To_Integer (Arena'Address);
      if Position < Base or else Position - Base >= Arena_Bytes then return; end if;
      H.Release (Heap, H.Offset (Position - Base), Status);
   end Release;

   function Usable_Size (Pointer : System.Address) return Interfaces.C.size_t is
      Position : constant Integer_Address := To_Integer (Pointer);
      Base : constant Integer_Address := To_Integer (Arena'Address);
   begin
      if Position < Base or else Position - Base >= Arena_Bytes then return 0; end if;
      declare
         Item : constant H.Offset := H.Offset (Position - Base);
      begin
         if H.Live (Heap, Item) then
            return Interfaces.C.size_t (Heap_Classes.Stride (H.Class_Of (Heap, H.Page_Of (Item))));
         end if;
         return 0;
      end;
   end Usable_Size;
   function Reserved_Bytes return Interfaces.C.size_t is (Arena_Bytes);
   function Metadata_Bytes return Interfaces.C.size_t is (Heap'Size / System.Storage_Unit);
end Heap_Bridge;
