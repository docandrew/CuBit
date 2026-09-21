with Heap_Classes;
with Heap_Extents;
with Heap_Slab_Instance;
package body Heap_Runtime is
   package S renames Heap_Slab_Instance;
   package E renames Heap_Extents;
   use type S.Release_Status;
   Slabs : S.State;
   Extents : E.State;
   procedure Initialize is
   begin
      S.Initialize (Slabs);
      E.Initialize (Extents);
   end Initialize;
   function Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64 is
      Value : S.Allocation;
      First : E.Page_Reference;
   begin
      if Bytes = 0 or else Bytes > Arena_Bytes or else Alignment = 0 or else
        Alignment > Maximum_Alignment or else (Alignment and (Alignment - 1)) /= 0
      then return No_Allocation; end if;
      if Bytes <= 4_096 and then Alignment <= 4_096 then
         S.Allocate (Slabs, Heap_Classes.Request_Size (Unsigned_64'Max (Bytes, Alignment)), Value);
         return (if Value.Value.Success then Unsigned_64 (Value.Value.Position) else No_Allocation);
      end if;
      E.Allocate (Extents, E.Run_Length ((Bytes - 1) / E.Page_Bytes + 1),
                  E.Run_Length (Unsigned_64'Max (1, Alignment / E.Page_Bytes)), First);
      return (if First = E.No_Page then No_Allocation
              else Arena_Bytes + Unsigned_64 (First - 1) * E.Page_Bytes);
   end Allocate;
   function Release (Offset : Unsigned_64) return Unsigned_64 is
      Status : S.Release_Status;
      OK : Boolean;
   begin
      if Offset < Arena_Bytes then
         S.Release (Slabs, S.Offset (Offset), Status);
         return (if Status = S.Released then 1 else 0);
      elsif Offset < Total_Bytes and then Offset mod E.Page_Bytes = 0 then
         E.Release (Extents, E.Page_Id ((Offset - Arena_Bytes) / E.Page_Bytes + 1), OK);
         return (if OK then 1 else 0);
      end if;
      return 0;
   end Release;
   function Usable_Size (Offset : Unsigned_64) return Unsigned_64 is
   begin
      if Offset < Arena_Bytes then
         if S.Live (Slabs, S.Offset (Offset)) then
            return Unsigned_64 (Heap_Classes.Stride (S.Class_Of (Slabs, S.Page_Of (S.Offset (Offset)))));
         end if;
      elsif Offset < Total_Bytes and then Offset mod E.Page_Bytes = 0 then
         return Unsigned_64 (E.Length (Extents, E.Page_Id ((Offset - Arena_Bytes) / E.Page_Bytes + 1))) * E.Page_Bytes;
      end if;
      return 0;
   end Usable_Size;
end Heap_Runtime;
