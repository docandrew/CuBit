pragma Ada_2022;
package body Retained_Record_Blocks is
   use System.Storage_Elements;
   use type System.Address;
   use type Pointers.Object_Pointer;

   function Find (Storage : Store; Index : Slot) return Element_Access is
      Page : constant Pointers.Object_Pointer :=
        Storage.Blocks (Index / Records_Per_Block);
   begin
      if Page = null then return null; end if;
      return Page.all (Index mod Records_Per_Block)'Unchecked_Access;
   end Find;

   function Allocated_Blocks (Storage : Store) return Natural is (Storage.Count);

   procedure Ensure
     (Storage : in out Store; Index : Slot; Initial : Element;
      Block_Limit : Natural; Value : out Element_Access;
      Result : out Allocation_Result)
   is
      Bytes : constant Storage_Count := Block'Object_Size / System.Storage_Unit;
      Address : System.Address;
      Page : Pointers.Object_Pointer;
   begin
      Value := Find (Storage, Index);
      if Value /= null then Result := Existing; return; end if;
      if Storage.Count >= Block_Limit then Result := At_Quota; return; end if;
      Address := Allocate (Bytes, Block'Alignment);
      if Address = System.Null_Address then
         Result := Out_Of_Memory;
         return;
      end if;
      if To_Integer (Address) mod Integer_Address (Block'Alignment) /= 0
        or else To_Integer (Address) > Integer_Address'Last -
          Integer_Address (Bytes - 1)
      then
         -- Do not dereference or free an invalid allocator response.
         Result := Invalid_Backing;
         return;
      end if;
      Page := Pointers.To_Pointer (Address);
      -- Initialize in place: a whole-block aggregate can materialize a page-
      -- sized temporary on the small kernel stack before copying it out.
      for I in Page.all'Range loop
         Page.all (I) := Initial;
      end loop;
      Storage.Blocks (Index / Records_Per_Block) := Page;
      Storage.Count := Storage.Count + 1;
      Value := Find (Storage, Index);
      Result := Added;
   end Ensure;

   procedure Next_Present
     (Storage : Store; From : Slot; Index : out Slot; Found : out Boolean)
   is
   begin
      Index := From;
      Found := False;
      for B in From / Records_Per_Block .. Storage.Blocks'Last loop
         if Storage.Blocks (B) /= null then
            Index := Natural'Max (From, B * Records_Per_Block);
            Found := True;
            return;
         end if;
      end loop;
   end Next_Present;
end Retained_Record_Blocks;
