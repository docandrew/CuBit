with System.Address_To_Access_Conversions;
package body DMA_Record_Blocks is
   use Interfaces;
   use System.Storage_Elements;
   use type System.Address;
   type Block is array (1 .. Records_Per_Block) of aliased Node;
   package Pointers is new System.Address_To_Access_Conversions (Block);
   function Block_Bytes return Unsigned_64 is
     ((Unsigned_64 (Block'Object_Size / System.Storage_Unit) +
       Unsigned_64 (Allocation_Granule) - 1) / Unsigned_64 (Allocation_Granule) *
       Unsigned_64 (Allocation_Granule));
   function Metadata_Bytes (Object : Pool) return Unsigned_64 is (Object.Bytes);
   function Value (Item : not null Reference) return Element is
   begin
      if not Item.Leased then raise Metadata_Error; end if;
      return Item.Data;
   end Value;
   procedure Set_Value (Item : not null Reference; Data : Element) is
   begin
      if not Item.Leased or else Item.Linked then raise Metadata_Error; end if;
      Item.Data := Data;
   end Set_Value;
   function Empty (Object : List) return Boolean is (Object.First = null);
   procedure Reserve
     (Object : in out Pool; Initial : Element; Byte_Limit : Unsigned_64;
      Item : out Reference; Status : out Result) is
      Address : System.Address;
      Page : Pointers.Object_Pointer;
   begin
      Item := null;
      if Object.Free = null then
         if Object.Bytes > Byte_Limit or else
           Block_Bytes > Byte_Limit - Object.Bytes
         then Status := Metadata_Quota; return; end if;
         Address := Allocate (Storage_Count (Block_Bytes), Block'Alignment);
         if Address = System.Null_Address then Status := No_Memory; return; end if;
         if To_Integer (Address) mod Block'Alignment /= 0 or else
           To_Integer (Address) > Integer_Address'Last - Integer_Address (Block_Bytes - 1)
         then Status := Invalid_Backing; return; end if;
         Page := Pointers.To_Pointer (Address);
         for I in Page.all'Range loop
            Page (I).Data := Initial;
            Page (I).Leased := False;
            Page (I).Linked := False;
            Page (I).Owner := Object'Address;
            Page (I).Next := Object.Free;
            Object.Free := Page (I)'Unchecked_Access;
         end loop;
         Object.Bytes := Object.Bytes + Block_Bytes;
      end if;
      Item := Object.Free;
      Object.Free := Item.Next;
      Item.Next := null;
      Item.Data := Initial;
      Item.Leased := True;
      Status := Ready;
   end Reserve;
   procedure Release (Object : in out Pool; Item : in out Reference) is
   begin
      if Item = null or else not Item.Leased or else Item.Linked or else
        Item.Owner /= Object'Address
      then raise Metadata_Error; end if;
      Item.Leased := False;
      Item.Next := Object.Free;
      Object.Free := Item;
      Item := null;
   end Release;
   procedure Push (Object : in out List; Item : not null Reference) is
   begin
      if not Item.Leased or else Item.Linked or else Item.Next /= null
      then raise Metadata_Error; end if;
      Item.Linked := True;
      if Object.Last = null then Object.First := Item;
      else Object.Last.Next := Item; end if;
      Object.Last := Item;
   end Push;
   procedure Pop (Object : in out List; Item : out Reference) is
   begin
      Item := Object.First;
      if Item = null then return; end if;
      Object.First := Item.Next;
      if Object.First = null then Object.Last := null; end if;
      Item.Next := null;
      Item.Linked := False;
   end Pop;
   procedure Move (Source, Target : in out List) is
   begin
      if Source'Address = Target'Address then raise Metadata_Error; end if;
      if Source.First = null then return; end if;
      if Target.Last = null then Target.First := Source.First;
      else Target.Last.Next := Source.First; end if;
      Target.Last := Source.Last;
      Source.First := null;
      Source.Last := null;
   end Move;
end DMA_Record_Blocks;
