with System.Address_To_Access_Conversions;
with System.Storage_Elements;
package body Owner_Record_Slabs is
   use Interfaces;
   use System.Storage_Elements;
   use type System.Address;
   Page_Bytes : constant Unsigned_64 := 4096;
   Bytes : Unsigned_64 := 0;
   package Arenas is new System.Address_To_Access_Conversions (Arena_Data);
   package Slabs is new System.Address_To_Access_Conversions (Slab);
   function Metadata_Bytes return Unsigned_64 is (Bytes);
   function Empty (Object : Arena) return Boolean is
     (Object = null or else Object.Live = 0);
   function Empty (Object : List) return Boolean is (Object.First = null);
   procedure Push (Object : in out List; Item : not null Reference) is
   begin
      if not Item.Leased or else Item.Linked or else Item.Next /= null then
         raise Program_Error with "Invalid slab list push";
      end if;
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
      if Source'Address = Target'Address then raise Program_Error with "Slab self move"; end if;
      if Source.First = null then return; end if;
      if Target.Last = null then Target.First := Source.First;
      else Target.Last.Next := Source.First; end if;
      Target.Last := Source.Last;
      Source.First := null;
      Source.Last := null;
   end Move;
   procedure Get_Page
     (Owner, Limit : Unsigned_64; Page : out System.Address; OK : out Boolean) is
   begin
      Page := System.Null_Address;
      OK := Owner /= 0 and then Bytes <= Limit and then Page_Bytes <= Limit - Bytes;
      if not OK then return; end if;
      Allocate_Page (Owner, Page, OK);
      if not OK then return; end if;
      if Page = System.Null_Address or else To_Integer (Page) mod 4096 /= 0 then
         raise Program_Error with "Invalid slab backing";
      end if;
      Bytes := Bytes + Page_Bytes;
   end Get_Page;
   procedure Put_Page (Page : System.Address) is
   begin
      Release_Page (Page);
      Bytes := Bytes - Page_Bytes;
   end Put_Page;
   procedure Open
     (Owner, Byte_Limit : Unsigned_64; Object : out Arena; OK : out Boolean) is
      Page : System.Address;
   begin
      Object := null;
      OK := Arena_Data'Object_Size <= 4096 * System.Storage_Unit and then
        Slab'Object_Size <= 4096 * System.Storage_Unit and then
        Arena_Data'Alignment <= 4096 and then Slab'Alignment <= 4096;
      if not OK then return; end if;
      Get_Page (Owner, Byte_Limit, Page, OK);
      if not OK then return; end if;
      Object := Arena (Arenas.To_Pointer (Page));
      Object.all := (Owner => Owner, Available => null, Live => 0, Accepting => True);
   end Open;
   procedure Close (Object : in out Arena) is
      Saved : constant Arena := Object;
   begin
      Object := null;
      if Saved = null then return; end if;
      if not Saved.Accepting then raise Program_Error with "Closed slab arena"; end if;
      Saved.Accepting := False;
      if Saved.Live = 0 then Put_Page (Saved.all'Address); end if;
   end Close;
   procedure Unlink (Block : Slab_Access) is
      Owner : constant Arena := Block.Parent;
   begin
      if Block.Previous = null then Owner.Available := Block.Following;
      else Block.Previous.Following := Block.Following; end if;
      if Block.Following /= null then Block.Following.Previous := Block.Previous; end if;
      Block.Previous := null;
      Block.Following := null;
   end Unlink;
   procedure Link (Block : Slab_Access) is
      Owner : constant Arena := Block.Parent;
   begin
      Block.Previous := null;
      Block.Following := Owner.Available;
      if Owner.Available /= null then Owner.Available.Previous := Block; end if;
      Owner.Available := Block;
   end Link;
   procedure Reserve
     (Object : Arena; Initial : Element; Byte_Limit : Unsigned_64;
      Item : out Reference; OK : out Boolean) is
      Page : System.Address;
      Block : Slab_Access;
   begin
      Item := null;
      OK := Object /= null and then Object.Accepting and then Object.Live < Unsigned_64'Last;
      if not OK then return; end if;
      Block := Object.Available;
      if Block = null then
         Get_Page (Object.Owner, Byte_Limit, Page, OK);
         if not OK then return; end if;
         Block := Slab_Access (Slabs.To_Pointer (Page));
         Block.Parent := Object;
         Block.Previous := null;
         Block.Following := null;
         Block.Free := null;
         Block.Live := 0;
         for I in Block.Entries'Range loop
            Block.Entries (I).Data := Initial;
            Block.Entries (I).Parent := Block;
            Block.Entries (I).Leased := False;
            Block.Entries (I).Linked := False;
            Block.Entries (I).Next := Block.Free;
            Block.Free := Block.Entries (I)'Unchecked_Access;
         end loop;
         Link (Block);
      end if;
      Item := Block.Free;
      Block.Free := Item.Next;
      Item.Next := null;
      Item.Data := Initial;
      Item.Leased := True;
      Block.Live := Block.Live + 1;
      Object.Live := Object.Live + 1;
      if Block.Free = null then Unlink (Block); end if;
   end Reserve;
   function Value (Item : not null Reference) return Element is
   begin
      if not Item.Leased then raise Program_Error with "Unleased slab record"; end if;
      return Item.Data;
   end Value;
   procedure Set_Value (Item : not null Reference; Data : Element) is
   begin
      if not Item.Leased or else Item.Linked then raise Program_Error with "Linked/unleased slab write"; end if;
      Item.Data := Data;
   end Set_Value;
   procedure Release (Item : in out Reference) is
      Block : Slab_Access;
      Owner : Arena;
      Was_Full : Boolean;
   begin
      if Item = null or else not Item.Leased or else Item.Linked then
         raise Program_Error with "Invalid slab record release";
      end if;
      Block := Item.Parent;
      Owner := Block.Parent;
      Was_Full := Block.Free = null;
      Item.Leased := False;
      Item.Next := Block.Free;
      Block.Free := Item;
      Item := null;
      Block.Live := Block.Live - 1;
      Owner.Live := Owner.Live - 1;
      if Was_Full then Link (Block); end if;
      if Block.Live = 0 then
         Unlink (Block);
         Put_Page (Block.all'Address);
      end if;
      if Owner.Live = 0 and then not Owner.Accepting then
         Put_Page (Owner.all'Address);
      end if;
   end Release;
end Owner_Record_Slabs;
