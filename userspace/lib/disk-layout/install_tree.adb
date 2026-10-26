------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body Install_Tree with SPARK_Mode is

   function Name_Allowed (Name : String) return Boolean is
     (Name'Length in Name_Length
      and then Name /= "." and then Name /= ".."
      and then (for all C of Name => C in ' ' .. '~' and then C /= '/'));

   procedure Add
     (Item : in out Tree; Kind : Entry_Kind; Parent : Parent_Index;
      Name : String; Size : File_Size; Result : out Add_Result) is
   begin
      if Item.Count = Maximum_Entries then
         Result := Tree_Full;
         return;
      elsif not Name_Allowed (Name) then
         Result := Bad_Name;
         return;
      elsif not Parent_Valid (Item, Parent) then
         Result := Bad_Parent;
         return;
      end if;
      if Name'Length > Pool_Bytes - Item.Used then
         Result := Tree_Full;
         return;
      end if;
      for I in 1 .. Item.Count loop
         if Item.Entries (I).Parent = Parent and then Name_Of (Item, I) = Name then
            Result := Bad_Name;
            return;
         end if;
      end loop;
      Item.Count := Item.Count + 1;
      Item.Entries (Item.Count) :=
        (Kind => Kind, Parent => Parent, First => Item.Used + 1, Length => Name'Length,
         Size => (if Kind = Directory then 0 else Size));
      Item.Names (Item.Used + 1 .. Item.Used + Name'Length) := Name;
      Item.Used := Item.Used + Name'Length;
      Result := Added;
   end Add;

end Install_Tree;
