------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Libc_Net_Names with SPARK_Mode is

   procedure Index_Of (T : in out Table; Name : String; Index : out Name_Count) is
   begin
      for I in 1 .. T.Count loop
         if Same (T.Entries (I).Text (1 .. T.Entries (I).Length), Name) then
            Index := I;
            return;
         end if;
      end loop;
      if T.Count = Capacity then
         Index := 0;
         return;
      end if;
      T.Count := T.Count + 1;
      T.Entries (T.Count).Text := [others => ' '];
      T.Entries (T.Count).Text (1 .. Name'Length) := Name;
      T.Entries (T.Count).Length := Name'Length;
      Index := T.Count;
   end Index_Of;

end CuBit.Libc_Net_Names;
