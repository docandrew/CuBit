------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Libc_Dirty_Map with SPARK_Mode is

   Golden : constant Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;
   Hash_Shift : constant := 40;

   function Hash (Tag, Page : Unsigned_32) return Map_Index is
     (Map_Index (Shift_Right ((Shift_Left (Unsigned_64 (Tag), 32) or Unsigned_64 (Page))
                              * Golden, Hash_Shift) and (Map_Slots - 1)));

   procedure Find (M : in out Map; Entries : Entry_Table; Tag, Page : Unsigned_32;
                   Found : out Integer)
   is
      I : Map_Index := Hash (Tag, Page);
   begin
      Found := -1;
      for N in 1 .. Map_Slots loop
         declare
            V : constant Map_Value := M.Values (I);
         begin
            exit when V = Empty;
            if V > 0 and then Entries (V - 1).Tag = Tag and then Entries (V - 1).Page = Page then
               if Entries (V - 1).Sequence /= 0 then
                  Found := V - 1;
               else
                  M.Values (I) := Tombstone;          --  the service took it
                  if M.Tombstones < Map_Slots then
                     M.Tombstones := M.Tombstones + 1;
                  end if;
               end if;
               return;
            end if;
         end;
         I := (I + 1) mod Map_Slots;
      end loop;
   end Find;

   procedure Remember (M : in out Map; Tag, Page : Unsigned_32; E : Entry_Index) is
      I : Map_Index := Hash (Tag, Page);
   begin
      for N in 1 .. Map_Slots loop
         if M.Values (I) <= Empty then
            if M.Values (I) = Tombstone and then M.Tombstones > 0 then
               M.Tombstones := M.Tombstones - 1;
            end if;
            M.Values (I) := E + 1;
            return;
         end if;
         I := (I + 1) mod Map_Slots;
      end loop;
   end Remember;

   procedure Rebuild (M : out Map; Entries : Entry_Table) is
   begin
      M := (Values => [others => Empty], Tombstones => 0, Used => 0);
      for E in Entry_Index loop
         if Entries (E).Sequence /= 0 then
            Remember (M, Entries (E).Tag, Entries (E).Page, E);
            if M.Used < FQ.Dirty_Entries then
               M.Used := M.Used + 1;
            end if;
         end if;
      end loop;
   end Rebuild;

end CuBit.Libc_Dirty_Map;
