--  The Linux preview's places: the host's own directories (a development
--  stand-in for the CuBit filesystem service; no CuBit authority applies).
with Ada.Calendar.Conversions;
with Ada.Directories;
with Ada.Environment_Variables;

package body CCL_Places is
   use Interfaces;
   package Dirs renames Ada.Directories;
   use type Dirs.File_Kind;
   MS_PER_SECOND : constant := 1_000;

   --  CCL_PREVIEW_PLACE, or the preview's working directory.
   function Home return String is
     (if Ada.Environment_Variables.Exists ("CCL_PREVIEW_PLACE")
      then Ada.Environment_Variables.Value ("CCL_PREVIEW_PLACE")
      else Dirs.Current_Directory);

   procedure List
     (Path : String; Entries : out Listing; Count : out Listed_Count;
      Total : out Natural; Result : out Result_Kind)
   is
      Search : Dirs.Search_Type;
      Item : Dirs.Directory_Entry_Type;
   begin
      Entries := [others => <>];
      Count := 0;
      Total := 0;
      if not Dirs.Exists (Path) or else Dirs.Kind (Path) /= Dirs.Directory then
         Result := Not_Found; return;
      end if;
      Dirs.Start_Search (Search, Path, "");
      while Dirs.More_Entries (Search) loop
         Dirs.Get_Next_Entry (Search, Item);
         declare
            Name : constant String := Dirs.Simple_Name (Item);
         begin
            if Name /= "." and then Name /= ".." then
               Total := Total + 1;
               if Count < Files.MAXIMUM_LISTED and then Name'Length in 1 .. Files.MAXIMUM_NAME then
                  Count := Count + 1;
                  Entries (Count).Name (1 .. Name'Length) := Name;
                  Entries (Count).Name_Length := Name'Length;
                  Entries (Count).Kind :=
                    (case Dirs.Kind (Item) is
                        when Dirs.Ordinary_File => Files.File,
                        when Dirs.Directory => Files.Directory,
                        when Dirs.Special_File => Files.Other);
                  if Dirs.Kind (Item) = Dirs.Ordinary_File then
                     Entries (Count).Size := Unsigned_64 (Dirs.Size (Item));
                  end if;
                  declare
                     Seconds : constant Long_Long_Integer := Long_Long_Integer
                       (Ada.Calendar.Conversions.To_Unix_Time (Dirs.Modification_Time (Item)));
                  begin
                     if Seconds > 0 then
                        Entries (Count).Modified_Ms := Unsigned_64 (Seconds) * MS_PER_SECOND;
                     end if;
                  end;
               end if;
            end if;
         end;
      end loop;
      Dirs.End_Search (Search);
      Result := Listed_All;
   exception
      when Dirs.Use_Error => Result := Access_Denied;
      when others => Result := Failed;
   end List;
end CCL_Places;
