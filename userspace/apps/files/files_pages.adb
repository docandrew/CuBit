with Files_Limits; use Files_Limits;

package body Files_Pages with SPARK_Mode is
   use Files_Listing;

   COLON : constant Unsigned_8 := Character'Pos (':');

   function Kind_Of (Wire : Unsigned_8) return Entry_Kind is
     (case Wire is
         when DP.Kind_File => File_Kind,
         when DP.Kind_Directory => Directory_Kind,
         when DP.Kind_Symlink => Link_Kind,
         when others => Unknown_Kind);

   function Has_Colon (Name : DP.Name_Bytes; Length : DP.Name_Length) return Boolean is
     (for some K in 1 .. Length => Name (K) = COLON);

   function Facts_Of (Item : DP.Facts) return Entry_Facts is
     (Kind => Kind_Of (Item.Kind),
      Size => Byte_Size (Item.Size),
      Size_Known => (Item.Valid and DP.Valid_Size) /= 0,
      Modified => Time_Ms (Item.Modified),
      Modified_Known => (Item.Valid and DP.Valid_Times) /= 0,
      Changed => Time_Ms (Item.Changed),
      Changed_Known => (Item.Valid and DP.Valid_Times) /= 0,
      Mode => Item.Mode,
      Mode_Known => (Item.Valid and DP.Valid_Mode) /= 0,
      Owner => Item.Owner,
      Group => Item.Group,
      Owner_Known => (Item.Valid and DP.Valid_Owner) /= 0,
      Object => (if (Item.Valid and DP.Valid_Object) /= 0 then Object_Identity (Item.Object)
                 else NO_OBJECT));

   procedure Take_Page
     (L : in out Listing; Page : Page_Image;
      Position : in out Cursor_State; Result : out Page_Result)
   is
      Copy : constant DP.Page := [for I in DP.Page_Index => Page (I + 1)];
      Page_Valid, Ended, OK : Boolean;
      Count : DP.Entry_Count;
      Used : DP.Used_Bytes;
      Resume, Stamp : Unsigned_64;
      At_Entry, Next : Natural;
      Item : DP.Facts;
      Raw : DP.Name_Bytes;
      Length : DP.Name_Length;
      Bytes : Natural := 0;
   begin
      Result := Page_Malformed;
      DP.Check (Copy, Page_Valid, Count, Used, Ended, Resume, Stamp);
      if not Page_Valid then
         return;
      end if;
      --  An enumeration that does not move on would never end.
      if not Ended and then (Count = 0 or else (Position.Started and then Resume = Position.Last)) then
         return;
      end if;
      --  Everything checked before anything is taken.
      At_Entry := DP.Header_Bytes;
      for Ordinal in 1 .. Count loop
         pragma Loop_Invariant (Bytes <= (Ordinal - 1) * MAXIMUM_NAME_BYTES);
         DP.Get (Copy, At_Entry, Used, Item, Raw, Length, Next, OK);
         if not OK or else Has_Colon (Raw, Length) then
            return;
         end if;
         Bytes := Bytes + Length;
         At_Entry := Next;
      end loop;
      if Count > L.Capacity - L.Count or else Bytes > L.Arena_Bytes - L.Used then
         Result := Listing_Full;
         return;
      end if;
      At_Entry := DP.Header_Bytes;
      for Ordinal in 1 .. Count loop
         pragma Loop_Invariant
           (Valid (L) and then Count - Ordinal + 1 <= L.Capacity - L.Count
            and then Bytes <= L.Arena_Bytes - L.Used and then L.Count >= L.Count'Loop_Entry);
         DP.Get (Copy, At_Entry, Used, Item, Raw, Length, Next, OK);
         --  The first pass took every entry; an entry's name must still fit
         --  what it counted.
         exit when not OK or else Length > Bytes;
         declare
            Known : constant DP.Name_Length := Length;
            Text : Name_Bytes (1 .. Known);
         begin
            for K in Text'Range loop
               Text (K) := Raw (K);
            end loop;
            Append (L, Text, Facts_Of (Item));
         end;
         Bytes := Bytes - Length;
         At_Entry := Next;
      end loop;
      Position := (Last => Resume, Started => True);
      Result := (if Ended then Page_Last else Page_Taken);
   end Take_Page;
end Files_Pages;
