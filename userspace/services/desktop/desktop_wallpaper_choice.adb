with Desktop_Wallpaper_Store;
package body Desktop_Wallpaper_Choice is
   package Store renames Desktop_Wallpaper_Store;

   procedure Choose
     (Requested : A.Preferences; Index : out Catalog.Wallpaper_Count; Result : out Outcome) is
   begin
      Index := Catalog.No_Wallpaper;
      Result := Kept;
      if Requested.Kind = A.Solid then
         return;
      elsif Catalog.Count = 0 then
         Result := No_Catalog;
         return;
      elsif Requested.Image.Length = 0 then
         Index := Catalog.Default_Wallpaper;
         Result := Defaulted;
         return;
      end if;
      Index := Catalog.Find (Requested.Image);
      if Index = Catalog.No_Wallpaper then
         Index := Catalog.Default_Wallpaper;
         Result := Unknown_Wallpaper;
      end if;
   end Choose;

   procedure Settle (Requested : in out A.Preferences; Result : out Outcome) is
      Index : Catalog.Wallpaper_Count;
   begin
      Choose (Requested, Index, Result);
      if Index /= Catalog.No_Wallpaper then
         Requested.Image := Catalog.Name (Index);
      end if;
   end Settle;

   function Shown_Index (Chosen : Catalog.Wallpaper_Count) return Catalog.Wallpaper_Count is
     (if Chosen = Catalog.No_Wallpaper then Catalog.No_Wallpaper
      elsif Store.Ready (Chosen) then Chosen
      elsif Store.Ready (Catalog.Default_Wallpaper) then Catalog.Default_Wallpaper
      else Catalog.No_Wallpaper);

   function Style_For (Requested : A.Preferences; Index : Catalog.Wallpaper_Count)
     return Desktop_Backdrop_Style.Backdrop
   is
      Result : Desktop_Backdrop_Style.Backdrop :=
        (Scheme => Requested.Scheme, Position => Requested.Position,
         Color => (if Requested.Kind = A.Solid then Requested.Color else A.Slate),
         others => <>);
   begin
      if Store.Ready (Index) then
         Result.Image := Index;
         Result.Width := Store.Width (Index);
         Result.Height := Store.Height (Index);
      end if;
      return Result;
   end Style_For;
end Desktop_Wallpaper_Choice;
