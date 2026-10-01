package body Desktop_Composition with SPARK_Mode is
   function End_At (Origin, Extent : Natural) return Natural is
     (Origin + Natural'Min (Extent, Natural'Last - Origin));

   function Plan
     (Target_Width, Target_Height, Source_Width, Source_Height : Natural;
      Destination : Rectangle; Clipped : Boolean; Clip : Rectangle)
      return Blit_Plan
   is
      Left : Natural := Destination.X;
      Top : Natural := Destination.Y;
      Right, Bottom : Natural;
      W : constant Natural := Natural'Min (Destination.W, Source_Width);
      H : constant Natural := Natural'Min (Destination.H, Source_Height);
   begin
      if Left >= Target_Width or else Top >= Target_Height then
         return (others => 0);
      end if;
      Right := Left + Natural'Min (W, Target_Width - Left);
      Bottom := Top + Natural'Min (H, Target_Height - Top);
      if Clipped then
         Left := Natural'Max (Left, Clip.X);
         Top := Natural'Max (Top, Clip.Y);
         Right := Natural'Min (Right, End_At (Clip.X, Clip.W));
         Bottom := Natural'Min (Bottom, End_At (Clip.Y, Clip.H));
      end if;
      if Left >= Right or else Top >= Bottom then
         return (others => 0);
      end if;
      return (Left, Top, Left - Destination.X, Top - Destination.Y,
              Right - Left, Bottom - Top);
   end Plan;
end Desktop_Composition;
