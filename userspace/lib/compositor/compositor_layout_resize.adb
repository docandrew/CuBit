pragma Ada_2022;
package body Compositor_Layout_Resize with SPARK_Mode is
   function Carry_Settings
     (Saved, Fresh : L.Layout; Minimum : Minimum_Extent) return L.Layout
   is
      Result : L.Layout := Fresh;
      Next_X : Natural := 0;
   begin
      for I in 1 .. Fresh.Count loop
         if Carries (Saved, Fresh, I) then
            Result.Items (I).Geometry.Scale := Saved.Items (I).Geometry.Scale;
            Result.Items (I).Geometry.Rotation := Saved.Items (I).Geometry.Rotation;
         end if;
         declare
            Width  : constant Positive :=
              Logical_Extent (Upright_Width (Result.Items (I).Geometry),
                              Result.Items (I).Geometry.Scale);
            Height : constant Positive :=
              Logical_Extent (Upright_Height (Result.Items (I).Geometry),
                              Result.Items (I).Geometry.Scale);
         begin
            if Width < Minimum.Width or else Height < Minimum.Height then
               return Fresh;
            end if;
            Result.Items (I).Geometry.X := G.Output_Origin (Next_X);
            Result.Items (I).Geometry.Y := 0;
            Next_X := Next_X + Width;
         end;
         pragma Loop_Invariant (Result.Count = Fresh.Count);
         pragma Loop_Invariant (Next_X <= I * Largest_Logical);
         pragma Loop_Invariant
           (for all J in 1 .. Fresh.Count =>
              Result.Items (J).Display = Fresh.Items (J).Display and then
              Result.Items (J).Geometry.Width = Fresh.Items (J).Geometry.Width and then
              Result.Items (J).Geometry.Height = Fresh.Items (J).Geometry.Height);
         pragma Loop_Invariant
           (for all J in 1 .. I =>
              (if Carries (Saved, Fresh, J) then
                 Result.Items (J).Geometry.Scale = Saved.Items (J).Geometry.Scale and then
                 Result.Items (J).Geometry.Rotation = Saved.Items (J).Geometry.Rotation));
      end loop;
      if L.Validate (Result).Status = L.Accepted then
         return Result;
      end if;
      return Fresh;
   end Carry_Settings;
end Compositor_Layout_Resize;
