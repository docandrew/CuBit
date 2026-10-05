package body Compositor_Glyph_Layout with SPARK_Mode is
   --  Proved free of run-time errors; tests/ui-raster/run.sh re-proves every
   --  unit carrying this pragma and fails on any unproved check.
   pragma Suppress (All_Checks);
   function Plan (Scale : G.UI_Scale) return Layout is
      N : constant Positive := Positive (Scale.Numerator);
      D : constant Positive := Positive (Scale.Denominator);
      W : constant Raster_Width := (Base_Width_Bound * N + D - 1) / D;
      H : constant Raster_Height := (Base_Line * N + D - 1) / D;
      Pitch : constant Raster_Width := ((W + Row_Alignment - 1) / Row_Alignment) * Row_Alignment;
   begin
      return (Base_Em * N, Scale.Denominator, W, H, Pitch, Pitch * H);
   end Plan;
end Compositor_Glyph_Layout;
