with Vulkan_Backdrop_Binding;
package body Vulkan_Backdrop_Test_Bridge is
   function Draw
     (Borrowed : System.Address; W, H, SW, SH, Mode, L, T, R, B : Interfaces.C.int)
      return Interfaces.C.int is
      package V renames Vulkan_Backdrop_Binding;
      Result : V.Outcome;
      use type Interfaces.C.int;
   begin
      if W not in 1 .. 65535 or H not in 1 .. 65535 or SW not in 1 .. 65535 or SH not in 1 .. 65535 or
        Mode not in 0 .. 2 or L not in 0 .. 65535 or T not in 0 .. 65535 or R not in 0 .. 65535 or B not in 0 .. 65535
      then return 2; end if;
      V.Draw_Output (Borrowed, V.B.G.Physical_Extent (W), V.B.G.Physical_Extent (H),
        V.B.G.Physical_Extent (SW), V.B.G.Physical_Extent (SH), V.B.S.Placement'Val (Mode),
        (V.B.G.Pixel_Edge (L), V.B.G.Pixel_Edge (T), V.B.G.Pixel_Edge (R), V.B.G.Pixel_Edge (B)), Result);
      return V.Outcome'Pos (Result);
   end Draw;
end Vulkan_Backdrop_Test_Bridge;
