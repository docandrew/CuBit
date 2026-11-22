with Vulkan_Copy_Binding;
package body Vulkan_Copy_Test_Bridge is
   function Draw (Borrowed : System.Address; Value : access constant Input)
     return Interfaces.C.int is
      use type Interfaces.C.int;
      R : Vulkan_Copy_Binding.Outcome;
      V : Input renames Value.all;
   begin
      pragma Assert (Input'Size = 13 * 32);
      Vulkan_Copy_Binding.Draw_Client
        (Borrowed, Natural (V.TW), Natural (V.TH), Natural (V.SW), Natural (V.SH),
         (Natural (V.X), Natural (V.Y), Natural (V.W), Natural (V.H)), V.Clipped /= 0,
         (Natural (V.CX), Natural (V.CY), Natural (V.CW), Natural (V.CH)), R);
      return Vulkan_Copy_Binding.Outcome'Pos (R);
   end Draw;
end Vulkan_Copy_Test_Bridge;
