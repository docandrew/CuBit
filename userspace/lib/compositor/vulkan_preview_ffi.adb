with Interfaces;
with Compositor_Transform;
package body Vulkan_Preview_FFI with SPARK_Mode => Off is
   use type P.A.Word, P.S.Wide;
   type Placement_ABI is record
      Left, Top : Interfaces.Integer_32;
      Width, Height : Interfaces.Unsigned_32;
   end record with Convention => C, Size => 128, Alignment => 4;
   for Placement_ABI use record
      Left at 0 range 0 .. 31;
      Top at 4 range 0 .. 31;
      Width at 8 range 0 .. 31;
      Height at 12 range 0 .. 31;
   end record;
   function Submit
     (Borrowed : System.Address; Draw : access constant P.A.Draw;
      Coefficients : access constant Compositor_Transform.Coefficients;
      Width, Height : P.A.Word; Placement : access constant Placement_ABI)
      return P.A.Word
     with Import, Convention => C, External_Name => "cubit_vulkan_record_preview";
   procedure Record_Draw
     (Borrowed : System.Address; Plan : P.Result;
      Width, Height : P.G.Physical_Extent; Accepted : out Boolean)
   is
      L : constant P.S.Wide := P.S.Left (Plan.Placement);
      T : constant P.S.Wide := P.S.Top (Plan.Placement);
   begin
      Accepted := False;
      if L not in P.S.Wide (Interfaces.Integer_32'First) .. P.S.Wide (Interfaces.Integer_32'Last)
        or else T not in P.S.Wide (Interfaces.Integer_32'First) .. P.S.Wide (Interfaces.Integer_32'Last)
      then return; end if;
      declare
         Draw : aliased constant P.A.Draw := Plan.Transform;
         Coefficients : aliased constant Compositor_Transform.Coefficients :=
           Compositor_Transform.Build (Draw, Width, Height);
         Placement : aliased constant Placement_ABI :=
           (Interfaces.Integer_32 (L), Interfaces.Integer_32 (T),
            Interfaces.Unsigned_32 (P.S.Draw_Width (Plan.Placement)),
            Interfaces.Unsigned_32 (P.S.Draw_Height (Plan.Placement)));
      begin
         Accepted := Submit (Borrowed, Draw'Access, Coefficients'Access,
           P.A.Word (Width), P.A.Word (Height), Placement'Access) = 0;
      end;
   end Record_Draw;
end Vulkan_Preview_FFI;
