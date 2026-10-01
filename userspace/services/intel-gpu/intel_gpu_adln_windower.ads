with Interfaces; use Interfaces;
with System;
with Intel_GPU_ADLN_Vertex_Fetch;
package Intel_GPU_ADLN_Windower with SPARK_Mode is
   -- Intel TGL Vol2d150-153. Force dispatch/kill must remain NORMAL.
   -- Bits27..30 reserved per PRM despite legacy fields in Mesa's packer.
   type B1 is mod 2 ** 1 with Size => 1;
   type B2 is mod 2 ** 2 with Size => 2;
   type B3 is mod 2 ** 3 with Size => 3;
   type B4 is mod 2 ** 4 with Size => 4;
   type B6 is mod 2 ** 6 with Size => 6;
   type Control is record
      Force_Kill : B2 := 0;
      Point_Rule : B1 := 0;
      Line_Stipple : B1 := 0;
      Polygon_Stipple : B1 := 0;
      Reserved_5 : B1 := 0;
      Line_AA_Width : B2 := 0;
      Endcap_AA_Width : B2 := 0;
      Reserved_10 : B1 := 0;
      Barycentric_Modes : B6 := 0;
      Position_ZW_Mode : B2 := 0;
      Force_Dispatch : B2 := 0;
      Early_Depth_Stencil : B2 := 0;
      Reserved_23 : B3 := 0;
      Legacy_Diamond_Lines : B1 := 0;
      Reserved_27 : B4 := 0;
      Statistics : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Control use record
      Force_Kill at 0 range 0 .. 1;
      Point_Rule at 0 range 2 .. 2;
      Line_Stipple at 0 range 3 .. 3;
      Polygon_Stipple at 0 range 4 .. 4;
      Reserved_5 at 0 range 5 .. 5;
      Line_AA_Width at 0 range 6 .. 7;
      Endcap_AA_Width at 0 range 8 .. 9;
      Reserved_10 at 0 range 10 .. 10;
      Barycentric_Modes at 0 range 11 .. 16;
      Position_ZW_Mode at 0 range 17 .. 18;
      Force_Dispatch at 0 range 19 .. 20;
      Early_Depth_Stencil at 0 range 21 .. 22;
      Reserved_23 at 0 range 23 .. 25;
      Legacy_Diamond_Lines at 0 range 26 .. 26;
      Reserved_27 at 0 range 27 .. 30;
      Statistics at 0 range 31 .. 31;
   end record;
   function Encode (V : Control) return Unsigned_32 is
     (Unsigned_32 (V.Force_Kill) or
      Shift_Left (Unsigned_32 (V.Point_Rule), 2) or
      Shift_Left (Unsigned_32 (V.Line_Stipple), 3) or
      Shift_Left (Unsigned_32 (V.Polygon_Stipple), 4) or
      Shift_Left (Unsigned_32 (V.Reserved_5), 5) or
      Shift_Left (Unsigned_32 (V.Line_AA_Width), 6) or
      Shift_Left (Unsigned_32 (V.Endcap_AA_Width), 8) or
      Shift_Left (Unsigned_32 (V.Reserved_10), 10) or
      Shift_Left (Unsigned_32 (V.Barycentric_Modes), 11) or
      Shift_Left (Unsigned_32 (V.Position_ZW_Mode), 17) or
      Shift_Left (Unsigned_32 (V.Force_Dispatch), 19) or
      Shift_Left (Unsigned_32 (V.Early_Depth_Stencil), 21) or
      Shift_Left (Unsigned_32 (V.Reserved_23), 23) or
      Shift_Left (Unsigned_32 (V.Legacy_Diamond_Lines), 26) or
      Shift_Left (Unsigned_32 (V.Reserved_27), 27) or
      Shift_Left (Unsigned_32 (V.Statistics), 31));
   type Words is array (Natural range 0 .. 1) of Unsigned_32;
   -- Fixed fragment shader has no barycentric inputs or early-fragment-test
   -- requirement. PS validity and writable RT state control normal dispatch.
   Initial : constant Words :=
     [Intel_GPU_ADLN_Vertex_Fetch.Encode
        (Intel_GPU_ADLN_Vertex_Fetch.Header'
           (Subopcode => 16#14#, others => <>)),
      Encode (Control'(Statistics => 1, others => <>))];
end Intel_GPU_ADLN_Windower;
