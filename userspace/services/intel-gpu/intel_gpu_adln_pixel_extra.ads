with Interfaces; use Interfaces;
with System;
with Intel_GPU_ADLN_Vertex_Fetch;
package Intel_GPU_ADLN_Pixel_Extra with SPARK_Mode is
   -- Intel TGL Vol2d68-73. Bit9 remains reserved per PRM, even though
   -- Mesa's shared packer calls it SimplePSHint; this probe never sets it.
   type B1 is mod 2 ** 1 with Size => 1;
   type B2 is mod 2 ** 2 with Size => 2;
   type B6 is mod 2 ** 6 with Size => 6;
   type Control is record
      Input_Coverage : B2 := 0;
      Has_UAV : B1 := 0;
      Pulls_Bary : B1 := 0;
      Per_Coarse_Pixel : B1 := 0;
      Computes_Stencil : B1 := 0;
      Per_Sample : B1 := 0;
      Disable_Alpha_Coverage : B1 := 0;
      Attributes : B1 := 0;
      Reserved_9 : B2 := 0;
      Evaluate_Message : B1 := 0;
      Reserved_12 : B6 := 0;
      Sample_Offsets : B1 := 0;
      Nonperspective_Coefficients : B1 := 0;
      Perspective_Coefficients : B1 := 0;
      Depth_W_Coefficients : B1 := 0;
      Requested_Coarse_Size : B1 := 0;
      Source_W : B1 := 0;
      Source_Depth : B1 := 0;
      Force_Depth : B1 := 0;
      Computed_Depth_Mode : B2 := 0;
      Kills_Pixel : B1 := 0;
      Output_Mask : B1 := 0;
      No_RT_Write : B1 := 0;
      Valid : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Control use record
      Input_Coverage at 0 range 0 .. 1;
      Has_UAV at 0 range 2 .. 2;
      Pulls_Bary at 0 range 3 .. 3;
      Per_Coarse_Pixel at 0 range 4 .. 4;
      Computes_Stencil at 0 range 5 .. 5;
      Per_Sample at 0 range 6 .. 6;
      Disable_Alpha_Coverage at 0 range 7 .. 7;
      Attributes at 0 range 8 .. 8;
      Reserved_9 at 0 range 9 .. 10;
      Evaluate_Message at 0 range 11 .. 11;
      Reserved_12 at 0 range 12 .. 17;
      Sample_Offsets at 0 range 18 .. 18;
      Nonperspective_Coefficients at 0 range 19 .. 19;
      Perspective_Coefficients at 0 range 20 .. 20;
      Depth_W_Coefficients at 0 range 21 .. 21;
      Requested_Coarse_Size at 0 range 22 .. 22;
      Source_W at 0 range 23 .. 23;
      Source_Depth at 0 range 24 .. 24;
      Force_Depth at 0 range 25 .. 25;
      Computed_Depth_Mode at 0 range 26 .. 27;
      Kills_Pixel at 0 range 28 .. 28;
      Output_Mask at 0 range 29 .. 29;
      No_RT_Write at 0 range 30 .. 30;
      Valid at 0 range 31 .. 31;
   end record;
   function Encode (V : Control) return Unsigned_32 is
     (Unsigned_32 (V.Input_Coverage) or
      Shift_Left (Unsigned_32 (V.Has_UAV), 2) or
      Shift_Left (Unsigned_32 (V.Pulls_Bary), 3) or
      Shift_Left (Unsigned_32 (V.Per_Coarse_Pixel), 4) or
      Shift_Left (Unsigned_32 (V.Computes_Stencil), 5) or
      Shift_Left (Unsigned_32 (V.Per_Sample), 6) or
      Shift_Left (Unsigned_32 (V.Disable_Alpha_Coverage), 7) or
      Shift_Left (Unsigned_32 (V.Attributes), 8) or
      Shift_Left (Unsigned_32 (V.Reserved_9), 9) or
      Shift_Left (Unsigned_32 (V.Evaluate_Message), 11) or
      Shift_Left (Unsigned_32 (V.Reserved_12), 12) or
      Shift_Left (Unsigned_32 (V.Sample_Offsets), 18) or
      Shift_Left (Unsigned_32 (V.Nonperspective_Coefficients), 19) or
      Shift_Left (Unsigned_32 (V.Perspective_Coefficients), 20) or
      Shift_Left (Unsigned_32 (V.Depth_W_Coefficients), 21) or
      Shift_Left (Unsigned_32 (V.Requested_Coarse_Size), 22) or
      Shift_Left (Unsigned_32 (V.Source_W), 23) or
      Shift_Left (Unsigned_32 (V.Source_Depth), 24) or
      Shift_Left (Unsigned_32 (V.Force_Depth), 25) or
      Shift_Left (Unsigned_32 (V.Computed_Depth_Mode), 26) or
      Shift_Left (Unsigned_32 (V.Kills_Pixel), 28) or
      Shift_Left (Unsigned_32 (V.Output_Mask), 29) or
      Shift_Left (Unsigned_32 (V.No_RT_Write), 30) or
      Shift_Left (Unsigned_32 (V.Valid), 31));
   type Words is array (Natural range 0 .. 1) of Unsigned_32;
   -- Constant-red shader, zero SBE attributes, no depth/stencil/UAV output,
   -- no discard, sample-mask input/output or per-sample/coarse dispatch.
   -- Compiler metadata assertions in the hosted oracle enforce this profile.
   Enabled : constant Words :=
     [Intel_GPU_ADLN_Vertex_Fetch.Encode
        (Intel_GPU_ADLN_Vertex_Fetch.Header'
           (Length => 0, Subopcode => 16#4F#, others => <>)),
      Encode (Control'(Valid => 1, others => <>))];
end Intel_GPU_ADLN_Pixel_Extra;
