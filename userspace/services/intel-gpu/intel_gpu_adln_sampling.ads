with Interfaces; use Interfaces;
with System;
with Intel_GPU_ADLN_Vertex_Fetch;
package Intel_GPU_ADLN_Sampling with SPARK_Mode is
   -- Intel TGL Vol2d50-51 and82. Fixed single-sample offscreen probe.
   -- Number of samples is log2, not a literal count; it must match every
   -- bound render target's SURFACE_STATE.Multisamples field.
   type B1 is mod 2 ** 1 with Size => 1;
   type B3 is mod 2 ** 3 with Size => 3;
   type B16 is mod 2 ** 16 with Size => 16;
   type B26 is mod 2 ** 26 with Size => 26;
   type Multisample_Control is record
      Reserved_0 : B1 := 0;
      Log2_Samples : B3 := 0;
      Pixel_Upper_Left : B1 := 0;
      Pixel_Position_Offset : B1 := 0;
      Reserved_6 : B26 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Multisample_Control use record
      Reserved_0 at 0 range 0 .. 0;
      Log2_Samples at 0 range 1 .. 3;
      Pixel_Upper_Left at 0 range 4 .. 4;
      Pixel_Position_Offset at 0 range 5 .. 5;
      Reserved_6 at 0 range 6 .. 31;
   end record;
   function Encode (V : Multisample_Control) return Unsigned_32 is
     (Unsigned_32 (V.Reserved_0) or
      Shift_Left (Unsigned_32 (V.Log2_Samples), 1) or
      Shift_Left (Unsigned_32 (V.Pixel_Upper_Left), 4) or
      Shift_Left (Unsigned_32 (V.Pixel_Position_Offset), 5) or
      Shift_Left (Unsigned_32 (V.Reserved_6), 6));

   type Coverage_Control is record
      Sample_Mask : B16 := 0;
      Reserved_16 : B16 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Coverage_Control use record
      Sample_Mask at 0 range 0 .. 15;
      Reserved_16 at 0 range 16 .. 31;
   end record;
   function Encode (V : Coverage_Control) return Unsigned_32 is
     (Unsigned_32 (V.Sample_Mask) or
      Shift_Left (Unsigned_32 (V.Reserved_16), 16));

   Single_Sample : constant Multisample_Control := (others => <>);
   -- Coverage is ANDed unconditionally; zero would suppress the triangle.
   -- Hardware ignores bits15..1 for one sample. Center location, no DX9 offset.
   Single_Coverage : constant Coverage_Control :=
     (Sample_Mask => 1, others => <>);
   type Words is array (Natural range 0 .. 3) of Unsigned_32;
   Initial : constant Words :=
     [Intel_GPU_ADLN_Vertex_Fetch.Encode
        (Intel_GPU_ADLN_Vertex_Fetch.Header'
           (Subopcode => 16#0D#, others => <>)),
      Encode (Single_Sample),
      Intel_GPU_ADLN_Vertex_Fetch.Encode
        (Intel_GPU_ADLN_Vertex_Fetch.Header'
           (Subopcode => 16#18#, others => <>)),
      Encode (Single_Coverage)];
end Intel_GPU_ADLN_Sampling;
