with Interfaces; use Interfaces;
with System;
with Intel_GPU_ADLN_Probe_Shaders;
with Intel_GPU_ADLN_Vertex_Fetch;
package Intel_GPU_ADLN_Viewport with SPARK_Mode is
   -- TGL Vol2d140-141,236,871-872. Fixed 64x64 viewport, depth [0,1].
   -- Full-width IEEE754 fields are retained as bits, no runtime FP required.
   type SF_State is record
      M00 : Unsigned_32 := 0;
      M11 : Unsigned_32 := 0;
      M22 : Unsigned_32 := 0;
      M30 : Unsigned_32 := 0;
      M31 : Unsigned_32 := 0;
      M32 : Unsigned_32 := 0;
      Reserved_6 : Unsigned_32 := 0;
      Reserved_7 : Unsigned_32 := 0;
      Guard_X_Min : Unsigned_32 := 0;
      Guard_X_Max : Unsigned_32 := 0;
      Guard_Y_Min : Unsigned_32 := 0;
      Guard_Y_Max : Unsigned_32 := 0;
      X_Min : Unsigned_32 := 0;
      X_Max : Unsigned_32 := 0;
      Y_Min : Unsigned_32 := 0;
      Y_Max : Unsigned_32 := 0;
   end record with Size => 512, Bit_Order => System.Low_Order_First;
   for SF_State use record
      M00 at 0 range 0 .. 31;
      M11 at 4 range 0 .. 31;
      M22 at 8 range 0 .. 31;
      M30 at 12 range 0 .. 31;
      M31 at 16 range 0 .. 31;
      M32 at 20 range 0 .. 31;
      Reserved_6 at 24 range 0 .. 31;
      Reserved_7 at 28 range 0 .. 31;
      Guard_X_Min at 32 range 0 .. 31;
      Guard_X_Max at 36 range 0 .. 31;
      Guard_Y_Min at 40 range 0 .. 31;
      Guard_Y_Max at 44 range 0 .. 31;
      X_Min at 48 range 0 .. 31;
      X_Max at 52 range 0 .. 31;
      Y_Min at 56 range 0 .. 31;
      Y_Max at 60 range 0 .. 31;
   end record;
   type SF_Words is array (Natural range 0 .. 15) of Unsigned_32;
   function Encode (V : SF_State) return SF_Words is
     [V.M00, V.M11, V.M22, V.M30, V.M31, V.M32, V.Reserved_6, V.Reserved_7, V.Guard_X_Min, V.Guard_X_Max, V.Guard_Y_Min, V.Guard_Y_Max, V.X_Min, V.X_Max, V.Y_Min, V.Y_Max];
   type CC_State is record
      Minimum_Depth : Unsigned_32 := 0;
      Maximum_Depth : Unsigned_32 := 16#3F800000#;
   end record with Size => 64, Bit_Order => System.Low_Order_First;
   for CC_State use record
      Minimum_Depth at 0 range 0 .. 31;
      Maximum_Depth at 4 range 0 .. 31;
   end record;
   type CC_Words is array (Natural range 0 .. 1) of Unsigned_32;
   function Encode (V : CC_State) return CC_Words is
     [V.Minimum_Depth, V.Maximum_Depth];
   type B5 is mod 2 ** 5 with Size => 5;
   type B6 is mod 2 ** 6 with Size => 6;
   type B26 is mod 2 ** 26 with Size => 26;
   type B27 is mod 2 ** 27 with Size => 27;
   type SF_Pointer is record
      Reserved : B6 := 0;
      Offset_64B : B26 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for SF_Pointer use record
      Reserved at 0 range 0 .. 5;
      Offset_64B at 0 range 6 .. 31;
   end record;
   type CC_Pointer is record
      Reserved : B5 := 0;
      Offset_32B : B27 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for CC_Pointer use record
      Reserved at 0 range 0 .. 4;
      Offset_32B at 0 range 5 .. 31;
   end record;
   function Encode (V : SF_Pointer) return Unsigned_32 is
     (Unsigned_32 (V.Reserved) or Shift_Left (Unsigned_32 (V.Offset_64B), 6));
   function Encode (V : CC_Pointer) return Unsigned_32 is
     (Unsigned_32 (V.Reserved) or Shift_Left (Unsigned_32 (V.Offset_32B), 5));
   SF_Offset : constant := 512;
   CC_Offset : constant := 576;
   SF : constant SF_Words := Encode (SF_State'
     (M00 => 16#42000000#,
      M11 => 16#42000000#,
      M22 => 16#3F800000#,
      M30 => 16#42000000#,
      M31 => 16#42000000#,
      M32 => 0,
      Reserved_6 => 0,
      Reserved_7 => 0,
      Guard_X_Min => 16#BF800000#,
      Guard_X_Max => 16#3F800000#,
      Guard_Y_Min => 16#BF800000#,
      Guard_Y_Max => 16#3F800000#,
      X_Min => 0,
      X_Max => 16#427C0000#,
      Y_Min => 0,
      Y_Max => 16#427C0000#));
   CC : constant CC_Words := Encode (CC_State'(others => <>));
   type Pointer_Words is array (Natural range 0 .. 3) of Unsigned_32;
   Pointers : constant Pointer_Words :=
     [Intel_GPU_ADLN_Vertex_Fetch.Encode
        (Intel_GPU_ADLN_Vertex_Fetch.Header'
           (Subopcode => 16#21#, others => <>)),
      Encode (SF_Pointer'(Offset_64B => SF_Offset / 64, others => <>)),
      Intel_GPU_ADLN_Vertex_Fetch.Encode
        (Intel_GPU_ADLN_Vertex_Fetch.Header'
           (Subopcode => 16#23#, others => <>)),
      Encode (CC_Pointer'(Offset_32B => CC_Offset / 32, others => <>))];
   pragma Compile_Time_Error
     (SF_Offset mod 64 /= 0 or CC_Offset mod 32 /= 0 or
      Intel_GPU_ADLN_Probe_Shaders.Vertex_Data_Offset +
        Intel_GPU_ADLN_Probe_Shaders.Vertices'Length * 4 > SF_Offset or
      SF_Offset + 64 > CC_Offset or CC_Offset + 8 > 4096,
      "viewport state overlaps probe data or exceeds dynamic page");
end Intel_GPU_ADLN_Viewport;
