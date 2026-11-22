with Interfaces; use Interfaces;
package Intel_GPU_ADLN_Probe_Shaders with SPARK_Mode is
   -- Fixed trusted probe only, Mesa26.2.3 ADL-N PCI46D2 compiler output.
   -- Reproduce with tests/mesa-anv/test-offscreen-shader.sh in main checkout.
   -- Not a compiler ABI or permission to submit application-provided shaders.
   type Words is array (Natural range <>) of Unsigned_32;
   Vertex : constant Words :=
     [16#80030061#,16#7F050220#,16#00460105#,0,
      16#00030061#,16#77054660#,0,0,
      16#00030061#,16#78054660#,0,0,
      16#00030061#,16#79054660#,0,0,
      16#00030061#,16#7A054660#,0,0,
      16#00030061#,16#7B050660#,16#00460205#,0,
      16#00030061#,16#7C050660#,16#00460305#,0,
      16#00030061#,16#7D050660#,16#00460405#,0,
      16#00030061#,16#7E050660#,16#00460505#,0,
      16#80000101#,0,0,0,
      16#00030131#,4,16#600E7F0C#,16#02007744#];
   Fragment : constant Words :=
     [16#A17F0061#,16#3F810000#,16#A17C0061#,16#00010000#,
      16#A17D0061#,16#00010000#,16#A17E0061#,16#3F810000#,
      16#00030132#,4,16#58007F0C#,16#00C47C1C#,
      0,0,0,0,
      16#A07E0061#,16#3F810000#,16#A0780061#,16#00010000#,
      16#A07A0061#,16#00010000#,16#A07C0061#,16#3F810000#,
      16#00040132#,4,16#50007E14#,16#00C47834#];
   Vertex_Offset : constant := 0;
   Fragment_Offset : constant := 256;
   Fragment_SIMD16_Offset : constant := 64;
   Vertex_GRF_Start : constant := 2;
   Fragment_GRF_Start : constant := 2;
   -- Mesa brw_compile_fs enables VMask on pre-XeHP, including ADL-N.
   -- SIMD8 uses kernel slot0, SIMD16 uses slot2; slot1 is unused.
   Fragment_Vector_Mask : constant Boolean := True;
   Vertex_Dispatch_Mode : constant := 3;
   Vertex_URB_Read_Length : constant := 1;
   Vertex_URB_Entry_Size : constant := 1;
   -- Three clip-space xyzw positions. IEEE754 single precision, stride16.
   Vertices : constant Words :=
     [16#BF000000#,16#BF000000#,0,16#3F800000#,
      16#3F000000#,16#BF000000#,0,16#3F800000#,
      0,16#3F000000#,0,16#3F800000#];
   Vertex_Data_Offset : constant := 256; -- Within Render_State_Page.
   pragma Compile_Time_Error
     (Vertex_Offset mod 64 /= 0 or Fragment_Offset mod 64 /= 0 or
      Vertex_Offset + Vertex'Length * 4 > Fragment_Offset or
      Fragment_Offset + Fragment'Length * 4 > 4096 or
      Fragment_SIMD16_Offset mod 64 /= 0 or
      Fragment_SIMD16_Offset >= Fragment'Length * 4,
      "probe shader layout exceeds instruction page or kernel alignment");
   pragma Compile_Time_Error
     (Vertex_Data_Offset < 128 or Vertex_Data_Offset mod 16 /= 0 or
      Vertex_Data_Offset + Vertices'Length * 4 > 4096,
      "probe vertices overlap surface state or exceed state page");
end Intel_GPU_ADLN_Probe_Shaders;
