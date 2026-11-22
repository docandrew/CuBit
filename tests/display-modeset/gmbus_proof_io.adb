package body GMBUS_Proof_IO with SPARK_Mode => Off is
   procedure Read (Offset : Intel_GPU_Display_Registers.Register_Offset; Value : out Unsigned_32) is
   begin
      Value := Unsigned_32 (Offset);
   end Read;
   procedure Write
     (Offset : Intel_GPU_Display_Registers.Register_Offset; Value : Unsigned_32; Written : out Boolean) is
   begin
      Written := Offset /= Value;
   end Write;
   procedure Now_Us (Value : out Unsigned_64) is
   begin
      Value := 0;
   end Now_Us;
end GMBUS_Proof_IO;
