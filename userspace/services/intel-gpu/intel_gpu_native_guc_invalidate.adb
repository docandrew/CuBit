with Intel_GPU_TLB_Registers;
package body Intel_GPU_Native_GuC_Invalidate is
   procedure Write_Request (Value : Unsigned_32; OK : out Boolean) is
   begin
      IO.Write_Register (Intel_GPU_TLB_Registers.GuC_Offset, Value, OK);
   end Write_Request;
   procedure Read_Status (Value : out Unsigned_32; OK : out Boolean) is
   begin
      IO.Read_Register (Intel_GPU_TLB_Registers.GuC_Offset, Value, OK);
   end Read_Status;
end Intel_GPU_Native_GuC_Invalidate;
