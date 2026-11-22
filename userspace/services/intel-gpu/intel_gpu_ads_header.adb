package body Intel_GPU_ADS_Header with SPARK_Mode is
   function Encode
     (Registers : Register_Descriptors;
      Policies, System_Info, Private_Data : Unsigned_32;
      Golden_Addresses, State_Sizes : Class_Values;
      Capture : Capture_Pointers) return Header_Bytes
   is
      Result : Header_Bytes := [others => 0];
      procedure Put (Offset : Natural; Value : Unsigned_32)
        with Pre => Offset <= 4568
      is
      begin
         for B in Natural range 0 .. 3 loop
            Result (Offset + B) := Unsigned_8 (Shift_Right (Value, B * 8) and 255);
         end loop;
      end Put;
   begin
      for I in Registers'Range loop Result (I) := Registers (I); end loop;
      Put (4100, Policies);
      Put (4104, System_Info);
      for C in Class_Values'Range loop
         Put (4116 + C * 4, Golden_Addresses (C));
         Put (4180 + C * 4, State_Sizes (C));
      end loop;
      Put (4244, Private_Data);
      for I in Capture'Range loop Result (4252 + I) := Capture (I); end loop;
      return Result;
   end Encode;
end Intel_GPU_ADS_Header;
