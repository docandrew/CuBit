package body Timer_Service_Verification with SPARK_Mode is
   procedure Read_Raw
     (Value : out AML_Decode.Integer_Value; Available : out Boolean)
   is
   begin
      Value := Sample_Microseconds;
      Available := Sample_Available;
   end Read_Raw;
end Timer_Service_Verification;
