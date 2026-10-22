package AML_Execution_Limits with SPARK_Mode, Pure is
   subtype Scalar_Transport_Capacity is Positive range 1 .. Positive'Last - 1;
   Max_Lexical_Depth : constant Positive := 64;
   Max_Method_Code_Bytes : constant Positive := 65_536;
   -- Each live continuation starts at a distinct opcode byte. One frame per
   -- admitted method byte is therefore a conservative method ceiling. Module
   -- code uses the same explicit policy bound even when its table is larger.
   -- This is static storage admission, not a native stack suitability claim.
   subtype Continuation_Capacity is Positive range 1 .. Max_Method_Code_Bytes;
   Default_Continuation_Capacity : constant Continuation_Capacity := 64;
end AML_Execution_Limits;
