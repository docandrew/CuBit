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
   -- Provider requests are bounded separately from AML bytecode charging.
   -- A composed field operation shares this counter with its selector work.
   type Field_Request_Count is new Natural range 0 .. 65_536;
   subtype Field_Request_Limit is Field_Request_Count range 1 .. Field_Request_Count'Last;
   Default_Field_Request_Limit : constant Field_Request_Limit := 1_024;
   -- Includes graph validation and continuation steps, even without dispatch.
   type Field_Work_Count is new Natural range 0 .. 1_048_576;
   subtype Field_Work_Limit is Field_Work_Count range 1 .. Field_Work_Count'Last;
   Default_Field_Work_Limit : constant Field_Work_Limit := 65_536;
   function Charged_Field_Work (Remaining : Field_Work_Count) return Field_Work_Count is
     (if Remaining = 0 then 0 else Remaining - 1)
     with Global => null,
       Post => (if Remaining = 0 then Charged_Field_Work'Result = 0
         else Charged_Field_Work'Result = Remaining - 1);
end AML_Execution_Limits;
