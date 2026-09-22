package Read_Proof with SPARK_Mode is
   -- Arbitrary reply length/status, not a contract assuming successful I/O.
   procedure Check
     (Length, Reply_Length : Natural; Reply_Success : Boolean;
      Result : out Boolean);
end Read_Proof;
