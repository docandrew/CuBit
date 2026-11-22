package Policy_Proof with SPARK_Mode is
   procedure Check (Requested, Installation, Session, Issuer : Boolean)
     with Ghost;
end Policy_Proof;
