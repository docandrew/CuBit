with CuBit.Authority_Policy; use CuBit.Authority_Policy;
package body Policy_Proof with SPARK_Mode is
   procedure Check (Requested, Installation, Session, Issuer : Boolean) is
      Result : constant Decision := Evaluate (Requested, Installation, Session, Issuer);
   begin
      pragma Assert ((Result = Approved) = (Requested and Installation and Session and Issuer));
      pragma Assert (Evaluate (Requested, False, Session, Issuer) /= Approved);
      pragma Assert (Evaluate (Requested, Installation, False, Issuer) /= Approved);
      pragma Assert (Evaluate (Requested, Installation, Session, False) /= Approved);
      pragma Assert (not Bootstrap_Approves (Log_Observation, False));
      pragma Assert (not Bootstrap_Approves (Master_Audio, False));
      pragma Assert (Bootstrap_Approves (Master_Audio, Session) = Session);
      pragma Assert (Bootstrap_Approves (Log_Observation, Session) = Session);
   end Check;
end Policy_Proof;
