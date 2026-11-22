with CuBit.Metric_Protocol;
package Issuance_Proof with SPARK_Mode is
   procedure Check (Requested, Installation, Session, Issuer : Boolean;
                    ID : CuBit.Metric_Protocol.Issuance);
end Issuance_Proof;
