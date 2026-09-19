with CuBit.Protocols.Stream_Connections;
package Proof_Cases with SPARK_Mode is
   procedure Check
     (Item : CuBit.Protocols.Stream_Connections.Request;
      Evidence : CuBit.Protocols.Stream_Connections.Approvals)
     with Ghost;
end Proof_Cases;
