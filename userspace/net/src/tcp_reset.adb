------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body TCP_Reset with SPARK_Mode is

   function For_Closed (A : Arrival) return Reply is
   begin
      if A.RST then
         return (others => <>);
      elsif A.ACK then
         return (Send => True, With_ACK => False, Seq_No => A.Ack_No, Ack_No => 0);
      else
         return (Send => True, With_ACK => True, Seq_No => 0,
                 Ack_No => A.Seq_No + Segment_Length (A));
      end if;
   end For_Closed;

end TCP_Reset;
