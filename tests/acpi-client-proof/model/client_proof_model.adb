pragma Ada_2022;
package body Client_Proof_Model with SPARK_Mode is
   procedure Exchange
     (State : in out Transport; Destination : Endpoint; Deadline : P.Time_Instant;
      Item : P.Packet; Reply : out P.Packet; Delivered : out P.Delivery) is
   begin
      State.Invocations := State.Invocations + 1;
      State.Last_Endpoint := Destination; State.Last_Deadline := Deadline; State.Last_Request := Item;
      Reply := State.Next_Reply; Delivered := State.Next_Delivery;
   end Exchange;
   procedure Read_Window (Now, Deadline : out P.Time_Instant) is
   begin Now := Clock_Now; Deadline := Clock_Deadline; end Read_Window;
end Client_Proof_Model;
