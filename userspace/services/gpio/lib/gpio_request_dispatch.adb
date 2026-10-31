pragma Ada_2022;
package body GPIO_Request_Dispatch with SPARK_Mode is
   use type W.Operation, W.Decode_Status, W.Control_Kind;
   procedure Handle (Caller : P.Process_ID; Badge : W.Word;
      Label : W.Operation; Protocol : W.Word; Words : W.Message_Words;
      Frame_Valid : Boolean; Result : out Report) is
      Parsed : W.Request_Decoding;
      Reply : Reply_Authority;
      Saved, Delivered, Taken : Boolean;
      Kind : constant W.Control_Kind :=
        (if Label = W.OP_GPIO_RELEASE then W.Release_Pin
         elsif Label = W.OP_GPIO_ACK then W.Acknowledge else W.Arm);
      Rejection : W.Completion := (Kind, W.Invalid_Frame, 0, 0);
   begin
      if not P.Is_Process (Caller) or else not Authorized (Caller, Badge) then
         Rejection.Status := W.Unauthorized;
      elsif Frame_Valid then
         Parsed := W.Decode (Label, Protocol, Words);
      end if;
      Save (Reply, Saved);
      if not Saved then
         if Parsed.Status = W.Decoded then Rejection.Status := W.Busy; end if;
         Answer_Current (Caller, Label, Rejection, Delivered);
      elsif Parsed.Status /= W.Decoded then
         Answer_Saved (Reply, Label, Rejection, Delivered);
      else
         Offer (Caller, Badge, Parsed.Value, Reply, Taken);
         if Taken then Result := (Control_Retained, W.Busy); return; end if;
         Rejection.Status := W.Busy;
         Rejection.Lease := (case Parsed.Value.Kind is
            when W.Arm => 0, when W.Release_Pin => Parsed.Value.Lease_To_Release,
            when W.Acknowledge => Parsed.Value.Lease_To_Ack);
         Rejection.Generation := (if Parsed.Value.Kind = W.Acknowledge then Parsed.Value.Generation_To_Ack else 0);
         Answer_Saved (Reply, Label, Rejection, Delivered);
      end if;
      Result := ((if Delivered then Rejected_Delivered else Rejected_Lost), Rejection.Status);
   end Handle;
end GPIO_Request_Dispatch;
