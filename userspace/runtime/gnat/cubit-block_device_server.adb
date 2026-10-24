------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
package body CuBit.Block_Device_Server with SPARK_Mode is
   procedure Handle
     (State    : Device_State;
      Sender   : CuBit.Messages.Process_ID;
      Request  : CuBit.Messages.Message;
      Response : out CuBit.Messages.Message)
   is
      Decoded : constant Decoded_Request := Decode (Request, State);
   begin
      case Decoded.Kind is
         when Describe =>
            Response := Describe_Reply (State);
         when Rejected =>
            Response := Status_Reply (False, 0);
         when Flush =>
            declare
               Durable : Boolean;
            begin
               Flush (State, Durable);
               Response := Status_Reply (Durable, 0);
            end;
         when Transfer =>
            declare
               Granted  : Loan;
               Admitted : Boolean;
               Moved    : Unsigned_64;
            begin
               Admit (Sender, Decoded.Data, Granted, Admitted);
               if Admitted then
                  Transfer (State, Granted.Item, Granted.Buffer, Moved);
                  Settle (Granted, Moved, Response);
               else
                  Response := Status_Reply (False, 0);
               end if;
            end;
      end case;
   end Handle;
end CuBit.Block_Device_Server;
