------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The decisions netstack makes when it services a stream channel
--  (serviceChannel), separated from the rings and IPC it drives: when to
--  ask the client for a kick, when a write-shutdown may close the
--  connection, and when a second look is needed so that no wakeup is lost.
--  The first piece of docs/netstack-redesign.md's channel-servicing plan.
--
--  Proved (tests/tcp-session, level 1), and stated on their own:
--  - a failed channel asks for no kick;
--  - Kick_On_Send only when everything the client sent was taken, and
--    Kick_On_Receive only when data waits but the receive ring is full;
--  - a write-shutdown closes the connection only once, only after all the
--    client's data was taken, and never during the handshake;
--  - after asking for a kick, netstack looks again unless the client's
--    index still matches what it saw (the kick-versus-flag race).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Channel_Service with SPARK_Mode, Pure is

   --  The kick bits of CuBit.Net_Channel_Layout (checked where both are seen).
   Kick_On_Send    : constant Unsigned_32 := 1;
   Kick_On_Receive : constant Unsigned_32 := 2;

   subtype Kick_Flags is Unsigned_32 range 0 .. 3;

   --  The kicks to ask of the client.
   function Kicks
     (Failed : Boolean; Send_Unconsumed : Natural;
      Readable, Receive_Space : Natural; Has_Connection : Boolean)
      return Kick_Flags
   is ((if not Failed and then Send_Unconsumed = 0 then Kick_On_Send else 0) or
       (if not Failed and then Has_Connection and then Readable > 0 and then
           Receive_Space = 0 then Kick_On_Receive else 0))
   with Post =>
     (if Failed then Kicks'Result = 0) and then
     ((Kicks'Result and Kick_On_Send) /= 0) = (not Failed and then Send_Unconsumed = 0) and then
     ((Kicks'Result and Kick_On_Receive) /= 0) =
       (not Failed and then Has_Connection and then Readable > 0 and then
        Receive_Space = 0);

   --  The client asked for a write-shutdown: close the connection now?
   function Close_Due
     (Already_Closed : Boolean; Send_Unconsumed : Natural;
      Shutdown_Asked : Boolean; In_Handshake : Boolean) return Boolean
   is (not Already_Closed and then Send_Unconsumed = 0 and then Shutdown_Asked and then
       not In_Handshake)
   with Post =>
     (if Close_Due'Result then
        not Already_Closed and then Send_Unconsumed = 0 and then Shutdown_Asked and then
        not In_Handshake);

   --  Having asked for Flags, look again unless every index they depend on
   --  is unchanged: the client may have moved it before it saw the flag.
   function Look_Again
     (Flags : Kick_Flags; Tx_Produced_Seen, Tx_Produced_Now : Unsigned_32;
      Rx_Consumed_Seen, Rx_Consumed_Now : Unsigned_32) return Boolean
   is (Flags /= 0 and then
       not (((Flags and Kick_On_Send) = 0 or else Tx_Produced_Now = Tx_Produced_Seen) and then
            ((Flags and Kick_On_Receive) = 0 or else Rx_Consumed_Now = Rx_Consumed_Seen)))
   with Post =>
     (if (Flags and Kick_On_Send) /= 0 and then Tx_Produced_Now /= Tx_Produced_Seen
      then Look_Again'Result) and then
     (if (Flags and Kick_On_Receive) /= 0 and then Rx_Consumed_Now /= Rx_Consumed_Seen
      then Look_Again'Result) and then
     (if Flags = 0 then not Look_Again'Result) and then
     (if Tx_Produced_Now = Tx_Produced_Seen and then Rx_Consumed_Now = Rx_Consumed_Seen
      then not Look_Again'Result);

end Channel_Service;
