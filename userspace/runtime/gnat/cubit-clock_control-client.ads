with CuBit.Clocks;

--  IPC client for the clock-control endpoint (Endpoint_Slot). Calling it
--  does not grant adjustment authority; without the minted endpoint the
--  clock service refuses the request.
package CuBit.Clock_Control.Client is
   procedure Submit
     (Item : Sample; Result : out Outcome;
      Quality : out CuBit.Clocks.Time_Quality; Success : out Boolean);
end CuBit.Clock_Control.Client;
