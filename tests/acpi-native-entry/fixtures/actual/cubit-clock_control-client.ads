with CuBit.Clocks;
with CuBit.Handles;

--  IPC client for the clock-control endpoint (Service: the startup handle). Calling it
--  does not grant adjustment authority; without the minted endpoint the
--  clock service refuses the request.
package CuBit.Clock_Control.Client is
   procedure Submit
     (Service : CuBit.Handles.Endpoint; Item : Sample; Result : out Outcome;
      Quality : out CuBit.Clocks.Time_Quality; Success : out Boolean);
end CuBit.Clock_Control.Client;
