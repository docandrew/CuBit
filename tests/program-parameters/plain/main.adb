--  parameters-plain: started only when launched without arguments.
with CuBit.Console_Route;
with CuBit.Messages;
with Startup;
pragma Unreferenced (Startup);

procedure Main is
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   CuBit.Messages.debugPrint ("parameters-plain: started without arguments" & ASCII.LF);
end Main;
