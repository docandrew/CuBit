--  parameters-plain: started only when launched without arguments.
with CuBit.Messages;
with Startup;
pragma Unreferenced (Startup);

procedure Main is
begin
   CuBit.Messages.debugPrint ("parameters-plain: started without arguments" & ASCII.LF);
end Main;
