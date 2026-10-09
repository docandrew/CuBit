with Main;
with CuBit.Messages;
procedure Driver_Tests is
begin
   Main;
   raise Program_Error with "driver returned without completing fixture";
exception
   when CuBit.Messages.Finished => CuBit.Messages.Verify;
end Driver_Tests;
