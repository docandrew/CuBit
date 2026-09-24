with Interfaces;
package GPU_Test_Policy is
   Delay_First_Output_Ms : constant Interfaces.Unsigned_64 := 0;
   --  Allow startup clearing, then reject subsequent clears on either head.
   --  A build-only fixture; no client opcode enables this in production.
   Reject_Client_Clear : constant Boolean := True;
end GPU_Test_Policy;
