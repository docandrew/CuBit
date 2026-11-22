------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body CuBit.Binder_Interface is

   procedure Initialize (SEH : System.Address) is
      pragma Unreferenced (SEH);
   begin
      null;
   end Initialize;

   procedure Finalize is
   begin
      null;
   end Finalize;

   procedure Runtime_Initialize (Install_Handler : int) is
      pragma Unreferenced (Install_Handler);
   begin
      null;
   end Runtime_Initialize;

   procedure Runtime_Finalize is
   begin
      null;
   end Runtime_Finalize;

end CuBit.Binder_Interface;
