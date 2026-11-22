------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The desktop endpoint of a UI program (CAP-002): its "desktop" startup
--  entry (manifest: request-service desktop read-write desktop), looked up
--  once. The null handle when the program has none.
------------------------------------------------------------------------------
with CuBit.Handles;

package UI_Desktop_Service is
   Binding : constant String := "desktop";
   function Endpoint return CuBit.Handles.Endpoint;
end UI_Desktop_Service;
