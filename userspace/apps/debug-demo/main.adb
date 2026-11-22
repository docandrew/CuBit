------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The debugger preview's demo target (DBG-001): starts the cooperative
--  stub, then two observed threads that keep changing Demo_World.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Console_Route;
with CuBit.Debug_Stub;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Waits;
with Demo_World;
with Startup;

procedure Main is
   Idle_Ms : constant := 1_000;
   Started : Boolean;
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   CuBit.Debug_Stub.Start ("debug-demo", Startup.Registration, Started);
   if not Started then
      debugPrint ("debug-demo: stub FAIL" & ASCII.LF);
   end if;
   CuBit.Debug_Stub.Spawn ("jobs", Demo_World.Run_Jobs'Access, Started);
   if Started then
      CuBit.Debug_Stub.Spawn ("sensors", Demo_World.Run_Sensors'Access, Started);
   end if;
   debugPrint ((if Started then "debug-demo: running" else "debug-demo: spawn FAIL") & ASCII.LF);
   loop
      CuBit.Waits.Sleep_For (Idle_Ms);
   end loop;
end Main;
