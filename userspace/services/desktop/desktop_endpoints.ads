------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Desktop's startup capabilities (CAP-002), looked up once.
--
--  @description
--  The startup table is loaded before the program's own units elaborate
--  and never changes afterwards, so each endpoint is read from it once
--  here; the hot paths (every frame's present) use these constants. Global
--  => null on the accessors records that assumption for SPARK callers.
------------------------------------------------------------------------------
with CuBit.Handles;
with Startup;

package Desktop_Endpoints with SPARK_Mode => Off is
   Display_Service : constant CuBit.Handles.Endpoint := Startup.Display;
   Config_Service  : constant CuBit.Handles.Endpoint := Startup.Config;
   Process_Manager : constant CuBit.Handles.Endpoint := Startup.Process_Manager;
   --  logstore's observer role (the bootstrap screen's recent records).
   Log_Observer    : constant CuBit.Handles.Endpoint := Startup.Log_Observer;
   Files           : constant CuBit.Handles.Endpoint := Startup.Filesystem;
   --  The clock service; null when the profile starts none (no time shown).
   Clock_Service   : constant CuBit.Handles.Endpoint :=
     (if Startup.Clock.Present then Startup.Clock.Value else CuBit.Handles.No_Endpoint);
   --  The mixer's control endpoint; null without an audio device.
   Mixer_Control   : constant CuBit.Handles.Endpoint :=
     (if Startup.Mixer_Control.Present then Startup.Mixer_Control.Value
      else CuBit.Handles.No_Endpoint);

   --  The admitted render session, or the null handle (software start).
   function Render return CuBit.Handles.Endpoint with Global => null;
   function Display return CuBit.Handles.Endpoint is (Display_Service) with Global => null;
end Desktop_Endpoints;
