------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The startup check for a program that declares logstore (CAP-003): the
--  same as CuBit.Startup_Check.Require, and the explanation also goes to
--  logstore as a Critical record when the program can still reach it.
--  A separate unit, so programs without logstore do not link the logging
--  client.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Startup_State;
with CuBit.Handles.Services;

package CuBit.Startup_Check.Logged is

   procedure Require
     (Expected : CuBit.Startup_State.Expectations;
      Program  : String;
      Log      : CuBit.Handles.Services.Logstore.Optional_Endpoint);

end CuBit.Startup_Check.Logged;
