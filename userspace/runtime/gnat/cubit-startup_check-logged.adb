------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Logging;
with CuBit.Log_Records;
with CuBit.Messages;

package body CuBit.Startup_Check.Logged is

   procedure Require
     (Expected : CuBit.Startup_State.Expectations;
      Program  : String;
      Log      : CuBit.Handles.Services.Logstore.Optional_Endpoint)
   is
      Ready, Published : Boolean;
      Text : String (1 .. Maximum_Text);
      Length : Text_Length;
   begin
      Explain (Expected, Program, Ready, Text, Length);
      if Ready then
         return;
      end if;
      CuBit.Messages.debugPrint (Text (1 .. Length));
      CuBit.Messages.debugPrint ((1 => ASCII.LF));
      if Log.Present then
         CuBit.Logging.Announce (Text (1 .. Length), Published, Log.Value, CuBit.Log_Records.Critical);
      end if;
      Stop;
   end Require;

end CuBit.Startup_Check.Logged;
