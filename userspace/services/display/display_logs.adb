------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with CuBit.Log_Records;
with CuBit.Logging;
with CuBit.Messages;
with CuBit.Text_To_Log;
with CuBit.Console_Route;
with Startup;

package body Display_Logs is
   package L renames CuBit.Log_Records;
   package T renames CuBit.Text_To_Log;
   use type T.Step_Kind;

   --  Minimal profiles start Display without logstore (the manifest
   --  declares it optional): then the console echo is all there is.
   Writer : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Startup.Logstore);
   Framer : T.Adapter;

   procedure Write (Text : String) is
      Step : T.Step;
      Accepted : Boolean;
   begin
      --  Routed (CuBit.Console_Route, ABI-002: Display has logstore): each
      --  line is one record in the route's ring, marked as console text,
      --  not also echoed. Unrouted (a minimal profile without logstore):
      --  the console echo, as before.
      if not CuBit.Console_Route.Routed then
         CuBit.Messages.debugPrint (Text);
      end if;
      for C of Text loop
         T.Feed (Framer, C, (Clock => L.Unspecified), Step);
         if T.Kind (Step) = T.Record_Ready then
            if CuBit.Console_Route.Routed then
               CuBit.Console_Route.Emit (T.Value (Step), Console => True, Accepted => Accepted);
            else
               CuBit.Logging.Emit (Writer, T.Value (Step), Accepted);
            end if;
         end if;
      end loop;
   end Write;
end Display_Logs;
