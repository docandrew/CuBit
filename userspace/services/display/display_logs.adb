------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with CuBit.Log_Records;
with CuBit.Logging;
with CuBit.Messages;
with CuBit.Text_To_Log;
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
      CuBit.Messages.debugPrint (Text);
      for C of Text loop
         T.Feed (Framer, C, (Clock => L.Unspecified), Step);
         if T.Kind (Step) = T.Record_Ready then
            CuBit.Logging.Emit (Writer, T.Value (Step), Accepted);
         end if;
      end loop;
   end Write;
end Display_Logs;
