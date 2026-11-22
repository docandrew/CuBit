with CuBit.Console_Route;
with CuBit.Log_Records;
with CuBit.Messages;

package body Remote_Log is
   package Logs renames CuBit.Log_Records;

   procedure Note (Text : String) is
      Line : constant String := "ccl-remote: " & Text;
   begin
      if CuBit.Console_Route.Routed then
         declare
            Made : constant Logs.Decoded :=
              Logs.Make (Line (Line'First .. Line'First + Natural'Min (Line'Length, Logs.Maximum_Text_Bytes) - 1),
                         Logs.Information);
            Accepted : Boolean;
         begin
            if Made.Success then
               CuBit.Console_Route.Emit (Made.Value, Console => True, Accepted => Accepted);
            end if;
         end;
      else
         --  Not routed: nothing takes the text but the console.
         CuBit.Messages.debugPrint (Line & ASCII.LF);
      end if;
   end Note;
end Remote_Log;
