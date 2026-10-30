pragma Ada_2022;
with Log_Capture;
package body CuBit.Log is
   procedure Set_Echo (Enabled : Boolean) is
      pragma Unreferenced (Enabled);
   begin null; end Set_Echo;
   procedure Write (Level : Logs.Severity; Text : String) is
   begin
      Log_Capture.Attempts := Log_Capture.Attempts + 1;
      Log_Capture.Last_Level := Level;
      if Text'Length > Log_Capture.Text'Length then
         raise Program_Error with "overlong diagnostic";
      end if;
      Log_Capture.Length := Text'Length;
      Log_Capture.Text (1 .. Text'Length) := Text;
      if Log_Capture.Fail then raise Program_Error with "logger unavailable"; end if;
   end Write;
   procedure Trace (Text : String) is begin Write (Logs.Trace, Text); end Trace;
   procedure Debug (Text : String) is begin Write (Logs.Debug, Text); end Debug;
   procedure Info (Text : String) is begin Write (Logs.Information, Text); end Info;
   procedure Warning (Text : String) is begin Write (Logs.Warning, Text); end Warning;
   procedure Error (Text : String) is begin Write (Logs.Error, Text); end Error;
   procedure Critical (Text : String) is begin Write (Logs.Critical, Text); end Critical;
   function Wanted (Level : Logs.Severity) return Boolean is
      pragma Unreferenced (Level);
   begin return True; end Wanted;
   procedure Flush is begin null; end Flush;
   function Lost return Unsigned_64 is (0);
end CuBit.Log;
