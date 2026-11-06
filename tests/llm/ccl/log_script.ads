--  What the scripted CCL_Log_IO (fake_ccl_log_io.adb) was asked to log.
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package Log_Script is
   Lines : Unbounded_String;
   Count : Natural := 0;
   procedure Append (Text : String);
end Log_Script;
