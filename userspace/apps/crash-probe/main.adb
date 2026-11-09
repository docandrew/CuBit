with CuBit.Messages; use CuBit.Messages;
with CuBit.Log_Records;
with CuBit.Logging;
with Startup;

--  Native regression for LOG-004: a program that logs one record through
--  its own log channel and then raises an exception nothing handles. The
--  runtime's last-chance handler prints "User-space exception at main.adb
--  line N" on the debug console as before, publishes "unhandled exception
--  at main.adb:N" as an Error through the same channel, and exits with
--  code 134; (logs.recent "crash-probe") must show it after the program is
--  gone (tests/headless/run.sh, CCL_CONSOLE_DEMO=kernel-log). The kernel
--  holds none of this text.
procedure Main is
   package L renames CuBit.Log_Records;
   Writer : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Startup.Logstore);
   Made : constant L.Decoded := L.Make ("crash-probe: raising an exception nobody handles");
   Accepted : Boolean := False;
begin
   if Made.Success then
      CuBit.Logging.Emit (Writer, Made.Value, Accepted);
   end if;
   debugPrint ((if Accepted then "crash-probe: raising an exception nobody handles"
                else "crash-probe: FAIL its log channel did not take a record") & ASCII.LF);
   raise Program_Error;
end Main;
