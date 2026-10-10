with Desktop_Log_IO;
package Desktop_Logs with SPARK_Mode, Abstract_State => State,
  Initializes => State is
   --  Echo Text to the debug console, and publish each complete line it
   --  ends as a typed record: a copy into the desktop's log ring
   --  (CuBit.Logging), no IPC and no completion. Lines that cannot be framed
   --  or are shed are counted, and the count is published when it changes.
   procedure Write (Text : String)
     with Global => (In_Out => (State, Desktop_Log_IO.State));
   --  Echo one line (Text, without its line feed) and publish it as a
   --  Warning record.
   procedure Warn (Text : String)
     with Global => (In_Out => (State, Desktop_Log_IO.State));
end Desktop_Logs;
