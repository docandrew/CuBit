package Desktop_Logs is
   --  Echo Text to the debug console, and publish each complete line it
   --  ends as a typed record: a copy into the desktop's log ring
   --  (CuBit.Logging), no IPC and no completion. Lines that cannot be framed
   --  or are shed are counted, and the count is published when it changes.
   procedure Write (Text : String);
end Desktop_Logs;
