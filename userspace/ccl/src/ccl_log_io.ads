with CCL.Objects;
--  The Workbench's view of logstore (platform-specific bodies: native/ and
--  the hosted preview's host/).
package CCL_Log_IO is
   --  Whether this process can ask logstore for records at all.
   function Available return Boolean;
   --  The most recent records of Service (a service name, or a process
   --  number), oldest first, as a LogEntries image for Contract
   --  (CCL.Interfaces.Logs). Success is False when Service names no live
   --  process or logstore refuses.
   procedure Recent
     (Service : String; Contract : CCL.Objects.Binding;
      Image : out CCL.Objects.Image; Success : out Boolean);
end CCL_Log_IO;
