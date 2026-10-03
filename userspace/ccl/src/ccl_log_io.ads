with CCL.Objects;
with CCL.Interfaces.Logs;
with CuBit.Failures;
--  The Workbench's view of logstore (platform-specific bodies: native/ and
--  the hosted preview's host/).
package CCL_Log_IO is
   --  Publish one record as this process ("started"); nothing when the
   --  process has no logstore binding or the platform has no logstore.
   procedure Announce (Text : String);
   --  Whether this process can ask logstore for records at all.
   function Available return Boolean;
   --  The most recent records of Service (a service name, or a process
   --  number), oldest first, as a LogEntries image for Contract
   --  (CCL.Interfaces.Logs). Success is False when Service names no live
   --  process or logstore refuses.
   procedure Recent
     (Service : String; Contract : CCL.Objects.Binding;
      Image : out CCL.Objects.Image; Success : out Boolean;
      Why : out CuBit.Failures.Failure);

   --  The least severe record logstore keeps.
   procedure Minimum
     (Level : out CCL.Interfaces.Logs.Severity; Success : out Boolean; Why : out CuBit.Failures.Failure);
   --  Keep Level and above from now on (log-control); Previous was kept before.
   procedure Set_Minimum
     (Level : CCL.Interfaces.Logs.Severity; Previous : out CCL.Interfaces.Logs.Severity;
      Success : out Boolean; Why : out CuBit.Failures.Failure);
end CCL_Log_IO;
