with CCL.Objects;
with CCL.Objects.Holders;
with CCL.Interfaces.Logs;
with CuBit.Failures;
--  The Workbench's view of logstore (platform-specific bodies: native/ and
--  the hosted preview's host/).
package CCL_Log_IO is
   --  Publish one record as this process ("started"); nothing when the
   --  process has no logstore binding or the platform has no logstore.
   procedure Announce (Text : String);
   --  Publish one record as this process and echo it on the debug console,
   --  as CuBit.Log does for services (the agent's request and tool-call
   --  records, AGENT-002).
   procedure Note (Text : String);
   --  Whether this process can ask logstore for records at all.
   function Available return Boolean;
   --  The records of Service (a service name, or a process number), oldest
   --  first, as a List<LogItem> value for Contract, as large as they are
   --  (CCL.Interfaces.Logs). Success is False when Service names no live
   --  process or logstore refuses.
   procedure Recent
     (Service : String; Contract : CCL.Objects.Binding;
      Image : out CCL.Objects.Holders.Holder; Success : out Boolean;
      Why : out CuBit.Failures.Failure);
   --  As Recent, for the records whose time is in Within
   --  (CCL.Interfaces.Logs.Log_Window): records outside it are not asked
   --  for, so no gap counts them; past CCL.Interfaces.Logs.RESULT_ITEM_CEILING
   --  a window shows its first and newest, with a Not_Shown gap between.
   procedure Between
     (Service : String; Within : CCL.Interfaces.Logs.Log_Window; Contract : CCL.Objects.Binding;
      Image : out CCL.Objects.Holders.Holder; Success : out Boolean;
      Why : out CuBit.Failures.Failure)
     with Pre => CCL.Interfaces.Logs.Valid (Within);

   --  The least severe record logstore keeps.
   procedure Minimum
     (Level : out CCL.Interfaces.Logs.Severity; Success : out Boolean; Why : out CuBit.Failures.Failure);
   --  Keep Level and above from now on (log-control); Previous was kept before.
   procedure Set_Minimum
     (Level : CCL.Interfaces.Logs.Severity; Previous : out CCL.Interfaces.Logs.Severity;
      Success : out Boolean; Why : out CuBit.Failures.Failure);
end CCL_Log_IO;
