with Interfaces;
with CCL.Catalog;
with CCL.Objects;
with CCL.Objects.Holders;
with CCL.Types;

--  The logs interface: (logs.recent "service") returns the recent records of
--  one service as a typed List<LogItem> (TYPE_SOURCE), with its gaps and
--  accounting; (logs.between (LogWindow "service" from to)) the same for
--  the records in a window of time, so a caller reads every retained
--  record in slices. (logs.minimum) says the least severe record logstore keeps;
--  (logs.set-minimum Severity.Debug) changes it, with log-control, and
--  returns the previous one. A host (the Workbench, the console) answers from logstore; this
--  package holds only the types, the catalog publication and the images.
package CCL.Interfaces.Logs with SPARK_Mode is

   --  What Publish adds to a catalog: one interface and this many data
   --  schemas. CCL_Host_Environment sums these to check the catalog's
   --  capacity at compile time; tests/ccl-install's probe checks a real run.
   PUBLISHED_INTERFACES : constant := 1;
   PUBLISHED_SCHEMAS : constant := 3;
   --  Declared types Publish adds to the catalog's registry (its named
   --  types and the list types it specializes).
   PUBLISHED_TYPES : constant := 8;
   use Standard.Interfaces;

   --  The types, as CCL source: checked by the CCL type checker when the
   --  interface is published. The digest and keys are SHA-256 of it (then
   --  "#" and the type's name); tests/ccl-console checks them.
   --  (logs.recent "service") is a List<LogItem> (LOG-005): the records
   --  (LogItem.Logged), each gap where records are missing (LogItem.Gap: how
   --  many, and why: given up by logstore's retained history, lost on the
   --  way to this reader, or retained but not shown because one result
   --  holds no more), then each instance's accounting (LogItem.Tally). A
   --  gap's and a tally's message says the same for people ("... 300
   --  records not retained ..."), so the literal reads as it means and a
   --  table of the list (CCL.Types.Shapes) shows it in LogEntry's message
   --  column. (logs.between (LogWindow "service" from to)) is the same list
   --  for the records whose time is in from .. to (milliseconds since boot,
   --  LogEntry's time), with the gaps that touch the window and the
   --  tallies; records outside it are not asked for, so they are in no gap.
   TYPE_SOURCE : constant String :=
     "(type Severity (enum Trace Debug Information Warning Error Critical)) (type LogEntry (record " &
     "(time Integer) (severity Severity) (source Integer) (message String))) (type GapReason (enum " &
     "Not_Retained Lost Not_Shown)) (type LogGap (record (time Integer) (source Integer) (count " &
     "Integer) (reason GapReason) (message String))) (type LogTally (record (source Integer) " &
     "(published Integer) (retained Integer) (not_retained Integer) (over_budget Integer) " &
     "(publisher_full Integer) (message String))) (type LogItem (variant (Logged LogEntry) (Gap " &
     "LogGap) (Tally LogTally))) (type LogWindow (record (service String) (from Integer) (to " &
     "Integer)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#87C2_A347_DEF4_1635#,
      16#6898_DA90_436A_5411#,
      16#4BCD_0B68_BA12_8CAA#,
      16#79D8_917C_E7B0_D05B#];
   SCHEMA_KEY : constant CCL.Objects.Schema_Key :=
     [16#726E_BEAC_373A_31C7#,
      16#2562_79D6_7155_D91D#,
      16#F89A_4E1B_7234_6D0D#,
      16#D79A_0860_7132_1850#];

   SEVERITY_KEY : constant CCL.Objects.Schema_Key :=
     [16#0860_D189_AD26_50D2#,
      16#CBB5_5E05_477A_3C82#,
      16#5AAB_08D3_4B6D_81AF#,
      16#4F65_95F4_2FBF_0B8F#];
   --  (logs.between (LogWindow "service" from to))'s argument.
   WINDOW_KEY : constant CCL.Objects.Schema_Key :=
     [16#B316_1C11_FBF7_E1E9#,
      16#A839_62C1_D592_6CA8#,
      16#4BB6_7607_C8F3_8703#,
      16#7FB6_0A30_C880_18CE#];

   --  Severity's members, in order (CuBit.Log_Records.Severity).
   type Severity is (Trace, Debug, Information, Warning, Error, Critical);

   --  A service name in (logs.recent "name") and a LogWindow.
   MAX_SERVICE_NAME : constant := 64;

   --  A record's time: milliseconds since boot, as logstore stamps it.
   subtype Boot_Milliseconds is Unsigned_64;
   --  The records a result covers: those whose time is in From .. To.
   type Log_Window is record
      From, To : Boot_Milliseconds := 0;
   end record;
   WHOLE_HISTORY : constant Log_Window := (From => Boot_Milliseconds'First, To => Boot_Milliseconds'Last);
   function Valid (Within : Log_Window) return Boolean is (Within.From <= Within.To);
   function Holds (Within : Log_Window; Time : Boot_Milliseconds) return Boolean is
     (Time in Within.From .. Within.To);
   --  Whether the span First .. Last (a gap, between the record before it
   --  and the next) touches the window.
   function Overlaps (Within : Log_Window; First, Last : Boot_Milliseconds) return Boolean is
     (First <= Within.To and then Last >= Within.From);

   --  Why records are missing at a gap (GapReason's members, in order).
   type Gap_Reason is (Not_Retained, Lost, Not_Shown);

   --  Cells one item takes in an image: its LogItem member (and payload
   --  record) cell, then a record's time, severity (member and unit
   --  payload), source and message; a gap's time, source, count, reason
   --  (member and unit payload) and message; a tally's six counts and
   --  message.
   ENTRY_CELLS : constant := 7;
   GAP_CELLS : constant := 8;
   TALLY_CELLS : constant := 9;
   --  The records and gaps one result shows (CAPACITY): a result is as
   --  large as the history it holds, up to this; past it the middle of the
   --  history is one Not_Shown gap. Every item at the most cells one takes
   --  fits one value's cell ceiling.
   RESULT_ITEM_CEILING : constant := 65_536;
   pragma Compile_Time_Error
     (1 + (RESULT_ITEM_CEILING + 16) * TALLY_CELLS > CCL.Objects.VALUE_CELL_CEILING,
      "a result's items must fit one value's cells");
   subtype Item_Count is Natural range 0 .. RESULT_ITEM_CEILING;
   --  The sources (instances) one service name may have had this boot.
   MAX_NAMED_SOURCES : constant := 64;
   --  A gap's and a tally's message, as Add_Gap and Add_Tally write them.
   function Gap_Message (Count : Unsigned_64; Reason : Gap_Reason) return String;
   function Tally_Message
     (Published, Retained, Not_Retained, Over_Budget, Publisher_Full : Unsigned_64) return String;

   --  Severity, LogEntry, GapReason, LogGap, LogTally, LogItem and
   --  List-LogItem and LogWindow in Types, and the bindings of the list type
   --  to SCHEMA_KEY, of Severity to SEVERITY_KEY and of LogWindow to
   --  WINDOW_KEY.
   procedure Define_Types
     (Types : in out CCL.Types.Registry; Entries : out CCL.Types.Type_Reference;
      Contract, Severity_Contract, Window_Contract : out CCL.Objects.Binding; Accepted : out Boolean);

   --  LogWindow's fields, in order.
   type Window_Field is (Service_Field, From_Field, To_Field);

   type Operation is (Recent, Minimum, Set_Minimum, Between);
   function Name (Op : Operation) return String is
     (case Op is
         when Recent => "recent", when Minimum => "minimum", when Set_Minimum => "set-minimum",
         when Between => "between");

   --  The schemas and the logs interface (its operations), for discovery only: a host
   --  separately installs a binding for the operations it answers.
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);

   --  A Severity value, and the member a Severity value holds.
   procedure Severity_Value
     (Contract : CCL.Objects.Binding; Level : Severity; Result : out CCL.Objects.Image; Built : out Boolean);
   procedure Severity_Of
     (Contract : CCL.Objects.Binding; Value : CCL.Objects.Image; Level : out Severity; Found : out Boolean);

   --  Building a result: Start an empty list, Add items while Added, the
   --  value is then complete and valid for the contract, as large as its
   --  items. Each adds a whole item or nothing: Added False only when the
   --  item would take the value past its ceilings.
   procedure Start (Contract : CCL.Objects.Binding; Result : out CCL.Objects.Holders.Holder);
   --  A record: (LogItem.Logged (LogEntry time severity source message)).
   procedure Add
     (Result : in out CCL.Objects.Holders.Holder; Time : Unsigned_64; Level : Severity;
      Source : Unsigned_64; Message : String; Added : out Boolean);
   --  A gap: (LogItem.Gap (LogGap time source count reason message)),
   --  Time where it falls (the record's just before it).
   procedure Add_Gap
     (Result : in out CCL.Objects.Holders.Holder; Time, Source, Count : Unsigned_64; Reason : Gap_Reason;
      Added : out Boolean);
   --  An instance's accounting: (LogItem.Tally (LogTally source published
   --  retained not_retained over_budget publisher_full message)).
   procedure Add_Tally
     (Result : in out CCL.Objects.Holders.Holder;
      Source, Published, Retained, Not_Retained, Over_Budget, Publisher_Full : Unsigned_64;
      Added : out Boolean);
end CCL.Interfaces.Logs;
