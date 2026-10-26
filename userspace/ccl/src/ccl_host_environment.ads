with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;
with CCL.Streams;
with CCL.Objects.Catalog;
with CCL.Types;
with CCL.Interfaces.Config;
with CCL.Interfaces.Logs;
with CCL.Interfaces.Images;
with CCL.Interfaces.Files;
with CCL.Interfaces.Processes;
with CCL.Interfaces.Wifi;
with CCL.Interfaces.Lexicon;
with CCL.Interfaces.Calc;
with CCL.Interfaces.Install;
with CCL.Interfaces.Clock;
with CCL.Interfaces.Timer;
with CCL.Interfaces.Programs;
with CCL.Interfaces.Console;
with CCL_Execution;

--  Every system interface a CCL front end offers (the Workbench, the console,
--  the remote session host): discovery, grants and dispatch in one place, so
--  no interface exists in one front end only. A front end adds only its own
--  UI interfaces (the Workbench's ui.*), and supplies its platform's clock.
generic
   --  Monotonic milliseconds, or Available = False.
   with function Monotonic_Ms (Available : out Boolean) return Interfaces.Unsigned_64;
package CCL_Host_Environment is
   --  The most Install publishes, summed from each interface's own counts,
   --  with every program a front end may launch, plus one front-end
   --  interface (the console's console.*, the Workbench's ui.*): checked
   --  against the catalog's capacity below, so adding an interface that
   --  does not fit fails to compile instead of failing at startup.
   MOST_INTERFACES : constant :=
     CCL.Interfaces.Config.PUBLISHED_INTERFACES + CCL_Execution.PUBLISHED_INTERFACES +
     CCL.Interfaces.Logs.PUBLISHED_INTERFACES + CCL.Interfaces.Images.PUBLISHED_INTERFACES +
     CCL.Interfaces.Files.PUBLISHED_INTERFACES + CCL.Interfaces.Processes.PUBLISHED_INTERFACES +
     CCL.Interfaces.Wifi.PUBLISHED_INTERFACES +
     CCL.Interfaces.Lexicon.PUBLISHED_INTERFACES + CCL.Interfaces.Calc.PUBLISHED_INTERFACES +
     CCL.Interfaces.Install.PUBLISHED_INTERFACES + CCL.Interfaces.Clock.PUBLISHED_INTERFACES +
     CCL.Interfaces.Timer.PUBLISHED_INTERFACES +
     CCL.Interfaces.Programs.MAXIMUM_PROGRAMS * CCL.Interfaces.Programs.PER_PROGRAM_INTERFACES +
     CCL.Interfaces.Console.PUBLISHED_INTERFACES;
   MOST_SCHEMAS : constant :=
     CCL.Interfaces.Config.PUBLISHED_SCHEMAS + CCL_Execution.PUBLISHED_SCHEMAS +
     CCL.Interfaces.Logs.PUBLISHED_SCHEMAS + CCL.Interfaces.Images.PUBLISHED_SCHEMAS +
     CCL.Interfaces.Files.PUBLISHED_SCHEMAS + CCL.Interfaces.Processes.PUBLISHED_SCHEMAS +
     CCL.Interfaces.Wifi.PUBLISHED_SCHEMAS +
     CCL.Interfaces.Lexicon.PUBLISHED_SCHEMAS + CCL.Interfaces.Calc.PUBLISHED_SCHEMAS +
     CCL.Interfaces.Install.PUBLISHED_SCHEMAS + CCL.Interfaces.Clock.PUBLISHED_SCHEMAS +
     CCL.Interfaces.Timer.PUBLISHED_SCHEMAS + CCL.Interfaces.Programs.SHARED_SCHEMAS +
     CCL.Interfaces.Programs.MAXIMUM_PROGRAMS * CCL.Interfaces.Programs.PER_PROGRAM_SCHEMAS +
     CCL.Interfaces.Console.PUBLISHED_SCHEMAS;
   MOST_TYPES : constant :=
     CCL.Interfaces.Config.PUBLISHED_TYPES + CCL_Execution.PUBLISHED_TYPES +
     CCL.Interfaces.Logs.PUBLISHED_TYPES + CCL.Interfaces.Images.PUBLISHED_TYPES +
     CCL.Interfaces.Files.PUBLISHED_TYPES + CCL.Interfaces.Processes.PUBLISHED_TYPES +
     CCL.Interfaces.Wifi.PUBLISHED_TYPES +
     CCL.Interfaces.Lexicon.PUBLISHED_TYPES + CCL.Interfaces.Calc.PUBLISHED_TYPES +
     CCL.Interfaces.Install.PUBLISHED_TYPES + CCL.Interfaces.Clock.PUBLISHED_TYPES +
     CCL.Interfaces.Timer.PUBLISHED_TYPES + CCL.Interfaces.Programs.SHARED_TYPES +
     CCL.Interfaces.Programs.MAXIMUM_PROGRAMS * CCL.Interfaces.Programs.PER_PROGRAM_TYPES +
     CCL.Interfaces.Console.PUBLISHED_TYPES;
   --  Room a session keeps in its copy of the registry for its own type
   --  declarations and the Task, Stream and List types its calls need
   --  (system.install's Task<Install_Report>, a timer's Stream<Integer>).
   SESSION_TYPES : constant := 24;
   pragma Compile_Time_Error
     (MOST_INTERFACES > CCL.Catalog.MAX_INTERFACES, "the CCL catalog cannot hold every interface");
   pragma Compile_Time_Error
     (MOST_TYPES + SESSION_TYPES > CCL.Types.Maximum_Declarations,
      "the CCL type registry cannot hold every interface's types and a session's own");
   pragma Compile_Time_Error
     (MOST_SCHEMAS > CCL.Objects.Catalog.Maximum_Schemas, "the CCL catalog cannot hold every schema");

   --  config (inspector) and typed Config collections, clock, logs: each
   --  published and granted only where this process may use it.
   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean);
   function Handles (Binding : Interfaces.Unsigned_32) return Boolean;
   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result);

   --  The session's streams (docs/ccl-streams.md): sources such as
   --  timer.every open them; evaluations read views through Read_Stream.
   procedure Read_Stream
     (Request : CCL.Streams.View_Request; Reply : in out CCL.Streams.View_Reply);
   --  Deliver what is due now. Arrived: at least one stream advanced, so
   --  live cells that read streams should run again.
   procedure Pump_Streams (Arrived : out Boolean);
   --  Close every stream (the session was reset or cleared).
   procedure Reset_Streams;
   --  Close every stream Held does not claim: after each entry, what the
   --  session no longer binds (a bare (timer.every n), a rebound name).
   generic
      with function Held (Handle : CCL.Streams.Handle) return Boolean;
   procedure Retain_Streams;
   function Open_Streams return Natural;
   --  Whether task Handle completed (an entry waiting on it can resume).
   function Task_Done (Handle : CCL.Streams.Handle) return Boolean;
   --  Milliseconds until the next element is due, or the next query about
   --  a followed install: 0 when one is due now, Unsigned_64'Last when
   --  nothing is.
   function Next_Stream_Delay return Interfaces.Unsigned_64;
end CCL_Host_Environment;
