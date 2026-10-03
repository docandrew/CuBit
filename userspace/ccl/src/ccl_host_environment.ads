with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;
with CCL.Streams;

--  Every system interface a CCL front end offers (the Workbench, the console,
--  the remote session host): discovery, grants and dispatch in one place, so
--  no interface exists in one front end only. A front end adds only its own
--  UI interfaces (the Workbench's ui.*), and supplies its platform's clock.
generic
   --  Monotonic milliseconds, or Available = False.
   with function Monotonic_Ms (Available : out Boolean) return Interfaces.Unsigned_64;
package CCL_Host_Environment is
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
   --  Milliseconds until the next element is due: 0 when one is due now,
   --  Unsigned_64'Last when no stream is open.
   function Next_Stream_Delay return Interfaces.Unsigned_64;
end CCL_Host_Environment;
