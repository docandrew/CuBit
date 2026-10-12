with Desktop_Breadcrumbs;
--  The renderer-startup step in progress, written by the thread that starts
--  the renderer (Desktop_Renderer_Worker) and read by the main thread for
--  the bootstrap screen and its checkpoint log lines. One atomic byte; the
--  writer never logs, allocates or uses the secondary stack.
package Desktop_Startup_Progress with SPARK_Mode,
  Abstract_State => (State with External => (Async_Readers, Async_Writers))
is
   subtype Stage is Desktop_Breadcrumbs.Stage;
   --  The step about to run (or just finished).
   procedure Enter (Point : Stage) with Global => (In_Out => State);
   function Current return Stage with Volatile_Function, Global => State;
end Desktop_Startup_Progress;
