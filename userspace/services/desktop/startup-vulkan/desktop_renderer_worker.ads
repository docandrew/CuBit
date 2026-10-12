with Interfaces;
with Compositor_Pool;
with Compositor_Backend_Selection;
with Desktop_Renderer_Startup;
--  Renderer startup on its own thread (NUC startup hang, 2026-10-11), so
--  the main thread keeps a deadline that nothing in the foreign GPU stack
--  can hold up, and keeps the bootstrap screen alive meanwhile.
--
--  The worker runs Desktop_Renderer_Startup.Run only: no logging and no
--  secondary-stack use (the user runtime has one secondary stack per
--  process). The main thread reads the results only after Finished.
--  Abandoned work is never touched again: the device it may still create
--  stays with that thread, and Desktop runs in software.
package Desktop_Renderer_Worker with SPARK_Mode => Off is
   --  Start the worker. Started = False: no thread (nothing was run).
   procedure Start
     (Configuration : Boolean; Width, Height : Interfaces.Unsigned_64;
      Epoch : Compositor_Pool.ID; Started : out Boolean);
   --  Run has returned on the worker.
   function Finished return Boolean;
   --  Run's results; only once Finished.
   procedure Take
     (Evidence : out Compositor_Backend_Selection.Readiness;
      Diagnostic : out Desktop_Renderer_Startup.Pipeline_Diagnostic)
     with Pre => Finished;
end Desktop_Renderer_Worker;
