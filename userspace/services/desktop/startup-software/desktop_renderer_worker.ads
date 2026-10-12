with Interfaces;
with Compositor_Pool;
with Compositor_Backend_Selection;
with Desktop_Renderer_Startup;
--  Software-only build: no renderer to start, so no thread; Start runs the
--  (empty) startup at once. Same interface as the Vulkan build's worker.
package Desktop_Renderer_Worker is
   procedure Start
     (Configuration : Boolean; Width, Height : Interfaces.Unsigned_64;
      Epoch : Compositor_Pool.ID; Started : out Boolean);
   function Finished return Boolean;
   procedure Take
     (Evidence : out Compositor_Backend_Selection.Readiness;
      Diagnostic : out Desktop_Renderer_Startup.Pipeline_Diagnostic)
     with Pre => Finished;
end Desktop_Renderer_Worker;
