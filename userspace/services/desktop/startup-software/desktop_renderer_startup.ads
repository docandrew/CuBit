with Interfaces;
with Desktop_Breadcrumbs;
with Desktop_Logs;
with Desktop_Log_IO;
with Desktop_Startup_Clock;
with Compositor_Pool;
with Compositor_Backend_Selection;
-- Native startup boundary. Readiness is evidence for the proved selection
-- policy, not proof of device completion or physical backing retirement.
package Desktop_Renderer_Startup with SPARK_Mode is
   use type Compositor_Backend_Selection.Readiness;
   type Pipeline_Diagnostic is record
      Stage, Index : Interfaces.Unsigned_32 := 0;
      Result : Interfaces.Integer_32 := 0;
      Valid : Boolean := False;
   end record;
   -- Software-only build: no GPU renderer exists to fall back from.
   GPU_Capable : constant Boolean := False;
   procedure Initialize
     (Configuration : Boolean; Width, Height : Interfaces.Unsigned_64;
      Epoch : Compositor_Pool.ID;
      Evidence : out Compositor_Backend_Selection.Readiness;
      Diagnostic : out Pipeline_Diagnostic)
     with Global => (Input => Desktop_Startup_Clock.Clock,
                     In_Out => (Desktop_Breadcrumbs.State, Desktop_Logs.State, Desktop_Log_IO.State)),
       Post => Evidence = Compositor_Backend_Selection.Readiness'(others => False)
         and not Diagnostic.Valid;
   -- Same interface as the Vulkan build (Desktop_Renderer_Worker, main).
   procedure Run
     (Configuration : Boolean; Width, Height : Interfaces.Unsigned_64;
      Epoch : Compositor_Pool.ID;
      Evidence : out Compositor_Backend_Selection.Readiness;
      Diagnostic : out Pipeline_Diagnostic)
     with Global => null,
       Post => Evidence = Compositor_Backend_Selection.Readiness'(others => False)
         and not Diagnostic.Valid;
   procedure Report
     (Evidence : Compositor_Backend_Selection.Readiness;
      Diagnostic : Pipeline_Diagnostic) with Global => null;
   procedure Report_Timeouts with Global => null;
   -- Retains uncertain startup backing; never implies physical reclamation.
   procedure Stop with Global => null;
end Desktop_Renderer_Startup;
