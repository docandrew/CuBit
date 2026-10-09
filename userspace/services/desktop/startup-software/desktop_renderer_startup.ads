with Interfaces;
with Desktop_Breadcrumbs;
with Desktop_Logs;
with Desktop_Log_IO;
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
   procedure Initialize
     (Configuration : Boolean; Width, Height : Interfaces.Unsigned_64;
      Epoch : Compositor_Pool.ID;
      Evidence : out Compositor_Backend_Selection.Readiness;
      Diagnostic : out Pipeline_Diagnostic)
     with Global => (In_Out => (Desktop_Breadcrumbs.State, Desktop_Logs.State, Desktop_Log_IO.State)),
       Post => Evidence = Compositor_Backend_Selection.Readiness'(others => False)
         and not Diagnostic.Valid;
   -- Retains uncertain startup backing; never implies physical reclamation.
   procedure Stop with Global => null;
end Desktop_Renderer_Startup;
