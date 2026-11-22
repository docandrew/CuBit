with Desktop_Breadcrumbs;
package body Desktop_Renderer_Startup with SPARK_Mode is
   procedure Initialize
     (Configuration : Boolean; Policy : Compositor_Backend_Selection.Composition;
      Width, Height : Interfaces.Unsigned_64;
      Epoch : Compositor_Pool.ID;
      Evidence : out Compositor_Backend_Selection.Readiness;
      Diagnostic : out Pipeline_Diagnostic) is
      pragma Unreferenced (Configuration, Policy, Width, Height, Epoch);
   begin
      -- No device admission or foreign-library elaboration in a software build.
      Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Init_Before);
      Evidence := (others => False);
      Diagnostic := (others => <>);
      Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Init_After);
   end Initialize;
   procedure Run
     (Configuration : Boolean; Policy : Compositor_Backend_Selection.Composition;
      Width, Height : Interfaces.Unsigned_64;
      Epoch : Compositor_Pool.ID;
      Evidence : out Compositor_Backend_Selection.Readiness;
      Diagnostic : out Pipeline_Diagnostic) is
      pragma Unreferenced (Configuration, Policy, Width, Height, Epoch);
   begin
      Evidence := (others => False);
      Diagnostic := (others => <>);
   end Run;
   procedure Report
     (Evidence : Compositor_Backend_Selection.Readiness;
      Diagnostic : Pipeline_Diagnostic) is null;
   procedure Report_Timeouts is null;
   procedure Stop is null;
end Desktop_Renderer_Startup;
