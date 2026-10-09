with Desktop_Breadcrumbs;
package body Desktop_Renderer_Startup with SPARK_Mode is
   procedure Initialize
     (Configuration : Boolean; Width, Height : Interfaces.Unsigned_64;
      Epoch : Compositor_Pool.ID;
      Evidence : out Compositor_Backend_Selection.Readiness;
      Diagnostic : out Pipeline_Diagnostic) is
      pragma Unreferenced (Configuration, Width, Height, Epoch);
   begin
      -- No device admission or foreign-library elaboration in a software build.
      Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Init_Before);
      Evidence := (others => False);
      Diagnostic := (others => <>);
      Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Init_After);
   end Initialize;
   procedure Stop is null;
end Desktop_Renderer_Startup;
