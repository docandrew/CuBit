with Interfaces; with Compositor_Pool; with Compositor_Backend_Selection;
package Desktop_Renderer_Startup with SPARK_Mode,
  Abstract_State => Observed, Initializes => Observed is
 type Pipeline_Diagnostic is record
  Stage, Index : Interfaces.Unsigned_32 := 0;
  Result : Interfaces.Integer_32 := 0; Valid : Boolean := False;
 end record;
 procedure Run(Configuration : Boolean; Width, Height : Interfaces.Unsigned_64;
 Epoch : Compositor_Pool.ID; Evidence : out Compositor_Backend_Selection.Readiness;
 Diagnostic : out Pipeline_Diagnostic);
 procedure Report(Evidence : Compositor_Backend_Selection.Readiness;
 Diagnostic : Pipeline_Diagnostic);
 procedure Report_Timeouts;
 procedure Initialize(Configuration : Boolean; Width, Height : Interfaces.Unsigned_64;
 Epoch : Compositor_Pool.ID; Evidence : out Compositor_Backend_Selection.Readiness;
 Diagnostic : out Pipeline_Diagnostic);
 procedure Stop;
end Desktop_Renderer_Startup;
