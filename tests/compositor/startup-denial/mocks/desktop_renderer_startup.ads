with Interfaces; with Compositor_Pool; with Compositor_Backend_Selection;
package Desktop_Renderer_Startup with SPARK_Mode is
 type Pipeline_Diagnostic is record
  Stage, Index : Interfaces.Unsigned_32 := 0;
  Result : Interfaces.Integer_32 := 0; Valid : Boolean := False;
 end record;
 procedure Initialize(Configuration : Boolean; Width, Height : Interfaces.Unsigned_64;
 Epoch : Compositor_Pool.ID; Evidence : out Compositor_Backend_Selection.Readiness;
 Diagnostic : out Pipeline_Diagnostic);
 procedure Stop;
end Desktop_Renderer_Startup;
