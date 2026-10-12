package body Desktop_Renderer_Worker is
   Ran : Boolean := False;
   Result : Compositor_Backend_Selection.Readiness := (others => False);
   Diagnosis : Desktop_Renderer_Startup.Pipeline_Diagnostic := (others => <>);
   procedure Start
     (Configuration : Boolean; Width, Height : Interfaces.Unsigned_64;
      Epoch : Compositor_Pool.ID; Started : out Boolean) is
   begin
      Desktop_Renderer_Startup.Run (Configuration, Width, Height, Epoch, Result, Diagnosis);
      Ran := True;
      Started := True;
   end Start;
   function Finished return Boolean is (Ran);
   procedure Take
     (Evidence : out Compositor_Backend_Selection.Readiness;
      Diagnostic : out Desktop_Renderer_Startup.Pipeline_Diagnostic) is
   begin
      Evidence := Result;
      Diagnostic := Diagnosis;
   end Take;
end Desktop_Renderer_Worker;
