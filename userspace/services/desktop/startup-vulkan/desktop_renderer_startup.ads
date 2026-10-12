with Interfaces;
with Desktop_Startup_Layout;
with Desktop_Startup_Progress;
with Desktop_Logs;
with Desktop_Log_IO;
with Desktop_Vulkan_Startup;
with Desktop_GPU_Calls;
with Compositor_Pool;
with Compositor_Backend_Selection;
-- Native startup boundary. Readiness is evidence for the proved selection
-- policy, not proof of device completion or physical backing retirement.
package Desktop_Renderer_Startup with SPARK_Mode,
  Abstract_State => Observed, Initializes => Observed is
   use type Interfaces.Unsigned_64;
   type Pipeline_Diagnostic is record
      Stage, Index : Interfaces.Unsigned_32 := 0;
      Result : Interfaces.Integer_32 := 0;
      Valid : Boolean := False;
   end record;
   -- A GPU renderer is built in; selecting software is a fallback to announce.
   GPU_Capable : constant Boolean := True;
   -- The foreign work of startup, without logging: safe on the renderer
   -- worker thread (Desktop_Renderer_Worker). The step in progress goes to
   -- Desktop_Startup_Progress; what to log is kept for Report.
   procedure Run
     (Configuration : Boolean; Width, Height : Interfaces.Unsigned_64;
      Epoch : Compositor_Pool.ID;
      Evidence : out Compositor_Backend_Selection.Readiness;
      Diagnostic : out Pipeline_Diagnostic)
     with Global => (Output => Observed,
                     In_Out => (Desktop_Startup_Progress.State,
                                Desktop_Vulkan_Startup.Engine, Desktop_GPU_Calls.Service)),
       Pre => Desktop_Vulkan_Startup.Valid,
       Post => Desktop_Vulkan_Startup.Valid and
         Evidence.Configuration = (Configuration and Epoch /= 0 and
           Desktop_Startup_Layout.Supported (Width, Height)) and
         (if Evidence.Targets then Evidence.Device and Evidence.Configuration) and
         (if Evidence.Pipeline then Evidence.Targets) and
         (if Evidence.Upload then Evidence.Pipeline) and
         (if Evidence.Readback then Evidence.Upload) and
         (if Diagnostic.Valid then not Evidence.Pipeline);
   -- Run's log lines (first unmet gate, pipeline failure, call timeouts),
   -- on the main thread once Run has returned.
   procedure Report
     (Evidence : Compositor_Backend_Selection.Readiness;
      Diagnostic : Pipeline_Diagnostic)
     with Global => (Input => Observed,
                     In_Out => (Desktop_GPU_Calls.Service, Desktop_Logs.State, Desktop_Log_IO.State));
   -- The GPU-service call timeouts so far, if any (also part of Report).
   procedure Report_Timeouts
     with Global => (In_Out => (Desktop_GPU_Calls.Service, Desktop_Logs.State, Desktop_Log_IO.State));
   -- Run then Report, on one thread.
   procedure Initialize
     (Configuration : Boolean; Width, Height : Interfaces.Unsigned_64;
      Epoch : Compositor_Pool.ID;
      Evidence : out Compositor_Backend_Selection.Readiness;
      Diagnostic : out Pipeline_Diagnostic)
     with Global => (Output => Observed,
                     In_Out => (Desktop_Startup_Progress.State,
                                Desktop_Logs.State, Desktop_Log_IO.State,
                                Desktop_Vulkan_Startup.Engine, Desktop_GPU_Calls.Service)),
       Pre => Desktop_Vulkan_Startup.Valid,
       Post => Desktop_Vulkan_Startup.Valid and
         Evidence.Configuration = (Configuration and Epoch /= 0 and
           Desktop_Startup_Layout.Supported (Width, Height)) and
         (if Evidence.Targets then Evidence.Device and Evidence.Configuration) and
         (if Evidence.Pipeline then Evidence.Targets) and
         (if Evidence.Upload then Evidence.Pipeline) and
         (if Evidence.Readback then Evidence.Upload) and
         (if Diagnostic.Valid then not Evidence.Pipeline);
   -- Retains uncertain startup backing; never implies physical reclamation.
   procedure Stop
     with Global => (In_Out => Desktop_Vulkan_Startup.Engine),
       Pre => Desktop_Vulkan_Startup.Valid, Post => Desktop_Vulkan_Startup.Valid;
end Desktop_Renderer_Startup;
