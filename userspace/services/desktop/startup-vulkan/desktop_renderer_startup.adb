with Desktop_Startup_Layout;
with Vulkan_Device_Pipeline_FFI;
with Desktop_Vulkan_Startup;
with Vulkan_Device_Owner;
with CuBit.Handles;
with Desktop_Endpoints;
with Mesa_Service;
pragma Elaborate_All (Mesa_Service);
with Vulkan_Context_Owner;
pragma Elaborate_All (Vulkan_Context_Owner);
with Desktop_Breadcrumbs;
with Desktop_Startup_Progress;
with Desktop_Logs;
with Desktop_GPU_Calls;
package body Desktop_Renderer_Startup with SPARK_Mode,
  Refined_State => (Observed => (Admission, Device_Created)) is
   use Interfaces;
   --  The longest one GPU-service reply may take before Desktop gives up the
   --  GPU path for software (IPC-004). A legitimate operation is far below it;
   --  one that is not is itself a fault worth reporting.
   GPU_Call_Timeout_Ms : constant := 5_000;
   package GPU renames Desktop_Vulkan_Startup;
   procedure debugPrint (Text : String) renames Desktop_Logs.Write;
   -- Shared staging for batched source uploads (GPU-004): one transfer
   -- submission carries every stale image that fits, so a full-screen
   -- window (1920x1080 BGRA, about 8 MiB) still goes in one chunk.
   Upload_Staging_Bytes : constant := 8 * 1024 * 1024;
   -- What Run observed, for Report on the main thread.
   type Slot_Admission is (Admitted, Empty_Slot, Unknown_Slot);
   Admission : Slot_Admission := Unknown_Slot;
   Device_Created : Boolean := False;
   procedure Run
     (Configuration : Boolean; Width, Height : Unsigned_64;
      Epoch : Compositor_Pool.ID;
      Evidence : out Compositor_Backend_Selection.Readiness;
      Diagnostic : out Pipeline_Diagnostic) is
      use type Vulkan_Device_Owner.Phase;
      package Step renames Desktop_Startup_Progress;
      Render : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
      Bytes : Natural;
   begin
      Evidence := (others => False);
      Diagnostic := (others => <>);
      Admission := Unknown_Slot;
      Desktop_GPU_Calls.Bound (GPU_Call_Timeout_Ms);
      --  procmgr installs the render entry only for an admitted session.
      Evidence.Admitted := CuBit.Handles.Valid (Desktop_Endpoints.Render);
      if Evidence.Admitted then Render := Desktop_Endpoints.Render; Admission := Admitted;
      else Admission := Empty_Slot;
      end if;
      Step.Enter (Desktop_Breadcrumbs.Init_Before);
      GPU.Initialize (Render);
      Step.Enter (Desktop_Breadcrumbs.Init_After);
      Device_Created := GPU.Current = Vulkan_Device_Owner.Ready;
      Evidence.Device := Device_Created;
      if Evidence.Device then Step.Enter (Desktop_Breadcrumbs.Health_Before);
      GPU.Check_Health (Evidence.Device);
      Step.Enter (Desktop_Breadcrumbs.Health_After); end if;
      Bytes := Desktop_Startup_Layout.Required_Bytes (Width, Height);
      Evidence.Configuration := Configuration and then Epoch /= 0 and then Bytes /= 0;
      if Evidence.Device and then Evidence.Configuration then
         Step.Enter (Desktop_Breadcrumbs.Targets_Before);
      GPU.Configure_Targets (Unsigned_32 (Width), Unsigned_32 (Height),
           Epoch, 128 * 1024 * 1024, Evidence.Targets);
      Step.Enter (Desktop_Breadcrumbs.Targets_After);
         if Evidence.Targets then Step.Enter (Desktop_Breadcrumbs.Pipeline_Before);
      GPU.Prepare_Pipeline (Evidence.Pipeline);
      if not Evidence.Pipeline then
         declare
            Stage, Index : Unsigned_32;
            Result : Integer_32;
            Valid : Boolean;
         begin
            Vulkan_Device_Pipeline_FFI.Last_Failure (Stage, Index, Result, Valid);
            if Valid then
               Diagnostic.Stage := Stage;
               Diagnostic.Index := Index;
               Diagnostic.Result := Result;
               Diagnostic.Valid := True;
            end if;
         end;
      end if;
      Step.Enter (Desktop_Breadcrumbs.Pipeline_After); end if;
         if Evidence.Pipeline then
            Step.Enter (Desktop_Breadcrumbs.Upload_Before);
      GPU.Configure_Upload (Upload_Staging_Bytes, Evidence.Upload);
      Step.Enter (Desktop_Breadcrumbs.Upload_After);
            -- Upload storage is required for this backend. Once it is denied,
            -- readback cannot make the renderer usable; retain failure evidence
            -- and avoid another allocation before software recovery.
            if Evidence.Upload then
               Step.Enter (Desktop_Breadcrumbs.Readback_Before);
               GPU.Configure_Readback (Bytes, Evidence.Readback);
               Step.Enter (Desktop_Breadcrumbs.Readback_After);
            end if;
         end if;
      end if;
   end Run;

   procedure Report
     (Evidence : Compositor_Backend_Selection.Readiness;
      Diagnostic : Pipeline_Diagnostic) is
   begin
      case Admission is
         when Empty_Slot =>
            debugPrint ("DESKTOP-OPTIONAL-RENDER: PASS empty slot" & ASCII.LF);
         when Unknown_Slot =>
            debugPrint ("desktop: render slot invalid; software selected" & ASCII.LF);
         when Admitted => null;
      end case;
      if Evidence.Targets and then not Evidence.Pipeline then
         if Diagnostic.Valid then
            debugPrint ("DESKTOP-VULKAN: pipeline failure stage=" &
              Diagnostic.Stage'Image & " index=" & Diagnostic.Index'Image &
              " vk=" & Diagnostic.Result'Image & ASCII.LF);
         else
            debugPrint ("DESKTOP-VULKAN: pipeline failure diagnostic unavailable" & ASCII.LF);
         end if;
      end if;
      -- Report the first unmet gate, not an inferred allocation/quota cause.
      if not Evidence.Admitted then
         debugPrint ("DESKTOP-VULKAN: setup unavailable stage=admission" & ASCII.LF);
      elsif not Device_Created then
         debugPrint ("DESKTOP-VULKAN: setup unavailable stage=device" & ASCII.LF);
      elsif not Evidence.Device then
         debugPrint ("DESKTOP-VULKAN: setup unavailable stage=health" & ASCII.LF);
      elsif not Evidence.Configuration then
         debugPrint ("DESKTOP-VULKAN: setup unavailable stage=configuration" & ASCII.LF);
      elsif not Evidence.Targets then
         debugPrint ("DESKTOP-VULKAN: setup unavailable stage=targets" & ASCII.LF);
      elsif not Evidence.Pipeline then
         debugPrint ("DESKTOP-VULKAN: setup unavailable stage=pipeline" & ASCII.LF);
      elsif not Evidence.Upload then
         debugPrint ("DESKTOP-VULKAN: setup unavailable stage=upload" & ASCII.LF);
      elsif not Evidence.Readback then
         debugPrint ("DESKTOP-VULKAN: setup unavailable stage=readback" & ASCII.LF);
      end if;
      Report_Timeouts;
   end Report;

   procedure Report_Timeouts is
      Timeouts, Label : Unsigned_32;
   begin
      Desktop_GPU_Calls.Read (Timeouts, Label);
      if Timeouts /= 0 then
         debugPrint ("DESKTOP-VULKAN: GPU service call timed out count=" &
           Timeouts'Image & " label=" & Label'Image & ASCII.LF);
      end if;
   end Report_Timeouts;

   procedure Initialize
     (Configuration : Boolean; Width, Height : Unsigned_64;
      Epoch : Compositor_Pool.ID;
      Evidence : out Compositor_Backend_Selection.Readiness;
      Diagnostic : out Pipeline_Diagnostic) is
   begin
      Run (Configuration, Width, Height, Epoch, Evidence, Diagnostic);
      Report (Evidence, Diagnostic);
   end Initialize;
   procedure Stop is
   begin
      GPU.Stop;
   end Stop;
end Desktop_Renderer_Startup;
