with Desktop_Startup_Layout;
with Vulkan_Device_Pipeline_FFI;
with Desktop_Vulkan_Startup;
with Vulkan_Device_Owner;
with CCL_Manifest_Bindings;
with Mesa_Service;
pragma Elaborate_All (Mesa_Service);
with Vulkan_Context_Owner;
pragma Elaborate_All (Vulkan_Context_Owner);
with Desktop_Breadcrumbs;
with Desktop_Logs;
with Desktop_Capability_IO;
package body Desktop_Renderer_Startup with SPARK_Mode is
   use Interfaces;
   package GPU renames Desktop_Vulkan_Startup;
   procedure debugPrint (Text : String) renames Desktop_Logs.Write;
   procedure Initialize
     (Configuration : Boolean; Width, Height : Unsigned_64;
      Epoch : Compositor_Pool.ID;
      Evidence : out Compositor_Backend_Selection.Readiness;
      Diagnostic : out Pipeline_Diagnostic) is
      use type Vulkan_Device_Owner.Phase;
      Capability : Desktop_Capability_IO.Words;
      Inspected : Boolean;
      Device_Created : Boolean;
      Slot : Unsigned_64 := 0;
      Bytes : Natural;
   begin
      Evidence := (others => False);
      Diagnostic := (others => <>);
      Desktop_Capability_IO.Inspect
        (CCL_Manifest_Bindings.Slot_render, Capability, Inspected);
      Evidence.Admitted := Inspected and then Capability (0) = 1 and then
        Capability (1) = 3 and then Capability (3) /= 0;
      if Evidence.Admitted then Slot := CCL_Manifest_Bindings.Slot_render;
      elsif Inspected and then Capability (0) = 0 then
         debugPrint ("DESKTOP-OPTIONAL-RENDER: PASS empty slot" & ASCII.LF);
      else
         debugPrint ("desktop: render slot invalid; software selected" & ASCII.LF);
      end if;
      Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Init_Before);
      GPU.Initialize (Slot);
      Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Init_After);
      Device_Created := GPU.Current = Vulkan_Device_Owner.Ready;
      Evidence.Device := Device_Created;
      if Evidence.Device then Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Health_Before);
      GPU.Check_Health (Evidence.Device);
      Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Health_After); end if;
      Bytes := Desktop_Startup_Layout.Required_Bytes (Width, Height);
      Evidence.Configuration := Configuration and then Epoch /= 0 and then Bytes /= 0;
      if Evidence.Device and then Evidence.Configuration then
         Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Targets_Before);
      GPU.Configure_Targets (Unsigned_32 (Width), Unsigned_32 (Height),
           Epoch, 128 * 1024 * 1024, Evidence.Targets);
      Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Targets_After);
         if Evidence.Targets then Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Pipeline_Before);
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
            if Valid then
               debugPrint ("DESKTOP-VULKAN: pipeline failure stage=" &
                 Stage'Image & " index=" & Index'Image & " vk=" & Result'Image & ASCII.LF);
            else
               debugPrint ("DESKTOP-VULKAN: pipeline failure diagnostic unavailable" & ASCII.LF);
            end if;
         end;
      end if;
      Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Pipeline_After); end if;
         if Evidence.Pipeline then
            Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Upload_Before);
      GPU.Configure_Upload (2 * 1024 * 1024, Evidence.Upload);
      Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Upload_After);
            -- Upload storage is required for this backend. Once it is denied,
            -- readback cannot make the renderer usable; retain failure evidence
            -- and avoid another allocation before software recovery.
            if Evidence.Upload then
               Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Readback_Before);
               GPU.Configure_Readback (Bytes, Evidence.Readback);
               Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Readback_After);
            end if;
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
   end Initialize;
   procedure Stop is
   begin
      GPU.Stop;
   end Stop;
end Desktop_Renderer_Startup;
