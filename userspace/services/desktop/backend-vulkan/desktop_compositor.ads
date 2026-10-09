with Interfaces;
with Compositor_Damage;
with Desktop_Vulkan_Startup;
with Compositor_Text;
with Compositor_Backend_Selection;
with CuBit.Appearance;
with System;
with Compositor_Formats;
with Desktop_Composition;
with CuBit.Display_Geometry;
with Compositor_Pool;
package Desktop_Compositor with SPARK_Mode, Abstract_State => Engine, Initializes => Engine,
  Initial_Condition => Valid is
   function Valid return Boolean with Ghost, Global => (Input => Engine);
   type Transfer_Counters is record
      GPU_Submitted, CPU_Copied : Interfaces.Unsigned_64 := 0;
      Saturated : Boolean := False;
   end record;
   -- Submitted payload and completed synchronous copies, not physical bus traffic.
   function Readback_Work return Transfer_Counters with Global => (Input => Engine);
   procedure Configure_Renderer (Evidence : Compositor_Backend_Selection.Readiness; Accepted : out Boolean)
     with Global => (In_Out => Engine), Pre => Valid, Post => Valid;
   type Recovery_Result is (Recovery_Pending, Recovery_Complete, Recovery_Unsafe);
   procedure Recover_Renderer (Key : Compositor_Backend_Selection.Recovery_Key;
      Writer_Retired, Repaint_Queued : Boolean; Result : out Recovery_Result)
     with Global => (In_Out => (Engine, Desktop_Vulkan_Startup.Engine)),
       Pre => Valid and Desktop_Vulkan_Startup.Valid,
       Post => Valid and Desktop_Vulkan_Startup.Valid;
   function Selected return Boolean with Global => (Input => Engine);
   -- Deferred GPU output owns command capture and readback. This flag routes
   -- drawing through the renderer; Begin_Output returns required repaint regions.
   function Full_Output return Boolean with Global => (Input => Engine);
   function Software_Text return Boolean with Global => (Input => Engine);
   type Output_Start is (Started, Deferred, Start_Unsafe);
   -- Selected renderers receive Begin before any native-output drawing or
   -- damage consumption. Deferred must retain no new scope/readers and lets
   -- Desktop keep fresh input/damage pending. Started binds this output until
   -- Complete_Output finishes it; subsequent Pending observations only poll.
   -- Repaint contains new scene changes on entry. On Started it contains the
   -- selected renderer target repair; do not requeue repair as new damage.
   -- CPU/softpipe start synchronously. Buffer mapping authority remains with
   -- the existing output owner; this hook grants no GPU import/scanout rights.
   procedure Begin_Output
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Writer : Compositor_Pool.Ticket; Screen : CuBit.Display_Geometry.Output;
      Secondary : Boolean; Result : out Output_Start;
      Repaint : in out Compositor_Damage.State;
      Writer_Repair : Compositor_Damage.State)
     with Global => (In_Out => (Engine, Desktop_Vulkan_Startup.Engine)),
       Pre => Valid and Desktop_Vulkan_Startup.Valid and Compositor_Damage.Valid (Repaint) and Compositor_Damage.Valid (Writer_Repair),
       Post => Valid and Desktop_Vulkan_Startup.Valid and Compositor_Damage.Valid (Repaint);

   -- Completes or cancels the complete batch before return. Repaint means a
   -- known-quiescent failure may have modified the target: replay its scene
   -- before publication. Must_Restart prohibits ordinary reuse/retirement.
   -- Synchronous fill of an already clipped physical-output rectangle.
   -- Preserves all packed BGRA bytes; known-quiescent failure permits CPU fill.
   procedure Draw_Fill
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Area : CuBit.Display_Geometry.Physical_Rectangle; Color : Compositor_Formats.Word;
      Secondary : Boolean; Drawn, Must_Restart : out Boolean)
     with Global => (Proof_In => Desktop_Vulkan_Startup.Engine, In_Out => Engine),
       Pre => Valid and Desktop_Vulkan_Startup.Valid,
       Post => Valid and Desktop_Vulkan_Startup.Valid;
   -- Capture checker strips as bounded scene work, not one layer per pixel.
   procedure Draw_Backdrop
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Style : CuBit.Appearance.Preferences; Secondary : Boolean;
      Drawn, Must_Restart : out Boolean)
     with Global => (In_Out => (Engine, Desktop_Vulkan_Startup.Engine)),
       Pre => Valid and Desktop_Vulkan_Startup.Valid,
       Post => Valid and Desktop_Vulkan_Startup.Valid;
   procedure Draw_Preview
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Bounds : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Style : CuBit.Appearance.Preferences; Secondary : Boolean;
      Drawn, Must_Restart : out Boolean)
     with Global => (In_Out => (Engine, Desktop_Vulkan_Startup.Engine)),
       Pre => Valid and Desktop_Vulkan_Startup.Valid,
       Post => Valid and Desktop_Vulkan_Startup.Valid;
   procedure Draw_Shadow
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Window : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Color : Compositor_Formats.Word; Secondary : Boolean;
      Drawn, Must_Restart : out Boolean)
     with Global => (Proof_In => Desktop_Vulkan_Startup.Engine, In_Out => Engine),
       Pre => Valid and Desktop_Vulkan_Startup.Valid,
       Post => Valid and Desktop_Vulkan_Startup.Valid;
   procedure Draw_Text
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output; Items : Compositor_Text.Glyphs;
      Length : Compositor_Text.Count; Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Tint : Compositor_Formats.Word; Secondary : Boolean;
      Drawn, Repaint, Must_Restart : out Boolean)
     with Global => (In_Out => (Engine, Desktop_Vulkan_Startup.Engine)),
       Pre => Valid and Desktop_Vulkan_Startup.Valid,
       Post => Valid and Desktop_Vulkan_Startup.Valid;
   procedure Draw_Client
     (Target, Source : Compositor_Formats.Image;
      Target_Bytes, Source_Bytes : Compositor_Formats.Byte_Count;
      Plan : Desktop_Composition.Blit_Plan; Drag_Target : Boolean;
      Drawn, Must_Restart : out Boolean)
     with Global => (Proof_In => Desktop_Vulkan_Startup.Engine, In_Out => Engine),
       Pre => Valid and Desktop_Vulkan_Startup.Valid,
       Post => Valid and Desktop_Vulkan_Startup.Valid;
   -- Over selects premultiplied source-over for immutable assets such as
   -- cursors. Source backing must remain alive until safe source retirement.
   procedure Draw_Output
     (Target, Source : Compositor_Formats.Image;
      Target_Bytes, Source_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output;
      Surface : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Secondary : Boolean; Drawn, Must_Restart : out Boolean;
      Over : Boolean := False; Straight_Alpha : Boolean := False)
     with Global => (In_Out => (Engine, Desktop_Vulkan_Startup.Engine)),
       Pre => Valid and Desktop_Vulkan_Startup.Valid,
       Post => Valid and Desktop_Vulkan_Startup.Valid;
   type Render_Completion is (Complete, Pending, Retry, Software_Required, Unsafe);
   -- Finish the output's drawing scope (Poll=False), then observe it only
   -- (Poll=True). Pending retains all source/target/descriptor leases and
   -- prohibits further writes to this target. Complete means every renderer
   -- reader/writer retired, not display retirement. Unsafe prohibits reuse.
   -- Retry confirms renderer quiescence but no publishable frame: restore
   -- captured damage and repaint the failed writer before any later reuse.
   -- CPU/softpipe complete synchronously. A deferred backend must be Selected
   -- and retain all captured source leases until completion; per-draw fallback
   -- is forbidden after it has queued any work. Calls must not wait for GPU.
   procedure Complete_Output
     (Target : System.Address; Writer : Compositor_Pool.Ticket;
      Secondary, Poll : Boolean; Result : out Render_Completion)
     with Global => (In_Out => (Engine, Desktop_Vulkan_Startup.Engine)),
       Pre => Valid and Desktop_Vulkan_Startup.Valid,
       Post => Valid and Desktop_Vulkan_Startup.Valid;
   type Source_Release is (Source_Retired, Source_Busy, Source_Unsafe);
   -- Busy retains every source reference and may be polled without waiting.
   -- Retired permits grant return; Unsafe forbids return or slot reuse.
   procedure Forget_Source (Pixels : System.Address; Result : out Source_Release)
     with Global => (In_Out => (Engine, Desktop_Vulkan_Startup.Engine)),
       Pre => Valid and Desktop_Vulkan_Startup.Valid,
       Post => Valid and Desktop_Vulkan_Startup.Valid;
   type Target_Release is (Targets_Retired, Targets_Busy, Targets_Unsafe);
   -- Retire/cancel all accepted work and drop target imports without new work.
   -- Busy retains every target; only Retired permits Display/grant teardown.
   procedure Forget_Targets (Result : out Target_Release)
     with Global => (In_Out => (Engine, Desktop_Vulkan_Startup.Engine)),
       Pre => Valid and Desktop_Vulkan_Startup.Valid,
       Post => Valid and Desktop_Vulkan_Startup.Valid;
end Desktop_Compositor;
