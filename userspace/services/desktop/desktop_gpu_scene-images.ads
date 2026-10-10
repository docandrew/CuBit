with Compositor_Damage;
with Desktop_Image_Registry;
with Compositor_Formats;
with Compositor_Source_Content;
with Desktop_GPU_Scene.Output;
-- Whole-frame capture adapter. A cold image rejects the capture, not just a
-- draw. Caller suppresses per-draw CPU fallback and uses this completion gate
-- whenever images may have started uploads during capture. An image the
-- device cannot allocate even after evicting every idle source is drawn as
-- an opaque placeholder for this frame; the renderer stays in GPU mode.
package Desktop_GPU_Scene.Images with SPARK_Mode is
   package Registry renames Desktop_Image_Registry;
   Placeholder_Color : constant V.A.Word := 16#0030_3030#;
   procedure Capture (Scene : in out State; Sources : in out Registry.State;
      Key : Compositor_Source_Content.Source_Key;
      Version : Compositor_Source_Content.Content_Version;
      Image : Compositor_Formats.Image; Bytes : Natural;
      Surface : V.A.G.Logical_Rectangle; Damage : V.A.G.Physical_Rectangle;
      Accepted : out Boolean; Over : Boolean := False; Straight_Alpha : Boolean := False)
     with Global => (In_Out => D.Engine),
       Pre => Valid (Scene) and Registry.Valid (Sources) and D.Valid,
       Post => Valid (Scene) and Registry.Valid (Sources) and D.Valid;
   procedure Complete
     (Scene : in out State; Sources : in out Registry.State;
      Copy : in out Output.R.State; Target : Compositor_Formats.Image;
      Bytes : Compositor_Formats.Byte_Count; Writer : Output.R.P.Ticket;
      Poll_Only, Capture_Accepted : Boolean; Byte_Budget : Natural;
      Result : out Output_Completion; Repair : Compositor_Damage.State)
     with Global => (In_Out => D.Engine),
       Pre => Compositor_Damage.Valid (Repair) and Valid (Scene) and Registry.Valid (Sources) and Output.R.Valid (Copy) and D.Valid,
       Post => Valid (Scene) and Registry.Valid (Sources) and Output.R.Valid (Copy) and D.Valid;
end Desktop_GPU_Scene.Images;
