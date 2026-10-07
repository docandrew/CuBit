with Desktop_Image_Registry;
with Compositor_Formats;
with Desktop_GPU_Scene.Output;
-- Whole-frame capture adapter. A cold image rejects the capture, not just a
-- draw. Caller suppresses per-draw CPU fallback and uses this completion gate
-- whenever images may have started uploads during capture.
package Desktop_GPU_Scene.Images with SPARK_Mode => Off is
   package Registry renames Desktop_Image_Registry;
   procedure Capture (Scene : in out State; Sources : in out Registry.State;
      Image : Compositor_Formats.Image; Bytes : Natural;
      Surface : V.A.G.Logical_Rectangle; Damage : V.A.G.Physical_Rectangle;
      Accepted : out Boolean; Over : Boolean := False; Straight_Alpha : Boolean := False);
   procedure Complete
     (Scene : in out State; Sources : in out Registry.State;
      Copy : in out Output.R.State; Target : Compositor_Formats.Image;
      Bytes : Compositor_Formats.Byte_Count; Writer : Output.R.P.Ticket;
      Poll_Only, Capture_Accepted : Boolean; Byte_Budget : Natural;
      Result : out Output_Completion);
end Desktop_GPU_Scene.Images;
