with Desktop_CPU_Software_Renderer;
with Mesa_Binding;
with Compositor_Policy;
with Compositor_Affine;
with Mesa_Binding.Affine;
with Mesa_Cache;
with Compositor_Glyph_Target;
with Compositor_Glyph_Renderer;
with Compositor_Damage;
with Vulkan_Device_Owner;
with Compositor_Software_Text;
with Compositor_Software_Text_Target;
with Desktop_GPU_Scene;
with Desktop_GPU_Scene.Backdrop;
with Desktop_Backdrop_Owner;
with Desktop_Backdrop_Style;
with Vulkan_Submission;
with Desktop_GPU_Scene.Drawing;
with Desktop_GPU_Scene.Images;
with Desktop_Readback_Output;
with Desktop_Image_Registry;
with Desktop_Icon_Atlas_Owner;
with Desktop_Icon_Pixels.Atlases;
with Desktop_Vulkan_Startup;
with Compositor_Backend_Selection;
with Compositor_Source_Region;
package body Desktop_Compositor with SPARK_Mode,
  Refined_State => (Engine => (Choice, Recovery_Targets_Retired, CPU.State, GPU.State)) is
package Selection renames Compositor_Backend_Selection;
use type Selection.Mode, Selection.Recovery_Phase;
Choice : Selection.State;
Recovery_Targets_Retired : Boolean := False;
function Use_GPU return Boolean is (Selection.Current (Choice) = Selection.GPU);
-- Non-GPU selection: CPU drawing, not softpipe (about 5x cheaper per pixel).
package CPU is new Desktop_CPU_Software_Renderer
  (Recovery_Result, Recovery_Unsafe,
     Output_Start, Started, Start_Unsafe,
     Render_Completion, Complete, Unsafe,
     Source_Release, Source_Retired, Source_Unsafe,
     Target_Release, Targets_Retired, Targets_Unsafe);

package GPU with Abstract_State => State, Initializes => State,
  Initial_Condition => Valid is
function Valid return Boolean with Ghost, Global => (Input => State);
function Readback_Work return Transfer_Counters with Global => (Input => State);
function Selected return Boolean;
function Full_Output return Boolean;
function Software_Text return Boolean;
procedure Begin_Output
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Writer : Compositor_Pool.Ticket; Screen : CuBit.Display_Geometry.Output;
      Secondary : Boolean; Result : out Output_Start;
      Repaint : in out Compositor_Damage.State;
      Writer_Repair : Compositor_Damage.State)
  with Pre => Valid and Desktop_Vulkan_Startup.Valid and Compositor_Damage.Valid (Repaint) and Compositor_Damage.Valid (Writer_Repair),
    Post => Valid and Desktop_Vulkan_Startup.Valid and Compositor_Damage.Valid (Repaint);
procedure Draw_Fill
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Area : CuBit.Display_Geometry.Physical_Rectangle; Color : Compositor_Formats.Word;
      Secondary : Boolean; Drawn, Must_Restart : out Boolean)
  with Pre => Valid and Desktop_Vulkan_Startup.Valid,
    Post => Valid and Desktop_Vulkan_Startup.Valid;
procedure Draw_Backdrop
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Style : CuBit.Appearance.Preferences; Secondary : Boolean;
      Drawn, Must_Restart : out Boolean)
  with Pre => Valid and Desktop_Vulkan_Startup.Valid,
    Post => Valid and Desktop_Vulkan_Startup.Valid;
procedure Draw_Shadow
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Window : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Color : Compositor_Formats.Word; Secondary : Boolean;
      Drawn, Must_Restart : out Boolean)
  with Pre => Valid and Desktop_Vulkan_Startup.Valid,
    Post => Valid and Desktop_Vulkan_Startup.Valid;
procedure Draw_Text
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output; Items : Compositor_Text.Glyphs;
      Length : Compositor_Text.Count; Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Tint : Compositor_Formats.Word; Secondary : Boolean;
      Drawn, Repaint, Must_Restart : out Boolean)
  with Pre => Valid and Desktop_Vulkan_Startup.Valid,
    Post => Valid and Desktop_Vulkan_Startup.Valid;
procedure Draw_Client
     (Target, Source : Compositor_Formats.Image;
      Target_Bytes, Source_Bytes : Compositor_Formats.Byte_Count;
      Plan : Desktop_Composition.Blit_Plan; Drag_Target : Boolean;
      Drawn, Must_Restart : out Boolean)
  with Pre => Valid and Desktop_Vulkan_Startup.Valid,
    Post => Valid and Desktop_Vulkan_Startup.Valid;
procedure Draw_Output
     (Target, Source : Compositor_Formats.Image;
      Target_Bytes, Source_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output;
      Surface : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Key : Compositor_Source_Content.Source_Key;
      Version : Compositor_Source_Content.Content_Version;
      Secondary : Boolean; Drawn, Must_Restart : out Boolean)
  with Pre => Valid and Desktop_Vulkan_Startup.Valid,
    Post => Valid and Desktop_Vulkan_Startup.Valid;
procedure Note_Source_Change
     (Key : Compositor_Source_Content.Source_Key; Rows : Compositor_Source_Content.Row_Band)
  with Pre => Valid, Post => Valid;
procedure Retire_Source (Key : Compositor_Source_Content.Source_Key)
  with Pre => Valid, Post => Valid;
function Resident_Sources return Natural;
function Last_Retry_Cause return Retry_Cause;
function Peak_Scene_Layers return Natural;
function Placeholder_Draws return Natural;
procedure Draw_Atlas
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output; Family : Desktop_Icon_Pixels.Family;
      Region : Compositor_Source_Region.Rectangle;
      Surface : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Straight_Alpha, Secondary : Boolean; Drawn, Must_Restart : out Boolean)
  with Pre => Valid and Desktop_Vulkan_Startup.Valid,
    Post => Valid and Desktop_Vulkan_Startup.Valid;
procedure Complete_Output
     (Target : System.Address; Writer : Compositor_Pool.Ticket;
      Secondary, Poll : Boolean; Result : out Render_Completion)
  with Pre => Valid and Desktop_Vulkan_Startup.Valid,
    Post => Valid and Desktop_Vulkan_Startup.Valid;
procedure Forget_Source (Pixels : System.Address; Result : out Source_Release)
  with Global => (In_Out => State, Proof_In => Desktop_Vulkan_Startup.Engine),
    Pre => Valid and Desktop_Vulkan_Startup.Valid,
    Post => Valid;
procedure Forget_Targets (Result : out Target_Release)
  with Pre => Valid and Desktop_Vulkan_Startup.Valid,
    Post => Valid and Desktop_Vulkan_Startup.Valid;
   procedure Draw_Preview
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Bounds : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Style : CuBit.Appearance.Preferences; Secondary : Boolean;
      Drawn, Must_Restart : out Boolean)
  with Pre => Valid and Desktop_Vulkan_Startup.Valid,
    Post => Valid and Desktop_Vulkan_Startup.Valid;
end GPU;

package body GPU with Refined_State => (State => (Scene, Sources, Backdrops, Atlases, Copy, Opened, Captured, Destination, Capacity, Held_Writer, Geometry, Copy_Repair, Asset_Cold, Retried, Cells_Prepared)) is
   use type Interfaces.Unsigned_64;

   package G renames Desktop_GPU_Scene;
   package D renames Desktop_Vulkan_Startup;
   package R renames Desktop_Image_Registry;
   use type Desktop_Readback_Output.Phase, D.Presentation_Ticket;
   use type G.Output_Completion, D.Capture_Admission, Compositor_Formats.Image,
     Compositor_Formats.Byte_Count, Compositor_Formats.Word, Compositor_Pool.ID,
     Compositor_Pool.Ticket, System.Address;
   Scene : G.State;
   Sources : R.State;
   package B renames Desktop_Backdrop_Owner;
   use type B.Phase, B.Outcome, G.Phase, CuBit.Appearance.Background;
   Backdrops : array (B.Slot) of B.State;
   package IA renames Desktop_Icon_Atlas_Owner;
   use type IA.Phase, IA.Outcome;
   Atlases : array (Desktop_Icon_Pixels.Family) of IA.State;
   function Atlas_Slot (Family : Desktop_Icon_Pixels.Family) return IA.Slot is
     (IA.Slot'Val (IA.Slot'Pos (IA.Slot'First) + Desktop_Icon_Pixels.Family'Pos (Family)));
   -- An atlas or backdrop draw in this capture found its image cold.
   Asset_Cold : Boolean := False;
   Retried : Retry_Cause := No_Retry;
   Cells_Prepared : Boolean := False;
   Copy : Desktop_Readback_Output.State;
   Opened, Captured : Boolean := False;
   Destination : Compositor_Formats.Image;
   Capacity : Compositor_Formats.Byte_Count := 0;
   Held_Writer : Compositor_Pool.Ticket := Compositor_Pool.None;
   Copy_Repair : Compositor_Damage.State;
   -- Inert geometry until Begin_Output accepts an actual target. Keep every
   -- state constituent initialized even while Opened is False.
   Geometry : CuBit.Display_Geometry.Output :=
     (Width => 1, Height => 1, others => <>);
   function Valid return Boolean is
     (G.Valid (Scene) and R.Valid (Sources) and
      Desktop_Readback_Output.Valid (Copy) and Compositor_Damage.Valid (Copy_Repair) and
      (for all Index in B.Slot => B.Valid (Backdrops (Index))) and
      (for all Family in Desktop_Icon_Pixels.Family => IA.Valid (Atlases (Family))));
   function Readback_Work return Transfer_Counters is
     (GPU_Submitted => Desktop_Readback_Output.GPU_Bytes (Copy),
      CPU_Copied => Desktop_Readback_Output.CPU_Bytes (Copy),
      Saturated => Desktop_Readback_Output.Counters_Saturated (Copy));
   function Selected return Boolean is (True);
   function Full_Output return Boolean is (True);
   function Software_Text return Boolean is (False);
   function Matches (Target : Compositor_Formats.Image; Bytes : Compositor_Formats.Byte_Count;
      Secondary : Boolean) return Boolean is
     (Opened and not Secondary and Target = Destination and Bytes = Capacity);
   procedure Begin_Output
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Writer : Compositor_Pool.Ticket; Screen : CuBit.Display_Geometry.Output;
      Secondary : Boolean; Result : out Output_Start;
      Repaint : in out Compositor_Damage.State;
      Writer_Repair : Compositor_Damage.State) is
      OK : Boolean;
   begin
      Result := Start_Unsafe;
      -- Asset uploads are row-chunked. Reject an impossible configuration
      -- before opening a scene or acquiring backing; it cannot make progress
      -- merely by retrying. Startup may provision more staging before retry.
      if D.Upload_Capacity < 4 * Natural'Max
        (Desktop_Backdrop_Style.Wallpaper_Width, Desktop_Backdrop_Style.Cubie_Width)
      then return; end if;
      if Opened or else Secondary or else Writer = Compositor_Pool.None or else
         Writer.Buffer = 0 or else Writer.Epoch = 0 or else Writer.Serial = 0 or else
         Target.Writable /= 1 or else
         not Compositor_Formats.Valid (Target, Target_Bytes) or else
         not D.Readback_Layout_Matches (Natural (Target.Width), Natural (Target.Height)) then return; end if;
      if Compositor_Damage.Count (Writer_Repair) > 0 and then
        (Compositor_Damage.Bounds (Writer_Repair).Right > Natural (Target.Width) or else
         Compositor_Damage.Bounds (Writer_Repair).Bottom > Natural (Target.Height)) then return; end if;
      if D.Admit_Capture (Screen) = D.Capture_Busy then Result := Deferred; return; end if;
      if not Compositor_Damage.Valid (Repaint) then return; end if;
      -- Only new scene changes dirty every GPU target. A target's historical
      -- repair is returned for this capture, never fed back as new damage.
      for I in 1 .. Compositor_Damage.Count (Repaint) loop
         D.Damage_Output (Compositor_Damage.Item (Repaint, I), OK);
         if not OK then return; end if;
         pragma Loop_Invariant (D.Valid);
      end loop;
      if not Cells_Prepared then
         -- Before the first capture, while the renderer is idle: allocate
         -- the glyph cells together rather than during interaction.
         declare Prepared : Natural; begin
            G.Prepare_Glyph_Cells (Scene, Screen.Scale, Prepared);
         end;
         Cells_Prepared := True;
      end if;
      G.Begin_Frame (Scene, Screen, 0, OK);
      if not OK then return; end if;
      G.Capture_Repaint (Scene, Repaint, OK);
      if not OK then return; end if;
      Copy_Repair := Writer_Repair;
      if Compositor_Damage.Count (Copy_Repair) = 0 then
         Compositor_Damage.Add (Copy_Repair, (0, 0, Natural (Target.Width), Natural (Target.Height)));
      end if;
      Destination := Target; Capacity := Target_Bytes; Held_Writer := Writer;
      Geometry := Screen; Opened := True; Captured := True; Asset_Cold := False; Result := Started;
   end Begin_Output;
   procedure Draw_Fill
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Area : CuBit.Display_Geometry.Physical_Rectangle; Color : Compositor_Formats.Word;
      Secondary : Boolean; Drawn, Must_Restart : out Boolean) is
      OK : Boolean;
   begin
      Must_Restart := not Matches (Target, Target_Bytes, Secondary); Drawn := True;
      if Must_Restart or not Captured then return; end if;
      G.Drawing.Fill (Scene, Area, Color, OK); Captured := OK;
   end Draw_Fill;
   procedure Draw_Backdrop
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Style : CuBit.Appearance.Preferences; Secondary : Boolean;
      Drawn, Must_Restart : out Boolean) is
      Source : Vulkan_Submission.Source_Ticket := Vulkan_Submission.No_Source;
      Status : B.Outcome;
      Index : constant B.Slot :=
        (if Style.Backdrop = CuBit.Appearance.Cubie then B.Slot'Last else B.Slot'First);
      OK : Boolean;
      use type CuBit.Appearance.Background;
   begin
      Must_Restart := not Matches (Target, Target_Bytes, Secondary);
      Drawn := True;
      if Must_Restart or not Captured then return; end if;
      if Desktop_Backdrop_Style.Has_Image (Style.Backdrop) then
         B.Acquire (Backdrops (Index), Index, Style.Backdrop, Source, Status);
         if Status /= B.Available then
            Captured := False; Asset_Cold := True;
            Must_Restart := Status = B.Unsafe;
            return;
         end if;
      end if;
      G.Backdrop.Capture (Scene, Style, Source, Damage, OK);
      Captured := OK;
   end Draw_Backdrop;
   procedure Draw_Shadow
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Window : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Color : Compositor_Formats.Word; Secondary : Boolean;
      Drawn, Must_Restart : out Boolean) is
      OK : Boolean;
   begin
      Must_Restart := not Matches (Target, Target_Bytes, Secondary);
      Drawn := True;
      if Must_Restart or not Captured then return; end if;
      G.Drawing.Shadow (Scene, Window, Damage, Color, OK);
      Captured := OK;
   end Draw_Shadow;
   procedure Draw_Text
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output; Items : Compositor_Text.Glyphs;
      Length : Compositor_Text.Count; Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Tint : Compositor_Formats.Word; Secondary : Boolean;
      Drawn, Repaint, Must_Restart : out Boolean) is
      OK : Boolean;
      use type CuBit.Display_Geometry.Output;
   begin
      Must_Restart := not Matches (Target, Target_Bytes, Secondary) or Screen /= Geometry;
      Drawn := True; Repaint := False;
      if Must_Restart or not Captured then return; end if;
      G.Drawing.Text (Scene, Items, Length, Damage, Tint, OK); Captured := OK;
   end Draw_Text;
   procedure Draw_Client
     (Target, Source : Compositor_Formats.Image;
      Target_Bytes, Source_Bytes : Compositor_Formats.Byte_Count;
      Plan : Desktop_Composition.Blit_Plan; Drag_Target : Boolean;
      Drawn, Must_Restart : out Boolean) is
   begin
      -- Legacy offscreen drag composition is outside native-output capture.
      -- Never silently mix it into a deferred frame.
      Drawn := True; Must_Restart := True;
   end Draw_Client;
   procedure Draw_Output
     (Target, Source : Compositor_Formats.Image;
      Target_Bytes, Source_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output;
      Surface : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Key : Compositor_Source_Content.Source_Key;
      Version : Compositor_Source_Content.Content_Version;
      Secondary : Boolean; Drawn, Must_Restart : out Boolean) is
      OK : Boolean;
      use type CuBit.Display_Geometry.Output;
   begin
      Must_Restart := not Matches (Target, Target_Bytes, Secondary) or Screen /= Geometry;
      Drawn := True;
      if Must_Restart or not Captured then return; end if;
      if Source_Bytes > Compositor_Formats.Byte_Count (Natural'Last) then Captured := False; return; end if;
      G.Images.Capture (Scene, Sources, Key, Version, Source, Natural (Source_Bytes), Surface, Damage, OK);
      Captured := OK;
   end Draw_Output;
   procedure Note_Source_Change
     (Key : Compositor_Source_Content.Source_Key; Rows : Compositor_Source_Content.Row_Band) is
   begin
      R.Note_Change (Sources, Key, Rows);
   end Note_Source_Change;
   procedure Retire_Source (Key : Compositor_Source_Content.Source_Key) is
   begin
      R.Retire (Sources, Key);
   end Retire_Source;
   function Resident_Sources return Natural is (R.Resident_Keys (Sources));
   procedure Draw_Atlas
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output; Family : Desktop_Icon_Pixels.Family;
      Region : Compositor_Source_Region.Rectangle;
      Surface : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Straight_Alpha, Secondary : Boolean; Drawn, Must_Restart : out Boolean) is
      Source : Vulkan_Submission.Source_Ticket;
      Status : IA.Outcome;
      OK : Boolean;
      use type CuBit.Display_Geometry.Output;
   begin
      Must_Restart := not Matches (Target, Target_Bytes, Secondary) or Screen /= Geometry;
      Drawn := True;
      if Must_Restart or not Captured then return; end if;
      IA.Acquire (Atlases (Family), Atlas_Slot (Family), Family, Source, Status);
      if Status /= IA.Available then
         -- Cold atlas: discard this capture; Complete_Output drives the upload.
         Captured := False; Asset_Cold := True; Must_Restart := Status = IA.Unsafe; return;
      end if;
      G.Drawing.Image_Region (Scene, Source, Surface, Damage, Region, OK,
        Over => True, Straight_Alpha => Straight_Alpha);
      Captured := OK;
   end Draw_Atlas;
   procedure Complete_Output
     (Target : System.Address; Writer : Compositor_Pool.Ticket;
      Secondary, Poll : Boolean; Result : out Render_Completion) is
      Finished : G.Output_Completion;
      Progress : B.Outcome;
      Atlas_Progress : IA.Outcome;
   begin
      Result := Unsafe;
      if not Opened or Secondary or Target /= Destination.Pixels or Writer /= Held_Writer then return; end if;
      for Index in Backdrops'Range loop
         if B.Current (Backdrops (Index)) = B.Quarantined then return; end if;
         if B.Current (Backdrops (Index)) in B.Uploading | B.Ready_To_Upload then
            if Poll then
               B.Poll (Backdrops (Index), Progress);
               if Progress in B.Unsafe | B.Rejected then return; end if;
            end if;
            Result := Pending; return;
         end if;
      end loop;
      for Family in Atlases'Range loop
         if IA.Current (Atlases (Family)) = IA.Quarantined then return; end if;
         if IA.Current (Atlases (Family)) in IA.Uploading | IA.Ready_To_Upload then
            if Poll then
               IA.Poll (Atlases (Family), Atlas_Progress);
               if Atlas_Progress in IA.Unsafe | IA.Rejected then return; end if;
            end if;
            Result := Pending; return;
         end if;
      end loop;
      G.Images.Complete (Scene, Sources, Copy, Destination, Capacity, Writer,
        Poll and not (not Captured and G.Current (Scene) = G.Capturing
                      and not R.Upload_Work (Sources)),
        Captured, 256 * 1024, Finished, Copy_Repair);
      Result := (case Finished is when G.Output_Complete => Complete,
        when G.Output_Repaint => Retry, when G.Output_Pending => Pending, when G.Output_Unsafe => Unsafe);
      if Finished in G.Output_Complete | G.Output_Repaint then
         Opened := False; Held_Writer := Compositor_Pool.None;
         -- No capture is open: free images of surfaces that have gone.
         R.Collect (Sources);
      end if;
      if Finished = G.Output_Repaint then
         Retried :=
           (case G.Last_Failure (Scene) is
              when G.Cold_Source => Cold_Upload,
              when G.Layer_Limit => Layer_Limit,
              when G.Glyph_Limit => Glyph_Limit,
              when G.Image_Limit => Image_Limit,
              when G.Rejected_Draw => Rejected_Draw,
              when G.No_Failure => (if Asset_Cold then Cold_Upload else Readback_Failed));
      elsif Finished = G.Output_Complete then
         Retried := No_Retry;
      end if;
   end Complete_Output;
   function Last_Retry_Cause return Retry_Cause is (Retried);
   function Peak_Scene_Layers return Natural is (Natural (G.Peak_Layers (Scene)));
   function Placeholder_Draws return Natural is (G.Placeholder_Draws (Scene));
   procedure Forget_Source (Pixels : System.Address; Result : out Source_Release) is
      Safe : Boolean;
   begin
      -- Never touches GPU state: the mapping is only read into staging.
      Result := Source_Busy;
      R.Forget (Sources, Pixels, Safe);
      if Safe then Result := Source_Retired;
      elsif R.Faulted (Sources) then Result := Source_Unsafe; end if;
   end Forget_Source;
   procedure Forget_Targets (Result : out Target_Release) is
      Safe : Boolean;
      Completion : Render_Completion;
   begin
      Result := Targets_Busy;
      if Opened then
         -- Desktop suspends normal presentation pumping during teardown.
         -- Observe existing work using its retained writer; never recapture
         -- or submit a new scene. The caller retains output mappings while
         -- Busy, and does not publish this final frame to Display.
         declare
            Retained_Writer : constant Compositor_Pool.Ticket := Held_Writer;
            Retained_Target : constant System.Address := Destination.Pixels;
         begin
            Complete_Output (Retained_Target, Retained_Writer, False, True, Completion);
         end;
         if Completion = Unsafe then Result := Targets_Unsafe; return; end if;
         if Opened then return; end if;
      end if;
      if Desktop_Readback_Output.Current (Copy) /= Desktop_Readback_Output.Idle or else
         D.Readback_Pending /= D.No_Presentation then
         Result := Targets_Unsafe; return;
      end if;
      R.Close (Sources, True, Safe);
      if not Safe then
         if R.Faulted (Sources) then Result := Targets_Unsafe; end if;
         return;
      end if;
      for Index in Backdrops'Range loop
         pragma Loop_Invariant (Valid and D.Valid);
         B.Close (Backdrops (Index), True, Safe);
         if not Safe then
            if B.Current (Backdrops (Index)) = B.Quarantined then Result := Targets_Unsafe; end if;
            return;
         end if;
      end loop;
      for Family in Atlases'Range loop
         pragma Loop_Invariant (Valid and D.Valid);
         IA.Close (Atlases (Family), True, Safe);
         if not Safe then
            if IA.Current (Atlases (Family)) = IA.Quarantined then Result := Targets_Unsafe; end if;
            return;
         end if;
      end loop;
      G.Close (Scene, Safe);
      Result := (if Safe then Targets_Retired else Targets_Unsafe);
   end Forget_Targets;

   procedure Draw_Preview
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Bounds : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Style : CuBit.Appearance.Preferences; Secondary : Boolean;
      Drawn, Must_Restart : out Boolean) is
      Source : Vulkan_Submission.Source_Ticket := Vulkan_Submission.No_Source;
      Status : B.Outcome;
      Index : constant B.Slot :=
        (if Style.Backdrop = CuBit.Appearance.Cubie then B.Slot'Last else B.Slot'First);
      OK : Boolean;
   begin
      Must_Restart := not Matches (Target, Target_Bytes, Secondary);
      Drawn := True;
      if Must_Restart or not Captured then return; end if;
      if Desktop_Backdrop_Style.Has_Image (Style.Backdrop) then
         B.Acquire (Backdrops (Index), Index, Style.Backdrop, Source, Status);
         if Status /= B.Available then
            Captured := False; Asset_Cold := True; Must_Restart := Status = B.Unsafe; return;
         end if;
      end if;
      G.Set_Clip (Scene, Damage, OK);
      if OK then G.Backdrop.Capture_Preview (Scene, Style, Source, Bounds, OK); end if;
      if OK then G.Set_Clip (Scene, (0, 0, Geometry.Width, Geometry.Height), OK); end if;
      Captured := OK;
   end Draw_Preview;
end GPU;
function Readback_Work return Transfer_Counters is (GPU.Readback_Work);

function Valid return Boolean is (GPU.Valid);
procedure Recover_Renderer (Key : Selection.Recovery_Key;
 Writer_Retired, Repaint_Queued : Boolean; Result : out Recovery_Result) is
 use type Selection.Recovery_Key, Vulkan_Device_Owner.Phase;
 E : Selection.Drain_Evidence;
 Accepted, Switched : Boolean;
 Targets : Target_Release;
begin
 Result := Recovery_Unsafe;
 if Selection.Recovery (Choice) = Selection.Idle then
   Selection.Request_Recovery (Choice, Key, Accepted);
   if not Accepted then return; end if;
 end if;
 if Selection.Key_Of (Choice) /= Key then return; end if;
 if Selection.Recovery (Choice) = Selection.Recovered then Result := Recovery_Complete; return; end if;
 if Selection.Recovery (Choice) /= Selection.Draining then return; end if;
 Result := Recovery_Pending;
 -- Once children are closed, never invoke scene/source operations again:
 -- Stop may already have moved the native device into retirement.
 if not Recovery_Targets_Retired then
   GPU.Forget_Targets (Targets);
   if Targets = Targets_Unsafe then E.Uncertain := True;
   elsif Targets = Targets_Retired then Recovery_Targets_Retired := True;
   end if;
 end if;
 if Recovery_Targets_Retired then
   Desktop_Vulkan_Startup.Stop;
   if Desktop_Vulkan_Startup.Current = Vulkan_Device_Owner.Quarantined then E.Uncertain := True;
   elsif Desktop_Vulkan_Startup.Current = Vulkan_Device_Owner.Retired then
     E.Renderer := True; E.Sources := True; E.Readback := True;
   end if;
 end if;
 E.Output_Writer := Writer_Retired; E.Full_Repaint := Repaint_Queued;
 Selection.Observe_Recovery (Choice, Key, E, Switched);
 if Switched then Result := Recovery_Complete;
 elsif Selection.Recovery (Choice) = Selection.Quarantined then Result := Recovery_Unsafe;
 end if;
end Recover_Renderer;
procedure Configure_Renderer (Evidence : Compositor_Backend_Selection.Readiness; Accepted : out Boolean) is
begin
 Selection.Select_Backend (Choice, Evidence, Accepted);
end Configure_Renderer;
function Selected return Boolean is
begin
if Use_GPU then return GPU.Selected; else return CPU.Selected; end if;
end Selected;
function Full_Output return Boolean is
begin
if Use_GPU then return GPU.Full_Output; else return CPU.Full_Output; end if;
end Full_Output;
function Software_Text return Boolean is
begin
if Use_GPU then return GPU.Software_Text; else return CPU.Software_Text; end if;
end Software_Text;
procedure Begin_Output
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Writer : Compositor_Pool.Ticket; Screen : CuBit.Display_Geometry.Output;
      Secondary : Boolean; Result : out Output_Start;
      Repaint : in out Compositor_Damage.State;
      Writer_Repair : Compositor_Damage.State) is
begin
Selection.Begin_Output (Choice);
if not Selection.Can_Capture (Choice) then
 Result := (if Selection.Recovery (Choice) = Selection.Quarantined then Start_Unsafe else Deferred);
 return;
end if;
if Use_GPU then GPU.Begin_Output (Target, Target_Bytes, Writer, Screen, Secondary, Result, Repaint, Writer_Repair); else CPU.Begin_Output (Target, Target_Bytes, Writer, Screen, Secondary, Result, Repaint, Writer_Repair); end if;
end Begin_Output;
procedure Draw_Fill
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Area : CuBit.Display_Geometry.Physical_Rectangle; Color : Compositor_Formats.Word;
      Secondary : Boolean; Drawn, Must_Restart : out Boolean) is
begin
if Use_GPU then GPU.Draw_Fill (Target, Target_Bytes, Area, Color, Secondary, Drawn, Must_Restart); else CPU.Draw_Fill (Target, Target_Bytes, Area, Color, Secondary, Drawn, Must_Restart); end if;
end Draw_Fill;
procedure Draw_Backdrop
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Style : CuBit.Appearance.Preferences; Secondary : Boolean;
      Drawn, Must_Restart : out Boolean) is
begin
if Use_GPU then GPU.Draw_Backdrop (Target, Target_Bytes, Damage, Style, Secondary, Drawn, Must_Restart); else CPU.Draw_Backdrop (Target, Target_Bytes, Damage, Style, Secondary, Drawn, Must_Restart); end if;
end Draw_Backdrop;
procedure Draw_Shadow
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Window : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Color : Compositor_Formats.Word; Secondary : Boolean;
      Drawn, Must_Restart : out Boolean) is
begin
if Use_GPU then GPU.Draw_Shadow (Target, Target_Bytes, Window, Damage, Color, Secondary, Drawn, Must_Restart); else CPU.Draw_Shadow (Target, Target_Bytes, Window, Damage, Color, Secondary, Drawn, Must_Restart); end if;
end Draw_Shadow;
procedure Draw_Text
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output; Items : Compositor_Text.Glyphs;
      Length : Compositor_Text.Count; Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Tint : Compositor_Formats.Word; Secondary : Boolean;
      Drawn, Repaint, Must_Restart : out Boolean) is
begin
if Use_GPU then GPU.Draw_Text (Target, Target_Bytes, Screen, Items, Length, Damage, Tint, Secondary, Drawn, Repaint, Must_Restart); else CPU.Draw_Text (Target, Target_Bytes, Screen, Items, Length, Damage, Tint, Secondary, Drawn, Repaint, Must_Restart); end if;
end Draw_Text;
procedure Draw_Client
     (Target, Source : Compositor_Formats.Image;
      Target_Bytes, Source_Bytes : Compositor_Formats.Byte_Count;
      Plan : Desktop_Composition.Blit_Plan; Drag_Target : Boolean;
      Drawn, Must_Restart : out Boolean) is
begin
if Use_GPU then GPU.Draw_Client (Target, Source, Target_Bytes, Source_Bytes, Plan, Drag_Target, Drawn, Must_Restart); else CPU.Draw_Client (Target, Source, Target_Bytes, Source_Bytes, Plan, Drag_Target, Drawn, Must_Restart); end if;
end Draw_Client;
procedure Draw_Output
     (Target, Source : Compositor_Formats.Image;
      Target_Bytes, Source_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output;
      Surface : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Key : Compositor_Source_Content.Source_Key;
      Version : Compositor_Source_Content.Content_Version;
      Secondary : Boolean; Drawn, Must_Restart : out Boolean) is
begin
if Use_GPU then GPU.Draw_Output (Target, Source, Target_Bytes, Source_Bytes, Screen, Surface, Damage, Key, Version, Secondary, Drawn, Must_Restart); else CPU.Draw_Output (Target, Source, Target_Bytes, Source_Bytes, Screen, Surface, Damage, Secondary, Drawn, Must_Restart); end if;
end Draw_Output;
-- Content bookkeeping is kept even while the CPU renderer is selected; it
-- holds no GPU resources and costs one band union per publication.
procedure Note_Source_Change
     (Key : Compositor_Source_Content.Source_Key; Rows : Compositor_Source_Content.Row_Band) is
begin
GPU.Note_Source_Change (Key, Rows);
end Note_Source_Change;
procedure Retire_Source (Key : Compositor_Source_Content.Source_Key) is
begin
GPU.Retire_Source (Key);
end Retire_Source;
procedure Draw_Icon
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output; Item : Desktop_Icon_Pixels.Asset;
      Surface : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Secondary : Boolean; Drawn, Must_Restart : out Boolean) is
begin
if Use_GPU then
   GPU.Draw_Atlas (Target, Target_Bytes, Screen, Item.Kind, Desktop_Icon_Pixels.Atlases.Region (Item),
     Surface, Damage, True, Secondary, Drawn, Must_Restart);
else Drawn := False; Must_Restart := False; end if;
end Draw_Icon;
procedure Draw_Cursor
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output; Cursor : Desktop_Cursors.Cursor_ID;
      Surface : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Secondary : Boolean; Drawn, Must_Restart : out Boolean) is
begin
if Use_GPU then
   GPU.Draw_Atlas (Target, Target_Bytes, Screen, Desktop_Icon_Pixels.Window_Control,
     Desktop_Icon_Pixels.Atlases.Cursor_Region (Cursor), Surface, Damage, False, Secondary,
     Drawn, Must_Restart);
else Drawn := False; Must_Restart := False; end if;
end Draw_Cursor;
function Backing_Events (Class : Vulkan_Submission.Source_Class; Freed : Boolean)
  return Interfaces.Unsigned_64 is
  (if Freed then Desktop_Vulkan_Startup.Released_In (Class)
   else Desktop_Vulkan_Startup.Allocated_In (Class));
function Resident_Sources return Natural is (GPU.Resident_Sources);
function Upload_Progress return Interfaces.Unsigned_64 is (Desktop_Vulkan_Startup.Transfers_Submitted);
function Peak_Scene_Layers return Natural is (GPU.Peak_Scene_Layers);
function Placeholder_Draws return Natural is (GPU.Placeholder_Draws);
function Last_Retry_Cause return Retry_Cause is (GPU.Last_Retry_Cause);
procedure Complete_Output
     (Target : System.Address; Writer : Compositor_Pool.Ticket;
      Secondary, Poll : Boolean; Result : out Render_Completion) is
begin
if Use_GPU then GPU.Complete_Output (Target, Writer, Secondary, Poll, Result); else CPU.Complete_Output (Target, Writer, Secondary, Poll, Result); end if;
end Complete_Output;
procedure Forget_Source (Pixels : System.Address; Result : out Source_Release) is
begin
if Use_GPU then GPU.Forget_Source (Pixels, Result); else CPU.Forget_Source (Pixels, Result); end if;
end Forget_Source;
procedure Forget_Targets (Result : out Target_Release) is
begin
if Use_GPU then GPU.Forget_Targets (Result); else CPU.Forget_Targets (Result); end if;
end Forget_Targets;
   procedure Draw_Preview
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Bounds : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Style : CuBit.Appearance.Preferences; Secondary : Boolean;
      Drawn, Must_Restart : out Boolean) is
begin
 if Use_GPU then GPU.Draw_Preview (Target, Target_Bytes, Bounds, Damage, Style, Secondary, Drawn, Must_Restart);
 else CPU.Draw_Preview (Target, Target_Bytes, Bounds, Damage, Style, Secondary, Drawn, Must_Restart); end if;
end Draw_Preview;
end Desktop_Compositor;
