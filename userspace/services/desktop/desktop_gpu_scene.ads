with Compositor_Damage;
with Compositor_Pool;
with Vulkan_Submission;
with Desktop_Glyph_Residency;
with Desktop_Vulkan_Startup;
with Vulkan_Glyph_Sources;
with Vulkan_Scene;
-- One owner per Desktop device lifetime. Captured metadata never escapes by
-- copy. This owner retains one reader per distinct glyph across the complete
-- scene, then retires its CPU snapshot and confirmed GPU readers together.
-- Non-glyph sources retain explicit renderer reader tokens during CPU capture
-- and GPU execution. Providers retain immutable backing until Release_Source
-- succeeds. This adds no client-buffer import or foreign authority.
package Desktop_GPU_Scene with SPARK_Mode is
   package D renames Desktop_Vulkan_Startup;
   package R renames Desktop_Glyph_Residency;
   package V renames Vulkan_Scene;
   use type R.C.Lease, V.Phase, D.Source_Reader;
   type Phase is (Idle, Capturing, Uploading, Submitted, Quarantined, Closed);
   type Outcome is (Complete, Retry, Pending, Rejected, Unsafe);
   type State is limited private with Default_Initial_Condition => Valid (State);
   function Valid (S : State) return Boolean;
   function Current (S : State) return Phase;
   function Reader_Count (S : State) return Natural;
   function Image_Reader_Count (S : State) return Natural;
   function Layer_Count (S : State) return V.Length;
   -- Why the last capture was discarded without a frame (evidence only).
   type Capture_Failure is
     (No_Failure, Cold_Source, Layer_Limit, Glyph_Limit, Image_Limit, Rejected_Draw);
   function Last_Failure (S : State) return Capture_Failure;
   -- Most layers any capture of this owner has held.
   function Peak_Layers (S : State) return V.Length;
   -- Surfaces drawn as a placeholder because the device refused their image
   -- even after every idle source was evicted (saturating count).
   function Placeholder_Draws (S : State) return Natural;
   procedure Note_Placeholder (S : in out State)
     with Global => null, Pre => Valid (S),
       Post => Valid (S) and Current (S) = Current (S)'Old and
         Layer_Count (S) = Layer_Count (S)'Old;
   procedure Capture_Repaint (S : State; Plan : out Compositor_Damage.State;
      Accepted : out Boolean)
     with Global => (Input => D.Engine), Pre => Valid (S) and D.Valid,
       Post => Compositor_Damage.Valid (Plan) and
         (if not Accepted then Compositor_Damage.Count (Plan) = 0);
   -- See Desktop_Glyph_Residency.Prepare_Cells. Only while Idle.
   procedure Prepare_Glyph_Cells (S : in out State; Scale : V.A.G.UI_Scale; Prepared : out Natural)
     with Global => (In_Out => D.Engine), Pre => Valid (S) and D.Valid,
       Post => Valid (S) and D.Valid;
   procedure Begin_Frame (S : in out State; Screen : V.A.G.Output;
      Background : V.A.Word; Accepted : out Boolean)
     with Global => (In_Out => D.Engine), Pre => Valid (S) and D.Valid,
       Post => Valid (S) and D.Valid and (if Accepted then Current (S) = Capturing);
   -- Solid, clip, texture and backdrop layers reuse the production scene
   -- representation. Glyphs must use Add_Glyph so their readers are retained.
   procedure Append (S : in out State; Item : V.Layer; Accepted : out Boolean)
     with Global => (In_Out => D.Engine), Pre => Valid (S) and D.Valid, Post => Valid (S) and D.Valid;
   procedure Set_Clip (S : in out State; Area : V.A.G.Physical_Rectangle; Accepted : out Boolean)
     with Global => null, Pre => Valid (S), Post => Valid (S) and Current (S) = Current (S)'Old and
       Layer_Count (S) = Layer_Count (S)'Old + (if Accepted then 1 else 0) and
       Reader_Count (S) = Reader_Count (S)'Old and
       Image_Reader_Count (S) = Image_Reader_Count (S)'Old;
   procedure Add_Glyph (S : in out State; Key : Vulkan_Glyph_Sources.Key;
      Cell : V.A.G.Logical_Rectangle; Tint : V.A.Word; Accepted : out Boolean)
     with Global => (In_Out => D.Engine), Pre => Valid (S) and D.Valid,
       Post => Valid (S) and D.Valid;
   -- Exactly one submission attempt. A cold glyph discards this snapshot;
   -- Poll completes its upload and releases CPU readers before a fresh capture.
   -- No partially captured scene is ever submitted; calls never wait.
   procedure Finish (S : in out State; Result : out Outcome)
     with Global => (In_Out => D.Engine), Pre => Valid (S) and D.Valid,
       Post => Valid (S) and D.Valid;
   procedure Poll (S : in out State; Result : out Outcome)
     with Global => (In_Out => D.Engine), Pre => Valid (S) and D.Valid,
       Post => Valid (S) and D.Valid;
   procedure Discard (S : in out State; Result : out Outcome)
     with Global => (In_Out => D.Engine), Pre => Valid (S) and D.Valid,
       Post => Valid (S) and D.Valid;
   -- Desktop output integration gate. Repaint authorizes a complete software
   -- reconstruction only after this capture and renderer readers have retired.
   -- Complete means the private GPU scene has finished. It does NOT authorize
   -- publication of a CPU output: the adapter must still complete readback and
   -- copy into the exact held output writer before reporting renderer Complete.
   -- Neither completion releases the display's front buffer.
   -- Repeated finish/cancel while a submission is live never submits again.
   type Output_Completion is (Output_Complete, Output_Pending, Output_Repaint, Output_Unsafe);
   function Software_Ready (S : State) return Boolean
     with Global => (Input => D.Engine), Pre => Valid (S) and D.Valid;
   procedure Complete_Output
     (S : in out State; Poll_Only, Capture_Accepted : Boolean;
      Result : out Output_Completion)
     with Global => (In_Out => D.Engine), Pre => Valid (S) and D.Valid,
       Post => Valid (S) and D.Valid and
         (if Result in Output_Complete | Output_Repaint then Software_Ready (S));
   procedure Close (S : in out State; Safe : out Boolean)
     with Global => (In_Out => D.Engine), Pre => Valid (S) and D.Valid,
       Post => Valid (S) and D.Valid;
private
   subtype Count is Natural range 0 .. 128;
   subtype Index is Positive range 1 .. 128;
   type Pin is record
      Key : Vulkan_Glyph_Sources.Key;
      Reader : R.C.Lease := R.C.No_Lease;
   end record;
   type Pins is array (Index) of Pin;
   subtype Image_Count is Natural range 0 .. Natural (Vulkan_Submission.Source_Slot'Last) + 1;
   subtype Image_Index is Positive range 1 .. Image_Count'Last;
   type Image_Pin is record
      Source : Vulkan_Submission.Source_Ticket := Vulkan_Submission.No_Source;
      Reader : D.Source_Reader := D.No_Source_Reader;
   end record;
   type Image_Pins is array (Image_Index) of Image_Pin;
   procedure Pin_Image (S : in out State; Source : Vulkan_Submission.Source_Ticket; Accepted : out Boolean)
     with Global => (In_Out => D.Engine), Pre => Valid (S) and D.Valid,
       Post => Valid (S) and D.Valid and Current (S) = Current (S)'Old and
       Layer_Count (S) = Layer_Count (S)'Old;
   type State is limited record
      Cache : R.State;
      Scene : V.State := V.Open ((1, 1, V.A.G.Unrotated, (1, 1), 0, 0));
      Readers : Pins;
      Used : Count := 0;
      Images : Image_Pins;
      Images_Used : Image_Count := 0;
      Reservation : Compositor_Pool.Ticket := Compositor_Pool.None;
      Status : Phase := Idle;
      Cold, Invalid : Boolean := False;
      Failure : Capture_Failure := No_Failure;
      Peak : V.Length := 0;
      Placeholders : Natural := 0;
   end record;
   function Valid (S : State) return Boolean is
     (R.Valid (S.Cache) and (if S.Status in Idle | Closed then S.Used = 0 and S.Images_Used = 0));
   function Current (S : State) return Phase is (S.Status);
   function Reader_Count (S : State) return Natural is (S.Used);
   function Image_Reader_Count (S : State) return Natural is (S.Images_Used);
   function Layer_Count (S : State) return V.Length is (V.Count (S.Scene));
   function Last_Failure (S : State) return Capture_Failure is (S.Failure);
   function Peak_Layers (S : State) return V.Length is (S.Peak);
   function Placeholder_Draws (S : State) return Natural is (S.Placeholders);
end Desktop_GPU_Scene;
