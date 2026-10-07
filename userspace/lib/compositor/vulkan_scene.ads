with Vulkan_Glyph_Sources;
with Compositor_Image_Sampling;
with Compositor_Affine;
with Compositor_Target_Damage;
with Vulkan_Submission;
with Compositor_Source_Region;
-- Immutable bounded command snapshot. Contains identities and geometry, never
-- pixels or authority to import memory. Submission owns referenced GPU leases.
package Vulkan_Scene with SPARK_Mode is
   package V renames Vulkan_Submission;
   package A renames Compositor_Affine;
   package D renames Compositor_Target_Damage;
   package R renames Compositor_Source_Region;
   use type R.Rectangle;
   use type V.Phase, V.Source_Ticket, D.P.Slot, A.G.Output, A.G.Logical_Rectangle;
   Maximum_Layers : constant := V.Maximum_Draws / D.D.Capacity;
   subtype Length is Natural range 0 .. Maximum_Layers;
   subtype Index is Positive range 1 .. Maximum_Layers;
   -- Clip commands reuse Surface; they neither draw nor acquire a source.
   -- Reset_Clip restores the whole physical output. Clips are absolute, not
   -- an implicit stack: callers capture their already-intersected UI clip.
   -- Glyph_Mask keeps the same source ticket and logical cell but uses the
   -- existing snapped, output-density glyph placement; Over and Mask are true.
   -- Physical_Solid uses output pixel coordinates in Surface, validated at
   -- admission. It shares existing storage and ordering with logical layers.
   type Layer_Kind is (Textured, Straight_Textured, Solid, Set_Clip, Reset_Clip, Glyph_Mask, Physical_Solid,
      Backdrop_Fill, Backdrop_Fit, Backdrop_Center, Set_Physical_Clip, Checker_Grid, Region_Textured, Straight_Region,
      Preview);
   -- Backdrop kinds retain a source ticket and store its physical dimensions
   -- in Surface.Right/Bottom; Left/Top, tint and blend flags must be zero.
   -- Use Append_Backdrop to capture this representation without extra storage.
   type Layer is record
      Source : V.Source_Ticket := V.No_Source;
      Surface : A.G.Logical_Rectangle := (0, 0, 0, 0);
      Over, Mask : Boolean := False;
      Tint : A.Word := 0;
      Kind : Layer_Kind := Textured;
   end record;
   type Phase is (Collecting, Sealed, Rejected);
   type Preview_Description is record
      Width, Height : Compositor_Image_Sampling.Extent := 1;
      Mode : Compositor_Image_Sampling.Placement := Compositor_Image_Sampling.Fill;
   end record;
   type State is private;
   function Current (S : State) return Phase;
   function Count (S : State) return Length;
   function Output (S : State) return A.G.Output;
   function Item (S : State; I : Index) return Layer with Pre => I <= Count (S);
   function Region_At (S : State; I : Index) return R.Rectangle with Pre => I <= Count (S);
   function Preview_At (S : State; I : Index) return Preview_Description with Pre => I <= Count (S);
   -- Only typed capture initializes preview metadata. Surface retains logical
   -- bounds; source dimensions are separate, never encoded in coordinates.
   procedure Append_Preview
     (S : in out State; Source : V.Source_Ticket; Bounds : A.G.Logical_Rectangle;
      Description : Preview_Description; Accepted : out Boolean)
     with Post => Output (S) = Output (S'Old) and
       (if Accepted then Current (S) = Collecting and Count (S) = Count (S'Old) + 1 and
          Item (S, Count (S)).Kind = Preview and Item (S, Count (S)).Source = Source and
          Item (S, Count (S)).Surface = Bounds and Preview_At (S, Count (S)) = Description
        else Current (S) = Rejected and Count (S) = Count (S'Old));
   function Sources_Ready (S : State; Submission : V.State) return Boolean;
   function Open (Screen : A.G.Output; Background : A.Word := 0) return State
     with Post => Current (Open'Result) = Collecting and Count (Open'Result) = 0 and Output (Open'Result) = Screen;
   -- Late edits and overflow reject without discarding captured metadata.
   procedure Append (S : in out State; Value : Layer; Accepted : out Boolean)
     with Post => Output (S) = Output (S'Old) and
       (if Accepted then Current (S) = Collecting and Count (S) = Count (S'Old) + 1 and
          Item (S, Count (S)) = Value
        else Current (S) = Rejected and Count (S) = Count (S'Old)) and
       (for all I in 1 .. Count (S'Old) => Item (S, I) = Item (S'Old, I) and Region_At (S, I) = Region_At (S'Old, I));
   -- Only this operation can initialize a region layer. Ordinary Append
   -- rejects region kinds so an uninitialized window cannot enter the scene.
   procedure Append_Region (S : in out State; Source : V.Source_Ticket;
      Surface : A.G.Logical_Rectangle; Region : R.Rectangle;
      Over, Straight_Alpha : Boolean; Accepted : out Boolean)
     with Post => Output (S) = Output (S'Old) and
       (if Accepted then Current (S) = Collecting and Count (S) = Count (S'Old) + 1 and
          Item (S, Count (S)).Source = Source and Region_At (S, Count (S)) = Region and
          Item (S, Count (S)).Kind in Region_Textured | Straight_Region
        else Current (S) = Rejected and Count (S) = Count (S'Old)) and
       (for all I in 1 .. Count (S'Old) => Item (S, I) = Item (S'Old, I) and
          Region_At (S, I) = Region_At (S'Old, I));
   -- Capture Desktop's already-transformed opaque output rectangle.
   procedure Append_Physical_Fill
     (S : in out State; Area : A.G.Physical_Rectangle; RGB : A.Word; Accepted : out Boolean)
     with Post => Output (S) = Output (S'Old) and
       (if Accepted then Current (S) = Collecting and Count (S) = Count (S'Old) + 1 and
          Item (S, Count (S)).Kind = Physical_Solid
        else Current (S) = Rejected and Count (S) = Count (S'Old)) and
       (for all I in 1 .. Count (S'Old) => Item (S, I) = Item (S'Old, I) and Region_At (S, I) = Region_At (S'Old, I));
   -- Physical clips are already transformed by Desktop. Preserve their exact
   -- pixel edges across fractional DPI, rotation and nonzero output origins.
   -- Empty/reversed clips suppress drawing until another clip or Reset_Clip.
   procedure Append_Physical_Clip
     (S : in out State; Area : A.G.Physical_Rectangle; Accepted : out Boolean)
     with Post => Output (S) = Output (S'Old) and
       (if Accepted then Current (S) = Collecting and Count (S) = Count (S'Old) + 1 and
          Item (S, Count (S)).Kind = Set_Physical_Clip
        else Current (S) = Rejected and Count (S) = Count (S'Old)) and
       (for all I in 1 .. Count (S'Old) => Item (S, I) = Item (S'Old, I) and Region_At (S, I) = Region_At (S'Old, I));
   procedure Append_Backdrop
     (S : in out State; Source : V.Source_Ticket;
      Source_W, Source_H : A.G.Physical_Extent;
      Mode : Compositor_Image_Sampling.Placement; Accepted : out Boolean)
     with Post => Output (S) = Output (S'Old) and
       (if Accepted then Current (S) = Collecting and Count (S) = Count (S'Old) + 1 and
          Item (S, Count (S)).Source = Source and
          Item (S, Count (S)).Kind in Backdrop_Fill | Backdrop_Fit | Backdrop_Center
        else Current (S) = Rejected and Count (S) = Count (S'Old)) and
       (for all I in 1 .. Count (S'Old) => Item (S, I) = Item (S'Old, I) and Region_At (S, I) = Region_At (S'Old, I));
   -- Typed glyph capture: resolve face/code/density against a retained source.
   -- A missing/stale/wrong-density association rejects the complete snapshot.
   procedure Append_Glyph
     (S : in out State; Submission : V.State; Sources : Vulkan_Glyph_Sources.State;
      Key : Vulkan_Glyph_Sources.Key; Cell : A.G.Logical_Rectangle;
      Tint : A.Word; Accepted : out Boolean)
     with Post => Output (S) = Output (S'Old) and
       (if Accepted then Current (S) = Collecting and Count (S) = Count (S'Old) + 1 and
          Item (S, Count (S)).Kind = Glyph_Mask and
          V.Source_Valid (Submission, Item (S, Count (S)).Source)
        else Current (S) = Rejected and Count (S) = Count (S'Old)) and
       (for all I in 1 .. Count (S'Old) => Item (S, I) = Item (S'Old, I) and Region_At (S, I) = Region_At (S'Old, I));
   -- Lower an opaque vertical gradient into at most 256 ordered solid bands.
   -- Overflow rejects the whole snapshot, including any captured band prefix.
   procedure Append_Gradient
     (S : in out State; Surface : A.G.Logical_Rectangle;
      Top, Bottom : A.Word; Accepted : out Boolean)
     with Post => Output (S) = Output (S'Old) and
       Count (S) >= Count (S'Old) and Count (S) <= Count (S'Old) + 256 and
       (if Accepted then Current (S) = Collecting else Current (S) = Rejected) and
       (for all I in 1 .. Count (S'Old) => Item (S, I) = Item (S'Old, I) and Region_At (S, I) = Region_At (S'Old, I));
   procedure Seal (S : in out State; Accepted : out Boolean)
     with Post => Output (S) = Output (S'Old) and Count (S) = Count (S'Old) and
       (for all I in 1 .. Count (S) => Item (S, I) = Item (S'Old, I)) and
       Accepted = (Current (S) = Sealed) and
       (if Current (S'Old) = Collecting then Accepted else Current (S) = Rejected);
   -- Preflight every ticket before any layer draw. The source table cannot
   -- change while recording. Replay restores background; caller captures every
   -- intersecting layer in back-to-front order; no truncation is permitted.
   procedure Replay
     (S : State; Submission : in out V.State; Damage : D.State; Accepted : out Boolean)
     with Pre => V.Current (Submission) = V.Recording and V.Pass_Active (Submission) and
       D.Valid (Damage) and not D.Faulted (Damage) and D.Active (Damage) /= 0,
       Post => V.Current (Submission) = V.Recording and V.Pass_Active (Submission) and
         V.Same_Sources (Submission, Submission'Old) and Accepted = V.Complete_Frame (Submission) and
         (if Accepted then Current (S) = Sealed) and
         (if not Sources_Ready (S, Submission'Old) or Current (S) /= Sealed then
            V.Draws (Submission) = V.Draws (Submission'Old));
private
   type Layers is array (Index) of Layer;
   type Region_Table is array (Index) of R.Rectangle;
   type Preview_Table is array (Index) of Preview_Description;
   type State is record
      Status : Phase := Collecting;
      Used : Length := 0;
      Screen : A.G.Output := (Width => 1, Height => 1, others => <>);
      Background : A.Word := 0;
      Entries : Layers;
      Regions : Region_Table;
      Previews : Preview_Table;
   end record;
   function Current (S : State) return Phase is (S.Status);
   function Count (S : State) return Length is (S.Used);
   function Output (S : State) return A.G.Output is (S.Screen);
   function Item (S : State; I : Index) return Layer is (S.Entries (I));
   function Region_At (S : State; I : Index) return R.Rectangle is (S.Regions (I));
   function Preview_At (S : State; I : Index) return Preview_Description is (S.Previews (I));
   function Sources_Ready (S : State; Submission : V.State) return Boolean is
     (for all I in 1 .. S.Used =>
        (if S.Entries (I).Kind in Textured | Straight_Textured | Glyph_Mask | Backdrop_Fill | Backdrop_Fit | Backdrop_Center | Region_Textured | Straight_Region | Preview then V.Source_Valid (Submission, S.Entries (I).Source)));
end Vulkan_Scene;
