with System;
with Compositor_Target_Damage;
with Vulkan_Submission;
with Compositor_Pool;
with Compositor_Affine;
-- Couples the actual Vulkan command/fence controller to the three-buffer pool.
-- Native target bindings are immutable, authorized, mutually nonaliasing scene
-- contexts for one output/epoch, retained through GPU AND display retirement.
-- This adapter supplies no evidence of a physical latch or scanout retirement.
package Vulkan_Frame with SPARK_Mode is
   package V renames Vulkan_Submission;
   package P renames Compositor_Pool;
   use type V.Phase, V.State, P.State, P.Ticket, P.Slot, Compositor_Target_Damage.State;
   package D renames Compositor_Target_Damage;
   type Targets is array (P.Live_Slot) of System.Address;
   type Admission is (Started, Deferred, Failed);
   -- Replay scene layers back-to-front after restoring an opaque background.
   -- Caller retains the scene snapshot. At most eight attempts per layer;
   -- the submission-wide draw cap remains authoritative.
   procedure Replay_Layer
     (Submission : in out V.State; Damage : D.State; Source : V.Source_Ticket;
      Screen : Compositor_Affine.G.Output;
      Surface : Compositor_Affine.G.Logical_Rectangle;
      Over, Mask : Boolean; Tint : Compositor_Affine.Word; Accepted : out Boolean;
      Clip : Compositor_Affine.G.Physical_Rectangle :=
        (0, 0, Compositor_Affine.G.Pixel_Edge'Last, Compositor_Affine.G.Pixel_Edge'Last);
      Raster_Glyph : Boolean := False; Straight_Alpha : Boolean := False)
     with Pre => V.Current (Submission) = V.Recording and V.Pass_Active (Submission) and
       D.Valid (Damage) and not D.Faulted (Damage) and D.Active (Damage) /= 0,
       Post => V.Current (Submission) = V.Recording and V.Pass_Active (Submission) and
         V.Same_Sources (Submission, Submission'Old) and
         V.Draws (Submission) >= V.Draws (Submission'Old) and
         V.Draws (Submission) <= V.Draws (Submission'Old) + 8 and
         Accepted = V.Complete_Frame (Submission);
   procedure Replay_Fill
     (Submission : in out V.State; Damage : D.State;
      Screen : Compositor_Affine.G.Output; Surface : Compositor_Affine.G.Logical_Rectangle;
      RGB : Compositor_Affine.Word; Accepted : out Boolean;
      Clip : Compositor_Affine.G.Physical_Rectangle :=
        (0, 0, Compositor_Affine.G.Pixel_Edge'Last, Compositor_Affine.G.Pixel_Edge'Last))
     with Pre => V.Current (Submission) = V.Recording and V.Pass_Active (Submission) and
       D.Valid (Damage) and not D.Faulted (Damage) and D.Active (Damage) /= 0,
       Post => V.Current (Submission) = V.Recording and V.Pass_Active (Submission) and
         V.Same_Sources (Submission, Submission'Old) and
         V.Draws (Submission) >= V.Draws (Submission'Old) and
         V.Draws (Submission) <= V.Draws (Submission'Old) + 8 and
         Accepted = V.Complete_Frame (Submission);
   -- Desktop's fill facade supplies already-scaled output pixels. Do not apply
   -- the logical output transform again; still intersect damage and UI clip.
   procedure Replay_Physical_Fill
     (Submission : in out V.State; Damage : D.State;
      Screen : Compositor_Affine.G.Output; Surface : Compositor_Affine.G.Physical_Rectangle;
      RGB : Compositor_Affine.Word; Accepted : out Boolean;
      Clip : Compositor_Affine.G.Physical_Rectangle :=
        (0, 0, Compositor_Affine.G.Pixel_Edge'Last, Compositor_Affine.G.Pixel_Edge'Last))
     with Pre => V.Current (Submission) = V.Recording and V.Pass_Active (Submission) and
       D.Valid (Damage) and not D.Faulted (Damage) and D.Active (Damage) /= 0,
       Post => V.Current (Submission) = V.Recording and V.Pass_Active (Submission) and
         V.Same_Sources (Submission, Submission'Old) and
         V.Draws (Submission) >= V.Draws (Submission'Old) and
         V.Draws (Submission) <= V.Draws (Submission'Old) + 8 and
         Accepted = V.Complete_Frame (Submission);
   procedure Begin_Record
     (Submission : in out V.State; Pool : in out P.State;
      Result : out Admission; Replace_Ready : Boolean := False)
     with Pre => P.Valid (Pool) and V.Current (Submission) = V.Idle,
       Post => P.Valid (Pool) and
         P.Front (Pool) = P.Front (Pool'Old) and P.Displayed (Pool) = P.Displayed (Pool'Old) and
         V.Same_Sources (Submission, Submission'Old) and
         (case Result is
            when Started => V.Current (Submission) = V.Recording and
              not V.Pass_Started (Submission) and V.Complete_Frame (Submission) and P.Rendering (Pool) and
              not P.Faulted (Pool) and P.Writer (Pool) /= P.None,
            when Deferred => Submission = Submission'Old and Pool = Pool'Old,
            when Failed => P.Faulted (Pool));
   procedure Begin_Scene
     (Submission : in out V.State; Pool : in out P.State; Bindings : Targets;
      Width, Height : Compositor_Affine.G.Physical_Extent; Accepted : out Boolean)
     with Pre => P.Valid (Pool) and P.Rendering (Pool) and
         V.Current (Submission) = V.Recording and not V.Pass_Started (Submission) and V.Complete_Frame (Submission),
       Post => P.Valid (Pool) and P.Writer (Pool) = P.Writer (Pool'Old) and
         P.Front (Pool) = P.Front (Pool'Old) and P.Displayed (Pool) = P.Displayed (Pool'Old) and
         V.Same_Sources (Submission, Submission'Old) and
         (if Accepted then P.Rendering (Pool) and V.Pass_Active (Submission) and V.Current (Submission) = V.Recording
          else P.Faulted (Pool));
   -- End the pass separately so the caller can record its audited final image
   -- layout/barrier commands before sealing. Both steps retain the writer.
   procedure End_Scene
     (Submission : in out V.State; Pool : in out P.State; Accepted : out Boolean)
     with Pre => P.Valid (Pool) and P.Rendering (Pool) and
         V.Current (Submission) = V.Recording and V.Pass_Active (Submission),
       Post => P.Valid (Pool) and P.Writer (Pool) = P.Writer (Pool'Old) and
         P.Front (Pool) = P.Front (Pool'Old) and P.Displayed (Pool) = P.Displayed (Pool'Old) and
         V.Same_Sources (Submission, Submission'Old) and
         V.Complete_Frame (Submission) = V.Complete_Frame (Submission'Old) and
         (if Accepted then P.Rendering (Pool) and V.Pass_Finished (Submission) and V.Current (Submission) = V.Recording
          else P.Faulted (Pool));
   procedure Submit
     (Submission : in out V.State; Pool : in out P.State; Accepted : out Boolean)
     with Pre => P.Valid (Pool) and P.Rendering (Pool) and
         V.Current (Submission) = V.Recording and V.Pass_Finished (Submission) and V.Complete_Frame (Submission),
       Post => P.Valid (Pool) and P.Writer (Pool) = P.Writer (Pool'Old) and
         P.Front (Pool) = P.Front (Pool'Old) and P.Displayed (Pool) = P.Displayed (Pool'Old) and
         V.Same_Sources (Submission, Submission'Old) and
         (if Accepted then V.Current (Submission) = V.Pending else P.Faulted (Pool));
   procedure Cancel
     (Submission : in out V.State; Pool : in out P.State; Released : out Boolean)
     with Pre => P.Valid (Pool) and P.Rendering (Pool) and V.Current (Submission) in V.Recording | V.Sealed,
       Post => P.Valid (Pool) and P.Ready (Pool) = P.Ready (Pool'Old) and
         P.Front (Pool) = P.Front (Pool'Old) and P.Displayed (Pool) = P.Displayed (Pool'Old) and
         V.Same_Sources (Submission, Submission'Old) and
         (if Released then V.Quiescent (Submission) and P.Writer (Pool) = P.None and not P.Faulted (Pool)
          else P.Faulted (Pool) and P.Writer (Pool) = P.Writer (Pool'Old));
   type Completion is (Still_Pending, Ready, Uncertain);
   procedure Poll
     (Submission : in out V.State; Pool : in out P.State; Result : out Completion)
     with Pre => P.Valid (Pool) and P.Rendering (Pool) and V.Current (Submission) = V.Pending,
       Post => P.Valid (Pool) and
         P.Front (Pool) = P.Front (Pool'Old) and P.Displayed (Pool) = P.Displayed (Pool'Old) and
         V.Same_Sources (Submission, Submission'Old) and
         (case Result is
            when Still_Pending => Submission = Submission'Old and Pool = Pool'Old,
            when Ready => V.Quiescent (Submission) and not P.Faulted (Pool) and
              P.Ready (Pool) = P.Writer (Pool'Old) and P.Writer (Pool) = P.None,
            when Uncertain => P.Faulted (Pool) and P.Writer (Pool) = P.Writer (Pool'Old));
   -- Repaint-aware admission/completion. Scene/submit errors leave the active
   -- plan held; a quarantined submission cannot admit another frame.
   procedure Begin_Record
     (Submission : in out V.State; Pool : in out P.State; Damage : in out D.State;
      Result : out Admission; Replace_Ready : Boolean := False)
     with Pre => P.Valid (Pool) and V.Current (Submission) = V.Idle and
         D.Valid (Damage) and not D.Faulted (Damage) and D.Active (Damage) = 0,
       Post => P.Valid (Pool) and D.Valid (Damage) and
         (if Result = Started then V.Current (Submission) = V.Recording and
            not V.Pass_Started (Submission) and V.Complete_Frame (Submission) and not D.Faulted (Damage) and P.Rendering (Pool) and D.Active (Damage) = P.Writer (Pool).Buffer
          else Damage = Damage'Old);
   procedure Cancel
     (Submission : in out V.State; Pool : in out P.State; Damage : in out D.State; Released : out Boolean)
     with Pre => P.Valid (Pool) and P.Rendering (Pool) and V.Current (Submission) in V.Recording | V.Sealed and
         D.Valid (Damage) and not D.Faulted (Damage) and D.Active (Damage) = P.Writer (Pool).Buffer,
       Post => P.Valid (Pool) and D.Valid (Damage) and
         (if Released then D.Active (Damage) = 0 else D.Faulted (Damage));
   procedure Poll
     (Submission : in out V.State; Pool : in out P.State; Damage : in out D.State; Result : out Completion)
     with Pre => P.Valid (Pool) and P.Rendering (Pool) and V.Current (Submission) = V.Pending and
         D.Valid (Damage) and not D.Faulted (Damage) and D.Active (Damage) = P.Writer (Pool).Buffer,
       Post => P.Valid (Pool) and D.Valid (Damage) and
         (case Result is
            when Still_Pending => Damage = Damage'Old,
            when Ready => D.Active (Damage) = 0 and not D.Faulted (Damage),
            when Uncertain => D.Faulted (Damage));
end Vulkan_Frame;
