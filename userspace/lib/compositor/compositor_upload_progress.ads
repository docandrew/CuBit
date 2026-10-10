with Interfaces; with Compositor_Upload;
-- One image's full-width row coverage: an initial upload of every row, or a
-- retained-content update of one damaged row band. The shared uploader must also
-- exclude other writers/GPU work and retire descriptors before Begin_Image.
-- Identities come from the matching backing ledger; never copy/reset state.
generic
   Last_Sequence : Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;
package Compositor_Upload_Progress with SPARK_Mode, Pure is
   package G renames Compositor_Upload;
   subtype Serial is Interfaces.Unsigned_64;
   use type Serial, G.Pixel_Format;
   type Phase is (Untouched, Preparing, Writing, Pending, Ready, Quarantined);
   type Ticket is private;
   No_Ticket : constant Ticket;
   type State is private;
   function Valid (S : State) return Boolean;
   function Current (S : State) return Phase;
   function Source_Identity (S : State) return Natural;
   function Completed_Rows (S : State) return G.Edge;
   function Height (S : State) return G.Edge;
   -- Exclusive end row of the current pass; Height when the image is Ready.
   function Pass_End (S : State) return G.Edge;
   -- The current pass keeps the previous content of rows outside its band,
   -- so its first chunk must not discard the image.
   function Retaining (S : State) return Boolean;
   function Sequence (S : State) return Serial;
   function Active (S : State; T : Ticket) return Boolean;
   function Can_Write (S : State; T : Ticket) return Boolean;
   function Publishable (S : State; Identity : Natural) return Boolean
     with Pre => Valid (S), Post => (if Publishable'Result then Completed_Rows (S) = Height (S));
   -- A new content version may reuse a retired descriptor's same allocation.
   -- The chunk sequence never resets, including across allocation replacement.
   procedure Begin_Image (S : in out State; Identity : Natural;
      Width, Height : G.Edge; Kind : G.Pixel_Format; Accepted : out Boolean)
     with Pre => Valid (S), Post => Valid (S) and Sequence (S) = Sequence (S'Old) and
       (if Accepted then Current (S) = Preparing and Source_Identity (S) = Identity and Completed_Rows (S) = 0 and
          Compositor_Upload_Progress.Height (S) = Height and Pass_End (S) = Height and not Retaining (S)
        else S = S'Old);
   -- Retained-content update of rows First .. Last - 1 of the same complete
   -- allocation. Rows outside the band keep the previous complete content.
   -- The image is unpublishable from acceptance until the whole band has
   -- completed; descriptors must already be retired, as for Begin_Image.
   procedure Begin_Update (S : in out State; Identity : Natural;
      First, Last : G.Edge; Accepted : out Boolean)
     with Pre => Valid (S), Post => Valid (S) and Sequence (S) = Sequence (S'Old) and
       Accepted = (Current (S'Old) = Ready and Identity > 0 and Identity = Source_Identity (S'Old) and
                   First < Last and Last <= Height (S'Old)) and
       (if Accepted then Current (S) = Preparing and Source_Identity (S) = Identity and
          Completed_Rows (S) = First and Pass_End (S) = Last and Retaining (S) and
          Height (S) = Height (S'Old)
        else S = S'Old);
   procedure Begin_Write (S : in out State; Capacity : G.Byte_Count;
      Plan : out G.Plan; T : out Ticket; Discard : out Boolean; Accepted : out Boolean;
      Row_Pixels : G.Edge := 0)
     with Pre => Valid (S), Post => Valid (S) and Completed_Rows (S) = Completed_Rows (S'Old) and
       Accepted = Can_Write (S, T) and (if Accepted then
         G.Valid (Plan) and G.Area (Plan).Y = Completed_Rows (S) and
         G.Area (Plan).Height > 0 and G.Area (Plan).Y + G.Area (Plan).Height <= Pass_End (S) and
         Pass_End (S) <= Height (S) and
         Discard = (Completed_Rows (S) = 0 and not Retaining (S)) and Sequence (S) > Sequence (S'Old)
        else S = S'Old and T = No_Ticket and not Discard);
   -- Call only after the matching transfer batch was accepted by submission.
   procedure Submitted (S : in out State; T : Ticket; Accepted : out Boolean)
     with Pre => Valid (S), Post => Valid (S) and Completed_Rows (S) = Completed_Rows (S'Old) and
       Sequence (S) = Sequence (S'Old) and Accepted = Can_Write (S'Old, T) and
       (if Accepted then Current (S) = Pending and Active (S, T) else S = S'Old);
   type Observation is (Still_Pending, Completed, Uncertain);
   -- Completion is observed through the matching submission's fence. Stale or
   -- duplicate observations cannot advance rows; uncertainty is never reset.
   procedure Observe (S : in out State; T : Ticket; Result : Observation)
     with Pre => Valid (S), Post => Valid (S) and Sequence (S) = Sequence (S'Old) and
       Completed_Rows (S) >= Completed_Rows (S'Old) and
       (if Current (S'Old) /= Pending or not Active (S'Old, T) or Result = Still_Pending then S = S'Old) and
       (if Current (S'Old) = Pending and Active (S'Old, T) then
          (case Result is when Completed => Completed_Rows (S) > Completed_Rows (S'Old) and Current (S) in Preparing | Ready,
             when Uncertain => Current (S) = Quarantined and Completed_Rows (S) = Completed_Rows (S'Old),
             when Still_Pending => True));
   -- Only a never-submitted writer may cancel. Confirmation must cover both
   -- producer retirement and any recorded-command cancellation. Pending work
   -- cannot be cancelled by this operation, even if Confirmed is true.
   procedure Cancel (S : in out State; T : Ticket; Confirmed : Boolean)
     with Pre => Valid (S), Post => Valid (S) and Completed_Rows (S) = Completed_Rows (S'Old) and
       Sequence (S) = Sequence (S'Old) and
       (if Can_Write (S'Old, T) then Current (S) = (if Confirmed then Preparing else Quarantined)
        else S = S'Old);
private
   type Ticket is record Identity : Natural := 0; Number : Serial := 0; end record;
   No_Ticket : constant Ticket := (others => <>);
   type State is record
      Mode : Phase := Untouched;
      Identity : Natural := 0;
      Width, Rows, Done, Limit : G.Edge := 0;
      Retain : Boolean := False;
      Kind : G.Pixel_Format := G.BGRA8;
      Last : Serial range 0 .. Last_Sequence := 0;
      Plan : G.Plan;
   end record;
   function Current (S : State) return Phase is (S.Mode);
   function Source_Identity (S : State) return Natural is (S.Identity);
   function Completed_Rows (S : State) return G.Edge is (S.Done);
   function Height (S : State) return G.Edge is (S.Rows);
   function Pass_End (S : State) return G.Edge is (S.Limit);
   function Retaining (S : State) return Boolean is (S.Retain);
   function Sequence (S : State) return Serial is (S.Last);
   function Active (S : State; T : Ticket) return Boolean is
     (S.Mode in Writing | Pending and T /= No_Ticket and T.Identity = S.Identity and T.Number = S.Last);
   function Can_Write (S : State; T : Ticket) return Boolean is (S.Mode = Writing and Active (S, T));
   function Publishable (S : State; Identity : Natural) return Boolean is
     (S.Mode = Ready and Identity /= 0 and Identity = S.Identity);
   function Valid (S : State) return Boolean is
     (S.Last <= Last_Sequence and
      (if S.Mode = Untouched then S.Identity = 0 and S.Done = 0
       else S.Identity > 0 and S.Width > 0 and S.Rows > 0 and S.Limit <= S.Rows and
         (if S.Mode = Ready then S.Done = S.Rows and S.Limit = S.Rows else S.Done < S.Limit)) and
      (if S.Mode in Writing | Pending then S.Last > 0 and G.Valid (S.Plan) and
         G.Image_Width (S.Plan) = S.Width and G.Image_Height (S.Plan) = S.Rows and G.Format (S.Plan) = S.Kind and
         G.Area (S.Plan).X = 0 and G.Area (S.Plan).Y = S.Done and G.Area (S.Plan).Width = S.Width and
         G.Area (S.Plan).Height > 0 and G.Area (S.Plan).Y + G.Area (S.Plan).Height <= S.Limit));
end Compositor_Upload_Progress;
