with Compositor_Glyph_Cache;
with Vulkan_Submission;
-- Metadata association owned by one submission/cache lifetime. Fresh owners
-- only; never reset a live association or use it with another submission owner.
package Vulkan_Glyph_Sources with SPARK_Mode is
   package V renames Vulkan_Submission;
   package Keys is new Compositor_Glyph_Cache;
   package L renames Keys.L;
   subtype Slot is Keys.Slot;
   subtype Key is Keys.Key;
   use type V.Source_Ticket, V.Phase;
   type State is private;
   function At_Slot (S : State; I : Slot) return V.Source_Ticket;
   function Matches (S : State; I : Slot; K : Key) return Boolean;
   function Same_Entry (Left, Right : State; I : Slot) return Boolean with Ghost;
   function Resolve (S : State; Submission : V.State; K : Key) return V.Source_Ticket
     with Post => (if Resolve'Result /= V.No_Source then
       V.Source_Valid (Submission, Resolve'Result) and then
       (for some I in Slot => Matches (S, I, K) and At_Slot (S, I) = Resolve'Result)
       else (for all I in Slot => not Matches (S, I, K) or else
          not V.Source_Valid (Submission, At_Slot (S, I))));
   -- The foreign image owner attests this R8 raster's actual layout/density.
   -- Binding does not upload, grant authority, complete work or release pixels.
   -- No renaming live images, duplicate live keys/tickets, or mutation in flight.
   procedure Bind
     (S : in out State; Submission : V.State; I : Slot; K : Key;
      Source : V.Source_Ticket; Raster : L.Layout; Accepted : out Boolean)
     with Post =>
       (if Accepted then V.Current (Submission) = V.Idle and
          V.Source_Valid (Submission, Source) and At_Slot (S, I) = Source and Matches (S, I, K) and
          Resolve (S, Submission, K) = Source and
          not V.Source_Valid (Submission, At_Slot (S'Old, I)) and
          L.Valid (Raster) and L.Same_Raster (Raster, L.Plan (K.Scale)) and
          (for all J in Slot => (if V.Source_Valid (Submission, At_Slot (S'Old, J)) then
             At_Slot (S'Old, J) /= Source and not Matches (S'Old, J, K)))
        else S = S'Old) and
       (for all J in Slot => (if J /= I then
          Same_Entry (S, S'Old, J)));
   -- Only discard metadata after the submission has actually retired its source.
   procedure Forget (S : in out State; Submission : V.State; I : Slot; Accepted : out Boolean)
     with Post =>
       (if Accepted then V.Current (Submission) = V.Idle and
          not V.Source_Valid (Submission, At_Slot (S'Old, I)) and At_Slot (S, I) = V.No_Source
        else S = S'Old) and
       (for all J in Slot => (if J /= I then Same_Entry (S, S'Old, J)));
private
   type Glyph_Entry is record
      Identity : Key;
      Source : V.Source_Ticket := V.No_Source;
   end record;
   type Entries is array (Slot) of Glyph_Entry;
   type State is record
      Items : Entries;
   end record;
   function Same_Entry (Left, Right : State; I : Slot) return Boolean is (Left.Items (I) = Right.Items (I));
   function At_Slot (S : State; I : Slot) return V.Source_Ticket is (S.Items (I).Source);
   function Matches (S : State; I : Slot; K : Key) return Boolean is
     (S.Items (I).Source /= V.No_Source and then Keys.Same (S.Items (I).Identity, K));
end Vulkan_Glyph_Sources;
