with Interfaces;
with Intel_GPU_Plane_Decode; with Intel_GPU_Cursor_Decode;
with Intel_GPU_Scanout_Range;
with Intel_GPU_Display_Presence;
package Intel_GPU_Scanout_Inventory with SPARK_Mode is
   use Interfaces;
   subtype Plane_Index is Positive range 1 .. 20;
   subtype Cursor_Index is Positive range 1 .. 4;
   type Plane_Observation is record
      Collected : Boolean := False;
      Before, After : Intel_GPU_Plane_Decode.Sample;
   end record;
   type Cursor_Observation is record
      Collected : Boolean := False;
      Before, After : Intel_GPU_Cursor_Decode.Sample;
   end record;
   type Planes is array (Plane_Index) of Plane_Observation;
   type Cursors is array (Cursor_Index) of Cursor_Observation;
   type Extents is array (Positive range 1 .. 24) of Intel_GPU_Scanout_Range.Extent;
   type Outcome is (Incomplete, Unsupported_Plane, Unsupported_Cursor, Complete);
   type Inventory is record
      Status : Outcome := Incomplete;
      Count : Natural range 0 .. 24 := 0;
      Ranges : Extents := (others => (others => <>));
   end record;
   -- Each physically present pipe has five planes and one cursor. Caller
   -- retains serialization and power across collection/publication. This is
   -- an exclusion inventory, NOT ownership of other GGTT addresses. Disabled
   -- objects never authorize reclaiming old PTEs or firmware allocations.
   -- Unknown presence is incomplete. Absent objects need no fabricated
   -- disabled sample. Presence evidence must belong to this same device.
   function Collect (P : Planes; C : Cursors; Table_Bytes : Unsigned_64;
     Presence : Intel_GPU_Display_Presence.Snapshot) return Inventory;
   -- Collect and plan from the same observations. Reject missing/unsupported
   -- objects and any target allocation overlapping a live plane or cursor.
   -- Caller retains power/serialization throughout; this is GGTT geometry,
   -- not physical-alias exclusion, ownership, producer completion or latch.
   function Plan_Linear_Flip
     (P : Planes; C : Cursors;
      Presence : Intel_GPU_Display_Presence.Snapshot;
      Selected : Plane_Index;
      Table_Bytes, Target_First, Target_Bytes : Unsigned_64)
      return Intel_GPU_Plane_Decode.Flip_Plan
     with Global => null;
   function Canonical (E : Intel_GPU_Scanout_Range.Extent) return Boolean is
     (E.Valid and then E.Bytes > 0 and then E.First <= Unsigned_64'Last - E.Bytes);
   function Disjoint (A, B : Intel_GPU_Scanout_Range.Extent) return Boolean is
     (if A.First >= B.First then A.First - B.First >= B.Bytes
      else B.First - A.First >= A.Bytes);
   function No_Scanout_Overlap (State : Inventory; Candidate : Intel_GPU_Scanout_Range.Extent)
     return Boolean
   with Post => No_Scanout_Overlap'Result =
     (State.Status = Complete and then Canonical (Candidate) and then
      (for all I in 1 .. State.Count =>
         Canonical (State.Ranges (I)) and then
         Disjoint (Candidate, State.Ranges (I))));
end Intel_GPU_Scanout_Inventory;
