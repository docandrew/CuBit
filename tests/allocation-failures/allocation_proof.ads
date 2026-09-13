with Interfaces; use Interfaces;
with Page_Allocation; use Page_Allocation;

-- A SPARK instantiation with arbitrary acquisition results. This checks the
-- shared algorithm's result control flow, NOT physical ownership/cleanup.
package Allocation_Proof with SPARK_Mode is
   procedure Exercise
     (Candidate : Unsigned_64; Tracking_OK, Ownership_OK, Mapping_OK : Boolean;
      Frame : out Unsigned_64; Outcome : out Result)
   with Post => (Frame /= 0) = (Outcome = Page_Added);
end Allocation_Proof;
