-- Common single-page acquisition sequence. Hardware/ownership callbacks
-- remain trusted boundaries; host tests inject failure at each callback.
package Page_Allocation with SPARK_Mode is
   type Result is
     (Page_Added, Frame_Tracking_Full, Frame_Quota_Full, Physical_Memory_Exhausted,
      Tracking_Storage_Exhausted, Frame_Ownership_Failed, Page_Table_Exhausted,
      Mapping_Already_Present);

   generic
      type Frame_Address is mod <>;
      with procedure Allocate (Frame : out Frame_Address);
      with procedure Release_Frame (Frame : Frame_Address);
      with procedure Track (Frame : Frame_Address; Accepted : out Boolean);
      with procedure Forget;
      with procedure Claim (Frame : Frame_Address; Accepted : out Boolean);
      -- False must mean no leaf mapping was published. Intermediate tables
      -- remain linked to the address space and owned by its teardown.
      with procedure Map (Frame : Frame_Address; Accepted : out Boolean);
   procedure Acquire (Frame : out Frame_Address; Outcome : out Result)
     with Post => (Frame /= 0) = (Outcome = Page_Added);
end Page_Allocation;
