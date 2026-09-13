package body Allocation_Proof with SPARK_Mode is
   procedure Exercise
     (Candidate : Unsigned_64; Tracking_OK, Ownership_OK, Mapping_OK : Boolean;
      Frame : out Unsigned_64; Outcome : out Result)
   is
      procedure Allocate (Value : out Unsigned_64) is
      begin
         Value := Candidate;
      end Allocate;
      procedure Release_Frame (Value : Unsigned_64) is null;
      procedure Track (Value : Unsigned_64; Accepted : out Boolean) is
         pragma Unreferenced (Value);
      begin
         Accepted := Tracking_OK;
      end Track;
      procedure Forget is null;
      procedure Claim (Value : Unsigned_64; Accepted : out Boolean) is
         pragma Unreferenced (Value);
      begin
         Accepted := Ownership_OK;
      end Claim;
      procedure Map (Value : Unsigned_64; Accepted : out Boolean) is
         pragma Unreferenced (Value);
      begin
         Accepted := Mapping_OK;
      end Map;
      procedure Acquire is new Page_Allocation.Acquire
        (Unsigned_64, Allocate, Release_Frame, Track, Forget, Claim, Map);
   begin
      Acquire (Frame, Outcome);
   end Exercise;
end Allocation_Proof;
