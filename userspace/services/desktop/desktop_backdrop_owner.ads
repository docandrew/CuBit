with CuBit.Appearance;
with Desktop_Vulkan_Startup;
with Vulkan_Owned_Targets;
with Vulkan_Submission;
-- One non-copyable owner per immutable asset. Caller reserves a distinct
-- dedicated backdrop slot and keeps this owner alive for all CPU/GPU scene readers.
-- No automatic eviction, heap allocation, spinning, or per-frame pixel copy.
package Desktop_Backdrop_Owner with SPARK_Mode is
   package D renames Desktop_Vulkan_Startup;
   package A renames Vulkan_Owned_Targets.A;
   package V renames Vulkan_Submission;
   subtype Slot is D.Backing_Slot range V.Backdrop_Slot'First .. V.Backdrop_Slot'Last;
   use type A.Ticket, V.Source_Ticket;
   type Phase is (Fresh, Ready_To_Upload, Uploading, Resident, Quarantined, Closed);
   type Outcome is (Available, Pending, Deferred, Rejected, Unsafe);
   type State is limited private with Default_Initial_Condition => Valid (State);
   function Valid (S : State) return Boolean;
   function Current (S : State) return Phase;
   procedure Acquire
     (S : in out State; Index : Slot; Asset : CuBit.Appearance.Background;
      Source : out V.Source_Ticket; Result : out Outcome)
     with Global => (In_Out => D.Engine), Pre => Valid (S) and D.Valid,
       Post => Valid (S) and D.Valid and
         (if Result = Available then Current (S) = Resident and Source /= V.No_Source and D.Source_Held (Source));
   -- Observe at most one upload fence, then attempt at most one next chunk.
   procedure Poll (S : in out State; Result : out Outcome)
     with Global => (In_Out => D.Engine), Pre => Valid (S) and D.Valid,
       Post => Valid (S) and D.Valid;
   -- Explicit CPU-capture retirement attestation plus healthy renderer idle
   -- are required. Pending/uncertain work never frees or refunds storage.
   procedure Close (S : in out State; Capture_Retired : Boolean; Safe : out Boolean)
     with Global => (In_Out => D.Engine), Pre => Valid (S) and D.Valid,
       Post => Valid (S) and D.Valid and (if Safe then Current (S) = Closed);
private
   type State is limited record
      Status : Phase := Fresh;
      Index : Slot := Slot'First;
      Asset : CuBit.Appearance.Background := CuBit.Appearance.Wallpaper;
      Lease : A.Ticket := A.No_Ticket;
      Source : V.Source_Ticket := V.No_Source;
   end record;
   function Current (S : State) return Phase is (S.Status);
   function Valid (S : State) return Boolean is
     ((if S.Status in Ready_To_Upload | Uploading | Resident then S.Lease /= A.No_Ticket) and
      (if S.Status = Resident then S.Source /= V.No_Source) and
      (if S.Status in Fresh | Closed then S.Lease = A.No_Ticket and S.Source = V.No_Source));
end Desktop_Backdrop_Owner;
