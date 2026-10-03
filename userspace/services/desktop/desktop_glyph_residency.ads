with Compositor_Glyph_Cache;
with Desktop_Vulkan_Startup;
with Vulkan_Glyph_Sources;
with Vulkan_Owned_Targets;
with Vulkan_Submission;
-- One cache per Desktop device lifetime, exclusively owning backing slots
-- 0..127. No reset/copy, heap storage or internal waiting. CPU scene builders
-- retain returned reader leases until their captured scene is retired; the
-- frame owner additionally prevents release while a GPU frame is pending.
package Desktop_Glyph_Residency with SPARK_Mode is
   package D renames Desktop_Vulkan_Startup;
   package V renames Vulkan_Submission;
   package A renames Vulkan_Owned_Targets.A;
   package C is new Compositor_Glyph_Cache (Maximum_Readers => 128);
   use type C.Token, C.Lease, C.Phase, A.Ticket, V.Source_Ticket;
   type State is limited private with Default_Initial_Condition => Valid (State);
   function Valid (S : State) return Boolean;
   type Outcome is (Available, Uploading, Deferred, Rejected, Unsafe);
   function Pending (S : State) return Boolean;
   function Held (S : State; Reader : C.Lease) return Boolean;
   function Charged (S : State) return C.Byte_Count;
   -- At most one eviction and one upload admission. A ready glyph returns a
   -- reader lease; saturation defers instead of evicting a captured glyph.
   procedure Acquire (S : in out State; Key : Vulkan_Glyph_Sources.Key;
      Source : out V.Source_Ticket; Reader : out C.Lease; Result : out Outcome)
     with Global => (In_Out => D.Engine), Pre => Valid (S) and D.Valid,
       Post => Valid (S) and D.Valid and
         (if Result = Available then Source /= V.No_Source and Held (S, Reader));
   -- One fence observation; partial/pending work retains ownership. Complete
   -- upload is imported and bound before it becomes a cache hit.
   procedure Poll (S : in out State; Result : out Outcome)
     with Global => (In_Out => D.Engine), Pre => Valid (S) and D.Valid,
       Post => Valid (S) and D.Valid;
   procedure Release (S : in out State; Reader : C.Lease; Capture_Retired : Boolean)
     with Global => (Input => D.Engine), Pre => Valid (S) and D.Valid,
       Post => Valid (S);
   procedure Close (S : in out State; Safe : out Boolean)
     with Global => (In_Out => D.Engine), Pre => Valid (S) and D.Valid,
       Post => Valid (S) and D.Valid and (if Safe then Charged (S) = 0);
private
   type Lease_Table is array (C.Slot) of A.Ticket;
   type Source_Table is array (C.Slot) of V.Source_Ticket;
   type Mode is (Idle, Transferring, Quarantined, Closed);
   type State is limited record
      Cache : C.State := C.Open (C.Byte_Count'Last);
      Backings : Lease_Table := (others => A.No_Ticket);
      Sources : Source_Table := (others => V.No_Source);
      Stage : Mode := Idle;
      Stopping : Boolean := False;
      Active : C.Token := C.No_Token;
      Key : Vulkan_Glyph_Sources.Key;
   end record;
   function Valid (S : State) return Boolean is
     (C.Valid (S.Cache) and then
      (if S.Stage = Closed then S.Stopping and C.Charged (S.Cache) = 0 and C.Reader_Count (S.Cache) = 0) and then
      (if S.Stage = Transferring then C.Current (S.Cache, S.Active) and then
         C.Status (S.Cache, S.Active) = C.Building and then S.Backings (S.Active.Position) /= A.No_Ticket));
   function Held (S : State; Reader : C.Lease) return Boolean is (C.Active (S.Cache, Reader));
   function Pending (S : State) return Boolean is (S.Stage = Transferring);
   function Charged (S : State) return C.Byte_Count is (C.Charged (S.Cache));
end Desktop_Glyph_Residency;
