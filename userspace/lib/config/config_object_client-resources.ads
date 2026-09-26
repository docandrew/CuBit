with CCL.Resources;
with CCL.Types;

-- Stable, single-owner Config backing for a CCL resource. A Collection owns
-- both the private IPC client and its exact registry lease. Neither callers
-- nor scripts can attach an arbitrary reference to an already-open client.
-- Keep this limited object alive at a stable address until Cleanup releases
-- it. Registry/context and completion tokens are allocated by the owning host.
package Config_Object_Client.Resources is
   type Collection is limited private;
   type Lifetime is (Vacant, Acquiring, Available, Calling, Draining, Closing, Retiring, Quarantined);
   function State (Object : Collection) return Lifetime;
   function Reference_Of (Object : Collection; Owner : CCL.Resources.Registry)
     return CCL.Resources.Reference;
   function Contract_Of (Object : Collection) return CCL.Objects.Binding;

   procedure Create
     (Object : in out Collection; Owner : in out CCL.Resources.Registry;
      Session : CCL.Resources.Run; Kind : CCL.Types.Type_Reference;
      Endpoint : CuBit.Messages.CapabilitySlot; Name : String;
      Contract : CCL.Objects.Binding; Access_Rights : Config_Object_Messages.Access_Mode;
      Context, Token : Number; Result : out Submission);
   -- Reserve a fresh resource lease before allocating a grant or submitting
   -- Create. Kind must be a one-parameter resource whose parameter exactly
   -- matches Contract in the registry's pinned type universe. No serialization.
   -- Calls grant no authority: Endpoint and the kernel/service policy remain
   -- the security boundary. Name/type descriptions are not grants.

   procedure Get
     (Object : in out Collection; Owner : in out CCL.Resources.Registry;
      Reference : CCL.Resources.Reference; Token : Number; Result : out Submission);
   procedure Set
     (Object : in out Collection; Owner : in out CCL.Resources.Registry;
      Reference : CCL.Resources.Reference; Value : CCL.Objects.Image;
      Expected_Revision, Token : Number; Result : out Submission);
   procedure Close
     (Object : in out Collection; Owner : in out CCL.Resources.Registry;
      Reference : CCL.Resources.Reference; Token : Number; Result : out Submission);
   -- Wrong, foreign, closed or stopped references fail before any IPC. Close
   -- consumes the reference on completion, even if the provider rejects it;
   -- host cleanup still owns any remaining remote handle.

   procedure Complete
     (Object : in out Collection; Owner : CCL.Resources.Registry;
      Completion : CuBit.Messages.CompletionEntry; Result : out Completion_Result);
   procedure Take_Result
     (Object : in out Collection; Owner : in out CCL.Resources.Registry;
      Item : out Response; Taken : out Boolean);
   -- Dispatch only kernel-authenticated completion entries. Take_Result also
   -- completes the registry transition. A success received after Stop is
   -- drained, but Reference_Of remains empty; Cleanup closes its backing handle.

   procedure Retire
     (Object : in out Collection; Owner : in out CCL.Resources.Registry);
   -- Immediately removes script access. It does not cancel an outstanding
   -- Create/Get/Set/Close, consume its receipt or recycle its loan frame.

   type Cleanup_Result is
     (Released, Completion_Pending, Close_Submitted, Not_Submitted,
      Grant_Pending, Quarantine_Required, Invalid_Owner, Invalid_Token);
   procedure Cleanup
     (Object : in out Collection; Owner : in out CCL.Resources.Registry;
      Token : Number; Result : out Cleanup_Result);
   -- Drive from the host event loop, not a polling loop inside this package.
   -- Drain completions first, close known handles, then confirm grant retirement
   -- before reclaiming the lease. Released objects can be reused, retaining
   -- their monotonically increasing completion-token floor across acquisitions.
   -- An uncertain acquisition with an unknown remote handle or an uncertain
   -- Close is quarantined, never silently reused. Service-lifetime reconciliation
   -- is required for that case; grant retirement alone is insufficient.
private
   type Collection is limited record
      Backend : Client;
      Current : Lifetime := Vacant;
      Lease, Call : CCL.Resources.Ticket := CCL.Resources.No_Ticket;
      Reference : CCL.Resources.Reference := CCL.Resources.No_Reference;
      Kind : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
      Unknown_Resource : Boolean := False;
   end record;
end Config_Object_Client.Resources;
