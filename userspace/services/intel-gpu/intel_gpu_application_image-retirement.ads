with Intel_GPU_GGTT_Reservations;
generic
   -- Trusted, serialized caller: current device ownership/power, every
   -- consumer deregistered/drained, no future submissions, owned non-scanout
   -- range and retained visible scratch. This is independent of the parent's
   -- preparation Owner_Ready: a retired application must no longer satisfy
   -- that live-session gate. Gate must establish cleanup authority itself.
   with function Gate (First, Bytes : Unsigned_64) return Boolean;
   with procedure Read_PTE (Index : Unsigned_64; Value : out Unsigned_64;
                            OK : out Boolean);
   with procedure Write_PTE (Index, Value : Unsigned_64; OK : out Boolean);
   with procedure Invalidate_And_Wait (OK : out Boolean);
package Intel_GPU_Application_Image.Retirement is
   type Result is (Rejected, Quarantined, Address_Released);
   -- Uses the exact allocation/range retained by successful publication.
   -- Address_Released removes only the exact GGTT reservation after completed
   -- scratch remapping/invalidation. All backing remains retained. This does
   -- not retire CPU grants, release supervisor tickets, or recycle sessions.
   -- Once attempted, image addresses cannot be used for registration/update.
   procedure Execute
     (Object : in out State; Ledger : in out Intel_GPU_GGTT_Reservations.Ledger;
      Scratch_DMA : Unsigned_64; Status : out Result);
   -- Trusted coordinator after exact parent slot/generation supervisor ack.
   -- Drops this image's public backing/root receipts, not physical memory or
   -- logical VM snapshots. Expected addresses identify this one-shot image;
   -- they are NOT authority or a substitute for checking the supervisor ack.
   -- No live-app gate is used after revocation. Failed address retirement,
   -- wrong receipts and duplicate completion reject without clearing state.
   -- Consumed preparation/update/context state is never reset or made reusable.
   procedure Forget_Backing_Receipt
     (Object : in out State; Expected_GPU : Unsigned_64;
      Expected_Root : Tables.Page_Mapping;
      Supervisor_Acknowledged : Boolean; Accepted : out Boolean);
end Intel_GPU_Application_Image.Retirement;
