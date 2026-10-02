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
   type Result is (Rejected, Quarantined, Detached);
   -- Uses the exact allocation/range retained by successful publication.
   -- Even Detached retains all backing and the GGTT reservation. It does not
   -- retire CPU grants, release supervisor tickets, or authorize reuse.
   -- Once attempted, image addresses cannot be used for registration/update.
   procedure Execute
     (Object : in out State; Ledger : Intel_GPU_GGTT_Reservations.Ledger;
      Scratch_DMA : Unsigned_64; Status : out Result);
end Intel_GPU_Application_Image.Retirement;
