-- Conditional table mutation only, not authorization or revocation.
-- The caller must authenticate CSPACE authority and the destination process
-- incarnation, and hold the mailbox locks across comparison and mutation.
-- GPU users must additionally retire queued work/receipts before slot reuse.
package Capabilities.Endpoint_Disposal with SPARK_Mode is
   function Matches (Installed, Expected : Capability) return Boolean is
     (Expected.capType = CAP_ENDPOINT and then
      Expected.object.ref /= 0 and then Expected.gen /= 0 and then
      Expected.authorityTag /= NO_AUTHORITY_TAG and then
      Installed = Expected);

   -- Empty is not success: callers must retain their own completion receipt.
   -- Exact equality includes rights, tag, object parameter and generation.
   -- Fresh nonwrapping tags are required to distinguish successive issuances;
   -- this operation cannot detect ABA if the identical cap is reinstalled.
   procedure Clear_If_Matching
     (Table : in out CapabilityTable; Slot : CapabilitySlot;
      Expected : Capability; Removed : out Boolean)
     with Global => null,
       Post => Removed = Matches (Table'Old (Slot), Expected) and then
         (if Removed then
            Table (Slot) = NULL_CAPABILITY and then
            (for all I in CapabilitySlot =>
               (if I /= Slot then Table (I) = Table'Old (I)))
          else Table = Table'Old);
end Capabilities.Endpoint_Disposal;
