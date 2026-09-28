--  Retirement policy for one allocation generation, not a device fence.
--  Trusted integration must supply genuine hardware completion evidence.
package Intel_GPU_DMA_Lifetime with SPARK_Mode is
   type State is
     (Private_Buffer, GPU_Reachable, Draining, GPU_Stopped,
      Reclaimable, Quarantined);
   type Event is
     (Publish, Retire, Stop_Confirmed, Mapping_Revoked, Owner_Lost);

   --  Publish must be recorded BEFORE installing any GPU-visible address.
   --  Stop_Confirmed means no outstanding or future fetch can use this buffer.
   --  Mapping_Revoked includes completion of required translation invalidation.
   --  Out-of-order/duplicate events cannot advance retirement. Quarantine is
   --  terminal here: recovery requires a separate trusted device-reset protocol.
   function Next (Before : State; Action : Event) return State
   with Global => null,
     Post =>
       (if Before = Quarantined then Next'Result = Quarantined) and then
       (if Before = Reclaimable then Next'Result = Reclaimable) and then
       (if Next'Result = Reclaimable and Before /= Reclaimable then
          (Before = Private_Buffer and Action in Retire | Owner_Lost) or
          (Before = GPU_Stopped and Action = Mapping_Revoked)) and then
       (if Next'Result = GPU_Stopped and Before /= GPU_Stopped then
          Before = Draining and Action = Stop_Confirmed) and then
       (if Action = Owner_Lost and
           Before in GPU_Reachable | Draining | GPU_Stopped then
          Next'Result = Quarantined);
end Intel_GPU_DMA_Lifetime;
