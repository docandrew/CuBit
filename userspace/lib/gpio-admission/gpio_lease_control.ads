pragma Ada_2022;
with Interfaces;
package GPIO_Lease_Control with Pure, SPARK_Mode is
   subtype Lease_ID is Interfaces.Unsigned_64;
   No_Lease : constant Lease_ID := 0;
   type Operation is (No_Control, Arming, Releasing);
   type Completion_Status is (Pending, Completed, Refused, Uncertain);
   type Reply is record
      Status : Completion_Status := Pending;
      Lease : Lease_ID := No_Lease;
   end record;
   -- Typed local result, not a wire/capability interface. Pending never proves
   -- authority or cancellation. Completed arm requires nonzero Lease; completed
   -- release echoes that same lease. Refused/Uncertain may retain a known lease.
   -- Native adapters authenticate actual GPIO completions and initiate once;
   -- Poll only observes retained state, never resubmits or consumes other tokens.
end GPIO_Lease_Control;
