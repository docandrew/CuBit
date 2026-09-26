with CCL.Resources;

-- Trusted host completion boundary for opaque resources. References are never
-- integer handles or native persistence images. The host must authenticate and
-- correlate the acquisition to this pending call/run, and retain ownership of
-- any rejected/late resource for cleanup. Type agreement is not authorization.
package CCL.VM.Resource_Values with SPARK_Mode is
   procedure Complete
     (Item : Validated_Program; State : in out Machine_State;
      Owner : CCL.Resources.Registry; Resource : CCL.Resources.Reference;
      Accepted : out Boolean)
     with Pre => Is_Valid (Item) and then Is_Well_Formed (Item, State),
       Post => Is_Well_Formed (Item, State);
   -- Rejection leaves the pending VM call intact. A successful completion
   -- transfers a resource value to the operand stack exactly once. The host
   -- must also validate each outgoing use against Owner before any IPC effect.
end CCL.VM.Resource_Values;
