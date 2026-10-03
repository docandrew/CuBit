package body Capabilities.Endpoint_Disposal with SPARK_Mode is
   procedure Clear_If_Matching
     (Table : in out CapabilityTable; Slot : CapabilitySlot;
      Expected : Capability; Removed : out Boolean)
   is
   begin
      Removed := Matches (Table (Slot), Expected);
      if Removed then
         Table (Slot) := NULL_CAPABILITY;
      end if;
   end Clear_If_Matching;
end Capabilities.Endpoint_Disposal;
