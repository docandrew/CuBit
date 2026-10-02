with System.Storage_Elements;
package body CuBit.Capability_Grants is
   use CuBit.Messages;
   type Inspection is array (0 .. 5) of Unsigned_64;
   function Inspect (Slot : CapabilitySlot) return Inspection;
   function Inspect (Slot : CapabilitySlot) return Inspection is
      Data : aliased Inspection := (others => 0);
      PID : constant Unsigned_64 := syscall (SYSCALL_GETPID);
      Result : Unsigned_64;
   begin
      if PID = 0 or else PID = Unsigned_64'Last then
         return (others => 0);
      end if;
      Result := syscall (SYSCALL_INSPECT_CAPABILITY, PID, Unsigned_64 (Slot),
        Unsigned_64 (System.Storage_Elements.To_Integer (Data'Address)));
      return (if Result = 1 then Data else (others => 0));
   end Inspect;
   function Valid (Target : Recipient) return Boolean is (Target.Wire /= 0);
   function Process_ID (Target : Recipient) return Unsigned_64 is
     (Target.Wire mod 2 ** 32);
   function Incarnation (Target : Recipient) return Unsigned_64 is
     (Target.Wire);
   function Capture (Slot : CapabilitySlot) return Recipient is
      Data : constant Inspection := Inspect (Slot);
   begin
      if Data (0) not in 1 | 6 | 10 or else
         Data (3) = 0 or else Data (3) > Unsigned_64 (Unsigned_32'Last) or else
         Data (5) = 0 or else Data (5) > Unsigned_64 (Unsigned_32'Last)
      then
         return (Wire => 0);
      end if;
      return (Wire => Data (5) * 2 ** 32 + Data (3));
   end Capture;
   function Endpoint_Matches
     (Slot : CapabilitySlot; Identity : Unsigned_64) return Boolean is
      Data : constant Inspection := Inspect (Slot);
   begin
      return Identity mod 2 ** 32 /= 0 and then Identity / 2 ** 32 /= 0
        and then Data (0) = 1 and then (Data (1) and 1) /= 0
        and then Data (3) = Identity mod 2 ** 32
        and then Data (5) = Identity / 2 ** 32;
   end Endpoint_Matches;
   function Install
     (Target : Recipient; Kind, Object, Parameter, Rights : Unsigned_64;
      Destination : CapabilitySlot) return Unsigned_64 is
   begin
      if not Valid (Target) or else Rights > 31 then
         return Unsigned_64'Last;
      end if;
      return syscall (SYSCALL_POLICY_MINT_CAPABILITY_FOR_INCARNATION,
                      Target.Wire, Kind, Object, Parameter, Rights,
                      Unsigned_64 (Destination));
   end Install;
   function Delegate_Endpoint
     (Target : Recipient; Source, Destination : CapabilitySlot;
      Rights, Authority_Tag : Unsigned_64) return Unsigned_64 is
   begin
      if not Valid (Target) or else Rights > 31 then
         return Unsigned_64'Last;
      end if;
      return syscall (SYSCALL_POLICY_DELEGATE_ENDPOINT,
                      Target.Wire, Unsigned_64 (Source),
                      Unsigned_64 (Destination), Rights, Authority_Tag, 0);
   end Delegate_Endpoint;
end CuBit.Capability_Grants;
