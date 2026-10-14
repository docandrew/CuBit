with Ada.Text_IO;
with Interfaces; use Interfaces;
with Capabilities; use Capabilities;
with Capability_ABI;
with Capability_Install; use Capability_Install;
--  Hardware catalog capabilities (CAP_HARDWARE_GROUP/REGISTER) under CAP-002:
--  the kinds keep their ordinals, and no derivation copies one into another
--  table (they originate only through trusted catalog admission), whatever
--  rights, tag or narrowing is asked.
procedure Capability_Tests is
   Checks : Natural := 0;
   procedure Check (Value : Boolean) is
   begin
      Checks := Checks + 1;
      if not Value then raise Program_Error with Checks'Image; end if;
   end Check;
   use type Capability_ABI.Refusal;
   Parent : Capability;
   Rights : CapabilityRights;
   Result : Outcome;
begin
   Check (CapabilityType'Pos (CAP_SCHEDULING) = 11);
   Check (CapabilityType'Pos (CAP_HARDWARE_GROUP) = 12);
   Check (CapabilityType'Pos (CAP_HARDWARE_REGISTER) = 13);
   for Kind in CapabilityType range CAP_HARDWARE_GROUP .. CAP_HARDWARE_REGISTER loop
      Check (not Transferable (Kind));
      Parent := (capType => Kind, rights => ALL_RIGHTS,
        authorityTag => 42, object => (ref => 7, param => 99), gen => 3);
      for Mask in Unsigned_64 range 0 .. 31 loop
         for Right in CapabilityRight loop
            Rights (Right) := (Mask and Shift_Left (1, CapabilityRight'Pos (Right))) /= 0;
         end loop;
         for Narrow in Boolean loop
            Result := Derive (Parent, (Rights => Rights, Narrow => Narrow,
                                       Base => 7, Length => (if Narrow then 1 else 0),
                                       others => <>));
            Check (not Result.Accepted
                   and then Result.Why = Capability_ABI.Not_Transferable);
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("HARDWARE-CAPABILITY-CANDIDATE: PASS" & Checks'Image);
end Capability_Tests;
