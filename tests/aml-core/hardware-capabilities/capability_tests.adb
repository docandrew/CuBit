with Ada.Text_IO;
with Interfaces; use Interfaces;
with Capabilities; use Capabilities;
procedure Capability_Tests is
   Checks : Natural := 0;
   procedure Check (Value : Boolean) is
   begin
      Checks := Checks + 1;
      if not Value then raise Program_Error with Checks'Image; end if;
   end Check;
   Parent, Child : Capability;
   Rights : CapabilityRights;
begin
   Check (CapabilityType'Pos (CAP_SCHEDULING) = 11);
   Check (CapabilityType'Pos (CAP_HARDWARE_GROUP) = 12);
   Check (CapabilityType'Pos (CAP_HARDWARE_REGISTER) = 13);
   for Kind in CapabilityType loop
      Check (isPolicyMintable (Kind) =
        (Kind not in CAP_NULL | CAP_REPLY | CAP_HARDWARE_GROUP | CAP_HARDWARE_REGISTER));
   end loop;
   for Kind in CapabilityType range CAP_HARDWARE_GROUP .. CAP_HARDWARE_REGISTER loop
      Parent := (capType => Kind, rights => ALL_RIGHTS,
        authorityTag => 42, object => (ref => 7, param => 99), gen => 3);
      for Mask in Unsigned_64 range 0 .. 31 loop
         for Right in CapabilityRight loop
            Rights (Right) := (Mask and Shift_Left (1, CapabilityRight'Pos (Right))) /= 0;
         end loop;
         Child := derive (Parent, Rights);
         Check (Child.object = Parent.object and Child.gen = Parent.gen);
         Check (Child.capType = Kind and Child.authorityTag = Parent.authorityTag);
         Check (Child.rights = Rights);
         pragma Assert (isAttenuationOf (Child, Parent));
         Check (not isPolicyMintable (Child.capType));
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("HARDWARE-CAPABILITY-CANDIDATE: PASS" & Checks'Image);
end Capability_Tests;
