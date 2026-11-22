with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Protocols; use CuBit.Protocols;
with CuBit.Protocols.Stream_Policies; use CuBit.Protocols.Stream_Policies;
with Proof_Cases;
procedure Main is
   Base : constant Policy :=
     (Element => INTEGER_64_CONTRACT,
      Delivery => (Kind => Lossless_Ordered, When_Full => Reject_Before_Acceptance),
      Capacity => (Slots => 8, Maximum_In_Flight => 1, Payload_Bytes => 64),
      Normal_Close => Drain_Accepted);
   Changed : Policy := Base;
   Profiles : constant array (Positive range <>) of Delivery_Policy :=
     [(Lossless_Ordered, Defer_Producer),
      (Lossless_Ordered, Reject_Before_Acceptance),
      (Lossless_Ordered, Fail_Stream),
      (Ordered_With_Gaps, Drop_Oldest_Pending),
      (Ordered_With_Gaps, Drop_Newest_Incoming),
      (Kind => Latest_Value)];
   Left, Right : Policy := Base;
   Checks : Natural := 0;
begin
   pragma Assert (Validate (Base) = Valid_Policy and Compatible (Base, Base));
   pragma Assert (Required_Payload_Bytes (Base) = 64);
   for I in Profiles'Range loop
      for J in Profiles'Range loop
         Left.Delivery := Profiles (I); Right.Delivery := Profiles (J);
         pragma Assert (Compatible (Left, Right) = (I = J));
         Proof_Cases.Check (Left, Right);
         Checks := Checks + 1;
      end loop;
   end loop;
   Changed.Element := NO_SCHEMA_CONTRACT;
   pragma Assert (Validate (Changed) = Invalid_Element_Schema);
   pragma Assert (Match (Changed, Base) = Invalid_Producer);
   pragma Assert (Match (Base, Changed) = Invalid_Consumer);
   Changed := Base; Changed.Element.Identity := INTEGER_64_SCHEMA + 1;
   pragma Assert (Match (Base, Changed) = Element_Mismatch);
   Changed := Base; Changed.Element.Version := 2;
   pragma Assert (Match (Base, Changed) = Element_Mismatch);
   Changed := Base; Changed.Element.Sizing := Bounded_Size;
   pragma Assert (Match (Base, Changed) = Element_Mismatch);
   Changed := Base; Changed.Element.Wire_Size := 7;
   pragma Assert (Match (Base, Changed) = Element_Mismatch);
   Changed := Base; Changed.Capacity.Maximum_In_Flight := 9;
   pragma Assert (Validate (Changed) = Too_Many_In_Flight);
   Changed := Base; Changed.Capacity.Payload_Bytes := 63;
   pragma Assert (Validate (Changed) = Insufficient_Payload_Budget);
   Changed := Base; Changed.Capacity.Payload_Bytes := 65;
   pragma Assert (Match (Base, Changed) = Capacity_Mismatch);
   Changed := Base; Changed.Capacity.Maximum_In_Flight := 2;
   pragma Assert (Match (Base, Changed) = Capacity_Mismatch);
   Changed := Base; Changed.Capacity.Slots := 7;
   pragma Assert (Match (Base, Changed) = Capacity_Mismatch);
   Changed := Base; Changed.Normal_Close := Discard_Pending_With_Report;
   pragma Assert (Match (Base, Changed) = Close_Mismatch);
   --  Extreme size accounting must not wrap into a small admitted budget.
   Changed := Base;
   Changed.Element.Wire_Size := Unsigned_32'Last;
   Changed.Capacity := (Slots => Slot_Count'Last, Maximum_In_Flight => Slot_Count'Last,
                        Payload_Bytes => Byte_Budget'Last);
   pragma Assert (Required_Payload_Bytes (Changed) = 16#FFFF_FFFE_0000_0001#);
   pragma Assert (Validate (Changed) = Valid_Policy);
   Changed.Capacity.Payload_Bytes := Unsigned_64 (Required_Payload_Bytes (Changed) - 1);
   pragma Assert (Validate (Changed) = Insufficient_Payload_Budget);
   --  Capacity boundary sweep, including exact and one-byte-short budgets.
   for Slots in Slot_Count range 1 .. 32 loop
      for Size in Unsigned_32 range 1 .. 32 loop
         Changed := Base; Changed.Element.Wire_Size := Size;
         Changed.Capacity := (Slots => Slots, Maximum_In_Flight => Slots,
                              Payload_Bytes => Unsigned_64 (Slots) * Unsigned_64 (Size));
         pragma Assert (Validate (Changed) = Valid_Policy);
         Proof_Cases.Check (Changed, Changed);
         if Changed.Capacity.Payload_Bytes > 1 then
            Changed.Capacity.Payload_Bytes := Changed.Capacity.Payload_Bytes - 1;
            pragma Assert (Validate (Changed) = Insufficient_Payload_Budget);
         end if;
         Checks := Checks + 1;
      end loop;
   end loop;
   Put_Line ("PASS: typed stream policy compatibility, delivery modes, close and bounded storage; matrix cases" & Checks'Image);
end Main;
