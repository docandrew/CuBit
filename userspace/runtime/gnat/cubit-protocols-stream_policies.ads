------------------------------------------------------------------------------
--  Portable stream delivery policy. No handles, authority, allocation or IPC.
--  A component of a future stream descriptor, NOT a complete stream contract.
------------------------------------------------------------------------------
package CuBit.Protocols.Stream_Policies with Pure, SPARK_Mode is
   type Delivery_Kind is (Lossless_Ordered, Ordered_With_Gaps, Latest_Value);
   type Non_Dropping_Overflow is
     (Defer_Producer, Reject_Before_Acceptance, Fail_Stream);
   type Lossy_Overflow is (Drop_Oldest_Pending, Drop_Newest_Incoming);

   --  Lossless policy cannot express silent loss. Latest-value replacement
   --  applies only to pending data, never to an element held by a consumer.
   --  With all slots held, reject latest-value submission before acceptance.
   type Delivery_Policy (Kind : Delivery_Kind := Lossless_Ordered) is record
      case Kind is
         when Lossless_Ordered =>
            When_Full : Non_Dropping_Overflow := Defer_Producer;
         when Ordered_With_Gaps =>
            When_Lossy_Full : Lossy_Overflow := Drop_Oldest_Pending;
         when Latest_Value => null;
      end case;
   end record;

   type Close_Policy is (Drain_Accepted, Discard_Pending_With_Report);
   subtype Slot_Count is Unsigned_32 range 1 .. Unsigned_32'Last;
   subtype Byte_Budget is Unsigned_64 range 1 .. Unsigned_64'Last;
   type Limits is record
      --  Slots include pending AND consumer-held elements. Control records,
      --  alignment and allocator overhead need separate admission accounting.
      Slots : Slot_Count := 1;
      Maximum_In_Flight : Slot_Count := 1;
      Payload_Bytes : Byte_Budget := 1;
   end record;

   type Policy is record
      Element : Schema_Contract := NO_SCHEMA_CONTRACT;
      Delivery : Delivery_Policy;
      Capacity : Limits;
      Normal_Close : Close_Policy := Drain_Accepted;
   end record;

   type Validation_Result is
     (Valid_Policy, Invalid_Element_Schema, Too_Many_In_Flight,
      Insufficient_Payload_Budget);
   --  Non-modular accounting: wrapping is not an allowed arithmetic result.
   --  The wire-facing fields remain U32/U64. Two U32 factors fit this range.
   type Payload_Count is range 0 .. 16#FFFF_FFFF_FFFF_FFFF#;
   function Required_Payload_Bytes (Item : Policy) return Payload_Count is
     (Payload_Count (Item.Capacity.Slots) *
        Payload_Count (Item.Element.Wire_Size));
   function Validate (Item : Policy) return Validation_Result is
     (if not Valid (Item.Element) then Invalid_Element_Schema
      elsif Item.Capacity.Maximum_In_Flight > Item.Capacity.Slots then
         Too_Many_In_Flight
      elsif Payload_Count (Item.Capacity.Payload_Bytes) <
        Required_Payload_Bytes (Item)
      then
         Insufficient_Payload_Budget
      else Valid_Policy);

   type Match_Result is
     (Policies_Match, Invalid_Producer, Invalid_Consumer, Element_Mismatch,
      Delivery_Mismatch, Capacity_Mismatch, Close_Mismatch);
   --  Exact profile matching, not negotiation or an authority decision.
   --  Select a mutually supported profile before reaching this boundary.
   function Match (Producer, Consumer : Policy) return Match_Result is
     (if Validate (Producer) /= Valid_Policy then Invalid_Producer
      elsif Validate (Consumer) /= Valid_Policy then Invalid_Consumer
      elsif not Compatible (Producer.Element, Consumer.Element) then
         Element_Mismatch
      elsif Producer.Delivery /= Consumer.Delivery then Delivery_Mismatch
      elsif Producer.Capacity /= Consumer.Capacity then Capacity_Mismatch
      elsif Producer.Normal_Close /= Consumer.Normal_Close then Close_Mismatch
      else Policies_Match);
   function Compatible (Producer, Consumer : Policy) return Boolean is
     (Match (Producer, Consumer) = Policies_Match);
end CuBit.Protocols.Stream_Policies;
