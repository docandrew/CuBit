with Interfaces; use Interfaces;
with CuBit.Protocols; use CuBit.Protocols;
package body Proof_Cases with SPARK_Mode is
   use CuBit.Protocols.Stream_Policies;
   procedure Check (Left, Right : Policy) is
   begin
      pragma Assert (Compatible (Left, Right) = Compatible (Right, Left));
      pragma Assert (Compatible (Left, Left) = (Validate (Left) = Valid_Policy));
      if Compatible (Left, Right) then
         pragma Assert (Validate (Left) = Valid_Policy and Validate (Right) = Valid_Policy);
         pragma Assert (Left.Element = Right.Element);
         pragma Assert (Left.Delivery = Right.Delivery);
         pragma Assert (Left.Capacity = Right.Capacity);
         pragma Assert (Left.Normal_Close = Right.Normal_Close);
         pragma Assert (Left.Capacity.Maximum_In_Flight <= Left.Capacity.Slots);
         pragma Assert
           (Required_Payload_Bytes (Left) <= Payload_Count (Left.Capacity.Payload_Bytes));
      end if;
      if Left.Delivery.Kind = Lossless_Ordered and Right.Delivery.Kind /= Lossless_Ordered then
         pragma Assert (not Compatible (Left, Right));
      end if;
   end Check;
end Proof_Cases;
